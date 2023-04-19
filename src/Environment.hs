-- | most of the code in this module is copied and edited from the ghc-pkg utility provided by GHC.

module Environment where

import qualified Control.Exception                      as Exception
import qualified Control.Foldl                          as F
import           Control.Monad

import           Data.Bifunctor
import qualified Data.ByteString                        as BS
import           Data.Char                              ( toLower )
import qualified Data.Foldable                          as F
import qualified Data.Graph                             as Graph
import           Data.List
import qualified Data.Map                               as Map
import           Data.Maybe
import qualified Data.Set                               as Set
import qualified Data.Traversable                       as F
import qualified Data.Version                           as Version

import           Distribution.Backpack
import           Distribution.InstalledPackageInfo      as Cabal
import           Distribution.ModuleName                ( ModuleName )
import qualified Distribution.ModuleName                as ModuleName
import           Distribution.Package                   hiding
                                                        ( installedUnitId )
import qualified Distribution.Parsec                    as Cabal
import           Distribution.Pretty                    ( Pretty (..) )
import qualified Distribution.Simple.PackageIndex       as PackageIndex
import           Distribution.Simple.Utils              ( readUTF8File,
                                                          toUTF8BS,
                                                          writeUTF8File )
import           Distribution.Text
import           Distribution.Types.LibraryName
import           Distribution.Types.MungedPackageId
import           Distribution.Types.MungedPackageName
import           Distribution.Types.UnqualComponentName
import           Distribution.Version

import           GHC.Base                               ( NonEmpty (..) )
import           GHC.BaseDir                            ( getBaseDir )
import qualified GHC.Data.ShortText                     as ST
import           GHC.IO                                 ()
import           GHC.IO.Exception                       ( IOErrorType (InappropriateType) )
import           GHC.Platform.Host                      ( hostPlatformArchOS )
import           GHC.Settings.Utils                     ( getTargetArchOS,
                                                          maybeReadFuzzy )
import           GHC.UniqueSubdir                       ( uniqueSubdir )
import           GHC.Unit.Database                      hiding
                                                        ( mkMungePathUrl )
import qualified GHC.Unit.Database                      as UDB
import           GHC.Version                            ( cProjectVersion )

import           Prelude

import           System.Directory                       ( XdgDirectory (XdgData),
                                                          createDirectoryIfMissing,
                                                          doesDirectoryExist,
                                                          doesFileExist,
                                                          getAppUserDataDirectory,
                                                          getCurrentDirectory,
                                                          getDirectoryContents,
                                                          getModificationTime,
                                                          getXdgDirectory,
                                                          removeFile )
import           System.Environment                     ( getEnv, getProgName )
import           System.Exit                            ( ExitCode (..),
                                                          exitWith )
import           System.FilePath                        as FilePath
import qualified System.FilePath.Posix                  as FilePath.Posix
import           System.IO
import           System.IO.Error

import qualified Turtle                                 as T
import           Turtle.Pattern                         ( suffix, text )
import qualified Turtle.Prelude                         as Tu
import           Turtle.Shell                           ( fold )

-- NOTE examples of package management from the ghc-pkg command line utlity
-- ["register", filename] ->
--     registerPackage filename verbosity cli
--                     multi_instance
--                     expand_env_vars False force

-- TODO add Nix
data BuildTool = Stack | Cabal deriving (Show, Eq)

-- NOTE for testing within GHCi
findBuildTool' :: T.FilePath -> IO (Maybe BuildTool)
findBuildTool' projectPath = Tu.cd projectPath >> findBuildTool


findBuildTool :: IO (Maybe BuildTool)
findBuildTool = do
  stack <- Tu.testfile "stack.yaml"
  if stack then return (Just Stack)
         else do
         dir <- Tu.pwd
         cabal <- fold (Tu.find (suffix ".cabal") dir) F.head
         if null cabal then return Nothing
                        else return (Just Cabal)


-- TODO convert to ExceptT for bettern composability and readablity
findInstalledPackageInfo :: BuildTool -> IO (Either (NonEmpty String) ([String], InstalledPackageInfo))
findInstalledPackageInfo Stack = undefined
findInstalledPackageInfo Cabal = do
  let location = "dist-newstyle/packagedb/"
  Tu.cd location
  dir <- Tu.pwd
  packageFile <- fold (Tu.find (text ".cache") location) F.head
  return undefined




-- TODO refactor with Moand transformers
getInstalledPackageInfo :: IO (Maybe InstalledPackageInfo)
getInstalledPackageInfo = do
  buildTool <- findBuildTool
  case findInstalledPackageInfo <$> buildTool of
    Nothing -> return Nothing
    Just packageInfo -> packageInfo >>= \case
      Left _                          -> return Nothing
      Right (_, installedPackageInfo) -> return $ Just installedPackageInfo


anyM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
anyM _ [] = return False
anyM p (x:xs) = do
  b <- p x
  if b
    then return True
    else anyM p xs


data Flag
  = FlagUser
  | FlagGlobal
  | FlagHelp
  | FlagVersion
  | FlagConfig FilePath
  | FlagGlobalConfig FilePath
  | FlagUserConfig FilePath
  | FlagForce
  | FlagForceFiles
  | FlagMultiInstance
  | FlagExpandEnvVars
  | FlagExpandPkgroot
  | FlagNoExpandPkgroot
  | FlagSimpleOutput
  | FlagNamesOnly
  | FlagIgnoreCase
  | FlagNoUserDb
  | FlagVerbosity (Maybe String)
  | FlagUnitId
  | FlagShowUnitIds
  deriving Eq


-- Do the business
data Force = NoForce | ForceFiles | ForceAll | CannotForce
  deriving (Eq,Ord)

-- | Represents how a package may be specified by a user on the command line.
data PackageArg
    -- | A package identifier foo-0.1, or a glob foo-*
    = Id GlobPackageIdentifier
    -- | An installed package ID foo-0.1-HASH.  This is guaranteed to uniquely
    -- match a single entry in the package database.
    | IUId UnitId
    -- | A glob against the package name.  The first string is the literal
    -- glob, the second is a function which returns @True@ if the argument
    -- matches.
    | Substring String (String -> Bool)


-- | Either an exact 'PackageIdentifier', or a glob for all packages
-- matching 'PackageName'.
data GlobPackageIdentifier
    = ExactPackageIdentifier MungedPackageId
    | GlobPackageIdentifier  MungedPackageName

displayGlobPkgId :: GlobPackageIdentifier -> String
displayGlobPkgId (ExactPackageIdentifier pid) = display pid
displayGlobPkgId (GlobPackageIdentifier pn)   = display pn ++ "-*"

-- -----------------------------------------------------------------------------
-- Package databases

-- Some commands operate on a single database:
--      register, unregister, expose, hide, trust, distrust
-- however these commands also check the union of the available databases
-- in order to check consistency.  For example, register will check that
-- dependencies exist before registering a package.
--
-- Some commands operate  on multiple databases, with overlapping semantics:
--      list, describe, field

data PackageDB (mode :: UDB.DbMode)
  = PackageDB {
      location, locationAbsolute :: !FilePath,
      -- We need both possibly-relative and definitely-absolute package
      -- db locations. This is because the relative location is used as
      -- an identifier for the db, so it is important we do not modify it.
      -- On the other hand we need the absolute path in a few places
      -- particularly in relation to the ${pkgroot} stuff.

      packageDbLock              :: !(UDB.DbOpenMode mode UDB.PackageDbLock),
      -- If package db is open in read write mode, we keep its lock around for
      -- transactional updates.

      packages                   :: [InstalledPackageInfo]
    }

type PackageDBStack = [PackageDB 'UDB.DbReadOnly]
        -- A stack of package databases.  Convention: head is the topmost
        -- in the stack.

-- | Selector for picking the right package DB to modify as 'register' and
-- 'recache' operate on the database on top of the stack, whereas 'modify'
-- changes the first database that contains a specific package.
data DbModifySelector = TopOne | ContainsPkg PackageArg

allPackagesInStack :: PackageDBStack -> [InstalledPackageInfo]
allPackagesInStack = concatMap packages

-- | Retain only the part of the stack up to and including the given package
-- DB (where the global package DB is the bottom of the stack). The resulting
-- package DB stack contains exactly the packages that packages from the
-- specified package DB can depend on, since dependencies can only extend
-- down the stack, not up (e.g. global packages cannot depend on user
-- packages).
stackUpTo :: FilePath -> PackageDBStack -> PackageDBStack
stackUpTo to_modify = dropWhile ((/= to_modify) . location)

getPkgDatabases :: UDB.DbOpenMode mode DbModifySelector
                -> Bool    -- use the user db
                -> Bool    -- read caches, if available
                -> Bool    -- expand vars, like ${pkgroot} and $topdir
                -> [Flag]
                -> IO (PackageDBStack,
                          -- the real package DB stack: [global,user] ++
                          -- DBs specified on the command line with -f.
                       UDB.DbOpenMode mode (PackageDB mode),
                          -- which one to modify, if any
                       PackageDBStack)
                          -- the package DBs specified on the command
                          -- line, or [global,user] otherwise.  This
                          -- is used as the list of package DBs for
                          -- commands that just read the DB, such as 'list'.

getPkgDatabases mode use_user use_cache expand_vars my_flags = do
  -- Second we determine the location of the global package config.  On Windows,
  -- this is found relative to the ghc-pkg.exe binary, whereas on Unix the
  -- location is passed to the binary using the --global-package-db flag by the
  -- wrapper script.
  let err_msg = "missing --global-package-db option, location of global package database unknown\n"
  global_conf <-
     case [ f | FlagGlobalConfig f <- my_flags ] of
        -- See Note [Base Dir] for more information on the base dir / top dir.
        [] -> do mb_dir <- getBaseDir
                 case mb_dir of
                   Nothing  -> die err_msg
                   Just dir -> do
                     r <- lookForPackageDBIn dir
                     case r of
                       Nothing -> die ("Can't find package database in " ++ dir)
                       Just path -> return path
        fs -> return (last fs)

  -- The value of the $topdir variable used in some package descriptions
  -- Note that the way we calculate this is slightly different to how it
  -- is done in ghc itself. We rely on the convention that the global
  -- package db lives in ghc's libdir.
  top_dir <- absolutePath (takeDirectory global_conf)

  let no_user_db = FlagNoUserDb `elem` my_flags

  -- get the location of the user package database, and create it if necessary
  -- getXdgDirectory can fail (e.g. if $HOME isn't set)

  mb_user_conf <-
    case [ f | FlagUserConfig f <- my_flags ] of
      _ | no_user_db -> return Nothing
      [] -> do
        -- See Note [Settings File] about this file, and why we need GHC to share it with us.
        let settingsFile = top_dir </> "settings"
        exists_settings_file <- doesFileExist settingsFile
        targetArchOS <- case exists_settings_file of
          False -> do
            warn $ "WARNING: settings file doesn't exist " ++ show settingsFile
            warn "cannot know target platform so guessing target == host (native compiler)."
            pure hostPlatformArchOS
          True -> do
            settingsStr <- readFile settingsFile
            mySettings <- case maybeReadFuzzy settingsStr of
              Just s  -> pure $ Map.fromList s
              -- It's excusable to not have a settings file (for now at
              -- least) but completely inexcusable to have a malformed one.
              Nothing -> die $ "Can't parse settings file " ++ show settingsFile
            case getTargetArchOS settingsFile mySettings of
              Right archOS -> pure archOS
              Left e       -> die e
        let subdir = uniqueSubdir targetArchOS

            getFirstSuccess :: [IO a] -> IO (Maybe a)
            getFirstSuccess [] = pure Nothing
            getFirstSuccess (a:as) = tryIO a >>= \case
              Left _ -> getFirstSuccess as
              Right d -> pure (Just d)
        -- The appdir used to be in ~/.ghc but to respect the XDG specification
        -- we want to move it under $XDG_DATA_HOME/
        -- However, old tooling (like cabal) might still write package environments
        -- to the old directory, so we prefer that if a subdirectory of ~/.ghc
        -- with the correct target and GHC version exists.
        --
        -- i.e. if ~/.ghc/$UNIQUE_SUBDIR exists we prefer that
        -- otherwise we use $XDG_DATA_HOME/$UNIQUE_SUBDIR
        --
        -- UNIQUE_SUBDIR is typically a combination of the target platform and GHC version
        m_appdir <- getFirstSuccess $ map (fmap (</> subdir))
          [ getAppUserDataDirectory "ghc"  -- this is ~/.ghc/
          , getXdgDirectory XdgData "ghc"  -- this is $XDG_DATA_HOME/
          ]
        case m_appdir of
          Nothing -> return Nothing
          Just dir -> do
            lookForPackageDBIn dir >>= \case
              Nothing -> return (Just (dir </> "package.conf.d", False))
              Just f  -> return (Just (f, True))
      fs -> return (Just (last fs, True))

  -- If the user database exists, and for "use_user" commands (which includes
  -- "ghc-pkg check" and all commands that modify the db) we will attempt to
  -- use the user db.
  let sys_databases
        | Just (user_conf,user_exists) <- mb_user_conf,
          use_user || user_exists = [user_conf, global_conf]
        | otherwise               = [global_conf]

  e_pkg_path <- tryIO (System.Environment.getEnv "GHC_PACKAGE_PATH")
  let env_stack =
        case e_pkg_path of
                Left  _ -> sys_databases
                Right path
                  | not (null path) && isSearchPathSeparator (last path)
                  -> splitSearchPath (init path) ++ sys_databases
                  | otherwise
                  -> splitSearchPath path

        -- The "global" database is always the one at the bottom of the stack.
        -- This is the database we modify by default.
      virt_global_conf = last env_stack

  let db_flags = mapMaybe is_db_flag my_flags
         where is_db_flag FlagUser
                      | Just (user_conf, _user_exists) <- mb_user_conf
                      = Just user_conf
               is_db_flag FlagGlobal     = Just virt_global_conf
               is_db_flag (FlagConfig f) = Just f
               is_db_flag _              = Nothing

  let flag_db_names | null db_flags = env_stack
                    | otherwise     = reverse (nub db_flags)

  -- For a "modify" command, treat all the databases as
  -- a stack, where we are modifying the top one, but it
  -- can refer to packages in databases further down the
  -- stack.

  -- -f flags on the command line add to the database
  -- stack, unless any of them are present in the stack
  -- already.
  let final_stack = filter (`notElem` env_stack)
                     [ f | FlagConfig f <- reverse my_flags ]
                     ++ env_stack

      top_db = if null db_flags
               then virt_global_conf
               else last db_flags

  (db_stack, db_to_operate_on) <- getDatabases top_dir mb_user_conf
                                               flag_db_names final_stack top_db

  let flag_db_stack = [ db | db_name <- flag_db_names,
                        db <- db_stack, location db == db_name ]

  return (db_stack, db_to_operate_on, flag_db_stack)
  where
    getDatabases top_dir mb_user_conf flag_db_names
                 final_stack top_db = case mode of
      -- When we open in read only mode, we simply read all of the databases/
      UDB.DbOpenReadOnly -> do
        db_stack <- mapM readDatabase final_stack
        return (db_stack, UDB.DbOpenReadOnly)

      -- The only package db we open in read write mode is the one on the top of
      -- the stack.
      UDB.DbOpenReadWrite TopOne -> do
        (db_stack, mto_modify) <- stateSequence Nothing
          [ \case
              to_modify@(Just _) -> (, to_modify) <$> readDatabase db_path
              Nothing -> if db_path /= top_db
                then (, Nothing) <$> readDatabase db_path
                else do
                  db <- readParseDatabase mb_user_conf
                                          mode use_cache db_path
                    `Exception.catch` couldntOpenDbForModification db_path
                  let ro_db = db { packageDbLock = UDB.DbOpenReadOnly }
                  return (ro_db, Just db)
          | db_path <- final_stack ]

        to_modify <- case mto_modify of
          Just db -> return db
          Nothing -> die "no database selected for modification"

        return (db_stack, UDB.DbOpenReadWrite to_modify)

      -- The package db we open in read write mode is the first one included in
      -- flag_db_names that contains specified package. Therefore we need to
      -- open each one in read/write mode first and decide whether it's for
      -- modification based on its contents.
      UDB.DbOpenReadWrite (ContainsPkg pkgarg) -> do
        (db_stack, mto_modify) <- stateSequence Nothing
          [ \case
              to_modify@(Just _) -> (, to_modify) <$> readDatabase db_path
              Nothing -> if db_path `notElem` flag_db_names
                then (, Nothing) <$> readDatabase db_path
                else do
                  let hasPkg :: PackageDB mode -> Bool
                      hasPkg = not . null . findPackage pkgarg . packages

                      openRo (e::IOError) = do
                        db <- readDatabase db_path
                        if hasPkg db
                          then couldntOpenDbForModification db_path e
                          else return (db, Nothing)

                  -- If we fail to open the database in read/write mode, we need
                  -- to check if it's for modification first before throwing an
                  -- error, so we attempt to open it in read only mode.
                  Exception.handle openRo $ do
                    db <- readParseDatabase mb_user_conf
                                            mode use_cache db_path
                    let ro_db = db { packageDbLock = UDB.DbOpenReadOnly }
                    if hasPkg db
                      then return (ro_db, Just db)
                      else do
                        -- If the database is not for modification after all,
                        -- drop the write lock as we are already finished with
                        -- the database.
                        case packageDbLock db of
                          UDB.DbOpenReadWrite lock ->
                            UDB.unlockPackageDb lock
                        return (ro_db, Nothing)
          | db_path <- final_stack ]

        to_modify <- case mto_modify of
          Just db -> return db
          Nothing -> cannotFindPackage pkgarg Nothing

        return (db_stack, UDB.DbOpenReadWrite to_modify)
      where
        couldntOpenDbForModification :: FilePath -> IOError -> IO a
        couldntOpenDbForModification db_path e = die $ "Couldn't open database "
          ++ db_path ++ " for modification: " ++ show e

        -- Parse package db in read-only mode.
        readDatabase :: FilePath -> IO (PackageDB 'UDB.DbReadOnly)
        readDatabase db_path = do
          db <- readParseDatabase mb_user_conf
                                  UDB.DbOpenReadOnly use_cache db_path
          if expand_vars
            then return $ mungePackageDBPaths top_dir db
            else return db

    stateSequence :: Monad m => s -> [s -> m (a, s)] -> m ([a], s)
    stateSequence s []     = return ([], s)
    stateSequence s (m:ms) = do
      (a, s')   <- m s
      (as, s'') <- stateSequence s' ms
      return (a : as, s'')


lookForPackageDBIn :: FilePath -> IO (Maybe FilePath)
lookForPackageDBIn dir = do
  let path_dir = dir </> "package.conf.d"
  exists_dir <- doesDirectoryExist path_dir
  if exists_dir then return (Just path_dir) else do
    let path_file = dir </> "package.conf"
    exists_file <- doesFileExist path_file
    if exists_file then return (Just path_file) else return Nothing

-- NOTE remove all the loging stuff and
readParseDatabase :: forall mode t. Maybe (FilePath,Bool)
                  -> UDB.DbOpenMode mode t
                  -> Bool -- use cache
                  -> FilePath
                  -> IO (PackageDB mode)
readParseDatabase mb_user_conf mode use_cache path
  -- the user database (only) is allowed to be non-existent
  | Just (user_conf,False) <- mb_user_conf, path == user_conf
  = do lock <- F.forM mode $ \_ -> do
         createDirectoryIfMissing True path
         UDB.lockPackageDb cache
       mkPackageDB [] lock
  | otherwise
  = do e <- tryIO $ getDirectoryContents path
       case e of
         Left err
           | ioeGetErrorType err == InappropriateType -> do
              -- We provide a limited degree of backwards compatibility for
              -- old single-file style db:
              mdb <- tryReadParseOldFileStyleDatabase
                       mb_user_conf mode use_cache path
              case mdb of
                Just db -> return db
                Nothing ->
                  die $ "ghc no longer supports single-file style package "
                     ++ "databases (" ++ path ++ ") use 'ghc-pkg init'"
                     ++ "to create the database with the correct format."

           | otherwise -> ioError err
         Right fs
           | not use_cache -> ignore_cache (const $ return ())
           | otherwise -> do
              e_tcache <- tryIO $ getModificationTime cache
              case e_tcache of
                Left _ -> ignore_cache (const $ return ())
                Right tcache -> do
                 -- If any of the .conf files is newer than package.cache, we
                  -- assume that cache is out of date.
                  cache_outdated <- (`anyM` confs) $ \conf ->
                    (tcache <) <$> getModificationTime conf
                  if not cache_outdated
                      then do UDB.readPackageDbForGhcPkg cache mode >>= uncurry mkPackageDB
                      else ignore_cache . const $ warn "old cache"
            where
                 confs = map (path </>) $ filter (".conf" `isSuffixOf`) fs

                 ignore_cache :: (FilePath -> IO ()) -> IO (PackageDB mode)
                 ignore_cache checkTime = do
                     -- If we're opening for modification, we need to acquire a
                     -- lock even if we don't open the cache now, because we are
                     -- going to modify it later.
                     lock <- F.mapM (const $ UDB.lockPackageDb cache) mode
                     let doFile f = do checkTime f
                                       parseSingletonPackageConf f
                     pkgs <- mapM doFile confs
                     mkPackageDB pkgs lock

  where
    cache = path </> cachefilename
    mkPackageDB :: [InstalledPackageInfo]
                -> UDB.DbOpenMode mode UDB.PackageDbLock
                -> IO (PackageDB mode)
    mkPackageDB pkgs lock = do
      path_abs <- absolutePath path
      return $ PackageDB {
          location = path,
          locationAbsolute = path_abs,
          packageDbLock = lock,
          packages = pkgs
        }

-- NOTE super important
parseSingletonPackageConf :: FilePath -> IO InstalledPackageInfo
parseSingletonPackageConf file = BS.readFile file >>= fmap fst . parsePackageInfo

cachefilename :: FilePath
cachefilename = "package.cache"

mungePackageDBPaths :: FilePath -> PackageDB mode -> PackageDB mode
mungePackageDBPaths top_dir db@PackageDB { packages = pkgs } =
    db { packages = map (mungePackagePaths top_dir pkgroot) pkgs }
  where
    pkgroot = takeDirectory $ dropTrailingPathSeparator (locationAbsolute db)
    -- It so happens that for both styles of package db ("package.conf"
    -- files and "package.conf.d" dirs) the pkgroot is the parent directory
    -- ${pkgroot}/package.conf  or  ${pkgroot}/package.conf.d/

-- | Perform path/URL variable substitution as per the Cabal ${pkgroot} spec
-- (http://www.haskell.org/pipermail/libraries/2009-May/011772.html)
-- Paths/URLs can be relative to ${pkgroot} or ${pkgrooturl}.
-- The "pkgroot" is the directory containing the package database.
--
-- Also perform a similar substitution for the older GHC-specific
-- "$topdir" variable. The "topdir" is the location of the ghc
-- installation (obtained from the -B option).
mungePackagePaths :: FilePath -> FilePath
                  -> InstalledPackageInfo -> InstalledPackageInfo
mungePackagePaths top_dir pkgroot pkg =
   -- TODO: similar code is duplicated in GHC.Unit.Database
    pkg {
      importDirs  = munge_paths (importDirs pkg),
      includeDirs = munge_paths (includeDirs pkg),
      libraryDirs = munge_paths (libraryDirs pkg),
      libraryDynDirs = munge_paths (libraryDynDirs pkg),
      frameworkDirs = munge_paths (frameworkDirs pkg),
      haddockInterfaces = munge_paths (haddockInterfaces pkg),
      -- haddock-html is allowed to be either a URL or a file
      haddockHTMLs = munge_paths (munge_urls (haddockHTMLs pkg))
    }
  where
    munge_paths = map munge_path
    munge_urls  = map munge_url
    (munge_path,munge_url) = mkMungePathUrl top_dir pkgroot

mkMungePathUrl :: FilePath -> FilePath -> (FilePath -> FilePath, FilePath -> FilePath)
mkMungePathUrl top_dir pkgroot = (munge_path, munge_url)
   where
    munge_path p
      | Just p' <- stripVarPrefix "${pkgroot}" p = pkgroot ++ p'
      | Just p' <- stripVarPrefix "$topdir"    p = top_dir ++ p'
      | otherwise                                = p

    munge_url p
      | Just p' <- stripVarPrefix "${pkgrooturl}" p = toUrlPath pkgroot p'
      | Just p' <- stripVarPrefix "$httptopdir"   p = toUrlPath top_dir p'
      | otherwise                                   = p

    toUrlPath r p = "file:///"
                 -- URLs always use posix style '/' separators:
                 ++ FilePath.Posix.joinPath
                        (r : -- We need to drop a leading "/" or "\\"
                             -- if there is one:
                             dropWhile (all isPathSeparator)
                                       (FilePath.splitDirectories p))

    -- We could drop the separator here, and then use </> above. However,
    -- by leaving it in and using ++ we keep the same path separator
    -- rather than letting FilePath change it to use \ as the separator
    stripVarPrefix var path = case stripPrefix var path of
                              Just []                             -> Just []
                              Just cs@(c : _) | isPathSeparator c -> Just cs
                              _                                   -> Nothing

-- -----------------------------------------------------------------------------
-- Workaround for old single-file style package dbs

-- Single-file style package dbs have been deprecated for some time, but
-- it turns out that Cabal was using them in one place. So this code is for a
-- workaround to allow older Cabal versions to use this newer ghc.

-- We check if the file db contains just "[]" and if so, we look for a new
-- dir-style db in path.d/, ie in a dir next to the given file.
-- We cannot just replace the file with a new dir style since Cabal still
-- assumes it's a file and tries to overwrite with 'writeFile'.

-- ghc itself also cooperates in this workaround

tryReadParseOldFileStyleDatabase :: Maybe (FilePath, Bool)
                                 -> UDB.DbOpenMode mode t -> Bool -> FilePath
                                 -> IO (Maybe (PackageDB mode))
tryReadParseOldFileStyleDatabase mb_user_conf
                                 mode use_cache path = do
  -- assumes we've already established that path exists and is not a dir
  content <- readFile path `catchIO` \_ -> return ""
  if take 2 content == "[]"
    then do
      path_abs <- absolutePath path
      let path_dir = adjustOldDatabasePath path
      warn $ "Warning: ignoring old file-style db and trying " ++ path_dir
      direxists <- doesDirectoryExist path_dir
      if direxists
        then do
          db <- readParseDatabase mb_user_conf mode use_cache path_dir
          -- but pretend it was at the original location
          return $ Just db {
              location         = path,
              locationAbsolute = path_abs
            }
         else do
           lock <- F.forM mode $ \_ -> do
             createDirectoryIfMissing True path_dir
             UDB.lockPackageDb $ path_dir </> cachefilename
           return $ Just PackageDB {
               location         = path,
               locationAbsolute = path_abs,
               packageDbLock    = lock,
               packages         = []
             }

    -- if the path is not a file, or is not an empty db then we fail
    else return Nothing

adjustOldFileStylePackageDB :: PackageDB mode -> IO (PackageDB mode)
adjustOldFileStylePackageDB db = do
  -- assumes we have not yet established if it's an old style or not
  mcontent <- liftM Just (readFile (location db)) `catchIO` \_ -> return Nothing
  case fmap (take 2) mcontent of
    -- it is an old style and empty db, so look for a dir kind in location.d/
    Just "[]" -> return db {
        location         = adjustOldDatabasePath $ location db,
        locationAbsolute = adjustOldDatabasePath $ locationAbsolute db
      }
    -- it is old style but not empty, we have to bail
    Just  _   -> die $ "ghc no longer supports single-file style package "
                    ++ "databases (" ++ location db ++ ") use 'ghc-pkg init'"
                    ++ "to create the database with the correct format."
    -- probably not old style, carry on as normal
    Nothing   -> return db

adjustOldDatabasePath :: FilePath -> FilePath
adjustOldDatabasePath = (<.> "d")

-- Registering

registerPackage :: FilePath
                -> [Flag]
                -> Bool              -- multi_instance
                -> Bool              -- expand_env_vars
                -> Bool              -- update
                -> Force
                -> IO ()
registerPackage input my_flags multi_instance
                expand_env_vars update force = do
  (db_stack, UDB.DbOpenReadWrite db_to_operate_on, _flag_dbs) <-
    getPkgDatabases (UDB.DbOpenReadWrite TopOne)
      True{-use user-} True{-use cache-} False{-expand vars-} my_flags

  let to_modify = location db_to_operate_on
  s <- readUTF8File input
  expanded <- if expand_env_vars then expandEnvVars s force
                                 else return s

  (pkg, _) <- parsePackageInfo $ toUTF8BS expanded
  -- validate the expanded pkg, but register the unexpanded
  pkgroot <- absolutePath (takeDirectory to_modify)
  let top_dir = takeDirectory (location (last db_stack))
      pkg_expanded = mungePackagePaths top_dir pkgroot pkg

  let truncated_stack = stackUpTo to_modify db_stack
  -- truncate the stack for validation, because we don't allow
  -- packages lower in the stack to refer to those higher up.
  validatePackageConfig pkg_expanded truncated_stack
                        multi_instance update force

  let
     -- In the normal mode, we only allow one version of each package, so we
     -- remove all instances with the same source package id as the one we're
     -- adding. In the multi instance mode we don't do that, thus allowing
     -- multiple instances with the same source package id.
     removes = [ RemovePackage p
               | not multi_instance,
                 p <- packages db_to_operate_on,
                 mungedId p == mungedId pkg,
                 -- Only remove things that were instantiated the same way!
                 instantiatedWith p == instantiatedWith pkg ]
  --
  changeDB (removes ++ [AddPackage pkg]) db_to_operate_on db_stack

parsePackageInfo
        :: BS.ByteString
        -> IO (InstalledPackageInfo, [ValidateWarning])
parsePackageInfo str =
    case parseInstalledPackageInfo str of
      Right (warnings, ok) -> pure (mungePackageInfo ok, ws)
        where
          ws = [ msg | msg <- warnings
                    , not ("Unrecognized field pkgroot" `isPrefixOf` msg) ]
      Left err -> die (unlines (F.toList err))
  where
    mungePackageInfo = id



-- -----------------------------------------------------------------------------
-- Making changes to a package database

data DBOp = RemovePackage InstalledPackageInfo
          | AddPackage    InstalledPackageInfo
          | ModifyPackage InstalledPackageInfo

changeDB :: [DBOp]
         -> PackageDB 'UDB.DbReadWrite
         -> PackageDBStack
         -> IO ()
changeDB cmds db db_stack = do
  let db' = updateInternalDB db cmds
  db'' <- adjustOldFileStylePackageDB db'
  createDirectoryIfMissing True (location db'')
  changeDBDir cmds db'' db_stack

updateInternalDB :: PackageDB 'UDB.DbReadWrite
                 -> [DBOp] -> PackageDB 'UDB.DbReadWrite
updateInternalDB db cmds = db{ packages = foldl do_cmd (packages db) cmds }
 where
  do_cmd pkgs (RemovePackage p) =
    filter ((/= installedUnitId p) . installedUnitId) pkgs
  do_cmd pkgs (AddPackage p) = p : pkgs
  do_cmd pkgs (ModifyPackage p) =
    do_cmd (do_cmd pkgs (RemovePackage p)) (AddPackage p)


changeDBDir :: [DBOp]
            -> PackageDB 'UDB.DbReadWrite
            -> PackageDBStack
            -> IO ()
changeDBDir cmds db db_stack = do
  mapM_ do_cmd cmds
  updateDBCache db db_stack
 where
  do_cmd (RemovePackage p) = do
    let file = location db </> display (installedUnitId p) <.> "conf"
    -- when (verbosity > Normal) $ infoLn ("removing " ++ file)
    removeFileSafe file
  do_cmd (AddPackage p) = do
    let file = location db </> display (installedUnitId p) <.> "conf"
    writeUTF8File file (showInstalledPackageInfo p)
  do_cmd (ModifyPackage p) =
    do_cmd (AddPackage p)

updateDBCache :: PackageDB 'UDB.DbReadWrite
              -> PackageDBStack
              -> IO ()
updateDBCache db db_stack = do
  let filename = location db </> cachefilename
      db_stack_below = stackUpTo (location db) db_stack

      pkgsCabalFormat :: [InstalledPackageInfo]
      pkgsCabalFormat = packages db

      -- | All the packages we can legally depend on in this step.
      dependablePkgsCabalFormat :: [InstalledPackageInfo]
      dependablePkgsCabalFormat = allPackagesInStack db_stack_below

      pkgsGhcCacheFormat :: [(PackageCacheFormat, Bool)]
      pkgsGhcCacheFormat
        -- See Note [Recompute abi-depends]
        = map (recomputeValidAbiDeps dependablePkgsCabalFormat . convertPackageInfoToCacheFormat) pkgsCabalFormat

      hasAnyAbiDepends :: InstalledPackageInfo -> Bool
      hasAnyAbiDepends x = not (null (abiDepends x))

  -- warn when we find any (possibly-)bogus abi-depends fields;
  -- Note [Recompute abi-depends]

  let d = fmap (fromPackageCacheFormat . fst) pkgsGhcCacheFormat
  UDB.writePackageDb filename d pkgsCabalFormat
    `catchIO` \e ->
      if isPermissionError e
      then die $ filename ++ ": you don't have permission to modify this file"
      else ioError e

  case packageDbLock db of
    UDB.DbOpenReadWrite lock -> UDB.unlockPackageDb lock


type PackageCacheFormat = UDB.GenericUnitInfo
                            ComponentId
                            PackageIdentifier
                            PackageName
                            UnitId
                            ModuleName
                            OpenModule

{- Note [Recompute abi-depends]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Like most fields, `ghc-pkg` relies on who-ever is performing package
registration to fill in fields; this includes the `abi-depends` field present
for the package.

However, this was likely a mistake, and is not very robust; in certain cases,
versions of Cabal may use bogus abi-depends fields for a package when doing
builds. Why? Because package database information is aggressively cached; it is
possible to work Cabal into a situation where it uses a cached version of
`abi-depends`, rather than the one in the actual database after it has been
recomputed.

However, there is an easy fix: ghc-pkg /already/ knows the `abi-depends` of a
package, because they are the ABIs of the packages pointed at by the `depends`
field. So it can simply look up the abi from the dependencies in the original
database, and ignore whatever the system registering gave it.

So, instead, we do two things here:

  - We throw away the information for a registered package's `abi-depends` field.

  - We recompute it: we simply look up the unit ID of the package in the original
    database, and use *its* abi-depends.

See #14381, and Cabal issue #4728.

Additionally, because we are throwing away the original (declared) ABI deps, we
return a boolean that indicates whether any abi-depends were actually
overridden.

-}

recomputeValidAbiDeps :: [InstalledPackageInfo]
                      -> PackageCacheFormat
                      -> (PackageCacheFormat, Bool)
recomputeValidAbiDeps db pkg =
  (pkg { UDB.unitAbiDepends = newAbiDeps }, abiDepsUpdated)
  where
    newAbiDeps =
      catMaybes . flip map (UDB.unitAbiDepends pkg) $ \(k, _) ->
        case filter (\d -> installedUnitId d == k) db of
          [x] -> Just (k, ST.pack $ unAbiHash (abiHash x))
          _   -> Nothing
    abiDepsUpdated =
      UDB.unitAbiDepends pkg /= newAbiDeps


-- | Convert from PackageCacheFormat to DbUnitInfo (the format used in
-- Ghc.PackageDb to store into the database)
-- NOTE DBUnitInfo is a dumbed down version of Cabal's InstalledPackageInfo
-- which only keeps the parts that GHC cares about. i.e, this is what we are going to
-- actually feed to GHC.
fromPackageCacheFormat :: PackageCacheFormat -> UDB.DbUnitInfo
fromPackageCacheFormat = UDB.mapGenericUnitInfo
     mkUnitId' mkComponentId' mkPackageIdentifier' mkPackageName' mkModuleName' mkModule'
   where
     displayBS :: Pretty a => a -> BS.ByteString
     displayBS            = toUTF8BS . display
     mkPackageIdentifier' = displayBS
     mkPackageName'       = displayBS
     mkComponentId'       = displayBS
     mkUnitId'            = displayBS
     mkModuleName'        = displayBS
     mkInstUnitId' i      = case i of
       IndefFullUnitId cid insts -> DbInstUnitId (mkComponentId' cid)
                                                 (fmap (bimap mkModuleName' mkModule') (Map.toList insts))
       DefiniteUnitId uid        -> DbUnitId (mkUnitId' (unDefUnitId uid))
     mkModule' m = case m of
       OpenModule uid n -> DbModule (mkInstUnitId' uid) (mkModuleName' n)
       OpenModuleVar n  -> DbModuleVar  (mkModuleName' n)


convertPackageInfoToCacheFormat :: InstalledPackageInfo -> PackageCacheFormat
convertPackageInfoToCacheFormat pkg =
    UDB.GenericUnitInfo {
       UDB.unitId             = installedUnitId pkg,
       UDB.unitInstanceOf     = installedComponentId pkg,
       UDB.unitInstantiations = instantiatedWith pkg,
       UDB.unitPackageId      = sourcePackageId pkg,
       UDB.unitPackageName    = packageName pkg,
       UDB.unitPackageVersion = Version.Version (versionNumbers (packageVersion pkg)) [],
       UDB.unitComponentName  =
         fmap (mkPackageName . unUnqualComponentName) (libraryNameString $ sourceLibName pkg),
       UDB.unitDepends        = depends pkg,
       UDB.unitAbiDepends     = map (\(AbiDependency k v) -> (k,ST.pack $ unAbiHash v)) (abiDepends pkg),
       UDB.unitAbiHash        = ST.pack $ unAbiHash (abiHash pkg),
       UDB.unitImportDirs     = map ST.pack $ importDirs pkg,
       UDB.unitLibraries      = map ST.pack $ hsLibraries pkg,
       UDB.unitExtDepLibsSys  = map ST.pack $ extraLibraries pkg,
       UDB.unitExtDepLibsGhc  = map ST.pack $ extraGHCiLibraries pkg,
       UDB.unitLibraryDirs    = map ST.pack $ libraryDirs pkg,
       UDB.unitLibraryDynDirs = map ST.pack $ libraryDynDirs pkg,
       UDB.unitExtDepFrameworks = map ST.pack $ frameworks pkg,
       UDB.unitExtDepFrameworkDirs = map ST.pack $ frameworkDirs pkg,
       UDB.unitLinkerOptions  = map ST.pack $ ldOptions pkg,
       UDB.unitCcOptions      = map ST.pack $ ccOptions pkg,
       UDB.unitIncludes       = map ST.pack $ includes pkg,
       UDB.unitIncludeDirs    = map ST.pack $ includeDirs pkg,
       UDB.unitHaddockInterfaces = map ST.pack $ haddockInterfaces pkg,
       UDB.unitHaddockHTMLs   = map ST.pack $ haddockHTMLs pkg,
       UDB.unitExposedModules = map convertExposed (exposedModules pkg),
       UDB.unitHiddenModules  = hiddenModules pkg,
       UDB.unitIsIndefinite   = indefinite pkg,
       UDB.unitIsExposed      = exposed pkg,
       UDB.unitIsTrusted      = trusted pkg
    }
  where
    convertExposed (ExposedModule n reexport) = (n, reexport)

-- -----------------------------------------------------------------------------
-- Exposing, Hiding, Trusting, Distrusting, Unregistering are all similar
-- Prints the highest (hidden or exposed) version of a package

-- Describe

findPackage :: PackageArg -> [InstalledPackageInfo] -> [InstalledPackageInfo]
findPackage pkgarg = filter (pkgarg `matchesPkg`)

findPackagesByDB :: PackageDBStack -> PackageArg
                 -> IO [(PackageDB 'UDB.DbReadOnly, [InstalledPackageInfo])]
findPackagesByDB db_stack pkgarg
  = case [ (db, matched)
         | db <- db_stack,
           let matched = findPackage pkgarg $ packages db,
           not (null matched) ] of
        [] -> cannotFindPackage pkgarg Nothing
        ps -> return ps

cannotFindPackage :: PackageArg -> Maybe (PackageDB mode) -> IO a
cannotFindPackage pkgarg mdb = die $ "cannot find package " ++ pkg_msg pkgarg
  ++ maybe "" (\db -> " in " ++ location db) mdb
  where
    pkg_msg (Id pkgid)           = displayGlobPkgId pkgid
    pkg_msg (IUId ipid)          = display ipid
    pkg_msg (Substring pkgpat _) = "matching " ++ pkgpat

matches :: GlobPackageIdentifier -> MungedPackageId -> Bool
GlobPackageIdentifier pn `matches` pid'
  = (pn == mungedName pid')
ExactPackageIdentifier pid `matches` pid'
  = mungedName pid == mungedName pid' &&
    (mungedVersion pid == mungedVersion pid' || mungedVersion pid == nullVersion)

matchesPkg :: PackageArg -> InstalledPackageInfo -> Bool
(Id pid)        `matchesPkg` pkg = pid `matches` mungedId pkg
(IUId ipid)     `matchesPkg` pkg = ipid == installedUnitId pkg
(Substring _ m) `matchesPkg` pkg = m (display (mungedId pkg))

-- -----------------------------------------------------------------------------
-- Field

-- -----------------------------------------------------------------------------
brokenPackages :: [InstalledPackageInfo] -> [InstalledPackageInfo]
brokenPackages pkgs = snd (closure [] pkgs)
  where
    closure :: [InstalledPackageInfo] -> [InstalledPackageInfo] -> ([InstalledPackageInfo], [InstalledPackageInfo])
    closure pkgs' db_stack = go pkgs' db_stack
      where
        go avail not_avail =
          case partition (depsAvailable avail) not_avail of
              ([],        not_avail') -> (avail, not_avail')
              (new_avail, not_avail') -> go (new_avail ++ avail) not_avail'

        depsAvailable :: [InstalledPackageInfo] -> InstalledPackageInfo
                      -> Bool
        depsAvailable pkgs_ok pkg = null dangling
              where dangling = filter (`notElem` pids) (depends pkg)
                    pids = map installedUnitId pkgs_ok

-----------------------------------------------------------------------------
-- Sanity-check a new package config, and automatically build GHCi libs
-- if requested.

type ValidateError   = (Force,String)
type ValidateWarning = String

newtype Validate a = V { runValidate :: IO (a, [ValidateError],[ValidateWarning]) }

instance Functor Validate where
    fmap = liftM

instance Applicative Validate where
    pure a = V $ pure (a, [], [])
    (<*>) = ap

instance Monad Validate where
   m >>= k = V $ do
      (a, es, ws) <- runValidate m
      (b, es', ws') <- runValidate (k a)
      return (b,es++es',ws++ws')

verror :: Force -> String -> Validate ()
verror f s = V (return ((),[(f,s)],[]))

vwarn :: String -> Validate ()
vwarn s = V (return ((),[],["Warning: " ++ s]))

liftIO :: IO a -> Validate a
liftIO k = V (k >>= \a -> return (a,[],[]))

-- returns False if we should die
reportValidateErrors :: [ValidateError] -> [ValidateWarning]
                     -> String -> Maybe Force -> IO Bool
reportValidateErrors es ws prefix mb_force = do
  oks <- mapM report es
  return (and oks)
  where
    report (f,s)
      | Just force <- mb_force
      = if force >= f
           then return True
           else if f < CannotForce
                   then do reportError (prefix ++ s ++ " (use --force to override)")
                           return False
                   else do reportError err
                           return False
      | otherwise = do reportError err
                       return False
      where
             err = prefix ++ s

validatePackageConfig :: InstalledPackageInfo
                      -> PackageDBStack
                      -> Bool   -- multi_instance
                      -> Bool   -- update, or check
                      -> Force
                      -> IO ()
validatePackageConfig pkg db_stack
                      multi_instance update force = do
  (_,es,ws) <- runValidate $
                 checkPackageConfig pkg db_stack
                                    multi_instance update
  ok <- reportValidateErrors es ws
          (display (mungedId pkg) ++ ": ") (Just force)
  unless ok $ exitWith (ExitFailure 1)

checkPackageConfig :: InstalledPackageInfo
                      -> PackageDBStack
                      -> Bool   -- multi_instance
                      -> Bool   -- update, or check
                      -> Validate ()
checkPackageConfig pkg db_stack
                   multi_instance update = do
  checkPackageId pkg
  checkUnitId pkg db_stack update
  checkDuplicates db_stack pkg multi_instance update
  mapM_ (checkDep db_stack) (depends pkg)
  checkDuplicateDepends (depends pkg)
  mapM_ (checkDir False "import-dirs")  (importDirs pkg)
  mapM_ (checkDir True  "library-dirs") (libraryDirs pkg)
  mapM_ (checkDir True  "dynamic-library-dirs") (libraryDynDirs pkg)
  mapM_ (checkDir True  "include-dirs") (includeDirs pkg)
  mapM_ (checkDir True  "framework-dirs") (frameworkDirs pkg)
  mapM_ (checkFile   True "haddock-interfaces") (haddockInterfaces pkg)
  mapM_ (checkDirURL True "haddock-html")       (haddockHTMLs pkg)
  checkDuplicateModules pkg
  checkExposedModules db_stack pkg
  checkOtherModules pkg
  let has_code = Set.null (openModuleSubstFreeHoles (Map.fromList (instantiatedWith pkg)))
  when has_code $ mapM_ (checkHSLib (libraryDirs pkg ++ libraryDynDirs pkg)) (hsLibraries pkg)
  -- ToDo: check these somehow?
  --    extra_libraries :: [String],
  --    c_includes      :: [String],

-- When the package name and version are put together, sometimes we can
-- end up with a package id that cannot be parsed.  This will lead to
-- difficulties when the user wants to refer to the package later, so
-- we check that the package id can be parsed properly here.
checkPackageId :: InstalledPackageInfo -> Validate ()
checkPackageId ipi =
  let str = display (mungedId ipi) in
  case Cabal.eitherParsec str :: Either String MungedPackageId of
    Left e -> verror CannotForce ("invalid package identifier: '" ++ str ++ "': " ++ e)
    Right _ -> pure ()

checkUnitId :: InstalledPackageInfo -> PackageDBStack -> Bool
                -> Validate ()
checkUnitId ipi db_stack update = do
  let uid = installedUnitId ipi
  when (null (display uid)) $ verror CannotForce "missing id field"
  when (display uid /= compatPackageKey ipi) $
    verror CannotForce $ "installed package info from too old version of Cabal "
                      ++ "(key field does not match id field)"
  let dups = [ p | p <- allPackagesInStack db_stack,
                   installedUnitId p == uid ]
  when (not update && not (null dups)) $
    verror CannotForce $
        "package(s) with this id already exist: " ++
         unwords (map (display.installedUnitId) dups)

checkDuplicates :: PackageDBStack -> InstalledPackageInfo
                -> Bool -> Bool-> Validate ()
checkDuplicates db_stack pkg multi_instance update = do
  let
        pkgid = mungedId pkg
        pkgs  = packages (head db_stack)
  --
  -- Check whether this package id already exists in this DB
  --
  when (not update && not multi_instance
                   && (pkgid `elem` map mungedId pkgs)) $
       verror CannotForce $
          "package " ++ display pkgid ++ " is already installed"

  let
        uncasep = map toLower . display
        dups = filter ((== uncasep pkgid) . uncasep) (map mungedId pkgs)

  when (not update && not multi_instance
                   && not (null dups)) $ verror ForceAll $
        "Package names may be treated case-insensitively in the future.\n"++
        "Package " ++ display pkgid ++
        " overlaps with: " ++ unwords (map display dups)


checkDir, checkFile, checkDirURL :: Bool -> String -> FilePath -> Validate ()
checkDir  = checkPath False True
checkFile = checkPath False False
checkDirURL = checkPath True True

checkPath :: Bool -> Bool -> Bool -> String -> FilePath -> Validate ()
checkPath url_ok is_dir warn_only thisfield d
 | url_ok && ("http://"  `isPrefixOf` d
           || "https://" `isPrefixOf` d) = return ()

 | url_ok
 , Just d' <- stripPrefix "file://" d
 = checkPath False is_dir warn_only thisfield d'

   -- Note: we don't check for $topdir/${pkgroot} here. We rely on these
   -- variables having been expanded already, see mungePackagePaths.

 | isRelative d = verror ForceFiles $
                     thisfield ++ ": " ++ d ++ " is a relative path which "
                  ++ "makes no sense (as there is nothing for it to be "
                  ++ "relative to). You can make paths relative to the "
                  ++ "package database itself by using ${pkgroot}."
        -- relative paths don't make any sense; #4134
 | otherwise = do
   there <- liftIO $ if is_dir then doesDirectoryExist d else doesFileExist d
   unless there $
       let msg = thisfield ++ ": " ++ d ++ " doesn't exist or isn't a "
                                        ++ if is_dir then "directory" else "file"
       in
       if warn_only
          then vwarn msg
          else verror ForceFiles msg

checkDep :: PackageDBStack -> UnitId -> Validate ()
checkDep db_stack pkgid
  | pkgid `elem` pkgids = return ()
  | otherwise = verror ForceAll ("dependency \"" ++ display pkgid
                                 ++ "\" doesn't exist")
  where
        all_pkgs = allPackagesInStack db_stack
        pkgids = map installedUnitId all_pkgs

checkDuplicateDepends :: [UnitId] -> Validate ()
checkDuplicateDepends deps
  | null dups = return ()
  | otherwise = verror ForceAll ("package has duplicate dependencies: " ++
                                     unwords (map display dups))
  where
       dups = [ p | (p:_:_) <- group (sort deps) ]

checkHSLib :: [String] -> String -> Validate ()
checkHSLib dirs lib = do
  let filenames = ["lib" ++ lib ++ ".a",
                   "lib" ++ lib ++ "_p.a",
                   "lib" ++ lib ++ "-ghc" ++ GHC.Version.cProjectVersion ++ ".so",
                   "lib" ++ lib ++ "-ghc" ++ GHC.Version.cProjectVersion ++ ".dylib",
                            lib ++ "-ghc" ++ GHC.Version.cProjectVersion ++ ".dll"]
  b <- liftIO $ doesFileExistOnPath filenames dirs
  unless b $
    verror ForceFiles ("cannot find any of " ++ show filenames ++
                       " on library path")

doesFileExistOnPath :: [FilePath] -> [FilePath] -> IO Bool
doesFileExistOnPath filenames paths = anyM doesFileExist fullFilenames
  where fullFilenames = [ path </> filename
                        | filename <- filenames
                        , path <- paths ]

-- | Perform validation checks (module file existence checks) on the
-- @hidden-modules@ field.
checkOtherModules :: InstalledPackageInfo -> Validate ()
checkOtherModules pkg = mapM_ (checkModuleFile pkg) (hiddenModules pkg)

-- | Perform validation checks (module file existence checks and module
-- reexport checks) on the @exposed-modules@ field.
checkExposedModules :: PackageDBStack -> InstalledPackageInfo -> Validate ()
checkExposedModules db_stack pkg =
  mapM_ checkExposedModule (exposedModules pkg)
  where
    checkExposedModule (ExposedModule modl reexport) = do
      let checkOriginal = checkModuleFile pkg modl
          checkReexport = checkModule "module reexport" db_stack pkg
      maybe checkOriginal checkReexport reexport

-- | Validates the existence of an appropriate @hi@ file associated with
-- a module.  Used for both @hidden-modules@ and @exposed-modules@ which
-- are not reexports.
checkModuleFile :: InstalledPackageInfo -> ModuleName -> Validate ()
checkModuleFile pkg modl =
      -- there's no interface file for GHC.Prim
      unless (modl == ModuleName.fromString "GHC.Prim") $ do
      let files = [ ModuleName.toFilePath modl <.> extension
                  | extension <- ["hi", "p_hi", "dyn_hi" ] ]
      b <- liftIO $ doesFileExistOnPath files (importDirs pkg)
      unless b $
         verror ForceFiles ("cannot find any of " ++ show files)

-- | Validates that @exposed-modules@ and @hidden-modules@ do not have duplicate
-- entries.
-- ToDo: this needs updating for signatures: signatures can validly show up
-- multiple times in the @exposed-modules@ list as long as their backing
-- implementations agree.
checkDuplicateModules :: InstalledPackageInfo -> Validate ()
checkDuplicateModules pkg
  | null dups = return ()
  | otherwise = verror ForceAll ("package has duplicate modules: " ++
                                     unwords (map display dups))
  where
    dups = [ m | (m:_:_) <- group (sort mods) ]
    mods = map exposedName (exposedModules pkg) ++ hiddenModules pkg

-- | Validates an original module entry, either the origin of a module reexport
-- or the backing implementation of a signature, by checking that it exists,
-- really is an original definition, and is accessible from the dependencies of
-- the package.
-- ToDo: If the original module in question is a backing signature
-- implementation, then we should also check that the original module in
-- question is NOT a signature (however, if it is a reexport, then it's fine
-- for the original module to be a signature.)
checkModule :: String
            -> PackageDBStack
            -> InstalledPackageInfo
            -> OpenModule
            -> Validate ()
checkModule _ _ _ (OpenModuleVar _) = error "Impermissible reexport"
checkModule field_name db_stack pkg
    (OpenModule (DefiniteUnitId def_uid) definingModule) =
  let definingPkgId = unDefUnitId def_uid
      mpkg = if definingPkgId == installedUnitId pkg
              then Just pkg
              else PackageIndex.lookupUnitId ipix definingPkgId
  in case mpkg of
      Nothing
           -> verror ForceAll (field_name ++ " refers to a non-existent " ++
                               "defining package: " ++
                                       display definingPkgId)

      Just definingPkg
        | not (isIndirectDependency definingPkgId)
           -> verror ForceAll (field_name ++ " refers to a defining " ++
                               "package that is not a direct (or indirect) " ++
                               "dependency of this package: " ++
                                       display definingPkgId)

        | otherwise
        -> case find ((==definingModule).exposedName)
                     (exposedModules definingPkg) of
            Nothing ->
              verror ForceAll (field_name ++ " refers to a module " ++
                               display definingModule ++ " " ++
                               "that is not exposed in the " ++
                               "defining package " ++ display definingPkgId)
            Just (ExposedModule {exposedReexport = Just _} ) ->
              verror ForceAll (field_name ++ " refers to a module " ++
                               display definingModule ++ " " ++
                               "that is reexported but not defined in the " ++
                               "defining package " ++ display definingPkgId)
            _ -> return ()
  where
    all_pkgs = allPackagesInStack db_stack
    ipix     = PackageIndex.fromList all_pkgs

    isIndirectDependency pkgid = fromMaybe False $ do
      thispkg  <- graphVertex (installedUnitId pkg)
      otherpkg <- graphVertex pkgid
      return (Graph.path depgraph thispkg otherpkg)
    (depgraph, _, graphVertex) =
      PackageIndex.dependencyGraph (PackageIndex.insert pkg ipix)

checkModule _ _ _ (OpenModule (IndefFullUnitId _ _) _) =
    -- TODO: add some checks here
    return ()


-- ---------------------------------------------------------------------------
-- expanding environment variables in the package configuration
expandEnvVars :: String -> Force -> IO String
expandEnvVars str0 force = go str0 ""
 where
   go "" acc = return $! reverse acc
   go ('$':'{':str) acc | (var, '}':rest) <- break close str
        = do value <- lookupEnvVar var
             go rest (reverse value ++ acc)
        where close c = c == '}' || c == '\n' -- don't span newlines
   go (c:str) acc
        = go str (c:acc)

   lookupEnvVar :: String -> IO String
   lookupEnvVar "pkgroot"    = return "${pkgroot}"    -- these two are special,
   lookupEnvVar "pkgrooturl" = return "${pkgrooturl}" -- we don't expand them
   lookupEnvVar nm =
        catchIO (System.Environment.getEnv nm)
           (\ _ -> do dieOrForceAll force ("Unable to expand variable " ++
                                        show nm)
                      return "")

-----------------------------------------------------------------------------

getProgramName :: IO String
getProgramName = fmap (`withoutSuffix` ".bin") getProgName
   where str `withoutSuffix` suff
            | suff `isSuffixOf` str = take (length str - length suff) str
            | otherwise             = str

die :: String -> IO a
die = dieWith 1

dieWith :: Int -> String -> IO a
dieWith ec s = do
  prog <- getProgramName
  reportError (prog ++ ": " ++ s)
  exitWith (ExitFailure ec)

dieOrForceAll :: Force -> String -> IO ()
dieOrForceAll ForceAll s = ignoreError s
dieOrForceAll _other s   = dieForcible s

warn :: String -> IO ()
warn = reportError

-- send info messages to stdout
infoLn :: String -> IO ()
infoLn = putStrLn

info :: String -> IO ()
info = putStr

ignoreError :: String -> IO ()
ignoreError s = reportError (s ++ " (ignoring)")

reportError :: String -> IO ()
reportError s = do hFlush stdout; hPutStrLn stderr s

dieForcible :: String -> IO ()
dieForcible s = die (s ++ " (use --force to override)")


catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

tryIO :: IO a -> IO (Either Exception.IOException a)
tryIO = Exception.try

-- removeFileSave doesn't throw an exceptions, if the file is already deleted
removeFileSafe :: FilePath -> IO ()
removeFileSafe fn =
  removeFile fn `catchIO` \ e ->
    unless (isDoesNotExistError e) $ ioError e

-- | Turn a path relative to the current directory into a (normalised)
-- absolute path.
absolutePath :: FilePath -> IO FilePath
absolutePath path = normalise . (</> path) <$> getCurrentDirectory
