{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Golden4 where

import           Control.Lens ( makeLenses )

import           GHC.Generics ( Generic )


data InfoConf = Info { _name  :: String
                     , _email :: String } deriving (Eq, Show, Generic)


newtype DaemonConfig = DConf { _runDaemon :: Bool } deriving (Show, Eq, Generic)


data OptHeader = OptH { _meditation :: Bool
                      , _alcohol    :: Bool
                      , _cigarette  :: Bool } deriving (Eq, Show)

newtype GenTemplate = GenTemp { _generateTemplate :: Bool } deriving (Eq, Show, Generic)


data TemplateConf = TempConf { _genTemplate     :: GenTemplate
                             , _optionalHeaders :: OptHeader } deriving (Eq, Show, Generic)


data ReportConf = RepConf { _emailReport          :: Bool
                          , _emailReportFrequency :: Int } deriving (Eq, Show, Generic)

data Config = Config { _info     :: InfoConf
                     , _daemon   :: DaemonConfig
                     , _template :: TemplateConf
                     , _report   :: ReportConf } deriving (Show, Eq, Generic)

makeLenses ''InfoConf
makeLenses ''DaemonConfig
makeLenses ''OptHeader
makeLenses ''GenTemplate
makeLenses ''TemplateConf
makeLenses ''ReportConf
makeLenses ''Config
