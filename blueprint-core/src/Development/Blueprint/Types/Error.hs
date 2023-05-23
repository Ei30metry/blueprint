-- |

module Development.Blueprint.Types.Error where

data CompEnvError = RnDeclError deriving Eq

instance Show CompEnvError where
  show RnDeclError = "GHC couldn't renamed declerations"
