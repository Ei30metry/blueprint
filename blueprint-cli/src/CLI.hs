-- module CLI (parseSearchEnv) where
module CLI ( commandLineInterface ) where

import           CLI.Parser
import Development.Blueprint.Types
import Options.Applicative


commandLineInterface :: IO SearchEnv
commandLineInterface = do
  execParser (info (parseSearchEnv <**> helper)
                          (fullDesc <> progDesc "Print recursive declerations of an entity"
                           <> header "A different approach to showing outgoing call hierarchy for Haskell source code."))
