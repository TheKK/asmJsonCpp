{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
  ( main,
  )
where

import Control.Monad.Trans.Except
import Options.Applicative.Simple
import qualified Paths_asmJsonCpp
import RIO
import RIO.Process
import RIO.Writer
import SubCommand.Cpp
import SubCommand.Example

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  }

data App = App
  { appLogFunc :: !LogFunc,
    appProcessContext :: !ProcessContext,
    appOptions :: !Options
    -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})

subCommands :: ExceptT (RIO App ()) (Writer (Mod CommandFields (RIO App ()))) ()
subCommands = do
  cppSubCmd
  exampleSubCmd

main :: IO ()
main = do
  (options, runSubCommand) <-
    simpleOptions
      $(simpleVersion Paths_asmJsonCpp.version)
      "Tool that takes your job but makes you happy."
      ( join
          [ "AsmJsonCpp let you use high level yet simple language to solve tedious jsoncpp problem. ",
            "Try the 'example' command to see how the language looks like."
          ]
      )
      ( Options
          <$> switch
            ( long "verbose"
                <> short 'v'
                <> help "Verbose output?"
            )
      )
      subCommands
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app =
          App
            { appLogFunc = lf,
              appProcessContext = pc,
              appOptions = options
            }
     in runRIO app runSubCommand
