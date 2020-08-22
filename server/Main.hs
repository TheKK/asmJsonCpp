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
import RIO.Writer

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  }

data App = App
  { appLogFunc :: !LogFunc,
    appOptions :: !Options
    -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

subCommands :: ExceptT () (Writer (Mod CommandFields ())) ()
subCommands = empty

main :: IO ()
main = do
  (options, ()) <-
    simpleOptions
      $(simpleVersion Paths_asmJsonCpp.version)
      "Header for command line arguments"
      "Program description, also for command line arguments"
      ( Options
          <$> switch
            ( long "verbose"
                <> short 'v'
                <> help "Verbose output?"
            )
      )
      subCommands
  lo <- logOptionsHandle stderr (optionsVerbose options)
  withLogFunc lo $ \lf ->
    let app =
          App
            { appLogFunc = lf,
              appOptions = options
            }
     in runRIO app $ logInfo "bye"
