{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
  ( main
    )
where

import Options.Applicative.Simple
import qualified Paths_asmJsonCpp
import RIO
import RIO.Process

-- | Command line arguments
data Options
  = Options
      { optionsVerbose :: !Bool
        }

data App
  = App
      { appLogFunc :: !LogFunc,
        appProcessContext :: !ProcessContext,
        appOptions :: !Options
        -- Add other app-specific configuration information here
        }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})

run :: RIO App ()
run = do
  logInfo "We're inside the application!"

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
      empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app =
          App
            { appLogFunc = lf,
              appProcessContext = pc,
              appOptions = options
              }
     in runRIO app run
