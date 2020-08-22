{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
  ( main,
  )
where

import AsmJsonCpp.Asm
import AsmJsonCpp.Compiler
import AsmJsonCpp.CppExpr
import AsmJsonCpp.Parser
import Control.Monad.Trans.Except
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import Options.Applicative.Simple
import qualified Paths_asmJsonCpp
import RIO
import RIO.Process
import RIO.Writer
import System.IO (putStrLn)
import Text.Megaparsec.Error

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

runAsmJsonCommand :: L.Text -> IO ()
runAsmJsonCommand input = do
  case parseAsmJson input of
    Left err -> putStrLn . errorBundlePretty $ err
    Right asm -> printGeneratedCppSourceCode asm

printGeneratedCppSourceCode :: AsmJson -> IO ()
printGeneratedCppSourceCode asm = do
  tryPrintingResultTypeDeclaration
  tryPrintingResultTypeDefinition
  printFunctionBody
  where
    tryPrintingResultTypeDeclaration =
      for_ typeForwardDecls $ \fd -> do
        L.putStrLn fd
        L.putStrLn ""

    tryPrintingResultTypeDefinition = case typeDefs of
      [] -> do
        L.putStrLn . L.unlines $
          [ "// Wow, primitive types rocks right?",
            "// Let's use them everywhere and get confused.",
            "// It's string! It's user name! It's email address as well!! What a lovely day."
          ]
      defs -> for_ defs L.putStrLn

    typeForwardDecls = (maybeToList . cppTypeRenderForwardDeclaration) =<< resultTypes
    typeDefs = (maybeToList . cppTypeRenderDefinition) =<< resultTypes

    resultTypes = toList $ compileToResultTypes asm cvNone

    printFunctionBody = L.putStrLn . cppFnRender . compileToCppFn "YOUR_FUNC" $ asm

subCommands :: ExceptT (RIO App ()) (Writer (Mod CommandFields (RIO App ()))) ()
subCommands = do
  cppSubCmd
  where
    cppSubCmd =
      addCommand "cpp" "generate cpp source code to validate & parse JSON value" cppRun $
        optional $
          strArgument
            ( metavar "QUERY"
                <> help "query string for JSON parsing, if not present then read from stdin"
            )

    cppRun :: Maybe String -> RIO App ()
    cppRun input = liftIO $ do
      input' <-
        fromMaybe
          (L.fromStrict <$> T.getContents)
          (return . L.pack <$> input)

      runAsmJsonCommand input'

main :: IO ()
main = do
  (options, runSubCommand) <-
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
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app =
          App
            { appLogFunc = lf,
              appProcessContext = pc,
              appOptions = options
            }
     in runRIO app runSubCommand
