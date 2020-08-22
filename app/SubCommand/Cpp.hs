{-# LANGUAGE OverloadedStrings #-}

module SubCommand.Cpp
  ( cppSubCmd,
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
import RIO
import RIO.Writer
import System.IO (putStrLn)
import Text.Megaparsec.Error

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

    typeForwardDecls = catMaybes . fmap cppTypeRenderForwardDeclaration $ resultTypes
    typeDefs = catMaybes $ fmap cppTypeRenderDefinition $ resultTypes

    resultTypes = toList $ compileToResultTypes asm cvNone

    printFunctionBody = L.putStrLn . cppFnRender . compileToCppFn "from_json" $ asm

cppSubCmd :: ExceptT (RIO app ()) (Writer (Mod CommandFields (RIO app ()))) ()
cppSubCmd =
  addCommand "cpp" "generate cpp source code to validate & parse JSON value" cppRun $
    optional $
      strArgument
        ( metavar "QUERY"
            <> help "query string for JSON parsing, if not present then read from stdin"
        )

cppRun :: Maybe String -> RIO app ()
cppRun input = liftIO $ do
  input' <-
    fromMaybe
      (L.fromStrict <$> T.getContents)
      (return . L.pack <$> input)

  case parseAsmJson input' of
    Left err -> putStrLn . errorBundlePretty $ err
    Right asm -> printGeneratedCppSourceCode asm
