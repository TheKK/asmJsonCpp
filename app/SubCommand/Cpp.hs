{-# LANGUAGE OverloadedStrings #-}

module SubCommand.Cpp
  ( cppSubCmd,
  )
where

import AsmJsonCpp.Asm
import AsmJsonCpp.Compiler
import AsmJsonCpp.Parser hiding (Parser)
import Control.Monad.Trans.Except
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import Options.Applicative.Simple
import RIO
import RIO.Writer

type Args = Maybe String

printGeneratedCppSourceCode :: AsmJson -> IO ()
printGeneratedCppSourceCode asm = L.putStrLn $ compileToFullCppSourceCode asm

cppSubCmd :: ExceptT (RIO app ()) (Writer (Mod CommandFields (RIO app ()))) ()
cppSubCmd =
  addCommand
    "cpp"
    "Generate cpp code to validate and parse JSON value."
    cppRun
    cppParse

cppParse :: Parser Args
cppParse =
  optional $
    strArgument
      ( metavar "QUERY"
          <> help "Query string that describes JSON format, if not present then read it from stdin"
      )

cppRun :: Args -> RIO app ()
cppRun input = liftIO $ do
  input' <-
    fromMaybe
      (L.fromStrict <$> T.getContents)
      (return . L.pack <$> input)

  case parseAsmJson input' of
    Left err -> L.putStrLn err
    Right asm -> printGeneratedCppSourceCode asm
