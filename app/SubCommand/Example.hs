{-# LANGUAGE OverloadedStrings #-}

module SubCommand.Example
  ( exampleSubCmd,
  )
where

import Control.Monad.Trans.Except
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import Options.Applicative.Simple
import RIO
import RIO.Writer

type Args = ()

exampleSubCmd :: ExceptT (RIO app ()) (Writer (Mod CommandFields (RIO app ()))) ()
exampleSubCmd =
  addCommand
    "example"
    "print example query string for you"
    exampleRun
    exampleParse

exampleRun :: Args -> RIO app ()
exampleRun _ = liftIO printExampleQueryString

exampleParse :: Parser Args
exampleParse = pure ()

printExampleQueryString :: IO ()
printExampleQueryString =
  L.putStrLn . L.unlines $
    [ "// Support line comment!",
      "",
      "/* Support",
      " * block comment as well!",
      " */",
      "",
      "// Parse fields of object into struct.",
      "AsObj FieldsToStruct MagicStructName [",
      "  // You can parse int.",
      "  (i, AsInt),",
      "",
      "  // Parse string.",
      "  (s, AsString),",
      "",
      "  // Parse into vector.",
      "  (vector_of_i, AsArray EachElement AsInt),",
      "",
      "  // Parse item inside array.",
      "  (one_of_array, AsArray AtNth 42 AsString),",
      "",
      "  // Parse field of object.",
      "  (in_obj, AsObj AtField xyz AsInt),",
      "]"
    ]
