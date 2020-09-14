{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import AsmJsonCpp.Compiler
import AsmJsonCpp.Parser
import qualified Data.Text.Lazy as L
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Cors
import RIO
import qualified RIO.ByteString.Lazy as BL
import Servant

type Api = "api" :> "v1" :> "compile" :> QueryParam' '[Required, Strict] "query" L.Text :> Get '[PlainText] L.Text

server :: Server Api
server = compile_v1

compile_v1 :: L.Text -> Servant.Handler L.Text
compile_v1 input =
  case parseAsmJson $ input of
    Left err -> throwError $ err400 {errBody = BL.fromStrict . encodeUtf8 . L.toStrict $ err}
    Right asm -> return $ compileToFullCppSourceCode asm

api :: Proxy Api
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main =
  Warp.runEnv 1234 $
    app
      & simpleCors
      & logStdoutDev
