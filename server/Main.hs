{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import AsmJsonCpp.Compiler
import AsmJsonCpp.Parser
import qualified Data.Text.Lazy as L
import Log
import Log.Backend.StandardOutput
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Log
import Network.Wai.Middleware.Cors
import RIO
import qualified RIO.ByteString.Lazy as BL
import Servant

type Api = "api" :> "v1" :> "compile" :> QueryParam' '[Required, Strict] "query" L.Text :> Get '[PlainText] L.Text

server :: Server Api
server = compile_v1

compile_v1 :: L.Text -> Servant.Handler L.Text
compile_v1 input =
  case parseAsmJson input of
    Left err -> throwError $ err400 {errBody = BL.fromStrict . encodeUtf8 . L.toStrict $ err}
    Right asm -> return $ compileToFullCppSourceCode asm

api :: Proxy Api
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = do
  withSimpleStdOutLogger $ \l -> do
    logger <- runLogT "server" l $ do
      mkApplicationLoggerWith defaultOptions

    Warp.runEnv 5587 $
      app
        & simpleCors
        & logger
