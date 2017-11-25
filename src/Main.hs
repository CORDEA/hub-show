-- Copyright 2017 Yoshihiro Tanaka
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--   http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
--
-- Author: Yoshihiro Tanaka <contact@cordea.jp>
-- date  : 2017-11-23

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Conduit
import Network.HTTP.Types.Header
import Network.HTTP.Base ( defaultUserAgent )
import Network.URI ( parseURI )
import Data.ByteString.Lazy
import Data.Maybe
import qualified Data.Text.IO as T
import qualified Data.ByteString.Char8 as B
import qualified Control.Exception as E
import qualified Issue
import qualified Option

baseUrl :: String
baseUrl = "https://api.github.com"

buildUrl :: String -> String -> String -> String
buildUrl token path "" =
    baseUrl ++ path
buildUrl token path param =
    baseUrl ++ path ++ "?" ++ param

setHeaders :: Request -> String -> Request
setHeaders req token =
    req {
        requestHeaders =
            ( hAccept, B.pack "application/vnd.github.full+json" ) :
            ( hAuthorization, B.pack tokenHeader ) :
            ( hUserAgent, B.pack defaultUserAgent ) :
            requestHeaders req
    }
    where
        tokenHeader = "token " ++ token

gitHubRequest :: String -> String -> String -> Request
gitHubRequest token path param =
    case parseUrl url of
        Nothing -> error ""
        Just req ->
            setHeaders req { method = "GET" } token
    where
        url = buildUrl token path param

sendRequest :: Request -> IO ( Response ByteString )
sendRequest req =
    withManager $ httpLbs req

main :: IO ()
main = do
    execParser Option.parserInfo >>= \parser -> do
        case parser of
          Option.Args token ( Option.Issue issue ) ->
              print issue
              {- sendRequest ( gitHubRequest "" "/user/issues" "" ) >>= \response -> do -}
                  {- case Issue.parseJson $ responseBody response of -}
                      {- Nothing -> error "" -}
                      {- Just response -> return response -}
                  {- >>= Issue.print -}
          Option.Args token ( Option.Pull pull ) ->
              print pull
