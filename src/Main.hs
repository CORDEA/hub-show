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
import Options.Applicative
import IssueOpts
import PullOpts
import qualified Data.Text.IO as T
import qualified Data.ByteString.Char8 as B
import qualified Control.Exception as E
import qualified PullRequest
import qualified Issue
import qualified Option

baseUrl :: String
baseUrl = "https://api.github.com"

buildUrl :: String -> String -> String -> String
buildUrl _ path "" =
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
        Nothing -> error "Illegal url."
        Just req ->
            setHeaders req { method = "GET" } token
    where
        url = buildUrl token path param

sendRequest :: Request -> IO ( Response ByteString )
sendRequest =
    withManager . httpLbs

fetchIssues :: Option.CommonOpts -> IssueOpts -> IO ()
fetchIssues commonOpts opts =
    sendRequest ( gitHubRequest token path "" ) >>= \response -> do
        case Issue.parseJson $ responseBody response of
            Nothing -> error "Failed to parse json."
            Just response -> return response
        >>= Issue.print
    where
        ( Option.CommonOpts token ) = commonOpts
        ( IssueOpts owner repo isOwn ) = opts
        path = Issue.path isOwn owner repo

fetchPullRequests :: Option.CommonOpts -> PullOpts -> IO ()
fetchPullRequests commonOpts opts =
    sendRequest ( gitHubRequest token path "" ) >>= \response -> do
        case PullRequest.parseJson $ responseBody response of
          Nothing -> error "Failed to parse json."
          Just response -> return response
    >>= PullRequest.print
    where
        ( Option.CommonOpts token ) = commonOpts
        ( PullOpts owner repo ) = opts
        path = PullRequest.path owner repo

main :: IO ()
main = do
    execParser Option.parserInfo >>= \parser -> do
        case parser of
          Option.Args opts ( Option.Issue issue ) ->
              fetchIssues opts issue
          Option.Args opts ( Option.Pull pull ) ->
              fetchPullRequests opts pull
