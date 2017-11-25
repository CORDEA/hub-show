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
-- date  : 2017-11-25

{-# LANGUAGE OverloadedStrings #-}

module PullRequest where

import Data.Aeson
import Data.Text
import Data.Monoid ((<>))
import Data.ByteString.Lazy
import qualified Milestone as M
import qualified User as U
import qualified Data.Text.IO as T

data Response = Response {
    url :: Text,
    state :: Text,
    title :: Text,
    body :: Text,
    user :: U.User,
    assignee :: Maybe U.User,
    milestone :: Maybe M.Milestone
    } deriving Show

instance FromJSON Response where
    parseJSON (Object v) = Response
        <$> v .: "url"
        <*> v .: "state"
        <*> v .: "title"
        <*> v .: "body"
        <*> v .: "user"
        <*> v .: "assignee"
        <*> v .: "milestone"

parseJson :: ByteString -> Maybe [Response]
parseJson json =
    decode json :: Maybe [Response]

path :: String -> String -> String
path "" "" =
    error ""
path "" _ =
    error ""
path _ "" =
    error ""
path owner repo =
    "/repos/" ++ owner ++ "/" ++ repo ++ "/pulls"

print :: [Response] -> IO ()
print [] = return ()
print (response : responses) = do
    T.putStrLn $ formattedTitle
    PullRequest.print responses
    where
        milestoneTitle = case milestone response of
                           Just m -> M.toString m <> " "
                           Nothing -> ""
        formattedTitle = milestoneTitle <> title response
