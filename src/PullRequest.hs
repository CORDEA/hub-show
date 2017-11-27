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
{-# LANGUAGE FlexibleInstances #-}

module PullRequest where

import Data.Aeson
import Data.Text
import Data.Monoid ((<>))
import Printer
import qualified Data.ByteString.Lazy as B
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

parseJson :: B.ByteString -> Maybe [Response]
parseJson json =
    decode json :: Maybe [Response]

path :: String -> String -> Int -> String
path owner repo 0 =
    "/repos/" ++ owner ++ "/" ++ repo ++ "/pulls"
path owner repo number =
    "/repos/" ++ owner ++ "/" ++ repo ++ "/pulls/" ++ show number

handleResponse :: B.ByteString -> Bool -> IO ()
handleResponse json True =
    case decode json :: Maybe Response of
      Nothing -> error "Failed to parse json."
      Just d -> Printer.print d
handleResponse json _ =
    case decode json :: Maybe [Response] of
      Nothing -> error "Failed to parse json."
      Just d -> Printer.print d

formattedTitle :: Response -> Text
formattedTitle response =
    milestoneTitle <> title response
    where
        milestoneTitle = case milestone response of
                           Just m -> M.toString m <> " "
                           Nothing -> ""


instance Print [Response] where
    print [] = return ()
    print (response : responses) = do
        T.putStrLn $ formattedTitle response
        Printer.print responses

instance Print Response where
    print response = do
        T.putStrLn $ formattedTitle response <> "\n"
        T.putStrLn $ body response
