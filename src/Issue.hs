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

module Issue where

import Data.Aeson
import Data.Text
import Data.ByteString.Lazy
import qualified Milestone as M
import qualified Data.Text.IO as T

data Response = Response {
    url :: Text,
    state :: Text,
    title :: Text,
    body :: Text,
    user :: User,
    assignee :: Maybe User,
    labels :: [Label],
    milestone :: M.Milestone
    } deriving Show

data User = User {
    login :: Text
    } deriving Show

data Label = Label {
    name :: Text
    } deriving Show

instance FromJSON Response where
    parseJSON (Object v) = Response
        <$> v .: "url"
        <*> v .: "state"
        <*> v .: "title"
        <*> v .: "body"
        <*> v .: "user"
        <*> v .: "assignee"
        <*> v .: "labels"
        <*> v .: "milestone"

instance FromJSON User where
    parseJSON (Object v) = User
        <$> v .: "login"

instance FromJSON Label where
    parseJSON (Object v) = Label
        <$> v .: "name"

parseJson :: ByteString -> Maybe [Response]
parseJson json =
    decode json :: Maybe [Response]

path :: Bool -> String -> String -> String
path True _ _ =
    "/user/issues"
path _ "" "" =
    "/issues"
path _ "" _ =
    error ""
path _ _ "" =
    error ""
path _ owner repo =
    "/repos/" ++ owner ++ "/" ++ repo ++ "/issues"

print :: [Response] -> IO ()
print [] = return ()
print (response : responses) = do
    T.putStrLn $ title response
    Issue.print responses
