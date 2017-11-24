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
import qualified Data.Text.IO as T

data Response = Response {
    url :: Text,
    state :: Text,
    title :: Text,
    body :: Text,
    user :: User,
    assignee :: User
    } deriving Show

data User = User {
    login :: Text
    } deriving Show

instance FromJSON Response where
    parseJSON (Object v) = Response
        <$> v .: "url"
        <*> v .: "state"
        <*> v .: "title"
        <*> v .: "body"
        <*> v .: "user"
        <*> v .: "assignee"

instance FromJSON User where
    parseJSON (Object v) = User
        <$> v .: "login"

parseJson :: ByteString -> Maybe [Response]
parseJson json =
    decode json :: Maybe [Response]

print :: [Response] -> IO ()
print [] = return ()
print (response : responses) =
    T.putStrLn $ title response
