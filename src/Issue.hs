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
{-# LANGUAGE FlexibleInstances #-}

module Issue where

import Data.Aeson
import Data.Text
import Data.Monoid ((<>))
import Printer
import qualified Data.ByteString.Lazy as B
import qualified Milestone as M
import qualified Label as L
import qualified User as U
import qualified Data.Text.IO as T

data Response = Response {
    url :: Text,
    state :: Text,
    title :: Text,
    body :: Text,
    user :: U.User,
    assignee :: Maybe U.User,
    labels :: [L.Label],
    milestone :: Maybe M.Milestone,
    comments :: Int
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
        <*> v .: "comments"

isSingle :: Int -> Bool
isSingle 0 =
    False
isSingle _ =
    True

path :: Bool -> String -> String -> Int -> String
path True "" "" 0 =
    "/user/issues"
path True _ _ _ =
    error "An option that can not be specify together is specified."
path _ "" "" 0 =
    "/issues"
path _ "" "" _ =
    error "An option that can not be specify together is specified."
path _ "" _ _ =
    error "Please pass the owner name."
path _ _ "" _ =
    error "Please pass the repository name."
path _ owner repo 0 =
    "/repos/" ++ owner ++ "/" ++ repo ++ "/issues"
path _ owner repo number =
    "/repos/" ++ owner ++ "/" ++ repo ++ "/issues/" ++ show number

handleResponse :: B.ByteString -> Bool -> IO ()
handleResponse json True =
    case decode json :: Maybe Response of
      Nothing -> error "Failed to parse json."
      Just d -> Printer.print d
handleResponse json _ =
    case decode json :: Maybe [Response] of
      Nothing -> error "Failed to parse json."
      Just d -> Printer.print d

instance Print [Response] where
    print [] = return ()
    print (response : responses) = do
        T.putStrLn $ formattedTitle
        Printer.print responses
        where
            milestoneTitle = case milestone response of
                            Just m -> M.toString m <> " "
                            Nothing -> ""
            labelTitles = case L.toString $ labels response of
                            Just s -> s <> " "
                            Nothing -> ""
            commentsTitle = pack $ " [" <> show ( comments response ) <> "] "
            formattedTitle = milestoneTitle <> labelTitles <> ( title response ) <> commentsTitle

instance Print Response where
    print response = do
        -- TODO
        T.putStrLn ""
