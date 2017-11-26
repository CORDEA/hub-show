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

module Option where

import Options.Applicative
import Data.Monoid
import PullOpts
import IssueOpts

data Args = Args CommonOpts Command
    deriving Show

data Command
    = Pull PullOpts
    | Issue IssueOpts
    deriving Show

data CommonOpts = CommonOpts
    {
        token :: String
    }
    deriving Show

commonOpts :: Parser CommonOpts
commonOpts = CommonOpts
    <$> strOption
        ( long "token"
        <> help "GitHub access token." )

pullParser :: Parser Command
pullParser = Pull <$> pullOpts

issueParser :: Parser Command
issueParser = Issue <$> issueOpts

parser :: Parser Args
parser = Args
    <$> commonOpts
    <*> hsubparser
        (
            command "pull"
            ( info pullParser
                ( progDesc "GitHub Pull requests" ) )
            <> command "issue"
            ( info issueParser
                ( progDesc "GitHub Issues" ) )
        )

parserInfo :: ParserInfo Args
parserInfo = info parser
    ( progDesc "GitHub show command" )
