module Termite.Interface (
    parse,
    TSyntax,
) where

import Swarm.Language.Pipeline
import Swarm.Language.Syntax.Pattern (TSyntax)
import Data.Text (Text)

parse :: Text -> Either Text (Maybe TSyntax)
parse = processTerm