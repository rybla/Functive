module Parsing
( parseRegex
) where

import           Control.Monad.Trans
import           Control.Monad.Trans.State as State
import           Data.ByteString           as BS
import           Data.ByteString.Internal
import           Data.Vector               as Vector
import           Regex.RE2                 as RE2

type Parser a = StateT ByteString Maybe a

parseRegex :: Pattern -> Parser ByteString
parseRegex p = State.StateT $ \str ->
  case RE2.match p str 0 (BS.length str) (Just AnchorStart) 1 of
    Nothing -> Nothing
    Just m ->
      let result = case matchGroup m 0 of { Just g -> g } in
      let rest = BS.drop (BS.length result) result in
      Just (result, rest)
