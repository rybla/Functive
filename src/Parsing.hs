module Parsing
( parseRegex
) where

import           Data.ByteString.Internal
import qualified Regex.RE2                as RE2

newtype Parser a = Parser (ByteString -> Maybe (a, ByteString))

instance Functor Parser where
  fmap f (Parser pa) s =
    case pa s of
      Nothing     -> None
      Just (a, s) -> (f a, s)


parseRegex :: RE2.Pattern -> Parser ByteString
parseRegex = error "unimplemented"
