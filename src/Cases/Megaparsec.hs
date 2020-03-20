module Cases.Megaparsec where

import Data.Void
import Text.Megaparsec (MonadParsec)

type CodeParsing = MonadParsec Void String
