{-# LANGUAGE OverloadedStrings #-}
module Parser where
import System.IO (stdin)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit.Binary (sourceHandle)
import Data.Conduit.Attoparsec (conduitParser)
import Control.Applicative ((<|>))
import Data.Conduit (($$), ($=))
import qualified Data.Conduit.Combinators as C
import Data.Conduit.Text (decode, utf8)
import Text.PrettyPrint.Leijen (text, (<>), Pretty(..), fillCat, cat, softbreak)
import Data.Char (isControl)
import qualified Data.Text as T
import Prelude hiding (take)
import Data.Attoparsec.Text.Lazy
       (Parser, string, char, many1, skipSpace,
        takeTill, manyTill, anyChar, many', option)

instance Pretty T.Text where
  pretty t = text $ T.unpack t

instance Pretty Play where
  pretty (Play h xs) = (text $ T.unpack h) <> softbreak <> (fillCat $ (\t -> softbreak <> pretty t) <$> xs)

instance Pretty Task where
  pretty (Task n xs) = (text $ T.unpack n) <> softbreak <> (cat $ (\t -> pretty t) <$> xs)

instance Pretty TaskOutput where
  pretty (TaskOutput st h yaml) = pretty st <> pretty h <> (text $ T.unpack yaml)

instance Pretty TaskState where
  pretty OK = text "ok"
  pretty Skipping = text "skipping"
  pretty _ = text "whatever"

data Play = Play T.Text [Task]
  deriving (Show)

data Task =
  Task {name :: T.Text
       ,taskoutput :: [TaskOutput]}
  deriving (Show, Eq)

data TaskOutput =
    TaskOutput TaskState
               T.Text
               SomeYAML
    deriving (Show,Eq)

type SomeYAML = T.Text

data TaskState
  = OK
  | Skipping
  | Changed
  | Failed
  deriving (Show, Eq)

parsePlay :: Parser Play
parsePlay = do
  skipSpace
  playname <- takeTill isControl
  skipSpace
  tasks <- many1 parseTask
  pure $ Play playname tasks

parseTask :: Parser Task
parseTask = do
  skipSpace
  name <- takeTill isControl
  char '\n'
  timestamp <- takeTill isControl
  verbose <- many' $ string "<" *> takeTill isControl
  output <- many' parseTaskOutput
  pure $ Task { name = T.concat [name, "\n", timestamp, T.concat verbose], taskoutput = output}

parseTaskOutput :: Parser TaskOutput
parseTaskOutput = do
  s <- parseTaskState
  h <- string " [" *> manyTill anyChar (string "]")
  y <- parseYAML
  case y of
    Just yaml -> pure $ TaskOutput s (T.pack h) yaml
    Nothing -> pure $ TaskOutput s (T.pack h) ""

parseYAML :: Parser (Maybe SomeYAML)
parseYAML = option Nothing (Just <$> (string " => " *> takeTill isControl))

parseTaskState :: Parser TaskState
parseTaskState =
    (skipSpace *> string "ok:" >> pure OK) <|>
    (skipSpace *> string "skipping:" >> pure Skipping) <|>
    (skipSpace *> string "changed:" >> pure Changed) <|>
    (skipSpace *> string "failed:" >> pure Failed)

conduitPretty = C.map (\(_, p) -> pretty p)

main :: IO ()
main = runResourceT $ sourceHandle stdin $= decode utf8 $= conduitParser parsePlay $$ conduitPretty $= C.print
