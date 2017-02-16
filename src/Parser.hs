{-# LANGUAGE OverloadedStrings #-}
module Parser where
import System.IO (stdin, stdout)
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Control.Monad.IO.Class (liftIO)
import Data.Conduit.Binary (sourceHandle, sinkHandle, sourceFile)
import Data.Conduit.Attoparsec (conduitParser, conduitParserEither, PositionRange)
import Control.Applicative ((<|>))
import Data.Conduit (($$), ($=), (=$), ConduitM)
import qualified Data.Conduit.Combinators as C
import Data.Conduit.Text (decode, utf8, encode)
import Text.PrettyPrint.Leijen (text, putDoc, (<>), empty, Pretty(..), fillCat)
import Data.Char (isControl)
import qualified Data.Conduit.List as CL
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import Data.Attoparsec.Text.Lazy
       (Parser, string, takeText, takeLazyText, char, many1, skipSpace,
        endOfLine, count, takeTill)

instance Pretty T.Text where
  pretty t = text $ T.unpack t

instance Pretty Play where
  pretty (Play h xs) = (text $ T.unpack h) <> (fillCat $ (\t -> pretty t) <$> xs)

instance Pretty Task where
  pretty (Task n xs) = (text $ T.unpack n) <> (fillCat $ (\t -> pretty t) <$> xs)

instance Pretty TaskOutput where
  pretty (TaskOutput st yaml) = pretty st <> (text $ T.unpack yaml)

instance Pretty TaskState where
  pretty OK = text "ok"
  pretty Skipping = text "skipping"
  pretty _ = text "whatever"

data Play = Play T.Text [Task]
  deriving (Show)

data Task =
  Task {name :: T.Text
       ,taskoutput :: [TaskOutput]}
  deriving (Show)

data TaskOutput =
  TaskOutput TaskState
             SomeYAML
  deriving (Show)

type SomeYAML = T.Text

data TaskState
  = OK
  | Skipping
  | Changed
  | Failed
  deriving (Show)

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
  output <- parseTaskOutput
  pure $ Task { name = T.concat [name, timestamp], taskoutput = output}

parseTaskOutput :: Parser [TaskOutput]
parseTaskOutput = many1 $ TaskOutput <$> parseTaskState <*> takeText

parseTaskState :: Parser TaskState
parseTaskState = (skipSpace *> string "ok:" >> pure OK) <|>
  (skipSpace *> string "skipping:" >> pure Skipping) <|>
  (skipSpace *> string "changed:" >> pure Changed) <|>
  (skipSpace *> string "failed:" >> pure Failed)

conduitPretty = C.map (\(_, p) -> pretty p)

main :: IO ()
main = runResourceT $ sourceHandle stdin $= decode utf8 $= conduitParser parsePlay $$ conduitPretty $= C.print
