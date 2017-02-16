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
        endOfLine, count, takeTill, manyTill, anyChar, many')

instance Pretty T.Text where
  pretty t = text $ T.unpack t

instance Pretty Play where
  pretty (Play h xs) = (text $ T.unpack h) <> (fillCat $ (\t -> pretty t) <$> xs)

instance Pretty Task where
  pretty (Task n xs) = (text $ T.unpack n) <> (fillCat $ (\t -> pretty t) <$> xs)

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
  output <- many' parseTaskOutput
  pure $ Task { name = T.concat [name, timestamp], taskoutput = output}

parseTaskOutput :: Parser TaskOutput
parseTaskOutput = do
  s <- parseTaskState
  h <- string " [" *> manyTill anyChar (string "]")
  r <- takeTill isControl
  pure $ TaskOutput s (T.pack h) r


parseTaskState :: Parser TaskState
parseTaskState =
    (skipSpace *> string "ok:" >> pure OK) <|>
    (skipSpace *> string "skipping:" >> pure Skipping) <|>
    (skipSpace *> string "changed:" >> pure Changed) <|>
    (skipSpace *> string "failed:" >> pure Failed)

conduitPretty = C.map (\(_, p) -> pretty p)

main :: IO ()
main = runResourceT $ sourceHandle stdin $= decode utf8 $= conduitParser parsePlay $$ conduitPretty $= C.print
