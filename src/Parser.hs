{-# LANGUAGE OverloadedStrings #-}
module Parser where
import           Control.Applicative              ((<|>))
import           Control.Monad.Trans.Resource     (runResourceT)
import qualified Data.Aeson                       as JSON
import           Data.Attoparsec.ByteString.Char8 (Parser, anyChar, char, many',
                                                   many1, manyTill, option,
                                                   skipSpace, string, takeTill)
import           Data.Attoparsec.Combinator       (lookAhead)
import qualified Data.ByteString.Char8            as B
import qualified Data.ByteString.Lazy.Char8       as BL
import           Data.Char                        (isControl)
import           Data.Conduit                     (($$), ($=))
import           Data.Conduit.Attoparsec          (conduitParser)
import           Data.Conduit.Binary              (sourceHandle)
import qualified Data.Conduit.Combinators         as C
import qualified Data.HashMap.Lazy                as HM
import qualified Data.Text                        as T
import qualified Data.Vector                      as V
import           Prelude                          hiding (take)
import           System.IO                        (stdin)
import           Text.PrettyPrint.ANSI.Leijen     (Pretty (..), cat, fillCat,
                                                   green, softbreak, text,
                                                   yellow, (<>))

instance Pretty Play where
  pretty (Play h xs) = (text $ B.unpack h) <> softbreak <> (fillCat $ (\t -> softbreak <> pretty t) <$> xs)

instance Pretty Task where
  pretty (Task n xs) = (text $ B.unpack n) <> softbreak <> (cat $ (\t -> pretty t) <$> xs)

instance Pretty TaskOutput where
  pretty (TaskOutput st h json) = case JSON.decode (BL.fromStrict json) :: Maybe JSON.Value of
    Just val -> pretty st <> pretty (B.unpack h) <> pretty val
    Nothing -> pretty st <> pretty (B.unpack h ++ B.unpack json)

instance Pretty T.Text where
  pretty t = pretty $ T.unpack t

instance Pretty JSON.Value where
  pretty (JSON.Object x) = prettyList $ HM.toList x
  pretty (JSON.Array x) = prettyList $ V.toList x
  pretty (JSON.String x) = pretty $ T.unpack x
  pretty whatever = pretty $ show whatever

instance Pretty TaskState where
  pretty OK = text "ok:"
  pretty Changed = yellow $ text "changed"
  pretty Skipping = yellow $ text "skipping"
  pretty _ = text "whatever"

data Play = Play B.ByteString [Task]
  deriving (Show)

data Task =
  Task {name       :: B.ByteString
       ,taskoutput :: [TaskOutput]}
  deriving (Show, Eq)

data TaskOutput =
    TaskOutput TaskState
               B.ByteString
               SomeJSON
    deriving (Show,Eq)

type SomeJSON = B.ByteString

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
  n <- takeTill isControl
  _ <- char '\n'
  timestamp <- takeTill isControl
  verbose <- many' $ string "<" *> takeTill isControl
  output <- many' parseTaskOutput
  pure $ Task { name = B.concat [n, "\n", timestamp, B.concat verbose], taskoutput = output}

parseTaskOutput :: Parser TaskOutput
parseTaskOutput = do
    s <- parseTaskState
    h <- string " [" *> manyTill anyChar (string "]")
    y <- (string "=> (" *> takeTill isControl) <|>
        lookAhead (string " => " *> takeTill isControl) <|>
        takeTill isControl
    pure $ TaskOutput s (B.pack h) y

itemInfo :: Parser B.ByteString
itemInfo = (\x y -> x `B.cons` y) <$> string " => " <*> takeTill isControl

parseJSON :: Parser (Maybe JSON.Value)
parseJSON = option Nothing (Just <$> (string " => " *> JSON.json))

parseTaskState :: Parser TaskState
parseTaskState =
    (skipSpace *> string "ok:" >> pure OK) <|>
    (skipSpace *> string "skipping:" >> pure Skipping) <|>
    (skipSpace *> string "changed:" >> pure Changed) <|>
    (skipSpace *> string "failed:" >> pure Failed)

conduitPretty = C.map (\(_, p) -> pretty p)

main :: IO ()
main = runResourceT $ sourceHandle stdin $= conduitParser parsePlay $$ conduitPretty $= C.print
