{-# LANGUAGE NamedFieldPuns #-}
module MediaMarker
  ( MediaMarker(MediaMarker, isbn, asin, registeredAt, readingStatus, comment)
  ) where

import           Data.ByteString.UTF8 (fromString, toString)
import           Data.Csv             (FromNamedRecord (parseNamedRecord), (.:))
import           Data.Time.Format     (defaultTimeLocale, parseTimeOrError)
import           Data.Time.LocalTime  (LocalTime)
import           Prelude              hiding (asin)

import           ReadingStatus        (ReadingStatus (Read, Reading, Yet))

data MediaMarker =
  MediaMarker
  { isbn          :: String
  , asin          :: String
  , registeredAt  :: LocalTime
  , readingStatus :: ReadingStatus
  , comment       :: String
  } deriving (Show)

instance FromNamedRecord MediaMarker where
  parseNamedRecord m = do
    isbn <- m .: fromString "ISBN/JAN"
    asin <- m .: fromString "ASIN（アマゾン商品コード）"
    registeredAt <- parseTime . toString <$> m .: fromString "登録日"
    comment <- m .: fromString "コメント"

    s <- m .: fromString "状態"
    readingStatus <- case s :: String of
      "未読" -> pure Yet
      "読中" -> pure Reading
      "読了" -> do
        readDayString <- m .: fromString "読了日"
        pure $ Read $ if null readDayString then Nothing else Just $ read readDayString
      _    -> fail "parseNameRecord"

    pure MediaMarker { isbn, asin, registeredAt, readingStatus, comment }

parseTime :: String -> LocalTime
parseTime = parseTimeOrError False defaultTimeLocale "%Y/%m/%d %T"
