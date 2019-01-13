{-# LANGUAGE NamedFieldPuns #-}
module Booklog (Booklog(Booklog, itemId, isbn, readingStatus, memo, registeredAt)) where

import qualified Data.ByteString.UTF8 as S (fromString)
import           Data.Csv             (ToField (toField), ToRecord (toRecord),
                                       record)
import           Data.Default.Class   (Default (def))
import           Data.Time.Calendar   (fromGregorian)
import           Data.Time.LocalTime  (LocalTime (LocalTime, localDay, localTimeOfDay),
                                       TimeOfDay (TimeOfDay))

import           ReadingStatus        (ReadingStatus (Read, Reading, Yet))

data Booklog =
  Booklog
  { serviceId     :: ServiceId
  , itemId        :: String
  , isbn          :: String
  , category      :: String
  , evaluation    :: String
  , readingStatus :: ReadingStatus
  , review        :: String
  , tag           :: String
  , memo          :: String
  , registeredAt  :: LocalTime
  } deriving (Show)

instance Default Booklog where
  def =
    Booklog
    { serviceId     = AmazonCoJp
    , itemId        = def
    , isbn          = def
    , category      = "MediaMarkerから"
    , evaluation    = def
    , readingStatus = Yet
    , review        = def
    , tag           = def
    , memo          = def
    , registeredAt  = LocalTime { localDay = fromGregorian 2019 1 1, localTimeOfDay = TimeOfDay 0 0 0 }
    }

instance ToRecord Booklog where
  toRecord Booklog { serviceId, itemId, isbn, category, evaluation, readingStatus, review, tag, memo, registeredAt } =
    record
      [ toField serviceId                               -- サービスID
      , toField itemId                                  -- アイテムID
      , toField isbn                                    -- 13桁ISBN
      , toField category                                -- カテゴリ
      , toField evaluation                              -- 評価
      , toField $ booklogReadingStatus readingStatus    -- 読書状況
      , toField review                                  -- レビュー
      , toField tag                                     -- タグ
      , toField memo                                    -- 読書メモ(非公開)
      , toField $ show registeredAt                     -- 登録日時
      , toField $ show <$> booklogReadDay readingStatus -- 読了日
      ]

instance ToField ServiceId where
  toField AmazonCoJp = S.fromString "1"
  toField AmazonCom  = S.fromString "2"

booklogReadingStatus :: ReadingStatus -> String
booklogReadingStatus Yet      = "読みたい"
booklogReadingStatus Reading  = "いま読んでる"
booklogReadingStatus (Read _) = "読み終わった"

booklogReadDay :: ReadingStatus -> Maybe LocalTime
booklogReadDay (Read (Just d)) = Just $ LocalTime { localDay = d, localTimeOfDay = TimeOfDay 0 0 0 }
booklogReadDay _ = Nothing

data ServiceId =
  AmazonCoJp | AmazonCom
  deriving (Show)
