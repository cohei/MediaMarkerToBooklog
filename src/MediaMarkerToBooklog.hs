{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
module MediaMarkerToBooklog (main) where

import           Data.ByteString.Lazy.UTF8 (fromString, toString)
import           Data.Csv                  (decodeByName, encode)
import           Data.Default.Class        (def)
import           Data.Foldable             (toList)
import           Prelude                   hiding (asin)
import           System.IO                 (TextEncoding, hSetEncoding,
                                            mkTextEncoding, stdin, stdout)

import           Booklog                   (Booklog (isbn, itemId, memo, readingStatus, registeredAt))
import           MediaMarker               (MediaMarker (MediaMarker, asin, comment, isbn, readingStatus, registeredAt))

main :: IO ()
main = do
  hSetEncoding stdin =<< shiftJis
  hSetEncoding stdout =<< shiftJis
  interact $ either id (toString . encode . toList . fmap convert . snd) . decodeByName . fromString

shiftJis :: IO TextEncoding
shiftJis = mkTextEncoding "Shift_JIS"

convert :: MediaMarker -> Booklog
convert MediaMarker { isbn, asin, registeredAt, readingStatus, comment } =
  def { itemId = asin, isbn, registeredAt, readingStatus, memo = comment }
