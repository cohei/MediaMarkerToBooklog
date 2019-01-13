module ReadingStatus (ReadingStatus(Yet, Reading, Read)) where

import           Data.Time.Calendar (Day)

data ReadingStatus =
  Yet | Reading | Read (Maybe Day)
  deriving (Show)
