

{-# LANGUAGE DeriveGeneric #-}

module ClockifyMapped
    ( TimeInterval(..)
    , TimeEntry(..)
    , convertTimeEntry
    ) where

import GHC.Generics ( Generic )
import Data.Time ( defaultTimeLocale, parseTimeM, UTCTime )
import qualified Clockify


data TimeInterval = TimeInterval
  { start :: UTCTime
  , end :: UTCTime
  , duration :: String
  -- , id :: String
  } deriving(Show, Generic)

data TimeEntry = TimeEntry
  { id :: String
  , description :: String
  , timeInterval :: TimeInterval
  } deriving (Show, Generic)

-- Mappings

parseUtcTime :: String -> Either String UTCTime
parseUtcTime dateString =
  let
    format = "%Y-%m-%dT%H:%M:%SZ"  -- format matching your date string
    utcTimeMaybe = parseTimeM True defaultTimeLocale format dateString :: Maybe UTCTime
  in case utcTimeMaybe of
    Nothing -> Left "Failed to parse UTCTime."
    Just x -> Right x

convertTimeInterval :: Clockify.TimeInterval -> Either String TimeInterval
convertTimeInterval timeIntervalInput =
  do
    startUtcTime <- parseUtcTime $ Clockify.start timeIntervalInput
    endUtcTime <- parseUtcTime $ Clockify.end timeIntervalInput
    pure $ ClockifyMapped.TimeInterval {
      ClockifyMapped.start = startUtcTime
      , ClockifyMapped.end = endUtcTime
      , ClockifyMapped.duration = Clockify.duration timeIntervalInput
    }

convertTimeEntry :: Clockify.TimeEntry -> Either String TimeEntry
convertTimeEntry timeEntryInput =
  do
    convertedTimeInterval <- convertTimeInterval $ Clockify.timeInterval timeEntryInput
    pure ClockifyMapped.TimeEntry {
      ClockifyMapped.id = Clockify.id timeEntryInput
      , ClockifyMapped.description = Clockify.description timeEntryInput
      , ClockifyMapped.timeInterval = convertedTimeInterval
    }
