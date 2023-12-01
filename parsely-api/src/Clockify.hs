
{-# LANGUAGE DeriveGeneric #-}

module Clockify
    ( getClockifyTimeEntriesIO
    , TimeEntry(..)
    , TimeInterval(..)
    ) where
import Data.Aeson (decode, FromJSON)
import Control.Exception (SomeException, catch, displayException)
import qualified Network.HTTP.Simple as Http
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.CaseInsensitive (mk)
import GHC.Generics (Generic)

-- Function to get time entries from Clockify
getClockifyTimeEntriesIO :: String -> String -> String -> IO (Either String [TimeEntry])
getClockifyTimeEntriesIO userId workspaceId clockifyApiKey = do
    let clockifyApiBaseEndpointUrl = "https://api.clockify.me/api/v1"
    let fullUrl = clockifyApiBaseEndpointUrl ++ "/workspaces/" ++ workspaceId ++ "/user/" ++ userId ++ "/time-entries"

    -- Initialize the request
    initialRequest <- Http.parseRequest fullUrl
    let myHeaderName = mk (S8.pack "X-Api-Key")
    let xApiKeyValue = [S8.pack clockifyApiKey]
    let request = Http.setRequestMethod (S8.pack "GET") $
                  Http.setRequestHeader myHeaderName xApiKeyValue initialRequest

    -- Perform the HTTP request and handle errors
    result <- catch (Right <$> Http.httpLBS request) handleException

    -- Handle the response
    case decodeToTimeEntries <$> result of
        Left err -> pure $ Left $ "Error: " ++ displayException err
        Right returnResponseMaybe ->
            pure $ case returnResponseMaybe of
                Nothing -> Left "Failed to decode return payload."
                Just timeEntries -> Right timeEntries


-- Exception handler
handleException :: SomeException -> IO (Either SomeException a)
handleException ex = return (Left ex)


decodeToTimeEntries :: Http.Response L8.ByteString -> Maybe [TimeEntry]
decodeToTimeEntries responseByString =
    decode (Http.getResponseBody responseByString) :: Maybe [TimeEntry]

data TimeInterval = TimeInterval
    { start :: String
    , end :: String
    , duration :: String
    } deriving(Show, Generic)

data TimeEntry = TimeEntry
    { id :: String
    , description :: String
    , timeInterval :: TimeInterval
    } deriving (Show, Generic)

instance FromJSON TimeInterval
instance FromJSON TimeEntry
