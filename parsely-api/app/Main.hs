{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Servant
import Data.Aeson
import qualified Data.Text as Txt
import GHC.Generics
import System.Environment (getEnv)
import Network.Wai (Middleware, Request, Response, ResponseReceived, pathInfo)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, simpleHeaders, CorsResourcePolicy(..))
import Clockify (getClockifyTimeEntriesIO)
import Data.Time
import Prelude
import qualified ClockifyMapped
import qualified Data.Map as Map
import Data.Foldable as Foldable ( Foldable(foldl) )
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TextEncoding
import Servant.Swagger
import qualified Data.Swagger as Swagger hiding (Response)
import Servant.Swagger.UI
import Control.Lens ( (&), (.~), (?~) )

-- Aggregation model
type RangeModelDateString = String
data RangeModel = RangeModel
  { rangeModelDateString :: RangeModelDateString
  , rangeModelTotalDiff :: NominalDiffTime
  , rangeModelTotalDiffPretty :: Txt.Text
  } deriving(Show, Generic)
instance ToJSON RangeModel

-- Define a data type for the JSON structure
data EchoData = EchoData { message :: Txt.Text } deriving (Show, Generic)
instance ToJSON EchoData
instance FromJSON EchoData

-- OpenAPI
instance Swagger.ToSchema RangeModel
instance Swagger.ToSchema EchoData

-- Define the API
type API =
    "api" :> 
    (
        "echo" :> ReqBody '[JSON] EchoData :> Post '[JSON] EchoData
        :<|> "defaultOvertime" :> Get '[JSON] (Map.Map RangeModelDateString (NominalDiffTime, Txt.Text))
        :<|> Get '[PlainText] Txt.Text
    )

type APIWithoutCors = SwaggerSchemaUI "swagger-ui" "swagger.json"


-- Define the server implementation
defaultServer :: Server API
defaultServer = echoHandler
              :<|> defaultOvertimeHandler
              :<|> rootHandler
  where
    echoHandler :: EchoData -> Handler EchoData
    echoHandler = return

    defaultOvertimeHandler :: Handler (Map.Map RangeModelDateString (NominalDiffTime, Txt.Text))
    defaultOvertimeHandler = do
      result <- liftIO overtimeEntriesIO
      case result of
        Left err -> throwError $ err500 { errBody = LBS.fromStrict (TextEncoding.encodeUtf8 $ Txt.pack err) }
        Right success -> return success

    rootHandler :: Handler Txt.Text
    rootHandler = return "Server works 🎉"

swaggerDoc :: Swagger.Swagger
swaggerDoc = toSwagger (Proxy :: Proxy API)
              & Swagger.info . Swagger.title       .~ "Parsely API"
              & Swagger.info . Swagger.version     .~ "1.0"
              & Swagger.info . Swagger.description ?~ "Clockify Extension Tools"

serverWithoutCors :: Server APIWithoutCors
serverWithoutCors = swaggerSchemaUIServer swaggerDoc

-- CORS policy with whitelisted origins

corsPolicyDefault :: Middleware
corsPolicyDefault = cors (const $ Just corsResourcePolicy)
  where
    corsResourcePolicy = simpleCorsResourcePolicy
      { corsOrigins = Just ([ "http://localhost:4200" ], True)
      , corsMethods = ["GET", "POST", "OPTIONS"]
      , corsRequestHeaders = simpleHeaders  -- or specify other headers you want
      , corsExposedHeaders = Nothing
      , corsMaxAge = Just 3600  -- cache preflight response for 1 hour
      , corsVaryOrigin = False
      , corsRequireOrigin = True
      , corsIgnoreFailures = False
      }

corsPolicyWithoutOrigins :: Middleware
corsPolicyWithoutOrigins = cors (const $ Just corsResourcePolicy)
  where
    corsResourcePolicy = simpleCorsResourcePolicy { corsRequireOrigin = False }

routeToApp :: Application -> Application -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
routeToApp appWithCors appWithoutCors request respondInput =
  case pathInfo request of
    ("api" : _) -> appWithCors request respondInput
    _ -> appWithoutCors request respondInput
    

-- UTIL

reverseArgs :: (a -> b -> c) -> (b -> a -> c)
reverseArgs f = \b a -> f a b


-- Calculate the difference of time interval spanning multiple days (Optionally).
convertToRangeModel :: ClockifyMapped.TimeInterval -> RangeModel
convertToRangeModel timeInterval =
  let
    anchorTimeInterval = ClockifyMapped.start timeInterval
    nominalDiffTime = diffUTCTime (ClockifyMapped.end timeInterval) anchorTimeInterval
    aggregateModel = aggregateStep anchorTimeInterval nominalDiffTime
  in
    aggregateModel

-- Aggregation

collectToMap :: RangeModel -> Map.Map RangeModelDateString [RangeModel] -> Map.Map RangeModelDateString [RangeModel]
collectToMap newModel =
  Map.alter updateFunction (rangeModelDateString newModel)
  where
    updateFunction Nothing = Just [newModel]
    updateFunction (Just xs) = Just (newModel:xs)

collectHoursPerDay :: [RangeModel] -> Map.Map RangeModelDateString [RangeModel]
collectHoursPerDay = Foldable.foldl (reverseArgs collectToMap) Map.empty

aggregateHoursPerDay :: Map.Map RangeModelDateString [RangeModel] -> Map.Map RangeModelDateString NominalDiffTime
aggregateHoursPerDay = fmap ((Foldable.foldl (+) 0) . (fmap rangeModelTotalDiff))


-- Presentation

formatDate :: UTCTime -> String
formatDate = formatTime defaultTimeLocale "%Y-%m-%d"

-- Converts NominalDiffTime to a sensible human-readable string
formatDiffTime :: NominalDiffTime -> Txt.Text
formatDiffTime diffTime =
    let totalSecs = floor diffTime :: Integer
        (days, remainSecs1) = totalSecs `divMod` 86400
        (hours, remainSecs2) = remainSecs1 `divMod` 3600
        (mins, secs) = remainSecs2 `divMod` 60
        showPart value unit = if value > 0 then Txt.pack (show value ++ unit) else Txt.empty
        parts = [showPart days "d", showPart hours "h", showPart mins "m", showPart secs "s"]
    in Txt.unwords $ filter (/= Txt.empty) parts

-- Assume that the time entries are at the same date.
aggregateStep :: UTCTime -> NominalDiffTime -> RangeModel
aggregateStep utcTime nominalDiffTime =
  RangeModel (formatDate utcTime) nominalDiffTime (formatDiffTime nominalDiffTime)

present :: Map.Map RangeModelDateString NominalDiffTime -> Map.Map RangeModelDateString (NominalDiffTime, Txt.Text)
present = fmap (\nominalDiffTime -> (nominalDiffTime, formatDiffTime nominalDiffTime))


-- Policy

applyWorkingHoursPolicy :: Map.Map RangeModelDateString NominalDiffTime -> Map.Map RangeModelDateString NominalDiffTime
applyWorkingHoursPolicy rangeModelMap =
  let
    workingHoursInSec = 8 * 60 * 60
    overtimeDiff = fmap (\nominalDiffTime -> nominalDiffTime - workingHoursInSec) rangeModelMap
    validOvertimeDiff = Map.filter (> 0) overtimeDiff -- Second iteration. Detect as anomaly.
  in
    validOvertimeDiff


overtimeEntriesIO :: IO (Either String (Map.Map RangeModelDateString (NominalDiffTime, Txt.Text)))
overtimeEntriesIO = do
  userId <- getEnv "USER_ID"
  workspaceId <- getEnv "WORKSPACE_ID"
  clockifyApiKey <- getEnv "CLOCKIFY_API_KEY"
  timeEntriesResult <- getClockifyTimeEntriesIO userId workspaceId clockifyApiKey
  let processor = present . applyWorkingHoursPolicy . aggregateHoursPerDay . collectHoursPerDay . fmap (convertToRangeModel . ClockifyMapped.timeInterval)
  let converter = mapM ClockifyMapped.convertTimeEntry
  let processedEntries = timeEntriesResult >>= (fmap processor . converter)
  return processedEntries

-- Start the server
main :: IO ()
main = do
  envPortStr <- getEnv "API_PORT"
  let envPort = read envPortStr :: Int
  putStrLn $ "Server running in port " ++ show envPort

  let appWithCors = corsPolicyDefault $ serve (Proxy :: Proxy API) defaultServer
  let appWithoutCors = corsPolicyWithoutOrigins $ serve (Proxy :: Proxy APIWithoutCors) serverWithoutCors
  let combinedApp request _respond = routeToApp appWithCors appWithoutCors request _respond

  run envPort combinedApp
