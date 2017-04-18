{-# LANGUAGE OverloadedStrings          #-}
module Network.Pushbullet (
    sendPush
  , module PBT
)where

import           Control.Lens                ((&), (?~))
import           Data.Aeson
import qualified Data.HashMap.Strict as H
import           Data.Monoid
import           Network.Pushbullet.Internal
import           Network.Pushbullet.Types    as PBT
import           Network.Wreq

-- | Send a push to a given device
-- if device is
sendPush :: PushSecret       -- ^ Access Token for pushbullet
         -> Maybe DeviceId   -- ^ Send to a specific device. If Nothing it will broadcast
         -> PushBullet       -- ^ Message to push
         -> IO ()            -- ^ Will eventuall return some sort of error/success
sendPush secret d p = do
  _ <- postWith
        (defaults & auth ?~ (unPushSecret secret))
        "https://api.pushbullet.com/v2/pushes"
        (addDeviceIden d $ toJSON p)
  return ()

addDeviceIden :: Maybe DeviceId -> Value -> Value
addDeviceIden (Just devId) (Object o) =
  Object $
    o <> H.fromList ["device_iden" .= unDeviceId devId]
addDeviceIden _ v = v
