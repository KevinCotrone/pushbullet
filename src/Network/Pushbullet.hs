module Network.Pushbullet (
  Pushable(..)
, PushBullet
)where

import Data.Aeson
import Network.Wreq
import Control.Lens

import Network.Pushbullet.Types
import Network.Pushbullet.Internal


sendPush :: (Pushable p) => PushSecret -> DeviceId -> p -> IO ()
sendPush secret devId p = do
  _ <- postWith (defaults & auth ?~ (unPushSecret secret)) "https://api.pushbullet.com/v2/pushes" (unPushable $ toPush p)
  return ()