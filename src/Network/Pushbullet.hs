module Network.Pushbullet (
    Pushable(..)
  , PushBullet
  , sendPush
)where

import Network.Wreq
import Control.Lens

import Network.Pushbullet.Types
import Network.Pushbullet.Internal


sendPush :: (Pushable p) => PushSecret -> Maybe DeviceId -> p -> IO ()
sendPush secret _ p = do
  _ <- postWith (defaults & auth ?~ (unPushSecret secret)) "https://api.pushbullet.com/v2/pushes" (unPushable $ toPush p)
  return ()