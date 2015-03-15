module Network.Pushbullet (
    Pushable(..)
  , PushBullet
  , sendPush
)where

import Network.Wreq
import Control.Lens

import Network.Pushbullet.Types
import Network.Pushbullet.Internal


sendPush :: (Pushable p) => PushSecret          -- ^ Access Token for pushbullet
                            -> Maybe DeviceId   -- ^ Send to a specific device. If Nothing it will broadcast
                            -> p                -- ^ Message to push
                            -> IO ()            -- ^ Will eventuall return some sort of error/success
sendPush secret _ p = do
  _ <- postWith (defaults & auth ?~ (unPushSecret secret)) "https://api.pushbullet.com/v2/pushes" (unPushable $ toPush p)
  return ()