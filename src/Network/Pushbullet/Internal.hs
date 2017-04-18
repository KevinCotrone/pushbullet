module Network.Pushbullet.Internal
    (
      PushSecret (..)
    ) where

import           Network.Wreq

newtype PushSecret = PushSecret {
  unPushSecret :: Auth
} deriving (Eq, Show)
