{-# LANGUAGE OverloadedStrings #-}

module Network.Pushbullet.Types(
    Title
  , Body
  , PushNote(..)
  , PushList(..)
  , ListItem(..)
  , PushLink(..)
  , PushSecret
  , pushSecret
  , DeviceId(..)
  , Pushable(..)
  , PushBullet(..)
) where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.ByteString
import           Data.Text
import           Network.Pushbullet.Internal
import           Network.Wreq

newtype DeviceId = DeviceId { unDeviceId :: Text } deriving (Eq, Show)
-- | A message to send
newtype PushBullet = PushBullet { unPushable :: Value } deriving (Eq, Show)


class Pushable a where
  toPush :: a -> PushBullet


type Title = Text
type Body = Text

-- | A note to push to a user
data PushNote = PushNote {
  noteTitle :: Title       -- ^ Title of the note
, noteBody  :: Body        -- ^ Body of the note
} deriving (Eq, Show)

instance ToJSON PushNote where
  toJSON (PushNote title body) = object ["type" .= ("note" :: Text), "title" .= title, "body" .= body]

instance FromJSON PushNote where
  parseJSON (Object v) = PushNote <$>
                          v .: "title" <*>
                          v .: "body"
  parseJSON _ = mzero

instance Pushable PushNote where
  toPush = PushBullet . toJSON

-- | A list to push to a user
data PushList = PushList {
  listTitle :: Title      -- ^ Title of the list
, listItems :: [ListItem] -- ^ List Items
} deriving (Eq, Show)

instance ToJSON PushList where
  toJSON (PushList title items) = object ["type" .= ("list" :: Text), "title" .= title, "items" .= items]

instance FromJSON PushList where
  parseJSON (Object v) = PushList <$>
                          v .: "title" <*>
                          v .: "items"
  parseJSON _ = mzero

instance Pushable PushList where
  toPush = PushBullet . toJSON

-- | A single item that is part of a list
data ListItem = ListItem {
  listItemChecked :: Bool -- ^ If the list item is active
, listItemText    :: Text -- ^ Text of the list item
} deriving (Eq, Show)

instance ToJSON ListItem where
  toJSON (ListItem checked text) = object ["checked" .= checked, "text" .= text]

instance FromJSON ListItem where
  parseJSON (Object v) = ListItem <$>
                          v .: "checked" <*>
                          v .: "text"
  parseJSON _ = mzero

-- | A link to push to a user
data PushLink = PushLink {
  linkTitle :: Title      -- ^ Title of the link
, linkBody  :: Body       -- ^ Message to go along with the link
, linkUrl   :: Text       -- ^ URL for the link
} deriving (Eq, Show)


instance ToJSON PushLink where
  toJSON (PushLink title body url) = object ["type" .= ("link" :: Text), "title" .= title, "body" .= body, "url" .= url]

instance FromJSON PushLink where
  parseJSON (Object v) = PushLink <$>
                          v .: "title" <*>
                          v .: "body" <*>
                          v .: "url"
  parseJSON _ = mzero

instance Pushable PushLink where
  toPush = PushBullet . toJSON

-- | Used to send push messages

pushSecret :: ByteString     -- ^ Access Token found in Account Settings
              -> PushSecret
pushSecret = PushSecret . oauth2Bearer
