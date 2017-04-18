{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Network.Pushbullet.Types(
    Title
  , Body
  , FileName
  , FileType
  , FileUrl
  , DeviceId(..)
  , PushBullet(..)
  , pushSecret
) where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.ByteString
import           Data.String
import           Data.Text
import           Network.Pushbullet.Internal
import           Network.Wreq

newtype DeviceId = DeviceId {
  unDeviceId :: Text
} deriving (Eq, Show, IsString)

type Title = Text
type Body = Text
type FileName = Text
type FileType = Text
type FileUrl = Text

data PushBullet =
    PushNote Title Body
    -- ^ A note with a title and body
  | PushLink Title Body Text
    -- ^ A link with a message and title
  | PushFile Body FileName FileType FileUrl
    -- ^ A File with a message, name, type, and url

instance ToJSON PushBullet where
  toJSON (PushNote title body) =
    object [
        "type" .= ("note" :: Text)
      , "title" .= title, "body" .= body
    ]
  toJSON (PushLink title body url) =
    object [
        "type" .= ("link" :: Text)
      , "title" .= title
      , "body" .= body
      , "url" .= url
    ]
  toJSON (PushFile body fileName fileType fileUrl) =
    object [
      "type" .= ("file" :: Text)
    , "body" .= body
    , "file_name" .= fileName
    , "file_type" .= fileType
    , "file_url" .= fileUrl
    ]

-- | Generate the PushSecret from a ByteString
pushSecret :: ByteString     -- ^ Access Token found in Account Settings
           -> PushSecret
pushSecret = PushSecret . oauth2Bearer
