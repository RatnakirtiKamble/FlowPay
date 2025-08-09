-- src/Models/User.hs
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.User where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), (.:), Value(Object))
import Data.Text (Text)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Servant.Auth.Server (ToJWT, FromJWT) -- Added this import

-- | The full User model as stored in the database.
data User = User
  { userId           :: Int
  , userName         :: Text
  , userEmail        :: Text
  , userPasswordHash :: Text -- This field is for internal use only.
  , userBalance      :: Double
  } deriving (Eq, Show, Generic)

-- Custom ToJSON instance to AVOID exposing the password hash.
instance ToJSON User where
  toJSON user = object
    [ "userId"    .= userId user
    , "userName"  .= userName user
    , "userEmail" .= userEmail user
    , "userBalance" .= userBalance user
    ]

-- Custom FromJSON instance. We don't expect to receive a password hash.
instance FromJSON User where
  parseJSON (Object v) = User
    <$> v .: "userId"
    <*> v .: "userName"
    <*> v .: "userEmail"
    <*> pure "" -- Set password hash to empty, as it's never received.
    <*> v .: "userBalance"
  parseJSON _ = mempty

-- Add the necessary instances for servant-auth
instance ToJWT User
instance FromJWT User

-- We still derive ToRow and FromRow to map to the database table.
instance FromRow User
instance ToRow User

-- | A public-facing view of a User, without the password hash.
data PublicUser = PublicUser
  { publicUserId    :: Int
  , publicUserName  :: Text
  , publicUserEmail :: Text
  , publicUserBalance :: Double
  } deriving (Eq, Show, Generic)

instance ToJSON PublicUser

-- | A function to convert a full User to a public-facing one.
toPublicUser :: User -> PublicUser
toPublicUser user = PublicUser
    { publicUserId = userId user
    , publicUserName = userName user
    , publicUserEmail = userEmail user
    , publicUserBalance = userBalance user
    }

-- | Request body for user registration.
data RegistrationRequest = RegistrationRequest
  { reqName     :: Text
  , reqEmail    :: Text
  , reqPassword :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON RegistrationRequest

-- | Request body for user login.
data LoginRequest = LoginRequest
  { loginEmail    :: Text
  , loginPassword :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON LoginRequest

-- | Response body for the balance check endpoint.
data BalanceResponse = BalanceResponse
  { currentBalance :: Double
  } deriving (Eq, Show, Generic)

instance ToJSON BalanceResponse
