{-|
Module      : Profile
Description : P atoms - Profile data types and operations
Copyright   : (c) 2024
License     : BSD3
Maintainer  : example@example.com
Stability   : experimental

This module defines Profile data types and functions for updating profiles.
P atoms represent profile/person data in the AP Statistics fundamental concepts.
-}

module Profile
    ( Profile(..)
    , ProfileId
    , updateProfile
    , createProfile
    , validateProfile
    ) where

import Data.Text (Text)
import Data.Time (UTCTime)

-- | Unique identifier for profiles
type ProfileId = Text

-- | Profile data structure representing a person/student in the system
data Profile = Profile
    { profileId :: ProfileId
    , profileName :: Text
    , profileEmail :: Text
    , profileCreatedAt :: UTCTime
    , profileUpdatedAt :: UTCTime
    -- Add more profile fields as needed
    } deriving (Show, Eq)

-- | Create a new profile with the given parameters
createProfile :: ProfileId -> Text -> Text -> UTCTime -> Profile
createProfile pid name email timestamp = Profile
    { profileId = pid
    , profileName = name
    , profileEmail = email
    , profileCreatedAt = timestamp
    , profileUpdatedAt = timestamp
    }

-- | Update an existing profile with new information
updateProfile :: Profile -> Text -> Text -> UTCTime -> Profile
updateProfile profile newName newEmail timestamp = profile
    { profileName = newName
    , profileEmail = newEmail
    , profileUpdatedAt = timestamp
    }

-- | Validate a profile to ensure it meets business rules
validateProfile :: Profile -> Either Text Profile
validateProfile profile
    | profileName profile == "" = Left "Profile name cannot be empty"
    | profileEmail profile == "" = Left "Profile email cannot be empty"
    | otherwise = Right profile
