{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}

module Mailgun where

import           ClassyPrelude
import           Control.Monad.Trans.Either
import           Servant.API
import           Servant.Client
import           Data.Proxy
import           Data.Aeson
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import           Data.Time.Format (formatTime)

import           Servant.Common.Req

-- parseTime defaultTimeLocale "%a, %e %b %Y %T %Z" "Thu, 13 Oct 2011 18:02:00 GMT" :: Maybe UTCTime
-- formatTime defaultTimeLocale "%a, %e %b %Y %T %Z" someUTCTime

data Creds = Creds
  { credsDomain :: Text
  , credsToken  :: AuthToken
  } deriving Show

newtype AuthToken = AuthToken
  { unAuthToken :: Text
  } deriving (Show, FromText)

newtype EncodedAuthToken = EncodedAuthToken
  { unEncodedAuthToken :: Text
  } deriving (Show, ToText)

data TrackingClicks = YesTC | NoTC | HtmlOnly

instance Show TrackingClicks where
  show = \case
    YesTC     -> "yes"
    NoTC      -> "no"
    HtmlOnly  -> "htmlonly"

data YesOrNo = Yes | No

instance Show YesOrNo where
  show = \case
    Yes -> "yes"
    No  -> "no"

newtype DateTime = DateTime
  { unDateTime :: UTCTime }
  
instance Show DateTime where
  show = formatTime defaultTimeLocale "%a, %e %b %Y %T %Z" . unDateTime

data Message = Message
  { from              :: EmailAddress
  , to                :: [EmailAddress]
  , cc                :: [EmailAddress]
  , bcc               :: [EmailAddress]
  , subject           :: Text
  , text              :: Text
  , html              :: Maybe Text
  , o_tag             :: Maybe [Text]
  , o_campaign        :: Maybe Text
  , o_deliverytime    :: Maybe DateTime
  , o_dkim            :: Maybe YesOrNo
  , o_testmode        :: Maybe YesOrNo
  , o_tracking        :: Maybe YesOrNo
  , o_tracking_clicks :: Maybe TrackingClicks
  , o_tracking_opens  :: Maybe YesOrNo
  , o_headers         :: [(Text, Text)]
  , o_vars            :: [(Text, Text)]
  } deriving Show

-- instance ToJSON Message where
--   toJSON Message{..} = object $
--     [ "from"              .= from
--     , "to"                .= to
--     , "cc"                .= cc
--     , "bcc"               .= bcc
--     , "subject"           .= subject
--     , "text"              .= text
--     , "html"              .= html
--     , "o:tag"             .= o_tag
--     , "o:campaign"        .= o_campaign
--     , "o:deliverytime"    .= o_deliverytime
--     , "o:dkim"            .= o_dkim
--     , "o:testmode"        .= o_testmode
--     , "o:tracking"        .= o_tracking
--     , "o:tracking-clicks" .= o_tracking_clicks
--     , "o:tracking-opens"  .= o_tracking_opens
--     ] <> headers <> vars
    
--     where headers = convert "h:" o_headers
--           vars    = convert "v:" o_vars
--           convert str = concatMap (\(k, v) -> [(str <> tshow k) .= v])

instance ToFormUrlEncoded Message where
  toFormUrlEncoded Message{..} =
    mconcat [basic, recipients, mhtml, optional, headers, vars]

    where basic =
            [ ("from"   , from)
            , ("subject", subject)
            , ("text"   , text)
            ]
          recipients = foldr convertList []
            [ ("to" , to)
            , ("cc" , cc)
            , ("bcc", bcc)
            ]
          mhtml = maybe [] (\v -> [("html", v)]) html
          tags = maybe [] (map (\v -> ("o:tag", v))) o_tag
          optional = foldr convertOptional []
            [ ("o:campaign"        , o_campaign)
            , ("o:deliverytime"    , tshow <$> o_deliverytime)
            , ("o:dkim"            , tshow <$> o_dkim)
            , ("o:testmode"        , tshow <$> o_testmode)
            , ("o:tracking"        , tshow <$> o_tracking)
            , ("o:tracking-clicks" , tshow <$> o_tracking_clicks)
            , ("o:tracking-opens"  , tshow <$> o_tracking_opens)
            ]
    
          headers = convert "h:" o_headers
          vars    = convert "v:" o_vars
          convert str = fmap (first (str <>))
          
          convertList (k, v) acc = case v of
            [] -> acc
            _  -> (k, intercalate ", " v) : acc

          convertOptional (k, mv) acc = case mv of
            Nothing -> acc
            Just v  -> (k, tshow v) : acc

emptyMessage :: Message
emptyMessage = Message
  { from              = ""
  , to                = []
  , cc                = []
  , bcc               = []
  , subject           = ""
  , text              = ""
  , html              = Nothing
  , o_tag             = Nothing
  , o_campaign        = Nothing
  , o_deliverytime    = Nothing
  , o_dkim            = Nothing
  , o_testmode        = Nothing
  , o_tracking        = Nothing
  , o_tracking_clicks = Nothing
  , o_tracking_opens  = Nothing
  , o_headers         = []
  , o_vars            = []
  }

validateMessage :: Message -> Either Text Message
validateMessage msg@Message{..}
  | null from    = Left "\"from\" field must contain an email address"
  | null to      = Left "\"to\" field must contain at least one email address"
  | null subject = Left "\"subject\" field must not be empty"
  | null text    = Left "\"text\" field must not be empty"
  | otherwise    = Right msg

type EmailAddress = Text
-- newtype EmailAddress =
--   EmailAddress Text
--   deriving (Show, Generic)

-- instance ToJSON EmailAddress

type Account = Text

data MessageResponse = MessageResponse
  { responseId      :: Text
  , responseContent :: Text
  } deriving (Show)

instance FromJSON MessageResponse where
  parseJSON (Object o) =
    MessageResponse
    <$> o .: "id"
    <*> o .: "message"

type MailgunAPI =
     Header "Authorization" EncodedAuthToken
  :> "v3"
  :> Capture "account" Account
  :> "messages"
  :> ReqBody '[FormUrlEncoded] Message
  :> Post '[JSON] MessageResponse
  :> Debug

data Debug

deriving instance Show Req

instance HasClient (Post a b :> Debug) where
  type Client (Post a b :> Debug) = Req
  clientWithRoute Proxy req baseurl = req

api :: Proxy MailgunAPI
api = Proxy

message' :: Maybe EncodedAuthToken
         -> Account
         -> Message
         -> Req -- EitherT ServantError IO MessageResponse

message' = client api (BaseUrl Https "api.mailgun.net" 443)

message :: AuthToken
        -> Account
        -> Message
        -> IO (Either ServantError MessageResponse)
message token account msg = runEitherT $ message' (Just $ creds token) account msg

creds :: AuthToken -> EncodedAuthToken
creds (AuthToken token) = EncodedAuthToken . decodeUtf8 . B64.encode . encodeUtf8 $
                          ("api:" <> token)
