{-# LANGUAGE OverloadedStrings #-}

module Lib ( callBreachesService, callPasswordHashService ) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.CaseInsensitive (CI)
import Control.Exception (tryJust)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import qualified Data.Monoid as M
import Data.Aeson (eitherDecode, FromJSON)
import Types

userAgent :: (CI B.ByteString, B.ByteString)
userAgent = ("User-Agent", "squeal")

truncateResponse :: B.ByteString
truncateResponse = C8.pack "truncateResponse=true"

callBreachesService :: FromJSON a => Breach -> IO (Either BreachError a)
callBreachesService (Breach email) =
  do breachesE <- (getBreaches email)
     return (breachesE >>= (\res -> stringToBreachError res $ eitherDecode res))

stringToBreachError :: L8.ByteString -> Either String a -> Either BreachError a
stringToBreachError response = either (\e -> Left $ InvalidResponse (DecodeError e) (ResponseString $ L8.unpack response)) Right

callPasswordHashService :: PasswordHash -> IO Bool
callPasswordHashService (PasswordHash hash) =
  do content <- L8.toStrict <$> getPasswordHashes hash
     let hashedLines = C8.split '\n' content
     let onlyHashes  = C8.takeWhile (/= ':') <$> hashedLines
     let hashSuffix  = suffix hash
     return $ any (== hashSuffix) onlyHashes

getBreaches :: Email -> IO (Either BreachError L8.ByteString)
getBreaches (Email email) =
  do manager  <- newManager tlsManagerSettings
     r1       <- parseUrlThrow $ "https://haveibeenpwned.com/api/v2/breachedaccount/" M.<> (T.unpack email)
     let request = r1 { requestHeaders = [userAgent], queryString = truncateResponse }
     responseE <- tryJust handleBreachesError $ httpLbs request manager
     return (responseBody <$> responseE)

handleBreachesError :: HttpException -> Maybe BreachError
handleBreachesError (HttpExceptionRequest _ (StatusCodeException response _)) = Just $ ApiCallError $ statusCodeToHttpError $ responseStatus response
handleBreachesError (HttpExceptionRequest req context) = Just $ InvalidContext (RequestString $ show req) (ContextString $ show context)
handleBreachesError (InvalidUrlException url reason)   = Just $ InvalidUrl (Url url) reason

statusCodeToHttpError :: Status -> HttpError
statusCodeToHttpError status
  | status == badRequest400      = BadRequest "The account does not comply with an acceptable format (i.e. it's an empty string)"
  | status == forbidden403       = Forbidden "No user agent has been specified in the request"
  | status == notFound404        = NotFound "Not found — the account could not be found and has therefore not been pwned"
  | status == tooManyRequests429 = TooManyRequests "Too many requests — the rate limit has been exceeded"
  | otherwise                    = OtherError (statusCode status) (statusMessage status)

getPasswordHashes :: Hash -> IO (L8.ByteString)
getPasswordHashes (Hash hashPrefix _) =
  do manager  <- newManager tlsManagerSettings
     r1       <- parseRequest $ "https://api.pwnedpasswords.com/range/" M.<> (C8.unpack hashPrefix)
     let request = r1 { requestHeaders = [userAgent]}
     response <- httpLbs request manager
     return (responseBody response)
