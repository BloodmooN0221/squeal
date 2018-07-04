{-# LANGUAGE OverloadedStrings #-}

module Lib ( callBreachesService, callPasswordHashService ) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.CaseInsensitive (CI)
import Data.List (find)
import Control.Exception (tryJust)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import qualified Data.Monoid as M
import Data.Aeson (eitherDecode, FromJSON)
import Types
import Text.Printf (printf)

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

callPasswordHashService :: PasswordHash -> IO (Either PasswordHashError Bool)
callPasswordHashService (PasswordHash hash) =
  do contentE <- getPasswordHashes hash
     return (processContent <$> contentE)
     where
       processContent :: L8.ByteString -> Bool
       processContent contentL8 =
        let content     = L8.toStrict contentL8
            hashedLines = C8.split '\n' content
            onlyHashes  = C8.takeWhile (/= ':') <$> hashedLines
            hashSuffix  = suffix hash
         in any (== hashSuffix) onlyHashes

getBreaches :: Email -> IO (Either BreachError L8.ByteString)
getBreaches (Email email) =
  do manager  <- newManager tlsManagerSettings
     r1       <- parseUrlThrow $ "https://haveibeenpwned.com/api/v2/breachedaccount/" M.<> (T.unpack email)
     let request = r1 { requestHeaders = [userAgent], queryString = truncateResponse }
     responseE <- tryJust handleBreachesError $ httpLbs request manager
     return (responseBody <$> responseE)

statusCodeToHttpError :: Response () -> HttpError
statusCodeToHttpError response = getStatus (responseStatus response)
  where
    getStatus :: Status -> HttpError
    getStatus status
      | status == badRequest400      = BadRequest "The account does not comply with an acceptable format (i.e. it's an empty string)"
      | status == forbidden403       = Forbidden "No user agent has been specified in the request"
      | status == notFound404        = NotFound "Not found — the account could not be found and has therefore not been pwned"
      | status == tooManyRequests429 = TooManyRequests $ printf "Too many requests — the rate limit has been exceeded. Try after: %s" (maybe "Unknown" (\b -> (C8.unpack b) M.<> " seconds") $ findHeader response "Retry-After")
      | otherwise                    = OtherError (statusCode status) (statusMessage status)

findHeader :: Response a -> CI B.ByteString -> Maybe B.ByteString
findHeader response headerName =
  let headers = responseHeaders response
      maybeMatch = find (\(k, _) -> k == headerName) headers
  in snd <$> maybeMatch

getPasswordHashes :: Hash -> IO (Either PasswordHashError L8.ByteString)
getPasswordHashes (Hash hashPrefix _) =
  do manager  <- newManager tlsManagerSettings
     r1       <- parseUrlThrow $ "https://api.pwnedpasswords.com/range/" M.<> (C8.unpack hashPrefix)
     let request = r1 { requestHeaders = [userAgent]}
     responseE <- tryJust handlePasswordHashError $ httpLbs request manager
     return (responseBody <$> responseE)


-- TODO: Remove this duplication
handleBreachesError :: HttpException -> Maybe BreachError
handleBreachesError (HttpExceptionRequest _ (StatusCodeException response _)) = Just $ BreachApiError $ ApiCallError $ statusCodeToHttpError response
handleBreachesError (HttpExceptionRequest req context) = Just $ BreachApiError $ InvalidContext (RequestString $ show req) (ContextString $ show context)
handleBreachesError (InvalidUrlException url reason)   = Just $ BreachApiError $ InvalidUrl (Url url) reason

-- TODO: Remove this duplication
handlePasswordHashError :: HttpException -> Maybe PasswordHashError
handlePasswordHashError (HttpExceptionRequest _ (StatusCodeException response _)) = Just $ PasswordHashApiError $ ApiCallError $ statusCodeToHttpError response
handlePasswordHashError (HttpExceptionRequest req context) = Just $ PasswordHashApiError $ InvalidContext (RequestString $ show req) (ContextString $ show context)
handlePasswordHashError (InvalidUrlException url reason)   = Just $ PasswordHashApiError $ InvalidUrl (Url url) reason

