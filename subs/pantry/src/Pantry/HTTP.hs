{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Pantry.HTTP
  ( module Export
  , withResponse
  , httpSink
  , httpSinkChecked
  , httpJSON
  , httpLbs
  ) where

import           Conduit
import           Data.Aeson (FromJSON)
import           Network.HTTP.Client          as Export (parseRequest)
import           Network.HTTP.Client          as Export (parseUrlThrow)
import           Network.HTTP.Client          as Export (getUri, path, checkResponse)
import           Network.HTTP.Client          as Export (BodyReader, HttpExceptionContent (StatusCodeException))
import qualified Network.HTTP.Client          as HTTP (withResponse)
import           Network.HTTP.Client.Internal as Export (setUri)
import           Network.HTTP.Client.TLS      (getGlobalManager)
import           Network.HTTP.Conduit         as Export (requestHeaders)
import           Network.HTTP.Simple          as Export (HttpException (..),
                                                         Request, Response,
                                                         addRequestHeader,
                                                         defaultRequest,
                                                         getResponseBody,
                                                         getResponseHeaders,
                                                         getResponseStatus,
                                                         getResponseStatusCode,
                                                         setRequestHeader,
                                                         setRequestHeaders)
import qualified Network.HTTP.Simple          as HTTP hiding (withResponse)
import           Network.HTTP.Types           as Export (Header, HeaderName,
                                                         Status, hCacheControl,
                                                         hContentLength, hContentMD5,
                                                         hRange, ok200,
                                                         partialContent206,
                                                         statusCode)
import qualified Pantry.SHA256                as SHA256
import           Pantry.Types
import           RIO
import qualified RIO.ByteString               as B
import qualified RIO.Text                     as T

setUserAgent :: Request -> Request
setUserAgent = setRequestHeader "User-Agent" ["Haskell pantry package"]

withResponse
  :: MonadUnliftIO m
  => HTTP.Request
  -> (Response BodyReader -> m a)
  -> m a
withResponse req inner = withRunInIO $ \run -> do
  manager <- getGlobalManager
  HTTP.withResponse (setUserAgent req) manager (run . inner)

httpSink
  :: MonadUnliftIO m
  => Request
  -> (Response () -> ConduitT ByteString Void m a)
  -> m a
httpSink req inner = HTTP.httpSink (setUserAgent req) inner

httpSinkChecked
  :: MonadUnliftIO m
  => Text
  -> Maybe SHA256
  -> Maybe FileSize
  -> ConduitT ByteString Void m a
  -> m (SHA256, FileSize, a)
httpSinkChecked url msha msize sink = do
    req <- liftIO $ parseUrlThrow $ T.unpack url
    httpSink req $ const $ getZipSink $ (,,)
      <$> ZipSink (checkSha msha)
      <*> ZipSink (checkSize msize)
      <*> ZipSink sink
  where
    checkSha mexpected = do
      actual <- SHA256.sinkHash
      for_ mexpected $ \expected -> unless (actual == expected) $
        throwIO $ DownloadInvalidSHA256 url Mismatch
          { mismatchExpected = expected
          , mismatchActual = actual
          }
      pure actual
    checkSize mexpected =
      loop 0
      where
        loop accum = do
          mbs <- await
          case mbs of
            Nothing ->
              case mexpected of
                Just (FileSize expected) | expected /= accum ->
                  throwIO $ DownloadInvalidSize url Mismatch
                    { mismatchExpected = FileSize expected
                    , mismatchActual = FileSize accum
                    }
                _ -> pure (FileSize accum)
            Just bs -> do
              let accum' = accum + fromIntegral (B.length bs)
              case mexpected of
                Just (FileSize expected)
                  | accum' > expected ->
                    throwIO $ DownloadTooLarge url Mismatch
                      { mismatchExpected = FileSize expected
                      , mismatchActual = FileSize accum'
                      }
                _ -> loop accum'

httpJSON :: (MonadIO m, FromJSON a) => Request -> m (Response a)
httpJSON = HTTP.httpJSON . setUserAgent


httpLbs :: MonadIO m => Request -> m (Response LByteString)
httpLbs = HTTP.httpLbs . setUserAgent
