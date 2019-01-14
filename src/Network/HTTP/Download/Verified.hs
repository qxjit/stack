{-# LANGUAGE RankNTypes #-}
module Network.HTTP.Download.Verified
    ( verifiedDownload
    , recoveringHttp
    , warnHttpRetry
    ) where

import Control.Retry (RetryPolicy, RetryStatus(..))
import Data.Conduit
import qualified Pantry.HTTP.Download.Verified as Pantry
import Path
import RIO
import Stack.PrettyPrint
import Stack.Types.Runner

-- | Copied and extended version of Network.HTTP.Download.download.
--
-- Has the following additional features:
-- * Verifies that response content-length header (if present)
--     matches expected length
-- * Limits the download to (close to) the expected # of bytes
-- * Verifies that the expected # bytes were downloaded (not too few)
-- * Verifies md5 if response includes content-md5 header
-- * Verifies the expected hashes
--
-- Throws VerifiedDownloadException.
-- Throws IOExceptions related to file system operations.
-- Throws HttpException.
verifiedDownload
         :: HasRunner env
         => Pantry.DownloadRequest
         -> Path Abs File -- ^ destination
         -> (Maybe Integer -> ConduitM ByteString Void (RIO env) ()) -- ^ custom hook to observe progress
         -> RIO env Bool -- ^ Whether a download was performed
verifiedDownload req destpath progressSink = do
  Pantry.verifiedDownload warnHttpRetry req destpath progressSink

recoveringHttp :: HasRunner env => RetryPolicy -> RIO env a -> RIO env a
recoveringHttp retryPolicy =
  Pantry.recoveringHttp warnHttpRetry retryPolicy

warnHttpRetry :: HasRunner env => RetryStatus -> RIO env ()
warnHttpRetry rs = prettyWarn $ vcat
          [ flow $ unwords
            [ "Retry number"
            , show (rsIterNumber rs)
            , "after a total delay of"
            , show (rsCumulativeDelay rs)
            , "us"
            ]
          , flow $ unwords
            [ "If you see this warning and stack fails to download,"
            , "but running the command again solves the problem,"
            , "please report here: https://github.com/commercialhaskell/stack/issues/3510"
            , "Make sure to paste the output of 'stack --version'"
            ]
          ]
