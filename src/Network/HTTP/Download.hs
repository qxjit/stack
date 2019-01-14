{-# LANGUAGE RankNTypes            #-}
module Network.HTTP.Download
    ( verifiedDownload
    , DownloadRequest(..)
    , drRetryPolicyDefault
    , HashCheck(..)
    , DownloadException(..)
    , CheckHexDigest(..)
    , LengthCheck
    , VerifiedDownloadException(..)

    , download
    , redownload
    , httpJSON
    , httpLbs
    , parseRequest
    , parseUrlThrow
    , setGithubHeaders
    , withResponse
    ) where

import Network.HTTP.Download.Verified
import Pantry.HTTP hiding (withResponse)
import Pantry.HTTP.Download hiding (download, redownload, verifiedDownload)
import qualified Pantry.HTTP.Download as Pantry
import Path (Path, Abs, File)
import RIO
import Stack.Types.Runner

-- | Download the given URL to the given location. If the file already exists,
-- no download is performed. Otherwise, creates the parent directory, downloads
-- to a temporary file, and on file download completion moves to the
-- appropriate destination.
--
-- Throws an exception if things go wrong
download :: HasRunner env
         => Request
         -> Path Abs File -- ^ destination
         -> RIO env Bool -- ^ Was a downloaded performed (True) or did the file already exist (False)?
download req destpath = Pantry.download warnHttpRetry req destpath

-- | Same as 'download', but will download a file a second time if it is already present.
--
-- Returns 'True' if the file was downloaded, 'False' otherwise
redownload :: HasRunner env
           => Request
           -> Path Abs File -- ^ destination
           -> RIO env Bool
redownload req destpath = Pantry.redownload warnHttpRetry req destpath
