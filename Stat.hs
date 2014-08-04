{-# LANGUAGE ScopedTypeVariables #-}

module HMisc.Stat (
	statFiles,
	statFile) where

import HMisc.StatInc
import System.Posix.Files
import Control.Monad
import Data.Maybe
import qualified Control.Exception as E

createFilePair a b = FilePair { path = a, stat = b }


statFiles :: [FilePath] -> IO [FilePair]
statFiles l = liftM catMaybes $ mapM statFile l


statFile :: FilePath -> IO (Maybe FilePair)
statFile s = liftM (Just . createFilePair s . fromJust) (statFileContainer s)


statFileContainer :: FilePath -> IO (Maybe FileStatus)
statFileContainer s =
	E.catch (getFileStatus s >>= \x -> return (Just x))
			(\(e :: E.SomeException) -> return Nothing)
