{-# LANGUAGE  GeneralizedNewtypeDeriving, MultiParamTypeClasses,DeriveDataTypeable,OverloadedStrings,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}

module Broken where



import Data.Acid            ( AcidState, Query, Update
                            , makeAcidic, openLocalStateFrom )


import Data.Traversable
import qualified System.IO as SIO
import Control.Applicative  ( (<$>) )
import Control.Exception    ( bracket )
import Control.Monad        ( msum )
import Control.Monad.Reader ( ask )
import Control.Monad.State  ( get, put )
import Control.Concurrent
import System.Posix.Process (getProcessID)
import Data.Typeable
import Data.Acid.Advanced   ( query', update' )
import Data.Acid.Local      ( createCheckpointAndClose )
import Data.SafeCopy        ( base, deriveSafeCopy )
import Filesystem hiding (writeFile,openFile)
import Filesystem.Path hiding (FilePath)


import System.Posix(Fd(Fd),
                    openFd,
                    fdWriteBuf,
                    fdToHandle,
                    closeFd,
                    OpenMode(WriteOnly,ReadWrite),
                    exclusive, trunc,
                    defaultFileFlags,
                    stdFileMode
                   )

open :: FilePath -> IO Handle
open filename = ( openFd filename ReadWrite (Just 0o600) (defaultFileFlags {exclusive = True, trunc = True})) >>= fdToHandle

data FHandle = FHandle Fd


data CounterState = CounterState { count :: Integer }
    deriving (Eq, Ord, Read, Show,  Typeable)

$(deriveSafeCopy 0 'base ''CounterState)


initialCounterState :: CounterState
initialCounterState = CounterState 0

incCountBy :: Integer -> Update CounterState Integer
incCountBy n =
    do c@CounterState{..} <- get
       let newCount = count + n
       put $ c { count = newCount }
       return newCount  


$(makeAcidic ''CounterState ['incCountBy])





openABunchOfStates n = do
  let aBunchOfStates = fmap show [1 .. n]
  wd <- getWorkingDirectory 
  setWorkingDirectory (wd </> "stress")
  traverse stateOpenFcn aBunchOfStates
  setWorkingDirectory wd
  where
    stateOpenFcn fn = do
      threadDelay (10*1000)
      openLocalStateFrom fn initialCounterState


writeABunchOfFiles n = do
  let aBunchOfFiles = fmap show [1 .. n]
  wd <- getWorkingDirectory 
  setWorkingDirectory (wd </> "stressIO")
  traverse writeFileFcn aBunchOfFiles
  setWorkingDirectory wd
  where
    writeFileFcn fn = do
      writeFile fn "Is this a bug?"


openABunchOfFiles n = do
  let aBunchOfFiles = fmap show [1 .. n]
  wd <- getWorkingDirectory 
  setWorkingDirectory (wd </> "stressIO")
  hs <- traverse openFileFcn aBunchOfFiles
  setWorkingDirectory wd
  return hs
  where
    openFileFcn fn = do
      open fn 

openAndCloseABunchOfFiles n = do
  hs <-(openABunchOfFiles n)  
  traverse (\h -> do
--               pid <- getProcessID
               SIO.hPutStrLn h (show "am I a bug?")
               SIO.hClose h ) hs
