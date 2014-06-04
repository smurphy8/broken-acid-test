{-# LANGUAGE  GeneralizedNewtypeDeriving, MultiParamTypeClasses,DeriveDataTypeable,OverloadedStrings,
  TemplateHaskell, TypeFamilies, RecordWildCards #-}

module Broken where



import Data.Acid            ( AcidState, Query, Update
                            , makeAcidic, openLocalStateFrom )


import Data.Traversable

import Control.Applicative  ( (<$>) )
import Control.Exception    ( bracket )
import Control.Monad        ( msum )
import Control.Monad.Reader ( ask )
import Control.Monad.State  ( get, put )
import Data.Typeable
import Data.Acid.Advanced   ( query', update' )
import Data.Acid.Local      ( createCheckpointAndClose )
import Data.SafeCopy        ( base, deriveSafeCopy )
import Filesystem
import Filesystem.Path

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
  traverse (flip openLocalStateFrom initialCounterState) aBunchOfStates
  setWorkingDirectory wd
