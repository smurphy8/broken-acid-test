import Prelude
import Data.Acid            ( AcidState, Query, Update
                            , makeAcidic, openLocalStateFrom )




import Data.Acid.Advanced   ( query', update' )
import Data.Acid.Local      ( createCheckpointAndClose )
import Data.SafeCopy        ( base, deriveSafeCopy )

import Broken

main = do
  print "stressing system"
  openABunchOfStates 2000
       
