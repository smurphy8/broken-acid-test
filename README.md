# broken-acid-state-test
Acid State appears to have a problem with file handles ...

I get this error when I run as indicated below.

``` haskell
00:00:57-scott~/programs/reference/broken-acid-state-test (master)$ ./dist/build/acidStress/acidStress
"stressing system"
11735
acidStress: 507/open.lock: Process 11735 could not create a lock: resource busy
00:01:03-scott~/programs/reference/broken-acid-state-test (master)$ 

```

## Installation

cabal sandbox init 
cabal install
cabal clean && cabal configure && cabal build



## Usage
*in working directory*
mkdir stress

./dist/build/acidStress/acidStress

## How to run tests
Ignore the tests and stuff, I just used hi to make this Q&D