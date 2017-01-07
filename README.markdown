# Test Haskell Scripting GHC API

This repository demonstrates a use of the GHC 8 API to load a pre-compiled library in the same cabal package.

Originally working under GHC 7.10, GHC 8 now complains:

```
package testhsscript-0.1-... is unusable due to missing or recursive dependencies:
```

on every imported package, likely due to some misunderstanding of how the API has changed. I would appreciate any advice.

To reproduce:

1. ```cabal sandbox init```
1. ```cabal install```
1. ```cabal run test```
