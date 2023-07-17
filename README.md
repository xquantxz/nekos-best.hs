# nekos-best

Haskell wrapper for nekos.best API

## Example

Get a neko image

```haskell
module Main (main) where

import NekosBest.API (getNbImage)
import qualified NekosBest.Category as C

main = do
    res <- getNbImage C.Neko
    print res
```