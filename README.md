# nekos-best
![](https://img.shields.io/hackage/v/nekos-best.svg)
![](https://img.shields.io/github/license/xquantxz/nekos-best%2ehs.svg)
![](https://img.shields.io/github/issues/xquantxz/nekos-best%2ehs.svg)

Haskell wrapper for [nekos.best](https://nekos.best) API

## Examples

Get a neko image

```haskell
import NekosBest.API (getNbImage)
import qualified NekosBest.Category as C

main = do
    res <- getNbImage C.Neko
    print res
```

___

For random images you can use `randomNbImage` passing a `RandomGen` value

```haskell
import NekosBest.API (randomNbImage)
import qualified NekosBest.Category as C

main = do
    gen <- getStdGen
    (res, gen') <- randomNbImage gen
    print res

```
___

Downloading an image

```haskell
import NekosBest.API (getNbImage, downloadNbImage)
import qualified NekosBest.Category as C

main = do
    res <- getNbImage C.Neko
    mapM_ (\x -> downloadNbImage x "filename") res
```
