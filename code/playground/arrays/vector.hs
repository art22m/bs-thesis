import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

ivec = do 
    let v = V.fromList ["a","b","c"]
    let e = v V.! 1
    print e

    let v1 = v V.// [(1,"x")]
    let e = v1 V.! 1
    print e

    let v2 = V.slice 1 2 v1
    print v2

mvec = do
    let v = V.fromList ["a","b","c"]  
    mv <- V.thaw v
    MV.write mv 1 "x"
    v1 <- V.freeze mv
    print v1

main = ivec >> mvec