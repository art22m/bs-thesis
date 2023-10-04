import Data.Array.MArray
import Data.Array.IO.Internals (IOArray(IOArray))


main = do 
    arr <- newArray (0, 3) 'x' :: IO (IOArray Int Char)
    writeArray arr 2 'y'

    arr_ <- getAssocs arr
    print arr_

    el <- readArray arr 2
    print el