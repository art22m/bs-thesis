import Data.Array.IArray

main = do 
    let arr = listArray (0, 2) ['x', 'y', 'z'] :: Array Int Char
    print arr

    let elm = arr ! 1
    print elm

    let elms = elems arr
    print elms

    let ids = indices arr
    print ids
    
    let enumerated = assocs arr
    print enumerated