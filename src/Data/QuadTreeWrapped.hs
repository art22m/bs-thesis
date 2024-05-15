module Data.QuadTreeWrapped where

-- import qualified Data.QuadTree as QT
-- import qualified Data.PackedMemoryQuadtree as PMQ

-- newtype QuadTreeWrapped a = QuadTreeWrapped {getQT :: QT.QuadTree a}
--   deriving (Show)

-- empty :: Int -> Int -> QuadTreeWrapped v
-- empty w h = QuadTreeWrapped {getQT = QT.makeTree (w, h) '.'}

-- insert :: PMQ.Coords n -> v -> QuadTreeWrapped v -> QuadTreeWrapped v
-- insert (PMQ.Coords x y) v !qt = QuadTreeWrapped {getQT = QT.setLocation loc v (getQT qt)}
--   where
--     loc = (x, y)
