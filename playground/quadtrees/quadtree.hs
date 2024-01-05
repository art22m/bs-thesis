type Point = (Float, Float)
type Rect = (Point, Point)

data Quad a = Quad a a a a
  deriving (Show)

data Quadrant a
  = Empty                     
  | Bucket a Point            
  | Split (Quad (QuadTree a)) 
  deriving (Show)

data QuadTree a = QuadTree Rect (Quadrant a)
  deriving (Show)

splitRect :: Rect -> Quad (QuadTree a)
splitRect ((l, b), (r, t)) = Quad
  (QuadTree ((l, b), (x, y)) Empty)   -- leftBottom
  (QuadTree ((l, y), (x, t)) Empty)   -- leftTop
  (QuadTree ((x, y), (r, t)) Empty)   -- rightBottom
  (QuadTree ((x, b), (r, y)) Empty)   -- rightTop
  where
    x = (r + l) / 2
    y = (b + t) / 2

insert :: a -> Point -> QuadTree a -> QuadTree a
insert x p (QuadTree rect Empty) = QuadTree rect (Bucket x p)
insert x p (QuadTree rect (Bucket y q))
  | p == q    = QuadTree rect (Bucket x p)
  | otherwise = insert x p (insert y q (QuadTree rect (Split (splitRect rect))))
insert x p (QuadTree rect (Split (Quad lb lt rt rb))) = QuadTree rect (Split quad)
  where
    quad
      | leftBottom = Quad (insert x p lb) lt rt rb
      | leftTop    = Quad lb (insert x p lt) rt rb
      | rightTop   = Quad lb lt (insert x p rt) rb
      | otherwise  = Quad lb lt rt (insert x p rb)

    leftBottom = left  p rect && bottom p rect
    leftTop    = left  p rect && top p rect
    rightTop   = right p rect && top p rect

left :: Point -> Rect -> Bool
left (x, _) ((l, _), (r, _)) = x < (l + r) / 2

right :: Point -> Rect -> Bool
right p = not . left p

bottom :: Point -> Rect -> Bool
bottom (_, y) ((_, b), (_, t)) = y < (b + t) / 2

top :: Point -> Rect -> Bool
top p = not . bottom p

toList :: QuadTree a -> [a]
toList (QuadTree _ Empty) = []
toList (QuadTree _ (Bucket x _)) = [x]
toList (QuadTree _ (Split (Quad a b c d)))
  = toList a ++ toList b ++ toList c ++ toList d

insideRect :: Rect -> Point -> Bool 
insideRect ((a, b), (c, d)) (x, y) =  (a <= x && x <= c) && (b <= y && y <= d)

getRange :: Rect -> QuadTree a -> [a]
getRange _ (QuadTree _ Empty) = []
getRange r (QuadTree _ (Bucket x p)) 
  | insideRect r p = [x]
  | otherwise = []
-- Dont go to all quadrants if rect in this quadrant not in the requested rect
getRange r (QuadTree _ (Split (Quad a b c d))) 
  = getRange r a ++ getRange r b ++ getRange r c ++ getRange r d

--------------------------------------------------------------
main = do
  let qt = QuadTree ((-10, -10), (10, 10)) Empty :: QuadTree String
  print qt
  
  let elems = toList qt
  print elems

  let qt1 = insert "Twit1" (9, 9) qt
      qt2 = insert "Twit2" (8, 8) qt1
      qt3 = insert "Twit3" (-5, 5) qt2
      qt4 = insert "Twit4" (5, -5) qt3
    
  let elems = toList qt4
  print elems

  let elems = getRange ((0, 0), (10, 10)) qt4
  print elems
