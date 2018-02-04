module Voronoi where 
  
import qualified Data.List as L
import Data.Ord (comparing, Down(..))
import Data.Function (on)
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Sequence as S
import qualified Data.Set as H -- H for "heap"

import Debug.Trace

uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq list = foldr go [] list
  where
    go e [] = [e]
    go e t@(x: _)
      | e /= x = e : t
      | otherwise = t

type Point2D a = (a, a)

compareByY :: Ord a => Point2D a -> Point2D a
compareByY (x, y) = (y, x)

circumcentre :: Integral a => Point2D a -> Point2D a -> Point2D a -> Maybe (Point2D a)
circumcentre (x1, y1) (x2, y2) (x3, y3)
  | determinant == 0 = Nothing
  | otherwise = Just (x, y)
  where
    x = (cy2 * c1 - cy1 * c2) `quot` determinant
    y = (cx1 * c2 - cx2 * c1) `quot` determinant
    determinant = cx1 * cy2 - cx2 * cy1
    cx1 = 2 * (x3 - x1)
    cx2 = 2 * (x3 - x2)
    cy1 = 2 * (y3 - y1)
    cy2 = 2 * (y3 - y2)
    c1 = x3 * x3 - x1 * x1 + y3 * y3 - y1 * y1
    c2 = x3 * x3 - x2 * x2 + y3 * y3 - y2 * y2
  
  
  
  

data HalfEdge a = 
  HalfEdge (Point2D a) (Point2D a) |
  LeftExtent (Point2D a) |
  RightExtent (Point2D a)
  deriving (Show)
    
lSite :: HalfEdge a -> Point2D a
lSite (RightExtent l) = l
lSite (HalfEdge l _) = l    

rSite :: HalfEdge a -> Point2D a
rSite (LeftExtent r) = r
rSite (HalfEdge r _) = r    
    
    
data EdgeEvent a = EdgeEvent 
    { intersection :: Point2D a
    , height :: a
    } deriving (Eq, Show)
      
instance Ord a => Ord (EdgeEvent a) where
    compare (EdgeEvent p1 h1) (EdgeEvent p2 h2)
        | h1 > h2 = GT
        | h1 < h2 = LT
        | otherwise = comparing compareByY p1 p2
        
    
    

type EdgeList i = S.Seq (HalfEdge i)
type EventHeap i = H.Set (EdgeEvent i)
data Vertex i = Vertex (Point2D i) [Point2D i] deriving (Show)
data Connection a = Connection (Point2D a) (Point2D a) deriving (Show)

-- Vs p l h e v  : Points, edge List, event Heap, Edges, Vertices
data Vstate i = Vs {
    pointsList :: [Point2D i],
    edgeList :: (EdgeList i), 
    eventHeap :: (EventHeap i), 
    edges :: [Connection i],
    vertices :: [Vertex i]
    } deriving (Show)

eEmit :: [Connection i] -> [Connection i] -> [Connection i]
eEmit = (++)

vEmit :: [Vertex i] -> [Vertex i] -> [Vertex i]
vEmit = (++)

-- takes the lowest points in the site list, adds them to the edge list and
-- records any edges (if multiple points)
voronoi2D :: (Show i, Integral i) => [Point2D i] -> Vstate i
voronoi2D p =
  let
    p' = uniq . L.sortOn compareByY $ p
    (_, startY) = head p'
    (lowPoints, p'') = span (\(_, y) -> y == startY) p'
    
    halfedges = zipWith HalfEdge lowPoints (tail lowPoints)
    left = LeftExtent (head lowPoints)
    right = RightExtent (last lowPoints)
    
    l = S.fromList $ left : halfedges ++ [right]
    
    h = H.empty
    e = eEmit (zipWith Connection lowPoints (tail lowPoints)) $ []
    v = []
  in
    getNext (Vs p'' l h e v)
    

getNext :: (Show i, Integral i) => Vstate i -> Vstate i
getNext st@(Vs p l h e v)
  | nh && np = stt
  | np = getNext . insertCollision $ stt
  | nh = getNext . insertSite $ stt
  | y < height = getNext . insertSite $ stt
  | otherwise = getNext . insertCollision $ stt
  where
    stt = trace (show st) st
    np = null p
    nh = null h
    (_, y) = head p
    EdgeEvent _ height = H.findMin h
      
    
    
    
insertSite :: (Show i, Integral i) => Vstate i -> Vstate i
insertSite (Vs p l h e v) =
  let
    site = head p
    p' = tail p
    
    index = findInSequenceGt compareCellToHE site l
    
    rrEdge = S.index l index
    llEdge = S.index l (index - 1)
    
    rEdge = HalfEdge site (lSite rrEdge)
    lEdge = HalfEdge (rSite llEdge) site
    
    l' = S.insertAt index lEdge . S.insertAt index rEdge $ l

    h' = checkCollision rEdge rrEdge . checkCollision llEdge lEdge $ h
    e' = eEmit [Connection (lSite rrEdge) site] $ e
  in
    (Vs p' l' h' e' v)
            
            
            
insertCollision :: (Show i, Integral i) => Vstate i -> Vstate i
insertCollision (Vs p l h e v) =
  let 
    EdgeEvent site _ = H.findMin h
    h' = H.deleteMin h
    index = findInSequenceGt compareIntersectionHE site l
    rEdge = S.index l (index + 1)
    lEdge = S.index l index     
      
    -- if collision is still present in edge list, replace two
    -- halfedges with the new one, and check for collisions in neighboring
    -- halfedges.
  in if compareIntersectionHE site lEdge == EQ && 
        compareIntersectionHE site rEdge == EQ
    then
      let
        HalfEdge rl rr = rEdge
        HalfEdge ll lr = lEdge
        he = HalfEdge ll rr
        
        l' = S.update index he . S.deleteAt index $ l
        
        lll = S.index l' (index - 1) 
        rrr = S.index l' (index + 1)
        
        h'' = checkCollision lll he . checkCollision he rrr $ h'
        
        e' = eEmit [Connection ll rr] e
        v' = vEmit [Vertex site [ll, lr, rr]] v
      in
        (Vs p l' h'' e' v')
    else 
      (Vs p l h' e v)
        

checkCollision :: (Show i, Integral i) => HalfEdge i -> HalfEdge i -> 
                  EventHeap i -> EventHeap i
checkCollision (LeftExtent _) _ st = st
checkCollision _ (RightExtent _) st = st
checkCollision 
  (HalfEdge ll@(xll, yll) lr@(xlr, ylr)) 
  (HalfEdge rl@(xrl, yrl) rr@(xrr, yrr)) 
  (h) =
  let 
    cCentre = trace ("cCentre: " ++ show (circumcentre ll lr rr)) (circumcentre ll lr rr)
    
    go Nothing = h
    go (Just cc@(xcc, ycc))
      | yll >= ylr && xcc < xll   = trace "1"  h -- is a right halfedge
      | yll <= ylr && xcc >= xlr  = trace "2"  h -- is a left halfedge
      | yrl >= yrr && xcc < xrl   = trace "3"  h -- is a right halfedge
      | yrl <= yrr && xcc >= xrr  = trace "4"  h -- is a left halfedge
      | otherwise = (trace "5" (H.insert (EdgeEvent cc height) h))
      
      -- We now introduce the possibility of being off by one
      -- (or two) in the Int range, but this is necessary
      where
        distSq = (xcc - xlr) * (xcc - xlr) + (ycc - ylr) * (ycc - ylr)
        distfloat = sqrt . fromIntegral $ distSq :: Double
        height = (ceiling distfloat) + ycc
  in 
    (go cCentre)

    
    
compareCellToHE :: Integral a => Point2D a -> HalfEdge a -> Ordering
compareCellToHE _ (LeftExtent _) = GT
compareCellToHE _ (RightExtent _) = LT
compareCellToHE c@(xc, yc) (HalfEdge l@(xl, yl) r@(xr, yr))
  | yl >= yr && xc < xl = LT -- is a right halfedge, and the cell is to the left
  | yl <= yr && xc > xr = GT -- is a left halfedge, and the cell it to the right
  | otherwise = 
    case cCentre of 
      Nothing -- cases of collinear points
        | yl > yr -> LT -- right facingline
        | yl < yr -> GT -- left facing line
        | yl == yr -> compare xc ((xr + xl) `quot` 2) -- vertical line
      Just (xcc, _) -> compare xc xcc -- regular case
  where 
    cCentre = circumcentre c l r
    

    
compareIntersectionHE :: Integral a => Point2D a -> HalfEdge a -> Ordering 
compareIntersectionHE _ (LeftExtent _) = GT
compareIntersectionHE _ (RightExtent _ ) = LT
compareIntersectionHE (xc, yc) (HalfEdge (xl, yl) (xr, yr))
  | yl >= yr && xc < xl = LT -- is a right halfedge and the cell is to the left
  | yl <= yr && xc >= xr = GT -- is a left halfedge and the point is to the right
  | dx >= dy = -- more vertical than horizontal
    let 
      numVert = xr * xr - xl * xl + yr * yr - yl * yl + yc * 2 * (yl - yr)
      denomVert = (xr - xl) * 2 -- project x onto a bisector
      projX = numVert `quot` denomVert
      vertTest -- if within limits, it coincides
        | xc < projX - epsilon = LT 
        | xc > projX + epsilon = GT
        | otherwise = EQ
    in 
      vertTest
  | dx < dy = -- more horizontal than vertical
    let  
      numHoriz = yr * yr - yl * yl + xr * xr - xl * xl + xc * 2 * (xl - xr)
      denomHoriz = (yr - yl) * 2
      projY = numHoriz `quot` denomHoriz
      horizTest
        | yc > projY + epsilon && yl >= yr = LT
        | yc > projY + epsilon && yl <= yr = GT
        | yc < projY - epsilon && yl >= yr = GT
        | yc < projY - epsilon && yl <= yr = LT
        | otherwise = EQ
      in 
        horizTest
  where
    epsilon = 1
    dx = abs (xl - xr)
    dy = abs (yl - yr)
        

-- finds where an item fits inside a sequence.
-- there is no requirement that the item be of the same type as the type
-- contained in the sequence. If it cannot find an equality, it will find
-- the smallest element larger than itself.
findInSequenceGt :: (a -> b -> Ordering) -> a -> S.Seq b -> Int
findInSequenceGt orderFun test sequence = 
  let
    go lbound ubound
      | lbound == ubound = lbound
      | order == GT = go (cursor + 1) ubound
      | otherwise = go lbound cursor
      where
        cursor = (lbound + ubound) `quot` 2
        order = orderFun test (S.index sequence cursor)
  in
    go 0 ((S.length sequence) - 1)