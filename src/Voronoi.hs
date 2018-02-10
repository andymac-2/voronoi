module Voronoi (
  Point2D,
  voronoi2D,
  Edge,
  Vert,
  ) where 
  
import Data.Ratio
import Data.Ord (comparing)
import Data.Hashable (Hashable)
import qualified Data.List as L
import qualified Data.Sequence as S
import qualified Data.HashMap.Strict as M
import qualified Data.Set as H -- H for "heap"

epsilonD :: Double
epsilonD = 0.001

compareWithEpsilonD :: Double -> Double -> Ordering
compareWithEpsilonD a b 
  | a > b + epsilonD = GT
  | a < b - epsilonD = LT
  | otherwise = EQ
  
toDouble :: (Integral a) => Ratio a -> Double
toDouble x = fromIntegral (numerator x) / fromIntegral (denominator x)
  
epsilonR :: (Integral a) => Ratio a
epsilonR = 1%1000

compareWithEpsilonR :: (Integral a) => Ratio a -> Ratio a -> Ordering
compareWithEpsilonR a b 
  | a > b + epsilonR = GT
  | a < b - epsilonR = LT
  | otherwise = EQ

uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq list = foldr go [] list
  where
    go e [] = [e]
    go e t@(x: _)
      | e /= x = e : t
      | otherwise = t

      
      
      
type Point2D a = (a, a)

roundPoint :: Integral a => Point2D (Ratio a) -> Point2D a
roundPoint (x, y) = (round x, round y)

compareByY :: Ord a => Point2D a -> Point2D a
compareByY (x, y) = (y, x)

-- comparePointToLine t a b
-- t: the point to test if it is on the line
-- a b: two points where the line runs through
-- assert that the line is not horizontal, i.e, a and b should have different y's
comparePointToLine :: Integral a => Point2D a -> Point2D a -> 
  Point2D a -> Ordering
comparePointToLine (xt, yt) (x1, y1) (x2, y2)
  | dx <= dy = compareWithEpsilonR (xt%1) projX -- more vertical than 45 degrees
  | m > 0 = compareWithEpsilonR projY (yt%1) -- more horizontal, positive gradient
  | m < 0 = compareWithEpsilonR (yt%1) projY -- more horizontal, negative gradient
  -- fail loudly for horizontal lines (m == 0)
  where
    m = (y2 - y1) % (x2 - x1)
    dx = abs (x1 - x2)
    dy = abs (y1 - y2)
    projX = (x2 * (yt - y1) + x1 * (y2 - yt)) % (y2 - y1) -- projected x
    projY = (y2 * (xt - x1) + y1 * (x2 - xt)) % (x2 - x1) -- projected y
    
circumcentre :: (Integral a) => Point2D a -> Point2D a -> Point2D a -> 
  Maybe (Point2D (Ratio a))
circumcentre (x1, y1) (x2, y2) (x3, y3)
  | determinant == 0 = Nothing
  | otherwise = Just (x, y)
  where
    x = (cy2 * c1 - cy1 * c2) % determinant
    y = (cx1 * c2 - cx2 * c1) % determinant
    determinant = cx1 * cy2 - cx2 * cy1
    cx1 = 2 * (x3 - x1)
    cx2 = 2 * (x3 - x2)
    cy1 = 2 * (y3 - y1)
    cy2 = 2 * (y3 - y2)
    c1 = x3 * x3 - x1 * x1 + y3 * y3 - y1 * y1
    c2 = x3 * x3 - x2 * x2 + y3 * y3 - y2 * y2
    
    
    

data HalfEdge a = 
  HalfEdge (Point2D a) (Point2D a)|
  LeftExtent (Point2D a)|
  RightExtent (Point2D a)
  deriving (Show)
  
getY :: (Integral a, Show a) => Ratio a -> HalfEdge a -> Double
getY x t@(HalfEdge (xl, yl) (xr, yr)) =
  let
    xD = toDouble x
    numHoriz = fromIntegral (yr * yr - yl * yl + xr * xr - xl * xl) + xD * fromIntegral (2 * (xl - xr))
    denomHoriz = fromIntegral ((yr - yl) * 2)
    y = numHoriz / denomHoriz
    
    xrD = fromIntegral xr
    yrD = fromIntegral yr
    
    distSq = (xD - xrD) * (xD - xrD) + (y - yrD) * (y - yrD)
    
    y' = sqrt distSq + y
  in
    y'
    
    
lSite :: HalfEdge a -> Point2D a
lSite (RightExtent l) = l
lSite (HalfEdge l _ ) = l

rSite :: HalfEdge a -> Point2D a
rSite (LeftExtent r) = r
rSite (HalfEdge _ r ) = r


    
    
    
data EdgeEvent a = EdgeEvent { 
  intersection :: Point2D a,
  height :: Double
  } deriving (Eq, Show)
      
instance Ord a => Ord (EdgeEvent a) where
  compare (EdgeEvent (x1, y1) h1) (EdgeEvent (x2, y2) h2) =
    compare h1 h2 `mappend` compare x1 x2 `mappend` compare y1 y2   
    


type EdgeList i = S.Seq (HalfEdge i)
type EventHeap i = H.Set (EdgeEvent i)

-- (i, a, b, c) i is the circumcentre of a, b, c, and the intersection of these cells
type Vertex i = (Point2D (Ratio i), [Point2D i])
type Vertices i = M.HashMap (Point2D i) (H.Set (Point2D i))

type Vert i = (Point2D i, [Point2D i])

-- ((l, r), [e]) l, r: the two cells which own the edge. [e]: edge endpoints
type Edge i = ((Point2D i, Point2D i), [Point2D i])
type Edges i = M.HashMap (Point2D i, Point2D i) [Point2D i]

-- Vs p l h e v  : Points, edge List, event Heap, Edges, Vertices
data Vstate i = Vs {
    pointsList :: [Point2D i],
    edgeList :: (EdgeList i), 
    eventHeap :: (EventHeap (Ratio i)), 
    edges :: Edges i,
    vertices :: Vertices i
    } deriving (Show)

    
    
eEmit :: (Integral i, Hashable i) => Edge i -> Edges i -> Edges i
eEmit ((a, b), c)
  | a > b = M.insert (a, b) c
  | otherwise = M.insert (b, a) c

vEmit :: (Integral i, Hashable i) => Vertex i -> Vstate i -> Vstate i
vEmit ((px, py), list) (Vs p l h e v) =
  let 
    integralPoint = (round px, round py)
    
    sites = H.fromList list
    v' = M.insertWith H.union integralPoint sites v
    
    -- add the vertex as edge endpoints
    (big, mid, small) = (H.findMax sites, H.elemAt 1 sites, H.findMin sites)
    e' = M.insertWith (++) (big, mid) [integralPoint] e
    e'' = M.insertWith (++) (big, small) [integralPoint] e'
    e''' = M.insertWith (++) (mid, small) [integralPoint] e''
  in
    (Vs p l h e''' v')

    
    
-- takes the lowest points in the site list, adds them to the edge list and
-- records any edges (if multiple points)
voronoi2D :: (Show i, Hashable i, Integral i ) => [Point2D i] -> ([Edge i], [Vert i])
voronoi2D p =
  let
    p' = uniq . L.sortOn compareByY $ p
    (_, startY) = head p'
    (lowPoints, p'') = span (\(_, y) -> y == startY) p'
    
    halfedges = zipWith HalfEdge lowPoints (tail lowPoints)
    left = LeftExtent (head lowPoints)
    right = RightExtent (last lowPoints)
    
    makeEdge x y = ((x, y), [])
    
    l = S.fromList $ left : halfedges ++ [right]
    
    h = H.empty
    
    e = foldr eEmit M.empty $ zipWith makeEdge lowPoints (tail lowPoints)
    v = M.empty
    
    (Vs _ _ _ edges vertices) = getNext (Vs p'' l h e v)
  in
    (M.toList edges, M.toList . M.map H.toList $ vertices)
    

getNext :: (Show i, Hashable i, Integral i) => Vstate i -> Vstate i
getNext st@(Vs p l h e v)
  | nh && np = st
  | np = getNext . insertCollision $ st
  | nh = getNext . insertSite $ st
  | y < floor height = getNext . insertSite $ st
  | otherwise = getNext . insertCollision $ st
  where
    np = null p
    nh = null h
    (_, y) = head p
    EdgeEvent _ height = H.findMin h
      
    
    
insertSite :: (Show i, Hashable i, Integral i) => Vstate i -> Vstate i
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
    e' = eEmit ((lSite rrEdge, site), []) $ e
  in
    (Vs p' l' h' e' v)
            
            
            
insertCollision :: (Show i, Hashable i, Integral i) => Vstate i -> Vstate i
insertCollision (Vs p l h e v) =
  let 
    ev@(EdgeEvent site _) = H.findMin h
    h' = H.deleteMin h
    index = findInSequenceGt compareIntersectionHE ev l
    rEdge = S.index l (index + 1)
    lEdge = S.index l index     
      
    -- if collision is still present in edge list, replace two
    -- halfedges with the new one, and check for collisions in neighboring
    -- halfedges.
  in if compareIntersectionHE ev lEdge == EQ && compareIntersectionHE ev rEdge == EQ
    then
      let
        HalfEdge rl rr = rEdge
        HalfEdge ll lr = lEdge
        he = HalfEdge ll rr
        
        l' = S.update index he . S.deleteAt index $ l
        
        lll = S.index l' (index - 1) 
        rrr = S.index l' (index + 1)
        
        h'' = checkCollision lll he . checkCollision he rrr $ h'
        
        e' = eEmit ((ll, rr), []) e
        
        state = vEmit (site, [ll, lr, rr]) (Vs p l' h'' e' v)
      in
        state
    else 
      (Vs p l h' e v)
        

checkCollision :: (Show a, Integral a) => HalfEdge a -> HalfEdge a -> 
                  EventHeap (Ratio a) -> EventHeap (Ratio a)
checkCollision (LeftExtent _) _ st = st
checkCollision _ (RightExtent _) st = st
checkCollision 
  l@(HalfEdge ll@(xll, yll) lr@(xlr, ylr)) 
  r@(HalfEdge rl@(xrl, yrl) rr@(xrr, yrr)) (h) =
  let
    cCentre = circumcentre ll lr rr
    
  in case cCentre of
    Nothing -> h
    
    (Just cc@(xcc, ycc))
      | yll >= ylr && xcc < (xll%1)   -> h -- is a right halfedge
      | yll <= ylr && xcc >= (xlr%1) -> h -- is a left halfedge
      | yrl >= yrr && xcc < (xrl%1)   -> h -- is a right halfedge
      | yrl <= yrr && xcc >= (xrr%1)  -> h -- is a left halfedge
      | yll == ylr -> H.insert (EdgeEvent cc vertHeight) h
      | otherwise -> H.insert (EdgeEvent cc height) h
      
      where
        height = getY xcc l
        
        xccD = toDouble xcc
        yccD = toDouble ycc
        xlrD = fromIntegral xlr
        ylrD = fromIntegral ylr
        
        distSq = (xccD - xlrD) * (xccD - xlrD) + (yccD - ylrD) * (yccD - ylrD)
        vertHeight = sqrt distSq + yccD
        
    
compareCellToHE :: (Integral a) => Point2D a -> HalfEdge a -> 
  Ordering
compareCellToHE _ (LeftExtent _) = GT
compareCellToHE _ (RightExtent _) = LT
compareCellToHE c@(xc, yc) (HalfEdge l@(xl, yl) r@(xr, yr))
  | yl == yr = compare xc ((xr + xl) `quot` 2) -- vertical line
  | xl == xr = case cCentre of
    Nothing -> compare yr yl
    Just (xcc, _)
      | yl > yr && xc < xl -> LT
      | yl < yr && xc >= xr -> GT
      | otherwise -> compare (xc%1) xcc
  | compare yr yl == comparedPoint = comparedPoint
  | comparedPoint == EQ = case () of
    _
      | yl > yr -> LT -- right facingline
      | yl < yr -> GT -- left facing line      
  | otherwise = case cCentre of 
    Just (xcc, _) -> compare (xc%1) xcc -- regular case
    -- we should have caught colinear points in the clause above
    
  where 
    comparedPoint = comparePointToLine c l r
    cCentre = circumcentre c l r
    

    
compareIntersectionHE :: (Show a, Integral a) => EdgeEvent (Ratio a) -> 
  HalfEdge a -> Ordering 
compareIntersectionHE _ (LeftExtent _) = GT
compareIntersectionHE _ (RightExtent _ ) = LT
compareIntersectionHE t2@(EdgeEvent (xc, yc) h) t@(HalfEdge (xl, yl) (xr, yr))
  | yl == yr = compare xc ((xr + xl) % 2) -- perfectly vertical line
  | yl > yr = case () of -- right halfedge
    _
      | xc < (xl%1) -> LT -- is a right halfedge and the cell is to the left
      | otherwise -> compareWithEpsilonD ystar h
      
  | yl < yr = case () of --left halfedge
    _ 
      | xc >= (xr %1) -> GT -- is a left halfedge and the point is to the right
      | otherwise -> compareWithEpsilonD h ystar
      
  where
    ystar = getY xc t
        

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