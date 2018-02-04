module Voronoi where 
  
import qualified Data.List as L
import Data.Ord (comparing, Down(..))
import Data.Function (on)
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Sequence as S
import qualified Data.Set as H -- H for "heap"

import Debug.Trace




newtype Point2D a = Point2D (a, a) deriving (Show, Eq)

circumcentre :: Integral a => Point2D a -> Point2D a -> Point2D a -> Maybe (Point2D a)
circumcentre (Point2D (x1, y1)) (Point2D (x2, y2)) (Point2D (x3, y3))
  | determinant == 0 = Nothing
  | otherwise = Just (Point2D (x, y))
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

instance Ord a => Ord (Point2D a) where
  compare (Point2D (x1, y1)) (Point2D (x2, y2))
    | y1 > y2 = GT
    | y1 < y2 = LT
    | x1 > x2 = GT
    | x1 < x2 = LT
    | otherwise = EQ

    
    
    
data Cell a = 
  Cell (Point2D a) |
  LeftExtent |
  RightExtent
  deriving (Show)
  
  
  

data HalfEdge a = HalfEdge 
    { lCell :: Cell a 
    , rCell :: Cell a 
    } deriving (Show)
    
    
    
data EdgeEvent a = EdgeEvent 
    { intersection :: Point2D a
    , height :: a
    } deriving (Eq, Show)
      
instance Ord a => Ord (EdgeEvent a) where
    compare (EdgeEvent p1 h1) (EdgeEvent p2 h2)
        | h1 > h2 = GT
        | h1 < h2 = LT
        | otherwise = compare p1 p2
        
    
    

type EdgeList i = S.Seq (HalfEdge i)
type EventHeap i = H.Set (EdgeEvent i)
data Vertex i = Vertex (Point2D i) [Point2D i] deriving (Show)
data Connection a = Connection (Point2D a) (Point2D a) deriving (Show)

-- Vstate p l h e v  : Points, edge List, event Heap, Edges, Vertices
data Vstate i = Vstate {
    pointsList :: [Point2D i],
    edgeList :: (EdgeList i), 
    eventHeap :: (EventHeap i), 
    edges :: [Connection i],
    vertices :: [Vertex i]
    } deriving (Show)
    
type VoronoiT i m = StateT (Vstate i ) m

lModify :: Monad m => (EdgeList i -> EdgeList i) -> VoronoiT i m ()
lModify f = modify (\(Vstate p l h e v)  -> Vstate p (f l) h e v)

lGet ::  Monad m => VoronoiT i m (EdgeList i)
lGet = gets (\(Vstate _ l _ _ _) -> l)

lIndex :: Monad m => Int -> VoronoiT i m (HalfEdge i)
lIndex index = gets (\(Vstate _ l _ _ _) -> S.index l index)

lInsertAt :: Monad m => Int -> (HalfEdge i) -> VoronoiT i m ()
lInsertAt x y = lModify (S.insertAt x y)

lDeleteAt :: (Monad m, Ord i) => Int -> VoronoiT i m ()
lDeleteAt x = lModify (S.deleteAt x)

lPut :: Monad m => (EdgeList i) -> VoronoiT i m ()
lPut l = modify (\(Vstate p _ h e v)  -> Vstate p l h e v)

hModify :: Monad m => (EventHeap i -> EventHeap i) -> VoronoiT i m ()
hModify f = modify (\(Vstate p l h e v)  -> Vstate p l (f h) e v)

hState :: Monad m => (EventHeap i -> (a, EventHeap i)) -> VoronoiT i m a
hState f = state (\(Vstate p l h e v) -> 
    let (result, state) = f h
    in (result, Vstate p l state e v))

hInsert :: (Monad m, Ord i) => EdgeEvent i -> VoronoiT i m ()
hInsert x = hModify (H.insert x)

hGet :: Monad m => VoronoiT i m (EventHeap i)
hGet = gets (\(Vstate _ _ h _ _) -> h)

hNull :: (Ord i, Monad m) => VoronoiT i m Bool
hNull = gets (\(Vstate _ _ h _ _) -> H.null h)

hFindMin :: (Ord i, Monad m) => VoronoiT i m (EdgeEvent i)
hFindMin = gets (\(Vstate _ _ h _ _) -> H.findMin h)

pModify :: Monad m => ([Point2D i] -> [Point2D i]) -> VoronoiT i m ()
pModify f = modify (\(Vstate p l h e v)  -> Vstate (f p) l h e v)

pState:: Monad m => ([Point2D i] -> (a, [Point2D i])) -> VoronoiT i m a
pState f = state (\(Vstate p l h e v) -> 
    let (result, state) = f p
    in (result, Vstate state l h e v))
 
pGet :: Monad m => VoronoiT i m [Point2D i]
pGet = gets (\(Vstate p _ _ _ _) -> p)
    
pNull :: Monad m => VoronoiT i m Bool
pNull = gets (\(Vstate p _ _ _ _) -> null p)

eEmit :: Monad m => ([Connection i]) -> VoronoiT i m ()
eEmit x = modify (\(Vstate p l h e v)  -> Vstate p l h (x ++ e) v)

vEmit :: Monad m => ([Vertex i]) -> VoronoiT i m ()
vEmit x = modify (\(Vstate p l h e v)  -> Vstate p l h e (x ++ v))

-- takes the lowest points in the site list, adds them to the edge list and
-- records any edges (if multiple points)
initialise :: (Eq i, Monad m) => VoronoiT i m ()
initialise = do
    lowPoints <- pState (\p ->
        let Point2D (_, startY) = head p
        in  span (\(Point2D (_, y)) -> y == startY) p
        )
    let lowCells = map Cell lowPoints
        rsites = lowCells ++ [RightExtent]
        lsites = LeftExtent : lowCells
        edgeList = zipWith HalfEdge lsites rsites
    lPut (S.fromList edgeList)
    let edges = zipWith Connection lowPoints (tail lowPoints)
    eEmit edges

getNext :: (Show i, Integral i, Monad m) => VoronoiT i m ()
getNext = do
    currState <- get
    put (trace (show currState) currState)
    points <- pGet
    heap <- hGet
    let Point2D (x, _) = head points
        go True True = return () -- finished!
        go True False = insertSite
        go false True = insertCollision
        go _ _ 
            | let   EdgeEvent _ h = H.findMin heap
                    lowest = min x h
              in    lowest == x = insertSite
            | otherwise = insertCollision
    go (null heap) (null points)
    unless ((null heap && null points)) getNext
    
    
    
    
insertSite :: (Show i, Integral i, Monad m) => VoronoiT i m ()
insertSite = do
    site <- pState (\p -> (head p, tail p))
    l <- lGet
    let index = findInSequenceGt compareCellToHE site l
        rrEdge@(HalfEdge (Cell rl) _) = S.index l index
        llEdge@(HalfEdge _ (Cell lr)) = S.index l (index - 1)
        rEdge = HalfEdge (Cell site) (Cell rl)
        lEdge = HalfEdge (Cell lr) (Cell site)
    lInsertAt index rEdge
    lInsertAt index lEdge
    checkCollision rEdge rrEdge
    checkCollision llEdge lEdge
    eEmit [Connection lr site]
    return ()
            
            
            
insertCollision :: (Show i, Integral i, Monad m) => VoronoiT i m ()
insertCollision = do
    EdgeEvent (Point2D (x, y)) h  <- hState (\h -> (H.findMin h, H.deleteMin h))
    l <- lGet
    let site = Point2D (x, y)
        index = findInSequenceGt compareIntersectionHE (trace ("Insert Collision:\nSite: " ++ (show site)) site) l
        rEdge = S.index l (index + 1)
        lEdge = S.index l index
        HalfEdge (Cell rl) (Cell rr) = rEdge
        HalfEdge (Cell ll) (Cell lr) = lEdge
        isEq = compareIntersectionHE site (trace ("lEdge: " ++ (show lEdge)) lEdge) == EQ && compareIntersectionHE site (trace ("rEdge: " ++ (show rEdge)) rEdge) == EQ
    if (trace ("isEq: " ++ (show isEq)) isEq)
        then do
            -- if collision is still present in edge list, replace two
            -- halfedges with the new one, and check for collisions in neighboring
            -- halfedges.
            lDeleteAt index
            lDeleteAt index
            let he = HalfEdge (Cell ll) (Cell rr)
            lInsertAt index he
            eEmit [Connection ll rr]
            vEmit [Vertex site [ll, lr, rr]]
            lll <- lIndex (index - 1)
            rrr <- lIndex (index + 1)
            checkCollision lll he
            checkCollision he rrr
            return ()
        else 
            return ()
        

checkCollision :: (Show i, Integral i, Monad m) => HalfEdge i -> HalfEdge i -> VoronoiT i m ()
checkCollision (HalfEdge LeftExtent _) _ = return ()
checkCollision _ (HalfEdge _ RightExtent) = return ()
checkCollision 
    (HalfEdge 
        (Cell ll@(Point2D (xll, yll))) 
        (Cell lr@(Point2D (xlr, ylr)))) 
    (HalfEdge 
        (Cell rl@(Point2D (xrl, yrl))) 
        (Cell rr@(Point2D (xrr, yrr)))) = 
    case cCentre of
    Nothing -> return () -- colinear, no collision
    Just cc@(Point2D (xcc, ycc))
        | yll >= ylr && xcc < xll -> return (trace "1" ()) -- is a right halfedge
        | yll <= ylr && xcc >= xlr -> return (trace "2" ()) -- is a left halfedge
        | yrl >= yrr && xcc < xrl -> return (trace "3" ()) -- is a right halfedge
        | yrl <= yrr && xcc >= xrr -> return (trace "4" ()) -- is a left halfedge
        | otherwise -> (trace "5" (hInsert (EdgeEvent cc height)))
        where
            -- We now introduce the possibility of being off by one
            -- (or two) in the Int range, but this is necessary
            distSq = (xcc - xlr) * (xcc - xlr) + (ycc - ylr) * (ycc - ylr)
            distfloat = sqrt . fromIntegral $ distSq :: Double
            height = (ceiling distfloat) + ycc
    where cCentre = trace ("cCentre: " ++ show (circumcentre ll lr rr)) (circumcentre ll lr rr)

    
    
compareCellToHE :: Integral a => Point2D a -> HalfEdge a -> Ordering
compareCellToHE _ (HalfEdge LeftExtent _) = GT
compareCellToHE _ (HalfEdge _ RightExtent) = LT
compareCellToHE c@(Point2D (xc, yc)) (HalfEdge (Cell l@(Point2D (xl, yl))) (Cell r@(Point2D (xr, yr))))
    | yl >= yr && xc < xl = LT -- is a right halfedge, and the cell is to the left
    | yl <= yr && xc > xr = GT -- is a left halfedge, and the cell it to the right
    | otherwise = case cCentre of 
        Nothing -- cases of collinear points
            | yl > yr -> LT -- right facingline
            | yl < yr -> GT -- left facing line
            | yl == yr -> compare xc ((xr + xl) `quot` 2) -- vertical line
        Just (Point2D (xcc, _)) -> compare xc xcc -- regular case
    where cCentre = circumcentre c l r
    
    
    
    
    
compareIntersectionHE :: Integral a => Point2D a -> HalfEdge a -> Ordering 
compareIntersectionHE _ (HalfEdge LeftExtent _) = GT
compareIntersectionHE _ (HalfEdge _ RightExtent) = LT
compareIntersectionHE c@(Point2D (xc, yc)) (HalfEdge (Cell l@(Point2D (xl, yl))) (Cell r@(Point2D (xr, yr))))
    | yl >= yr && xc < xl = LT -- is a right halfedge and the cell is to the left
    | yl <= yr && xc >= xr = GT -- is a left halfedge and the point is to the right
    | dx >= dy = let -- more vertical than horizontal
        numVert = xr * xr - xl * xl + yr * yr - yl * yl + yc * 2 * (yl - yr)
        denomVert = (xr - xl) * 2 -- project x onto a bisector
        projX = numVert `quot` denomVert
        vertTest -- if within limits, it coincides
            | xc < projX - epsilon = LT 
            | xc > projX + epsilon = GT
            | otherwise = EQ
        in vertTest
    | dx < dy = let  -- more horizontal than vertical
        numHoriz = yr * yr - yl * yl + xr * xr - xl * xl + xc * 2 * (xl - xr)
        denomHoriz = (yr - yl) * 2
        projY = numHoriz `quot` denomHoriz
        horizTest
            | yc > projY + epsilon && yl >= yr = LT
            | yc > projY + epsilon && yl <= yr = GT
            | yc < projY - epsilon && yl >= yr = GT
            | yc < projY - epsilon && yl <= yr = LT
            | otherwise = EQ
        in horizTest
    where
        epsilon = 1
        dx = abs (xl - xr)
        dy = abs (yl - yr)
        

-- finds where an item fits inside a sequence.
-- there is no requirement that the item be of the same type as the type
-- contained in the sequence. If it cannot find an equality, it will find
-- the smallest element larger than itself.
findInSequenceGt :: (a -> b -> Ordering) -> a -> S.Seq b -> Int
findInSequenceGt orderFun test sequence = go 0 ((S.length sequence) - 1) where
  go lbound ubound
    | lbound == ubound = lbound
    | order == GT = go (cursor + 1) ubound
    | otherwise = go lbound cursor
    where
        cursor = (lbound + ubound) `quot` 2
        order = orderFun test (S.index sequence cursor)
  
voronoi2D ::(Show a, Integral a) => [Point2D a] -> Vstate a
voronoi2D a = execState (do
    initialise
    pModify L.sort
    getNext
    ) (Vstate a S.empty H.empty [] [])