module Algorithm.Search.JumpPoint.Pathing
  ( rotate45C
  , rotate45CC
  , rotate90C
  , rotate90CC
  , rotate135C
  , rotate135CC
  , rotate180
  , translate
  , isAxisJump
  , isDiagJump
  , isPathOpenOnAxis
  , Direction(..)
  , JumpGetter
  , IsOpen
  , Point
  , jpsPath
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes, isJust)
import           Data.PriorityQueue.FingerTree (PQueue)
import qualified Data.PriorityQueue.FingerTree as PQ
import           Data.Tuple (swap)

-- | A point on the grid.
type Point = (Int, Int)

type IsOpen = Point -> Bool

data Direction
    = North
    | South
    | East
    | West
    | Northeast
    | Southeast
    | Northwest
    | Southwest
    deriving (Show,Eq)

rotate45C :: Direction -> Direction
rotate45C dir = case dir of
    North     -> Northeast
    South     -> Southwest
    East      -> Southeast
    West      -> Northwest
    Northeast -> East
    Southeast -> South
    Northwest -> North
    Southwest -> West

rotate45CC :: Direction -> Direction
rotate45CC dir = case dir of
    North     -> Northwest
    South     -> Southeast
    East      -> Northeast
    West      -> Southwest
    Northeast -> North
    Southeast -> East
    Northwest -> West
    Southwest -> South

rotate90C :: Direction -> Direction
rotate90C dir = case dir of
    North     -> East
    South     -> West
    East      -> South
    West      -> North
    Northeast -> Southeast
    Southeast -> Southwest
    Northwest -> Northeast
    Southwest -> Northwest

rotate90CC :: Direction -> Direction
rotate90CC dir = case dir of
    North     -> West
    South     -> East
    East      -> North
    West      -> South
    Northeast -> Northwest
    Southeast -> Northeast
    Northwest -> Southwest
    Southwest -> Southeast

rotate135C :: Direction -> Direction
rotate135C dir = case dir of
    North     -> Southeast
    South     -> Northwest
    East      -> Southwest
    West      -> Northeast
    Northeast -> South
    Southeast -> West
    Northwest -> East
    Southwest -> North

rotate135CC :: Direction -> Direction
rotate135CC dir = case dir of
    North     -> Southwest
    South     -> Northeast
    East      -> Northwest
    West      -> Southeast
    Northeast -> West
    Southeast -> North
    Northwest -> South
    Southwest -> East

rotate180 :: Direction -> Direction
rotate180 dir = case dir of
    North     -> South
    South     -> North
    East      -> West
    West      -> East
    Northeast -> Southwest
    Southeast -> Northwest
    Northwest -> Southeast
    Southwest -> Northeast

push :: Ord a => PQueue a a -> a -> PQueue a a
push pq a = PQ.insert a a pq

pop :: Ord a => PQueue a a -> Maybe (PQueue a a, a)
pop = fmap swap . PQ.minView

translate :: Int -> Direction -> Point -> Point
translate n dir (x,y) = case dir of
    North     -> (x, y + n)
    South     -> (x, y - n)
    East      -> (x + n, y)
    West      -> (x - n, y)
    Northeast -> (x + n, y + n)
    Southeast -> (x + n, y - n)
    Northwest -> (x - n, y + n)
    Southwest -> (x - n, y - n)

isAxisJump :: Direction -> IsOpen -> Point -> Bool
isAxisJump dir isOpen xy =
    isOpen xy && isOpen e &&
    ((not (isOpen n) && isOpen ne) || (not (isOpen s) && isOpen se))
    where
    n  = translate 1 (rotate90CC dir) xy
    ne = translate 1 (rotate45CC dir) xy
    e  = translate 1 (           dir) xy
    se = translate 1 (rotate45C  dir) xy
    s  = translate 1 (rotate90C  dir) xy


isDiagJump :: Direction -> IsOpen -> Point -> Bool
isDiagJump dir isOpen xy =
    if isOpen xy then
        case (isOpen w, isOpen s) of
            (False, True) -> isOpen n && isOpen nw
            (True, False) -> isOpen e && isOpen se
            _ -> False
    else
        False
    where
    n  = translate 1 (rotate45CC  dir) xy
    s  = translate 1 (rotate135C  dir) xy
    e  = translate 1 (rotate45C   dir) xy
    w  = translate 1 (rotate135CC dir) xy
    nw = translate 1 (rotate90CC  dir) xy
    se = translate 1 (rotate90C   dir) xy

type JumpGetter = Direction -> Point -> Maybe Point

isPathOpenOnAxis :: IsOpen -> Point -> Point -> Bool
isPathOpenOnAxis isOpen start@(sx,sy) goal@(gx,gy) =
    if sx == gx then
        if sy == gy then
            True
        else
            if sy > gy then
                isDirectionOpen South start
            else
                isDirectionOpen North start
    else
        if sy == gy then
            if sx > gx then
                isDirectionOpen West start
            else
                isDirectionOpen East start
        else
            False
    where
    isDirectionOpen dir xy = (xy == goal && isOpen goal) || (isOpen xy && isDirectionOpen dir (translate 1 dir xy))

expandNode :: IsOpen -> JumpGetter -> Point -> Point -> (Direction, Point) -> [(Direction, Point)]
expandNode isOpen jg start goal (nodeDir, xy) =
    if xy == start then
        startNodes
    else
        case nodeDir of
                North -> exAxis nodeDir
                South -> exAxis nodeDir
                East  -> exAxis nodeDir
                West  -> exAxis nodeDir
                _     -> exDiag nodeDir
    where
    startNodes = concat
        [ exDiag Northeast
        , exDiag Southeast
        , exDiag Northwest
        , exDiag Southwest
        ]
    exAxis dir = expandAxis isOpen jg goal dir xy
    exDiag dir = expandDiag isOpen jg goal dir xy

expandAxis :: IsOpen -> JumpGetter -> Point -> Direction -> Point -> [(Direction, Point)]
expandAxis isOpen jg goal dir xy =
    let n     = translate 1             dir  xy
        e     = translate 1 (rotate90C  dir) xy
        w     = translate 1 (rotate90CC dir) xy
        ne    = translate 1 neDir            xy
        nw    = translate 1 nwDir            xy
        nOpen = isOpen n
        eOpen = isOpen e
        wOpen = isOpen w
        neOpen = isOpen ne
        nwOpen = isOpen nw
        neDir = rotate45C dir
        nwDir = rotate45CC dir
        forcedNE = if not eOpen && nOpen && neOpen then Just (neDir, ne) else Nothing
        forcedNW = if not wOpen && nOpen && nwOpen then Just (nwDir, nw) else Nothing
        naturalN = fmap (\p -> (dir, p)) $ jg dir xy in
    if isOpen xy then
        if isPathOpenOnAxis isOpen xy goal then
            [(North, goal)]
        else
            catMaybes [forcedNE, forcedNW, naturalN]
    else
        []

expandDiag :: IsOpen -> JumpGetter -> Point -> Direction -> Point -> [(Direction, Point)]
expandDiag _ _ _ North _ = error "ExpandDiag N"
expandDiag _ _ _ South _ = error "ExpandDiag S"
expandDiag _ _ _ East _ = error "ExpandDiag E"
expandDiag _ _ _ West _ = error "ExpandDiag W"
expandDiag isOpen jg goal dir xy =
    let n     = translate 1 (rotate45CC  dir) xy
        s     = translate 1 (rotate135C  dir) xy
        e     = translate 1 (rotate45C   dir) xy
        w     = translate 1 (rotate135CC dir) xy
        nw    = translate 1 nwDir             xy
        se    = translate 1 seDir             xy
        ne    = translate 1 dir               xy
        nwDir = rotate90CC dir
        seDir = rotate90C  dir
        forcedNW  = if not (isOpen w) && isOpen n && isOpen nw then Just (nwDir, nw) else Nothing
        forcedSE  = if not (isOpen s) && isOpen e && isOpen se then Just (seDir, se) else Nothing
        naturalNE = nextDiagJump isOpen jg goal dir ne
        naturalN  = fmap (\p -> (rotate45CC dir, p)) $ jg (rotate45CC dir) xy
        naturalE  = fmap (\p -> (rotate45C  dir, p)) $ jg (rotate45C  dir) xy in
            if isOpen xy then
                if isPathOpenOnAxis isOpen xy goal then
                    [(North, goal)]
                else
                    catMaybes [naturalNE, forcedNW, forcedSE, naturalN, naturalE]
            else
                []

nextDiagJump :: IsOpen -> JumpGetter -> Point -> Direction -> Point -> Maybe (Direction, Point)
nextDiagJump isOpen jg goal dir xy =
    if isDiagJump dir isOpen xy then
        Just (dir, xy)
    else if isJust naturalN || isJust naturalE then
        Just (dir, xy)
    else if isOpen xy then
        if isPathOpenOnAxis isOpen xy goal then
            Just (dir, xy)
        else if isOpen (translate 1 (rotate45CC dir) xy) || isOpen (translate 1 (rotate45C dir) xy) then
            nextDiagJump isOpen jg goal dir $ translate 1 dir xy
        else
            Nothing
    else
        Nothing
    where
    naturalN = jg (rotate45CC dir) xy
    naturalE = jg (rotate45C  dir) xy

data Node = Node
    { _g :: !Double
    , _h :: !Double
    , _f :: !Double
    , _dir :: !Direction
    , _xy :: !Point
    } deriving (Eq,Show)

instance Ord Node where
    compare n1 n2 =
        if isDiag (_dir n1) && not (isDiag $ _dir n2) then
            LT
        else
            _f n1 `compare` _f n2
        where
        isDiag d = case d of
            Northwest -> True
            Southwest -> True
            Northeast -> True
            Southeast -> True
            _         -> False

jpsPath :: IsOpen -> JumpGetter -> Point -> Point -> Maybe [Point]
jpsPath isOpen jg start goal = runJPS isOpen jg (PQ.singleton startPoint startPoint) Map.empty start goal
    where
    dist = getDist start goal
    startPoint = Node 0 dist dist North start

getDist :: Point -> Point -> Double
getDist (x1,y1) (x2,y2) = sqrt $ fromIntegral $ (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2)

runJPS :: IsOpen -> JumpGetter -> PQueue Node Node -> Map Point Point -> Point -> Point -> Maybe [Point]
runJPS isOpen jg open closed start goal =
    case pop open of
        Nothing -> Nothing
        Just (open2, node) ->
            if _xy node == goal then
                Just (reconstruct closed goal)
            else
                let neighbors = expandNode isOpen jg start goal (_dir node, _xy node)
                    nearby = filter (\(_, xy) -> Map.notMember xy closed) neighbors
                    open3 = foldl (\pq (dir, xy) ->
                            let adjDist = getDist xy (_xy node)
                                g = _g node + adjDist
                                h = getDist xy goal
                                f = g + h in
                            push pq $ Node {
                                _g = g,
                                _h = h,
                                _f = f,
                                _dir = dir,
                                _xy = xy
                            }
                        ) open2 nearby
                    closed2 = foldl (\clsd (_,xy2) -> Map.insert xy2 (_xy node) clsd) closed nearby in
                runJPS isOpen jg open3 closed2 start goal

reconstruct :: Map Point Point -> Point -> [Point]
reconstruct = recon []
    where
    recon :: [Point] -> Map Point Point -> Point -> [Point]
    recon xs im b = case Map.lookup b im of
        Just ok -> recon (b : xs) im ok
        Nothing -> b : xs
