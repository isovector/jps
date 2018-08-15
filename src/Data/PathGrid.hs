{-# OPTIONS_GHC -Wall #-}

module Data.PathGrid
  ( JumpGrid ()
  , make
  , closeArea
  , openArea
  , findPath
  , isTileOpen
  ) where

import qualified Data.Pathing        as P
import qualified Data.Vector.Unboxed as V
import           Data.Pathing (Point, Direction(North, South, East, West))
import           Data.Vector.Unboxed (Vector)
import           Data.Word (Word16)

{-
16 bits should be enough to encode the distance of any jump.
If you want a map larger than 65536x65536 you have a problem.
A value of 0 means there is no jump from that point.
-}
type Jumps =
    ( Word16 -- North jump distance
    , Word16 -- East  jump distance
    , Word16 -- South jump distance
    , Word16 -- West  jump distance
    )

data JumpGrid = JumpGrid
    !Int -- Width
    !Int -- Height
    !(Vector Bool) -- indexed by (y * w + x)
    !(Vector Jumps) -- indexed by (y * w + x)

data JumpChange = JumpChange
    !Direction
    !Word16
    !Int
    !Int

-- Creates a grid with all nodes open.
make :: (Int,Int) -> JumpGrid
make (w,h) = JumpGrid w h vec jumps
    where
    jumps = V.replicate (w * h) (0,0,0,0)
    vec   = V.replicate (w * h) True

closeArea :: Point -> Point -> JumpGrid -> JumpGrid
closeArea = changeArea False

openArea :: Point -> Point -> JumpGrid -> JumpGrid
openArea = changeArea True

findPath :: JumpGrid -> Point -> Point -> Maybe [Point]
findPath jg start goal = P.jpsPath (isTileOpen jg) (readJumpSafe jg) start goal

isTileOpen :: JumpGrid -> Point -> Bool
isTileOpen jg@(JumpGrid w _ tiles _) xy@(x, y) =
    if isDefinedOn jg xy then
        V.unsafeIndex tiles (y * w + x)
    else
        False

readTranslateJumpSafe :: JumpGrid -> Direction -> Point -> Word16 -> Maybe Point
readTranslateJumpSafe jg dir xy n =
    if isDefinedOn jg xy then
        if n > 0 then
            let jump = P.translate (fromIntegral n) dir xy in
            if isDefinedOn jg jump then
                Just jump
            else
                Nothing
                -- error $ "readTranslateJumpSafe: Bad jump from " ++ show xy ++ " to " ++ show jump
        else
            Nothing
    else
        error $ "readTranslateJumpSafe: " ++ show xy ++ " is out of bounds"

readJumpsSafe :: JumpGrid -> Point -> Jumps
readJumpsSafe jg xy =
    if isDefinedOn jg xy then
        readJumpsUnsafe jg xy
    else
        error $ "readJumpsSafe: " ++ show xy ++ " is out of bounds"

readJumpsUnsafe :: JumpGrid -> Point -> Jumps
readJumpsUnsafe (JumpGrid w _ _ jumps) (x,y) = V.unsafeIndex jumps (y * w + x)

readNorthJumpSafe :: JumpGrid -> Point -> Maybe Point
readNorthJumpSafe jg xy =
    let (nj,_,_,_) = readJumpsSafe jg xy in
    readTranslateJumpSafe jg P.North xy nj

readSouthJumpSafe :: JumpGrid -> Point -> Maybe Point
readSouthJumpSafe jg xy =
    let (_,_,sj,_) = readJumpsSafe jg xy in
    readTranslateJumpSafe jg P.South xy sj

readEastJumpSafe :: JumpGrid -> Point -> Maybe Point
readEastJumpSafe jg xy =
    let (_,ej,_,_) = readJumpsSafe jg xy in
    readTranslateJumpSafe jg P.East xy ej

readWestJumpSafe :: JumpGrid -> Point -> Maybe Point
readWestJumpSafe jg xy =
    let (_,_,_,wj) = readJumpsSafe jg xy in
    readTranslateJumpSafe jg P.West xy wj

readJumpSafe :: JumpGrid -> Direction -> Point -> Maybe Point
readJumpSafe jg dir xy =
    if isDefinedOn jg xy then
        case dir of
            P.North -> readNorthJumpSafe jg xy
            P.South -> readSouthJumpSafe jg xy
            P.East  -> readEastJumpSafe  jg xy
            P.West  -> readWestJumpSafe  jg xy
            _     -> error "readJumpSafe doesn't accept diagonal directions"
    else
        Nothing

-- Is the point within the JumpGrid or out of bounds?
isDefinedOn :: JumpGrid -> Point -> Bool
isDefinedOn (JumpGrid w h _ _) (x,y) =
    x < w && y < h && x >= 0 && y >= 0

setJumpChange :: Jumps -> JumpChange -> Jumps
setJumpChange (nj,ej,sj,wj) (JumpChange dir dist _ _) =
    case dir of
        North -> (dist, ej, sj, wj)
        South -> (nj, ej, dist, wj)
        East  -> (nj, dist, sj, wj)
        West  -> (nj, ej, sj, dist)
        other -> error $ "setJumpChange received a bad Direction: " ++ show other

incorporateJumpChanges :: JumpGrid -> [JumpChange] -> V.Vector Jumps
incorporateJumpChanges (JumpGrid w _ _ jumps) =
    V.unsafeAccum setJumpChange jumps . map (\jc@(JumpChange _ _ x y) -> (w * y + x, jc))

-- Get the vertical or horizontal distance between two points.
-- Assumes they are aligned along a column or row.
-- Fails if they aren't aligned.
axisDist :: Point -> Point -> Int
axisDist xya@(xa,ya) xyb@(xb,yb) =
    if xa == xb
    then abs $ ya - yb
    else
    if ya == yb
    then abs $ xa - xb
    else error $ "axisDist failed on " ++ show xya ++ " & " ++ show xyb

jumpChange :: Direction -> Point -> Point -> JumpChange
jumpChange dir jump xy@(x,y) =
    let dist = fromIntegral $ axisDist xy jump in
    case dir of
        North -> JumpChange North dist x y
        South -> JumpChange South dist x y
        East  -> JumpChange East  dist x y
        West  -> JumpChange West  dist x y
        other -> error ("No jump for " ++ show other ++ " at " ++ show xy)

isAxisJump :: JumpGrid -> Direction -> Point -> Bool
isAxisJump jg dir xy = P.isAxisJump dir (isTileOpen jg) xy

accum :: JumpGrid -> Direction -> Direction -> Point -> (Maybe Point, [Point])
accum jg jumpDir dir start = acc (P.translate 1 dir start) []
    where
    acc :: Point -> [Point] -> (Maybe Point, [Point])
    acc xy xys =
        if isAxisJump jg jumpDir xy then
            (Just xy, xy:xys)
        else
            if isTileOpen jg xy then
                acc (P.translate 1 dir xy) (xy:xys)
            else
                (Nothing, xys)

changeArea :: Bool -> Point -> Point -> JumpGrid -> JumpGrid
changeArea openOrClose (xa,ya) (xb,yb) (JumpGrid w h tiles jumps) =
    JumpGrid w h newTiles fixedJumps
    where
    xMax = max xa xb
    xMin = min xa xb
    yMax = max ya yb
    yMin = min ya yb

    affected = [(x,y) | x <- [xMin - 1 .. xMax + 1], y <- [yMin - 1 .. yMax + 1]]
    specified = [(x,y) | x <- [xMin .. xMax], y <- [yMin .. yMax]]
    newTiles = (V.//) tiles $ map (\(x,y) -> (y * w + x, openOrClose)) specified

    jg = JumpGrid w h newTiles jumps

    clearClosed :: [JumpChange]
    clearClosed = concat $ map (\dir -> map (\xy -> jumpChange dir xy xy) specified) [North,South,East,West]

    correctJumps :: [JumpChange]
    correctJumps = concat $ concat $ map (\xy -> map (\dir -> correctJump jg dir xy) [North,South,East,West]) affected

    fixedJumps = incorporateJumpChanges jg $ if openOrClose then correctJumps else correctJumps ++ clearClosed

correctJump :: JumpGrid -> Direction -> Point -> [JumpChange]
correctJump jg dir xy =
    case accum jg dir (P.rotate180 dir) xy of
        (Just backJump, _) ->
            case accum jg dir dir backJump of
                (Just frwdJump, _:xs) -> map (jumpChange dir frwdJump) (backJump:xs)
                (_, xs) -> map (\p -> jumpChange dir p p) (backJump:xs)
        (_, lastP:_) ->
            case accum jg dir dir lastP of
                (Just frwdJump, _:xs) -> map (jumpChange dir frwdJump) (lastP:xs)
                (_, xs) -> map (\p -> jumpChange dir p p) (lastP:xs)
        _ -> []
