{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE CPP             #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Algorithm.Search.JumpPoint
  ( JumpGrid ()
  , Point
  , make
  , closeArea
  , openArea
  , findPath
  , isTileOpen
  ) where

import           Algorithm.Search.JumpPoint.Pathing (Point, Direction(..))
import qualified Algorithm.Search.JumpPoint.Pathing as P
import           Control.Lens
#if !(MIN_VERSION_base(4,11,1))
import           Data.Semigroup
#endif
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Word (Word16)

{-
16 bits should be enough to encode the distance of any jump.
If you want a map larger than 65536x65536 you have a problem.
A value of 0 means there is no jump from that point.
-}
data Jumps = Jumps
  { _nJumpDist :: {-# UNPACK #-} !Word16
  , _eJumpDist :: {-# UNPACK #-} !Word16
  , _sJumpDist :: {-# UNPACK #-} !Word16
  , _wJumpDist :: {-# UNPACK #-} !Word16
  }

makeLenses ''Jumps

instance Semigroup Jumps where
  Jumps na ea sa wa <> Jumps nb eb sb wb =
    Jumps (na + nb)
          (ea + eb)
          (sa + sb)
          (wa + wb)


instance Monoid Jumps where
  mempty = Jumps 0 0 0 0
  mappend = (<>)


------------------------------------------------------------------------------
-- | The pathfinding grid.
data JumpGrid = JumpGrid
  { jgWidth  :: {-# UNPACK #-} !Int -- Width
  , jgHeight :: {-# UNPACK #-} !Int -- Height
  , jgTiles  :: {-# UNPACK #-} !(Vector Bool) -- indexed by (y * w + x)
  , jgJumps  :: {-# UNPACK #-} !(Vector Jumps) -- indexed by (y * w + x)
  }

data JumpChange = JumpChange
  !Direction
  !Word16
  !(Int, Int)


------------------------------------------------------------------------------
-- | Creates a 'JumpGrid' with all nodes open.
make
    :: Int  -- ^ Grid width
    -> Int  -- ^ Grid height
    -> JumpGrid
make w h = JumpGrid w h vec jumps
  where
  jumps = V.replicate (w * h) mempty
  vec   = V.replicate (w * h) True


------------------------------------------------------------------------------
-- | Close a square defined by two points on the 'JumpGrid'.
closeArea
    :: Point  -- ^ First point
    -> Point  -- ^ Second point
    -> JumpGrid
    -> JumpGrid
closeArea = changeArea False


------------------------------------------------------------------------------
-- | Open a square defined by two points on the 'JumpGrid'.
openArea
    :: Point  -- ^ First point
    -> Point  -- ^ Second point
    -> JumpGrid
    -> JumpGrid
openArea = changeArea True


------------------------------------------------------------------------------
-- | Attempt to pathfind over a 'JumpGrid'.
findPath
    :: JumpGrid  -- ^ Grid
    -> Point     -- ^ Source location
    -> Point     -- ^ Destination location
    -> Maybe [Point]
findPath jg start goal = P.jpsPath (isTileOpen jg) (readJumpSafe jg) start goal


------------------------------------------------------------------------------
-- | Returns 'True' iff a point on the grid is open.
isTileOpen
    :: JumpGrid
    -> Point
    -> Bool
isTileOpen jg xy =
  if isDefinedOn jg xy then
    V.unsafeIndex (jgTiles jg) $ indexOf jg xy
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

indexOf :: JumpGrid -> Point -> Int
indexOf jg (x, y) = y * jgWidth jg + x

readJumpsUnsafe :: JumpGrid -> Point -> Jumps
readJumpsUnsafe jg@(JumpGrid _ _ _ jumps) p =
  V.unsafeIndex jumps $ indexOf jg p

dirToLens :: Direction -> Lens' Jumps Word16
dirToLens P.North = nJumpDist
dirToLens P.South = sJumpDist
dirToLens P.East  = eJumpDist
dirToLens P.West  = wJumpDist
dirToLens _ = error "dirToLens doesn't accept diagonal directions"

readJumpSafe :: JumpGrid -> Direction -> Point -> Maybe Point
readJumpSafe jg dir xy =
  if isDefinedOn jg xy then
    readTranslateJumpSafe jg dir xy
    . view (dirToLens dir)
    $ readJumpsSafe jg xy
  else
    Nothing

-- Is the point within the JumpGrid or out of bounds?
isDefinedOn :: JumpGrid -> Point -> Bool
isDefinedOn jg (x,y) =
  x < jgWidth jg && y < jgHeight jg && x >= 0 && y >= 0

setJumpChange :: Jumps -> JumpChange -> Jumps
setJumpChange jumps (JumpChange dir dist _) =
  jumps & dirToLens dir .~ dist

incorporateJumpChanges :: JumpGrid -> [JumpChange] -> V.Vector Jumps
incorporateJumpChanges jg
  = V.unsafeAccum setJumpChange (jgJumps jg)
  . map (\jc@(JumpChange _ _ xy) -> (indexOf jg xy, jc))

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
jumpChange dir jump xy =
  let dist = fromIntegral $ axisDist xy jump in
  case dir of
    North -> JumpChange North dist xy
    South -> JumpChange South dist xy
    East  -> JumpChange East  dist xy
    West  -> JumpChange West  dist xy
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

minAndMax :: Ord a => a -> a -> (a, a)
minAndMax a b = (min a b, max a b)

changeArea :: Bool -> Point -> Point -> JumpGrid -> JumpGrid
changeArea openOrClose (xa,ya) (xb,yb) (JumpGrid w h tiles jumps) =
  JumpGrid w h newTiles fixedJumps
  where
  (xMin, xMax) = minAndMax xa xb
  (yMin, yMax) = minAndMax ya yb

  affected = [(x,y) | x <- [xMin - 1 .. xMax + 1], y <- [yMin - 1 .. yMax + 1]]
  specified = [(x,y) | x <- [xMin .. xMax], y <- [yMin .. yMax]]
  newTiles = (V.//) tiles $ map (\(x,y) -> (y * w + x, openOrClose)) specified

  jg = JumpGrid w h newTiles jumps

  clearClosed :: [JumpChange]
  clearClosed = do
    xy <- specified
    dir <- [North, South, East, West]
    pure $ jumpChange dir xy xy

  correctJumps :: [JumpChange]
  correctJumps = do
    xy  <- affected
    dir <- [North, South, East, West]
    correctJump jg dir xy

  fixedJumps =
    incorporateJumpChanges jg $
    if openOrClose
       then correctJumps
       else correctJumps ++ clearClosed

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

