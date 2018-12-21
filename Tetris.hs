-- | The Tetris game (main module)
module Main where
import ConsoleGUI       -- cabal install ansi-terminal 
--import CodeWorldGUI     -- cabal install codeworld-api
import Shapes
import Data.List(transpose)

--------------------------------------------------------------------------------
-- * The code that puts all the piece together

main = runGame tetrisGame

tetrisGame = Game { startGame     = startTetris,
                    stepGame      = stepTetris,
                    drawGame      = drawTetris,
                    gameInfo      = defaultGameInfo prop_Tetris,
                    tickDelay     = defaultDelay,
                    gameInvariant = prop_Tetris }

--------------------------------------------------------------------------------
-- * The various parts of the Tetris game implementation


-- | The state of the game
data Tetris = Tetris (Vector,Shape) Shape [Shape]
-- The state consists of three parts:
--   * The position and shape of the falling piece
--   * The well (the playing field), where the falling pieces pile up
--   * An infinite supply of random shapes

-- ** Positions and sizes

type Vector = (Int,Int)

-- | The size of the well
wellSize :: (Int,Int)
wellSize = (wellWidth,wellHeight)
wellWidth = 30
wellHeight = 50

-- | Starting position for falling pieces
startPosition :: Vector
startPosition = (wellWidth `div` 2 - 2, 0)

-- | Vector addition
vAdd :: Vector -> Vector -> Vector
(x1,y1) `vAdd` (x2,y2) = (x1+x2,y1+y2)

-- | Move the falling piece into position
place :: (Vector,Shape) -> Shape
place (v,s) = shiftShape v s


-- | An invariant that startTetris and stepTetris should uphold
-- prop_Shape is defined in Shapes.hs
prop_Tetris :: Tetris -> Bool
prop_Tetris (Tetris (_,p) w _) = prop_Shape p && prop_Well w
  where prop_Well w = shapeSize w == wellSize


-- | Add black walls around a shape
addWalls :: Shape -> Shape
addWalls s = iterate (rotateShape . addWall) s !! 4
  where addWall (S (s:ss)) = S $ (s:ss) ++ [createWall $ length s]
        createWall l = replicate l (Just Black)


-- | Visualize the current game state. This is what the user will see
-- when playing the game.
drawTetris :: Tetris -> Shape
drawTetris (Tetris (v,p) w _) = addWalls (combine w (place (v, p)))


-- | The initial game state
startTetris :: [Double] -> Tetris
startTetris rs = Tetris (startPosition,shape1) (emptyShape wellSize) supply
  where
    shape1:supply = map (allShapes !!) (chooseShape rs)

chooseShape :: [Double] -> [Int]
chooseShape = map (\ x -> floor (fromIntegral(length allShapes) * x))

-- | React to input. The function returns 'Nothing' when it's game over,
-- and @'Just' (n,t)@, when the game continues in a new state @t@.
stepTetris :: Action -> Tetris -> Maybe (Int,Tetris)
stepTetris Tick t = tick t
stepTetris MoveLeft t = Just (0, movePiece (-1) t)
stepTetris MoveRight t = Just (0, movePiece 1 t)
stepTetris MoveDown t = tick t
stepTetris Rotate t = Just (0, rotatePiece t)
stepTetris Compress t = Just (0, compressPiece t)

-- constants (only values of (x,y) actually needed)
left  = (-1,0)
right = (1,0)
down  = (0,1)

-- | Handles the tick action
tick :: Tetris -> Maybe (Int, Tetris)
tick t | collision t' = dropNewPiece t
       | otherwise = Just (0, move (0, 1) t)
          where t' = move (0, 1) t


-- | Detects collisions
collision :: Tetris -> Bool
collision t = tooFarDown t 
           || overlapsWithWell t 
           || tooFarLeft t 
           || tooFarRight t

tooFarRight :: Tetris -> Bool
tooFarRight (Tetris ((x, _), p) w _) = x + fst (shapeSize p) > wellWidth

tooFarLeft :: Tetris -> Bool
tooFarLeft (Tetris ((x, y), p) w _) = x < 0

tooFarDown :: Tetris -> Bool
tooFarDown (Tetris ((_, y), p) w _) = y + snd (shapeSize p) > wellHeight

overlapsWithWell :: Tetris -> Bool
overlapsWithWell (Tetris (v, p) w i) = shiftShape v p `overlaps` w

-- | Move the falling piece
move :: Vector -> Tetris -> Tetris
move (x,y) (Tetris ((vx, vy),p) w i) = Tetris ((x + vx, y + vy), p) w i

movePiece :: Int -> Tetris -> Tetris
movePiece x t | not $ collision moved = moved
              | otherwise = t
                where moved = move (x, 0) t

-- | Rotate the falling piece
rotate :: Tetris -> Tetris
rotate (Tetris (v,p) w s) = 
  Tetris (v, rotateShape p) w s

rotatePiece :: Tetris -> Tetris
rotatePiece t = 
  if not $ collision t'
    then t'
    else t
      where t' = rotate t

-- | Compress the falling piece
compress :: Tetris -> Tetris
compress (Tetris (v,p) w s) =
  Tetris (v, compressShape p) w s

compressPiece :: Tetris -> Tetris
compressPiece t = 
  if not $ collision t'
    then t'
    else t
      where t' = compress t


-- | Drops a new piece when the previous one cannot move down further
dropNewPiece :: Tetris -> Maybe (Int, Tetris)
dropNewPiece (Tetris (v, p) w (s:ss))
  | overlaps w' (place p') = Nothing
  | otherwise              = Just (sc, Tetris p' w' ss)
    where (sc, w') = clearLines $ combine (place (v, p)) w
          p'       = (startPosition, s)

clearLines :: Shape -> (Int, Shape)
clearLines s = (n, shiftShape (0, n) s')
   where n = snd (shapeSize s) - snd (shapeSize s')
         s' = clearLines' s
         clearLines' s = S $ filter (not . isComplete) (rows s)

isComplete :: Row -> Bool
isComplete = notElem Nothing