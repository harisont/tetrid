-- | Common interface between the game and the various user interfaces modules
module GameInterface where
import Shapes(Shape)

-- | Bookkeeping
data Book = Book { score,rowCount::Int }

updateBook :: Int -> Book -> Book
updateBook rows (Book score rowCount) = Book score' (rowCount+rows)
  where
    score' = score + 100*rows*rows

-- * User interface

-- | Interface to the game implementation
data Game state =
     Game { startGame    :: [Double] -> state,
                         -- ^ Initial state
            stepGame     :: Action -> state -> Maybe (Int,state),
                         -- ^ @Nothing@ ends the game, @Just@ continues.
                         -- The @Int@ is the number of rows that were cleared
            drawGame     :: state -> Shape,
                         -- ^ Visualize the game state
            gameInfo     :: state -> Book -> [String],
                         -- ^ Extra info to show to the user
            tickDelay    :: state -> Book -> Int,
                         -- ^ Delay between @Tick@ steps
            gameInvariant :: state -> Bool
                          -- ^ for run-time verification
          }

-- | A type for the actions that advances the game (user input and timer ticks)
data Action = Tick | MoveLeft | MoveRight | MoveDown | Rotate | Compress
              deriving (Eq,Read,Show,Bounded,Enum)

-- | Extra info that the user interface may display
defaultGameInfo invariant t b =
  [show (score b)++" points",
   show (rowCount b)++" rows",
   --if rowCount t>0 then show (score t `div` rowCount t)++" ppr" else "",
   if invariant t then "                          "
                  else "TETRIS INVARIANT VIOLATION"]


-- | The delay between ticks (in milliseconds) as a function of
-- of current game state
defaultDelay :: state -> Book -> Int
defaultDelay t b = round (500*0.8^(rowCount b `div` 10)) `max` 100
