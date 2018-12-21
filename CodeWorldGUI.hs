-- | Run a game using the the "CodeWorld" API.
-- Open the game in your web browser.
module CodeWorldGUI(module GameInterface,runGame) where
import Data.Text(pack,unpack)
import System.Random(randomRs,newStdGen)
import CodeWorld
import GameInterface
import Shapes(rows)

-- | Run a game
runGame g =
     do world0 <- startGame g . randomRs (0,1) <$> newStdGen
        interactionOf (0,(Right world0,book0)) time event draw
  where
    draw (t,(st,book))= either gameOver running st
      where
        running state  = renderGame state book
        gameOver state = renderGame state book & at (5,0) (string "Game over")

    renderGame state book = render (drawGame g state,gameInfo g state book)
    book0 = Book 0 0

    time dt (t0,(Right state,book))
            | t1>=delay = (t1-delay,stepGame' Tick state book)
      where
        t1=t0+dt
        delay = fromIntegral (tickDelay g state book)/1000
    time dt (t0,st) = (t0+dt,st)

    
    event e w@(t,(Right state,book)) =
        case readAction e of
          Just a -> (t,stepGame' a state book)
          _ -> w
    event _ w = w

    stepGame' a t b = maybe end continue (stepGame g a t)
      where
        end = (Left t,b)
        continue (n,t) = (Right t,updateBook n b)

readAction e =
  case e of
    KeyPress txt ->
      case unpack txt of
        " "     -> Just MoveDown
        "Left"  -> Just MoveLeft
        "Right" -> Just MoveRight 
        "Up"    -> Just Rotate
        "Down"  -> Just MoveDown
        "J"     -> Just MoveLeft
        "L"     -> Just MoveRight 
        "K"     -> Just Rotate
--      "P"     -> Just Pause
--      "Q"     -> Just Quit
        _       -> Nothing
    _ -> Nothing


render (game,info) = translated (-7) 9 . scaled 0.8 0.8 $
                     renderGame game & renderInfo info

renderInfo = foldr (&) blank . zipWith renderMessage [0..]
renderMessage y = at (15,y) . string

renderGame = foldr (&) blank . zipWith renderRow [0..] . rows
renderRow y = foldr (&) blank . zipWith (renderBlock y) [0..]
renderBlock y x Nothing  = blank
renderBlock y x (Just c) = at (x,y) (block c)

block c   = colored (colors!!fromEnum c) (solidRectangle 1 1)
            & rectangle 1 1

colors = [black,red,green,yellow,blue,magenta,cyan,grey 0.7]

string = text . pack
at (x,y) = translated (fromIntegral x) (-fromIntegral y)
