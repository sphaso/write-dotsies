{-# LANGUAGE LambdaCase #-}

module Core where

import qualified Termbox

type Point = (Int, Int)

data Visualization = Dotsies | Human deriving (Eq)

data State =
  State {
      mode :: Visualization
    , text :: String
    , cursor :: Point
  }

data Cmd
  = Key Char
  | Backspace
  | Newline
  | Toggle
  | Ignore
  | Quit

initState :: State
initState = State Dotsies "" (0, 0)

update :: State -> Cmd -> State
update i@(State m s p@(x, y)) = \case
  Key c ->
      State m (s ++ [c]) (x + 1, y)

  Backspace ->
      State m (backspace s) (x - 1, y)

  Newline ->
      State m (s ++ "\n") (x, y + 7)

  Toggle ->
      State (toggle m) s p

  Ignore ->
      i

  Quit ->
      i

backspace :: String -> String
backspace "" = ""
backspace str = init str

toggle :: Visualization -> Visualization
toggle Dotsies = Human
toggle Human = Dotsies

on :: Termbox.Cell
on = Termbox.Cell ' ' mempty Termbox.magenta

off :: Termbox.Cell
off = Termbox.Cell ' ' mempty mempty

clearChar :: Char -> [Termbox.Cell]
clearChar c = [Termbox.Cell c mempty Termbox.blue]

translate :: Char -> [Termbox.Cell]
translate 'a' = [on]
translate 'b' = [off, on]
translate 'c' = [off, off, on]
translate 'd' = [off, off, off, on]
translate 'e' = [off, off, off, off, on]
translate 'f' = [on, on]
translate 'g' = [off, on, on]
translate 'h' = [off, off, on, on]
translate 'i' = [off, off, off, on, on]
translate 'j' = [on, off, on]
translate 'k' = [off, on, off, on]
translate 'l' = [off, off, on, off, on]
translate 'm' = [on, off, off, on]
translate 'n' = [off, on, off, off, on]
translate 'o' = [on, off, off, off, on]
translate 'p' = [on, on, on]
translate 'q' = [on, on, off, on]
translate 'r' = [on, off, on, on]
translate 's' = [off, on, on, on]
translate 't' = [off, on, on, off, on]
translate 'u' = [off, on, off, on, on]
translate 'v' = [off, off, on, on, on]
translate 'w' = [on, on, off, off, on]
translate 'x' = [on, off, on, off, on]
translate 'y' = [on, off, off, on, on]
translate 'z' = [on, on, off, on, on]
translate  c  = [Termbox.Cell c Termbox.blue mempty]
