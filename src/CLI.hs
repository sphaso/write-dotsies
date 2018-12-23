{-# LANGUAGE LambdaCase #-}

module CLI where

import Data.Char (toLower)
import qualified Termbox

import qualified Core

run :: IO ()
run =
  Termbox.main $ loop Core.initState

loop :: Core.State -> IO ()
loop state = do
  Termbox.clear mempty mempty
  Termbox.hideCursor

  render state
  renderCursor state

  Termbox.flush

  cmd <- Termbox.poll
  case translateEvent cmd of
    Core.Quit -> pure ()
    anyCmd    -> loop $ Core.update state anyCmd

translateEvent :: Termbox.Event -> Core.Cmd
translateEvent =
  \case
    Termbox.EventKey Termbox.KeyCtrlC _ ->
        Core.Quit

    Termbox.EventKey Termbox.KeyTab _ ->
        Core.Toggle

    Termbox.EventKey Termbox.KeySpace _ ->
        Core.Key ' '

    Termbox.EventKey Termbox.KeyEnter _ ->
        Core.Newline

    Termbox.EventKey Termbox.KeyBackspace2 _ ->
        Core.Backspace

    Termbox.EventKey (Termbox.KeyChar key) _ ->
        Core.Key key

    _ ->
        Core.Ignore

setCell :: Bool -> Int -> (Char, Int) -> IO ()
setCell clearview offset (c, x) =
    let
        cells = if clearview then
                    Core.clearChar c
                else
                    Core.translate (toLower c)
    in
        mapM_
          (\(cc, i) -> Termbox.set x i cc)
          (zip cells [offset..])

drawCell :: Bool -> Int -> [(Char, Int)] -> IO ()
drawCell _ _ [] = pure ()
drawCell b offset (('\n', _):xs) = drawCell b (offset + 7) (zip (map fst xs) [0..])
drawCell b offset (x:xs)         = setCell b offset x >> drawCell b offset xs

render :: Core.State -> IO ()
render (Core.State m s _) = drawCell (m == Core.Human) 0 (zip s [0..])

renderCursor :: Core.State -> IO ()
renderCursor (Core.State _ _ (cx, cy)) =
  Termbox.set cx cy $ Termbox.Cell '@' Termbox.yellow mempty
