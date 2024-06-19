module Main where

import Control.Monad (when, void, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Lens
import Control.Concurrent
import Brick (EventM, Widget, str)
import Brick                         qualified as B
import Brick.Main (customMain, App (..))
import Brick.Main                    qualified as BM
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border          qualified as BB
import Brick.Widgets.Border.Style    qualified as BS
import Brick.Widgets.Center          qualified as C
import Graphics.Vty                  qualified as V
import Graphics.Vty.CrossPlatform    qualified as V
import Graphics.Vty.Attributes.Color qualified as VC
import Termite.State
import Termite.Event
import Swarm.TUI.View (chooseCursor)
import Swarm.TUI.View qualified as TV
import Swarm.TUI.Model (uiState)
import Swarm.TUI.Model               qualified as M
import Swarm.TUI.Model.Name (Name)
import Swarm.TUI.Model.UI (uiAttrMap)

app :: App AppState AppEvent Name 
app =
    App
        { appDraw = drawUI
        , appChooseCursor = findCursor
        , appHandleEvent = eventHandler
        , appStartEvent = enablePasteMode
        , appAttrMap = view $ (swarmState . uiState) . uiAttrMap
        }

findCursor :: AppState -> [B.CursorLocation Name] -> Maybe (B.CursorLocation Name)
findCursor _ [] = Nothing
findCursor _ [c] = Just c
findCursor appState (c : cs) = if view (termiteState . showSwarm) appState
    then chooseCursor (appState ^. swarmState) (c : cs)
    else Just c

drawUI :: AppState -> [Widget Name]
drawUI state = if view (termiteState . showSwarm) state
    then TV.drawUI $ state ^. swarmState
    else [B.withBorderStyle BS.unicode $ BB.borderWithLabel (str "Termite") $ C.center (str "Hello World!")]

enablePasteMode :: EventM n s ()
enablePasteMode = do
  vty <- BM.getVtyHandle
  let output = V.outputIface vty
  when (V.supportsMode output V.BracketedPaste) $
    liftIO $
        V.setMode output V.BracketedPaste True

main :: IO ()
main = do
    let buildVty = V.mkVty V.defaultConfig {V.configPreferredColorMode = Just VC.FullColor}
    vty <- buildVty

    V.setMode (V.outputIface vty) V.Mouse True

    appState <- defaultAppState

    chan <- newBChan 5
    _ <- forkIO $
        forever $ do
            threadDelay 33_333 -- cap maximum framerate at 30 FPS
            writeBChan chan $ SEvent M.Frame

    void $ customMain vty buildVty (Just chan) app appState