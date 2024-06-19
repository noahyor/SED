{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Termite.Event (
    eventHandler,
    AppEvent (..),
    TermiteEvent (..),
    SwarmEvent,
) where

import Brick (BrickEvent (VtyEvent), EventM)
import Brick qualified as B
import Brick.Main (halt)
import Brick.Types (zoom)
import Graphics.Vty
import Control.Lens hiding (zoom)
import Control.Monad.State.Class
import Termite.State
import Swarm.TUI.Model qualified as M
import Swarm.TUI.Model.Name (Name)
import Swarm.TUI.Controller qualified as C

data AppEvent = TEvent TermiteEvent | SEvent SwarmEvent

newtype TermiteEvent = TermiteEvent ()
type SwarmEvent = M.AppEvent

termiteEventHandler :: BrickEvent Name AppEvent -> EventM Name AppState ()
termiteEventHandler = \case
    _ -> return mempty

eventHandler :: BrickEvent Name AppEvent -> EventM Name AppState ()
eventHandler event = case globalEventHandler event of
    Right act -> act
    Left e -> decideHandler e

decideHandler :: BrickEvent Name AppEvent -> EventM Name AppState ()
decideHandler event = case splitEvent event of
    Left (SEvent sE) -> zoom swarmState $ C.handleEvent $ B.AppEvent sE
    Left (TEvent tE) -> termiteEventHandler $ B.AppEvent (TEvent tE)
    Right e -> decideHandlerState e

globalEventHandler :: BrickEvent Name AppEvent -> Either (BrickEvent Name AppEvent) (EventM Name AppState ())
globalEventHandler = \case
    VtyEvent (EvKey (KChar 's') [MCtrl]) -> Right $ modify $ over (termiteState . showSwarm) not
    VtyEvent (EvKey (KChar 'q') [MMeta]) -> Right halt
    e -> Left e

decideHandlerState :: (forall a. BrickEvent Name a) -> EventM Name AppState ()
decideHandlerState e = get >>= \s ->
    if   s^.termiteState.showSwarm
    then zoom swarmState $ C.handleEvent e
    else termiteEventHandler e

splitEvent :: BrickEvent Name AppEvent -> Either AppEvent (forall a. BrickEvent Name a)
splitEvent = \case
    B.VtyEvent e -> Right $ B.VtyEvent e
    B.MouseDown n b m l -> Right $ B.MouseDown n b m l
    B.MouseUp n b l -> Right $ B.MouseUp n b l
    B.AppEvent e -> Left e
