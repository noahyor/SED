{-# LANGUAGE TemplateHaskell #-}

module Termite.State (
    AppState (..),
    TermiteState (..),
    SwarmState,
    defaultTermiteState,
    defaultSwarmState,
    defaultAppState,

    -- Lenses
    showSwarm,
    termiteState,
    swarmState,
) where

import Swarm.TUI.Model qualified as M
import Swarm.TUI.Model.StateUpdate (initAppState)
import Control.Carrier.Lift (runM)
import Control.Carrier.Throw.Either (runThrow)
import Control.Lens
import Data.Coerce
import Swarm.Game.Failure (SystemFailure)
import Swarm.Language.Pretty (prettyString)

data AppState = AppState {
    _termiteState :: TermiteState,
    _swarmState :: SwarmState
    }

type SwarmState = M.AppState

newtype TermiteState = TermiteState {
    _showSwarm :: Bool
}

makeLenses ''TermiteState
makeLenses ''AppState

defaultTermiteState :: TermiteState
defaultTermiteState = TermiteState {
    _showSwarm = False
}

defaultSwarmState :: IO M.AppState
defaultSwarmState = do
    res <- runM . runThrow $ initAppState M.defaultAppOpts
    case res of    
        Left err -> error (prettyString @SystemFailure err)
        Right state -> return state

defaultAppState :: IO AppState
defaultAppState = do
    swSt <- defaultSwarmState
    return $ AppState {_termiteState = defaultTermiteState, _swarmState = coerce swSt}

