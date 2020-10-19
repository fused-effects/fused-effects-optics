{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Control.Effect.Optics.Indexed
  ( -- * Indexed reader operations
    iview,
    iviews,

    -- * Indexed state operations
    iuse,
    iuses,
  )
where

import Control.Effect.Reader as Reader
import Control.Effect.State as State
import Optics.Core hiding (iview, iviews)
import qualified Optics.Core as Optics

-- | View the index and value of an indexed getter into the current environment as a pair.
iview :: (Is k A_Getter, is `HasSingleIndex` i, Has (Reader.Reader r) sig m) => Optic' k is r a -> m (i, a)
iview l = Reader.asks (Optics.iview l)

-- | View the index and value of an indexed getter into the current environment and pass them to the provided function.
iviews :: (Is k A_Getter, is `HasSingleIndex` i, Has (Reader.Reader r) sig m) => Optic' k is r a -> (i -> a -> m b) -> m b
iviews l f = Reader.ask >>= Optics.iviews l f

-- | Extract the index and target of an indexed getter in the current state as a pair.
iuse :: (Is k A_Getter, is `HasSingleIndex` i, Has (State s) sig m) => Optic' k is s a -> m (i, a)
iuse l = State.gets (Optics.iview l)

-- | Extract the index and target of an indexed getter in the current state and pass them to the provided function.
iuses :: (Is k A_Getter, is `HasSingleIndex` i, Has (State s) sig m) => Optic' k is s a -> (i -> a -> m b) -> m b
iuses l r = State.get >>= Optics.iviews l r
