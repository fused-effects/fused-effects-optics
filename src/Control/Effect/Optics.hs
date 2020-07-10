{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Control.Effect.Optics
  ( eview,
    eviews,
    use,
    uses,
    preuse,
    assign,
    modifying,
    (.=),
    (?=),
    (%=),
  )
where

import Control.Effect.Reader as Reader
import Control.Effect.State as State
import Optics.Core

-- * Reader operations

-- | View the target of a 'Lens', 'Iso', or 'Getter' in the current context.
--
-- This function is prefixed so as not to collide with 'Optics.Core.view'.
--
eview ::
  forall r a m sig k is.
  ( Is k A_Getter,
    Has (Reader.Reader r) sig m
  ) =>
  Optic' k is r a ->
  m a
eview l = Reader.asks (view l)
{-# INLINE eview #-}

-- | Apply a function to the target of a 'Lens', 'Iso', or 'Getter' in the current context.
eviews ::
  forall r a b m sig k is.
  ( Is k A_Getter,
    Has (Reader.Reader r) sig m
  ) =>
  Optic' k is r a ->
  (a -> b) ->
  m b
eviews l f = Reader.asks (f . view l)
{-# INLINE eviews #-}

-- * State operations

-- | Use the target of a 'Lens', 'Iso', or 'Getter' in the current state.
use ::
  forall s a m sig k is.
  ( Is k A_Getter,
    Has (State.State s) sig m
  ) =>
  Optic' k is s a ->
  m a
use l = State.gets (view l)
{-# INLINE use #-}

-- | Apply a function to the target of a 'Lens', 'Iso', or 'Getter' in the current state.
uses ::
  forall s a b m sig k is.
  ( Is k A_Getter,
    Has (State.State s) sig m
  ) =>
  Optic' k is s a ->
  (a -> b) ->
  m b
uses l f = State.gets (f . view l)
{-# INLINE uses #-}

-- | Use the target of a 'AffineTraversal' or 'AffineFold' in the current state.
preuse ::
  forall s a m sig k is.
  ( Is k An_AffineFold,
    Has (State.State s) sig m
  ) =>
  Optic' k is s a ->
  m (Maybe a)
preuse l = State.gets (preview l)
{-# INLINE preuse #-}

-- | Replace the target(s) of an Optic in our monadic state with a new value, irrespective of the old.
-- The action and the optic operation are applied strictly.
--
-- This is aprefix form of '.='.
assign ::
  forall s a b m sig k is.
  ( Is k A_Setter,
    Has (State.State s) sig m
  ) =>
  Optic k is s s a b ->
  b ->
  m ()
assign l x = State.modify (set' l x)
{-# INLINE assign #-}

-- | Map over the target(s) of an 'Optic' in our monadic state.
-- The action and the optic operation are applied strictly.
modifying ::
  ( Is k A_Setter,
    Has (State.State s) sig m
  ) =>
  Optic k is s s a b ->
  (a -> b) ->
  m ()
modifying l x = State.modify (over' l x)
{-# INLINE modifying #-}

-- * Operators

infix 4 .=

infix 4 ?=

infix 4 %=

-- | Replace the target(s) of an Optic in our monadic state with a new value, irrespective of the old.
-- The action and the optic operation are applied strictly.
--
-- This is an infix form of 'assign'.
(.=) ::
  forall s a b m sig k is.
  ( Is k A_Setter,
    Has (State.State s) sig m
  ) =>
  Optic k is s s a b ->
  b ->
  m ()
(.=) = assign
{-# INLINE (.=) #-}

-- | Replace the target(s) of an Optic in our monadic state with 'Just' a new value, irrespective of the old.
-- The action and the optic operation are applied strictly.
(?=) ::
  forall s a b m sig k is.
  ( Is k A_Setter,
    Has (State.State s) sig m
  ) =>
  Optic k is s s a (Maybe b) ->
  b ->
  m ()
l ?= a = State.modify (set l (Just a))
{-# INLINE (?=) #-}

-- | Map over the target(s) of an 'Optic' in our monadic state.
-- The action and the optic operation are applied strictly.
(%=) ::
  ( Is k A_Setter,
    Has (State.State s) sig m
  ) =>
  Optic k is s s a b ->
  (a -> b) ->
  m ()
(%=) = modifying
{-# INLINE (%=) #-}
