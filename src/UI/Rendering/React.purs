module UI.Rendering.React where

import Prelude

import Control.Comonad (class Comonad)
import Effect.Class (class MonadEffect, liftEffect)
import React as R
import UI (Component, UI)
import UI as UI

-- | TODO: write documentation
type ReactUI w m f = UI m w f R.ReactElement

-- | TODO: write documentation
type ReactComponent w m f = Component m w f R.ReactElement

-- | TODO: write documentation
explore :: forall w m f. Comonad w => MonadEffect m => (forall a. f a -> a) -> ReactComponent w m f -> R.ReactClass {}
explore alg initialSpace =
  R.pureComponent "ReactComponent" \this ->
    pure
      { state: { space: initialSpace }
      , render: ado
          { space } <- R.getState this
          in alg $
            UI.explore
              (_.space <$> liftEffect (R.getState this))
              (liftEffect <<< R.setState this <<< { space: _ })
              space
      }

-- | TODO: write documentation
fromComponent :: forall w m f. Comonad w => MonadEffect m => (forall a. f a -> a) -> ReactComponent w m f -> R.ReactElement
fromComponent alg component = flip R.unsafeCreateLeafElement {} (explore alg component)
