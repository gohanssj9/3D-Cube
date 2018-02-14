module Control.Monad.Eff.DOM where

import Prelude

import Control.Monad.Eff (Eff)
import Graphics.Canvas

foreign import animate :: forall e. Context2D
                -> (Context2D -> Eff (canvas :: CANVAS | e) Unit)
                -> Eff (canvas :: CANVAS | e) Unit

foreign import data Event :: Type

foreign import addEventListener :: forall a. CanvasElement -> String
                                    -> (Event -> Eff (canvas :: CANVAS | a) Unit)
                                    -> Eff (canvas :: CANVAS | a) Unit
