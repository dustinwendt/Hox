{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}

module Gtk where

import           Reactive.Banana
import           Reactive.Banana.Frameworks

import           Control.Exception
import           Data.GI.Base
import           Data.GI.Base.Attributes
import           Data.GI.Base.ManagedPtr    (unsafeCastTo)
import           Data.GI.Base.Overloading   (HasAttributeList (..),
                                             ResolveAttribute (..))
import           Data.GI.Base.ShortPrelude  (Symbol (..))
import           Data.GI.Base.Signals
import           Data.Text
import           GHC.TypeLits
import           GI.Gtk                     (IsBuilder, builderGetObject)

-- Code lifted from: https://github.com/mr/reactive-banana-gi-gtk/blob/master/reactive-banana-gi-gtk/src/Reactive/Banana/GI/Gtk.hs


newtype BuilderCastException = UnknownIdException String deriving (Show)

instance Exception BuilderCastException

castB :: (IsBuilder a, GObject o, MonadIO m)
      => a
      -> Text
      -> (ManagedPtr o -> o)
      -> m o
castB builder ident gtype =
  liftIO $ do
    o <- builderGetObject builder ident
    case o of
      Just a -> unsafeCastTo gtype a
      Nothing ->
        throw $ UnknownIdException $ unpack ident

signalAddHandler :: (SignalInfo info, GObject self)
                    => self
                    -> SignalProxy self info
                    -> ((a -> IO b)
                    -> HaskellCallbackType info)
                    -> b
                    -> IO (AddHandler a)
signalAddHandler self signal f b = do
  (addHandler, fire) <- newAddHandler
  on self signal (f $ \x -> fire x >> return b)
  return addHandler


signalEN :: (SignalInfo info, GObject self)
            => self
            -> SignalProxy self info
            -> ((a -> IO b) -> HaskellCallbackType info)
            -> b
            -> MomentIO (Event a)
signalEN self signal f b = do
  addHandler <- liftIO $ signalAddHandler self signal f b
  fromAddHandler addHandler

-- Equality constraint from GADT?
-- Reactive.Banana.Event from Data.GI.Base.Signals that produces nothing
signalE0 :: (HaskellCallbackType info ~ IO (), SignalInfo info, GObject self)
            => self
            -> SignalProxy self info
            -> MomentIO (Event ())
signalE0 self signal = signalEN self signal ($ ()) ()

-- Reactive.Banana.Event from Data.GI.Base.Signals that produces one argument
signalE1 :: (HaskellCallbackType info ~ (a -> IO ()), SignalInfo info, GObject self)
            => self
            -> SignalProxy self info
            -> MomentIO (Event a)
signalE1 self signal = signalEN self signal id ()

  -- id :: a -> a
  -- signalEn self signal (() -> ())

signalE2 :: (HaskellCallbackType info ~ ((a -> b) -> IO ()), SignalInfo info, GObject self)
            => self
            -> SignalProxy self info
            -> MomentIO (Event (a -> b))
signalE2 self signal = signalEN self signal id ()

-- Get a 'Reactive.Banana.Event' from a 'Data.GI.Base.Attributes.AttrLabelProxy' that produces one argument
attrE :: (GObject self, AttrGetC info self attr result, KnownSymbol (AttrLabel info))
         => self
         -> AttrLabelProxy (attr :: Symbol)
         -> MomentIO (Event result)
attrE self attr = do
  e <- signalE1 self (PropertyNotify attr)
  (const $ get self attr) `mapEventIO` e

-- stepper on 'attrE'
attrB :: (GObject self, AttrGetC info self attr result, KnownSymbol (AttrLabel info))
         => self
         -> AttrLabelProxy (attr :: Symbol)
         -> MomentIO (Behavior result)
attrB self attr = do
  e <- attrE self attr
  initV <- get self attr
  stepper initV e

data AttrOpBehavior self tag where
  (:==)
    ::
      (HasAttributeList self
      , info ~ ResolveAttribute attr self
      , AttrInfo info
      , AttrBaseTypeConstraint info self
      , AttrOpAllowed tag info self
      , AttrSetTypeConstraint info b)
      => AttrLabelProxy (attr :: Symbol)
      -> Behavior b
      -> AttrOpBehavior self tag
  (:==>)
    ::
      (HasAttributeList self
      , info ~ ResolveAttribute attr self
      , AttrInfo info
      , AttrBaseTypeConstraint info self
      , AttrOpAllowed tag info self
      , AttrSetTypeConstraint info b)
      => AttrLabelProxy (attr :: Symbol)
      -> Behavior (IO b)
      -> AttrOpBehavior self tag
  (:~~)
    ::
      (HasAttributeList self
      , info ~ ResolveAttribute attr self
      , AttrInfo info
      , AttrBaseTypeConstraint info self
      , tag ~ AttrSet
      , AttrOpAllowed AttrSet info self
      , AttrOpAllowed AttrGet info self
      , AttrSetTypeConstraint info b
      , a ~ AttrGetType info)
      => AttrLabelProxy (attr :: Symbol)
      -> Behavior (a -> b)
      -> AttrOpBehavior self tag
  (:~~>)
    ::
      (HasAttributeList self
      , info ~ ResolveAttribute attr self
      , AttrInfo info
      , AttrBaseTypeConstraint info self
      , tag ~ AttrSet
      , AttrOpAllowed AttrSet info self
      , AttrOpAllowed AttrGet info self
      , AttrSetTypeConstraint info b
      , a ~ AttrGetType info)
      => AttrLabelProxy (attr :: Symbol)
      -> Behavior (a -> IO b)
      -> AttrOpBehavior self tag

infixr 0 :==
infixr 0 :==>
infixr 0 :~~
infixr 0 :~~>

sink1 :: GObject self => self -> AttrOpBehavior self AttrSet -> MomentIO ()
sink1 self (attr :== b) = do
  x <- valueBLater b
  liftIOLater $ set self [attr := x]
  e <- changes b
  reactimate' $ fmap (\x -> set self [attr := x]) <$> e
sink1 self (attr :==> b) = do
  x <- valueBLater b
  liftIOLater $ set self [attr :=> x]
  e <- changes b
  reactimate' $ fmap (\x -> set self [attr :=> x]) <$> e
sink1 self (attr :~~ b) = do
  x <- valueBLater b
  liftIOLater $ set self [attr :~ x]
  e <- changes b
  reactimate' $ fmap (\x -> set self [attr :~ x]) <$> e
sink1 self (attr :~~> b) = do
  x <- valueBLater b
  liftIOLater $ set self [attr :~> x]
  e <- changes b
  reactimate' $ fmap (\x -> set self [attr :~> x]) <$> e

-- "Animate" an attribute with a 'Reactive.Banana.Behavior'
-- Example:
-- clickedE <- signalE0 button #clicked
-- clickedCount <- accumB (0 :: Int) ((+1) <$ clickedE)
-- sink myLabel [#label :== (T.pack . show) <$> clickedCount]
--
sink :: GObject self => self -> [AttrOpBehavior self AttrSet] -> MomentIO ()
sink self = mapM_ (sink1 self)
