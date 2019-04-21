{-# OPTIONS_GHC -fno-warn-name-shadowing          #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Grammar
  ( Grammar
  , syntax
  , forwards
  , backwards
    -- * Atoms
  , boolean
  , true
  , false
  , integral
  , floating
  , string
  , symbol
  , nullable
    -- * Objects
  , lenientObject
  , strictObject
  , key
    -- * Arrays
  , array
    -- * Tuples
  , lenientTuple
  , strictTuple
  , element
  ) where

import Control.Category
import Control.Monad    ((>=>), guard)
import Data.Aeson       (Array, Object, Value(..))
import Data.Bifunctor   (first)
import Data.Kind        (Type)
import Data.Scientific  (floatingOrInteger)
import Data.Text        (Text)
import Data.Vector      (Vector)
import Prelude          hiding ((.))

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector         as Vector


data Grammar (a :: Type) (b :: Type) where
  Syntax ::
       (a -> Maybe b)
    -> (b -> Maybe a)
    -> Grammar a b

  (:.) ::
       Grammar b c
    -> Grammar a b
    -> Grammar a c

instance Category Grammar where
  id :: Grammar a a
  id =
    Syntax Just Just

  (.) =
    (:.)

syntax ::
     (a -> Maybe b)
  -> (b -> Maybe a)
  -> Grammar a b
syntax =
  Syntax

forwards ::
     (forall x. Grammar (a, x) (b, x))
  -> a
  -> Maybe b
forwards g =
  (, ()) >>> forwards_ g >>> fmap fst

forwards_ ::
     Grammar a b
  -> a
  -> Maybe b
forwards_ g =
  case g of
    Syntax f _ ->
      f

    g1 :. g2 ->
      forwards_ g2 >=> forwards_ g1

backwards ::
     (forall x. Grammar (a, x) (b, x))
  -> b
  -> Maybe a
backwards g =
  (, ()) >>> backwards_ g >>> fmap fst

backwards_ ::
     Grammar a b
  -> b
  -> Maybe a
backwards_ g =
  case g of
    Syntax _ f ->
      f

    g1 :. g2 ->
      backwards_ g1 >=> backwards_ g2

boolean :: Grammar (Value, x) (Bool, x)
boolean =
  Syntax
    (\v -> do
      (Bool b, x) <- pure v
      pure (b, x))
    (first Bool >>> Just)

true :: Grammar (Value, x) x
true =
  boolean >>>
    Syntax
      (\case
        (True, x) -> Just x
        _ -> Nothing)
      ((True ,) >>> Just)

false :: Grammar (Value, x) x
false =
  boolean >>>
    Syntax
      (\case
        (False, x) -> Just x
        _ -> Nothing)
      ((False ,) >>> Just)

integral :: Integral a => Grammar (Value, x) (a, x)
integral =
  Syntax
    (\v -> do
      (Number s, x) <- pure v
      Right n <- pure (floatingOrInteger s :: Either Double _)
      pure (n, x))
    (first (fromIntegral >>> Number) >>> Just)

floating :: RealFloat a => Grammar (Value, x) (a, x)
floating =
  Syntax
    (\v -> do
      (Number s, x) <- pure v
      Left n <- pure (floatingOrInteger s :: Either _ Integer)
      pure (n, x))
    (first (realToFrac >>> Number) >>> Just)

string :: Grammar (Value, x) (Text, x)
string =
  Syntax
    (\v -> do
      (String s, x) <- pure v
      pure (s, x))
    (first String >>> Just)

symbol :: Text -> Grammar (Value, x) x
symbol s =
  string >>>
    Syntax
      (\(t, x) -> do
        guard (s == t)
        pure x)
      ((s ,) >>> Just)

nullable ::
     Grammar (Value, x) (a, x)
  -> Grammar (Value, x) (Maybe a, x)
nullable g =
  Syntax
    (\(v, x) ->
      case v of
        Null -> Just (Nothing, x)
        _ -> first Just <$> forwards_ g (v, x))
    (\case
      (Nothing, x) -> Just (Null, x)
      (Just v, x) -> backwards_ g (v, x))

lenientObject ::
     Grammar (Object, x) (Object, y)
  -> Grammar (Value, x) y
lenientObject g =
  Syntax
    (\v -> do
      (Object o, x) <- pure v
      snd <$> forwards_ g (o, x))
    ((HashMap.empty ,) >>> backwards_ g >>> fmap (first Object))

strictObject ::
     Grammar (Object, x) (Object, y)
  -> Grammar (Value, x) y
strictObject g =
  Syntax
    (\v -> do
      (Object o, x) <- pure v
      (o', y) <- forwards_ g (o, x)
      guard (HashMap.null o')
      pure y)
    ((HashMap.empty ,) >>> backwards_ g >>> fmap (first Object))

object ::
     Grammar (Object, x) (Object, y)
  -> Grammar (Value, x) (Object, y)
object g =
  Syntax
    (\v -> do
      (Object o, x) <- pure v
      (forwards_ g (o, x)))
    (backwards_ g >>> fmap (first Object))

key ::
     Text
  -> Grammar (Value, x) y
  -> Grammar (Object, x) (Object, y)
key k g =
  Syntax
    (\(m, x) -> do
      v <- HashMap.lookup k m
      (HashMap.delete k m ,) <$> forwards_ g (v, x))
    (\(m, y) ->
      first (\v -> HashMap.insert k v m) <$> backwards_ g y)

array ::
     (forall x. Grammar (Value, x) (b, x))
  -> Grammar (Value, x) (Vector b, x)
array g =
  Syntax
    (\v -> do
      (Array vs, x) <- pure v
      (fmap fst >>> (, x)) <$> traverse ((, ()) >>> forwards_ g) vs)
    (\(bs, x) ->
      (fmap fst >>> Array >>> (, x)) <$> traverse ((, ()) >>> backwards_ g) bs)

lenientTuple ::
     Grammar (Array, x) (Array, y)
  -> Grammar (Value, x) y
lenientTuple g =
  Syntax
    (\v -> do
      (Array vs, x) <- pure v
      snd <$> forwards_ g (vs, x))
    ((Vector.empty ,) >>> backwards_ g >>> fmap (first Array))

strictTuple ::
     Grammar (Array, x) (Array, y)
  -> Grammar (Value, x) y
strictTuple g =
  Syntax
    (\v -> do
      (Array vs, x) <- pure v
      (vs', y) <- forwards_ g (vs, x)
      guard (Vector.null vs')
      pure y)
    ((Vector.empty ,) >>> backwards_ g >>> fmap (first Array))

element ::
     Grammar (Value, x) y
  -> Grammar (Array, x) (Array, y)
element g =
  Syntax
    (\(vs, x) ->
      (vs Vector.!? 0) >>=
        ((, x) >>> forwards_ g >>> fmap (Vector.drop 1 vs ,)))
    (\(vs, y) ->
      first (`Vector.cons` vs) <$> backwards_ g y)
