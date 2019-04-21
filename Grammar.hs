module Grammar
  ( Grammar
  , forwards
  , backwards
    -- * Atoms
  , int
  , string
  , nullable
    -- * Objects
  , object
  , key
    -- * Arrays
  , array
    -- * Tuples
  , tuple
  , el
  ) where

import Control.Category
import Control.Monad    ((>=>))
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

int :: Grammar (Value, x) (Int, x)
int =
  Syntax
    (\v -> do
      (Number s, x) <- pure v
      Right n <- pure (floatingOrInteger s :: Either Double Int)
      pure (n, x))
    (first (fromIntegral >>> Number) >>> Just)


string :: Grammar (Value, x) (Text, x)
string =
  Syntax
    (\v -> do
      (String s, x) <- pure v
      pure (s, x))
    (first String >>> Just)

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

object ::
     Grammar (Object, x) (Object, y)
  -> Grammar (Value, x) y
object g =
  Syntax
    (\v -> do
      (Object o, x) <- pure v
      snd <$> forwards_ g (o, x))
    ((HashMap.empty ,) >>> backwards_ g >>> fmap (first Object))

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

tuple ::
     Grammar (Array, x) (Array, y)
  -> Grammar (Value, x) y
tuple g =
  Syntax
    (\v -> do
      (Array vs, x) <- pure v
      snd <$> forwards_ g (vs, x))
    ((Vector.empty ,) >>> backwards_ g >>> fmap (first Array))

el ::
     Grammar (Value, x) y
  -> Grammar (Array, x) (Array, y)
el g =
  Syntax
    (\(vs, x) ->
      (vs Vector.!? 0) >>=
        ((, x) >>> forwards_ g >>> fmap (Vector.drop 1 vs ,)))
    (\(vs, y) ->
      first (`Vector.cons` vs) <$> backwards_ g y)
