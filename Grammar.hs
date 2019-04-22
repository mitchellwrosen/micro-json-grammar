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
  , tuple
  , element
    -- * To/from JSON
  , grammarToJSON
  , grammarParseJSON
  ) where

import Control.Applicative (empty, (<|>))
import Control.Category
import Control.Monad       (guard, (>=>))
import Data.Aeson          (Object, Value(..))
import Data.Aeson.Types    (Parser)
import Data.Bifunctor      (first)
import Data.Foldable       (toList)
import Data.Kind           (Type)
import Data.Maybe
import Data.Scientific     (floatingOrInteger)
import Data.Sequence       (Seq)
import Data.Text           (Text)
import Data.Vector         (Vector)
import Prelude             hiding ((.))

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Sequence       as Seq
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

  (:|) ::
       Grammar a b
    -> Grammar a b
    -> Grammar a b

instance Category Grammar where
  id :: Grammar a a
  id =
    Syntax Just Just

  (.) =
    (:.)

instance Semigroup (Grammar a b) where
  (<>) =
    (:|)

-- | Construct a grammar from an invertible syntax.
syntax ::
     (a -> Maybe b)
  -> (b -> Maybe a)
  -> Grammar a b
syntax =
  Syntax

-- | Run a grammar forwards.
forwards ::
     Grammar a b
  -> a
  -> Maybe b
forwards g =
  case g of
    Syntax f _ ->
      f

    g1 :. g2 ->
      forwards g2 >=> forwards g1

    g1 :| g2 ->
      \x ->
        forwards g1 x <|> forwards g2 x

-- | Run a grammar backwards.
backwards ::
     Grammar a b
  -> b
  -> Maybe a
backwards g =
  case g of
    Syntax _ f ->
      f

    g1 :. g2 ->
      backwards g1 >=> backwards g2

    g1 :| g2 ->
      \x ->
        backwards g1 x <|> backwards g2 x

-- | Match any boolean.
boolean :: Grammar (Value, x) (Bool, x)
boolean =
  Syntax
    (\v -> do
      (Bool b, x) <- pure v
      pure (b, x))
    (first Bool >>> Just)

-- | Match 'True'.
true :: Grammar (Value, x) x
true =
  boolean >>>
    Syntax
      (\case
        (True, x) -> Just x
        _ -> Nothing)
      ((True ,) >>> Just)

-- | Match 'False'.
false :: Grammar (Value, x) x
false =
  boolean >>>
    Syntax
      (\case
        (False, x) -> Just x
        _ -> Nothing)
      ((False ,) >>> Just)

-- | Match any integral number.
integral :: Integral a => Grammar (Value, x) (a, x)
integral =
  Syntax
    (\v -> do
      (Number s, x) <- pure v
      Right n <- pure (floatingOrInteger s :: Either Double _)
      pure (n, x))
    (first (fromIntegral >>> Number) >>> Just)

-- | Match any floating point number.
floating :: RealFloat a => Grammar (Value, x) (a, x)
floating =
  Syntax
    (\v -> do
      (Number s, x) <- pure v
      Left n <- pure (floatingOrInteger s :: Either _ Integer)
      pure (n, x))
    (first (realToFrac >>> Number) >>> Just)

-- | Match any string.
string :: Grammar (Value, x) (Text, x)
string =
  Syntax
    (\v -> do
      (String s, x) <- pure v
      pure (s, x))
    (first String >>> Just)

-- | Match the given string.
symbol :: Text -> Grammar (Value, x) x
symbol s =
  string >>>
    Syntax
      (\(t, x) -> do
        guard (s == t)
        pure x)
      ((s ,) >>> Just)

-- | Modify a grammar to additionally match @null@.
nullable ::
     Grammar (Value, x) (a, x)
  -> Grammar (Value, x) (Maybe a, x)
nullable g =
  Syntax
    (\(v, x) ->
      case v of
        Null -> Just (Nothing, x)
        _ -> first Just <$> forwards g (v, x))
    (\case
      (Nothing, x) -> Just (Null, x)
      (Just v, x) -> backwards g (v, x))

-- | Match an object grammar constructed with 'key' leniently (allowing
-- additional keys in the forward direction).
lenientObject ::
     Grammar (Object, x) (Object, y)
  -> Grammar (Value, x) y
lenientObject g =
  object g >>> anyObject

anyObject :: Grammar (Object, x) x
anyObject =
  Syntax
    (snd >>> Just)
    ((HashMap.empty ,) >>> Just)

-- | Match an object grammar constructed with 'key' strictly (disallowing
-- additional keys in the forward direction).
strictObject ::
     Grammar (Object, x) (Object, y)
  -> Grammar (Value, x) y
strictObject g =
  object g >>> emptyObject

emptyObject :: Grammar (Object, x) x
emptyObject =
  Syntax
    (\(o, x) -> do
      guard (HashMap.null o)
      pure x)
    ((HashMap.empty ,) >>> Just)

object ::
     Grammar (Object, x) (Object, y)
  -> Grammar (Value, x) (Object, y)
object g =
  Syntax
    (\v -> do
      (Object o, x) <- pure v
      (forwards g (o, x)))
    (backwards g >>> fmap (first Object))

-- | Match a grammar at the given key in an object.
key ::
     Text
  -> Grammar (Value, x) y
  -> Grammar (Object, x) (Object, y)
key k g =
  Syntax
    (\(m, x) -> do
      v <- HashMap.lookup k m
      (HashMap.delete k m ,) <$> forwards g (v, x))
    (\(m, y) ->
      first (\v -> HashMap.insert k v m) <$> backwards g y)

-- | Match a homogenous array grammar.
array ::
     (forall x. Grammar (Value, x) (b, x))
  -> Grammar (Value, x) (Vector b, x)
array g =
  Syntax
    (\v -> do
      (Array vs, x) <- pure v
      (fmap fst >>> (, x)) <$> traverse ((, ()) >>> forwards g) vs)
    (\(bs, x) ->
      (fmap fst >>> Array >>> (, x)) <$> traverse ((, ()) >>> backwards g) bs)

newtype TupleGrammar x y
  = TupleGrammar (Grammar (Seq Value, x) (Seq Value, y))

-- | Match a heterogeneous array grammar constructed with 'element'.
tuple ::
     TupleGrammar x y
  -> Grammar (Value, x) y
tuple (TupleGrammar g) =
  Syntax
    (\v -> do
      (Array vs, x) <- pure v
      (vs', y) <- forwards g (vectorToSeq vs, x)
      guard (Seq.null vs')
      pure y)
    ((Seq.empty ,) >>> backwards g >>> fmap (first (Array . seqToVector)))

vectorToSeq :: Vector a -> Seq a
vectorToSeq =
  Seq.fromList . Vector.toList

seqToVector :: Seq a -> Vector a
seqToVector =
  Vector.fromList . toList

-- | Match a grammar at the current element in an array.
element ::
     Grammar (Value, x) y
  -> TupleGrammar x y
element g =
  TupleGrammar
    (Syntax
      (\(vs, x) -> do
        v Seq.:<| vs' <- pure vs
        (vs' ,) <$> forwards g (v, x))
      (\(vs, y) ->
        first (Seq.:<| vs) <$> backwards g y))

-- | Use a grammar to derive a 'Data.Aeson.ToJSON' instance.
--
-- @
-- instance ToJSON Foo where
--   toJSON = grammarToJSON fooGrammar
-- @
grammarToJSON :: (forall x. Grammar (Value, x) (a, x)) -> a -> Value
grammarToJSON g x =
  maybe Null fst (backwards g (x, ()))

-- | Use a grammar to derive a 'Data.Aeson.FromJSON' instance.
--
-- @
-- instance FromJSON Foo where
--   parseJSON = grammarParseJSON fooGrammar
-- @
grammarParseJSON :: (forall x. Grammar (Value, x) (a, x)) -> Value -> Parser a
grammarParseJSON g v =
  maybe empty (pure . fst) (forwards g (v, ()))
