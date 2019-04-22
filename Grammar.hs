{-# OPTIONS_GHC -fno-warn-name-shadowing          #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Grammar
  ( -- * Grammar
    Grammar
  , syntax
  , forwards
  , backwards
    -- * Booleans
  , boolean
  , true
  , false
    -- * Numbers
  , number
  , integral
  , floating
    -- * Strings
  , string
  , symbol
    -- * Objects
  , ObjectGrammar
  , lenientObject
  , strictObject
  , key
    -- * Arrays
  , array
    -- * Tuples
  , TupleGrammar
  , tuple
  , element
    -- * Nulls
  , nullable
    -- * To/from JSON
  , grammarToJSON
  , grammarParseJSON
  ) where

import Control.Applicative (empty, (<|>))
import Control.Category    (Category(..), (>>>))
import Control.Monad       (guard, (>=>))
import Data.Aeson          (Object, Value(..))
import Data.Aeson.Types    (Parser)
import Data.Bifunctor      (first)
import Data.Foldable       (toList)
import Data.Scientific     (Scientific, floatingOrInteger)
import Data.Sequence       (Seq)
import Data.Text           (Text)
import Data.Vector         (Vector)
import Prelude             hiding (id, (.))

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Sequence       as Seq
import qualified Data.Vector         as Vector


-- | A grammar represents an invertible mapping between two types.
--
-- For example, the 'boolean' grammar represents the mapping between a JSON
-- value and a boolean value. A JSON value /may/ be able to be converted to a
-- boolean value, whereas a boolean value can /always/ be converted to a JSON
-- value.
--
-- Though this particular mapping seems partial only in one direction, a grammar
-- is necessarily partial in both directions, perhaps violating your intuition
-- about parsing and printing, which are typically partial and total,
-- respectively.
--
-- This is one downside of using invertible syntaxes; another is the
-- preponderance of nested tuples and dealing with boilerplate @()@ values.
--
-- You might expect the type of 'boolean' to be
--
-- @
-- 'Grammar' Value Bool
-- @
--
-- but it is written in "open-style"
--
-- @
-- 'Grammar' (Value, x) (Bool, x)
-- @
--
-- instead. This is because grammars are sequenced with the category-composition
-- operator: when parsing multiple values out of a single JSON value, as is the
-- case with objects and tuples (heterogeneous arrays), the output of one
-- grammar needs to be compatible with the input of the next.
--
-- As an example, observe the type of a grammar that parses a boolean and a
-- string out of a JSON object:
--
-- @
-- >>> :t 'lenientObject' ('key' "foo" 'boolean' >>> 'key' "bar" 'string')
-- 'Grammar' (Value, x) (Text, (Bool, x))
-- @
data Grammar a b where
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
  Syntax
    (\v -> do
      (Bool True, x) <- pure v
      pure x)
    ((Bool True ,) >>> Just)

-- | Match 'False'.
false :: Grammar (Value, x) x
false =
  Syntax
    (\v -> do
      (Bool False, x) <- pure v
      pure x)
    ((Bool False ,) >>> Just)

-- | Match any number.
number :: Grammar (Value, x) (Scientific, x)
number =
  Syntax
    (\v -> do
      (Number n, x) <- pure v
      pure (n, x))
    (first Number >>> Just)

-- | Match any integral number.
integral :: Integral a => Grammar (Value, x) (a, x)
integral =
  Syntax
    (\v -> do
      (Number s, x) <- pure v
      Right n <- pure (floatingOrInteger @Double s)
      pure (n, x))
    (first (fromIntegral >>> Number) >>> Just)

-- | Match any floating point number.
floating :: RealFloat a => Grammar (Value, x) (a, x)
floating =
  Syntax
    (\v -> do
      (Number s, x) <- pure v
      Left n <- pure (floatingOrInteger @_ @Integer s)
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
  Syntax
    (\v -> do
      (String t, x) <- pure v
      guard (s == t)
      pure x)
    ((String s ,) >>> Just)

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

-- | An object grammar represents an invertible mapping between a JSON object
-- and a collection of types.
--
-- For example, a value of type
--
-- @
-- 'ObjectGrammar' x (A, (B, (C, x)))
-- @
--
-- represents an invertible mapping between a JSON object and three values with
-- types A, B, and C.
--
-- It may be applied leniently ('lenientObject') or strictly ('strictObject');
-- that is, in the forward (parsing) direction, additional keys may be allowed
-- in the object.
--
-- Here is an example @ghci@ session that demonstrates how object grammars work:
--
-- @
-- __The simplest object grammar: one key/value pair__
-- >>> :t 'key' "foo" 'boolean'
-- 'ObjectGrammar' x (Bool, x)
--
-- __Two key/value pairs__
-- >>> :t 'key' "foo" 'boolean' >>> 'key' "bar" 'string'
-- 'ObjectGrammar' x (Text, (Bool, x))
--
-- __Lifting to a grammar__
-- >>> :t 'lenientObject' ('key' "foo" 'boolean' >>> 'key' "bar" 'string')
-- 'Grammar' (Value, x) (Text, (Bool, x))
--
-- __Running it forwards, parsing {"foo":true,"bar":"baz"}__
-- >>> :t 'forwards' ('lenientObject' ('key' "foo" 'boolean' >>> 'key' "bar" 'string'))
-- (Value, x) -> Maybe (Text, (Bool, x))
--
-- __Running it backwards, printing as {"foo":true,"bar":"baz"}__
-- >>> :t 'backwards' ('lenientObject' ('key' "foo" 'boolean' >>> 'key' "bar" 'string'))
-- (Text, (Bool, x)) -> Maybe (Value, x)
-- @
newtype ObjectGrammar x y
  = ObjectGrammar (Grammar (Object, x) (Object, y))

instance Category ObjectGrammar where
  id = ObjectGrammar id
  ObjectGrammar x . ObjectGrammar y = ObjectGrammar (x . y)

instance Semigroup (ObjectGrammar x y) where
  ObjectGrammar x <> ObjectGrammar y = ObjectGrammar (x <> y)

-- | Match an object grammar leniently (allowing additional keys in the forward
-- direction).
lenientObject ::
     ObjectGrammar x y
  -> Grammar (Value, x) y
lenientObject (ObjectGrammar g) =
  Syntax
    (\v -> do
      (Object o, x) <- pure v
      snd <$> forwards g (o, x))
    (backwardsObject g)

-- | Match an object grammar strictly (disallowing additional keys in the
-- forward direction).
strictObject ::
     ObjectGrammar x y
  -> Grammar (Value, x) y
strictObject (ObjectGrammar g) =
  Syntax
    (\v -> do
      (Object o, x) <- pure v
      (o', y) <- forwards g (o, x)
      guard (HashMap.null o')
      pure y)
    (backwardsObject g)

backwardsObject ::
     Grammar (Object, x) (Object, y)
  -> y
  -> Maybe (Value, x)
backwardsObject g =
  (HashMap.empty ,) >>> backwards g >>> fmap (first Object)

-- | Match a grammar at the given key in an object.
key ::
     Text
  -> Grammar (Value, x) y
  -> ObjectGrammar x y
key k g =
  ObjectGrammar
    (Syntax
      (\(m, x) -> do
        v <- HashMap.lookup k m
        (HashMap.delete k m ,) <$> forwards g (v, x))
      (\(m, y) ->
        first (\v -> HashMap.insert k v m) <$> backwards g y))

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

-- | A tuple grammar represents an invertible mapping between a fixed-size JSON
-- array and a collection of types.
--
-- For example, a value of type
--
-- @
-- 'TupleGrammar' x (A, (B, (C, x)))
-- @
--
-- represents an invertible mapping between a three-element JSON array and three
-- values with types A, B, and C.
--
-- Here is an example @ghci@ session that demonstrates how tuple grammars work:
--
-- @
-- __The simplest tuple grammar: one element__
-- >>> :t 'element' 'boolean'
-- 'TupleGrammar' x (Bool, x)
--
-- __Two elements__
-- >>> :t 'element' 'boolean' >>> 'element' 'string'
-- 'TupleGrammar' x (Text, (Bool, x))
--
-- __Lifting to a grammar__
-- >>> :t 'tuple' ('element' 'boolean' >>> 'element' 'string')
-- 'Grammar' (Value, x) (Text, (Bool, x))
--
-- __Running it forwards, parsing [true,"baz"]__
-- >>> :t 'forwards' ('tuple' ('element' 'boolean' >>> 'element' 'string'))
-- (Value, x) -> Maybe (Text, (Bool, x))
--
-- __Running it backwards, printing as [true,"baz"]__
-- >>> :t 'backwards' ('tuple' ('element' 'boolean' >>> 'element' 'string'))
-- (Text, (Bool, x)) -> Maybe (Value, x)
-- @
newtype TupleGrammar x y
  = TupleGrammar (Grammar (Seq Value, x) (Seq Value, y))

instance Category TupleGrammar where
  id = TupleGrammar id
  TupleGrammar x . TupleGrammar y = TupleGrammar (x . y)

instance Semigroup (TupleGrammar x y) where
  TupleGrammar x <> TupleGrammar y = TupleGrammar (x <> y)

-- | Match a tuple (heterogeneous array) grammar.
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
