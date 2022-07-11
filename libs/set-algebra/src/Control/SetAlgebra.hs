{-# LANGUAGE GADTs #-}

-- | Operations for manipulating sets and maps using mathematicial operators. Concrete types that can be
--   interpreted as sets and maps are made instances of the 'Basic' class. In order to make sets and maps
--   behave uniformly, an instance @(Basic f)@ implies @f@ is a binary type constructor. For types interpreted as
--   maps, the type
--  @(f k v)@ means that @k@ is the type of the map's key, and @v@ is the type of map's value. For types
--  interpreted as sets, the value is always the unit type: (). The binary GADT 'Sett' links to the
--  usual 'Data.Set' type. Its constructor has the following type @Sett :: Set k -> Sett k ()@, programmers can
--  use similar strategies to interpret other types as sets. Predefined instances of Basic include 'Data.Map',
--  'Data.Set', 'List', 'BiMap', and 'Single'. Programmers can add 'Basic' instances for their own types as well.
--
--  A typical set algebra expression (involving range restriction, ('▷'), here) looks like @(eval (x ▷ y))@.
--  Where @x@ and @y@ are program variables or expressions, the operator ('▷') builds an 'Exp' tree, and
-- 'eval' simplifys the tree, and then evaluates the simplfied tree to get the result.
-- Here is the actual type of the range restrict operator.
--
-- @
-- (▷) :: (Ord k, Iter g, Ord v, HasExp s1 (f k v), HasExp s2 (g v ())) => s1 -> s2 -> Exp (f k v)
-- @
--
-- As the type indicates, in order to support simplifcation and evaluation the types of the
-- operands to ('▷') must be instances of several classes: 'Basic', 'HasExp', and 'Iter'.
--
-- 1. @(Basic f)@ meaning @(f k v)@ must be interpreted as a map or set, with two type parameters @k@ and @v@.
-- 2. @(HasExp t (f k v))@  meaning the actual type @t@ of the operands @x@ and @y@ can be interpreted as a @Basic@ type @f@
-- 3. @(Iter f)@ meaning the @Basic@ type @f@ supports certain (usually fast) operations, that can be combined.
-- 4. @(Embed concrete f)@ meaning the types @concrete@ and @(f k v)@ form an isomorphism.

module Control.SetAlgebra
  (

    -- * In addition to 'Data.Map.Map' and 'Data.Set.Set', types interpretable as maps and sets.
    -- $MapAndSetTypes
    List,
    BiMap, 
    Bimap, 
    Single (..),

    -- Classes supporting abstract constructors of Set Algebra Expressions. These show up in the types of overloaded functions.
    Basic (..),
    Iter (..),
    Embed (..),
    HasExp (..),
    BaseRep (..),
    -- Overloaded functions acting as abstract constructors of Set Algebra Expressions. These correspond
    -- with the operators in the specification, except here sets are thought of as a map with a Unit value. (Map k ())
    dom,
    rng,
    dexclude,
    drestrict,
    rexclude,
    rrestrict,
    unionleft,
    unionright,
    unionplus,
    singleton,
    setSingleton,
    intersect,
    subset,
    keyeq,
    (◁),
    (⋪),
    (▷),
    (⋫),
    (∈),
    (∉),
    (∪),
    (⨃),
    (∪+),
    (∩),
    (⊆),
    (≍),
    (<|),
    (|>),
    (➖),
    -- The only exported concrete Constructor of Set Algebra Expressons. Needed to make 'HasExp' and 'Embed'
    -- instances of new kinds of sets (Basically,  Data.Map's wrapped in a newtype).
    -- See: Cardano.Ledger.Shelley.TxBody and Cardano.Ledger.Shelley.UTxO and
    -- Cardano.Ledger.Shelley.Delegation.Certificates
    -- for example uses of this.
    Exp (Base),
    -- Evaluate an abstract Set Algebra Expression to the Set (Map) it represents.
    eval,
    -- Functions to build concrete Set-like things useable as Set Algebra Expressions
    materialize,
    biMapToMap,
    biMapFromMap,
    biMapFromList,
    biMapEmpty,
    fromList,
    keysEqual,
    forwards,
    backwards,
  )
where

import Control.Iterate.BaseTypes (BaseRep (..), Basic (..), Embed (..), Iter (..), List, Single (..))
import Control.Iterate.Exp
  ( Exp (..),
    HasExp (..),
    dexclude,
    dom,
    drestrict,
    intersect,
    keyeq,
    rexclude,
    rng,
    rrestrict,
    setSingleton,
    singleton,
    subset,
    unionleft,
    unionplus,
    unionright,
    (<|),
    (|>),
    (∈),
    (∉),
    (∩),
    (∪),
    (∪+),
    (≍),
    (⊆),
    (⋪),
    (⋫),
    (▷),
    (◁),
    (➖),
    (⨃),
  )
import Control.Iterate.SetAlgebra
import Data.BiMap (BiMap (..), Bimap, biMapEmpty, biMapFromList, biMapFromMap, biMapToMap)
import Data.Map (Map)
import Data.MapExtras (keysEqual)
import Data.Set (Set)

forwards :: BiMap v k v -> Map k v
forwards (MkBiMap l _r) = l

backwards :: BiMap v k v -> Map v (Set k)
backwards (MkBiMap _l r) = r
