{-# LANGUAGE DeriveDataTypeable #-}

{-|
Module: Data.Neither
Description: The Neither datatype and instances

In this module, instances have been annotated with the interpretation of the Neither type that they use.
-}
module Data.Neither where

import Control.Applicative
import Control.Monad
import Control.Monad.Zip
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Arrow
import Control.Category
import Data.Data
import Data.Functor.Contravariant
import Data.Functor.Classes
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.String
import Data.Ix
import Data.Fixed
import Data.Bits

-- | The Neither type has a single constructor and ignores its type arguments
data Neither a b = Neither deriving (Read, Show, Eq, Ord, Enum, Bounded, Data)

-- | Produces a Neither value whose types match the arguments, discarding the arguments
neither :: a -> b -> Neither a b
neither _ _ = Neither

-- | An empty container
instance Functor (Neither a) where
    fmap _ _ = Neither
    _ <$ _ = Neither

-- | An empty container
instance Applicative (Neither a) where
    pure _ = Neither
    _ <*> _ = Neither
    _ *> _ = Neither
    _ <* _ = Neither

-- | An empty container
instance Monad (Neither a) where
    _ >>= _ = Neither

-- | An empty container
instance MonadFail (Neither a) where
    fail _ = Neither

-- | An empty container
instance MonadIO (Neither a) where
    liftIO _ = Neither

-- | The trivial single element semigroup
instance Semigroup (Neither a b) where
    _ <> _ = Neither

-- | The trivial single element monoid
instance Monoid (Neither a b) where
    mempty = Neither

-- | A trivial single element group with no-ops, where every integer is Neither
instance Num (Neither a b) where
    _ + _ = Neither
    _ - _ = Neither
    _ * _ = Neither
    negate _ = Neither
    abs _ = Neither
    signum _ = Neither
    fromInteger _ = Neither

-- | A number type that only contains zero
instance Real (Neither a b) where
    toRational _ = 0

-- | A number type that only contains zero, or a trivial single element euclidean domain
instance Integral (Neither a b) where
    quot _ _ = Neither
    rem _ _ = Neither
    div _ _ = Neither
    mod _ _ = Neither
    quotRem _ _ = (Neither, Neither)
    divMod _ _ = (Neither, Neither)
    toInteger _ = 0

-- | A trivial single element field where every ratio is Neither
instance Fractional (Neither a b) where
    _ / _ = Neither
    recip _ = Neither
    fromRational _ = Neither

-- | A trivial single element exponential field, where every number is Neither
instance Floating (Neither a b) where
    pi = Neither
    exp _ = Neither
    log _ = Neither
    sqrt _ = Neither
    _ ** _ = Neither
    logBase _ _ = Neither
    sin _ = Neither
    cos _ = Neither
    tan _ = Neither
    asin _ = Neither
    acos _ = Neither
    atan _ = Neither
    sinh _ = Neither
    cosh _ = Neither
    tanh _ = Neither
    asinh _ = Neither
    acosh _ = Neither
    atanh _ = Neither

-- | A number type that only contains zero
instance RealFrac (Neither a b) where
    properFraction _ = (0, Neither)
    truncate _ = 0
    round _ = 0
    ceiling _ = 0
    floor _ = 0

-- | A bitstring with zero length
instance Bits (Neither a b) where
    _ .&. _ = Neither
    _ .|. _ = Neither
    xor _ _ = Neither
    complement _ = Neither
    shift _ _ = Neither
    rotate _ _ = Neither
    zeroBits = Neither
    bit _ = Neither
    setBit _ _ = Neither
    clearBit _ _ = Neither
    complementBit _ _ = Neither
    testBit _ _ = False
    bitSizeMaybe _ = Just 0
    bitSize _ = 0
    isSigned _ = False
    shiftL _ _ = Neither
    shiftR _ _ = Neither
    rotateL _ _ = Neither
    rotateR _ _ = Neither
    popCount _ = 0

-- | A bitstring with zero length
instance FiniteBits (Neither a b) where
    finiteBitSize _ = 0
    countLeadingZeros _ = 0
    countTrailingZeros _ = 0

-- | An empty container with no-ops
instance Alternative (Neither a) where
    empty = Neither
    _ <|> _ = Neither
    some _ = Neither
    many _ = Neither

-- | An empty container with no-ops
instance MonadPlus (Neither a) where
    mzero = Neither
    mplus _ _ = Neither

-- | An empty container or an uncallable or no-op function representation
instance Contravariant (Neither a) where
    contramap _ _ = Neither
    _ >$ _ = Neither

-- | An empty container
instance Bifunctor Neither where
    bimap _ _ _ = Neither
    first _ _ = Neither
    second _ _ = Neither

-- | An empty container
instance MonadZip (Neither a) where
    mzip _ _ = Neither
    mzipWith _ _ _ = Neither
    munzip _ = (Neither, Neither)

-- | All functions from Neither to Neither must have Neither as a fixed point (if you ignore types)
instance MonadFix (Neither a) where
    mfix _ = Neither

-- | An empty container which folds to the starting value or the identity
instance Foldable (Neither a) where
    foldMap _ _ = mempty
    foldr _ x _ = x
    foldl _ x _ = x
    elem _ _ = False
    sum _ = 0
    product _ = 1

-- | An empty container, that, when in an applicative, must always be a minimally wrapped constant value
instance Traversable (Neither a) where
    traverse _ _ = pure Neither
    sequenceA _ = pure Neither
    mapM _ _ = pure Neither
    sequence _ = pure Neither

-- | An empty container which folds to the starting value or the identity
instance Bifoldable Neither where
    bifold _ = mempty
    bifoldMap _ _ _ = mempty
    bifoldr _ _ x _ = x
    bifoldl _ _ x _ = x

-- | An empty container, that, when in an applicative, must always be a minimally wrapped constant value
instance Bitraversable Neither where
    bitraverse _ _ _ = pure Neither

-- | The constant functor from Set to 1
instance Category Neither where
    id = Neither
    _ . _ = Neither

-- | A dummy function representation
instance Arrow Neither where
    arr _ = Neither
    first _ = Neither
    second _ = Neither
    _ *** _ = Neither
    _ &&& _ = Neither

-- | A dummy function representation, so the zero arrow is merely unique by its type parameters
instance ArrowZero Neither where
    zeroArrow = Neither

-- |     The trivial single element monoid on a dummy function representation
instance ArrowPlus Neither where
    _ <+> _ = Neither

-- | A dummy function representation, where all choices are the same
instance ArrowChoice Neither where
    left _ = Neither
    right _ = Neither
    _ +++ _ = Neither
    _ ||| _ = Neither

-- | A dummy function representation, where application returns another dummy
instance ArrowApply Neither where
    app = Neither

-- | A dummy function representation, where recursion creation returns another dummy
instance ArrowLoop Neither where
    loop _ = Neither

-- | Every string is Neither
instance IsString (Neither a b) where
    fromString _ = Neither

-- | There is only one Neither value
instance Ix (Neither a b) where
    range _ = [Neither]
    index _ _ = 0
    inRange _ _ = True
    rangeSize _ = 1

-- | Empty container that is equal to itself (with no values to compare)
instance Eq1 (Neither a) where
    liftEq _ _ _ = True

-- | Empty container that is equal to itself (with no values to compare)
instance Ord1 (Neither a) where
    liftCompare _ _ _ = EQ

-- | Empty container that is equal to itself (with no values to compare)
instance Eq2 Neither where
    liftEq2 _ _ _ _ = True

-- | Empty container that is equal to itself (with no values to compare)
instance Ord2 Neither where
    liftCompare2 _ _ _ _ = EQ

