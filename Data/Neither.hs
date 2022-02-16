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

data Neither a b = Neither deriving (Read, Show, Eq, Ord, Enum, Bounded, Typeable, Data)

neither :: a -> b -> Neither a b
neither _ _ = Neither
-- ^ Produces a Neither value whose types match the arguments, discarding the arguments

instance Functor (Neither a) where
-- ^ An empty container
    fmap _ _ = Neither
    _ <$ _ = Neither

instance Applicative (Neither a) where
-- ^ An empty container
    pure _ = Neither
    _ <*> _ = Neither
    _ *> _ = Neither
    _ <* _ = Neither

instance Monad (Neither a) where
-- ^ An empty container
    _ >>= _ = Neither

instance MonadFail (Neither a) where
-- ^ An empty container
    fail _ = Neither

instance MonadIO (Neither a) where
-- ^ An empty container
    liftIO _ = Neither

instance Semigroup (Neither a b) where
-- ^ The trivial single element semigroup
    _ <> _ = Neither

instance Monoid (Neither a b) where
-- ^ The trivial single element monoid
    mempty = Neither

instance Num (Neither a b) where
-- ^ A trivial single element group with no-ops, where every integer is Neither
    _ + _ = Neither
    _ - _ = Neither
    _ * _ = Neither
    negate _ = Neither
    abs _ = Neither
    signum _ = Neither
    fromInteger _ = Neither

instance Real (Neither a b) where
-- ^ A number type that only contains zero
    toRational _ = 0

instance Integral (Neither a b) where
-- ^ A number type that only contains zero, or a trivial single element euclidean domain
    quot _ _ = Neither
    rem _ _ = Neither
    div _ _ = Neither
    mod _ _ = Neither
    quotRem _ _ = (Neither, Neither)
    divMod _ _ = (Neither, Neither)
    toInteger _ = 0

instance Fractional (Neither a b) where
-- ^ A trivial single element field where every ratio is Neither
    _ / _ = Neither
    recip _ = Neither
    fromRational _ = Neither

instance Floating (Neither a b) where
-- ^ A trivial single element exponential field, where every number is Neither
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

instance RealFrac (Neither a b) where
-- A number type that only contains zero
    properFraction _ = (0, Neither)
    truncate _ = 0
    round _ = 0
    ceiling _ = 0
    floor _ = 0

instance Bits (Neither a b) where
-- ^ A bitstring with zero length
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

instance FiniteBits (Neither a b) where
-- ^ A bitstring with zero length
    finiteBitSize _ = 0
    countLeadingZeros _ = 0
    countTrailingZeros _ = 0

instance Alternative (Neither a) where
-- ^ An empty container with no-ops
    empty = Neither
    _ <|> _ = Neither
    some _ = Neither
    many _ = Neither

instance MonadPlus (Neither a) where
-- ^ An empty container with no-ops
    mzero = Neither
    mplus _ _ = Neither

instance Contravariant (Neither a) where
-- ^ An empty container or an uncallable or no-op function representation
    contramap _ _ = Neither
    _ >$ _ = Neither

instance Bifunctor Neither where
-- ^ An empty container
    bimap _ _ _ = Neither
    first _ _ = Neither
    second _ _ = Neither

instance MonadZip (Neither a) where
-- ^ An empty container
    mzip _ _ = Neither
    mzipWith _ _ _ = Neither
    munzip _ = (Neither, Neither)

instance MonadFix (Neither a) where
-- ^ All functions from Neither to Neither must have Neither as a fixed point (if you ignore types)
    mfix _ = Neither

instance Foldable (Neither a) where
-- ^ An empty container which folds to the starting value or the identity
    foldMap _ _ = mempty
    foldr _ x _ = x
    foldl _ x _ = x
    elem _ _ = False
    sum _ = 0
    product _ = 1

instance Traversable (Neither a) where
-- ^ An empty container, that, when in an applicative, must always be a minimally wrapped constant value
    traverse _ _ = pure Neither
    sequenceA _ = pure Neither
    mapM _ _ = pure Neither
    sequence _ = pure Neither

instance Bifoldable Neither where
-- ^ An empty container which folds to the starting value or the identity
    bifold _ = mempty
    bifoldMap _ _ _ = mempty
    bifoldr _ _ x _ = x
    bifoldl _ _ x _ = x

instance Bitraversable Neither where
-- ^ An empty container, that, when in an applicative, must always be a minimally wrapped constant value
    bitraverse _ _ _ = pure Neither

instance Category Neither where
-- ^ The constant functor from Set to 1
    id = Neither
    _ . _ = Neither

instance Arrow Neither where
-- ^ A dummy function representation
    arr _ = Neither
    first _ = Neither
    second _ = Neither
    _ *** _ = Neither
    _ &&& _ = Neither

instance ArrowZero Neither where
-- ^ A dummy function representation, so the zero arrow is merely unique by its type parameters
    zeroArrow = Neither

instance ArrowPlus Neither where
-- ^ The trivial single element monoid on a dummy function representation
    _ <+> _ = Neither

instance ArrowChoice Neither where
-- ^ A dummy function representation, where all choices are the same
    left _ = Neither
    right _ = Neither
    _ +++ _ = Neither
    _ ||| _ = Neither

instance ArrowApply Neither where
-- ^ A dummy function representation, where application returns another dummy
    app = Neither

instance ArrowLoop Neither where
-- ^ A dummy function representation, where recursion creation returns another dummy
    loop _ = Neither

instance IsString (Neither a b) where
-- ^ Every string is Neither
    fromString _ = Neither

instance Ix (Neither a b) where
-- ^ There is only one Neither value
    range _ = [Neither]
    index _ _ = 0
    inRange _ _ = True
    rangeSize _ = 1

instance Eq1 (Neither a) where
-- ^ Empty container that is equal to itself (with no values to compare)
    liftEq _ _ _ = True

instance Ord1 (Neither a) where
-- ^ Empty container that is equal to itself (with no values to compare)
    liftCompare _ _ _ = EQ

instance Eq2 Neither where
-- ^ Empty container that is equal to itself (with no values to compare)
    liftEq2 _ _ _ _ = True

instance Ord2 Neither where
-- ^ Empty container that is equal to itself (with no values to compare)
    liftCompare2 _ _ _ _ = EQ

