module Data.Neither where

import Data.Functor.Contravariant
import Control.Applicative
import Control.Monad
import Control.Monad.Zip
import Control.Monad.Fix
import Control.Arrow as Arrow
import Control.Category as Cat
import Data.Bifunctor

data Neither a b = Neither deriving (Read, Show, Eq, Ord)

neither :: a -> b -> Neither a b
neither _ _ = Neither

instance Functor (Neither a) where
    fmap _ _ = Neither
    _ <$ _ = Neither

instance Applicative (Neither a) where
    pure _ = Neither
    _ <*> _ = Neither
    _ *> _ = Neither
    _ <* _ = Neither

instance Monad (Neither a) where
    _ >>= _ = Neither
    _ >> _ = Neither
    return _ = Neither

instance Semigroup (Neither a b) where
    _ <> _ = Neither

instance Monoid (Neither a b) where
    mempty = Neither
    mappend _ _ = Neither

instance Alternative (Neither a) where
    empty = Neither
    _ <|> _ = Neither
    some _ = Neither
    many _ = Neither

instance MonadPlus (Neither a) where
    mzero = Neither
    mplus _ _ = Neither

instance Contravariant (Neither a) where
    contramap _ _ = Neither
    _ >$ _ = Neither

instance Bifunctor Neither where
    bimap _ _ _ = Neither
    first _ _ = Neither
    second _ _ = Neither

instance MonadZip (Neither a) where
    mzip _ _ = Neither
    mzipWith _ _ _ = Neither
    munzip _ = (Neither, Neither)

instance MonadFix (Neither a) where
    mfix _ = Neither

instance Category Neither where
    id = Neither
    _ . _ = Neither

instance Arrow Neither where
    arr _ = Neither
    first _ = Neither
    second _ = Neither
    _ *** _ = Neither
    _ &&& _ = Neither

instance ArrowZero Neither where
    zeroArrow = Neither

instance ArrowPlus Neither where
    _ <+> _ = Neither

instance ArrowChoice Neither where
    left _ = Neither
    right _ = Neither
    _ +++ _ = Neither
    _ ||| _ = Neither

instance ArrowApply Neither where
    app = Neither

instance ArrowLoop Neither where
    loop _ = Neither

