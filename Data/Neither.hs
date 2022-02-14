module Data.Neither where

import Data.Functor.Contravariant
import Control.Applicative
import Control.Monad
import Control.Monad.Zip
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Arrow as Arrow
import Control.Category as Cat
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Functor.Classes

data Neither a b = Neither deriving (Read, Show, Eq, Ord, Enum, Bounded)

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

instance MonadFail (Neither a) where
    fail _ = Neither

instance MonadIO (Neither a) where
    liftIO _ = Neither

instance Semigroup (Neither a b) where
    _ <> _ = Neither

instance Monoid (Neither a b) where
    mempty = Neither

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

instance Foldable (Neither a) where
    foldMap _ _ = mempty
    foldr _ x _ = x
    foldl _ x _ = x
    elem _ _ = False
    sum _ = 0
    product _ = 1

instance Traversable (Neither a) where
    traverse _ _ = pure Neither
    sequenceA _ = pure Neither
    mapM _ _ = pure Neither
    sequence _ = pure Neither

instance Bifoldable Neither where
    bifold _ = mempty
    bifoldMap _ _ _ = mempty
    bifoldr _ _ x _ = x
    bifoldl _ _ x _ = x

instance Bitraversable Neither where
    bitraverse _ _ _ = pure Neither

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
