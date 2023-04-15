{-# language ExistentialQuantification #-}
{-# language RecordWildCards #-}
{-# language Rank2Types #-}
{-# options_ghc -Wno-unused-imports #-}
module Text.XML.Lens.Micro where

import Data.Maybe (isNothing)

-- case-insensitive
import qualified Data.CaseInsensitive as CI
-- containers
import Data.Map (Map)
-- microlens
import Lens.Micro.GHC (Lens', Traversal', ix, filtered)
import Lens.Micro.Extras (preview)
-- text
import Data.Text (Text)
-- xml-conduit
import Text.XML (Element(..), nameLocalName, Name(..), Node(..))

-- | Traverse elements which has the specified *local* name (case-insensitive).
named :: CI.CI Text -> Traversal' Element Element
named n f s
    | CI.mk (nameLocalName (elementName s)) == n = f s
    | otherwise = pure s
{-# INLINE named #-}

nodes :: Lens' Element [Node]
nodes f e = fmap (\x -> e { elementNodes = x }) $ f $ elementNodes e
{-# INLINE nodes #-}


attrs :: Lens' Element (Map Name Text)
attrs f e = fmap (\x -> e { elementAttributes = x }) $ f $ elementAttributes e
{-# INLINE attrs #-}

attributeSatisfies :: Name -> (Text -> Bool) -> Traversal' Element Element
attributeSatisfies n p = attributeSatisfies' n (maybe False p)
{-# INLINE attributeSatisfies #-}

attributeSatisfies' :: Name -> (Maybe Text -> Bool) -> Traversal' Element Element
attributeSatisfies' n p = filtered (p . preview (attrs . ix n))
{-# INLINE attributeSatisfies' #-}

withoutAttribute :: Name -> Traversal' Element Element
withoutAttribute n = attributeSatisfies' n isNothing
{-# INLINE withoutAttribute #-}

attributeIs :: Name -> Text -> Traversal' Element Element
attributeIs n v = attributeSatisfies n (==v)
{-# INLINE attributeIs #-}


-- from https://blog.jle.im/entry/lenses-products-prisms-sums.html#through-the-looking-prism
data Prism' s a = forall q. Prism'
    { match  :: s -> Either a q
    , inject :: Either a q -> s
    }

-- preview :: Prism' s a -> (s -> Maybe a)
-- preview Prism'{..} x = case match x of
--     Left  y -> Just y
--     Right _ -> Nothing

{- prisms for sum types

data Shape = Circle  Double           -- radius
           | RegPoly Natural Double   -- number of sides, length of sides

_Circle :: Prism' Shape Double
_Circle = Prism'
    { match  = \case
        Circle  r    -> Left r
        RegPoly n s  -> Right (n, s)
    , inject = \case
        Left   r     -> Circle r
        Right (n, s) -> RegPoly n s
-}
