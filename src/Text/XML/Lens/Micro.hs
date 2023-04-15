{-# LANGUAGE LambdaCase #-}
{-# language ExistentialQuantification #-}
{-# language RecordWildCards #-}
{-# language Rank2Types #-}
{-# options_ghc -Wno-unused-imports #-}
module Text.XML.Lens.Micro (
  -- * Traversals
  named,
  nodes,
  -- ** node attribute combinators
  attrs,
  attributeSatisfies,
  attributeIs,
  withoutAttribute,
  -- * prisms
  previewP,
  _Element,
  _Content,
  Prism'
                           ) where

import Data.Maybe (isNothing)
import Data.Monoid (First(..))

-- case-insensitive
import qualified Data.CaseInsensitive as CI
-- containers
import Data.Map (Map)
-- microlens
import Lens.Micro.GHC (Getting, Lens', Traversal', ix, filtered)
import Lens.Micro.Extras (preview)
-- text
import Data.Text (Text)
-- xml-conduit
import Text.XML (Element(..), Name(..), Node(..))

-- | Traverse elements which has the specified *local* name (case-insensitive).
named :: CI.CI Text -> Traversal' Element Element
named n f s
    | CI.mk (nameLocalName (elementName s)) == n = f s
    | otherwise = pure s
{-# INLINE named #-}

-- | All 'Node's of an 'Element'
nodes :: Lens' Element [Node]
nodes f e = fmap (\x -> e { elementNodes = x }) $ f $ elementNodes e
{-# INLINE nodes #-}

-- | Node attributes
attrs :: Lens' Element (Map Name Text)
attrs f e = fmap (\x -> e { elementAttributes = x }) $ f $ elementAttributes e
{-# INLINE attrs #-}

attributeSatisfies :: Name -- ^ attribute name
                   -> (Text -> Bool) -- ^ predicate on the value of the attribute
                   -> Traversal' Element Element
attributeSatisfies n p = attributeSatisfies' n (maybe False p)
{-# INLINE attributeSatisfies #-}

attributeSatisfies' :: Name -> (Maybe Text -> Bool) -> Traversal' Element Element
attributeSatisfies' n p = filtered (p . preview (attrs . ix n))
{-# INLINE attributeSatisfies' #-}

withoutAttribute :: Name -> Traversal' Element Element
withoutAttribute n = attributeSatisfies' n isNothing
{-# INLINE withoutAttribute #-}

attributeIs :: Name -- ^ attribute name
            -> Text -- ^ value of the attribute
            -> Traversal' Element Element
attributeIs n v = attributeSatisfies n (==v)
{-# INLINE attributeIs #-}


-- from https://blog.jle.im/entry/lenses-products-prisms-sums.html#through-the-looking-prism
data Prism' s a = forall q. Prism'
    { match  :: s -> Either a q
    , inject :: Either a q -> s
    }

-- | Focus on node elements
_Element :: Prism' Node Element
_Element = Prism' {
  match = \case
      NodeElement e -> Left e
      i -> Right i
  , inject = \case
      Left e -> NodeElement e
      Right i -> i
                  }

-- | Focus on the text content of nodes
_Content :: Prism' Node Text
_Content = Prism' {
  match = \case
      NodeContent c -> Left c
      i -> Right i
  , inject = \case
      Left c -> NodeContent c
      Right i -> i
                  }



-- | 'preview' for 'Prism''
previewP :: Prism' s a -> (s -> Maybe a)
previewP Prism'{..} x = case match x of
    Left  y -> Just y
    Right _ -> Nothing

