{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# language ExistentialQuantification #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language Rank2Types #-}
{-# options_ghc -Wno-unused-imports #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.XML.Lens.Micro
-- Copyright   :  (c) 2015-2023 Fumiaki Kinoshita, 2023 Marco Zocca
-- License     :  BSD-style
--
-- Maintainer  :  ocramz
-- Stability   :  experimental
-- Portability :  portable
--
--
-----------------------------------------------------------------------------
module Text.XML.Lens.Micro (
  root,
  epilogue,
  named,
  nodes,
  subtree,
  -- ** node attribute combinators
  attrs,
  attributeSatisfies,
  attributeIs,
  withoutAttribute,
  remapAttributes,
                           ) where


import Data.Maybe (isNothing)
import Data.Monoid (First(..), Any(..))

-- case-insensitive
import qualified Data.CaseInsensitive as CI
-- containers
import qualified Data.Map as M (Map, singleton, fromList, foldrWithKey)
-- microlens
import Lens.Micro.GHC (to, Getting, Lens', (^.), Traversal', ix, filtered)
import Lens.Micro.Extras (preview)
-- text
import Data.Text (Text)
-- xml-conduit
import Text.XML (Prologue(..), Doctype(..), Document(..), Element(..), Name(..), Node(..), Miscellaneous(..))




-- | The root element of the document.
root :: Lens' Document Element
root f doc = fmap (\p -> doc { documentRoot = p} ) $ f $ documentRoot doc
{-# INLINE root #-}

epilogue :: Lens' Document [Miscellaneous]
epilogue f doc = fmap (\p -> doc { documentEpilogue = p} ) $ f $ documentEpilogue doc
{-# INLINE epilogue #-}

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
attrs :: Lens' Element (M.Map Name Text)
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

-- nodesSatisfy :: ([Node] -> Bool) -> Traversal' Element Element
-- nodesSatisfy p = nodesSatisfy' (maybe False p)

-- nodesSatisfy' :: (Maybe [Node] -> Bool) -> Traversal' Element Element
-- nodesSatisfy' p = filtered (p . preview (nodes))


withoutAttribute :: Name -> Traversal' Element Element
withoutAttribute n = attributeSatisfies' n isNothing
{-# INLINE withoutAttribute #-}

attributeIs :: Name -- ^ attribute name
            -> Text -- ^ value of the attribute
            -> Traversal' Element Element
attributeIs n v = attributeSatisfies n (==v)
{-# INLINE attributeIs #-}


-- | Isolate a DOM subtree that satisfies the given predicates
subtree :: (Text -> Bool) -- ^ predicate on element name
        -> (Text -> Text -> Bool) -- ^ predicate on attribute name, value
        -> Getting r Element (Maybe Element)
subtree f h = to (_subtree f h)

_subtree :: (Text -> Bool)
         -> (Text -> Text -> Bool) -> Element -> Maybe Element
_subtree f h el@(Element n ats nds) = case f (nameLocalName n) && (getAny $ M.foldrWithKey (\k v acc -> Any (h (nameLocalName k) v) <> acc) mempty ats) of
  True -> Just el
  False -> getFirst $ foldMap (First . g) nds
  where
    g = \case
      NodeElement e -> _subtree f h e
      _ -> Nothing


-- | Handy for editing HREF targets etc.
remapAttributes :: (Name -> Text -> Maybe (Name, Text)) -- ^ operate on element attribute (name, value)
                -> Getting r Element Element
remapAttributes f = to (_remapAttributes f)

_remapAttributes :: (Name -> Text -> Maybe (Name, Text))
               -> Element -> Element
_remapAttributes p el@(Element _ ats _) =
  el{ elementAttributes = M.foldrWithKey (\k v acc -> case p k v of
                                             Nothing -> M.singleton k v <> acc
                                             Just (k', v') -> M.singleton k' v' <> acc
                                         ) mempty ats,
      elementNodes = map (\nn -> case nn of
                             NodeElement e -> NodeElement (_remapAttributes p e)
                             x -> x
                         ) $ elementNodes el }



-- t0 :: TL.Text
-- t0 = "<!DOCTYPE html><html><head><title>Page Title</title></head><body><h1>My First Heading</h1><p>My first paragraph.</p><div id=\'z42\'></div></body></html>"
-- t0e :: Either SomeException Document
-- t0e = parseText def t0

-- dok :: Document
-- dok = Document {documentPrologue = Prologue {prologueBefore = [], prologueDoctype = Just (Doctype {doctypeName = "html", doctypeID = Nothing}), prologueAfter = []}, documentRoot = Element {elementName = Name {nameLocalName = "html", nameNamespace = Nothing, namePrefix = Nothing}, elementAttributes = M.fromList [], elementNodes = [NodeElement (Element {elementName = Name {nameLocalName = "head", nameNamespace = Nothing, namePrefix = Nothing}, elementAttributes = M.fromList [], elementNodes = [NodeElement (Element {elementName = Name {nameLocalName = "title", nameNamespace = Nothing, namePrefix = Nothing}, elementAttributes = M.fromList [], elementNodes = [NodeContent "Page Title"]})]}),NodeElement (Element {elementName = Name {nameLocalName = "body", nameNamespace = Nothing, namePrefix = Nothing}, elementAttributes = M.fromList [], elementNodes = [NodeElement (Element {elementName = Name {nameLocalName = "h1", nameNamespace = Nothing, namePrefix = Nothing}, elementAttributes = M.fromList [], elementNodes = [NodeContent "My First Heading"]}),NodeElement (Element {elementName = Name {nameLocalName = "p", nameNamespace = Nothing, namePrefix = Nothing}, elementAttributes = M.fromList [], elementNodes = [NodeContent "My first paragraph."]}),NodeElement (Element {elementName = Name {nameLocalName = "div", nameNamespace = Nothing, namePrefix = Nothing}, elementAttributes = M.fromList [(Name {nameLocalName = "id", nameNamespace = Nothing, namePrefix = Nothing},"z42")], elementNodes = []})]})]}, documentEpilogue = []}












-- nodeElement :: (Element -> Maybe Element) -> Node -> Maybe Node
-- nodeElement f = \case
--   NodeElement e -> NodeElement <$> f e
--   _ -> Nothing

-- nodeContent :: (Text -> Maybe Text) -> Node -> Maybe Node
-- nodeContent f = \case
--   NodeContent c -> NodeContent <$> f c
--   _ -> Nothing

-- -- from https://blog.jle.im/entry/lenses-products-prisms-sums.html#through-the-looking-prism
-- data Prism' s a = forall q. Prism'
--     { match  :: s -> Either a q
--     , inject :: Either a q -> s
--     }

-- -- | Focus on node elements
-- _Element :: Prism' Node Element
-- _Element = Prism' {
--   match = \case
--       NodeElement e -> Left e
--       i -> Right i
--   , inject = \case
--       Left e -> NodeElement e
--       Right i -> i
--                   }

-- -- | Focus on the text content of nodes
-- _Content :: Prism' Node Text
-- _Content = Prism' {
--   match = \case
--       NodeContent c -> Left c
--       i -> Right i
--   , inject = \case
--       Left c -> NodeContent c
--       Right i -> i
--                   }


-- -- | 'preview' for 'Prism''
-- previewP :: Prism' s a -> (s -> Maybe a)
-- previewP Prism'{..} x = case match x of
--     Left  y -> Just y
--     Right _ -> Nothing

