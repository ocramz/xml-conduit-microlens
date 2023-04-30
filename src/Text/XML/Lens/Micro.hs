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
-- XML (and HTML) DOM selectors for `xml-conduit` based on `microlens`.
--
-- This library provides combinators for traversing and folding over XML trees.
-- It could be useful for editing trees, adding attributes selectively (e.g. refactoring CSS,
-- adding HTMX attributes etc.)
--
-- Some definitions are taken from 'xml-lens' but we import 'microlens' to achieve
-- a smaller dependency footprint.
-----------------------------------------------------------------------------
module Text.XML.Lens.Micro (
  subtree,
  remapAttributes,
  -- * From 'xml-lens'
  root,
  prologue,
  epilogue,
  named,
  nodes,
  attrs,
  attributeSatisfies,
  attributeIs,
  withoutAttribute,

                           ) where


import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid (First(..), Any(..), Ap(..))
import GHC.Exception (SomeException(..))

-- case-insensitive
import qualified Data.CaseInsensitive as CI
-- containers
import qualified Data.Map as M (Map, insert, lookup, singleton, fromList, foldrWithKey)
-- microlens
import Lens.Micro.GHC (to, (&), Getting, Lens, Lens', lens, (^.), (^..), (.~), Traversal', ix, mapped, traversed, filtered, _Just)
import Lens.Micro.Extras (preview)
-- text
import Data.Text (Text)
import qualified Data.Text.Lazy as TL (Text)
import qualified Data.Text.Lazy.IO as TL (putStrLn)
-- xml-conduit
import Text.XML (def, Prologue(..), Doctype(..), Document(..), Element(..), Name(..), Node(..), Miscellaneous(..), ParseSettings, RenderSettings, parseText, renderText)




-- | The root element of the 'Document'.
root :: Lens' Document Element
root f doc = fmap (\p -> doc { documentRoot = p} ) $ f $ documentRoot doc
{-# INLINE root #-}

-- | 'Prologue' of the 'Document'
prologue :: Lens' Document Prologue
prologue f doc = fmap (\p -> doc { documentPrologue = p} ) $ f $ documentPrologue doc
{-# INLINE prologue #-}

-- | Epilogue, i.e. the last elements, of the 'Document'
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

-- | Traverse over only the elements such that the value of the given attribute satisfy a predicate
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

-- | Traverse over only the elements with a given attribute name and value
attributeIs :: Name -- ^ attribute name
            -> Text -- ^ value of the attribute
            -> Traversal' Element Element
attributeIs n v = attributeSatisfies n (== v)
{-# INLINE attributeIs #-}


-- | Extract a DOM subtree whose root element satisfies the given predicates
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

-- _subtree2 f h el@(Element n ats nds) =
--   case f (nameLocalName n) && (getAny $ M.foldrWithKey (\k v acc -> Any (h (nameLocalName k) v) <> acc) mempty ats) of
--     -- True -> pure el
--     False -> foldMap g nds
--       where
--         g = \case
--           NodeElement e -> _subtree2 f h e
--           _ -> mempty


-- | Remap all attributes. Handy for editing HREF or SRC targets, adding HTMX attributes to certain elements only, etc.
--
-- If the callback returns Nothing, the element attributes are left unchanged
remapAttributes ::
  (Name -> M.Map Name Text -> Maybe (M.Map Name Text)) -- ^ element name, element attributes
  -> Getting r Element Element
remapAttributes f = to (_remapAttributes f)

_remapAttributes :: (Name -> M.Map Name Text -> Maybe (M.Map Name Text))
                     -> Element -> Element
_remapAttributes f el@(Element n ats _) =
  el{ elementAttributes = fromMaybe ats (f n ats),
      elementNodes = map (\nn -> case nn of
                             NodeElement e -> NodeElement (_remapAttributes f e)
                             x -> x
                         ) $ elementNodes el }


-- -- * A lens for focusing on a given subtree

-- lensSubtree :: (Text -> M.Map Name Text -> Bool) -- ^ predicate on element name and element attributes
--             -> Lens' Element (Maybe Element)
-- lensSubtree f = lens' lget lset
--   where
--     lget el@(Element nam ats nds) =
--       if f (nameLocalName nam) ats
--       then Just el
--       else Nothing -- FIXME this only looks at the top level
--     lset el@(Element nam ats _) elm' =
--       if f (nameLocalName nam) ats
--       then
--         case elm' of
--           Just el' -> el'
--           Nothing -> el
--       else el

lens' :: (s -> a) -> (s -> a -> s) -> Lens' s a
lens' = lens

-- -- new attempt at lensSubtree : 

-- firstNode :: (Name -> M.Map Name Text -> Bool) -> Lens' Element (Maybe Element)
-- firstNode f = lens' (fnGet f) (fnSet f)


-- fnSet :: (Name -> M.Map Name Text -> Bool)
--       -> Element -> Maybe Element -> Element
-- fnSet f e elm' = go e
--   where
--     g :: Node -> Node
--     g = \case
--       NodeElement eBelow -> NodeElement $ go eBelow
--       x -> x
--     go (Element nam ats nds) = if f nam ats
--                              then case elm' of
--                                     Just e' -> e'
--                                     Nothing -> e
--                              else e{ elementNodes = map g nds }

-- fnGet :: (Name -> M.Map Name Text -> Bool)
--              -> Element -> Maybe Element
-- fnGet f = getFirst . go
--   where
--     go el = foldMap g $ elementNodes el
--     g = \case
--       NodeElement e@(Element nam ats _ ) ->
--         if f nam ats
--         then First $ Just e
--         else go e
--       _ -> mempty



-- t0 :: TL.Text
-- t0 = "<!DOCTYPE html><html><head><title>Page Title</title></head><body><h1>My First Heading</h1><p>My first paragraph.</p><div id=\'z42\'></div></body></html>"
-- -- --t0e :: Either SomeException Document
-- -- t0e = parseText def t0

-- -- testSet :: Document
-- testSet = case parseText def t0 of
--   Right d ->
--     let
--       d1 = d & root . firstNode (\n ats -> n == "div" && any (== "z42") ats) .~ Nothing
--       d2 = d & root . firstNode (\n ats -> n == "div" && any (== "z42") ats) .~ Just (Element "moo" mempty mempty)
--     in
--       do
--         TL.putStrLn $ renderText def d
--         TL.putStrLn $ renderText def d1
--         TL.putStrLn $ renderText def d2

-- testGet :: [Maybe Element]
-- testGet = case parseText def t0 of
--   Right dok -> dok ^.. root . firstNode (\n ats -> n == "div" && any (== "z42") ats)


-- defParseSettings :: ParseSettings
-- defParseSettings = def
-- defRenderSettings :: RenderSettings
-- defRenderSettings = def

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

