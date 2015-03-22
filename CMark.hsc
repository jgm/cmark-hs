{-# LANGUAGE CPP, ForeignFunctionInterface, GeneralizedNewtypeDeriving,
    DeriveGeneric, DeriveDataTypeable, FlexibleContexts #-}

module CMark (
    commonmarkToHtml
  , commonmarkToXml
  , commonmarkToMan
  , commonmarkToNode
  , nodeToCommonmark
  , optSourcePos
  , optNormalize
  , optHardBreaks
  , optSmart
  , Node(..)
  , NodeType(..)
  , PosInfo(..)
  , DelimType(..)
  , ListType(..)
  , ListAttributes(..)
  , Url
  , Title
  , Level
  , Info
  , CMarkOption
  ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String (CString, withCString)
import qualified System.IO.Unsafe as Unsafe
import GHC.Generics (Generic)
import Control.Applicative ((<$>))
import Data.List (foldl')
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Text (Text, empty, pack, unpack)
import qualified Data.Text.Foreign as TF

#include <cmark.h>

type NodePtr = Ptr ()

data Node = Node (Maybe PosInfo) NodeType [Node]
     deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

data DelimType =
    PERIOD_DELIM
  | PAREN_DELIM
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

data ListType =
    BULLET_LIST
  | ORDERED_LIST
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

data ListAttributes = ListAttributes{
    listType     :: ListType
  , listTight    :: Bool
  , listStart    :: Int
  , listDelim    :: DelimType
  } deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

type Url = Text

type Title = Text

type Level = Int

type Info = Text

data NodeType =
    DOCUMENT
  | HRULE
  | PARAGRAPH
  | BLOCK_QUOTE
  | HTML Text
  | CODE_BLOCK Info Text
  | HEADER Level
  | LIST ListAttributes
  | ITEM
  | TEXT Text
  | SOFTBREAK
  | LINEBREAK
  | INLINE_HTML Text
  | CODE Text
  | EMPH
  | STRONG
  | LINK Url Title
  | IMAGE Url Title
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

data PosInfo = PosInfo{ startLine   :: Int
                      , startColumn :: Int
                      , endLine     :: Int
                      , endColumn   :: Int
                      }
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

newtype CMarkOption = CMarkOption { unCMarkOption :: CInt }

-- | Combine a list of options into a single option, using bitwise or.
combineOptions :: [CMarkOption] -> CInt
combineOptions = foldr ((.|.) . unCMarkOption) 0

-- Include a @data-sourcepos@ attribute on block elements.
optSourcePos :: CMarkOption
optSourcePos = CMarkOption #const CMARK_OPT_SOURCEPOS

-- Render @softbreak@ elements as hard line breaks.
optHardBreaks :: CMarkOption
optHardBreaks = CMarkOption #const CMARK_OPT_HARDBREAKS

-- Normalize the document by consolidating adjacent text nodes.
optNormalize :: CMarkOption
optNormalize = CMarkOption #const CMARK_OPT_NORMALIZE

-- Convert straight quotes to curly, @---@ to em-dash, @--@ to en-dash.
optSmart :: CMarkOption
optSmart = CMarkOption #const CMARK_OPT_SMART

ptrToNodeType :: NodePtr -> NodeType
ptrToNodeType ptr =
  case (c_cmark_node_get_type ptr) of
             #const CMARK_NODE_DOCUMENT
               -> DOCUMENT
             #const CMARK_NODE_HRULE
               -> HRULE
             #const CMARK_NODE_PARAGRAPH
               -> PARAGRAPH
             #const CMARK_NODE_BLOCK_QUOTE
               -> BLOCK_QUOTE
             #const CMARK_NODE_HTML
               -> HTML literal
             #const CMARK_NODE_CODE_BLOCK
               -> CODE_BLOCK info literal
             #const CMARK_NODE_LIST
               -> LIST listAttr
             #const CMARK_NODE_ITEM
               -> ITEM
             #const CMARK_NODE_HEADER
               -> HEADER level
             #const CMARK_NODE_EMPH
               -> EMPH
             #const CMARK_NODE_STRONG
               -> STRONG
             #const CMARK_NODE_LINK
               -> LINK url title
             #const CMARK_NODE_IMAGE
               -> IMAGE url title
             #const CMARK_NODE_TEXT
               -> TEXT literal
             #const CMARK_NODE_CODE
               -> CODE literal
             #const CMARK_NODE_INLINE_HTML
               -> INLINE_HTML literal
             #const CMARK_NODE_SOFTBREAK
               -> SOFTBREAK
             #const CMARK_NODE_LINEBREAK
               -> LINEBREAK
             _ -> error "Unknown node type"
  where literal   = totext $ c_cmark_node_get_literal ptr
        level     = c_cmark_node_get_header_level ptr
        listAttr  = ListAttributes{
            listType  = case c_cmark_node_get_list_type ptr of
                             (#const CMARK_ORDERED_LIST) -> ORDERED_LIST
                             (#const CMARK_BULLET_LIST)  -> BULLET_LIST
                             _                           -> BULLET_LIST
          , listDelim  = case c_cmark_node_get_list_delim ptr of
                             (#const CMARK_PERIOD_DELIM) -> PERIOD_DELIM
                             (#const CMARK_PAREN_DELIM)  -> PAREN_DELIM
                             _                           -> PERIOD_DELIM
          , listTight  = c_cmark_node_get_list_tight ptr == 1
          , listStart  = c_cmark_node_get_list_start ptr
          }
        url       = totext $ c_cmark_node_get_url ptr
        title     = totext $ c_cmark_node_get_title ptr
        info      = totext $ c_cmark_node_get_fence_info ptr

getPosInfo :: NodePtr -> Maybe PosInfo
getPosInfo ptr =
   case (c_cmark_node_get_start_line ptr,
         c_cmark_node_get_start_column ptr,
         c_cmark_node_get_end_line ptr,
         c_cmark_node_get_end_column ptr) of
       (0, 0, 0, 0) -> Nothing
       (sl, sc, el, ec) -> Just PosInfo{ startLine = sl
                                       , startColumn = sc
                                       , endLine = el
                                       , endColumn = ec }

handleNode :: (Maybe PosInfo -> NodeType -> [a] -> a) -> NodePtr -> a
handleNode f ptr = f posinfo (ptrToNodeType ptr) children
   where children = handleNodes f $ c_cmark_node_first_child ptr
         posinfo  = getPosInfo ptr
         handleNodes f' ptr' =
           if ptr' == nullPtr
              then []
              else handleNode f' ptr' : handleNodes f' (c_cmark_node_next ptr')

toNode :: NodePtr -> Node
toNode = handleNode Node

withLiteral :: Text -> IO NodePtr -> IO NodePtr
withLiteral t getnode = do
  n <- getnode
  c_cmark_node_set_literal n (fromtext t)
  return n

fromNode :: Node -> IO NodePtr
fromNode (Node _ nodeType children) = do
  base <- case nodeType of
            DOCUMENT    -> c_cmark_node_new (#const CMARK_NODE_DOCUMENT)
            HRULE       -> c_cmark_node_new (#const CMARK_NODE_HRULE)
            PARAGRAPH   -> c_cmark_node_new (#const CMARK_NODE_PARAGRAPH)
            BLOCK_QUOTE -> c_cmark_node_new (#const CMARK_NODE_BLOCK_QUOTE)
            HTML literal -> withLiteral literal
                            (c_cmark_node_new (#const CMARK_NODE_HTML))
            CODE_BLOCK info literal -> withLiteral literal
                           -- TODO add info
                            (c_cmark_node_new (#const CMARK_NODE_CODE_BLOCK))
            LIST attr   -> -- TODO add attribs
                            c_cmark_node_new (#const CMARK_NODE_LIST)
            ITEM        -> c_cmark_node_new (#const CMARK_NODE_ITEM)
            HEADER lev  -> -- TODO add lev
                           c_cmark_node_new (#const CMARK_NODE_HEADER)
            EMPH        -> c_cmark_node_new (#const CMARK_NODE_EMPH)
            STRONG      -> c_cmark_node_new (#const CMARK_NODE_STRONG)
            LINK url title ->
                           -- TODO add url, title
                           c_cmark_node_new (#const CMARK_NODE_LINK)
            IMAGE url title ->
                           -- TODO add url, title
                           c_cmark_node_new (#const CMARK_NODE_IMAGE)
            TEXT literal -> withLiteral literal
                            (c_cmark_node_new (#const CMARK_NODE_TEXT))
            CODE literal -> withLiteral literal
                            (c_cmark_node_new (#const CMARK_NODE_CODE))
            INLINE_HTML literal -> withLiteral literal
                            (c_cmark_node_new (#const CMARK_NODE_INLINE_HTML))
            SOFTBREAK   -> c_cmark_node_new (#const CMARK_NODE_SOFTBREAK)
            LINEBREAK   -> c_cmark_node_new (#const CMARK_NODE_LINEBREAK)
  mapM_ (\child -> fromNode child >>= c_cmark_node_append_child base) children
  return base

-- | Convert CommonMark formatted text to Html, using cmark's
-- built-in renderer.
commonmarkToHtml :: [CMarkOption] -> Text -> Text
commonmarkToHtml opts s = io $
  TF.withCStringLen s $ \(ptr, len) ->
    return (totext $ c_cmark_markdown_to_html ptr len (combineOptions opts))

-- | Convert CommonMark formatted text to CommonMark XML, using cmark's
-- built-in renderer.
commonmarkToXml :: [CMarkOption] -> Text -> Text
commonmarkToXml opts s = io $
  TF.withCStringLen s $ \(ptr, len) -> do
    let opts' = combineOptions opts
    doc <- c_cmark_parse_document ptr len opts'
    return (totext $ c_cmark_render_xml doc opts')

-- | Convert CommonMark formatted text to groff man, using cmark's
-- built-in renderer.
commonmarkToMan :: [CMarkOption] -> Text -> Text
commonmarkToMan opts s = io $
  TF.withCStringLen s $ \(ptr, len) -> do
    let opts' = combineOptions opts
    doc <- c_cmark_parse_document ptr len opts'
    return (totext $ c_cmark_render_man doc opts')

-- | Convert CommonMark formatted text to a structured 'Node' tree,
-- which can be transformed or rendered using Haskell code.
commonmarkToNode :: [CMarkOption] -> Text -> Node
commonmarkToNode opts s = io $
      TF.withCStringLen s $ \(ptr, len) ->
        toNode <$> c_cmark_parse_document ptr len (combineOptions opts)

nodeToCommonmark :: [CMarkOption] -> Int -> Node -> Text
nodeToCommonmark opts width node = totext $
  c_cmark_render_commonmark (io $ fromNode node) (combineOptions opts) width

io :: IO a -> a
io = Unsafe.unsafePerformIO

totext :: CString -> Text
totext str
  | str == nullPtr = empty
  | otherwise      = io $ TF.peekCStringLen (str, c_strlen str)

fromtext :: Text -> CString
fromtext t = io $ withCString (unpack t) return

foreign import ccall "string.h strlen"
    c_strlen :: CString -> Int

foreign import ccall "cmark.h cmark_node_new"
    c_cmark_node_new :: Int -> IO NodePtr

foreign import ccall "cmark.h cmark_markdown_to_html"
    c_cmark_markdown_to_html :: CString -> Int -> CInt -> CString

foreign import ccall "cmark.h cmark_render_xml"
    c_cmark_render_xml :: NodePtr -> CInt -> CString

foreign import ccall "cmark.h cmark_render_man"
    c_cmark_render_man :: NodePtr -> CInt -> CString

foreign import ccall "cmark.h cmark_render_commonmark"
    c_cmark_render_commonmark :: NodePtr -> CInt -> Int -> CString

foreign import ccall "cmark.h cmark_parse_document"
    c_cmark_parse_document :: CString -> Int -> CInt -> IO NodePtr

foreign import ccall "cmark.h cmark_node_get_type"
    c_cmark_node_get_type :: NodePtr -> Int

foreign import ccall "cmark.h cmark_node_first_child"
    c_cmark_node_first_child :: NodePtr -> NodePtr

foreign import ccall "cmark.h cmark_node_next"
    c_cmark_node_next :: NodePtr -> NodePtr

foreign import ccall "cmark.h cmark_node_get_literal"
    c_cmark_node_get_literal :: NodePtr -> CString

foreign import ccall "cmark.h cmark_node_get_url"
    c_cmark_node_get_url :: NodePtr -> CString

foreign import ccall "cmark.h cmark_node_get_title"
    c_cmark_node_get_title :: NodePtr -> CString

foreign import ccall "cmark.h cmark_node_get_header_level"
    c_cmark_node_get_header_level :: NodePtr -> Int

foreign import ccall "cmark.h cmark_node_get_list_type"
    c_cmark_node_get_list_type :: NodePtr -> Int

foreign import ccall "cmark.h cmark_node_get_list_tight"
    c_cmark_node_get_list_tight :: NodePtr -> Int

foreign import ccall "cmark.h cmark_node_get_list_start"
    c_cmark_node_get_list_start :: NodePtr -> Int

foreign import ccall "cmark.h cmark_node_get_list_delim"
    c_cmark_node_get_list_delim :: NodePtr -> Int

foreign import ccall "cmark.h cmark_node_get_fence_info"
    c_cmark_node_get_fence_info :: NodePtr -> CString

foreign import ccall "cmark.h cmark_node_get_start_line"
    c_cmark_node_get_start_line :: NodePtr -> Int

foreign import ccall "cmark.h cmark_node_get_start_column"
    c_cmark_node_get_start_column :: NodePtr -> Int

foreign import ccall "cmark.h cmark_node_get_end_line"
    c_cmark_node_get_end_line :: NodePtr -> Int

foreign import ccall "cmark.h cmark_node_get_end_column"
    c_cmark_node_get_end_column :: NodePtr -> Int

foreign import ccall "cmark.h cmark_node_append_child"
    c_cmark_node_append_child :: NodePtr -> NodePtr -> IO Int

foreign import ccall "cmark.h cmark_node_set_literal"
    c_cmark_node_set_literal :: NodePtr -> CString -> IO Int

foreign import ccall "cmark.h cmark_node_set_url"
    c_cmark_node_set_url :: NodePtr -> CString -> IO Int

foreign import ccall "cmark.h cmark_node_set_title"
    c_cmark_node_set_title :: NodePtr -> CString -> IO Int

foreign import ccall "cmark.h cmark_node_set_header_level"
    c_cmark_node_set_header_level :: NodePtr -> Int -> IO Int

foreign import ccall "cmark.h cmark_node_set_list_type"
    c_cmark_node_set_list_type :: NodePtr -> Int -> IO Int

foreign import ccall "cmark.h cmark_node_set_list_tight"
    c_cmark_node_set_list_tight :: NodePtr -> Int -> IO Int

foreign import ccall "cmark.h cmark_node_set_list_start"
    c_cmark_node_set_list_start :: NodePtr -> Int -> IO Int

foreign import ccall "cmark.h cmark_node_set_list_delim"
    c_cmark_node_set_list_delim :: NodePtr -> Int -> IO Int

foreign import ccall "cmark.h cmark_node_set_fence_info"
    c_cmark_node_set_fence_info :: NodePtr -> CString -> IO Int

foreign import ccall "cmark.h cmark_node_set_start_line"
    c_cmark_node_set_start_line :: NodePtr -> Int -> IO Int

foreign import ccall "cmark.h cmark_node_set_start_column"
    c_cmark_node_set_start_column :: NodePtr -> Int -> IO Int
