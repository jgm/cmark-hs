{-# LANGUAGE CPP, ForeignFunctionInterface, GeneralizedNewtypeDeriving,
    DeriveGeneric, DeriveDataTypeable, FlexibleContexts #-}

module CMark (
    Node
  , NodeType
  , PosInfo
  , markdownToHtml
  , parseDocument
  , optSourcePos
  , optNormalize
  , optHardBreaks
  , optSmart
  ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String (CString)
import qualified System.IO.Unsafe as Unsafe
import GHC.Generics (Generic)
import Data.Generics (Data, Typeable)
import Data.Bits ( (.|.) )
import Data.Text (Text)
import qualified Data.Text.Foreign as TF

#include <cmark.h>

type NodePtr = Ptr ()

data Node = Node (Maybe PosInfo) NodeType [Node]
     deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

data NodeType =
    DOCUMENT
  | HRULE
  | PARAGRAPH
  | BLOCK_QUOTE
  | HEADER Int
  | TEXT Text
  | EMPH
  | STRONG
  | SOFTBREAK
  | LINEBREAK
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

type PosInfo = Int -- for now

-- | A type for PCRE compile-time options. These are newtyped CInts,
-- which can be bitwise-or'd together, using '(Data.Bits..|.)'
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
             #const CMARK_NODE_EMPH
               -> EMPH
             #const CMARK_NODE_STRONG
               -> STRONG
             #const CMARK_NODE_TEXT
               -> TEXT string_content
             #const CMARK_NODE_SOFTBREAK
               -> SOFTBREAK
             #const CMARK_NODE_LINEBREAK
               -> LINEBREAK
  where string_content = Unsafe.unsafePerformIO $
                            TF.peekCStringLen (str, c_strlen str)
        str            = c_cmark_node_get_literal ptr

handleNode :: (Maybe PosInfo -> NodeType -> [a] -> a) -> NodePtr -> a
handleNode f ptr = f posinfo (ptrToNodeType ptr) children
   where children = handleNodes f $ c_cmark_node_first_child ptr
         posinfo = Nothing
         handleNodes f ptr =
           if ptr == nullPtr
              then []
              else handleNode f ptr : handleNodes f (c_cmark_node_next ptr)

toNode :: NodePtr -> Node
toNode = handleNode Node

foreign import ccall "string.h strlen"
    c_strlen :: CString -> Int

foreign import ccall "cmark.h cmark_markdown_to_html"
    c_cmark_markdown_to_html :: CString -> Int -> CInt -> CString

foreign import ccall "cmark.h cmark_parse_document"
    c_cmark_parse_document :: CString -> Int -> CInt -> NodePtr

foreign import ccall "cmark.h cmark_node_get_type"
    c_cmark_node_get_type :: NodePtr -> Int

foreign import ccall "cmark.h cmark_node_first_child"
    c_cmark_node_first_child :: NodePtr -> NodePtr

foreign import ccall "cmark.h cmark_node_next"
    c_cmark_node_next :: NodePtr -> NodePtr

foreign import ccall "cmark.h cmark_node_get_literal"
    c_cmark_node_get_literal :: NodePtr -> CString

markdownToHtml :: [CMarkOption] -> Text -> Text
markdownToHtml opts s = Unsafe.unsafePerformIO $
  TF.withCStringLen s $ \(ptr, len) -> do
    let str = c_cmark_markdown_to_html ptr len (combineOptions opts)
    let len = c_strlen str
    TF.peekCStringLen (str, len)

parseDocument :: [CMarkOption] -> Text -> Node
parseDocument opts s =
  Unsafe.unsafePerformIO $
      TF.withCStringLen s $ \(ptr, len) ->
        return $ toNode $ c_cmark_parse_document ptr len (combineOptions opts)
