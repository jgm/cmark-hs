{-# LANGUAGE CPP, ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}

module CommonMark (
    Node
  , NodeType
  , markdownToHtml
  , parseDocument
  ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe

#include <cmark.h>

type NodePtr = Ptr ()

data Node = Node NodeType [Node]
     deriving Show

data NodeType =
    DOCUMENT
  | PARAGRAPH
  | BLOCK_QUOTE
  | HEADER Int
  | TEXT String
  | EMPH
  | STRONG
  deriving Show

toNode :: NodePtr -> Node
toNode ptr =
  case c_cmark_node_get_type ptr of
       #const CMARK_NODE_DOCUMENT
         -> Node DOCUMENT children
       #const CMARK_NODE_PARAGRAPH
         -> Node PARAGRAPH children
       #const CMARK_NODE_BLOCK_QUOTE
         -> Node BLOCK_QUOTE children
       #const CMARK_NODE_EMPH
         -> Node EMPH children
       #const CMARK_NODE_STRONG
         -> Node STRONG children
       #const CMARK_NODE_TEXT
         -> Node (TEXT string_content) []
  where children = toNodes $ c_cmark_node_first_child ptr
        string_content = unsafePerformIO $ peekCString $
                         c_cmark_node_get_string_content ptr

toNodes :: NodePtr -> [Node]
toNodes ptr =
  if ptr == nullPtr
     then []
     else toNode ptr : toNodes nextptr
  where nextptr = c_cmark_node_next ptr

foreign import ccall "cmark.h cmark_markdown_to_html"
    c_cmark_markdown_to_html :: CString -> Int -> CString

foreign import ccall "cmark.h cmark_parse_document"
    c_cmark_parse_document :: CString -> Int -> NodePtr

foreign import ccall "cmark.h cmark_node_get_type"
    c_cmark_node_get_type :: NodePtr -> Int

foreign import ccall "cmark.h cmark_node_first_child"
    c_cmark_node_first_child :: NodePtr -> NodePtr

foreign import ccall "cmark.h cmark_node_next"
    c_cmark_node_next :: NodePtr -> NodePtr

foreign import ccall "cmark.h cmark_node_get_string_content"
    c_cmark_node_get_string_content :: NodePtr -> CString

markdownToHtml :: String -> String
markdownToHtml s = unsafePerformIO $
  withCStringLen s $ \(ptr, len) ->
    peekCString $ c_cmark_markdown_to_html ptr len

parseDocument :: String -> Node
parseDocument s =
  unsafePerformIO $
      withCStringLen s $ \(ptr, len) ->
        return $ toNode $ c_cmark_parse_document ptr len

