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

-- Flexible approach -- we can use this to generate HTML, text,
-- a structure like Pandoc, etc.  TODO:  make the function take
-- NodeType rather than a NodePtr, and add a NodePtr -> NodeType
-- conversion.
handleNode :: (NodePtr -> [a] -> a) -> NodePtr -> a
handleNode f ptr = f ptr children
   where children = handleNodes f $ c_cmark_node_first_child ptr
         handleNodes f ptr =
           if ptr == nullPtr
              then []
              else handleNode f ptr : handleNodes f (c_cmark_node_next ptr)

toNode :: NodePtr -> Node
toNode = handleNode ptrToNode

ptrToNode :: NodePtr -> [Node] -> Node
ptrToNode ptr xs = Node z xs
  where z = case (c_cmark_node_get_type ptr) of
             #const CMARK_NODE_DOCUMENT
               -> DOCUMENT
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
        string_content = unsafePerformIO $ peekCString $
                         c_cmark_node_get_string_content ptr

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

