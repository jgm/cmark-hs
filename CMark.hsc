{-# LANGUAGE CPP, ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}

module CMark (
    Node
  , NodeType
  , markdownToHtml
  , parseDocument
  ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import qualified System.IO.Unsafe as Unsafe

#include <cmark.h>

type NodePtr = Ptr ()

data Node = Node (Maybe PosInfo) NodeType [Node]
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

type PosInfo = Int -- for now

ptrToNodeType :: NodePtr -> NodeType
ptrToNodeType ptr =
  case (c_cmark_node_get_type ptr) of
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
  where string_content = Unsafe.unsafePerformIO $ peekCString $
                         c_cmark_node_get_literal ptr


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

foreign import ccall "cmark.h cmark_node_get_literal"
    c_cmark_node_get_literal :: NodePtr -> CString

markdownToHtml :: String -> String
markdownToHtml s = Unsafe.unsafePerformIO $
  withCStringLen s $ \(ptr, len) ->
    peekCString $ c_cmark_markdown_to_html ptr len

parseDocument :: String -> Node
parseDocument s =
  Unsafe.unsafePerformIO $
      withCStringLen s $ \(ptr, len) ->
        return $ toNode $ c_cmark_parse_document ptr len

