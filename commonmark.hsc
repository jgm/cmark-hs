{-# LANGUAGE CPP, ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}

module CommonMark (
    CommonMark
  , markdownToHtml
  , withDocument
  , renderHtml
  ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe
import Control.Monad.State

#include <cmark.h>

type NodePtr = Ptr ()

-- | A 'Node' is a mutable object that just wraps a pointer
-- to the C structure.

newtype CommonMark a = CommonMark (State NodePtr a)
  deriving (Monad)

foreign import ccall "cmark.h cmark_markdown_to_html"
    c_cmark_markdown_to_html :: CString -> Int -> CString

foreign import ccall "cmark.h cmark_parse_document"
    c_cmark_parse_document :: CString -> Int -> NodePtr

foreign import ccall "cmark.h cmark_render_html"
    c_cmark_render_html :: NodePtr -> CString

markdownToHtml :: String -> String
markdownToHtml s = unsafePerformIO $
  withCStringLen s $ \(ptr, len) ->
    peekCString $ c_cmark_markdown_to_html ptr len

withDocument :: String -> CommonMark a -> a
withDocument s (CommonMark prog) =
  evalState prog (unsafePerformIO $
      withCStringLen s $ \(ptr, len) ->
        return $ c_cmark_parse_document ptr len)

renderHtml :: CommonMark String
renderHtml = CommonMark $ do
  ndptr <- get
  return $ unsafePerformIO $ peekCString $ c_cmark_render_html ndptr
