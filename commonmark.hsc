{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module CommonMark where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe

#include <cmark.h>

foreign import ccall "cmark.h cmark_markdown_to_html"
    c_cmark_markdown_to_html :: CString -> Int -> CString

markdown_to_html :: String -> String
markdown_to_html s = unsafePerformIO $
  withCStringLen s $ \(ptr, len) ->
    peekCString $ c_cmark_markdown_to_html ptr len
