import qualified Text.Pandoc as Pandoc
import qualified Cheapskate as Cheapskate
import qualified Cheapskate.Html as CheapskateHtml
import qualified CMark as CMark
import qualified Text.Sundown.Html.Text as Sundown
import qualified Text.Discount as Discount
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import qualified Text.Markdown as Markdown
import Data.Text (Text)
import Data.Text as T
import Data.Text.Lazy (fromChunks, toChunks)
import Data.Text.IO as T
import Criterion.Main
import Criterion.Config
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  (conf,_) <- parseArgs defaultConfig defaultOptions args
  sample <- T.readFile "bench/sample.md"
  defaultMainWith conf (return ()) [
      mkBench "cmark" (CMark.commonmarkToHtml []) sample
    , mkBench "cheapskate" (T.concat . toChunks . Blaze.renderHtml . CheapskateHtml.renderDoc . Cheapskate.markdown Cheapskate.def) sample
    , mkBench "discount" (Discount.parseMarkdownUtf8 []) sample
    , mkBench "pandoc" (T.concat . toChunks . Blaze.renderHtml . Pandoc.writeHtml Pandoc.def . Pandoc.readMarkdown Pandoc.def . T.unpack) sample
    , mkBench "markdown" (T.concat . toChunks . Blaze.renderHtml . Markdown.markdown Markdown.def . fromChunks . (:[])) sample
    ]

-- Note: when full-sample.md rather than sample.md is used, pandoc and markdown
-- hang (> 1 minute).

-- even with sample.md, sundown gives this error
-- , mkBench "sundown" (Sundown.renderHtml Sundown.noExtensions Sundown.noHtmlModes False Nothing) sample
-- bench-cmark(50437,0x7fff7bfbe310) malloc: *** error for object 0x7ffde3d00928: incorrect checksum for freed object - object was probably modified after being freed.
--  *** set a breakpoint in malloc_error_break to debug

mkBench :: String -> (Text -> Text) -> Text -> Benchmark
mkBench name converter inp =
  bench name $ nf converter inp
