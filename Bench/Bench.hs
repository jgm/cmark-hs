import CMark
import Data.Text.IO as T

main = T.readFile "README.md" >>= T.putStrLn . (markdownToHtml [])
