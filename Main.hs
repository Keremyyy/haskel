import Data.Char (isSpace)

main :: IO ()
main = do
    putStrLn "Geef een bestand"
    name <- getLine
    contents <- readFile name
    let outputName = repleceHTML ".html" ".md" name
    writeFile outputName (removeTussenLijn (toLeft (tags (htmlToMarkdown contents))))

toLeft :: String -> String
toLeft = unlines . map (dropWhile myIsSpace) . lines
myIsSpace :: Char -> Bool
myIsSpace c = c == ' ' || c == '\t' || c == '\n'


tags :: String -> String
tags = go
  where
    go [] = []
    go ('<':xs) = go (drop 1 $ dropWhile (/= '>') xs) -- in plaats van $ had ik er ook haakjes neer kunnen zetten
    go (x:xs) = x : go xs

removeTussenLijn :: String -> String
removeTussenLijn = unlines . filter (not . all isSpace) . lines

htmlToMarkdown :: String -> String
htmlToMarkdown = repleceHTML "</li>" "\n"
              . repleceHTML "</ol>" "\n"
              . repleceHTML "<li>" "- "
              . repleceHTML "<ol>" "\n"
              . repleceHTML "</ul>" "\n"
              . repleceHTML "<ul>" "\n"
              . repleceHTML "<br/>" "\n"
              . repleceHTML "<br>" "\n"
              . repleceHTML "</p>" "\n\n"
              . repleceHTML "<p>" ""
              . repleceHTML "</h3>" "\n"
              . repleceHTML "<h3>" "### "
              . repleceHTML "</h2>" "\n"
              . repleceHTML "<h2>" "## "
              . repleceHTML "</h1>" "\n"
              . repleceHTML "<h1>" "# "
              . repleceHTML "</em>" "_"
              . repleceHTML "<em>" "_"
              . repleceHTML "</i>" "_"
              . repleceHTML "<i>" "_"
              . repleceHTML "</strong>" "**"
              . repleceHTML "<strong>" "**"
              . repleceHTML "</b>" "**"
              . repleceHTML "<b>" "**"

repleceHTML :: String -> String -> String -> String
repleceHTML old new str = go str
  where
    go [] = []
    go xs@(x:rest)
      | old `isPrefixOf` xs = new ++ go (drop (length old) xs)
      | otherwise = x : go rest

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys




