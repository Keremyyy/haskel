
main :: IO ()
main = do
    putStrLn "Geef een bestand"
    name <- getLine
    contents <- readFile name
    let outputName = replaceHTML ".html" ".md" name
    writeFile outputName (removeTussenLijn (toLeft (tags (htmlToMarkdown contents))))

toLeft :: String -> String
toLeft = unlines . map (dropWhile geenSpatie) . lines
geenSpatie :: Char -> Bool
geenSpatie c = c == ' ' || c == '\n'


tags :: String -> String
tags = go
  where
    go [] = []
    go ('<':xs) = go (drop 1 $ dropWhile (/= '>') xs) -- in plaats van $ had ik er ook haakjes neer kunnen zetten
    go (x:xs) = x : go xs

removeTussenLijn :: String -> String
removeTussenLijn = unlines . filter (not . all geenSpatie) . lines

htmlToMarkdown :: String -> String
htmlToMarkdown =
      replaceHTML "<h1>" "# "
    . replaceHTML "</h1>" "\n\n"
    . replaceHTML "<h2>" "## "
    . replaceHTML "</h2>" "\n\n"
    . replaceHTML "<h3>" "### "
    . replaceHTML "</h3>" "\n\n"
    . replaceHTML "<h4>" "#### "
    . replaceHTML "</h4>" "\n\n"
    . replaceHTML "<h5>" "##### "
    . replaceHTML "</h5>" "\n\n"
    . replaceHTML "<h6>" "###### "
    . replaceHTML "</h6>" "\n\n"

    -- Tekststijl
    . replaceHTML "<b>" "**"
    . replaceHTML "</b>" "**"
    . replaceHTML "<strong>" "**"
    . replaceHTML "</strong>" "**"
    . replaceHTML "<i>" "_"
    . replaceHTML "</i>" "_"
    . replaceHTML "<em>" "_"
    . replaceHTML "</em>" "_"
    . replaceHTML "<code>" "`"
    . replaceHTML "</code>" "`"
    . replaceHTML "<pre>" "```\n"
    . replaceHTML "</pre>" "\n```"

    -- Lijsten
    . replaceHTML "<ul>" "\n"
    . replaceHTML "</ul>" "\n"
    . replaceHTML "<ol>" "\n"
    . replaceHTML "</ol>" "\n"
    . replaceHTML "<li>" "- "
    . replaceHTML "</li>" "\n"

    -- Links & afbeeldingen
    . replaceHTML "<a href=\"" "["
    . replaceHTML "\">" "]("
    . replaceHTML "</a>" ")"
    . replaceHTML "<img src=\"" "!["
    . replaceHTML "\" alt=\"" "]("
    . replaceHTML "\">" ")"

    -- Regels en paragrafen
    . replaceHTML "<br>" "  \n"
    . replaceHTML "<br/>" "  \n"
    . replaceHTML "<p>" "\n\n"
    . replaceHTML "</p>" "\n\n"
    . replaceHTML "<hr>" "\n---\n"
    . replaceHTML "<hr/>" "\n---\n"


replaceHTML :: String -> String -> String -> String
replaceHTML old new str = go str
  where
    go [] = []
    go xs@(x:rest)
      | old `isPrefixVan` xs = new ++ go (drop (length old) xs)
      | otherwise = x : go rest

isPrefixVan :: Eq a => [a] -> [a] -> Bool
isPrefixVan [] _ = True
isPrefixVan _ [] = False
isPrefixVan (x:xs) (y:ys) = x == y && isPrefixVan xs ys




