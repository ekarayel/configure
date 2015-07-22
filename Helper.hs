module Configure.Helper where

import Data.List
import System.IO
import System.Process
import System.Exit

-- | Returns cabal file fields for the MIT Lincense (consistent with the location from GitHub.com)
mitLicense :: [String]
mitLicense =
    [ "license: MIT"
    , "license-file: LICENSE"
    ]

-- | Returns cabal file fields for author and maintainer
authorAndMaintainer :: String -> [String]
authorAndMaintainer author =
    [ "author: " ++ author
    , "maintainer: " ++ author
    ]

-- | Returns cabal package description entries for source-repository, version, homepage, description
-- and synopsis entries using the README.md file and git tags.
gitHubProject :: String -> IO [String]
gitHubProject packageUrl =
    do gitTag <- getGitTag
       synopsisSection <- getMarkdownSection "README.md" "Synopsis" id "synopsis"
       descriptionSection <- getMarkdownSection "README.md" "Description" escape "description"
       return $
           [ "version: " ++ gitTag
           , "homepage: " ++ packageUrl
           , "bug-reports: " ++ packageUrl ++ "/issues"
           , "package-url: " ++ packageUrl
           ] ++ synopsisSection
           ++ descriptionSection
           ++ [ "source-repository head"
           , "    type: git"
           , "    location: " ++ packageUrl ++ " --recursive"
           , "source-repository this"
           , "    type: git"
           , "    tag: " ++ gitTag
           , "    location: " ++ packageUrl ++ " --recursive"
           ]

getGitTag =
    do (Nothing, Just hout, Nothing, ph) <-
           createProcess (shell "git describe --always") { std_out = CreatePipe }
       res <- hGetLine hout
       ExitSuccess <- waitForProcess ph
       return res

_TAB_SIZE_ :: Int
_TAB_SIZE_ = 4

indent :: [String] -> [String]
indent = map ((replicate _TAB_SIZE_ ' ') ++)

-- | Convert markdown sections to package descriptions
escape :: [String] -> [String]
escape = map go
    where
      go "" = "."
      go "```" = "@"
      go "```haskell" = "@"
      go line = concat $ (map (const "&#32;") spaces) ++ (map go' rest)
          where (spaces, rest) = span (==' ') line
      go' '{' = "&#123;"
      go' '}' = "&#125;"
      go' '#' = "&#35;"
      go' s = [s]

-- | Extract source code examples from Markdown files
toHaskell :: [String] -> [String]
toHaskell ls = go False ls
    where
      go _ [] = []
      go visible (l:ls)
          | l == "```" = go False ls
          | l == "```haskell" = go True ls
          | visible = (if visible then (l:) else id) (go visible ls)

getMarkdownSection :: FilePath -> String -> ([String] -> [String]) -> String -> IO [String]
getMarkdownSection filePath sectionName transform fieldName =
    do contents <- readFile filePath
       return $
           case dropWhile (/= ("## " ++ sectionName)) $ lines contents of
             [] -> []
             (sectionHead:sectionRest) ->
                 ((fieldName ++ ":"):)
                 $ indent
                 $ transform
                 $ fst
                 $ break ("#" `isPrefixOf`) sectionRest
