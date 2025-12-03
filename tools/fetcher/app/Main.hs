{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (displayException, try)
import Control.Monad (forM_, unless, when)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Network.HTTP.Simple
import Options.Applicative
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Text.HTML.TagSoup (renderTags)
import Text.HTML.TagSoup.Tree (TagTree (..), flattenTree, parseTree)

userAgent :: BS.ByteString
userAgent = "github.com/Sornaensis/aoc25 (fetch-day tool)"

data Args = Args
    { argDay :: Int
    , argYear :: Int
    , argOutDir :: FilePath
    , argSession :: Maybe String
    , argDoSetup :: Bool
    }

argsParser :: Parser Args
argsParser =
    Args
        <$> option auto (long "day" <> metavar "DAY" <> help "Puzzle day (1-25)" <> completeWith (map show ([1 .. 25] :: [Int])))
        <*> option auto (long "year" <> metavar "YEAR" <> help "Puzzle year")
        <*> strOption (long "out-dir" <> metavar "DIR" <> value "." <> showDefault <> help "Directory that receives problem.md/input.txt")
        <*> optional (strOption (long "session" <> metavar "TOKEN" <> help "Explicit AoC session token"))
        <*> switch (long "do-setup" <> help "Copy templates/day/* into --out-dir before writing files")

requireSession :: Args -> IO String
requireSession a = do
    let fromArg = argSession a
    envVar <- lookupEnv "AOC_SESSION"
    case fromArg <|> envVar of
        Just token | not (null token) -> pure token
        _ -> fail "AOC_SESSION environment variable (or --session) is required."

fetchText :: String -> String -> IO T.Text
fetchText url session = do
    initialReq <- parseRequest url
    let req =
            setRequestHeaders
                [ ("Cookie", BS.pack ("session=" <> session))
                , ("User-Agent", userAgent)
                ]
                initialReq
    result <- try (httpLBS req) :: IO (Either HttpException (Response LBS.ByteString))
    response <- either (fail . ("HTTP error: " <>) . displayException) pure result
    if getResponseStatusCode response /= 200
        then fail $ "Request failed with code " <> show (getResponseStatusCode response)
        else pure $ TE.decodeUtf8 $ LBS.toStrict $ getResponseBody response

extractArticles :: T.Text -> [T.Text]
extractArticles html =
        [ renderTags (flattenTree [branch])
        | branch@(TagBranch name _ _) <- walk (parseTree html)
        , T.toCaseFold name == "article"
        ]
    where
        walk :: [TagTree T.Text] -> [TagTree T.Text]
        walk = concatMap go

        go :: TagTree T.Text -> [TagTree T.Text]
        go leaf@(TagLeaf _) = [leaf]
        go branch@(TagBranch _ _ children) = branch : walk children

buildMarkdown :: Int -> Int -> String -> T.Text -> T.Text
buildMarkdown year day problemUrl html =
    documentHeader body
  where
    body =
        case extractArticles html of
            [] -> placeholder
            sections -> T.intercalate "\n\n<hr />\n\n" (map stripTrailing sections)

    documentHeader body =
        T.unlines
            [ "# Advent of Code " <> T.pack (show year) <> " - Day " <> padDay day
            , ""
            , "> Source: " <> T.pack problemUrl
            , ""
            , "<!-- Puzzle content copied from Advent of Code (HTML). -->"
            , ""
            , body
            ]
    padDay d = let s = show d in if d < 10 then T.pack ('0' : s) else T.pack s
    stripTrailing = T.dropWhileEnd (`elem` (' ' : "\n"))

    placeholder =
        let snippet = T.take 500 trimmedHtml <> if T.length trimmedHtml > 500 then "..." else ""
            trimmedHtml = T.strip html
         in T.unlines
                [ "_Could not find puzzle <article> tags. The puzzle might not be unlocked yet or the site layout changed._"
                , ""
                , "Fetched HTML snippet for debugging:" 
                , "```html"
                , snippet
                , "```"
                , "Re-run the fetcher after the puzzle unlocks to update this file."
                ]

writeFiles :: FilePath -> T.Text -> T.Text -> IO ()
writeFiles outDir markdown input = do
    putStrLn $ "Ensuring output directory exists: " <> outDir
    createDirectoryIfMissing True outDir
    putStrLn $ "Writing files into: " <> outDir
    TIO.writeFile (outDir </> "problem.md") markdown
    TIO.writeFile (outDir </> "input.txt") input

copyTemplateScaffold :: FilePath -> FilePath -> IO ()
copyTemplateScaffold templateRoot outDir = do
    exists <- doesDirectoryExist templateRoot
    unless exists $ fail ("Template directory does not exist: " <> templateRoot)
    putStrLn $ "Copying template contents from " <> templateRoot <> " into " <> outDir
    copyDir templateRoot outDir
  where
    copyDir src dst = do
        createDirectoryIfMissing True dst
        entries <- listDirectory src
        forM_ entries $ \entry -> do
            let srcPath = src </> entry
                dstPath = dst </> entry
            isDir <- doesDirectoryExist srcPath
            if isDir
                then copyDir srcPath dstPath
                else do
                    fileExists <- doesFileExist dstPath
                    when fileExists $ putStrLn ("Overwriting file: " <> dstPath)
                    copyFile srcPath dstPath

main :: IO ()
main = do
    args <- execParser (info (argsParser <**> helper) (fullDesc <> progDesc "Fetch AoC puzzle statement and input"))
    when (argDay args < 1 || argDay args > 25) $ fail "Day must be between 1 and 25"
    session <- requireSession args
    let day = argDay args
        year = argYear args
        outDir = argOutDir args
        baseUrl = "https://adventofcode.com/" <> show year <> "/day/" <> show day
    when (argDoSetup args) $ do
        let templateRoot = "templates" </> "day"
        copyTemplateScaffold templateRoot outDir
    putStrLn $ "Fetching puzzle page from: " <> baseUrl
    problemHtml <- fetchText baseUrl session
    let inputUrl = baseUrl <> "/input"
    putStrLn $ "Fetching puzzle input from: " <> inputUrl
    puzzleInput <- fetchText inputUrl session
    let markdown = buildMarkdown year day baseUrl problemHtml
    writeFiles outDir markdown puzzleInput
    putStrLn $ "Saved puzzle text to " <> (outDir </> "problem.md")
    putStrLn $ "Saved puzzle input to " <> (outDir </> "input.txt")
