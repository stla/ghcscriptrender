module ScriptToHtml where

import           CommonFunctions
import           Data.IORef
import           Data.List         (findIndices)
import qualified Data.String.Utils as SU
import           System.IO
import           System.Process    (readProcess)

-- format the hs file given as lines
formatHsfile :: [FilePath] -> [String]
formatHsfile hsfile = (SU.split "\n" ("\n" ++ (SU.strip $ SU.join "\n" hsfile))) ++ [""]

-- get the HsColour html from a script
getHsColourHtml :: FilePath -> IO([String])
getHsColourHtml hsfile =
  do
    hscolourhtml <- readProcess "HsColour" [hsfile, "-css"] ""
    let html = formatHsColourHtml hscolourhtml
    return ( SU.split "\n" html)

formatHsColourHtml :: String -> String
formatHsColourHtml html = SU.replace "<pre>" "<div class='sourceCode'><pre class='scriptHaskell'><code class='scriptHaskell'>"
                            $ SU.replace "</pre>" "</code></pre></div>\n"
                              $ SU.replace "<span class='hs-conop'>:</span><span class='hs-varid'>" "<span class='command'>:"
                                $ SU.replace "<span class='hs-conop'>:</span><span class='hs-layout'>{" "<span class='m'>:{"
                                  $ SU.replace "<span class='hs-conop'>:</span><span class='hs-layout'>}" "<span class='m'>:}"
                                    $ html

-- insert strings in html spans
toSpan :: String -> String
toSpan string = "<span class='output'>" ++ string ++ "</span>"

toSpans :: [String] -> String
toSpans strings | length(strings) == 1 = toSpan (strings !! 0)
                | otherwise = toSpan (strings !! 0) ++ "\n" ++ (toSpans $ drop 1 strings)

-- add a span for the prompt
promptInputs :: [String] -> Int -> Int -> [String]
promptInputs html i j = head ++ middle ++ end
                    where head = take i html
                          end =  drop (length html - j) html
                          middle = map (\x -> "<span class='prompt'>></span> " ++ x) $ (!! j).iterate init $ drop i html

-- get html for a module
moduleToHtml :: FilePath -> Bool -> IO(String)
moduleToHtml hsfile fragment =
  do
    hscolourhtml <- readProcess "HsColour" [hsfile, "-css"] ""
    let htmlInLines = SU.split "\n" hscolourhtml
    let html0 = if fragment then init.init $ drop 9 htmlInLines else htmlInLines
    let emptyLines = findIndices (=="") html0
    let html1 = insertAtIndices emptyLines " " $ html0
    let html2 = [x | x <- html1, not $ x == ""]
    let html = SU.replace "<pre>" "<div class='sourceCode'><pre class='scriptHaskell'><code class='scriptHaskell'>"
                  $ SU.replace (if fragment then "</pre></body>" else "</pre>") "</code></pre></div>"
                    $ SU.replace "<span class='hs-conop'>:</span><span class='hs-varid'>" "<span class='command'>:"
                      $ SU.join "\n"
                        $ html2
    return ( html )


-- ~~## Case multiline outputs ##~~ --

-- split the input file
writeTruncatedFiles :: FilePath -> IO( ([FilePath], [Int]) )
writeTruncatedFiles hsfile =
  do
    fileLines <- fmap lines (readFile hsfile)
    let formattedLines = formatHsfile fileLines
    tmphs <- temporaryFile "hsfile.hs"
    tmptxt <- temporaryFile "hsfile_"
    writeFormattedFile <- writeFile tmphs $ SU.join "\n" formattedLines
    let breaks = isOutput formattedLines
    let tmpfiles = map (\i -> tmptxt ++ (show i) ++ ".txt") [0..(length breaks - 1)]
    writefiles <- mapM_ (\i -> writeFile (tmpfiles !! i) (SU.join "\n" $ take (breaks !! i + 1) formattedLines)) [0..(length breaks - 1)]
    return (tmpfiles, breaks)

getListOutputs :: [FilePath] -> IO([[String]])
getListOutputs outputfiles =
  do
    filesLines <- getFiles outputfiles
    let f = \i -> if i==0 then filesLines !! 0 else drop ((length $ filesLines !! (i-1))-1) (filesLines !! i)
    return (map (init.f) [0..(length outputfiles -1)])

mainFun :: FilePath -> Maybe String -> IO ( ([[String]], [Int]) )
mainFun hsfile packages =
  do
    (tmpfiles, breaks) <- writeTruncatedFiles hsfile
    tmptxt <- temporaryFile "ghcOutput_"
    let outputfiles = map (\i -> tmptxt ++ (show i) ++ ".txt") [0..(length tmpfiles - 1)]
    i <- newIORef 0
    mainLoop i tmpfiles outputfiles packages
    listOutputs <- getListOutputs outputfiles
    return (listOutputs, breaks)

mainLoop :: IORef Int -> [FilePath] -> [FilePath] -> Maybe String -> IO ()
mainLoop i tmpfiles outputfiles packages =
  do
    let nfiles = length tmpfiles
    j <- readIORef i
    if j >= nfiles
      then return ()
      else
        do
          let hsfile = tmpfiles !! j
          let outfile = outputfiles !! j
          outhi <- openFile outfile WriteMode
          output <- getGHCIoutputs hsfile packages
          hPutStrLn outhi output
          hClose outhi
          modifyIORef i (+1)    -- increase IO Ref by 1
          mainLoop i tmpfiles outputfiles packages

scriptToHtmlWithOutputs :: FilePath -> Bool -> Maybe String -> IO()
scriptToHtmlWithOutputs hsfile fragment packages =
  do
    (outputs, breaks) <- mainFun hsfile packages
    let indices = if fragment then map (+1) breaks else map (+10) breaks
    let listOutputs = map toSpans outputs
    let (i,j) = if fragment then (1,1) else (10, 4)
    tmphs <- temporaryFile "hsfile.hs"
    html0 <- getHsColourHtml tmphs
    let html = promptInputs (if fragment then init.init.init $ drop 9 html0 else html0) i j
    let htmlWithInsertions = SU.replace "<code class='scriptHaskell'>\n" "<code class='scriptHaskell'>"
                               $ SU.join "\n"
                                 $ insertAtIndices2 indices listOutputs html
    let htmlOut = if fragment
                     then htmlWithInsertions
                     else SU.replace ("<title>" ++ tmphs) ("<title>" ++ basefilename) htmlWithInsertions
                          where basefilename = last $ SU.split "/" hsfile
    putStrLn htmlOut


-- ~~## Case monoline outputs ##~~ --

getOutputsAndBreaks :: FilePath -> Maybe String -> IO( [String], [Int] )
getOutputsAndBreaks hsfile packages =
  do
    fileLines <- fmap lines (readFile hsfile)
    let formattedLines = formatHsfile fileLines
    tmphs <- temporaryFile "hsfile.hs"
    writeFormattedFile <- writeFile tmphs $ SU.join "\n" formattedLines
    let breaks = isOutput formattedLines
    runGHC <- getGHCIoutputs tmphs packages
    let outputs = map toSpan $ SU.split "\n" runGHC
    return (outputs, breaks)

scriptToHtmlWithMonoOutputs :: FilePath -> Bool -> Maybe String -> IO()
scriptToHtmlWithMonoOutputs hsfile fragment packages =
  do
    (outputs, breaks) <- getOutputsAndBreaks hsfile packages
    let indices = if fragment then map (+1) breaks else map (+10) breaks
    let (i,j) = if fragment then (1,1) else (10, 4)
    tmphs <- temporaryFile "hsfile.hs"
    html0 <- getHsColourHtml tmphs
    let html = promptInputs (if fragment then init.init.init $ drop 9 html0 else html0) i j
    let htmlWithInsertions = SU.replace "<code class='scriptHaskell'>\n" "<code class='scriptHaskell'>"
                               $ SU.join "\n"
                                 $ insertAtIndices2 indices outputs html

    case fragment of
         True -> putStrLn htmlWithInsertions
         False -> putStrLn $ SU.replace ("<title>" ++ tmphs) ("<title>" ++ basefilename) htmlWithInsertions
                    where basefilename = last $ SU.split "/" hsfile


-- ##~~ Case placeholders ~~## --

getBreaks :: FilePath -> IO([Int])
getBreaks hsfile =
  do
    fileLines <- fmap lines (readFile hsfile)
    let formattedLines = formatHsfile fileLines
    tmphs <- temporaryFile "hsfile.hs"
    writeFormattedFile <- writeFile tmphs $ SU.join "\n" formattedLines
    let breaks = isOutput formattedLines
    return breaks

scriptToHtmlNoOutputs :: FilePath -> Bool -> IO()
scriptToHtmlNoOutputs hsfile fragment =
  do
    breaks <- getBreaks hsfile
    let indices = if fragment then map (+1) breaks else map (+10) breaks
    let (i,j) = if fragment then (1,1) else (10, 4)
    tmphs <- temporaryFile "hsfile.hs"
    html0 <- getHsColourHtml tmphs
    let html = promptInputs (if fragment then init.init.init $ drop 9 html0 else html0) i j
    let htmlWithInsertions = SU.replace "<code class='scriptHaskell'>\n" "<code class='scriptHaskell'>"
                               $ SU.join "\n"
                                 $ insertAtIndices indices "<span class='output'>OUTPUT GOES HERE</span>" html
    let htmlOut = if fragment
                     then htmlWithInsertions
                     else SU.replace ("<title>" ++ tmphs) ("<title>" ++ basefilename) htmlWithInsertions
                          where basefilename = last $ SU.split "/" hsfile
    putStrLn htmlOut
