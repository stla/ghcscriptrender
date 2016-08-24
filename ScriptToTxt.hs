module ScriptToTxt where

import System.IO
import qualified Data.String.Utils as SU
import Data.IORef
import CommonFunctions


-- format the hs file given as lines
formatHsfile :: [FilePath] -> [String]
formatHsfile hsfile = (SU.split "\n" ((SU.strip $ SU.join "\n" hsfile))) ++ [""]

-- add the prompt 
promptInputs :: [String] -> [String]
promptInputs txt = map (\x -> "> " ++ x) txt

-- ~~## Case multiline outputs ##~~ -- 

-- split the input file
writeTruncatedFiles :: FilePath -> IO( ([FilePath], [Int]) )
writeTruncatedFiles hsfile =
  do
    fileLines <- fmap lines (readFile hsfile)
    let formattedLines = formatHsfile fileLines
    writeFormattedFile <- writeFile "/tmp/hsfile.hs" $ SU.join "\n" formattedLines
    let breaks = isOutput formattedLines
    let tmpfiles = map (\i -> "/tmp/hsfile_" ++ (show i) ++ ".txt") [0..(length breaks - 1)]
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
    let outputfiles = map (\i -> "/tmp/ghcOutput_" ++ (show i) ++ ".txt") [0..(length tmpfiles - 1)]
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
          modifyIORef i (+1)    -- increase it by 1
          mainLoop i tmpfiles outputfiles packages

scriptToTxtWithOutputs :: FilePath -> Bool -> Maybe String -> IO()
scriptToTxtWithOutputs hsfile markdown packages = 
  do
    (outputs, breaks) <- mainFun hsfile packages
    let indices = map (+1) $ breaks
    let listOutputs = map (\x -> SU.join "\n" x) outputs
    txt <- fmap lines (readFile "/tmp/hsfile.hs")
    let basefilename = last $ SU.split "/" hsfile
    let txtWithInsertions = SU.join "\n" 
                              $ insertAtIndices2 indices listOutputs 
                                $ promptInputs txt 
    case markdown of
         False -> putStrLn txtWithInsertions
         True -> putStrLn $ "```haskell\n" ++ txtWithInsertions ++ "\n```"


-- ~~## Case monoline outputs ##~~ -- 

getOutputsAndBreaks :: FilePath -> Maybe String -> IO( [String], [Int] )
getOutputsAndBreaks hsfile packages = 
  do
    fileLines <- fmap lines (readFile hsfile)
    let formattedLines = formatHsfile fileLines
    writeFormattedFile <- writeFile "/tmp/hsfile.hs" $ SU.join "\n" formattedLines
    let breaks = isOutput formattedLines
    runGHC <- getGHCIoutputs hsfile packages
    let outputs = SU.split "\n" runGHC
    return (outputs, breaks)

scriptToTxtWithMonoOutputs :: FilePath -> Bool -> Maybe String -> IO()
scriptToTxtWithMonoOutputs hsfile markdown packages = 
  do
    (outputs, breaks) <- getOutputsAndBreaks hsfile packages
    let indices = map (+1) $ breaks
    txt <- fmap lines (readFile "/tmp/hsfile.hs")
    let basefilename = last $ SU.split "/" hsfile
    let txtWithInsertions = SU.join "\n" 
                               $ insertAtIndices2 indices outputs 
                                 $ promptInputs txt 
    case markdown of
         False -> putStrLn txtWithInsertions
         True -> putStrLn $ "```haskell\n" ++ txtWithInsertions ++ "\n```"
