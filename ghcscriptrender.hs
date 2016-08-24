module Main where

import Options.Applicative
import Data.Monoid
import ScriptToHtml (scriptToHtmlWithOutputs, scriptToHtmlWithMonoOutputs, scriptToHtmlNoOutputs)
import ScriptToTxt (scriptToTxtWithOutputs, scriptToTxtWithMonoOutputs)


data Arguments = Arguments
  { infile :: String
  , singlelines :: Bool
  , nooutput :: Bool
  , filetype :: Maybe String
  , fragment :: Bool
  , packages :: Maybe String }

renderScript :: Arguments -> IO()
renderScript (Arguments infile False False Nothing fragment packages) = do scriptToHtmlWithOutputs infile fragment packages
renderScript (Arguments infile True _ Nothing fragment packages) = do scriptToHtmlWithMonoOutputs infile fragment packages
renderScript (Arguments infile False True Nothing fragment _) = do scriptToHtmlNoOutputs infile fragment
renderScript (Arguments infile False False (Just "html") fragment packages) = do scriptToHtmlWithOutputs infile fragment packages
renderScript (Arguments infile True _ (Just "html") fragment packages) = do scriptToHtmlWithMonoOutputs infile fragment packages
renderScript (Arguments infile False True (Just "html") fragment _) = do scriptToHtmlNoOutputs infile fragment
renderScript (Arguments infile False _ (Just "txt") _ packages) = do scriptToTxtWithOutputs infile False packages
renderScript (Arguments infile True _ (Just "txt") _ packages) = do scriptToTxtWithMonoOutputs infile False packages
renderScript (Arguments infile False _ (Just "md") _ packages) = do scriptToTxtWithOutputs infile True packages
renderScript (Arguments infile True _ (Just "md") _ packages) = do scriptToTxtWithMonoOutputs infile True packages

run :: Parser Arguments
run = Arguments
     <$> argument str 
          ( metavar "FILE"
         <> help "The Haskell script" )
     <*> switch
          ( long "singleoutputs"
         <> short 's'
         <> help "In case every output takes only one line" )
     <*> switch
          ( long "nooutputs"
         <> short 'n'
         <> help "Don't include the outputs" )
     <*> ( optional $ strOption 
          ( metavar "TYPE"
         <> long "type"
         <> short 't'
         <> help "Output type: html (default), txt, or md" ))
     <*> switch
          ( long "fragment"
         <> short 'f'
         <> help "Generate a html block (for --type html)" )
     <*> ( optional $ strOption 
          ( metavar "package-database"
         <> long "package-db"
         <> short 'p'
         <> help "Add the package database" ))

main :: IO ()
main = execParser opts >>= renderScript
  where
    opts = info (helper <*> run)
      ( fullDesc
     <> progDesc "Convert a Haskell script to html or txt, including the outputs."
     <> header "ghcscriptrender" )

