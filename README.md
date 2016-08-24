# ghcscriptrender
Render a Haskell script, including the outputs.

For `html`rendering, this tool depends on the executable file `HsColour` provided by the [hscolour package](https://hackage.haskell.org/package/hscolour).

## Usage 

```bash
$ ghcscriptrender --help
ghcscriptrender

Usage: ghcscriptrender FILE [-s|--singleoutputs] [-n|--nooutputs]
                       [-t|--type TYPE] [-f|--fragment]
                       [-p|--package-db package-database]
  Convert a Haskell script to html or txt, including the outputs.

Available options:
  -h,--help                Show this help text
  FILE                     The Haskell script
  -s,--singleoutputs       In case every output takes only one line
  -n,--nooutputs           Don't include the outputs
  -t,--type TYPE           Output type: html (default), txt, or md
  -f,--fragment            Generate a html block (for --type html)
  -p,--package-db package-database
                           Add the package database
```

#### Options:

- `-t`: output type: `html`, `txt` or `md`.

- `-f`: for `--type html` only; returns a html block, without header and body tags.

- `-n`: for `--type html` only; don't include the outputs, but only placeholders.

- `-s`: use this option in case every output of the script fits on one line, to gain speed.

- `-p`: the package database, the same as the option `-package-db` of `ghc`.


## Compilation

- The compilation requires the libraries `optparse-applicative` and `MissingH`.

- As it is given, it only works on Linux. For Windows, change the occurences of `"/tmp/..."` to another temporary directory.

## Rendering of the html examples 

- [Example 1](http://htmlpreview.github.io/?https://raw.githubusercontent.com/stla/ghcscriptrender/master/tests/test_monoline_nooutput.html) (no outputs)

- [Example 2](http://htmlpreview.github.io/?https://raw.githubusercontent.com/stla/ghcscriptrender/master/tests/test_monoline.html) (single lines of outputs)

- [Example 3](http://htmlpreview.github.io/?https://raw.githubusercontent.com/stla/ghcscriptrender/master/tests/test_multiline.html) (multiple lines of outputs)


## The `md` (markdown) option

The `-t md` option returns a block enclosed between `` ```haskell `` and `` ``` ``, such as:

    ```haskell
    > -- define a new type: 
    > data Point = Point Float Float deriving (Show)
    > -- variable of type Point:
    > let point = (Point 3 4)
    > point
    Point 3.0 4.0
    > :t point
    point :: Point
    > -- 'Point' is like a function:
    > :t Point
    Point :: Float -> Float -> Point
    > -- a function acting on 'Point' variables:
    > :{
    > let squareNorm :: Point -> Float
    >     squareNorm (Point x y) = x^2+y^2
    > :}
    > squareNorm point
    25.0
    ```

This markdown block of code is ready for conversion to highlighted html by using either

- `node.js` with the [pygments library](https://github.com/pksunkara/pygments.js),

- the Python command line [pygmentize](http://pygments.org/docs/cmdline/),

- or [Pandoc](http://pandoc.org/demos.html). 

This is [the output](http://htmlpreview.github.io/?https://raw.githubusercontent.com/stla/ghcscriptrender/master/tests/test_monoline_md_pandoc.html) of Pandoc using the option `--highlight-style haddock`. 