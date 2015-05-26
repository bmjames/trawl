# trawl

`trawl` is a wrapper around `ghc-pkg` which makes it easier to locate documentation for modules and packages.

## Usage

    $ trawl --help
    Usage: trawl ((-p|--package PACKAGE) | (-m|--module MODULE))

    Available options:
      -h,--help                Show this help text
      -p,--package PACKAGE     Find the haddock index for PACKAGE
      -m,--module MODULE       Find the haddock page for MODULE
