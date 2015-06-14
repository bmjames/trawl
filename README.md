# trawl

`trawl` is a wrapper around `ghc-pkg` which makes it easier to locate documentation for installed packages.

Given the name of a package or module, `trawl` simply prints the location of the corresponding Haddock HTML on your filesystem.

## Usage

    $ trawl --help
    Usage: trawl ((-p|--package PACKAGE) | (-m|--module MODULE))

    Available options:
      -h,--help                Show this help text
      -p,--package PACKAGE     Find the haddock index for PACKAGE
      -m,--module MODULE       Find the haddock page for MODULE

### A note about Cabal sandboxes

`trawl` has no explicit support for Cabal sandboxes. To find haddocks in a sandbox, run using `cabal exec`, for example:

    $ cabal exec trawl -- -m Data.List
