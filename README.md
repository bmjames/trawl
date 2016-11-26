# trawl

`trawl` is a wrapper around `ghc-pkg` which makes it easier to locate documentation for installed packages.

Given the name of a package or module, `trawl` simply prints the location of the corresponding Haddock HTML on your filesystem.

## Installation

    $ cabal install trawl

## Usage

    $ trawl --help
    Usage: trawl ((-p|--package PACKAGE) | (-m|--module MODULE) |
                 (-v|--member MODULE.MEMBER)) [--stack]

    Available options:
      -h,--help                Show this help text
      -p,--package PACKAGE     Find the haddock index for PACKAGE
      -m,--module MODULE       Find the haddock page for MODULE
      -v,--member MODULE.MEMBER
                               Find the haddock page section for MEMBER of MODULE
      --stack                  Use stack environment

`trawl` is intended to be used in conjunction with your favourite program for viewing HTML documents. For example,

    $ chromium-browser $(trawl -p async)

### Stack support

`trawl` supports Stack projects with the `--stack` option. The same effect can be achieved by running `trawl` via `stack exec`. (Under the hood, `trawl` with the `--stack` option is just invoking `ghc-pkg` via `stack exec`.)

### Cabal sandboxes

`trawl` has no explicit support for Cabal sandboxes. To find haddocks in a sandbox, run using `cabal exec`, for example:

    $ cabal exec trawl -- -m Control.Concurrent.Async
