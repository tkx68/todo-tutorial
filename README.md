Here I follow the tutorial at https://typeable.io/blog/2021-03-15-reflex-1. The accompanying GitHub repo can be found at https://github.com/typeable/blog-posts-ru.git.

# Setup Development Environment

First I set up a development environment using the instructions at https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.rst.

* Here I have to choose cabal version 2.4 instead of the latest 3.* version.
* Do not fix the version of base (which determines the compiler version) but leave it blank and let the build system make the choice.

After performing the steps https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.rst and https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.rst I can indeed build the two executables:

    nix-build -o backend-result -A ghc.backend
    nix-build -o frontend-result -A ghcjs.frontend

After creating the cabal*.project files I can build with cabal in a nix shell:

    $ nix-shell -A shells.ghc
    [nix-shell:~/path]$ cabal build all

resp. with GHCJS:

    $ nix-shell -A shells.ghcjs
    [nix-shell:~/path]$ cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs build all

And it also works without explicitly entering the nix shell:

    $ nix-shell -A shells.ghc --run "cabal build all"
    $ nix-shell -A shells.ghcjs --run "cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs build all"

Building a local executable frontend:

> JSaddle is a set of libraries that allows Reflex-DOM to swap out its JavaScript backend easily. By default, Reflex-DOM’s mainWidget will work on GHC out of the box, using the jsaddle-webkit2gtk backend. So simply building your frontend package using GHC will produce a working native program that renders DOM using WebKit. This is recommended for native desktop releases.

    $ nix-build -o ghc-frontend-result -A ghc.frontend

Or with cabal:

    $ nix-shell -A shells.ghc --run "cabal new-build frontend"

I skip building mobile apps (https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.rst#building-mobile-apps) and using the jsaddle-warp package here.

> jsaddle-warp is an alternative JSaddle backend that uses a local warp server and WebSockets to control a browser from a native Haskell process. This is recommended to allow testing different browsers, and to make use of a browser’s significantly better developer tools.
