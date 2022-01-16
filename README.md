Here I follow the tutorial at https://typeable.io/blog/2021-03-15-reflex-1. The accompanying GitHub repo can be found at https://github.com/typeable/blog-posts-ru.git.

# Setup Development Environment

First I set up a development environment using the instructions at https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.rst.

- Here I have to choose cabal version 2.4 instead of the latest 3.\* version.
- Do not fix the version of base (which determines the compiler version) but leave it blank and let the build system make the choice.

After performing the steps https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.rst and https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.rst I can indeed build the two executables:

    nix-build -o backend-result -A ghc.backend
    nix-build -o frontend-result -A ghcjs.frontend

After creating the cabal\*.project files I can build with cabal in a nix shell:

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

Since we followed the reflex-platform instructions for setting up the development environment the default.nix file looks somewhat different. We have reflex-platform as a submodule and therefore we have

    import ./reflex-platform

in our default.nix. Instead the tutorial chooses to include from GitHub like so:

    reflex-platform ? ((import <nixpkgs> {}).fetchFromGitHub {
        owner = "reflex-frp";
        repo = "reflex-platform";
        rev = "efc6d923c633207d18bd4d8cae3e20110a377864";
        sha256 = "121rmnkx8nwiy96ipfyyv6vrgysv0zpr2br46y70zf4d0y1h1lz5";
        })

This probably means that I use the latest master branch of the reflex-platform whereas the tutorial uses a fixed version. This might give me some problems in the next steps. But anyway...

Ups, forgot to enable `useWarp = true;`! Can we still build everything?

    nix-build -o backend-result -A ghc.backend
    nix-build -o frontend-result -A ghcjs.frontend

Yes!

Now I go back to the tutorial. There it is suggested to use ghcid. So we change the frontend slightly and compile with GHC in the nix shell:

    $ nix-shell . -A shells.ghc
    [nix-shell:~/devel/try-reflex/todo-tutorial]$ ghcid --command 'cabal repl frontend' --test 'Main.main'

And indeed we now have a server at http://localhost:3003/ that shows "Hello, reflex!". If I change the text in the frontend Main.hs file the code is reloaded instantly and the page changes after reloading. That's a nice and quick development cycle.

> The port number is searched for in the JSADDLE_WARP_PORT environment variable. If this variable is not set, value 3003 is used by default.

Note that the backend doesn't do anything up to now. Its just the frontend running on its own warp server.

# How to get Haskell Language Server working

Simply install HLS with ghcup in the main Linux OS. The start nix shell with

    $ nix-shell . -A shells.ghc

and within nix shell start VS Code:

    [nix-shell:~/devel/try-reflex/todo-tutorial]$ code .

Now set your normal wrapper as the HLS executable in VS Code options. You can find this with e.g.

    [nix-shell:~/devel/try-reflex/todo-tutorial]$ which haskell-language-server-wrapper
    /home/torsten/.local/bin/haskell-language-server-wrapper

In this case search for "haskell server" in the options and set

    ${HOME}/.local/bin/haskell-language-server-wrapper

there. No restart VS Code.

# Compiler and language extensions

We use the compiler version GHC 8.6.5 with the current setup. This limits our useful language extensions to the following list:

    * BangPatterns
    * BinaryLiterals
    * BlockArguments
    * ConstrainedClassMethods
    * ConstraintKinds
    * DataKinds
    * DeriveDataTypeable
    * DeriveFoldable
    * DeriveFunctor
    * DeriveGeneric
    * DeriveLift
    * DeriveTraversable
    * DerivingStrategies
    * DoAndIfThenElse
    * EmptyCase
    * EmptyDataDecls
    * EmptyDataDeriving
    * ExistentialQuantification
    * ExplicitForAll
    * FlexibleContexts
    * FlexibleInstances
    * FunctionalDependencies
    * GADTs
    * GADTSyntax
    * GeneralisedNewtypeDeriving
    * HexFloatLiterals
    * InstanceSigs
    * KindSignatures
    * LambdaCase
    * MonoLocalBinds
    * MonomorphismRestriction
    * MultiParamTypeClasses
    * MultiWayIf
    * NamedFieldPuns
    * NamedWildCards
    * NoImplicitPrelude
    * NumericUnderscores
    * OverloadedStrings
    * PatternGuards
    * PolyKinds
    * PostfixOperators
    * QuasiQuotes
    * RankNTypes
    * RecordWildCards
    * RecursiveDo
    * ScopedTypeVariables
    * StandaloneDeriving
    * TemplateHaskell
    * TupleSections
    * TypeApplications
    * TypeFamilies
    * TypeOperators
    * TypeSynonymInstances
    * UnicodeSyntax
    * ViewPatterns

Further we add the following compiler options in order to get stricter errors and more warnings:

    * -Wall
    * -Wcompat
    * -optP-Wno-nonportable-include-path
    * -fdiagnostics-color=always
    * -Werror=missing-methods
    * -Werror=unused-top-binds
    * -Werror=unused-pattern-binds
    * -Werror=incomplete-patterns
    * -Werror=overlapping-patterns
    * -Werror=missing-methods
    * -Werror=unrecognised-pragmas
    * -Werror=duplicate-exports
    * -Werror=missing-fields
    * -Werror=wrong-do-bind
    * -Werror=simplifiable-class-constraints
    * -Werror=deferred-type-errors
    * -Werror=tabs
    * -Werror=inaccessible-code
    * -Werror=star-binder
    * -fwarn-tabs
    * -Wno-orphans
    * -Wunused-packages
    * -Wincomplete-uni-patterns

# Simplest TODO Application

I use the Prelude replacement Relude in order not to make things too easy. Now add the frontend elements 
from the tutorial. Things work except that I had to fix the optimised version of the `todoWidget` function.

Due to BlockArguments language extension I can drop the $ signs in front of the `do`s now. Furthermore I 
add Bootstrap 5 support and my favorite icon.

# Using EventWriter

Again there is a slight mistage in `todoWidget`. I have to add a `void $` here.

# Using ghcjs-dom

It works!

# Using JS FFI

Now I have to build with

   $ nix-build . -A ghcjs.frontend -o frontend-bin

This gives me a new folder `frontend` with a rich structure underneath. Amongst others there is an 
index.html and frontend.jsexe. Opening the index.html shows the application.

The correct versions in the 'default.nix' file seem to be important. The package `ghcjs-dom-contrib` 
can otherwise not be built. Therefore I drop the approach to git clone the imported packages.

The first compilation takes an abysmal amount of time. I am curious whether this speeds up the second time. Okay, following build times are much shorter.

# Let's better understand the  dependencies

The nix function that loads reflex-platform is [fetchFromGitHub](https://nixos.org/manual/nixpkgs/stable/#fetchfromgithub). And documentation says:

> fetchFromGitHub expects four arguments. owner is a string corresponding to the GitHub user or organization that controls this repository. repo corresponds to the name of the software repository. These are located at the top of every GitHub HTML page as owner/repo. rev corresponds to the Git commit hash or tag (e.g v1.0) that will be downloaded from Git. Finally, sha256 corresponds to the hash of the extracted directory. Again, other hash algorithms are also available but sha256 is currently preferred.

> fetchFromGitHub uses fetchzip to download the source archive generated by GitHub for the specified revision. If leaveDotGit, deepClone or fetchSubmodules are set to true, fetchFromGitHub will use fetchgit instead. Refer to its section for documentation of these options.

So this fetches the zip file for the required revision from GitHub and checks the sha256 for this zip file. Given some zip file a.zip we can find the sha256 with

    $ shasum -a 256 a.zip

but we don't have this file. There is an [issue](https://github.com/NixOS/nix/issues/1880) in the nix repo that solves this problem.

    $ nix-prefetch-url --unpack https://github.com/reflex-frp/reflex-platform/archive/efc6d923c633207d18bd4d8cae3e20110a377864.zip

    path is '/nix/store/y162njkhx15nr8z0c5g8yn58rm7bswqx-efc6d923c633207d18bd4d8cae3e20110a377864.zip' 121rmnkx8nwiy96ipfyyv6vrgysv0zpr2br46y70zf4d0y1h1lz5

The man page for nix-prefetch-url says:

> The  command nix-prefetch-url downloads the file referenced by the URL url, prints its cryptographic hash, and copies it into the Nix store.  The file name in the store is hash-baseName, where baseName is everything following the final slash in url.

> This command is just a convenience for Nix expression writers. Often a Nix expression fetches some source distribution from the network using the fetchurl ex‐ pression  contained  in  Nixpkgs.  However, fetchurl requires a cryptographic hash. If you don’t know the hash, you would have to download the file first, and then fetchurl would download it again when you build your Nix expression. Since fetchurl uses the same name for the downloaded file as  nix-prefetch-url,  the redundant download can be avoided.

> If  hash  is  specified,  then  a download is not performed if the Nix store already contains a file with the same hash and base name.  Otherwise, the file is downloaded, and an error is signaled if the actual hash of the file does not match the specified hash.

> This command prints the hash on standard output. Additionally, if the option --print-path is used, the path of the downloaded file in the Nix  store  is  also printed.

So this command prints the sha256 after downloading the zip file. And indeed `121rmnkx8nwiy96ipfyyv6vrgysv0zpr2br46y70zf4d0y1h1lz5` is what typeable has it their 'default.nix' file.

The revision hash of reflex-platform conforms to release 0.6.2 as can be seen with `git show`:

    $ git show efc6d923c633207d18bd4d8cae3e20110a377864 --stat

    commit efc6d923c633207d18bd4d8cae3e20110a377864 (tag: v0.6.2.0, origin/release/0.6.2.0)
    Author: Ali Abrar <aliabrar@gmail.com>
    Date:   Tue Dec 1 15:57:36 2020 -0500

        Update changelog

    ChangeLog.md | 2 +-
    1 file changed, 1 insertion(+), 1 deletion(-)

In the same way we can find the data for reflex-dom-contrib:

    $ git show --stat 11db20865fd275362be9ea099ef88ded425789e7

    commit 11db20865fd275362be9ea099ef88ded425789e7
    Merge: b9e2965 4067455
    Author: Doug Beardsley <mightybyte@gmail.com>
    Date:   Fri Jun 26 09:33:43 2020 -0400

        Merge pull request #67 from malte-v/master
        
        Loosen upper bounds

    reflex-dom-contrib.cabal | 8 ++++----
    1 file changed, 4 insertions(+), 4 deletions(-)

So this is quite old by now. Let's see if we can update these dependencies. The command `git tag -n` shows me all tags with a short description:

    $ git tag -n

    v0.1.0.0        Update changelog and adopt version numbering
    v0.2.0.0        Merge pull request #602 from reflex-frp/fix-change-log-date
    v0.3.0.0        Merge pull request #567 from reflex-frp/misc-bumps-before-19.09
    v0.4.0.0        Merge pull request #537 from reflex-frp/nixpkgs-19.09
    v0.4.1.0        Assign version in change log for realase
    v0.4.2.0        Bump version number for release
    v0.5.0.0        Bump version number
    v0.5.1.0        Merge pull request #624 from reflex-frp/bump-reflex
    v0.5.2.0        Merge pull request #641 from reflex-frp/release/0.5.2.0
    v0.5.3.0        Version 0.5.3.0
    v0.6.0.0        Merge branch 'develop' into rc/0.6.0.0
    v0.6.1.0        Update changelog
    v0.6.2.0        Update changelog
    v0.7.0.0        Update changelog
    v0.7.1.0        Merge branch 'aa/reflex-0.8.1.0' into develop
    v0.8.0.0        Bump verion number for release
    v0.8.0.1        Fix change log slightly
    v0.8.0.2        Add Change Log entry for another tiny release
    v0.8.0.3        Document next patch release
    v0.9.0.0        Release as 0.9.0.0
    v0.9.1.0        Update changelog
    v0.9.2.0        haskell-overlays: reflex-dom-core -> 0.7.0.0 (#761)

Of course we want the latest version! But what is its `rev` and `sha512`?

    $ git rev-list -n 1 v0.9.2.0

    123a6f487ca954fd983f6d4cd6b2a69d4c463d10

Here `-n 1` limits output to one commit. Now let's calculate the sha256 using `nix-prefetch-url`:

    $ nix-prefetch-url --unpack https://github.com/reflex-frp/reflex-platform/archive/123a6f487ca954fd983f6d4cd6b2a69d4c463d10.zip

    path is '/nix/store/cz0k9bgbvch56nxc9gb55n101idi1v8v-123a6f487ca954fd983f6d4cd6b2a69d4c463d10.zip'
    16q1rq0rwi6l28fv46q8m0hvb9rxrzf574j865vaz04xy8d5p1ya

The second line should be the sha256 for this commit. Let's insert this into our default.nix file and see how it works.

Ok, it builds a lot of stuff and this must be the right way. But in the end we get some errors from reflex-dom-contrib:

    $ nix-build . -A ghcjs.frontend -o frontend-bin

    ...
    Building library for reflex-dom-contrib-0.6..
    [ 1 of 21] Compiling Reflex.Contrib.Interfaces ( src/Reflex/Contrib/Interfaces.hs, dist/build/Reflex/Contrib/Interfaces.js_o )
    [ 2 of 21] Compiling Reflex.Contrib.Utils ( src/Reflex/Contrib/Utils.hs, dist/build/Reflex/Contrib/Utils.js_o )
    [ 3 of 21] Compiling Reflex.Dom.Contrib.CssClass ( src/Reflex/Dom/Contrib/CssClass.hs, dist/build/Reflex/Dom/Contrib/CssClass.js_o )
    [ 4 of 21] Compiling Reflex.Dom.Contrib.Geoposition ( src/Reflex/Dom/Contrib/Geoposition.hs, dist/build/Reflex/Dom/Contrib/Geoposition.js_o )
    [ 5 of 21] Compiling Reflex.Dom.Contrib.KeyEvent ( src/Reflex/Dom/Contrib/KeyEvent.hs, dist/build/Reflex/Dom/Contrib/KeyEvent.js_o )
    [ 6 of 21] Compiling Reflex.Dom.Contrib.Router ( src/Reflex/Dom/Contrib/Router.hs, dist/build/Reflex/Dom/Contrib/Router.js_o )

    src/Reflex/Dom/Contrib/Router.hs:82:5: error:
        Not in scope: type constructor or class ‘HasJSContext’
    |
    82 |   , HasJSContext m
    |     ^^^^^^^^^^^^

    src/Reflex/Dom/Contrib/Router.hs:83:5: error:
        Not in scope: type constructor or class ‘HasJSContext’
    |
    83 |   , HasJSContext (Performable m)
    |     ^^^^^^^^^^^^

    src/Reflex/Dom/Contrib/Router.hs:110:5: error:
        Not in scope: type constructor or class ‘HasJSContext’
        |
    110 |   , HasJSContext m
        |     ^^^^^^^^^^^^

    src/Reflex/Dom/Contrib/Router.hs:111:5: error:
        Not in scope: type constructor or class ‘HasJSContext’
        |
    111 |   , HasJSContext (Performable m)
        |     ^^^^^^^^^^^^

    src/Reflex/Dom/Contrib/Router.hs:135:5: error:
        Not in scope: type constructor or class ‘HasJSContext’
        |
    135 |   , HasJSContext m
        |     ^^^^^^^^^^^^

    src/Reflex/Dom/Contrib/Router.hs:136:5: error:
        Not in scope: type constructor or class ‘HasJSContext’
        |
    136 |   , HasJSContext (Performable m)
        |     ^^^^^^^^^^^^

    src/Reflex/Dom/Contrib/Router.hs:198:15: error:
        Not in scope: type constructor or class ‘HasJSContext’
        |
    198 | goForward :: (HasJSContext m, MonadJSM m) => m ()
        |               ^^^^^^^^^^^^

    src/Reflex/Dom/Contrib/Router.hs:203:12: error:
        Not in scope: type constructor or class ‘HasJSContext’
        |
    203 | goBack :: (HasJSContext m, MonadJSM m) => m ()
        |            ^^^^^^^^^^^^

    src/Reflex/Dom/Contrib/Router.hs:208:17: error:
        Not in scope: type constructor or class ‘HasJSContext’
        |
    208 | withHistory :: (HasJSContext m, MonadJSM m) => (History -> m a) -> m a
        |                 ^^^^^^^^^^^^

    src/Reflex/Dom/Contrib/Router.hs:222:12: error:
        Not in scope: type constructor or class ‘HasJSContext’
        |
    222 | getLoc :: (HasJSContext m, MonadJSM m) => m Location
        |            ^^^^^^^^^^^^

    src/Reflex/Dom/Contrib/Router.hs:236:16: error:
        Not in scope: type constructor or class ‘HasJSContext’
        |
    236 | getUrlText :: (HasJSContext m, MonadJSM m) => m T.Text
        |                ^^^^^^^^^^^^

    src/Reflex/Dom/Contrib/Router.hs:245:12: error:
        Not in scope: type constructor or class ‘HasJSContext’
        |
    245 | getURI :: (HasJSContext m, MonadJSM m) => m URI
        |            ^^^^^^^^^^^^

    error: builder for '/nix/store/0nw3qicfcma0imfz53hsbhcacd2rmaca-reflex-dom-contrib-0.6.drv' failed with exit code 1
    error: 1 dependencies of derivation '/nix/store/jxpdg4rj8ja03yrrf3mlkwlnbl65h8rl-frontend-0.1.0.0.drv' failed to build
    nix-build . -A ghcjs.frontend -o frontend-bin  40.42s user 8.95s system 100% cpu 49.207 total

It seems I have to update reflex-dom-contrib as well. So...

    $ git tag -n

    0.1             Add more docs
    0.2             Version bump
    0.3             Fix warnings
    0.4             Bump version bounds
    0.4.1           Fix warnings

Ups this is older than the version 0.6 that failed above. It seems they didn't tag any versions recently. So lets try the current master:

    $ git rev-list -n 1 master

    e19e6eeaccbb1bbd2bdbcce6e388611e40839d95

But fetchGit cannot find this:

    error: Cannot find Git revision 'e19e6eeaccbb1bbd2bdbcce6e388611e40839d95' in ref 'master' of repository 'https://github.com/reflex-frp/reflex-dom-contrib.git'! Please make sure that the rev exists on the ref you've specified or add allRefs = true; to fetchGit.
    (use '--show-trace' to show detailed location information)

Ok, getting the latest commit manually from GitHub gives me `09d1223a3eadda768a6701410b4532fda8c7033d` and the build starts with thisbut fails again:

    Building library for reflex-dom-contrib-0.6..
    [ 1 of 21] Compiling Reflex.Contrib.Interfaces ( src/Reflex/Contrib/Interfaces.hs, dist/build/Reflex/Contrib/Interfaces.js_o )
    [ 2 of 21] Compiling Reflex.Contrib.Utils ( src/Reflex/Contrib/Utils.hs, dist/build/Reflex/Contrib/Utils.js_o )
    [ 3 of 21] Compiling Reflex.Dom.Contrib.CssClass ( src/Reflex/Dom/Contrib/CssClass.hs, dist/build/Reflex/Dom/Contrib/CssClass.js_o )
    [ 4 of 21] Compiling Reflex.Dom.Contrib.Geoposition ( src/Reflex/Dom/Contrib/Geoposition.hs, dist/build/Reflex/Dom/Contrib/Geoposition.js_o )
    [ 5 of 21] Compiling Reflex.Dom.Contrib.KeyEvent ( src/Reflex/Dom/Contrib/KeyEvent.hs, dist/build/Reflex/Dom/Contrib/KeyEvent.js_o )
    [ 6 of 21] Compiling Reflex.Dom.Contrib.Router ( src/Reflex/Dom/Contrib/Router.hs, dist/build/Reflex/Dom/Contrib/Router.js_o )

    src/Reflex/Dom/Contrib/Router.hs:82:5: error:
        Not in scope: type constructor or class ‘HasJSContext’
    |
    82 |   , HasJSContext m
    |     ^^^^^^^^^^^^

    src/Reflex/Dom/Contrib/Router.hs:83:5: error:
        Not in scope: type constructor or class ‘HasJSContext’
    |
    83 |   , HasJSContext (Performable m)
    |     ^^^^^^^^^^^^

    src/Reflex/Dom/Contrib/Router.hs:110:5: error:
        Not in scope: type constructor or class ‘HasJSContext’
        |
    110 |   , HasJSContext m
        |     ^^^^^^^^^^^^

    src/Reflex/Dom/Contrib/Router.hs:111:5: error:
        Not in scope: type constructor or class ‘HasJSContext’
        |
    111 |   , HasJSContext (Performable m)
        |     ^^^^^^^^^^^^

    src/Reflex/Dom/Contrib/Router.hs:135:5: error:
        Not in scope: type constructor or class ‘HasJSContext’
        |
    135 |   , HasJSContext m
        |     ^^^^^^^^^^^^

    src/Reflex/Dom/Contrib/Router.hs:136:5: error:
        Not in scope: type constructor or class ‘HasJSContext’
        |
    136 |   , HasJSContext (Performable m)
        |     ^^^^^^^^^^^^

    src/Reflex/Dom/Contrib/Router.hs:198:15: error:
        Not in scope: type constructor or class ‘HasJSContext’
        |
    198 | goForward :: (HasJSContext m, MonadJSM m) => m ()
        |               ^^^^^^^^^^^^

    src/Reflex/Dom/Contrib/Router.hs:203:12: error:
        Not in scope: type constructor or class ‘HasJSContext’
        |
    203 | goBack :: (HasJSContext m, MonadJSM m) => m ()
        |            ^^^^^^^^^^^^

    src/Reflex/Dom/Contrib/Router.hs:208:17: error:
        Not in scope: type constructor or class ‘HasJSContext’
        |
    208 | withHistory :: (HasJSContext m, MonadJSM m) => (History -> m a) -> m a
        |                 ^^^^^^^^^^^^

    src/Reflex/Dom/Contrib/Router.hs:222:12: error:
        Not in scope: type constructor or class ‘HasJSContext’
        |
    222 | getLoc :: (HasJSContext m, MonadJSM m) => m Location
        |            ^^^^^^^^^^^^

    src/Reflex/Dom/Contrib/Router.hs:236:16: error:
        Not in scope: type constructor or class ‘HasJSContext’
        |
    236 | getUrlText :: (HasJSContext m, MonadJSM m) => m T.Text
        |                ^^^^^^^^^^^^

    src/Reflex/Dom/Contrib/Router.hs:245:12: error:
        Not in scope: type constructor or class ‘HasJSContext’
        |
    245 | getURI :: (HasJSContext m, MonadJSM m) => m URI
        |            ^^^^^^^^^^^^

    error: builder for '/nix/store/r1zjlisa24k855ah9n880dqcnydv96ds-reflex-dom-contrib-0.6.drv' failed with exit code 1
    error: 1 dependencies of derivation '/nix/store/fnd9wlqyz2qxm9h4kdxwca807gcdgzaz-frontend-0.1.0.0.drv' failed to build


So it seems noone has fixed this yet. As all these errors seem to have a common root and this in turn sjust an import error for `HasJSContext` we can try to fix it. In reflex-dom-core version 0.6 this was in the module Foreign.JavaScript.TH but its gone in the current release 0.7.
