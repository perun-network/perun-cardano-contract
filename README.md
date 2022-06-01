# Perun Plutus

Implementation for the Perun protocol in Plutus.

# Development

Developing contracts in Plutus requires having the [Plutus Haskell SDK](https://github.com/input-output-hk/plutus-apps) installed.

Subpoints in the following contain some trivia and reasoning for why things have to be done this way.

In summary the following steps have to be done:

1. Either use [NixOS](https://nixos.org/) or have a working [nix](https://nixos.wiki/wiki/Nix_Installation_Guide) installation:

    * Nix is just a functional and **deterministic** package management system. This shall guarantee a reproducible development environment without painful conversations like:

        > Q: Why does this not work?

        > A: Dunno, it does for me.

    * Make sure you [configure your `nix` environment](https://nixos.wiki/wiki/Flakes) to enable `flakes` support for your installation!

    * `flakes` should in theory just streamline dependency management within `nix`. As of today (01.06.22) it is still experimental but is used throughout the ecosystem requiring enabling it manually.

2. Clone the [Plutus Haskell SDK](https://github.com/input-output-hk/plutus-apps) and `cd` into its directory.

3. **IMPORTANT**: Make sure you follow the instructions to [setup IOHK's binary caches](https://github.com/input-output-hk/plutus-apps#how-to-set-up-the-iohk-binary-caches). Otherwise you will arrive at the next step and wait hours upon hours for everyting to compile:

    * Binary caches for nix serve precompiled binaries which are referenced in your `nix` environment/project. Since IOHK uses a lot of custom Haskell libraries and executables they serve most configurations already prebuilt, s.t. you do not have to do that explicitly everytime you run `nix-shell`. Bear in mind though, this holds only for the dependencies! When you `cabal build` your Haskell project which depends on custom dependencies within your `nix` environment `cabal` will STILL build quite a lot anew.

4. Run `nix-shell`:

    * If you did not bother with step 1.2 then execute `nix-shell --extra-experimental-features flakes`

    * `nix-shell` reads the `default.nix` file in the directory where it was invoked in, pulls and builds all defined dependencies and creates a shell with an environment referencing everything necessary related to `nix` and all dependencies to work. This includes binaries, libraries and scripts which might be bound to extra commands. Thus one cannot just call `nix-shell` wherever, to use the SDK (at the time of writing), one has to navigate to his own `plutus-apps` clone, invoke `nix-shell` there and afterwards navigate to the project which uses the dependencies located in the `nix` environment.

    * A solution for the aforemention point is to create a `default.nix` file for this repository, but this is a TODO.

**DONE** you are ready to develop.

One might now wonder what the development looks like. In summary: All commands which are related to the project of interest like `cabal build`, `cabal update`, `cabal test`, as well as all binaries necessary to run the Plutus SDK **have to be run from the current `nix-shell` environment**!

## Getting HLS (Haskell-Language-Server) running

Using HLS greatly improves effiency and gives a lot of code-style suggestions which lets a Haskell developer explore the language way better. Especially if they are new. Modern IDEs and Code-Editors allow to specify the exact binary which should be run as a language server for specific filetypes. So the following might have some pitfalls depending on the editor you are using but in essence all which has to be done is to let your editor use the command `haskell-language-server --lsp` which is available in the active `nix-shell` together with all paths for all required project dependencies.

This should be achievable by just executing your editor within `nix-shell` and configuring the LSP command for Haskell files in its settings:

```
[nix-shell: user@machine:path/to/project$ code .
```

**Note**: This also means that one has to navigate to the project itself within the current `nix-shell`.

### Help HLS does not seem to be running

The very first start of HLS will `cabal build` the project in the project. Despite all preparation of prebuilt binaries this can take quite some time. If you want to know when it SHOULD be ready just `cabal build` the project the very first time yourself or after you upgrade the SDK to know when it is done. Using HLS afterwards should just work.
