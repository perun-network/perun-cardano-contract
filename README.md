# Perun Plutus

Implementation for the Perun protocol in Plutus.

# Development

## Plutus SDK Reference

Hosted by cardano, it is almost a requirement to have the haddock docs open in
the background as a reference. Especially when working with Haskell libraries:
  * [Hackage Libraries | Everything that does not come from the Plutus SDK](https://hackage.haskell.org/)
  * [Plutus SDK](https://input-output-hk.github.io/plutus/master/)

It can also be beneficial to access the up-to-date docs for your locally checked out SDK version via `nix`.
For this follow the [Nix-Section](##Nix).
Afterwards issue the command in the `plutus-apps` root directory: `$ nix-shell --run build-and-serve-docs`.

## Setup

Developing contracts in Plutus can be done in one of two ways: **Native** or utilizing **Nix**.

## Native

Developing Plutus contracts using your native Haskell installation works just like developing any other basic Haskell project.

Everything custom to Plutus is related to the sources WHERE `cabal` searches for dependencies.

All these peculiarities are handled in the `cabal.project` file, which references, pins and exposes all dependencies needed.

It can be a hassle to manually manage these dependencies, but you most likely won't have to deal with them and the issue persists even when using `Nix`.

### Haskell Installation

Do yourself a favor and [install `ghcup`](https://www.haskell.org/ghcup/).

`ghcup` is an installer for the Haskell language and related tools.

It also makes it VERY easy to switch between _active_ compiler, HLS and build-tool versions.

Remember to add the `$PATH` for `ghcup` binaries.

This project uses `GHC-8.10.7`, `cabal-3.6.2.0`, `HLS-1.7.0.0`.

Using `ghcup tui` will bring up an interface where each appropriate version can be installed **AND** set.

If you already have other `GHC` versions installed make sure you see ✔✔  and not just ✔.

Otherwise the toolchain is only installed but not enabled.

### Start developing

Issue a `cabal build` in the project directory.

It will take some time the first time around, because all dependencies have to be build and will be faster in subsequent runs, unless you/we update dependencies again.

**NOTE**: If `cabal build` fails and `ghc` is complaining about some _missing files_ in _some_ package, check out `perun-plutus.cabal`, uncomment the `-dynamic` option for `ghc-opions` and try again

If this works, please file a PR where this line stays uncommented.

As a last resort one can also try to enable `-dynamic` for every dependency by issuing `cabal build --ghc-options=-dynamic`.
Most of the time this only has to be done once, s.t. `cabal` and `ghc` have the problem dependency build.

## Nix

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

Using HLS greatly improves effiency and gives a lot of code-style suggestions which lets a Haskell developer explore the language way better.

Especially if they are new.

Modern IDEs and Code-Editors allow to specify the exact binary which should be run as a language server for specific filetypes.

**NOTE**: If you use the `native` development option `HLS_COMMAND = haskell-language-server-wrapper`, for nix use `HLS_COMMAND = haskell-language-server`!

So the following might have some pitfalls depending on the editor you are using but in essence all which has to be done is to let your editor use the command `${HLS_COMMAND} --lsp` which is available in the active `nix-shell` together with all paths for all required project dependencies.

This should be achievable by just executing your editor within `nix-shell` and configuring the LSP command for Haskell files in its settings:

```
[nix-shell: user@machine:path/to/project$ code .
```

**Note**: This also means that one has to navigate to the project itself within the current `nix-shell`.

### Help HLS does not seem to be running

The very first start of HLS will `cabal build` the project in the project. Despite all preparation of prebuilt binaries this can take quite some time. If you want to know when it SHOULD be ready just `cabal build` the project the very first time yourself or after you upgrade the SDK to know when it is done. Using HLS afterwards should just work.

# Known Issues

HLS is throwing an error related to Template-Haskell along the following lines:

```
src/PerunDummy.hs|1-2 col 1 error|
Program error:
GHC Core to PLC plugin:
E043:Error:
  Reference to a name which is not a local, a builtin, or an external INLINABLE function:
  Variable Plutus.V1.Ledger.Contexts.scriptContextTxInfo [[RecSel]] No unfolding Context:
  Compiling expr:
  Plutus.V1.Ledger.Contexts.scriptContextTxInfo Context:
  Compiling expr:
  Plutus.V1.Ledger.Contexts.scriptContextTxInfo ctx Context:
  Compiling expr:
  let { info :: Plutus.V1.Ledger.Contexts.TxInfo [LclId, Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False, ConLike=False, WorkFree=False, Expandable=True, Guidance=IF_ARGS [] 20 0}] info = Plutus.V1.Ledger.Contexts.scriptContextTxInfo ctx }
    in let { ds_d2z2G :: (Plutus.V1.Ledger.Tx.TxOut, PerunDummy.ChannelDatum) [LclId, Unf=Unf{Src=<vanilla>, TopLvl=False, Value=False, ConLike=False, WorkFree=False, Expandable=False, Guidance=IF_ARGS [] 728 30}]
             ds_d2z2G = join { fail_d2z2E [Occ=Once2!T[1]] :: GHC.Prim.Void# -> (Plutus.V1.Ledger.Tx.TxOut, PerunDummy.ChannelDatum) [LclId[JoinId(1)], Arity=1, Str
  ...
```

HLS will still work for the rest of the file and this error is only an annoyance and can be considered a _false negative_.

HLS seems to not respect the order of how all symbols and dependencies should be resolved.

IF, however, this error occurs when using `cabal build` it probably is really because none of the conditions `Reference to a name which is not a local, a builtin, or an external INLINABLE function` is true (;
