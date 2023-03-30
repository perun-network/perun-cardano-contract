# Perun Contract

This library defines the Perun on-chain script, used to validate state transitions and access rights to a Perun channel living on Cardano.

* `Perun.Onchain`: Contains said on-chain script
* `Perun.Offchain`: Contains a plutus-contract implementation to interact with a Perun channel script in the context of a Plutus `Contract`.
* `Perun.Adjudicator`: Contains the subscription logic for a Perun adjudicator instance, also in context of a Plutus `Contract`.

As stated above, this is a library and can be used in other Haskell projects by referencing `perun-contract`.
