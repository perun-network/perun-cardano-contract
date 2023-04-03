# Perun Client

This library contains client functionality to interact with PAB instances from `perun-pab` and `perun-contract` over HTTP.
We also ship an example and test client, that can be used to run an example trace between two parties, showcasing the on-chain functionality of Perun.

* `Perun.Client`: Contains the `PerunClient`, `AdjudicatorClient` and `MultiClient` implementations:
    * `PerunClient`: Interacts with the Perun contract instance of the [`perun-pab-exe`](../perun-pab/README.md).
    * `AdjudicatorClient`: Interacts with the Adjudicator contract instance of the [`perun-pab-exe`](../perun-pab/README.md).
    * `MultiClient`: Is an eDSL to describe the interaction of multiple clients. An example for its use can be found in [`app/Client/Main.hs`](./app/Client/Main.hs#L146).
* `Perun.Websocket`: Is a small wrapper around a websocket connection to the [`perun-pab-exe`](../perun-pab/README.md).
