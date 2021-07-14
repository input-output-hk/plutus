# plutus-tx-plugin: Plutus Tx compiler plugin

This contains the Plutus Tx compiler plugin itself. This should
be added as a dependency, but used via the functions in the
`plutus-tx` package.

This is in a separate package because it depends on `ghc`. Packages
which need to be cross-compiled should depend on this conditionally.
Use the following snippet:

.package.cabal
----
if !(impl(ghcjs) || os(ghcjs) || os(windows))
    build-depends: plutus-tx-plugin
----
