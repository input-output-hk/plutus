# Building

## Server

### stack

```sh
stack build plutus-playground-server
stack exec -- plutus-playground-server psgenerator ./plutus-playground-client/generated
stack exec -- plutus-playground-server webserver
```

### nix

```sh
$(nix-build -A plutus-playground.server)/bin/plutus-playground-server webserver
```

> **NOTE**: You may notice that the executable that nix exposes does not accept
> arguments; that's because the exposed command is actually a script run by
> [this attribute](https://github.com/input-output-hk/plutus/blob/master/plutus-playground-client/default.nix#L43).
> Use the above `nix-build` sub-command to expose the actual haskell executable.

## Testing

Tests should be run with nix:

```sh
nix build -L -f default.nix plutus.haskell.packages.plutus-playground-server.checks
```
