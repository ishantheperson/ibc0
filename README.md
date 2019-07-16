# ibc0

A work-in-progress compiler for a simple language which targets BC0.

### Dependencies

Requires the Haskell package `lens`

```
% cabal install lens
```

### Compiling

Simply compile with `make`. 

## Usage

This consists of an executable named `simple`. Without any options,
it will interpret a file. With `-b`, it will compile the file to a `.bc0` executable.
With `-i` the REPL will launch. 

Examples are contained in the `examples/` directory. 