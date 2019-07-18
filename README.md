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

## Language Guide

### Comments
Comments are C++ style
```c
/* Inline comment */ 
// Line comment
```

### Variables 

Currently variables are not typed and do not capture scope.
There are 2 scopes, function scope and non-function scope. They don't overlap.

```c
a = 10
b = true
c = "hello world"
```

### Printing

To print something out simply use the `print` statement.

```python
print "hello world";
a = 10
print a;
print a + 20;
```

### Functions 

Functions can be declared traditionally or be expression bodied similar to Javascript lambdas.
Functions are not first class, cannot be nested, etc. 

```javascript
f() {
  print "in function f";
  // ...
}

square(x) => x * x;
```

### Loops

Currently only `while` loops are part of the language

```c
i = 0
while (i < 10) {
  i = i + 1;
  print i;
}
```

Loops do not create a new scope. Braces must be used.