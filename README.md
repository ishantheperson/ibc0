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

This consists of an executable named `simple`. There was an interpreter/REPL
but these have been removed in order to focus on the code generator.

The BC0 generator is run by passing the `-b` flag to `simple`, followed by file names.
```sh
% ls 
conditional_test.txt
string_test.txt

% simple -b *.txt

% ls 
conditional_test.bc0
string_test.bc0
conditional_test.txt
string_test.txt
```
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

```c
a = 10;
b = true;
c = "hello ";
d = "world";
e = c + d;
f = "there are " + 6 + " variables in this example";
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

> Note that functions are not implemented yet in the code generator 

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