# roo-haskell

A Haskell list utility library demonstrating filter, map, and fold patterns with comprehensive test coverage.

## Overview

This project provides safe list operations and common list manipulation functions, serving as an example of:

- Safe functional programming patterns (avoiding partial functions)
- Property-based testing with QuickCheck
- Unit testing with HSpec
- Automatic test discovery with hspec-discover
- Cabal build system configuration

## Installation

### Prerequisites

- GHC (Glasgow Haskell Compiler) >= 8.10
- Cabal >= 3.0

### Building

```bash
# Build the library and executable
cabal build

# Run the demo executable
cabal run roo-haskell-exe

# Run the test suite
cabal test

# Run tests with verbose output
cabal test --test-show-details=direct
```

## Library Functions

### Safe Accessors

#### `safeHead :: [a] -> Maybe a`

Safe version of `head` that returns `Nothing` for empty lists.

```haskell
>>> safeHead [1, 2, 3]
Just 1

>>> safeHead []
Nothing
```

#### `safeLast :: [a] -> Maybe a`

Safe version of `last` that returns `Nothing` for empty lists.

```haskell
>>> safeLast [1, 2, 3]
Just 3

>>> safeLast []
Nothing
```

### Filtering

#### `filterBy :: (a -> Bool) -> [a] -> [a]`

Filter a list using a predicate function.

```haskell
>>> filterBy even [1, 2, 3, 4, 5, 6]
[2, 4, 6]

>>> filterBy (> 0) [-2, -1, 0, 1, 2]
[1, 2]
```

### Folding

#### `sumList :: Num a => [a] -> a`

Sum all elements in a list using a fold.

```haskell
>>> sumList [1, 2, 3, 4, 5]
15

>>> sumList []
0
```

#### `reverseList :: [a] -> [a]`

Reverse a list using a fold.

```haskell
>>> reverseList [1, 2, 3]
[3, 2, 1]

>>> reverseList "hello"
"olleh"
```

### Mapping

#### `mapDouble :: Num a => [a] -> [a]`

Double each element in a numeric list.

```haskell
>>> mapDouble [1, 2, 3]
[2, 4, 6]

>>> mapDouble [-1, 0, 1]
[-2, 0, 2]
```

## Testing

The test suite includes:

- **19 unit tests** covering all functions with specific inputs and expected outputs
- **20 property-based tests** using QuickCheck to verify invariants

### Running Tests

```bash
# Run all tests
cabal test

# Run with detailed output
cabal test --test-show-details=direct

# Run with QuickCheck verbose mode (shows generated test cases)
cabal test --test-option=--qc-max-success=1000
```

### Test Properties

Key properties verified by QuickCheck:

| Function | Property |
|----------|----------|
| `safeHead` | Non-empty list returns `Just (head xs)` |
| `safeLast` | `safeHead xs == safeLast (reverseList xs)` |
| `filterBy` | All elements in result satisfy predicate |
| `sumList` | `sumList (xs ++ ys) == sumList xs + sumList ys` |
| `reverseList` | Reversing twice gives original list |
| `mapDouble` | `sumList (mapDouble xs) == 2 * sumList xs` |

## Project Structure

```
roo-haskell/
├── roo-haskell.cabal      # Cabal build configuration
├── Setup.hs               # Standard Cabal setup
├── README.md              # This file
├── src/
│   └── ListUtils.hs       # Main library module
├── app/
│   └── Main.hs            # Demo executable
└── test/
    ├── Spec.hs            # hspec-discover entry point
    └── ListUtilsSpec.hs   # Comprehensive test suite
```

## License

MIT License
