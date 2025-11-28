# Advent of Code 2025

## Getting started

### Prerequisites

- [Nix](https://nixos.org/download.html) with flakes enabled
- [direnv](https://direnv.net/)

### Setup

1. Navigate to this directory:
   ```bash
   cd 2025
   ```

2. Allow direnv to load the environment:
   ```bash
   direnv allow
   ```

### Running solutions

```bash
cabal run
```

### Development workflow

Start a REPL and load your code:
```bash
cabal repl
```

The REPL automatically loads all modules. You can:
```haskell
-- Import and use a specific day's solution
import Day01
part1 "test input"

-- Or load a specific module directly
:l Day01
part1 <$> readFile "data/day_01_test.txt"

-- Reload after changes
:r
```

Other useful commands:
```bash
cabal build                        # Build the project
cabal test                         # Run tests (when added)
ghcid                              # Auto-reload on changes
hlint .                            # Lint code
ormolu --mode inplace src/**/*.hs  # Format code
```

## Utilities

To quickly set up a new day:

```bash
./add-day.sh 09  # Replace 09 with whatever day you need
```
