# Advent of Code 2020

This repository houses my solutions to the 2020 edition of the [advent of code](https://adventofcode.com).

The language of choice for my solutions is Haskell.

# Organization

All inputs are in the `inputs/` directory.

Each day is separated into its own binary, so each has its own Main module in the `app/dayX` directory.

# Running

To execute the project, you need [stack](https://docs.haskellstack.org/en/stable/README/).

in the root directory of the project, run

```sh
stack run dayX
```

changing `X` to the number of the day you want to run.
