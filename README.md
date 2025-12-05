# Advent of Code 2025

> All in Haskell this year, to force myself to get better with the language.

Each `.hs` files corresponds to a specific day, and reads input from stdin. They can be run as follows:

```sh
# for a given day N (padded with a leading zero if necessary)
runhaskell DayN.hs < input.txt

# e.g. for day 3
runhaskell Day03.hs < data/day03-input.txt
```

The files can also be compiled with `ghc` by using the `-main-is` flag to set the module name. Some examples:

```sh
# the most simple working example
ghc -main-is Day04 Day04.hs
./Day04 < data/day04-input.txt

# with optimisations (and explicitly setting the main function)
ghc -O2 -main-is Day02.main Day02.hs
./Day02 < data/day02-input.txt

# to keep the root directory clean
ghc -O2 -main-is Day05 -outputdir ./out -o ./out/Day05 Day05.hs
./out/Day05 < data/day05-input.txt
```
