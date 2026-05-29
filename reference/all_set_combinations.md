# Enumerate all 2^n - 1 combination labels for a set of names

Generates labels in cardinality-first order: singletons first, then
pairs, then triples, etc. Within each cardinality, set names appear in
their original order. Used only by the Venn path (bounded at n = 5).

## Usage

``` r
all_set_combinations(setnames)
```

## Arguments

- setnames:

  a character vector of set names

## Value

A character vector of length 2^n - 1.
