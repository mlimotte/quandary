# Quandary

Quandary is a Clojure library for solving constraint satisfaction and optimization problems. It wraps Google's [OR-Tools CP-SAT solver](https://developers.google.com/optimization/reference/python/sat/python/cp_model) behind a high-level DSL so you can express problems as variable domains and equations rather than low-level solver API calls.

## What it does

You define:
- **Domains** — the set of possible values for each variable (ranges, enumerations, booleans, intervals)
- **Equations** — constraints those variables must satisfy (`=`, `<`, `!=`, `=>`, etc.)

Quandary feeds these to CP-SAT and returns all satisfying assignments.

```clojure
(require '[quandary.api :as api])

;; Rabbits and pheasants: 20 heads, 56 legs. How many of each?
(let [rules (api/qdsl {}
              {"r" [:range 0 100]
               "p" [:range 0 100]}
              [= (+ "r" "p") 20]
              [= (+ (* 4 "r") (* 2 "p")) 56])]
  (api/solve rules {:enumerate-all true}))
;; => [{"r" 8, "p" 12}]
```

The `qdsl` macro provides a more ergonomic interface for larger problems, with Clojure-native variable binding, temporary variables, and composable constraint blocks via `collate`.

## Documentation

- [DSL Reference](docs/dsl.md) — full guide to `qdsl`, domain specs, operators, `collate`, `solve` options, and debugging

## Running the tests

```bash
clj -M:test
```

To run a single test var:

```bash
clj -M:test -v quandary.quandary-test/test-name
```

