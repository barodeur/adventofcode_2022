(lang dune 3.6)

(name aoc2022)

(generate_opam_files true)

(using menhir 2.1)

(package
 (name aoc2022)
 (depends
  ocaml
  dune
  (core (and (>= v0.15.1) (< v0.16.0)))
  (ppx_compare (and (>= v0.15.0) (< v0.16.0)))
  (ppx_fields_conv (and (>= v0.15.0) (< v0.16.0)))
  (ppx_sexp_conv (and (>= v0.15.1) (< v0.16.0)))
  (re2 (and (>= v0.15.0) (< v0.16.0)))
  (eio_main (= 0.7))
 ))

