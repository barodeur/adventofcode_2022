(lang dune 3.6)

(name aoc2022)

(generate_opam_files true)

(package
 (name aoc2022)
 (depends
  ocaml
  dune
  (core (and (>= 0.15.0) (< 0.16.0)))
  eio_main
 ))

