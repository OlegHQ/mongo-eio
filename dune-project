(lang dune 3.16)

(name mongo)

(package
 (name mongo)
 (synopsis "OCaml MongoDB driver with additive Eio wrappers")
 (description "Modern Dune packaging for MassD mongo.ml plus additive OCaml 5/Eio wrappers.")
 (depends
  (ocaml (>= 5.0))
  (dune (>= 3.16))
  bson
  eio))

