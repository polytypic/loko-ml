(lang dune 3.14)

(name loko)

(generate_opam_files true)

(implicit_transitive_deps false)

(authors "Vesa Karvonen <vesa.a.j.k@gmail.com>")

(maintainers "Vesa Karvonen <vesa.a.j.k@gmail.com>")

(source
 (github polytypic/loko-ml))

(homepage https://github.com/polytypic/loko-ml)

(license ISC)

(using mdx 0.4)

(package
 (name loko)
 (synopsis "Lower-Kinded Optics for OCaml")
 (depends
  (ocaml
   (>= 5.3))
  ;;
  (mdx
   (and
    (>= 2.5.0)
    :with-test))
  ;;
  ))
