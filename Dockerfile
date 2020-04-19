FROM ocaml/opam2:debian-10-ocaml-4.10
RUN opam install ocamlbuild && mkdir /home/opam/project

