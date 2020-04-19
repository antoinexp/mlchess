pull:

`git pull https://github.com/antoinexp/mlchess.git`


start:

`docker-compose up -d`


build:

`docker-compose exec ocaml opam config exec -- make build`


run:

`docker-compose exec ocaml opam config exec -- make run`


stop:

`docker-compose down`
