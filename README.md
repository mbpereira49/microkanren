# &mu;Kanren
Implementation of [microKanren](http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf) using OCaml. Based on [original Scheme implementation](https://github.com/jasonhemann/microKanren).

## Usage
Can compile the tests into a binary `program.o` via:
```
ocamlc -o program.o -I src/ -I utils/ -I test/ src/types.ml src/microkanren.ml utils/print.ml utils/sugar.ml test/test_programs.ml test/tests.ml
```