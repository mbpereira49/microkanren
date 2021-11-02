# microkanren
Implementation of microKanren using OCaml

## Usage
Can run the tests via:
```
ocamlc -o program.o -I src/ -I utils/ -I test/ src/types.ml src/microkanren.ml utils/print.ml utils/sugar.ml test/test_programs.ml test/tests.ml
```