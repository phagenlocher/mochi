# mochi

### Done 
* Basic LTL simplification
* LTL to NNF
* NNF to GBA based on algorithm by Gerth et al.
* GBA to HOA

### Planned
* Better LTL simplifications / rewriting
* Adding G and F to the GBA translation
* Ongoing bugfixing ;)

### Known Bugs
* Single LTL formulas do NOT produce the correct GBA (see failing-ltl.txt)

### How to build
The system needs to have OCaml installed. 
``` make ``` should do the trick and create _mochi.native_ as a soft link to the executable.

### How to test
_test\_inf.sh_ runs ltlcross with mochi and ltl2tgba until an error was found.
The formulas are generated randomly by mochi. A complexity of more than 3 takes _alot_ of time.

```Usage: ./test_inf.sh <recursive complexity of the LTL formula>```

_test\_one.sh_ runs ltlcross with mochi and ltl2tgba with one specified formula.

```Usage: ./test_one.sh <the LTL formula to test>```

