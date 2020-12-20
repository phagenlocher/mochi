# mochi

### General Info

**mochi**, a retired student project, is a tiny LTL model checker for a simplified, concurrent programming language. It generates a Büchi automaton from the negation normal form of an LTL formula and a Kripke structure from the program, taking into account every possible order of process execution. It verifies the formula by intersecting the automaton with the structure and searching for infinite loops with DFS. 

This implementation is *highly* inefficient.

### How to use

The behaviour of mochi is decided by the mode it is in.
* -l activates LTL mode (requires LTL formula and outputs NNF of the formula)
* -b activates Büchi mode (requires LTL formula and outputs HOA)
* -g if Büchi mode is active the automaton will not be degeneralized before outputting HOA
* -k activates Kripke mode (requires i7w program and outputs DOT)

No specified mode means that mochi will try to do model checking with a supplied formula and program.

**-f** is used to supply the formula and **-p** to supply the program.

So, in order to do model checking one would use:
```./mochi.native -f <formula> -p <program>```

In order to force fairness in process execution one can use **-fair**:
```./mochi.native -f <formula> -p <program> -fair```

### Known Bugs
* Single LTL formulas do NOT produce the correct GBA (see failing-ltl.txt)

### How to build
The system needs to have OCaml installed. 
``` make ``` should do the trick and create _mochi.native_ as a soft link to the executable.

### How to test Büchi translation
_test\_inf.sh_ runs ltlcross with mochi and ltl2tgba until an error was found.
The formulas are generated randomly by mochi. A complexity of more than 3 takes _alot_ of time.

```Usage: ./test_inf.sh <recursive complexity of the LTL formula>```

_test\_one.sh_ runs ltlcross with mochi and ltl2tgba with one specified formula.

```Usage: ./test_one.sh <the LTL formula to test>```

_test\_file.sh_ runs ltlcross with mochi and ltl2tgba on a file of LTL formulas.

```Usage: ./test_file.sh <the file with LTL formulas to test>```
