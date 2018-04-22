
This repository provides a set of command-line tools for parsing, checking and translating specifications and programs written using the [B method](https://en.wikipedia.org/wiki/B-Method).

### Tools

  * `bparse` checks that a B machine is syntactically valid.
  * `bformat` reformats a B machine. **Remark:** comments are lost and definitions are expanded.
  * `brandom` generates a random and syntactically valid B machine.
  * `btags` generates a \_tag file for a B machine using the [ctags](http://ctags.sourceforge.net/) format.
  * `btypecheck` typechecks a B machine.
  * `b2ada` convert a B implementation into an Ada program (this is work in progress in branch code_gen_2).

### Known limitations

#### All tools:
  * Machine renaming is not supported (All tools).

#### `btypechecks` and `b2ada`:
  * Machines with parameters are not supported.
  * The following clauses are not supported: CONSTRAINTS, USES.
  * The following clauses are only supported in implementations: PROMOTES, EXTENDS.

#### `b2ada`
  * Records are not supported.
  * Support for arrays is limited:
    * Only concrete sets can be used to index arrays.

### TODO
  * Document the code.
  * Write more tests.
  * Improve error localization when using definitions.
  * Work on known limitations given above.

### Tools I might write in the future:
  * Ada to B translator.
  * Proof Obligation (PO) generator.
  * B to Rust translator.
  * Generator of well-typed machines.
