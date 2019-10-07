
This repository provides a set of command-line tools for parsing, checking and translating specifications and programs written using the [B method](https://en.wikipedia.org/wiki/B-Method).

### Tools

  * `bformat` reformats a B machine. **Remark:** comments are lost and definitions are expanded.
  * `brandom` generates a random and syntactically valid B machine.
  * `btags` generates a \_tag file for a B machine using the [ctags](http://ctags.sourceforge.net/) format.
  * `btypecheck` typechecks a B machine.
  * `b2sexp` prints the AST of a B machine as an s-expression (mostly useful for debugging the parser).
  * `bdefs` dumps the definition table (mostly useful for debugging the lexer).

### Known limitations

#### All tools:
  * Machine renaming is not supported.

#### `btypechecks`:
  * Machines with parameters are not supported.
  * The following clauses are not supported: CONSTRAINTS, USES.
  * The following clauses are only supported in implementations: LOCAL\_OPERATIONS.

### Experimental B to Ada and Rust translators
  * [Ada](https://github.com/rsaill/parser-b/tree/b2ada)
  * [Rust](https://github.com/rsaill/parser-b/tree/rework_rust)
