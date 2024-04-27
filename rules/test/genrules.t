Get rules:
  $ cd example
  $ echo '(lang dune 3.0)' >dune-project
  $ touch lintcstubs.opam
  $ dune rules >rules

Try to parse them
  $ ../parse_test.exe <rules
  OK

Get recursive rules:
  $ dune rules -r >rules

Try to parse recursive rules
  $ ../parse_test.exe <rules
  OK
