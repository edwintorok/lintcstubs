Get rules:
  $ dune rules >rules

  $ cat rules
  ((deps ((File (In_source_tree parse_test.exe))))
   (targets ((files (_build/default/parse_test.exe)) (directories ())))
   (context default)
   (action (copy parse_test.exe _build/default/parse_test.exe)))
  
  ((deps ((File (In_source_tree rules))))
   (targets ((files (_build/default/rules)) (directories ())))
   (context default)
   (action (copy rules _build/default/rules)))

Try to parse them
  $ ./parse_test.exe <rules
  OK
  $ dune rules >rules

Get recursive rules:
  $ dune rules -r >rules
  $ cat rules
  ((deps ((File (In_source_tree parse_test.exe))))
   (targets ((files (_build/default/parse_test.exe)) (directories ())))
   (context default)
   (action (copy parse_test.exe _build/default/parse_test.exe)))
  
  ((deps ((File (In_source_tree rules))))
   (targets ((files (_build/default/rules)) (directories ())))
   (context default)
   (action (copy rules _build/default/rules)))

Try to parse them
  $ ./parse_test.exe <rules
  OK
