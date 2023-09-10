Given a file `foo.ml` that contains a C stub and a 'foostubs.c' that implements.

Generate some dune rules, for example by doing this:
```sh
$ lintcstubs-dune-rules >dune.analysis.inc
```

And then adding this snippet to your dune file:
```
(include dune.analysis.inc)
```

The generated file can be kept up-to-date by running:
```sh
dune runtest --auto-promote
```

You will have to create a `compile_commands.json`, e.g. by using [`dune-compiledb`](https://github.com/edwintorok/dune-compiledb/):
```sh
dune rules | dune-compiledb
```

Now you can run `dune build @analyze` to trigger a static analysis.

TODO: this is very experimental.