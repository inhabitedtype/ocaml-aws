Important:

Each of the subdirectories within this directory have portions that
are automatically generated. The tree of each subdirectory is the
following, with automatically generated portions asterisked:

```
libname/
  _oasis  ***********
  lib/
    _.ml  ***********
    _.mli  **********
  lib_test/
    test.ml
    ...
  _oasis_append
  setup.ml  *********
  myocamlbuild.ml  **
  Makefile
```

If you edit any of the generated files, your changes will likely get
overridden.

Anything in \_oasis\_append will automatically be appended to the \_oasis file
that is automatically generated. The intention is to support the existence of
manually written and maintained test suites (in lib\_test) alongside the
automatically generated library bindings (in lib).
