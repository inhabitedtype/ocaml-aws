# ocaml-aws

ocaml-aws is an Amazon Web Services SDK for OCaml. Its source distribution
includes a core runtime API and a code generation tool that generates
individual libraries from [botocore][] service descriptions.

[botocore]: https://github.com/boto/botocore

[![Build Status](https://travis-ci.org/inhabitedtype/ocaml-aws.svg?branch=master)](https://travis-ci.org/inhabitedtype/ocaml-aws)

[API Documentation](http://inhabitedtype.github.io/ocaml-aws/)

## Development

You can install the core library and its dependencies by pinning:

```bash
opam pin add aws .
```

After pinning, you can install to the latest development version by checking
out the commit and running:

```bash
opam update aws
```

Running individual test suites for a Service
``` bash
AWS_ACCESS_KEY_ID=???? \
AWS_SECRET_ACCESS_KEY=???? \
AWS_DEFAULT_REGION=ap-southeast-2 \
dune runtest libraries/ec2/lib_test
```

### Code Generation

To generate service libaries during development and in preparation for release,
first configure the build to enable code generation and run the `gen` target of
the Makefile. Note that the code generation tool has its own set of
dependencies that must be installed in order for the build to succeed.
In addition, the Makefile is written for GNU make.
Some platforms such as OS X and FreeBSD do not have a GNU-compatible make
installed by default. If you get strage error messages at this stage of the
build, check your make. The following commands will configure the build for
code generation and regenerate the libraries from the current definitions:

```bash
# Compile code generator
make gen

# Compile generated libraries
make
```

### Release

To generate a release to opam requires `dune-release` to be installed, then run:

``` bash
make opam-release

```

## Example

Here's how you use the library and EC2 bindings to retrieve a list of regions
from AWS and print them to `stdout`. This example uses the Async runtime, but
the equivalent lwt example is identical in structure.

```ocaml
open Core
open Async
open Aws_ec2

let to_result = function
  | `Ok x    -> Ok x
  | `Error e -> Error (Aws.Error.format Errors.to_string e)

let main () =
  Aws_async.Runtime.run_request
    ~region:"us-east-1" ~access_key:"<...>" ~secret_key:"<...>"
    (module DescribeRegions)
    (Types.DescribeRegionsRequest.make ())
  >>| to_result
  >>| Option.value_exn
  >>|? List.iter ~f:(fun x -> Log.Global.info "%s" x.Types.Region.region_name)
  >>> function
    | Ok ()   -> exit 0
    | Error e -> Log.Global.error "%s" e; exit 1

Scheduler.go_main ~main ()
```

## DEBUGGING

If you get this error when running test or example code
`Fatal error: exception (Failure "No SSL or TLS support compiled into Conduit")`

You need to install the ssl versions of your chosen async framework

`opam install lwt_ssl async_ssl`

## License

BSD3, see LICENSE file for its text.
