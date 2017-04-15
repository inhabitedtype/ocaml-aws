# ocaml-aws

ocaml-aws is an Amazon Web Services SDK for OCaml. Its source distribution
includes a core runtime API and a code generation tool that generates
individual libraries from [botocore][] service descriptions.

[botocore]: https://github.com/boto/botocore

[![Build Status](https://travis-ci.org/inhabitedtype/ocaml-aws.svg?branch=master)](https://travis-ci.org/inhabitedtype/ocaml-aws)


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

### Code Generation

To generate service libaries during development and in preparation for release,
first configure the build to enable code generation and run the `gen` target of
the Makefile. Note that the code generation tool has its own set of
dependencies that must be installed in order for the build to succeed. See the
`_oasis` file for details. In addition, the Makefile is written for GNU make.
Some platforms such as OS X and FreeBSD do not have a GNU-compatible make
installed by default. If you get strage error messages at this stage of the
build, check your make. The following commands will configure the build for
code generation and regenrate the libraries from the current definitions:

```bash
./configure --enable-gen
make gen
```

## Example

Here's how you use the library and EC2 bindings to retrieve a list of regions
from AWS and print them to `stdout`.

An example using the `Async` runtime:

```ocaml
open Async.Std
open Core.Std
open Aws_ec2

let to_result = function
  | `Ok x    -> Result.Ok x
  | `Error e -> Result.Error (Aws.Error.format Errors.to_string e)

let main () =
  Aws_async.Runtime.run_request
    ~region:"us-east-1" ~access_key:"<...>" ~secret_key:"<...>"
    (module DescribeRegions)
    (Types.DescribeRegionsRequest.make ())
  >>| to_result >>| Option.value_exn
  >>|? List.iter ~f:(fun x -> Printf.printf "%s\n%!" x.Types.Region.region_name)
  >>> function
    | Result.Ok ()   -> exit 0
    | Result.Error e -> Printf.eprintf "%s\n%!" e; exit 1
;;

Scheduler.go_main ~main ()
```

The equivalent `lwt` example:

```ocaml
let describe_regions () =
  let response_t = Aws_lwt.Runtime.run_request
      ~region:"us-east-1" ~access_key:"<...>" ~secret_key:"<...>"
      (module EC2.DescribeRegions)
      (EC2.Types.DescribeRegionsRequest.make ()) in
  response_t >>=
  function
  | `Error _ -> raise Not_found
  | `Ok x ->
    let regions = x.EC2.Types.DescribeRegionsResult.regions in
    let mappings = List.map (fun x ->
        let region_name = match x.EC2.Types.Region.region_name with
          | None -> "REGION_NAME_NOT_FOUND"
          | Some x -> x in
        let endpoint = match x.EC2.Types.Region.endpoint with
          | None -> "ENDPOINT_NOT_FOUND"
          | Some x -> x in
        (region_name, endpoint)) regions in
    Lwt.return mappings

let () =
  let mappings = Lwt_main.run (describe_regions ()) in
  List.iter (fun (region_name, endpoint) -> Printf.printf "%s -> %s\n%!" region_name endpoint) mappings

```

### FreeBSD

In order to install the library dependencies&mdash;specifically zarith which is
a transitive dependency of nocrypto&mdash;you must first make the following
modifications to your system and environment:

```bash
# zarith asusmes an installation of gcc
sudo ln /usr/bin/cc /usr/local/bin/gcc

# libgmp-associated files are installed in /usr/local, which is not in the
# default search path for clang.
export CLFAGS=-I/usr/local/include
export LDFLAGS=-L/usr/local/lib
```

## License

BSD3, see LICENSE file for its text.
