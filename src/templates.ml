(*----------------------------------------------------------------------------
    Copyright (c) 2016 Inhabited Type LLC.

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    3. Neither the name of the author nor the names of his contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)
let opam ~service_name =
  Printf.sprintf
    {|opam-version: "2.0"
maintainer: "Tim McGilchrist <timmcgil@gmail.com>"
authors: [ "Spiros Eliopoulos <spiros@inhabitedtype.com>"
           "Daniel Patterson <dbp@dbpmail.net>"
           "Tim McGilchrist <timmcgil@gmail.com>"
         ]
synopsis: "Amazon Web Services SDK bindings to %s"
description: "Amazon Web Services SDK bindings to %s"
version: "1.2"
license: "BSD-3-clause"
homepage: "https://github.com/inhabitedtype/ocaml-aws"
dev-repo: "git+https://github.com/inhabitedtype/ocaml-aws.git"
bug-reports: "https://github.com/inhabitedtype/ocaml-aws/issues"
doc: "https://github.com/inhabitedtype/ocaml-aws"
build: [
  ["dune" "subst"] {pinned}
  ["dune" "build" "-p" name "-j" jobs]
]
depends: [
  "ocaml" {>= "4.08"}
  "aws" {= version}
  "dune" {>= "2.7"}
  "ounit" {>= "2.2.4" & with-test}
]
|}
    service_name
    service_name

let dune ~lib_name ~service_name =
  Printf.sprintf
    {|(library
 (name        aws_%s)
 (public_name aws-%s)
 (synopsis "aws-%s")
 (flags (:standard -w -27))
 (libraries aws))
|}
    lib_name
    lib_name
    service_name

let dune_test ~lib_name =
  (* Necessary cause '%' is reserved string in 'sprintf' and I didn't know
     how to escape it.
  *)
  let d = "%{deps}" in
  Printf.sprintf
    {|(executables
 (names test_async test_lwt)
 (flags (:standard -w -27 -w -33))
 (modules test_async test_lwt aws_%s_test)
 (libraries aws aws_%s aws-async aws-lwt
            oUnit yojson
            async cohttp-async
            lwt cohttp-lwt cohttp-lwt-unix))

(rule
 (alias runtest)
 (deps test_async.exe)
 (action (run %s)))

(rule
 (alias runtest)
 (deps test_lwt.exe)
 (action (run %s)))
|}
    lib_name
    lib_name
    d
    d

let test_async ~lib_name =
  Printf.sprintf
    {|open Aws_%s_test

module T = TestSuite(struct
    type 'a m = 'a Async.Deferred.t

    let access_key = Unix.getenv "AWS_ACCESS_KEY_ID_ID"
    let secret_key = Unix.getenv "AWS_SECRET_ACCESS_KEY"
    let region = Unix.getenv "AWS_DEFAULT_REGION"

    let run_request x = Aws_async.Runtime.run_request ~region ~access_key ~secret_key x
    let un_m v = Async.Thread_safe.block_on_async_exn (fun () -> v)
  end)
|}
    lib_name

let test_lwt ~lib_name =
  Printf.sprintf
    {|open Aws_%s_test

module T = TestSuite(struct
    type 'a m = 'a Lwt.t

    let access_key = Unix.getenv "AWS_ACCESS_KEY_ID_ID"
    let secret_key = Unix.getenv "AWS_SECRET_ACCESS_KEY"
    let region = Unix.getenv "AWS_DEFAULT_REGION"

    let run_request x = Aws_lwt.Runtime.run_request ~region ~access_key ~secret_key x
    let un_m = Lwt_main.run
  end)
|}
    lib_name

let service_test ~lib_name =
  let upper_lib_name = String.uppercase_ascii lib_name in
  Printf.sprintf
    {|open OUnit
open Aws_%s

module TestSuite(Runtime : sig
    type 'a m
    val run_request :
      (module Aws.Call with type input = 'input
                           and type output = 'output
                           and type error = 'error)
      -> 'input
      -> [`Ok of 'output | `Error of 'error Aws.Error.t] m
    val un_m : 'a m -> 'a
  end) = struct

  let noop_test () =
    "Noop %s test succeeds"
    @?false

  let test_cases =
    [ "%s noop" >:: noop_test ]

  let rec was_successful =
    function
    | [] -> true
    | RSuccess _::t
    | RSkip _::t ->
      was_successful t
    | RFailure _::_
    | RError _::_
    | RTodo _::_ ->
      false
  let _ =
    let suite = "Tests" >::: test_cases in
    let verbose = ref false in
    let set_verbose _ = verbose := true in
    Arg.parse
      [("-verbose", Arg.Unit set_verbose, "Run the test in verbose mode.");]
      (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
      ("Usage: " ^ Sys.argv.(0) ^ " [-verbose]");
    if not (was_successful (run_test_tt ~verbose:!verbose suite)) then
      exit 1
end
|}
    lib_name
    upper_lib_name
    upper_lib_name
