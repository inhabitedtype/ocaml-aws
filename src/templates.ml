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
version: "1.2"
synopsis: "Amazon Web Services SDK bindings to %s"
description: "Amazon Web Services SDK bindings to %s"
maintainer: "Tim McGilchrist <timmcgil@gmail.com>"
authors: [ 
  "Spiros Eliopoulos <spiros@inhabitedtype.com>"
  "Daniel Patterson <dbp@dbpmail.net>"
  "Tim McGilchrist <timmcgil@gmail.com>"
]
license: "BSD-3-clause"
homepage: "https://github.com/inhabitedtype/ocaml-aws"
bug-reports: "https://github.com/inhabitedtype/ocaml-aws/issues"
doc: "https://github.com/inhabitedtype/ocaml-aws"
dev-repo: "git+https://github.com/inhabitedtype/ocaml-aws.git"
depends: [
  "ocaml" {>= "4.08"}
  "aws" {= version}
  "dune" {>= "2.7"}
  "ounit2" {with-test & >= "2.2.4"}
  "async" {with-test & >= "v0.14.0"}
  "cohttp-async" {with-test & >= "2.4.0"}
  "cohttp-lwt" {with-test & >= "2.4.0"}
  "cohttp-lwt-unix" {with-test & >= "2.4.0"}
  "lwt" {with-test & >= "4.0.0"}
  "yojson" {with-test & >= "1.7.0"}
]
build: [
  ["dune" "subst"] {pinned}
  ["dune" "build" "-p" name "-j" jobs]
]|}
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

let dune_test ?optional_libs ~lib_name () =
  (* Necessary cause '%' is reserved string in 'sprintf' and I didn't know
     how to escape it.
  *)
  let optional_libs =
    (match optional_libs with
    | Some libs -> libs
    | None -> [])
    |> Fmt.str "%a" (Fmt.list ~sep:Fmt.sp Fmt.string)
  in
  let d = "%{deps}" in
  Printf.sprintf
    {|(tests
 (names test_async test_lwt)
 (package aws-%s)
 (flags (:standard -w -27 -w -33))
 (modules test_async test_lwt aws_%s_test)
 (libraries aws aws-%s %s aws-async aws-lwt
            ounit2 yojson
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
    lib_name
    optional_libs
    d
    d

let test_async ~lib_name =
  Printf.sprintf
    {|open Aws_%s_test

module T = TestSuite(struct
    type 'a m = 'a Async.Deferred.t
    let run_request = Aws_async.Runtime.run_request
    let un_m v = Async.Thread_safe.block_on_async_exn (fun () -> v)
  end)
|}
    lib_name

let test_lwt ~lib_name =
  Printf.sprintf
    {|open Aws_%s_test

module T = TestSuite(struct
    type 'a m = 'a Lwt.t
    let run_request = Aws_lwt.Runtime.run_request
    let un_m = Lwt_main.run
  end)
|}
    lib_name

let service_test ~lib_name =
  let upper_lib_name = String.uppercase_ascii lib_name in
  Printf.sprintf
    {|open OUnit2
open Aws_%s

type config =
  { access_key : string
  ; secret_key : string
  ; region : string
  }
    
let ( @? ) = assert_bool

module TestSuite(Runtime : sig
    type 'a m
    val run_request :
         region:string
      -> access_key:string
      -> secret_key:string
      -> ?token:string
      -> (module Aws.Call with type input = 'input
                           and type output = 'output
                           and type error = 'error)
      -> 'input
      -> [`Ok of 'output | `Error of 'error Aws.Error.t] m
    val un_m : 'a m -> 'a
  end) = struct

  let noop_test config () =
    "Noop %s test succeeds"
    @?false

  let suite config =
    "Test %s" >::: [ "%s noop" >:: noop_test config ]

  let () =
      let access_key =
        try Some (Unix.getenv "AWS_ACCESS_KEY_ID") with Not_found -> None
      in
      let secret_key =
        try Some (Unix.getenv "AWS_SECRET_ACCESS_KEY") with Not_found -> None
      in
      let region = try Some (Unix.getenv "AWS_DEFAULT_REGION") with Not_found -> None in
      
      match access_key, secret_key, region with
      | Some access_key, Some secret_key, Some region ->
          run_test_tt_main (suite { access_key; secret_key; region })
      | _, _, _ ->
          Printf.eprintf
            "Skipping running tests. Environment variables AWS_ACCESS_KEY_ID, \
             AWS_SECRET_ACCESS_KEY and AWS_DEFAULT_REGION not available. ";
          exit 0    
end
|}
    lib_name
    upper_lib_name
    upper_lib_name
    upper_lib_name
