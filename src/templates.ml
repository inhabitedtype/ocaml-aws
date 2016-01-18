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

let oasis append service full_name modules =
  Printf.sprintf "OASISFormat: 0.4
Name:        aws_%s
Version:     0.1.0
Synopsis:    %s
Authors:     Spiros Eliopoulos <spiros@inhabitedtype.com>, Daniel Patterson <dbp@dbpmail.net>
Maintainers: Spiros Eliopoulos <spiros@inhabitedtype.com>
Homepage:    https://github.com/inhabitedtype/ocaml-aws
Copyrights:  (C) 2016 Inhabited Type LLC
License:     BSD-3-Clause
Plugins:     META (0.4), DevFiles (0.4)
BuildTools: ocamlbuild

Library aws_%s
  Path:         lib
  Findlibname:  aws_%s
  Pack:         true
  Modules:      Types, Errors, %s
  BuildDepends: aws

Document aws_%s
  Title:                aws_%s Docs
  Type:                 ocamlbuild (0.4)
  BuildTools+:          ocamldoc
  Install:              true
  XOCamlbuildPath:      lib
  XOCamlbuildLibraries: aws_%s

%s" service full_name service service (String.concat ", " modules) service service service
    append
