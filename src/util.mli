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

module Printing : sig
  val write_all : filename:string -> string -> unit

  val write_structure : string -> Parsetree.structure -> unit

  val write_signature : string -> Parsetree.signature -> unit
end

val to_variant_name : string -> string
(** This translates strings into legal variant identfiers. This is
    used, for example, in creating enum types and exception variants. *)

val to_field_name : string -> string
(** This translates strings into legal field names. *)

module StringTable : Map.S with type key = string

val inline_shapes :
     Structures.Operation.t list
  -> Structures.Shape.parsed StringTable.t
  -> Structures.Shape.t StringTable.t * Structures.Operation.t list
(** NOTE(dbp 2015-01-26): Shapes that just have primitive types
   (boolean, integer, etc) types aren't actually useful (they
   communicate no information, since all of the typing is
   structural). Some string types are `enums`, which we support by translating
   them into actual types, but non-enum string shapes are similarly not
   useful. So, we will both remove references to them in fields (by
   just replacing them with Integer, Boolean, etc), and removing the
   shapes themselves.

   NOTE(dbp 2015-02-04): Another optimization: get rid of structs with no fields.
   Many of these are actually error descriptions, but once we support those, they
   would get parsed before this pass (similarly to how enums don't get inlined
   away as strings).
*)

val filter_map : 'a list -> f:('a -> 'b option) -> 'b list

val option_map : string option -> f:string -> string option

val of_option_exn : 'a option -> 'a
