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

module Structure : sig
  type member =
    { name : string
    ; shape : string
    ; loc_name : string option
    ; field_name : string
    ; required : bool
    }
  (** Most shapes are structs (the rest are primitive, lists, maps or
      enums), which have a list of members. The fields are:

      name: the name of the member.
      shape: the name of the shape that this member is.
      loc_name: the name that the member is referenced as within XML. If
        this is not present, then 'name' is used.
      field_name: since 'name' is usually UpperCased, it isn't a
        valid record field name, so we translate it into upper_cased,
        which is what field_name stores.
  *)

  type t = member list
end

module Shape : sig
  (** Shapes are the name that data structures have within the input
      files we have, so we use that for our names. They have a name
      and some contents, which is either a Structure, List, or Enum.

      Note that in the input file, there are also 'primitive' shapes -
      String, Blob, Boolean, Integer, Long, Double, Float, DateTime.
      We provide the implementation of those types in the core `aws`
      library, which means that we only ever need references to them
      via Structure members, and so they do not exist as our
      types. There is a small caveat here: there are many shapes that
      have type 'string' (and maybe other primitives), which is
      distinct from having a Structure with a member of shape
      'String'. We have decided to eliminate these shapes that just
      wrap primitive types, by inlining them away (so if a Structure
      Foo had a member Bar that was shape Baz, and Baz had type
      string, we would eliminate Baz and change Bar to have shape
      String). In all the cases we examined, the extra benefit of
      documentation of the shape name, beyond the member name in the
      containing structure, seemed negligible.

      Lists have the shape name of what they contain, but they can
      also optionally have a loc_name, analogous to above in
      Structure.member.  If it is present, it is the tag that
      individual elements of the list are wrapped in when in XML. If
      it is missing, the tag defaults to 'member'.

  *)

  type contents =
    | Structure of Structure.member list
    | List of string * string option * bool
    | Enum of string list
    | Map of (string * string option) * (string * string option)

  type t =
    { name : string
    ; content : contents
    }

  type parsed = string * string * contents option
  (** We parse all shapes, but after inlining/filtering, the base
      types (Boolean, Double, et) no longer exist, so the bulk of the
      code deals with the [t] above, but we parse into this [parsed]
      type, which is Name, Shape Name, Contents if Structure,List, or
      Enum. *)
end

module Operation : sig
  type t =
    { name : string
    ; http_meth : string
    ; http_uri : string
    ; input_shape : string option
    ; output_shape : string option
    ; output_wrapper : string option
    ; errors : string list
    }
  (** Operations are individual API endpoints. They take an input
      shape, and may produce an output shape. In the generated APIs,
      no output corresponds to a unit return value. The output_wrapper
      exists because certain operations have an additional xml tag
      wrapped around the result. The output_wrapper, if present, is
      that tag name. Finally, the errors is a list of errors that this
      endpoint can return, in addition to the common errors. Note
      that some services (like EC2) don't specify any this way in
      the default input source material.
  *)
end

module Error : sig
  type t =
    { shape_name : string
    ; string_name : string
    ; variant_name : string
    ; http_code : int option
    }
  (** shape_name is the (probably legacy) name that it is referred to
      from operation descriptions.

      string_name is the name as it appears in responses. This sometimes
      includes a '.', which is not a legal identifier character.

      variant_name is the string_name translated into a legal identifier.
      This means that all non alphanum characters are replaced with _, and
      if there is a leading integer an "N" is prefixed.

      http_code is optional because the EC2 descriptions do not specify the
      code that will come with a given error.
  *)

  val compare : t -> t -> int
  (** Ignores the legacy shape name to remove duplicates *)
end
