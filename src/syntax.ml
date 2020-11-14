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

open Migrate_parsetree
open Ast_404
open Parsetree
open Ast_helper
open Ast_convenience_404
open Asttypes

let strloc txt = { txt; loc = !default_loc }

(* open Module (in .ml) *)
let open_ nm = Str.open_ (Opn.mk (lid nm))

(* open Module (in .mli) *)
let sopen_ nm = Sig.open_ (Opn.mk (lid nm))

(* let open Module in E *)
let letom nm = Exp.open_ Fresh (lid nm)

(* module nm1 = nm2 *)
let modlet nm1 nm2 = Str.module_ (Mb.mk (strloc nm1) (Mod.ident (lid nm2)))

(* nm (as a type) *)
let ty0 nm = Typ.constr (lid nm) []

(* nm2 nm1 (as a type) *)
let ty1 nm1 nm2 = Typ.constr (lid nm1) [ ty0 nm2 ]

(* nm2 nm1 (as a type) *)
let ty2 nm1 nm2 nm3 = Typ.constr (lid nm1) [ ty0 nm2; ty0 nm3 ]

(* type nm = { fs.. } *)
let tyreclet nm fs =
  Str.type_
    Recursive
    [ Type.mk
        ~kind:(Ptype_record (List.map (fun (nm, ty) -> Type.field (strloc nm) ty) fs))
        (strloc nm)
    ]

(* type nm = unit *)
let tyunit nm = Str.type_ Recursive [ Type.mk ~manifest:(ty0 "unit") (strloc nm) ]

(* type nm = ty (in .ml) *)
let tylet nm ty = Str.type_ Recursive [ Type.mk ~manifest:ty (strloc nm) ]

(* type nm = | nm0 of ty0 | ... *)
let tyvariantlet nm variants =
  Str.type_
    Recursive
    [ Type.mk
        ~kind:
          (Ptype_variant
             (List.map
                (fun (cnm, args) ->
                  Type.constructor ~args:(Pcstr_tuple args) (strloc cnm))
                variants))
        (strloc nm)
    ]

(* type nm = ty (in .mli) *)
let stylet nm ty = Sig.type_ Recursive [ Type.mk ~manifest:ty (strloc nm) ]

(* let nm = body *)
let let_ nm body = Str.value Nonrecursive [ Vb.mk (Pat.var (strloc nm)) body ]

(* let nm = value in body *)
let letin nm value body = Exp.let_ Nonrecursive [ Vb.mk (Pat.var (strloc nm)) value ] body

(* fun ~arg -> body *)
let funlab arg body = Exp.fun_ (Labelled arg) None (Pat.var (strloc arg)) body

(* fun ?arg -> body *)
let funopt arg body = Exp.fun_ (Optional arg) None (Pat.var (strloc arg)) body

(* fun ?(arg=exp) -> body *)
let funopt_def exp arg body =
  Exp.fun_ (Optional arg) (Some exp) (Pat.var (strloc arg)) body

(* fun arg -> body *)
let fun_ arg body = Exp.fun_ Nolabel None (Pat.var (strloc arg)) body

(* fun arg1 arg2 -> body *)
let fun2 arg1 arg2 body =
  Exp.fun_
    Nolabel
    None
    (Pat.var (strloc arg1))
    (Exp.fun_ Nolabel None (Pat.var (strloc arg2)) body)

(* fun arg1 arg2 arg3 -> body *)
let fun3 arg1 arg2 arg3 body =
  Exp.fun_
    Nolabel
    None
    (Pat.var (strloc arg1))
    (Exp.fun_
       Nolabel
       None
       (Pat.var (strloc arg2))
       (Exp.fun_ Nolabel None (Pat.var (strloc arg3)) body))

(* fun () -> body *)
let fununit body = Exp.fun_ Nolabel None (Pat.construct (lid "()") None) body

(* { fs .. } (as value) *)
let record fs = Exp.record (List.map (fun (nm, exp) -> lid nm, exp) fs) None

(* i (the identifier) *)
let ident i = Exp.ident (lid i)

(* f a *)
let app1 f a = Exp.apply (ident f) [ Nolabel, a ]

(* f a0 a1 *)
let app2 f a0 a1 = Exp.apply (ident f) [ Nolabel, a0; Nolabel, a1 ]

(* f a0 a1 a2 *)
let app3 f a0 a1 a2 = Exp.apply (ident f) [ Nolabel, a0; Nolabel, a1; Nolabel, a2 ]

(* s (the string literal) *)
let str s = Exp.constant (Pconst_string (s, None))

(* n (the int literal) *)
let int n = Exp.constant (Pconst_integer (string_of_int n, None))

(* (a, b) *)
let pair a b = Exp.tuple [ a; b ]

let tuple = Exp.tuple

let unit () = Exp.tuple []

(* [x; ..] (the list of expressions) *)
let list xs =
  List.fold_left
    (fun rest x -> Exp.construct (lid "::") (Some (pair x rest)))
    (Exp.construct (lid "[]") None)
    xs

let list_expr x rest = Exp.construct (lid "::") (Some (pair x rest))

(* `v *)
let variant v = Exp.variant v None

(* `v a *)
let variant1 v a = Exp.variant v (Some a)

(* module nm = vs *)
let module_ nm vs = Str.module_ (Mb.mk (strloc nm) (Mod.structure vs))

(* try body with _ -> with_ *)
let try_ body with_ = Exp.try_ body [ Exp.case (Pat.any ()) with_ ]

(* try body with Failure msg -> with_ (so msg is bound) *)
let tryfail body with_ =
  Exp.try_
    body
    [ Exp.case (Pat.construct (lid "Failure") (Some (Pat.var (strloc "msg")))) with_ ]

(* try body with Exception msg -> with_ (so msg in bound) *)
let try_msg exc body with_ =
  Exp.try_
    body
    [ Exp.case (Pat.construct (lid exc) (Some (Pat.var (strloc "msg")))) with_ ]

(* include nm with_ (in .mli; where with_ is list of elements
   created with `withy` below) *)
let sinclude_ nm with_ =
  Sig.include_
    { pincl_mod = Mty.with_ (Mty.ident (lid nm)) with_
    ; pincl_loc = !default_loc
    ; pincl_attributes = []
    }

(* with nm0 := nm1 (in .mli; for use in include) *)
let withty _nm0 nm1 = Pwith_typesubst (Type.mk ~manifest:(ty0 nm1) (strloc nm1))

(* if cond then thn else els *)
let ifthen cond thn els = Exp.ifthenelse cond thn (Some els)

(* match exp with | Constructor -> body | Constructor -> body ... *)
let matchvar exp branches =
  Exp.match_
    exp
    (List.map (fun (nm, body) -> Exp.case (Pat.construct (lid nm) None) body) branches)

(* match exp with | "String" -> body ... | _ -> els ... *)
let matchstrs exp branches els =
  Exp.match_
    exp
    (List.map
       (fun (nm, body) -> Exp.case (Pat.constant (Pconst_string (nm, None))) body)
       branches
    @ [ Exp.case (Pat.any ()) els ])

(* match exp with | Some var -> some_body | None -> none_body *)
let matchoption exp some_body none_body =
  Exp.match_
    exp
    [ Exp.case (Pat.construct (lid "Some") (Some (Pat.var (strloc "var")))) some_body
    ; Exp.case (Pat.construct (lid "None") None) none_body
    ]
