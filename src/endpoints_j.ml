(* Auto-generated from "endpoints.atd" *)
[@@@ocaml.warning "-27-32-35-39"]

type service_defaults = Endpoints_t.service_defaults =
  { protocols : string list option
  ; ssl_common_name : string option
  }

type endpoint = Endpoints_t.endpoint = { hostname : string option }

type service = Endpoints_t.service =
  { defaults : service_defaults option
  ; endpoints : (string * endpoint) list
  }

type region = Endpoints_t.region = { description : string }

type partition_defaults = Endpoints_t.partition_defaults =
  { hostname : string option
  ; protocols : string list
  ; signature_versions : string list
  }

type partition = Endpoints_t.partition =
  { defaults : partition_defaults
  ; dns_suffix : string
  ; partition : string
  ; partition_name : string
  ; region_regex : string
  ; regions : (string * region) list
  ; services : (string * service) list
  }

type endpoints = Endpoints_t.endpoints = { partitions : partition list }

let write__2 = Atdgen_runtime.Oj_run.write_list Yojson.Safe.write_string

let string_of__2 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__2 ob x;
  Bi_outbuf.contents ob

let read__2 = Atdgen_runtime.Oj_run.read_list Atdgen_runtime.Oj_run.read_string

let _2_of_string s = read__2 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write__3 = Atdgen_runtime.Oj_run.write_std_option write__2

let string_of__3 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__3 ob x;
  Bi_outbuf.contents ob

let read__3 p lb =
  Yojson.Safe.read_space p lb;
  match Yojson.Safe.start_any_variant p lb with
  | `Edgy_bracket -> (
      match Yojson.Safe.read_ident p lb with
      | "None" ->
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_gt p lb;
          (None : _ option)
      | "Some" ->
          Atdgen_runtime.Oj_run.read_until_field_value p lb;
          let x = read__2 p lb in
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_gt p lb;
          (Some x : _ option)
      | x -> Atdgen_runtime.Oj_run.invalid_variant_tag p x)
  | `Double_quote -> (
      match Yojson.Safe.finish_string p lb with
      | "None" -> (None : _ option)
      | x -> Atdgen_runtime.Oj_run.invalid_variant_tag p x)
  | `Square_bracket -> (
      match Atdgen_runtime.Oj_run.read_string p lb with
      | "Some" ->
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_comma p lb;
          Yojson.Safe.read_space p lb;
          let x = read__2 p lb in
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_rbr p lb;
          (Some x : _ option)
      | x -> Atdgen_runtime.Oj_run.invalid_variant_tag p x)

let _3_of_string s = read__3 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write__1 = Atdgen_runtime.Oj_run.write_nullable Yojson.Safe.write_string

let string_of__1 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__1 ob x;
  Bi_outbuf.contents ob

let read__1 p lb =
  Yojson.Safe.read_space p lb;
  (if Yojson.Safe.read_null_if_possible p lb
   then None
   else Some (Atdgen_runtime.Oj_run.read_string p lb)
    : _ option)

let _1_of_string s = read__1 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write_service_defaults : _ -> service_defaults -> _ =
 fun ob x ->
  Bi_outbuf.add_char ob '{';
  let is_first = ref true in
  (match x.protocols with
  | None -> ()
  | Some x ->
      if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"protocols\":";
      write__2 ob x);
  (match x.ssl_common_name with
  | None -> ()
  | Some x ->
      if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"ssl_common_name\":";
      Yojson.Safe.write_string ob x);
  Bi_outbuf.add_char ob '}'

let string_of_service_defaults ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_service_defaults ob x;
  Bi_outbuf.contents ob

let read_service_defaults p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_lcurl p lb;
  let field_protocols = ref None in
  let field_ssl_common_name = ref None in
  try
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_object_end lb;
    Yojson.Safe.read_space p lb;
    let f s pos len =
      if pos < 0 || len < 0 || pos + len > String.length s
      then invalid_arg "out-of-bounds substring position or length";
      match len with
      | 9 ->
          if String.unsafe_get s pos = 'p'
             && String.unsafe_get s (pos + 1) = 'r'
             && String.unsafe_get s (pos + 2) = 'o'
             && String.unsafe_get s (pos + 3) = 't'
             && String.unsafe_get s (pos + 4) = 'o'
             && String.unsafe_get s (pos + 5) = 'c'
             && String.unsafe_get s (pos + 6) = 'o'
             && String.unsafe_get s (pos + 7) = 'l'
             && String.unsafe_get s (pos + 8) = 's'
          then 0
          else -1
      | 15 ->
          if String.unsafe_get s pos = 's'
             && String.unsafe_get s (pos + 1) = 's'
             && String.unsafe_get s (pos + 2) = 'l'
             && String.unsafe_get s (pos + 3) = '_'
             && String.unsafe_get s (pos + 4) = 'c'
             && String.unsafe_get s (pos + 5) = 'o'
             && String.unsafe_get s (pos + 6) = 'm'
             && String.unsafe_get s (pos + 7) = 'm'
             && String.unsafe_get s (pos + 8) = 'o'
             && String.unsafe_get s (pos + 9) = 'n'
             && String.unsafe_get s (pos + 10) = '_'
             && String.unsafe_get s (pos + 11) = 'n'
             && String.unsafe_get s (pos + 12) = 'a'
             && String.unsafe_get s (pos + 13) = 'm'
             && String.unsafe_get s (pos + 14) = 'e'
          then 1
          else -1
      | _ -> -1
    in
    let i = Yojson.Safe.map_ident p f lb in
    Atdgen_runtime.Oj_run.read_until_field_value p lb;
    (match i with
    | 0 ->
        if not (Yojson.Safe.read_null_if_possible p lb)
        then field_protocols := Some (read__2 p lb)
    | 1 ->
        if not (Yojson.Safe.read_null_if_possible p lb)
        then field_ssl_common_name := Some (Atdgen_runtime.Oj_run.read_string p lb)
    | _ -> Yojson.Safe.skip_json p lb);
    while true do
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_sep p lb;
      Yojson.Safe.read_space p lb;
      let f s pos len =
        if pos < 0 || len < 0 || pos + len > String.length s
        then invalid_arg "out-of-bounds substring position or length";
        match len with
        | 9 ->
            if String.unsafe_get s pos = 'p'
               && String.unsafe_get s (pos + 1) = 'r'
               && String.unsafe_get s (pos + 2) = 'o'
               && String.unsafe_get s (pos + 3) = 't'
               && String.unsafe_get s (pos + 4) = 'o'
               && String.unsafe_get s (pos + 5) = 'c'
               && String.unsafe_get s (pos + 6) = 'o'
               && String.unsafe_get s (pos + 7) = 'l'
               && String.unsafe_get s (pos + 8) = 's'
            then 0
            else -1
        | 15 ->
            if String.unsafe_get s pos = 's'
               && String.unsafe_get s (pos + 1) = 's'
               && String.unsafe_get s (pos + 2) = 'l'
               && String.unsafe_get s (pos + 3) = '_'
               && String.unsafe_get s (pos + 4) = 'c'
               && String.unsafe_get s (pos + 5) = 'o'
               && String.unsafe_get s (pos + 6) = 'm'
               && String.unsafe_get s (pos + 7) = 'm'
               && String.unsafe_get s (pos + 8) = 'o'
               && String.unsafe_get s (pos + 9) = 'n'
               && String.unsafe_get s (pos + 10) = '_'
               && String.unsafe_get s (pos + 11) = 'n'
               && String.unsafe_get s (pos + 12) = 'a'
               && String.unsafe_get s (pos + 13) = 'm'
               && String.unsafe_get s (pos + 14) = 'e'
            then 1
            else -1
        | _ -> -1
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      match i with
      | 0 ->
          if not (Yojson.Safe.read_null_if_possible p lb)
          then field_protocols := Some (read__2 p lb)
      | 1 ->
          if not (Yojson.Safe.read_null_if_possible p lb)
          then field_ssl_common_name := Some (Atdgen_runtime.Oj_run.read_string p lb)
      | _ -> Yojson.Safe.skip_json p lb
    done;
    assert false
  with Yojson.End_of_object ->
    ({ protocols = !field_protocols; ssl_common_name = !field_ssl_common_name }
      : service_defaults)

let service_defaults_of_string s =
  read_service_defaults (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write_endpoint : _ -> endpoint -> _ =
 fun ob x ->
  Bi_outbuf.add_char ob '{';
  let is_first = ref true in
  (match x.hostname with
  | None -> ()
  | Some x ->
      if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"hostname\":";
      Yojson.Safe.write_string ob x);
  Bi_outbuf.add_char ob '}'

let string_of_endpoint ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_endpoint ob x;
  Bi_outbuf.contents ob

let read_endpoint p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_lcurl p lb;
  let field_hostname = ref None in
  try
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_object_end lb;
    Yojson.Safe.read_space p lb;
    let f s pos len =
      if pos < 0 || len < 0 || pos + len > String.length s
      then invalid_arg "out-of-bounds substring position or length";
      if len = 8
         && String.unsafe_get s pos = 'h'
         && String.unsafe_get s (pos + 1) = 'o'
         && String.unsafe_get s (pos + 2) = 's'
         && String.unsafe_get s (pos + 3) = 't'
         && String.unsafe_get s (pos + 4) = 'n'
         && String.unsafe_get s (pos + 5) = 'a'
         && String.unsafe_get s (pos + 6) = 'm'
         && String.unsafe_get s (pos + 7) = 'e'
      then 0
      else -1
    in
    let i = Yojson.Safe.map_ident p f lb in
    Atdgen_runtime.Oj_run.read_until_field_value p lb;
    (match i with
    | 0 ->
        if not (Yojson.Safe.read_null_if_possible p lb)
        then field_hostname := Some (Atdgen_runtime.Oj_run.read_string p lb)
    | _ -> Yojson.Safe.skip_json p lb);
    while true do
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_sep p lb;
      Yojson.Safe.read_space p lb;
      let f s pos len =
        if pos < 0 || len < 0 || pos + len > String.length s
        then invalid_arg "out-of-bounds substring position or length";
        if len = 8
           && String.unsafe_get s pos = 'h'
           && String.unsafe_get s (pos + 1) = 'o'
           && String.unsafe_get s (pos + 2) = 's'
           && String.unsafe_get s (pos + 3) = 't'
           && String.unsafe_get s (pos + 4) = 'n'
           && String.unsafe_get s (pos + 5) = 'a'
           && String.unsafe_get s (pos + 6) = 'm'
           && String.unsafe_get s (pos + 7) = 'e'
        then 0
        else -1
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      match i with
      | 0 ->
          if not (Yojson.Safe.read_null_if_possible p lb)
          then field_hostname := Some (Atdgen_runtime.Oj_run.read_string p lb)
      | _ -> Yojson.Safe.skip_json p lb
    done;
    assert false
  with Yojson.End_of_object -> ({ hostname = !field_hostname } : endpoint)

let endpoint_of_string s =
  read_endpoint (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write__5 =
  Atdgen_runtime.Oj_run.write_assoc_list Yojson.Safe.write_string write_endpoint

let string_of__5 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__5 ob x;
  Bi_outbuf.contents ob

let read__5 =
  Atdgen_runtime.Oj_run.read_assoc_list Atdgen_runtime.Oj_run.read_string read_endpoint

let _5_of_string s = read__5 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write__4 = Atdgen_runtime.Oj_run.write_std_option write_service_defaults

let string_of__4 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__4 ob x;
  Bi_outbuf.contents ob

let read__4 p lb =
  Yojson.Safe.read_space p lb;
  match Yojson.Safe.start_any_variant p lb with
  | `Edgy_bracket -> (
      match Yojson.Safe.read_ident p lb with
      | "None" ->
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_gt p lb;
          (None : _ option)
      | "Some" ->
          Atdgen_runtime.Oj_run.read_until_field_value p lb;
          let x = read_service_defaults p lb in
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_gt p lb;
          (Some x : _ option)
      | x -> Atdgen_runtime.Oj_run.invalid_variant_tag p x)
  | `Double_quote -> (
      match Yojson.Safe.finish_string p lb with
      | "None" -> (None : _ option)
      | x -> Atdgen_runtime.Oj_run.invalid_variant_tag p x)
  | `Square_bracket -> (
      match Atdgen_runtime.Oj_run.read_string p lb with
      | "Some" ->
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_comma p lb;
          Yojson.Safe.read_space p lb;
          let x = read_service_defaults p lb in
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_rbr p lb;
          (Some x : _ option)
      | x -> Atdgen_runtime.Oj_run.invalid_variant_tag p x)

let _4_of_string s = read__4 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write_service : _ -> service -> _ =
 fun ob x ->
  Bi_outbuf.add_char ob '{';
  let is_first = ref true in
  (match x.defaults with
  | None -> ()
  | Some x ->
      if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"defaults\":";
      write_service_defaults ob x);
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"endpoints\":";
  write__5 ob x.endpoints;
  Bi_outbuf.add_char ob '}'

let string_of_service ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_service ob x;
  Bi_outbuf.contents ob

let read_service p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_lcurl p lb;
  let field_defaults = ref None in
  let field_endpoints = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let bits0 = ref 0 in
  try
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_object_end lb;
    Yojson.Safe.read_space p lb;
    let f s pos len =
      if pos < 0 || len < 0 || pos + len > String.length s
      then invalid_arg "out-of-bounds substring position or length";
      match len with
      | 8 ->
          if String.unsafe_get s pos = 'd'
             && String.unsafe_get s (pos + 1) = 'e'
             && String.unsafe_get s (pos + 2) = 'f'
             && String.unsafe_get s (pos + 3) = 'a'
             && String.unsafe_get s (pos + 4) = 'u'
             && String.unsafe_get s (pos + 5) = 'l'
             && String.unsafe_get s (pos + 6) = 't'
             && String.unsafe_get s (pos + 7) = 's'
          then 0
          else -1
      | 9 ->
          if String.unsafe_get s pos = 'e'
             && String.unsafe_get s (pos + 1) = 'n'
             && String.unsafe_get s (pos + 2) = 'd'
             && String.unsafe_get s (pos + 3) = 'p'
             && String.unsafe_get s (pos + 4) = 'o'
             && String.unsafe_get s (pos + 5) = 'i'
             && String.unsafe_get s (pos + 6) = 'n'
             && String.unsafe_get s (pos + 7) = 't'
             && String.unsafe_get s (pos + 8) = 's'
          then 1
          else -1
      | _ -> -1
    in
    let i = Yojson.Safe.map_ident p f lb in
    Atdgen_runtime.Oj_run.read_until_field_value p lb;
    (match i with
    | 0 ->
        if not (Yojson.Safe.read_null_if_possible p lb)
        then field_defaults := Some (read_service_defaults p lb)
    | 1 ->
        field_endpoints := read__5 p lb;
        bits0 := !bits0 lor 0x1
    | _ -> Yojson.Safe.skip_json p lb);
    while true do
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_sep p lb;
      Yojson.Safe.read_space p lb;
      let f s pos len =
        if pos < 0 || len < 0 || pos + len > String.length s
        then invalid_arg "out-of-bounds substring position or length";
        match len with
        | 8 ->
            if String.unsafe_get s pos = 'd'
               && String.unsafe_get s (pos + 1) = 'e'
               && String.unsafe_get s (pos + 2) = 'f'
               && String.unsafe_get s (pos + 3) = 'a'
               && String.unsafe_get s (pos + 4) = 'u'
               && String.unsafe_get s (pos + 5) = 'l'
               && String.unsafe_get s (pos + 6) = 't'
               && String.unsafe_get s (pos + 7) = 's'
            then 0
            else -1
        | 9 ->
            if String.unsafe_get s pos = 'e'
               && String.unsafe_get s (pos + 1) = 'n'
               && String.unsafe_get s (pos + 2) = 'd'
               && String.unsafe_get s (pos + 3) = 'p'
               && String.unsafe_get s (pos + 4) = 'o'
               && String.unsafe_get s (pos + 5) = 'i'
               && String.unsafe_get s (pos + 6) = 'n'
               && String.unsafe_get s (pos + 7) = 't'
               && String.unsafe_get s (pos + 8) = 's'
            then 1
            else -1
        | _ -> -1
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      match i with
      | 0 ->
          if not (Yojson.Safe.read_null_if_possible p lb)
          then field_defaults := Some (read_service_defaults p lb)
      | 1 ->
          field_endpoints := read__5 p lb;
          bits0 := !bits0 lor 0x1
      | _ -> Yojson.Safe.skip_json p lb
    done;
    assert false
  with Yojson.End_of_object ->
    if !bits0 <> 0x1
    then Atdgen_runtime.Oj_run.missing_fields p [| !bits0 |] [| "endpoints" |];
    ({ defaults = !field_defaults; endpoints = !field_endpoints } : service)

let service_of_string s = read_service (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write_region : _ -> region -> _ =
 fun ob x ->
  Bi_outbuf.add_char ob '{';
  let is_first = ref true in
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"description\":";
  Yojson.Safe.write_string ob x.description;
  Bi_outbuf.add_char ob '}'

let string_of_region ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_region ob x;
  Bi_outbuf.contents ob

let read_region p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_lcurl p lb;
  let field_description = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let bits0 = ref 0 in
  try
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_object_end lb;
    Yojson.Safe.read_space p lb;
    let f s pos len =
      if pos < 0 || len < 0 || pos + len > String.length s
      then invalid_arg "out-of-bounds substring position or length";
      if len = 11
         && String.unsafe_get s pos = 'd'
         && String.unsafe_get s (pos + 1) = 'e'
         && String.unsafe_get s (pos + 2) = 's'
         && String.unsafe_get s (pos + 3) = 'c'
         && String.unsafe_get s (pos + 4) = 'r'
         && String.unsafe_get s (pos + 5) = 'i'
         && String.unsafe_get s (pos + 6) = 'p'
         && String.unsafe_get s (pos + 7) = 't'
         && String.unsafe_get s (pos + 8) = 'i'
         && String.unsafe_get s (pos + 9) = 'o'
         && String.unsafe_get s (pos + 10) = 'n'
      then 0
      else -1
    in
    let i = Yojson.Safe.map_ident p f lb in
    Atdgen_runtime.Oj_run.read_until_field_value p lb;
    (match i with
    | 0 ->
        field_description := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x1
    | _ -> Yojson.Safe.skip_json p lb);
    while true do
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_sep p lb;
      Yojson.Safe.read_space p lb;
      let f s pos len =
        if pos < 0 || len < 0 || pos + len > String.length s
        then invalid_arg "out-of-bounds substring position or length";
        if len = 11
           && String.unsafe_get s pos = 'd'
           && String.unsafe_get s (pos + 1) = 'e'
           && String.unsafe_get s (pos + 2) = 's'
           && String.unsafe_get s (pos + 3) = 'c'
           && String.unsafe_get s (pos + 4) = 'r'
           && String.unsafe_get s (pos + 5) = 'i'
           && String.unsafe_get s (pos + 6) = 'p'
           && String.unsafe_get s (pos + 7) = 't'
           && String.unsafe_get s (pos + 8) = 'i'
           && String.unsafe_get s (pos + 9) = 'o'
           && String.unsafe_get s (pos + 10) = 'n'
        then 0
        else -1
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      match i with
      | 0 ->
          field_description := Atdgen_runtime.Oj_run.read_string p lb;
          bits0 := !bits0 lor 0x1
      | _ -> Yojson.Safe.skip_json p lb
    done;
    assert false
  with Yojson.End_of_object ->
    if !bits0 <> 0x1
    then Atdgen_runtime.Oj_run.missing_fields p [| !bits0 |] [| "description" |];
    ({ description = !field_description } : region)

let region_of_string s = read_region (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write_partition_defaults : _ -> partition_defaults -> _ =
 fun ob x ->
  Bi_outbuf.add_char ob '{';
  let is_first = ref true in
  (match x.hostname with
  | None -> ()
  | Some x ->
      if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"hostname\":";
      Yojson.Safe.write_string ob x);
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"protocols\":";
  write__2 ob x.protocols;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"signatureVersions\":";
  write__2 ob x.signature_versions;
  Bi_outbuf.add_char ob '}'

let string_of_partition_defaults ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_partition_defaults ob x;
  Bi_outbuf.contents ob

let read_partition_defaults p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_lcurl p lb;
  let field_hostname = ref None in
  let field_protocols = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_signature_versions = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let bits0 = ref 0 in
  try
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_object_end lb;
    Yojson.Safe.read_space p lb;
    let f s pos len =
      if pos < 0 || len < 0 || pos + len > String.length s
      then invalid_arg "out-of-bounds substring position or length";
      match len with
      | 8 ->
          if String.unsafe_get s pos = 'h'
             && String.unsafe_get s (pos + 1) = 'o'
             && String.unsafe_get s (pos + 2) = 's'
             && String.unsafe_get s (pos + 3) = 't'
             && String.unsafe_get s (pos + 4) = 'n'
             && String.unsafe_get s (pos + 5) = 'a'
             && String.unsafe_get s (pos + 6) = 'm'
             && String.unsafe_get s (pos + 7) = 'e'
          then 0
          else -1
      | 9 ->
          if String.unsafe_get s pos = 'p'
             && String.unsafe_get s (pos + 1) = 'r'
             && String.unsafe_get s (pos + 2) = 'o'
             && String.unsafe_get s (pos + 3) = 't'
             && String.unsafe_get s (pos + 4) = 'o'
             && String.unsafe_get s (pos + 5) = 'c'
             && String.unsafe_get s (pos + 6) = 'o'
             && String.unsafe_get s (pos + 7) = 'l'
             && String.unsafe_get s (pos + 8) = 's'
          then 1
          else -1
      | 17 ->
          if String.unsafe_get s pos = 's'
             && String.unsafe_get s (pos + 1) = 'i'
             && String.unsafe_get s (pos + 2) = 'g'
             && String.unsafe_get s (pos + 3) = 'n'
             && String.unsafe_get s (pos + 4) = 'a'
             && String.unsafe_get s (pos + 5) = 't'
             && String.unsafe_get s (pos + 6) = 'u'
             && String.unsafe_get s (pos + 7) = 'r'
             && String.unsafe_get s (pos + 8) = 'e'
             && String.unsafe_get s (pos + 9) = 'V'
             && String.unsafe_get s (pos + 10) = 'e'
             && String.unsafe_get s (pos + 11) = 'r'
             && String.unsafe_get s (pos + 12) = 's'
             && String.unsafe_get s (pos + 13) = 'i'
             && String.unsafe_get s (pos + 14) = 'o'
             && String.unsafe_get s (pos + 15) = 'n'
             && String.unsafe_get s (pos + 16) = 's'
          then 2
          else -1
      | _ -> -1
    in
    let i = Yojson.Safe.map_ident p f lb in
    Atdgen_runtime.Oj_run.read_until_field_value p lb;
    (match i with
    | 0 ->
        if not (Yojson.Safe.read_null_if_possible p lb)
        then field_hostname := Some (Atdgen_runtime.Oj_run.read_string p lb)
    | 1 ->
        field_protocols := read__2 p lb;
        bits0 := !bits0 lor 0x1
    | 2 ->
        field_signature_versions := read__2 p lb;
        bits0 := !bits0 lor 0x2
    | _ -> Yojson.Safe.skip_json p lb);
    while true do
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_sep p lb;
      Yojson.Safe.read_space p lb;
      let f s pos len =
        if pos < 0 || len < 0 || pos + len > String.length s
        then invalid_arg "out-of-bounds substring position or length";
        match len with
        | 8 ->
            if String.unsafe_get s pos = 'h'
               && String.unsafe_get s (pos + 1) = 'o'
               && String.unsafe_get s (pos + 2) = 's'
               && String.unsafe_get s (pos + 3) = 't'
               && String.unsafe_get s (pos + 4) = 'n'
               && String.unsafe_get s (pos + 5) = 'a'
               && String.unsafe_get s (pos + 6) = 'm'
               && String.unsafe_get s (pos + 7) = 'e'
            then 0
            else -1
        | 9 ->
            if String.unsafe_get s pos = 'p'
               && String.unsafe_get s (pos + 1) = 'r'
               && String.unsafe_get s (pos + 2) = 'o'
               && String.unsafe_get s (pos + 3) = 't'
               && String.unsafe_get s (pos + 4) = 'o'
               && String.unsafe_get s (pos + 5) = 'c'
               && String.unsafe_get s (pos + 6) = 'o'
               && String.unsafe_get s (pos + 7) = 'l'
               && String.unsafe_get s (pos + 8) = 's'
            then 1
            else -1
        | 17 ->
            if String.unsafe_get s pos = 's'
               && String.unsafe_get s (pos + 1) = 'i'
               && String.unsafe_get s (pos + 2) = 'g'
               && String.unsafe_get s (pos + 3) = 'n'
               && String.unsafe_get s (pos + 4) = 'a'
               && String.unsafe_get s (pos + 5) = 't'
               && String.unsafe_get s (pos + 6) = 'u'
               && String.unsafe_get s (pos + 7) = 'r'
               && String.unsafe_get s (pos + 8) = 'e'
               && String.unsafe_get s (pos + 9) = 'V'
               && String.unsafe_get s (pos + 10) = 'e'
               && String.unsafe_get s (pos + 11) = 'r'
               && String.unsafe_get s (pos + 12) = 's'
               && String.unsafe_get s (pos + 13) = 'i'
               && String.unsafe_get s (pos + 14) = 'o'
               && String.unsafe_get s (pos + 15) = 'n'
               && String.unsafe_get s (pos + 16) = 's'
            then 2
            else -1
        | _ -> -1
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      match i with
      | 0 ->
          if not (Yojson.Safe.read_null_if_possible p lb)
          then field_hostname := Some (Atdgen_runtime.Oj_run.read_string p lb)
      | 1 ->
          field_protocols := read__2 p lb;
          bits0 := !bits0 lor 0x1
      | 2 ->
          field_signature_versions := read__2 p lb;
          bits0 := !bits0 lor 0x2
      | _ -> Yojson.Safe.skip_json p lb
    done;
    assert false
  with Yojson.End_of_object ->
    if !bits0 <> 0x3
    then
      Atdgen_runtime.Oj_run.missing_fields
        p
        [| !bits0 |]
        [| "protocols"; "signature_versions" |];
    ({ hostname = !field_hostname
     ; protocols = !field_protocols
     ; signature_versions = !field_signature_versions
     }
      : partition_defaults)

let partition_defaults_of_string s =
  read_partition_defaults (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write__7 =
  Atdgen_runtime.Oj_run.write_assoc_list Yojson.Safe.write_string write_service

let string_of__7 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__7 ob x;
  Bi_outbuf.contents ob

let read__7 =
  Atdgen_runtime.Oj_run.read_assoc_list Atdgen_runtime.Oj_run.read_string read_service

let _7_of_string s = read__7 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write__6 =
  Atdgen_runtime.Oj_run.write_assoc_list Yojson.Safe.write_string write_region

let string_of__6 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__6 ob x;
  Bi_outbuf.contents ob

let read__6 =
  Atdgen_runtime.Oj_run.read_assoc_list Atdgen_runtime.Oj_run.read_string read_region

let _6_of_string s = read__6 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write_partition : _ -> partition -> _ =
 fun ob x ->
  Bi_outbuf.add_char ob '{';
  let is_first = ref true in
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"defaults\":";
  write_partition_defaults ob x.defaults;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"dnsSuffix\":";
  Yojson.Safe.write_string ob x.dns_suffix;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"partition\":";
  Yojson.Safe.write_string ob x.partition;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"partitionName\":";
  Yojson.Safe.write_string ob x.partition_name;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"regionRegex\":";
  Yojson.Safe.write_string ob x.region_regex;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"regions\":";
  write__6 ob x.regions;
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"services\":";
  write__7 ob x.services;
  Bi_outbuf.add_char ob '}'

let string_of_partition ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_partition ob x;
  Bi_outbuf.contents ob

let read_partition p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_lcurl p lb;
  let field_defaults = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_dns_suffix = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_partition = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_partition_name = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_region_regex = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_regions = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let field_services = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let bits0 = ref 0 in
  try
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_object_end lb;
    Yojson.Safe.read_space p lb;
    let f s pos len =
      if pos < 0 || len < 0 || pos + len > String.length s
      then invalid_arg "out-of-bounds substring position or length";
      match len with
      | 7 ->
          if String.unsafe_get s pos = 'r'
             && String.unsafe_get s (pos + 1) = 'e'
             && String.unsafe_get s (pos + 2) = 'g'
             && String.unsafe_get s (pos + 3) = 'i'
             && String.unsafe_get s (pos + 4) = 'o'
             && String.unsafe_get s (pos + 5) = 'n'
             && String.unsafe_get s (pos + 6) = 's'
          then 5
          else -1
      | 8 -> (
          match String.unsafe_get s pos with
          | 'd' ->
              if String.unsafe_get s (pos + 1) = 'e'
                 && String.unsafe_get s (pos + 2) = 'f'
                 && String.unsafe_get s (pos + 3) = 'a'
                 && String.unsafe_get s (pos + 4) = 'u'
                 && String.unsafe_get s (pos + 5) = 'l'
                 && String.unsafe_get s (pos + 6) = 't'
                 && String.unsafe_get s (pos + 7) = 's'
              then 0
              else -1
          | 's' ->
              if String.unsafe_get s (pos + 1) = 'e'
                 && String.unsafe_get s (pos + 2) = 'r'
                 && String.unsafe_get s (pos + 3) = 'v'
                 && String.unsafe_get s (pos + 4) = 'i'
                 && String.unsafe_get s (pos + 5) = 'c'
                 && String.unsafe_get s (pos + 6) = 'e'
                 && String.unsafe_get s (pos + 7) = 's'
              then 6
              else -1
          | _ -> -1)
      | 9 -> (
          match String.unsafe_get s pos with
          | 'd' ->
              if String.unsafe_get s (pos + 1) = 'n'
                 && String.unsafe_get s (pos + 2) = 's'
                 && String.unsafe_get s (pos + 3) = 'S'
                 && String.unsafe_get s (pos + 4) = 'u'
                 && String.unsafe_get s (pos + 5) = 'f'
                 && String.unsafe_get s (pos + 6) = 'f'
                 && String.unsafe_get s (pos + 7) = 'i'
                 && String.unsafe_get s (pos + 8) = 'x'
              then 1
              else -1
          | 'p' ->
              if String.unsafe_get s (pos + 1) = 'a'
                 && String.unsafe_get s (pos + 2) = 'r'
                 && String.unsafe_get s (pos + 3) = 't'
                 && String.unsafe_get s (pos + 4) = 'i'
                 && String.unsafe_get s (pos + 5) = 't'
                 && String.unsafe_get s (pos + 6) = 'i'
                 && String.unsafe_get s (pos + 7) = 'o'
                 && String.unsafe_get s (pos + 8) = 'n'
              then 2
              else -1
          | _ -> -1)
      | 11 ->
          if String.unsafe_get s pos = 'r'
             && String.unsafe_get s (pos + 1) = 'e'
             && String.unsafe_get s (pos + 2) = 'g'
             && String.unsafe_get s (pos + 3) = 'i'
             && String.unsafe_get s (pos + 4) = 'o'
             && String.unsafe_get s (pos + 5) = 'n'
             && String.unsafe_get s (pos + 6) = 'R'
             && String.unsafe_get s (pos + 7) = 'e'
             && String.unsafe_get s (pos + 8) = 'g'
             && String.unsafe_get s (pos + 9) = 'e'
             && String.unsafe_get s (pos + 10) = 'x'
          then 4
          else -1
      | 13 ->
          if String.unsafe_get s pos = 'p'
             && String.unsafe_get s (pos + 1) = 'a'
             && String.unsafe_get s (pos + 2) = 'r'
             && String.unsafe_get s (pos + 3) = 't'
             && String.unsafe_get s (pos + 4) = 'i'
             && String.unsafe_get s (pos + 5) = 't'
             && String.unsafe_get s (pos + 6) = 'i'
             && String.unsafe_get s (pos + 7) = 'o'
             && String.unsafe_get s (pos + 8) = 'n'
             && String.unsafe_get s (pos + 9) = 'N'
             && String.unsafe_get s (pos + 10) = 'a'
             && String.unsafe_get s (pos + 11) = 'm'
             && String.unsafe_get s (pos + 12) = 'e'
          then 3
          else -1
      | _ -> -1
    in
    let i = Yojson.Safe.map_ident p f lb in
    Atdgen_runtime.Oj_run.read_until_field_value p lb;
    (match i with
    | 0 ->
        field_defaults := read_partition_defaults p lb;
        bits0 := !bits0 lor 0x1
    | 1 ->
        field_dns_suffix := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x2
    | 2 ->
        field_partition := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x4
    | 3 ->
        field_partition_name := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x8
    | 4 ->
        field_region_regex := Atdgen_runtime.Oj_run.read_string p lb;
        bits0 := !bits0 lor 0x10
    | 5 ->
        field_regions := read__6 p lb;
        bits0 := !bits0 lor 0x20
    | 6 ->
        field_services := read__7 p lb;
        bits0 := !bits0 lor 0x40
    | _ -> Yojson.Safe.skip_json p lb);
    while true do
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_sep p lb;
      Yojson.Safe.read_space p lb;
      let f s pos len =
        if pos < 0 || len < 0 || pos + len > String.length s
        then invalid_arg "out-of-bounds substring position or length";
        match len with
        | 7 ->
            if String.unsafe_get s pos = 'r'
               && String.unsafe_get s (pos + 1) = 'e'
               && String.unsafe_get s (pos + 2) = 'g'
               && String.unsafe_get s (pos + 3) = 'i'
               && String.unsafe_get s (pos + 4) = 'o'
               && String.unsafe_get s (pos + 5) = 'n'
               && String.unsafe_get s (pos + 6) = 's'
            then 5
            else -1
        | 8 -> (
            match String.unsafe_get s pos with
            | 'd' ->
                if String.unsafe_get s (pos + 1) = 'e'
                   && String.unsafe_get s (pos + 2) = 'f'
                   && String.unsafe_get s (pos + 3) = 'a'
                   && String.unsafe_get s (pos + 4) = 'u'
                   && String.unsafe_get s (pos + 5) = 'l'
                   && String.unsafe_get s (pos + 6) = 't'
                   && String.unsafe_get s (pos + 7) = 's'
                then 0
                else -1
            | 's' ->
                if String.unsafe_get s (pos + 1) = 'e'
                   && String.unsafe_get s (pos + 2) = 'r'
                   && String.unsafe_get s (pos + 3) = 'v'
                   && String.unsafe_get s (pos + 4) = 'i'
                   && String.unsafe_get s (pos + 5) = 'c'
                   && String.unsafe_get s (pos + 6) = 'e'
                   && String.unsafe_get s (pos + 7) = 's'
                then 6
                else -1
            | _ -> -1)
        | 9 -> (
            match String.unsafe_get s pos with
            | 'd' ->
                if String.unsafe_get s (pos + 1) = 'n'
                   && String.unsafe_get s (pos + 2) = 's'
                   && String.unsafe_get s (pos + 3) = 'S'
                   && String.unsafe_get s (pos + 4) = 'u'
                   && String.unsafe_get s (pos + 5) = 'f'
                   && String.unsafe_get s (pos + 6) = 'f'
                   && String.unsafe_get s (pos + 7) = 'i'
                   && String.unsafe_get s (pos + 8) = 'x'
                then 1
                else -1
            | 'p' ->
                if String.unsafe_get s (pos + 1) = 'a'
                   && String.unsafe_get s (pos + 2) = 'r'
                   && String.unsafe_get s (pos + 3) = 't'
                   && String.unsafe_get s (pos + 4) = 'i'
                   && String.unsafe_get s (pos + 5) = 't'
                   && String.unsafe_get s (pos + 6) = 'i'
                   && String.unsafe_get s (pos + 7) = 'o'
                   && String.unsafe_get s (pos + 8) = 'n'
                then 2
                else -1
            | _ -> -1)
        | 11 ->
            if String.unsafe_get s pos = 'r'
               && String.unsafe_get s (pos + 1) = 'e'
               && String.unsafe_get s (pos + 2) = 'g'
               && String.unsafe_get s (pos + 3) = 'i'
               && String.unsafe_get s (pos + 4) = 'o'
               && String.unsafe_get s (pos + 5) = 'n'
               && String.unsafe_get s (pos + 6) = 'R'
               && String.unsafe_get s (pos + 7) = 'e'
               && String.unsafe_get s (pos + 8) = 'g'
               && String.unsafe_get s (pos + 9) = 'e'
               && String.unsafe_get s (pos + 10) = 'x'
            then 4
            else -1
        | 13 ->
            if String.unsafe_get s pos = 'p'
               && String.unsafe_get s (pos + 1) = 'a'
               && String.unsafe_get s (pos + 2) = 'r'
               && String.unsafe_get s (pos + 3) = 't'
               && String.unsafe_get s (pos + 4) = 'i'
               && String.unsafe_get s (pos + 5) = 't'
               && String.unsafe_get s (pos + 6) = 'i'
               && String.unsafe_get s (pos + 7) = 'o'
               && String.unsafe_get s (pos + 8) = 'n'
               && String.unsafe_get s (pos + 9) = 'N'
               && String.unsafe_get s (pos + 10) = 'a'
               && String.unsafe_get s (pos + 11) = 'm'
               && String.unsafe_get s (pos + 12) = 'e'
            then 3
            else -1
        | _ -> -1
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      match i with
      | 0 ->
          field_defaults := read_partition_defaults p lb;
          bits0 := !bits0 lor 0x1
      | 1 ->
          field_dns_suffix := Atdgen_runtime.Oj_run.read_string p lb;
          bits0 := !bits0 lor 0x2
      | 2 ->
          field_partition := Atdgen_runtime.Oj_run.read_string p lb;
          bits0 := !bits0 lor 0x4
      | 3 ->
          field_partition_name := Atdgen_runtime.Oj_run.read_string p lb;
          bits0 := !bits0 lor 0x8
      | 4 ->
          field_region_regex := Atdgen_runtime.Oj_run.read_string p lb;
          bits0 := !bits0 lor 0x10
      | 5 ->
          field_regions := read__6 p lb;
          bits0 := !bits0 lor 0x20
      | 6 ->
          field_services := read__7 p lb;
          bits0 := !bits0 lor 0x40
      | _ -> Yojson.Safe.skip_json p lb
    done;
    assert false
  with Yojson.End_of_object ->
    if !bits0 <> 0x7f
    then
      Atdgen_runtime.Oj_run.missing_fields
        p
        [| !bits0 |]
        [| "defaults"
         ; "dns_suffix"
         ; "partition"
         ; "partition_name"
         ; "region_regex"
         ; "regions"
         ; "services"
        |];
    ({ defaults = !field_defaults
     ; dns_suffix = !field_dns_suffix
     ; partition = !field_partition
     ; partition_name = !field_partition_name
     ; region_regex = !field_region_regex
     ; regions = !field_regions
     ; services = !field_services
     }
      : partition)

let partition_of_string s =
  read_partition (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write__8 = Atdgen_runtime.Oj_run.write_list write_partition

let string_of__8 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__8 ob x;
  Bi_outbuf.contents ob

let read__8 = Atdgen_runtime.Oj_run.read_list read_partition

let _8_of_string s = read__8 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

let write_endpoints : _ -> endpoints -> _ =
 fun ob x ->
  Bi_outbuf.add_char ob '{';
  let is_first = ref true in
  if !is_first then is_first := false else Bi_outbuf.add_char ob ',';
  Bi_outbuf.add_string ob "\"partitions\":";
  write__8 ob x.partitions;
  Bi_outbuf.add_char ob '}'

let string_of_endpoints ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_endpoints ob x;
  Bi_outbuf.contents ob

let read_endpoints p lb =
  Yojson.Safe.read_space p lb;
  Yojson.Safe.read_lcurl p lb;
  let field_partitions = ref (Obj.magic (Sys.opaque_identity 0.0)) in
  let bits0 = ref 0 in
  try
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_object_end lb;
    Yojson.Safe.read_space p lb;
    let f s pos len =
      if pos < 0 || len < 0 || pos + len > String.length s
      then invalid_arg "out-of-bounds substring position or length";
      if len = 10
         && String.unsafe_get s pos = 'p'
         && String.unsafe_get s (pos + 1) = 'a'
         && String.unsafe_get s (pos + 2) = 'r'
         && String.unsafe_get s (pos + 3) = 't'
         && String.unsafe_get s (pos + 4) = 'i'
         && String.unsafe_get s (pos + 5) = 't'
         && String.unsafe_get s (pos + 6) = 'i'
         && String.unsafe_get s (pos + 7) = 'o'
         && String.unsafe_get s (pos + 8) = 'n'
         && String.unsafe_get s (pos + 9) = 's'
      then 0
      else -1
    in
    let i = Yojson.Safe.map_ident p f lb in
    Atdgen_runtime.Oj_run.read_until_field_value p lb;
    (match i with
    | 0 ->
        field_partitions := read__8 p lb;
        bits0 := !bits0 lor 0x1
    | _ -> Yojson.Safe.skip_json p lb);
    while true do
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_sep p lb;
      Yojson.Safe.read_space p lb;
      let f s pos len =
        if pos < 0 || len < 0 || pos + len > String.length s
        then invalid_arg "out-of-bounds substring position or length";
        if len = 10
           && String.unsafe_get s pos = 'p'
           && String.unsafe_get s (pos + 1) = 'a'
           && String.unsafe_get s (pos + 2) = 'r'
           && String.unsafe_get s (pos + 3) = 't'
           && String.unsafe_get s (pos + 4) = 'i'
           && String.unsafe_get s (pos + 5) = 't'
           && String.unsafe_get s (pos + 6) = 'i'
           && String.unsafe_get s (pos + 7) = 'o'
           && String.unsafe_get s (pos + 8) = 'n'
           && String.unsafe_get s (pos + 9) = 's'
        then 0
        else -1
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      match i with
      | 0 ->
          field_partitions := read__8 p lb;
          bits0 := !bits0 lor 0x1
      | _ -> Yojson.Safe.skip_json p lb
    done;
    assert false
  with Yojson.End_of_object ->
    if !bits0 <> 0x1
    then Atdgen_runtime.Oj_run.missing_fields p [| !bits0 |] [| "partitions" |];
    ({ partitions = !field_partitions } : endpoints)

let endpoints_of_string s =
  read_endpoints (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
