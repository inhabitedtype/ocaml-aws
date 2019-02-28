(* Auto-generated from "endpoints.atd" *)
[@@@ocaml.warning "-27-32-35-39"]

type raw_json = Yojson.Safe.json

type constraint_op = Endpoints_t.constraint_op

type constraint_on = Endpoints_t.constraint_on

type constraint_data = Endpoints_t.constraint_data = {
  data: string option list
}

type constraint_ = Endpoints_t.constraint_

type endpoint = Endpoints_t.endpoint = {
  uri: string;
  constraints: constraint_ list option
}

type endpoints = Endpoints_t.endpoints

let write_raw_json = (
  Yojson.Safe.write_json
)
let string_of_raw_json ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_raw_json ob x;
  Bi_outbuf.contents ob
let read_raw_json = (
  Yojson.Safe.read_json
)
let raw_json_of_string s =
  read_raw_json (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_constraint_op = (
  fun ob x ->
    match x with
      | `STARTS_WITH -> Bi_outbuf.add_string ob "\"startsWith\""
      | `NOT_EQUALS -> Bi_outbuf.add_string ob "\"notEquals\""
      | `EQUALS -> Bi_outbuf.add_string ob "\"equals\""
      | `ONE_OF -> Bi_outbuf.add_string ob "\"oneOf\""
      | `NOT_STARTS_WITH -> Bi_outbuf.add_string ob "\"notStartsWith\""
)
let string_of_constraint_op ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_constraint_op ob x;
  Bi_outbuf.contents ob
let read_constraint_op = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "startsWith" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `STARTS_WITH
            | "notEquals" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `NOT_EQUALS
            | "equals" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `EQUALS
            | "oneOf" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `ONE_OF
            | "notStartsWith" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `NOT_STARTS_WITH
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | "startsWith" ->
              `STARTS_WITH
            | "notEquals" ->
              `NOT_EQUALS
            | "equals" ->
              `EQUALS
            | "oneOf" ->
              `ONE_OF
            | "notStartsWith" ->
              `NOT_STARTS_WITH
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
let constraint_op_of_string s =
  read_constraint_op (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_constraint_on = (
  fun ob x ->
    match x with
      | `REGION -> Bi_outbuf.add_string ob "\"region\""
)
let string_of_constraint_on ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_constraint_on ob x;
  Bi_outbuf.contents ob
let read_constraint_on = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          match Yojson.Safe.read_ident p lb with
            | "region" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              `REGION
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | "region" ->
              `REGION
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
let constraint_on_of_string s =
  read_constraint_on (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__1 = (
  Atdgen_runtime.Oj_run.write_nullable (
    Yojson.Safe.write_string
  )
)
let string_of__1 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__1 ob x;
  Bi_outbuf.contents ob
let read__1 = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    (if Yojson.Safe.read_null_if_possible p lb then None
    else Some ((
      Atdgen_runtime.Oj_run.read_string
    ) p lb) : _ option)
)
let _1_of_string s =
  read__1 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__2 = (
  Atdgen_runtime.Oj_run.write_list (
    write__1
  )
)
let string_of__2 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__2 ob x;
  Bi_outbuf.contents ob
let read__2 = (
  Atdgen_runtime.Oj_run.read_list (
    read__1
  )
)
let _2_of_string s =
  read__2 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_constraint_data : _ -> constraint_data -> _ = (
  Atdgen_runtime.Oj_run.write_with_adapter Constraint_adapter.StringList.restore (
    fun ob x ->
      Bi_outbuf.add_char ob '{';
      let is_first = ref true in
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"data\":";
      (
        write__2
      )
        ob x.data;
      Bi_outbuf.add_char ob '}';
  )
)
let string_of_constraint_data ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_constraint_data ob x;
  Bi_outbuf.contents ob
let read_constraint_data = (
  Atdgen_runtime.Oj_run.read_with_adapter Constraint_adapter.StringList.normalize (
    fun p lb ->
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_lcurl p lb;
      let field_data = ref (Obj.magic (Sys.opaque_identity 0.0)) in
      let bits0 = ref 0 in
      try
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_end lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            if len = 4 && String.unsafe_get s pos = 'd' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'a' then (
              0
            )
            else (
              -1
            )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_data := (
                (
                  read__2
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
        while true do
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_object_sep p lb;
          Yojson.Safe.read_space p lb;
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              if len = 4 && String.unsafe_get s pos = 'd' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'a' then (
                0
              )
              else (
                -1
              )
          in
          let i = Yojson.Safe.map_ident p f lb in
          Atdgen_runtime.Oj_run.read_until_field_value p lb;
          (
            match i with
              | 0 ->
                field_data := (
                  (
                    read__2
                  ) p lb
                );
                bits0 := !bits0 lor 0x1;
              | _ -> (
                  Yojson.Safe.skip_json p lb
                )
          );
        done;
        assert false;
      with Yojson.End_of_object -> (
          if !bits0 <> 0x1 then Atdgen_runtime.Oj_run.missing_fields p [| !bits0 |] [| "data" |];
          (
            {
              data = !field_data;
            }
           : constraint_data)
        )
  )
)
let constraint_data_of_string s =
  read_constraint_data (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_constraint_ = (
  fun ob x ->
    Bi_outbuf.add_char ob '[';
    (let x, _, _ = x in
    (
      write_constraint_on
    ) ob x
    );
    Bi_outbuf.add_char ob ',';
    (let _, x, _ = x in
    (
      write_constraint_op
    ) ob x
    );
    Bi_outbuf.add_char ob ',';
    (let _, _, x = x in
    (
      write_constraint_data
    ) ob x
    );
    Bi_outbuf.add_char ob ']';
)
let string_of_constraint_ ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_constraint_ ob x;
  Bi_outbuf.contents ob
let read_constraint_ = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    let std_tuple = Yojson.Safe.start_any_tuple p lb in
    let len = ref 0 in
    let end_of_tuple = ref false in
    (try
      let x0 =
        let x =
          (
            read_constraint_on
          ) p lb
        in
        incr len;
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        x
      in
      let x1 =
        let x =
          (
            read_constraint_op
          ) p lb
        in
        incr len;
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        x
      in
      let x2 =
        let x =
          (
            read_constraint_data
          ) p lb
        in
        incr len;
        (try
          Yojson.Safe.read_space p lb;
          Yojson.Safe.read_tuple_sep2 p std_tuple lb;
        with Yojson.End_of_tuple -> end_of_tuple := true);
        x
      in
      if not !end_of_tuple then (
        try
          while true do
            Yojson.Safe.skip_json p lb;
            Yojson.Safe.read_space p lb;
            Yojson.Safe.read_tuple_sep2 p std_tuple lb;
          done
        with Yojson.End_of_tuple -> ()
      );
      (x0, x1, x2)
    with Yojson.End_of_tuple ->
      Atdgen_runtime.Oj_run.missing_tuple_fields p !len [ 0; 1; 2 ]);
)
let constraint__of_string s =
  read_constraint_ (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__3 = (
  Atdgen_runtime.Oj_run.write_list (
    write_constraint_
  )
)
let string_of__3 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__3 ob x;
  Bi_outbuf.contents ob
let read__3 = (
  Atdgen_runtime.Oj_run.read_list (
    read_constraint_
  )
)
let _3_of_string s =
  read__3 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__4 = (
  Atdgen_runtime.Oj_run.write_std_option (
    write__3
  )
)
let string_of__4 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__4 ob x;
  Bi_outbuf.contents ob
let read__4 = (
  fun p lb ->
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
              let x = (
                  read__3
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Some x : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Double_quote -> (
          match Yojson.Safe.finish_string p lb with
            | "None" ->
              (None : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
      | `Square_bracket -> (
          match Atdgen_runtime.Oj_run.read_string p lb with
            | "Some" ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  read__3
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Some x : _ option)
            | x ->
              Atdgen_runtime.Oj_run.invalid_variant_tag p x
        )
)
let _4_of_string s =
  read__4 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_endpoint : _ -> endpoint -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"uri\":";
    (
      Yojson.Safe.write_string
    )
      ob x.uri;
    (match x.constraints with None -> () | Some x ->
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"constraints\":";
      (
        write__3
      )
        ob x;
    );
    Bi_outbuf.add_char ob '}';
)
let string_of_endpoint ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_endpoint ob x;
  Bi_outbuf.contents ob
let read_endpoint = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_uri = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_constraints = ref (None) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 3 -> (
                if String.unsafe_get s pos = 'u' && String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'i' then (
                  0
                )
                else (
                  -1
                )
              )
            | 11 -> (
                if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = 'a' && String.unsafe_get s (pos+7) = 'i' && String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = 't' && String.unsafe_get s (pos+10) = 's' then (
                  1
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Atdgen_runtime.Oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_uri := (
              (
                Atdgen_runtime.Oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_constraints := (
                Some (
                  (
                    read__3
                  ) p lb
                )
              );
            )
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 3 -> (
                  if String.unsafe_get s pos = 'u' && String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'i' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 11 -> (
                  if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = 'a' && String.unsafe_get s (pos+7) = 'i' && String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = 't' && String.unsafe_get s (pos+10) = 's' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Atdgen_runtime.Oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_uri := (
                (
                  Atdgen_runtime.Oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_constraints := (
                  Some (
                    (
                      read__3
                    ) p lb
                  )
                );
              )
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x1 then Atdgen_runtime.Oj_run.missing_fields p [| !bits0 |] [| "uri" |];
        (
          {
            uri = !field_uri;
            constraints = !field_constraints;
          }
         : endpoint)
      )
)
let endpoint_of_string s =
  read_endpoint (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__5 = (
  Atdgen_runtime.Oj_run.write_list (
    write_endpoint
  )
)
let string_of__5 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__5 ob x;
  Bi_outbuf.contents ob
let read__5 = (
  Atdgen_runtime.Oj_run.read_list (
    read_endpoint
  )
)
let _5_of_string s =
  read__5 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__6 = (
  Atdgen_runtime.Oj_run.write_assoc_list (
    Yojson.Safe.write_string
  ) (
    write__5
  )
)
let string_of__6 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__6 ob x;
  Bi_outbuf.contents ob
let read__6 = (
  Atdgen_runtime.Oj_run.read_assoc_list (
    Atdgen_runtime.Oj_run.read_string
  ) (
    read__5
  )
)
let _6_of_string s =
  read__6 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_endpoints = (
  write__6
)
let string_of_endpoints ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_endpoints ob x;
  Bi_outbuf.contents ob
let read_endpoints = (
  read__6
)
let endpoints_of_string s =
  read_endpoints (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
