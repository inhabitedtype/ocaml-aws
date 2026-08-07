open Aws.BaseTypes

type t =
  { code : Integer.t option
  ; type_ : Integer.t option
  }

let make ?code ?type_ () = { code; type_ }

let parse xml =
  Some
    { code = Aws.Util.option_bind (Aws.Xml.member "code" xml) Integer.parse
    ; type_ = Aws.Util.option_bind (Aws.Xml.member "type" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.type_ (fun f ->
             Aws.Query.Pair ("Type", Integer.to_query f))
       ; Aws.Util.option_map v.code (fun f -> Aws.Query.Pair ("Code", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.type_ (fun f -> "type", Integer.to_json f)
       ; Aws.Util.option_map v.code (fun f -> "code", Integer.to_json f)
       ])

let of_json j =
  { code = Aws.Util.option_map (Aws.Json.lookup j "code") Integer.of_json
  ; type_ = Aws.Util.option_map (Aws.Json.lookup j "type") Integer.of_json
  }
