open Aws.BaseTypes

type t =
  { type_ : String.t option
  ; count : Integer.t option
  }

let make ?type_ ?count () = { type_; count }

let parse xml =
  Some
    { type_ = Aws.Util.option_bind (Aws.Xml.member "type" xml) String.parse
    ; count = Aws.Util.option_bind (Aws.Xml.member "count" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.count (fun f ->
             Aws.Query.Pair ("Count", Integer.to_query f))
       ; Aws.Util.option_map v.type_ (fun f -> Aws.Query.Pair ("Type", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.count (fun f -> "count", Integer.to_json f)
       ; Aws.Util.option_map v.type_ (fun f -> "type", String.to_json f)
       ])

let of_json j =
  { type_ = Aws.Util.option_map (Aws.Json.lookup j "type") String.of_json
  ; count = Aws.Util.option_map (Aws.Json.lookup j "count") Integer.of_json
  }
