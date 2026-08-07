open Aws.BaseTypes

type t =
  { type_ : String.t
  ; count : Integer.t option
  }

let make ~type_ ?count () = { type_; count }

let parse xml =
  Some
    { type_ =
        Aws.Xml.required
          "Type"
          (Aws.Util.option_bind (Aws.Xml.member "Type" xml) String.parse)
    ; count = Aws.Util.option_bind (Aws.Xml.member "Count" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.count (fun f ->
             Aws.Query.Pair ("Count", Integer.to_query f))
       ; Some (Aws.Query.Pair ("Type", String.to_query v.type_))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.count (fun f -> "Count", Integer.to_json f)
       ; Some ("Type", String.to_json v.type_)
       ])

let of_json j =
  { type_ = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Type"))
  ; count = Aws.Util.option_map (Aws.Json.lookup j "Count") Integer.of_json
  }
