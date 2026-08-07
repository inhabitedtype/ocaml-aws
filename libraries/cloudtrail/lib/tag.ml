open Aws.BaseTypes

type t =
  { key : String.t
  ; value : String.t option
  }

let make ~key ?value () = { key; value }

let parse xml =
  Some
    { key =
        Aws.Xml.required
          "Key"
          (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse)
    ; value = Aws.Util.option_bind (Aws.Xml.member "Value" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.value (fun f ->
             Aws.Query.Pair ("Value", String.to_query f))
       ; Some (Aws.Query.Pair ("Key", String.to_query v.key))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.value (fun f -> "Value", String.to_json f)
       ; Some ("Key", String.to_json v.key)
       ])

let of_json j =
  { key = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key"))
  ; value = Aws.Util.option_map (Aws.Json.lookup j "Value") String.of_json
  }
