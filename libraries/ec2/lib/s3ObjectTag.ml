open Aws.BaseTypes

type t =
  { key : String.t option
  ; value : String.t option
  }

let make ?key ?value () = { key; value }

let parse xml =
  Some
    { key = Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse
    ; value = Aws.Util.option_bind (Aws.Xml.member "Value" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.value (fun f ->
             Aws.Query.Pair ("Value", String.to_query f))
       ; Aws.Util.option_map v.key (fun f -> Aws.Query.Pair ("Key", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.value (fun f -> "Value", String.to_json f)
       ; Aws.Util.option_map v.key (fun f -> "Key", String.to_json f)
       ])

let of_json j =
  { key = Aws.Util.option_map (Aws.Json.lookup j "Key") String.of_json
  ; value = Aws.Util.option_map (Aws.Json.lookup j "Value") String.of_json
  }
