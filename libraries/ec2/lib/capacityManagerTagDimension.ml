open Aws.BaseTypes

type t =
  { key : String.t option
  ; value : String.t option
  }

let make ?key ?value () = { key; value }

let parse xml =
  Some
    { key = Aws.Util.option_bind (Aws.Xml.member "key" xml) String.parse
    ; value = Aws.Util.option_bind (Aws.Xml.member "value" xml) String.parse
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
       [ Aws.Util.option_map v.value (fun f -> "value", String.to_json f)
       ; Aws.Util.option_map v.key (fun f -> "key", String.to_json f)
       ])

let of_json j =
  { key = Aws.Util.option_map (Aws.Json.lookup j "key") String.of_json
  ; value = Aws.Util.option_map (Aws.Json.lookup j "value") String.of_json
  }
