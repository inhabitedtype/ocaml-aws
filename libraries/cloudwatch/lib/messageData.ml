open Aws.BaseTypes

type t =
  { code : String.t option
  ; value : String.t option
  }

let make ?code ?value () = { code; value }

let parse xml =
  Some
    { code = Aws.Util.option_bind (Aws.Xml.member "Code" xml) String.parse
    ; value = Aws.Util.option_bind (Aws.Xml.member "Value" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.value (fun f ->
             Aws.Query.Pair ("Value", String.to_query f))
       ; Aws.Util.option_map v.code (fun f -> Aws.Query.Pair ("Code", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.value (fun f -> "Value", String.to_json f)
       ; Aws.Util.option_map v.code (fun f -> "Code", String.to_json f)
       ])

let of_json j =
  { code = Aws.Util.option_map (Aws.Json.lookup j "Code") String.of_json
  ; value = Aws.Util.option_map (Aws.Json.lookup j "Value") String.of_json
  }
