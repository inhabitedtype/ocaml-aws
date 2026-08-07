open Aws.BaseTypes

type t =
  { arn : String.t option
  ; name : String.t option
  }

let make ?arn ?name () = { arn; name }

let parse xml =
  Some
    { arn = Aws.Util.option_bind (Aws.Xml.member "arn" xml) String.parse
    ; name = Aws.Util.option_bind (Aws.Xml.member "name" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ; Aws.Util.option_map v.arn (fun f -> Aws.Query.Pair ("Arn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.name (fun f -> "name", String.to_json f)
       ; Aws.Util.option_map v.arn (fun f -> "arn", String.to_json f)
       ])

let of_json j =
  { arn = Aws.Util.option_map (Aws.Json.lookup j "arn") String.of_json
  ; name = Aws.Util.option_map (Aws.Json.lookup j "name") String.of_json
  }
