open Aws.BaseTypes

type t =
  { key_id : String.t option
  ; key_arn : String.t option
  }

let make ?key_id ?key_arn () = { key_id; key_arn }

let parse xml =
  Some
    { key_id = Aws.Util.option_bind (Aws.Xml.member "KeyId" xml) String.parse
    ; key_arn = Aws.Util.option_bind (Aws.Xml.member "KeyArn" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.key_arn (fun f ->
             Aws.Query.Pair ("KeyArn", String.to_query f))
       ; Aws.Util.option_map v.key_id (fun f ->
             Aws.Query.Pair ("KeyId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.key_arn (fun f -> "KeyArn", String.to_json f)
       ; Aws.Util.option_map v.key_id (fun f -> "KeyId", String.to_json f)
       ])

let of_json j =
  { key_id = Aws.Util.option_map (Aws.Json.lookup j "KeyId") String.of_json
  ; key_arn = Aws.Util.option_map (Aws.Json.lookup j "KeyArn") String.of_json
  }
