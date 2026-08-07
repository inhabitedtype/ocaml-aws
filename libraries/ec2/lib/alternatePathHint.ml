open Aws.BaseTypes

type t =
  { component_id : String.t option
  ; component_arn : String.t option
  }

let make ?component_id ?component_arn () = { component_id; component_arn }

let parse xml =
  Some
    { component_id = Aws.Util.option_bind (Aws.Xml.member "componentId" xml) String.parse
    ; component_arn =
        Aws.Util.option_bind (Aws.Xml.member "componentArn" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.component_arn (fun f ->
             Aws.Query.Pair ("ComponentArn", String.to_query f))
       ; Aws.Util.option_map v.component_id (fun f ->
             Aws.Query.Pair ("ComponentId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.component_arn (fun f -> "componentArn", String.to_json f)
       ; Aws.Util.option_map v.component_id (fun f -> "componentId", String.to_json f)
       ])

let of_json j =
  { component_id = Aws.Util.option_map (Aws.Json.lookup j "componentId") String.of_json
  ; component_arn = Aws.Util.option_map (Aws.Json.lookup j "componentArn") String.of_json
  }
