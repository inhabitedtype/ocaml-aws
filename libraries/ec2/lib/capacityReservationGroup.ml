open Aws.BaseTypes

type t =
  { group_arn : String.t option
  ; owner_id : String.t option
  }

let make ?group_arn ?owner_id () = { group_arn; owner_id }

let parse xml =
  Some
    { group_arn = Aws.Util.option_bind (Aws.Xml.member "groupArn" xml) String.parse
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ; Aws.Util.option_map v.group_arn (fun f ->
             Aws.Query.Pair ("GroupArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ; Aws.Util.option_map v.group_arn (fun f -> "groupArn", String.to_json f)
       ])

let of_json j =
  { group_arn = Aws.Util.option_map (Aws.Json.lookup j "groupArn") String.of_json
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  }
