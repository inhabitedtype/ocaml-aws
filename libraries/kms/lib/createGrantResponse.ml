open Aws.BaseTypes

type t =
  { grant_token : String.t option
  ; grant_id : String.t option
  }

let make ?grant_token ?grant_id () = { grant_token; grant_id }

let parse xml =
  Some
    { grant_token = Aws.Util.option_bind (Aws.Xml.member "GrantToken" xml) String.parse
    ; grant_id = Aws.Util.option_bind (Aws.Xml.member "GrantId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.grant_id (fun f ->
             Aws.Query.Pair ("GrantId", String.to_query f))
       ; Aws.Util.option_map v.grant_token (fun f ->
             Aws.Query.Pair ("GrantToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.grant_id (fun f -> "GrantId", String.to_json f)
       ; Aws.Util.option_map v.grant_token (fun f -> "GrantToken", String.to_json f)
       ])

let of_json j =
  { grant_token = Aws.Util.option_map (Aws.Json.lookup j "GrantToken") String.of_json
  ; grant_id = Aws.Util.option_map (Aws.Json.lookup j "GrantId") String.of_json
  }
