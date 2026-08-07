open Aws.BaseTypes

type t =
  { security_group_id : String.t option
  ; status : String.t option
  }

let make ?security_group_id ?status () = { security_group_id; status }

let parse xml =
  Some
    { security_group_id =
        Aws.Util.option_bind (Aws.Xml.member "SecurityGroupId" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.security_group_id (fun f ->
             Aws.Query.Pair ("SecurityGroupId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Aws.Util.option_map v.security_group_id (fun f ->
             "SecurityGroupId", String.to_json f)
       ])

let of_json j =
  { security_group_id =
      Aws.Util.option_map (Aws.Json.lookup j "SecurityGroupId") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  }
