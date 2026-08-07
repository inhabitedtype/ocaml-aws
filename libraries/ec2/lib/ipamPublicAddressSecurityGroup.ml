open Aws.BaseTypes

type t =
  { group_name : String.t option
  ; group_id : String.t option
  }

let make ?group_name ?group_id () = { group_name; group_id }

let parse xml =
  Some
    { group_name = Aws.Util.option_bind (Aws.Xml.member "groupName" xml) String.parse
    ; group_id = Aws.Util.option_bind (Aws.Xml.member "groupId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.group_id (fun f ->
             Aws.Query.Pair ("GroupId", String.to_query f))
       ; Aws.Util.option_map v.group_name (fun f ->
             Aws.Query.Pair ("GroupName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.group_id (fun f -> "groupId", String.to_json f)
       ; Aws.Util.option_map v.group_name (fun f -> "groupName", String.to_json f)
       ])

let of_json j =
  { group_name = Aws.Util.option_map (Aws.Json.lookup j "groupName") String.of_json
  ; group_id = Aws.Util.option_map (Aws.Json.lookup j "groupId") String.of_json
  }
