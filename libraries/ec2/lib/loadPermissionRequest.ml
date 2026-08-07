open Aws.BaseTypes

type t =
  { group : PermissionGroup.t option
  ; user_id : String.t option
  }

let make ?group ?user_id () = { group; user_id }

let parse xml =
  Some
    { group = Aws.Util.option_bind (Aws.Xml.member "Group" xml) PermissionGroup.parse
    ; user_id = Aws.Util.option_bind (Aws.Xml.member "UserId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.user_id (fun f ->
             Aws.Query.Pair ("UserId", String.to_query f))
       ; Aws.Util.option_map v.group (fun f ->
             Aws.Query.Pair ("Group", PermissionGroup.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.user_id (fun f -> "UserId", String.to_json f)
       ; Aws.Util.option_map v.group (fun f -> "Group", PermissionGroup.to_json f)
       ])

let of_json j =
  { group = Aws.Util.option_map (Aws.Json.lookup j "Group") PermissionGroup.of_json
  ; user_id = Aws.Util.option_map (Aws.Json.lookup j "UserId") String.of_json
  }
