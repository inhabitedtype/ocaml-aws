open Aws.BaseTypes

type t =
  { user_id : String.t option
  ; group : PermissionGroup.t option
  }

let make ?user_id ?group () = { user_id; group }

let parse xml =
  Some
    { user_id = Aws.Util.option_bind (Aws.Xml.member "userId" xml) String.parse
    ; group = Aws.Util.option_bind (Aws.Xml.member "group" xml) PermissionGroup.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.group (fun f ->
             Aws.Query.Pair ("Group", PermissionGroup.to_query f))
       ; Aws.Util.option_map v.user_id (fun f ->
             Aws.Query.Pair ("UserId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.group (fun f -> "group", PermissionGroup.to_json f)
       ; Aws.Util.option_map v.user_id (fun f -> "userId", String.to_json f)
       ])

let of_json j =
  { user_id = Aws.Util.option_map (Aws.Json.lookup j "userId") String.of_json
  ; group = Aws.Util.option_map (Aws.Json.lookup j "group") PermissionGroup.of_json
  }
