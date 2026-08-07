open Aws.BaseTypes

type t =
  { user_groups : UserGroupList.t
  ; marker : String.t option
  }

let make ?(user_groups = []) ?marker () = { user_groups; marker }

let parse xml =
  Some
    { user_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "UserGroups" xml) UserGroupList.parse)
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Some (Aws.Query.Pair ("UserGroups.member", UserGroupList.to_query v.user_groups))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Some ("UserGroups", UserGroupList.to_json v.user_groups)
       ])

let of_json j =
  { user_groups =
      UserGroupList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UserGroups"))
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  }
