open Aws.BaseTypes

type t = { user_group_id : String.t }

let make ~user_group_id () = { user_group_id }

let parse xml =
  Some
    { user_group_id =
        Aws.Xml.required
          "UserGroupId"
          (Aws.Util.option_bind (Aws.Xml.member "UserGroupId" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("UserGroupId", String.to_query v.user_group_id)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt [ Some ("UserGroupId", String.to_json v.user_group_id) ])

let of_json j =
  { user_group_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UserGroupId"))
  }
