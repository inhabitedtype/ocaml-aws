open Aws.BaseTypes

type t =
  { user_group_id : String.t
  ; engine : String.t
  ; user_ids : UserIdListInput.t
  ; tags : TagList.t
  }

let make ~user_group_id ~engine ?(user_ids = []) ?(tags = []) () =
  { user_group_id; engine; user_ids; tags }

let parse xml =
  Some
    { user_group_id =
        Aws.Xml.required
          "UserGroupId"
          (Aws.Util.option_bind (Aws.Xml.member "UserGroupId" xml) String.parse)
    ; engine =
        Aws.Xml.required
          "Engine"
          (Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse)
    ; user_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "UserIds" xml) UserIdListInput.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Some (Aws.Query.Pair ("UserIds.member", UserIdListInput.to_query v.user_ids))
       ; Some (Aws.Query.Pair ("Engine", String.to_query v.engine))
       ; Some (Aws.Query.Pair ("UserGroupId", String.to_query v.user_group_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Tags", TagList.to_json v.tags)
       ; Some ("UserIds", UserIdListInput.to_json v.user_ids)
       ; Some ("Engine", String.to_json v.engine)
       ; Some ("UserGroupId", String.to_json v.user_group_id)
       ])

let of_json j =
  { user_group_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UserGroupId"))
  ; engine = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Engine"))
  ; user_ids =
      UserIdListInput.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UserIds"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  }
