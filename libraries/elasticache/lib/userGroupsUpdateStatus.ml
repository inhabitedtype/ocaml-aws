type t =
  { user_group_ids_to_add : UserGroupIdList.t
  ; user_group_ids_to_remove : UserGroupIdList.t
  }

let make ?(user_group_ids_to_add = []) ?(user_group_ids_to_remove = []) () =
  { user_group_ids_to_add; user_group_ids_to_remove }

let parse xml =
  Some
    { user_group_ids_to_add =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "UserGroupIdsToAdd" xml)
             UserGroupIdList.parse)
    ; user_group_ids_to_remove =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "UserGroupIdsToRemove" xml)
             UserGroupIdList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "UserGroupIdsToRemove.member"
              , UserGroupIdList.to_query v.user_group_ids_to_remove ))
       ; Some
           (Aws.Query.Pair
              ( "UserGroupIdsToAdd.member"
              , UserGroupIdList.to_query v.user_group_ids_to_add ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("UserGroupIdsToRemove", UserGroupIdList.to_json v.user_group_ids_to_remove)
       ; Some ("UserGroupIdsToAdd", UserGroupIdList.to_json v.user_group_ids_to_add)
       ])

let of_json j =
  { user_group_ids_to_add =
      UserGroupIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "UserGroupIdsToAdd"))
  ; user_group_ids_to_remove =
      UserGroupIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "UserGroupIdsToRemove"))
  }
