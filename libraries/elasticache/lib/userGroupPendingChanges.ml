type t =
  { user_ids_to_remove : UserIdList.t
  ; user_ids_to_add : UserIdList.t
  }

let make ?(user_ids_to_remove = []) ?(user_ids_to_add = []) () =
  { user_ids_to_remove; user_ids_to_add }

let parse xml =
  Some
    { user_ids_to_remove =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "UserIdsToRemove" xml) UserIdList.parse)
    ; user_ids_to_add =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "UserIdsToAdd" xml) UserIdList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("UserIdsToAdd.member", UserIdList.to_query v.user_ids_to_add))
       ; Some
           (Aws.Query.Pair
              ("UserIdsToRemove.member", UserIdList.to_query v.user_ids_to_remove))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("UserIdsToAdd", UserIdList.to_json v.user_ids_to_add)
       ; Some ("UserIdsToRemove", UserIdList.to_json v.user_ids_to_remove)
       ])

let of_json j =
  { user_ids_to_remove =
      UserIdList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UserIdsToRemove"))
  ; user_ids_to_add =
      UserIdList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UserIdsToAdd"))
  }
