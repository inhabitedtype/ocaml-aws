open Aws.BaseTypes

type t =
  { user_group_id : String.t
  ; user_ids_to_add : UserIdListInput.t
  ; user_ids_to_remove : UserIdListInput.t
  ; engine : String.t option
  }

let make ~user_group_id ?(user_ids_to_add = []) ?(user_ids_to_remove = []) ?engine () =
  { user_group_id; user_ids_to_add; user_ids_to_remove; engine }

let parse xml =
  Some
    { user_group_id =
        Aws.Xml.required
          "UserGroupId"
          (Aws.Util.option_bind (Aws.Xml.member "UserGroupId" xml) String.parse)
    ; user_ids_to_add =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "UserIdsToAdd" xml) UserIdListInput.parse)
    ; user_ids_to_remove =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "UserIdsToRemove" xml)
             UserIdListInput.parse)
    ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.engine (fun f ->
             Aws.Query.Pair ("Engine", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("UserIdsToRemove.member", UserIdListInput.to_query v.user_ids_to_remove))
       ; Some
           (Aws.Query.Pair
              ("UserIdsToAdd.member", UserIdListInput.to_query v.user_ids_to_add))
       ; Some (Aws.Query.Pair ("UserGroupId", String.to_query v.user_group_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
       ; Some ("UserIdsToRemove", UserIdListInput.to_json v.user_ids_to_remove)
       ; Some ("UserIdsToAdd", UserIdListInput.to_json v.user_ids_to_add)
       ; Some ("UserGroupId", String.to_json v.user_group_id)
       ])

let of_json j =
  { user_group_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UserGroupId"))
  ; user_ids_to_add =
      UserIdListInput.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UserIdsToAdd"))
  ; user_ids_to_remove =
      UserIdListInput.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "UserIdsToRemove"))
  ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
  }
