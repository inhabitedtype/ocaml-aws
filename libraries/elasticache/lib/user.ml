open Aws.BaseTypes

type t =
  { user_id : String.t option
  ; user_name : String.t option
  ; status : String.t option
  ; engine : String.t option
  ; minimum_engine_version : String.t option
  ; access_string : String.t option
  ; user_group_ids : UserGroupIdList.t
  ; authentication : Authentication.t option
  ; a_r_n : String.t option
  }

let make
    ?user_id
    ?user_name
    ?status
    ?engine
    ?minimum_engine_version
    ?access_string
    ?(user_group_ids = [])
    ?authentication
    ?a_r_n
    () =
  { user_id
  ; user_name
  ; status
  ; engine
  ; minimum_engine_version
  ; access_string
  ; user_group_ids
  ; authentication
  ; a_r_n
  }

let parse xml =
  Some
    { user_id = Aws.Util.option_bind (Aws.Xml.member "UserId" xml) String.parse
    ; user_name = Aws.Util.option_bind (Aws.Xml.member "UserName" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
    ; minimum_engine_version =
        Aws.Util.option_bind (Aws.Xml.member "MinimumEngineVersion" xml) String.parse
    ; access_string =
        Aws.Util.option_bind (Aws.Xml.member "AccessString" xml) String.parse
    ; user_group_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "UserGroupIds" xml) UserGroupIdList.parse)
    ; authentication =
        Aws.Util.option_bind (Aws.Xml.member "Authentication" xml) Authentication.parse
    ; a_r_n = Aws.Util.option_bind (Aws.Xml.member "ARN" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.a_r_n (fun f -> Aws.Query.Pair ("ARN", String.to_query f))
       ; Aws.Util.option_map v.authentication (fun f ->
             Aws.Query.Pair ("Authentication", Authentication.to_query f))
       ; Some
           (Aws.Query.Pair
              ("UserGroupIds.member", UserGroupIdList.to_query v.user_group_ids))
       ; Aws.Util.option_map v.access_string (fun f ->
             Aws.Query.Pair ("AccessString", String.to_query f))
       ; Aws.Util.option_map v.minimum_engine_version (fun f ->
             Aws.Query.Pair ("MinimumEngineVersion", String.to_query f))
       ; Aws.Util.option_map v.engine (fun f ->
             Aws.Query.Pair ("Engine", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.user_name (fun f ->
             Aws.Query.Pair ("UserName", String.to_query f))
       ; Aws.Util.option_map v.user_id (fun f ->
             Aws.Query.Pair ("UserId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.a_r_n (fun f -> "ARN", String.to_json f)
       ; Aws.Util.option_map v.authentication (fun f ->
             "Authentication", Authentication.to_json f)
       ; Some ("UserGroupIds", UserGroupIdList.to_json v.user_group_ids)
       ; Aws.Util.option_map v.access_string (fun f -> "AccessString", String.to_json f)
       ; Aws.Util.option_map v.minimum_engine_version (fun f ->
             "MinimumEngineVersion", String.to_json f)
       ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Aws.Util.option_map v.user_name (fun f -> "UserName", String.to_json f)
       ; Aws.Util.option_map v.user_id (fun f -> "UserId", String.to_json f)
       ])

let of_json j =
  { user_id = Aws.Util.option_map (Aws.Json.lookup j "UserId") String.of_json
  ; user_name = Aws.Util.option_map (Aws.Json.lookup j "UserName") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
  ; minimum_engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "MinimumEngineVersion") String.of_json
  ; access_string = Aws.Util.option_map (Aws.Json.lookup j "AccessString") String.of_json
  ; user_group_ids =
      UserGroupIdList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UserGroupIds"))
  ; authentication =
      Aws.Util.option_map (Aws.Json.lookup j "Authentication") Authentication.of_json
  ; a_r_n = Aws.Util.option_map (Aws.Json.lookup j "ARN") String.of_json
  }
