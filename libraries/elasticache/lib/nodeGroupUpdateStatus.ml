open Aws.BaseTypes

type t =
  { node_group_id : String.t option
  ; node_group_member_update_status : NodeGroupMemberUpdateStatusList.t
  }

let make ?node_group_id ?(node_group_member_update_status = []) () =
  { node_group_id; node_group_member_update_status }

let parse xml =
  Some
    { node_group_id = Aws.Util.option_bind (Aws.Xml.member "NodeGroupId" xml) String.parse
    ; node_group_member_update_status =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "NodeGroupMemberUpdateStatus" xml)
             NodeGroupMemberUpdateStatusList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "NodeGroupMemberUpdateStatus.member"
              , NodeGroupMemberUpdateStatusList.to_query v.node_group_member_update_status
              ))
       ; Aws.Util.option_map v.node_group_id (fun f ->
             Aws.Query.Pair ("NodeGroupId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "NodeGroupMemberUpdateStatus"
           , NodeGroupMemberUpdateStatusList.to_json v.node_group_member_update_status )
       ; Aws.Util.option_map v.node_group_id (fun f -> "NodeGroupId", String.to_json f)
       ])

let of_json j =
  { node_group_id = Aws.Util.option_map (Aws.Json.lookup j "NodeGroupId") String.of_json
  ; node_group_member_update_status =
      NodeGroupMemberUpdateStatusList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "NodeGroupMemberUpdateStatus"))
  }
