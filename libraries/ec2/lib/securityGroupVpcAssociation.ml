open Aws.BaseTypes

type t =
  { group_id : String.t option
  ; vpc_id : String.t option
  ; vpc_owner_id : String.t option
  ; state : SecurityGroupVpcAssociationState.t option
  ; state_reason : String.t option
  ; group_owner_id : String.t option
  }

let make ?group_id ?vpc_id ?vpc_owner_id ?state ?state_reason ?group_owner_id () =
  { group_id; vpc_id; vpc_owner_id; state; state_reason; group_owner_id }

let parse xml =
  Some
    { group_id = Aws.Util.option_bind (Aws.Xml.member "groupId" xml) String.parse
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse
    ; vpc_owner_id = Aws.Util.option_bind (Aws.Xml.member "vpcOwnerId" xml) String.parse
    ; state =
        Aws.Util.option_bind
          (Aws.Xml.member "state" xml)
          SecurityGroupVpcAssociationState.parse
    ; state_reason = Aws.Util.option_bind (Aws.Xml.member "stateReason" xml) String.parse
    ; group_owner_id =
        Aws.Util.option_bind (Aws.Xml.member "groupOwnerId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.group_owner_id (fun f ->
             Aws.Query.Pair ("GroupOwnerId", String.to_query f))
       ; Aws.Util.option_map v.state_reason (fun f ->
             Aws.Query.Pair ("StateReason", String.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", SecurityGroupVpcAssociationState.to_query f))
       ; Aws.Util.option_map v.vpc_owner_id (fun f ->
             Aws.Query.Pair ("VpcOwnerId", String.to_query f))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Aws.Util.option_map v.group_id (fun f ->
             Aws.Query.Pair ("GroupId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.group_owner_id (fun f -> "groupOwnerId", String.to_json f)
       ; Aws.Util.option_map v.state_reason (fun f -> "stateReason", String.to_json f)
       ; Aws.Util.option_map v.state (fun f ->
             "state", SecurityGroupVpcAssociationState.to_json f)
       ; Aws.Util.option_map v.vpc_owner_id (fun f -> "vpcOwnerId", String.to_json f)
       ; Aws.Util.option_map v.vpc_id (fun f -> "vpcId", String.to_json f)
       ; Aws.Util.option_map v.group_id (fun f -> "groupId", String.to_json f)
       ])

let of_json j =
  { group_id = Aws.Util.option_map (Aws.Json.lookup j "groupId") String.of_json
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "vpcId") String.of_json
  ; vpc_owner_id = Aws.Util.option_map (Aws.Json.lookup j "vpcOwnerId") String.of_json
  ; state =
      Aws.Util.option_map
        (Aws.Json.lookup j "state")
        SecurityGroupVpcAssociationState.of_json
  ; state_reason = Aws.Util.option_map (Aws.Json.lookup j "stateReason") String.of_json
  ; group_owner_id = Aws.Util.option_map (Aws.Json.lookup j "groupOwnerId") String.of_json
  }
