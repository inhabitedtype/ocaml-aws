open Aws.BaseTypes

type t =
  { description : String.t option
  ; user_id : String.t option
  ; group_name : String.t option
  ; group_id : String.t option
  ; vpc_id : String.t option
  ; vpc_peering_connection_id : String.t option
  ; peering_status : String.t option
  }

let make
    ?description
    ?user_id
    ?group_name
    ?group_id
    ?vpc_id
    ?vpc_peering_connection_id
    ?peering_status
    () =
  { description
  ; user_id
  ; group_name
  ; group_id
  ; vpc_id
  ; vpc_peering_connection_id
  ; peering_status
  }

let parse xml =
  Some
    { description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; user_id = Aws.Util.option_bind (Aws.Xml.member "userId" xml) String.parse
    ; group_name = Aws.Util.option_bind (Aws.Xml.member "groupName" xml) String.parse
    ; group_id = Aws.Util.option_bind (Aws.Xml.member "groupId" xml) String.parse
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse
    ; vpc_peering_connection_id =
        Aws.Util.option_bind (Aws.Xml.member "vpcPeeringConnectionId" xml) String.parse
    ; peering_status =
        Aws.Util.option_bind (Aws.Xml.member "peeringStatus" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.peering_status (fun f ->
             Aws.Query.Pair ("PeeringStatus", String.to_query f))
       ; Aws.Util.option_map v.vpc_peering_connection_id (fun f ->
             Aws.Query.Pair ("VpcPeeringConnectionId", String.to_query f))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Aws.Util.option_map v.group_id (fun f ->
             Aws.Query.Pair ("GroupId", String.to_query f))
       ; Aws.Util.option_map v.group_name (fun f ->
             Aws.Query.Pair ("GroupName", String.to_query f))
       ; Aws.Util.option_map v.user_id (fun f ->
             Aws.Query.Pair ("UserId", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.peering_status (fun f -> "peeringStatus", String.to_json f)
       ; Aws.Util.option_map v.vpc_peering_connection_id (fun f ->
             "vpcPeeringConnectionId", String.to_json f)
       ; Aws.Util.option_map v.vpc_id (fun f -> "vpcId", String.to_json f)
       ; Aws.Util.option_map v.group_id (fun f -> "groupId", String.to_json f)
       ; Aws.Util.option_map v.group_name (fun f -> "groupName", String.to_json f)
       ; Aws.Util.option_map v.user_id (fun f -> "userId", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ])

let of_json j =
  { description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; user_id = Aws.Util.option_map (Aws.Json.lookup j "userId") String.of_json
  ; group_name = Aws.Util.option_map (Aws.Json.lookup j "groupName") String.of_json
  ; group_id = Aws.Util.option_map (Aws.Json.lookup j "groupId") String.of_json
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "vpcId") String.of_json
  ; vpc_peering_connection_id =
      Aws.Util.option_map (Aws.Json.lookup j "vpcPeeringConnectionId") String.of_json
  ; peering_status =
      Aws.Util.option_map (Aws.Json.lookup j "peeringStatus") String.of_json
  }
