open Aws.BaseTypes

type t =
  { group_id : String.t option
  ; referencing_vpc_id : String.t option
  ; vpc_peering_connection_id : String.t option
  ; transit_gateway_id : String.t option
  }

let make ?group_id ?referencing_vpc_id ?vpc_peering_connection_id ?transit_gateway_id () =
  { group_id; referencing_vpc_id; vpc_peering_connection_id; transit_gateway_id }

let parse xml =
  Some
    { group_id = Aws.Util.option_bind (Aws.Xml.member "groupId" xml) String.parse
    ; referencing_vpc_id =
        Aws.Util.option_bind (Aws.Xml.member "referencingVpcId" xml) String.parse
    ; vpc_peering_connection_id =
        Aws.Util.option_bind (Aws.Xml.member "vpcPeeringConnectionId" xml) String.parse
    ; transit_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "transitGatewayId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway_id (fun f ->
             Aws.Query.Pair ("TransitGatewayId", String.to_query f))
       ; Aws.Util.option_map v.vpc_peering_connection_id (fun f ->
             Aws.Query.Pair ("VpcPeeringConnectionId", String.to_query f))
       ; Aws.Util.option_map v.referencing_vpc_id (fun f ->
             Aws.Query.Pair ("ReferencingVpcId", String.to_query f))
       ; Aws.Util.option_map v.group_id (fun f ->
             Aws.Query.Pair ("GroupId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway_id (fun f ->
             "transitGatewayId", String.to_json f)
       ; Aws.Util.option_map v.vpc_peering_connection_id (fun f ->
             "vpcPeeringConnectionId", String.to_json f)
       ; Aws.Util.option_map v.referencing_vpc_id (fun f ->
             "referencingVpcId", String.to_json f)
       ; Aws.Util.option_map v.group_id (fun f -> "groupId", String.to_json f)
       ])

let of_json j =
  { group_id = Aws.Util.option_map (Aws.Json.lookup j "groupId") String.of_json
  ; referencing_vpc_id =
      Aws.Util.option_map (Aws.Json.lookup j "referencingVpcId") String.of_json
  ; vpc_peering_connection_id =
      Aws.Util.option_map (Aws.Json.lookup j "vpcPeeringConnectionId") String.of_json
  ; transit_gateway_id =
      Aws.Util.option_map (Aws.Json.lookup j "transitGatewayId") String.of_json
  }
