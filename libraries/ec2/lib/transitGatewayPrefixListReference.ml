open Aws.BaseTypes

type t =
  { transit_gateway_route_table_id : String.t option
  ; prefix_list_id : String.t option
  ; prefix_list_owner_id : String.t option
  ; state : TransitGatewayPrefixListReferenceState.t option
  ; blackhole : Boolean.t option
  ; transit_gateway_attachment : TransitGatewayPrefixListAttachment.t option
  }

let make
    ?transit_gateway_route_table_id
    ?prefix_list_id
    ?prefix_list_owner_id
    ?state
    ?blackhole
    ?transit_gateway_attachment
    () =
  { transit_gateway_route_table_id
  ; prefix_list_id
  ; prefix_list_owner_id
  ; state
  ; blackhole
  ; transit_gateway_attachment
  }

let parse xml =
  Some
    { transit_gateway_route_table_id =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayRouteTableId" xml)
          String.parse
    ; prefix_list_id =
        Aws.Util.option_bind (Aws.Xml.member "prefixListId" xml) String.parse
    ; prefix_list_owner_id =
        Aws.Util.option_bind (Aws.Xml.member "prefixListOwnerId" xml) String.parse
    ; state =
        Aws.Util.option_bind
          (Aws.Xml.member "state" xml)
          TransitGatewayPrefixListReferenceState.parse
    ; blackhole = Aws.Util.option_bind (Aws.Xml.member "blackhole" xml) Boolean.parse
    ; transit_gateway_attachment =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayAttachment" xml)
          TransitGatewayPrefixListAttachment.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway_attachment (fun f ->
             Aws.Query.Pair
               ("TransitGatewayAttachment", TransitGatewayPrefixListAttachment.to_query f))
       ; Aws.Util.option_map v.blackhole (fun f ->
             Aws.Query.Pair ("Blackhole", Boolean.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", TransitGatewayPrefixListReferenceState.to_query f))
       ; Aws.Util.option_map v.prefix_list_owner_id (fun f ->
             Aws.Query.Pair ("PrefixListOwnerId", String.to_query f))
       ; Aws.Util.option_map v.prefix_list_id (fun f ->
             Aws.Query.Pair ("PrefixListId", String.to_query f))
       ; Aws.Util.option_map v.transit_gateway_route_table_id (fun f ->
             Aws.Query.Pair ("TransitGatewayRouteTableId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway_attachment (fun f ->
             "transitGatewayAttachment", TransitGatewayPrefixListAttachment.to_json f)
       ; Aws.Util.option_map v.blackhole (fun f -> "blackhole", Boolean.to_json f)
       ; Aws.Util.option_map v.state (fun f ->
             "state", TransitGatewayPrefixListReferenceState.to_json f)
       ; Aws.Util.option_map v.prefix_list_owner_id (fun f ->
             "prefixListOwnerId", String.to_json f)
       ; Aws.Util.option_map v.prefix_list_id (fun f -> "prefixListId", String.to_json f)
       ; Aws.Util.option_map v.transit_gateway_route_table_id (fun f ->
             "transitGatewayRouteTableId", String.to_json f)
       ])

let of_json j =
  { transit_gateway_route_table_id =
      Aws.Util.option_map (Aws.Json.lookup j "transitGatewayRouteTableId") String.of_json
  ; prefix_list_id = Aws.Util.option_map (Aws.Json.lookup j "prefixListId") String.of_json
  ; prefix_list_owner_id =
      Aws.Util.option_map (Aws.Json.lookup j "prefixListOwnerId") String.of_json
  ; state =
      Aws.Util.option_map
        (Aws.Json.lookup j "state")
        TransitGatewayPrefixListReferenceState.of_json
  ; blackhole = Aws.Util.option_map (Aws.Json.lookup j "blackhole") Boolean.of_json
  ; transit_gateway_attachment =
      Aws.Util.option_map
        (Aws.Json.lookup j "transitGatewayAttachment")
        TransitGatewayPrefixListAttachment.of_json
  }
