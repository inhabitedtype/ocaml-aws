open Aws.BaseTypes

type t =
  { vpn_connection_id : String.t
  ; transit_gateway_id : String.t option
  ; customer_gateway_id : String.t option
  ; vpn_gateway_id : String.t option
  ; dry_run : Boolean.t option
  }

let make
    ~vpn_connection_id
    ?transit_gateway_id
    ?customer_gateway_id
    ?vpn_gateway_id
    ?dry_run
    () =
  { vpn_connection_id; transit_gateway_id; customer_gateway_id; vpn_gateway_id; dry_run }

let parse xml =
  Some
    { vpn_connection_id =
        Aws.Xml.required
          "VpnConnectionId"
          (Aws.Util.option_bind (Aws.Xml.member "VpnConnectionId" xml) String.parse)
    ; transit_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "TransitGatewayId" xml) String.parse
    ; customer_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "CustomerGatewayId" xml) String.parse
    ; vpn_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "VpnGatewayId" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.vpn_gateway_id (fun f ->
             Aws.Query.Pair ("VpnGatewayId", String.to_query f))
       ; Aws.Util.option_map v.customer_gateway_id (fun f ->
             Aws.Query.Pair ("CustomerGatewayId", String.to_query f))
       ; Aws.Util.option_map v.transit_gateway_id (fun f ->
             Aws.Query.Pair ("TransitGatewayId", String.to_query f))
       ; Some (Aws.Query.Pair ("VpnConnectionId", String.to_query v.vpn_connection_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.vpn_gateway_id (fun f -> "VpnGatewayId", String.to_json f)
       ; Aws.Util.option_map v.customer_gateway_id (fun f ->
             "CustomerGatewayId", String.to_json f)
       ; Aws.Util.option_map v.transit_gateway_id (fun f ->
             "TransitGatewayId", String.to_json f)
       ; Some ("VpnConnectionId", String.to_json v.vpn_connection_id)
       ])

let of_json j =
  { vpn_connection_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VpnConnectionId"))
  ; transit_gateway_id =
      Aws.Util.option_map (Aws.Json.lookup j "TransitGatewayId") String.of_json
  ; customer_gateway_id =
      Aws.Util.option_map (Aws.Json.lookup j "CustomerGatewayId") String.of_json
  ; vpn_gateway_id = Aws.Util.option_map (Aws.Json.lookup j "VpnGatewayId") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
