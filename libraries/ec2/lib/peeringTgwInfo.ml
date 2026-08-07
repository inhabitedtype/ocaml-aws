open Aws.BaseTypes

type t =
  { transit_gateway_id : String.t option
  ; core_network_id : String.t option
  ; owner_id : String.t option
  ; region : String.t option
  }

let make ?transit_gateway_id ?core_network_id ?owner_id ?region () =
  { transit_gateway_id; core_network_id; owner_id; region }

let parse xml =
  Some
    { transit_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "transitGatewayId" xml) String.parse
    ; core_network_id =
        Aws.Util.option_bind (Aws.Xml.member "coreNetworkId" xml) String.parse
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; region = Aws.Util.option_bind (Aws.Xml.member "region" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.region (fun f ->
             Aws.Query.Pair ("Region", String.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ; Aws.Util.option_map v.core_network_id (fun f ->
             Aws.Query.Pair ("CoreNetworkId", String.to_query f))
       ; Aws.Util.option_map v.transit_gateway_id (fun f ->
             Aws.Query.Pair ("TransitGatewayId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.region (fun f -> "region", String.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ; Aws.Util.option_map v.core_network_id (fun f ->
             "coreNetworkId", String.to_json f)
       ; Aws.Util.option_map v.transit_gateway_id (fun f ->
             "transitGatewayId", String.to_json f)
       ])

let of_json j =
  { transit_gateway_id =
      Aws.Util.option_map (Aws.Json.lookup j "transitGatewayId") String.of_json
  ; core_network_id =
      Aws.Util.option_map (Aws.Json.lookup j "coreNetworkId") String.of_json
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; region = Aws.Util.option_map (Aws.Json.lookup j "region") String.of_json
  }
