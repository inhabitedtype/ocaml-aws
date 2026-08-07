open Aws.BaseTypes

type t =
  { transit_gateway_asn : Long.t option
  ; peer_asn : Long.t option
  ; transit_gateway_address : String.t option
  ; peer_address : String.t option
  ; bgp_status : BgpStatus.t option
  }

let make
    ?transit_gateway_asn
    ?peer_asn
    ?transit_gateway_address
    ?peer_address
    ?bgp_status
    () =
  { transit_gateway_asn; peer_asn; transit_gateway_address; peer_address; bgp_status }

let parse xml =
  Some
    { transit_gateway_asn =
        Aws.Util.option_bind (Aws.Xml.member "transitGatewayAsn" xml) Long.parse
    ; peer_asn = Aws.Util.option_bind (Aws.Xml.member "peerAsn" xml) Long.parse
    ; transit_gateway_address =
        Aws.Util.option_bind (Aws.Xml.member "transitGatewayAddress" xml) String.parse
    ; peer_address = Aws.Util.option_bind (Aws.Xml.member "peerAddress" xml) String.parse
    ; bgp_status = Aws.Util.option_bind (Aws.Xml.member "bgpStatus" xml) BgpStatus.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.bgp_status (fun f ->
             Aws.Query.Pair ("BgpStatus", BgpStatus.to_query f))
       ; Aws.Util.option_map v.peer_address (fun f ->
             Aws.Query.Pair ("PeerAddress", String.to_query f))
       ; Aws.Util.option_map v.transit_gateway_address (fun f ->
             Aws.Query.Pair ("TransitGatewayAddress", String.to_query f))
       ; Aws.Util.option_map v.peer_asn (fun f ->
             Aws.Query.Pair ("PeerAsn", Long.to_query f))
       ; Aws.Util.option_map v.transit_gateway_asn (fun f ->
             Aws.Query.Pair ("TransitGatewayAsn", Long.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.bgp_status (fun f -> "bgpStatus", BgpStatus.to_json f)
       ; Aws.Util.option_map v.peer_address (fun f -> "peerAddress", String.to_json f)
       ; Aws.Util.option_map v.transit_gateway_address (fun f ->
             "transitGatewayAddress", String.to_json f)
       ; Aws.Util.option_map v.peer_asn (fun f -> "peerAsn", Long.to_json f)
       ; Aws.Util.option_map v.transit_gateway_asn (fun f ->
             "transitGatewayAsn", Long.to_json f)
       ])

let of_json j =
  { transit_gateway_asn =
      Aws.Util.option_map (Aws.Json.lookup j "transitGatewayAsn") Long.of_json
  ; peer_asn = Aws.Util.option_map (Aws.Json.lookup j "peerAsn") Long.of_json
  ; transit_gateway_address =
      Aws.Util.option_map (Aws.Json.lookup j "transitGatewayAddress") String.of_json
  ; peer_address = Aws.Util.option_map (Aws.Json.lookup j "peerAddress") String.of_json
  ; bgp_status = Aws.Util.option_map (Aws.Json.lookup j "bgpStatus") BgpStatus.of_json
  }
