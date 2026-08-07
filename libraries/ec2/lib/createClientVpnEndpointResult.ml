open Aws.BaseTypes

type t =
  { client_vpn_endpoint_id : String.t option
  ; status : ClientVpnEndpointStatus.t option
  ; dns_name : String.t option
  }

let make ?client_vpn_endpoint_id ?status ?dns_name () =
  { client_vpn_endpoint_id; status; dns_name }

let parse xml =
  Some
    { client_vpn_endpoint_id =
        Aws.Util.option_bind (Aws.Xml.member "clientVpnEndpointId" xml) String.parse
    ; status =
        Aws.Util.option_bind (Aws.Xml.member "status" xml) ClientVpnEndpointStatus.parse
    ; dns_name = Aws.Util.option_bind (Aws.Xml.member "dnsName" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dns_name (fun f ->
             Aws.Query.Pair ("DnsName", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", ClientVpnEndpointStatus.to_query f))
       ; Aws.Util.option_map v.client_vpn_endpoint_id (fun f ->
             Aws.Query.Pair ("ClientVpnEndpointId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dns_name (fun f -> "dnsName", String.to_json f)
       ; Aws.Util.option_map v.status (fun f ->
             "status", ClientVpnEndpointStatus.to_json f)
       ; Aws.Util.option_map v.client_vpn_endpoint_id (fun f ->
             "clientVpnEndpointId", String.to_json f)
       ])

let of_json j =
  { client_vpn_endpoint_id =
      Aws.Util.option_map (Aws.Json.lookup j "clientVpnEndpointId") String.of_json
  ; status =
      Aws.Util.option_map (Aws.Json.lookup j "status") ClientVpnEndpointStatus.of_json
  ; dns_name = Aws.Util.option_map (Aws.Json.lookup j "dnsName") String.of_json
  }
