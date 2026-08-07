open Aws.BaseTypes

type t =
  { client_vpn_endpoint_id : String.t option
  ; timestamp : String.t option
  ; connection_id : String.t option
  ; username : String.t option
  ; connection_established_time : String.t option
  ; ingress_bytes : String.t option
  ; egress_bytes : String.t option
  ; ingress_packets : String.t option
  ; egress_packets : String.t option
  ; client_ip : String.t option
  ; client_ipv6_address : String.t option
  ; common_name : String.t option
  ; status : ClientVpnConnectionStatus.t option
  ; connection_end_time : String.t option
  ; posture_compliance_statuses : ValueStringList.t
  }

let make
    ?client_vpn_endpoint_id
    ?timestamp
    ?connection_id
    ?username
    ?connection_established_time
    ?ingress_bytes
    ?egress_bytes
    ?ingress_packets
    ?egress_packets
    ?client_ip
    ?client_ipv6_address
    ?common_name
    ?status
    ?connection_end_time
    ?(posture_compliance_statuses = [])
    () =
  { client_vpn_endpoint_id
  ; timestamp
  ; connection_id
  ; username
  ; connection_established_time
  ; ingress_bytes
  ; egress_bytes
  ; ingress_packets
  ; egress_packets
  ; client_ip
  ; client_ipv6_address
  ; common_name
  ; status
  ; connection_end_time
  ; posture_compliance_statuses
  }

let parse xml =
  Some
    { client_vpn_endpoint_id =
        Aws.Util.option_bind (Aws.Xml.member "clientVpnEndpointId" xml) String.parse
    ; timestamp = Aws.Util.option_bind (Aws.Xml.member "timestamp" xml) String.parse
    ; connection_id =
        Aws.Util.option_bind (Aws.Xml.member "connectionId" xml) String.parse
    ; username = Aws.Util.option_bind (Aws.Xml.member "username" xml) String.parse
    ; connection_established_time =
        Aws.Util.option_bind (Aws.Xml.member "connectionEstablishedTime" xml) String.parse
    ; ingress_bytes =
        Aws.Util.option_bind (Aws.Xml.member "ingressBytes" xml) String.parse
    ; egress_bytes = Aws.Util.option_bind (Aws.Xml.member "egressBytes" xml) String.parse
    ; ingress_packets =
        Aws.Util.option_bind (Aws.Xml.member "ingressPackets" xml) String.parse
    ; egress_packets =
        Aws.Util.option_bind (Aws.Xml.member "egressPackets" xml) String.parse
    ; client_ip = Aws.Util.option_bind (Aws.Xml.member "clientIp" xml) String.parse
    ; client_ipv6_address =
        Aws.Util.option_bind (Aws.Xml.member "clientIpv6Address" xml) String.parse
    ; common_name = Aws.Util.option_bind (Aws.Xml.member "commonName" xml) String.parse
    ; status =
        Aws.Util.option_bind (Aws.Xml.member "status" xml) ClientVpnConnectionStatus.parse
    ; connection_end_time =
        Aws.Util.option_bind (Aws.Xml.member "connectionEndTime" xml) String.parse
    ; posture_compliance_statuses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "postureComplianceStatusSet" xml)
             ValueStringList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "PostureComplianceStatusSet"
              , ValueStringList.to_query v.posture_compliance_statuses ))
       ; Aws.Util.option_map v.connection_end_time (fun f ->
             Aws.Query.Pair ("ConnectionEndTime", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", ClientVpnConnectionStatus.to_query f))
       ; Aws.Util.option_map v.common_name (fun f ->
             Aws.Query.Pair ("CommonName", String.to_query f))
       ; Aws.Util.option_map v.client_ipv6_address (fun f ->
             Aws.Query.Pair ("ClientIpv6Address", String.to_query f))
       ; Aws.Util.option_map v.client_ip (fun f ->
             Aws.Query.Pair ("ClientIp", String.to_query f))
       ; Aws.Util.option_map v.egress_packets (fun f ->
             Aws.Query.Pair ("EgressPackets", String.to_query f))
       ; Aws.Util.option_map v.ingress_packets (fun f ->
             Aws.Query.Pair ("IngressPackets", String.to_query f))
       ; Aws.Util.option_map v.egress_bytes (fun f ->
             Aws.Query.Pair ("EgressBytes", String.to_query f))
       ; Aws.Util.option_map v.ingress_bytes (fun f ->
             Aws.Query.Pair ("IngressBytes", String.to_query f))
       ; Aws.Util.option_map v.connection_established_time (fun f ->
             Aws.Query.Pair ("ConnectionEstablishedTime", String.to_query f))
       ; Aws.Util.option_map v.username (fun f ->
             Aws.Query.Pair ("Username", String.to_query f))
       ; Aws.Util.option_map v.connection_id (fun f ->
             Aws.Query.Pair ("ConnectionId", String.to_query f))
       ; Aws.Util.option_map v.timestamp (fun f ->
             Aws.Query.Pair ("Timestamp", String.to_query f))
       ; Aws.Util.option_map v.client_vpn_endpoint_id (fun f ->
             Aws.Query.Pair ("ClientVpnEndpointId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "postureComplianceStatusSet"
           , ValueStringList.to_json v.posture_compliance_statuses )
       ; Aws.Util.option_map v.connection_end_time (fun f ->
             "connectionEndTime", String.to_json f)
       ; Aws.Util.option_map v.status (fun f ->
             "status", ClientVpnConnectionStatus.to_json f)
       ; Aws.Util.option_map v.common_name (fun f -> "commonName", String.to_json f)
       ; Aws.Util.option_map v.client_ipv6_address (fun f ->
             "clientIpv6Address", String.to_json f)
       ; Aws.Util.option_map v.client_ip (fun f -> "clientIp", String.to_json f)
       ; Aws.Util.option_map v.egress_packets (fun f -> "egressPackets", String.to_json f)
       ; Aws.Util.option_map v.ingress_packets (fun f ->
             "ingressPackets", String.to_json f)
       ; Aws.Util.option_map v.egress_bytes (fun f -> "egressBytes", String.to_json f)
       ; Aws.Util.option_map v.ingress_bytes (fun f -> "ingressBytes", String.to_json f)
       ; Aws.Util.option_map v.connection_established_time (fun f ->
             "connectionEstablishedTime", String.to_json f)
       ; Aws.Util.option_map v.username (fun f -> "username", String.to_json f)
       ; Aws.Util.option_map v.connection_id (fun f -> "connectionId", String.to_json f)
       ; Aws.Util.option_map v.timestamp (fun f -> "timestamp", String.to_json f)
       ; Aws.Util.option_map v.client_vpn_endpoint_id (fun f ->
             "clientVpnEndpointId", String.to_json f)
       ])

let of_json j =
  { client_vpn_endpoint_id =
      Aws.Util.option_map (Aws.Json.lookup j "clientVpnEndpointId") String.of_json
  ; timestamp = Aws.Util.option_map (Aws.Json.lookup j "timestamp") String.of_json
  ; connection_id = Aws.Util.option_map (Aws.Json.lookup j "connectionId") String.of_json
  ; username = Aws.Util.option_map (Aws.Json.lookup j "username") String.of_json
  ; connection_established_time =
      Aws.Util.option_map (Aws.Json.lookup j "connectionEstablishedTime") String.of_json
  ; ingress_bytes = Aws.Util.option_map (Aws.Json.lookup j "ingressBytes") String.of_json
  ; egress_bytes = Aws.Util.option_map (Aws.Json.lookup j "egressBytes") String.of_json
  ; ingress_packets =
      Aws.Util.option_map (Aws.Json.lookup j "ingressPackets") String.of_json
  ; egress_packets =
      Aws.Util.option_map (Aws.Json.lookup j "egressPackets") String.of_json
  ; client_ip = Aws.Util.option_map (Aws.Json.lookup j "clientIp") String.of_json
  ; client_ipv6_address =
      Aws.Util.option_map (Aws.Json.lookup j "clientIpv6Address") String.of_json
  ; common_name = Aws.Util.option_map (Aws.Json.lookup j "commonName") String.of_json
  ; status =
      Aws.Util.option_map (Aws.Json.lookup j "status") ClientVpnConnectionStatus.of_json
  ; connection_end_time =
      Aws.Util.option_map (Aws.Json.lookup j "connectionEndTime") String.of_json
  ; posture_compliance_statuses =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "postureComplianceStatusSet"))
  }
