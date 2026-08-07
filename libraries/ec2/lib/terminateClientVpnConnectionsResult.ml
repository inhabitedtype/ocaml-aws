open Aws.BaseTypes

type t =
  { client_vpn_endpoint_id : String.t option
  ; username : String.t option
  ; connection_statuses : TerminateConnectionStatusSet.t
  }

let make ?client_vpn_endpoint_id ?username ?(connection_statuses = []) () =
  { client_vpn_endpoint_id; username; connection_statuses }

let parse xml =
  Some
    { client_vpn_endpoint_id =
        Aws.Util.option_bind (Aws.Xml.member "clientVpnEndpointId" xml) String.parse
    ; username = Aws.Util.option_bind (Aws.Xml.member "username" xml) String.parse
    ; connection_statuses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "connectionStatuses" xml)
             TerminateConnectionStatusSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "ConnectionStatuses"
              , TerminateConnectionStatusSet.to_query v.connection_statuses ))
       ; Aws.Util.option_map v.username (fun f ->
             Aws.Query.Pair ("Username", String.to_query f))
       ; Aws.Util.option_map v.client_vpn_endpoint_id (fun f ->
             Aws.Query.Pair ("ClientVpnEndpointId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "connectionStatuses"
           , TerminateConnectionStatusSet.to_json v.connection_statuses )
       ; Aws.Util.option_map v.username (fun f -> "username", String.to_json f)
       ; Aws.Util.option_map v.client_vpn_endpoint_id (fun f ->
             "clientVpnEndpointId", String.to_json f)
       ])

let of_json j =
  { client_vpn_endpoint_id =
      Aws.Util.option_map (Aws.Json.lookup j "clientVpnEndpointId") String.of_json
  ; username = Aws.Util.option_map (Aws.Json.lookup j "username") String.of_json
  ; connection_statuses =
      TerminateConnectionStatusSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "connectionStatuses"))
  }
