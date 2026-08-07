open Aws.BaseTypes

type t =
  { client_vpn_endpoint_id : String.t
  ; connection_id : String.t option
  ; username : String.t option
  ; dry_run : Boolean.t option
  }

let make ~client_vpn_endpoint_id ?connection_id ?username ?dry_run () =
  { client_vpn_endpoint_id; connection_id; username; dry_run }

let parse xml =
  Some
    { client_vpn_endpoint_id =
        Aws.Xml.required
          "ClientVpnEndpointId"
          (Aws.Util.option_bind (Aws.Xml.member "ClientVpnEndpointId" xml) String.parse)
    ; connection_id =
        Aws.Util.option_bind (Aws.Xml.member "ConnectionId" xml) String.parse
    ; username = Aws.Util.option_bind (Aws.Xml.member "Username" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.username (fun f ->
             Aws.Query.Pair ("Username", String.to_query f))
       ; Aws.Util.option_map v.connection_id (fun f ->
             Aws.Query.Pair ("ConnectionId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("ClientVpnEndpointId", String.to_query v.client_vpn_endpoint_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.username (fun f -> "Username", String.to_json f)
       ; Aws.Util.option_map v.connection_id (fun f -> "ConnectionId", String.to_json f)
       ; Some ("ClientVpnEndpointId", String.to_json v.client_vpn_endpoint_id)
       ])

let of_json j =
  { client_vpn_endpoint_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ClientVpnEndpointId"))
  ; connection_id = Aws.Util.option_map (Aws.Json.lookup j "ConnectionId") String.of_json
  ; username = Aws.Util.option_map (Aws.Json.lookup j "Username") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
