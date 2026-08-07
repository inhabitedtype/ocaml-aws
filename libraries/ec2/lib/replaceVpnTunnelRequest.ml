open Aws.BaseTypes

type t =
  { vpn_connection_id : String.t
  ; vpn_tunnel_outside_ip_address : String.t
  ; apply_pending_maintenance : Boolean.t option
  ; dry_run : Boolean.t option
  }

let make
    ~vpn_connection_id
    ~vpn_tunnel_outside_ip_address
    ?apply_pending_maintenance
    ?dry_run
    () =
  { vpn_connection_id; vpn_tunnel_outside_ip_address; apply_pending_maintenance; dry_run }

let parse xml =
  Some
    { vpn_connection_id =
        Aws.Xml.required
          "VpnConnectionId"
          (Aws.Util.option_bind (Aws.Xml.member "VpnConnectionId" xml) String.parse)
    ; vpn_tunnel_outside_ip_address =
        Aws.Xml.required
          "VpnTunnelOutsideIpAddress"
          (Aws.Util.option_bind
             (Aws.Xml.member "VpnTunnelOutsideIpAddress" xml)
             String.parse)
    ; apply_pending_maintenance =
        Aws.Util.option_bind (Aws.Xml.member "ApplyPendingMaintenance" xml) Boolean.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.apply_pending_maintenance (fun f ->
             Aws.Query.Pair ("ApplyPendingMaintenance", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "VpnTunnelOutsideIpAddress"
              , String.to_query v.vpn_tunnel_outside_ip_address ))
       ; Some (Aws.Query.Pair ("VpnConnectionId", String.to_query v.vpn_connection_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.apply_pending_maintenance (fun f ->
             "ApplyPendingMaintenance", Boolean.to_json f)
       ; Some ("VpnTunnelOutsideIpAddress", String.to_json v.vpn_tunnel_outside_ip_address)
       ; Some ("VpnConnectionId", String.to_json v.vpn_connection_id)
       ])

let of_json j =
  { vpn_connection_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VpnConnectionId"))
  ; vpn_tunnel_outside_ip_address =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "VpnTunnelOutsideIpAddress"))
  ; apply_pending_maintenance =
      Aws.Util.option_map (Aws.Json.lookup j "ApplyPendingMaintenance") Boolean.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
