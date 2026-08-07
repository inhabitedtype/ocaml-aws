open Aws.BaseTypes

type t =
  { vpn_connection_id : String.t
  ; vpn_tunnel_outside_ip_address : String.t
  ; tunnel_options : ModifyVpnTunnelOptionsSpecification.t
  ; dry_run : Boolean.t option
  ; skip_tunnel_replacement : Boolean.t option
  ; pre_shared_key_storage : String.t option
  }

let make
    ~vpn_connection_id
    ~vpn_tunnel_outside_ip_address
    ~tunnel_options
    ?dry_run
    ?skip_tunnel_replacement
    ?pre_shared_key_storage
    () =
  { vpn_connection_id
  ; vpn_tunnel_outside_ip_address
  ; tunnel_options
  ; dry_run
  ; skip_tunnel_replacement
  ; pre_shared_key_storage
  }

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
    ; tunnel_options =
        Aws.Xml.required
          "TunnelOptions"
          (Aws.Util.option_bind
             (Aws.Xml.member "TunnelOptions" xml)
             ModifyVpnTunnelOptionsSpecification.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; skip_tunnel_replacement =
        Aws.Util.option_bind (Aws.Xml.member "SkipTunnelReplacement" xml) Boolean.parse
    ; pre_shared_key_storage =
        Aws.Util.option_bind (Aws.Xml.member "PreSharedKeyStorage" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.pre_shared_key_storage (fun f ->
             Aws.Query.Pair ("PreSharedKeyStorage", String.to_query f))
       ; Aws.Util.option_map v.skip_tunnel_replacement (fun f ->
             Aws.Query.Pair ("SkipTunnelReplacement", Boolean.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "TunnelOptions"
              , ModifyVpnTunnelOptionsSpecification.to_query v.tunnel_options ))
       ; Some
           (Aws.Query.Pair
              ( "VpnTunnelOutsideIpAddress"
              , String.to_query v.vpn_tunnel_outside_ip_address ))
       ; Some (Aws.Query.Pair ("VpnConnectionId", String.to_query v.vpn_connection_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.pre_shared_key_storage (fun f ->
             "PreSharedKeyStorage", String.to_json f)
       ; Aws.Util.option_map v.skip_tunnel_replacement (fun f ->
             "SkipTunnelReplacement", Boolean.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some
           ("TunnelOptions", ModifyVpnTunnelOptionsSpecification.to_json v.tunnel_options)
       ; Some ("VpnTunnelOutsideIpAddress", String.to_json v.vpn_tunnel_outside_ip_address)
       ; Some ("VpnConnectionId", String.to_json v.vpn_connection_id)
       ])

let of_json j =
  { vpn_connection_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VpnConnectionId"))
  ; vpn_tunnel_outside_ip_address =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "VpnTunnelOutsideIpAddress"))
  ; tunnel_options =
      ModifyVpnTunnelOptionsSpecification.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TunnelOptions"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; skip_tunnel_replacement =
      Aws.Util.option_map (Aws.Json.lookup j "SkipTunnelReplacement") Boolean.of_json
  ; pre_shared_key_storage =
      Aws.Util.option_map (Aws.Json.lookup j "PreSharedKeyStorage") String.of_json
  }
