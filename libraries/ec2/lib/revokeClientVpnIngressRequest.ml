open Aws.BaseTypes

type t =
  { client_vpn_endpoint_id : String.t
  ; target_network_cidr : String.t
  ; access_group_id : String.t option
  ; revoke_all_groups : Boolean.t option
  ; dry_run : Boolean.t option
  }

let make
    ~client_vpn_endpoint_id
    ~target_network_cidr
    ?access_group_id
    ?revoke_all_groups
    ?dry_run
    () =
  { client_vpn_endpoint_id
  ; target_network_cidr
  ; access_group_id
  ; revoke_all_groups
  ; dry_run
  }

let parse xml =
  Some
    { client_vpn_endpoint_id =
        Aws.Xml.required
          "ClientVpnEndpointId"
          (Aws.Util.option_bind (Aws.Xml.member "ClientVpnEndpointId" xml) String.parse)
    ; target_network_cidr =
        Aws.Xml.required
          "TargetNetworkCidr"
          (Aws.Util.option_bind (Aws.Xml.member "TargetNetworkCidr" xml) String.parse)
    ; access_group_id =
        Aws.Util.option_bind (Aws.Xml.member "AccessGroupId" xml) String.parse
    ; revoke_all_groups =
        Aws.Util.option_bind (Aws.Xml.member "RevokeAllGroups" xml) Boolean.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.revoke_all_groups (fun f ->
             Aws.Query.Pair ("RevokeAllGroups", Boolean.to_query f))
       ; Aws.Util.option_map v.access_group_id (fun f ->
             Aws.Query.Pair ("AccessGroupId", String.to_query f))
       ; Some
           (Aws.Query.Pair ("TargetNetworkCidr", String.to_query v.target_network_cidr))
       ; Some
           (Aws.Query.Pair
              ("ClientVpnEndpointId", String.to_query v.client_vpn_endpoint_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.revoke_all_groups (fun f ->
             "RevokeAllGroups", Boolean.to_json f)
       ; Aws.Util.option_map v.access_group_id (fun f ->
             "AccessGroupId", String.to_json f)
       ; Some ("TargetNetworkCidr", String.to_json v.target_network_cidr)
       ; Some ("ClientVpnEndpointId", String.to_json v.client_vpn_endpoint_id)
       ])

let of_json j =
  { client_vpn_endpoint_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ClientVpnEndpointId"))
  ; target_network_cidr =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TargetNetworkCidr"))
  ; access_group_id =
      Aws.Util.option_map (Aws.Json.lookup j "AccessGroupId") String.of_json
  ; revoke_all_groups =
      Aws.Util.option_map (Aws.Json.lookup j "RevokeAllGroups") Boolean.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
