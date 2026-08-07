open Aws.BaseTypes

type t =
  { client_vpn_endpoint_id : String.t
  ; target_network_cidr : String.t
  ; access_group_id : String.t option
  ; authorize_all_groups : Boolean.t option
  ; description : String.t option
  ; client_token : String.t option
  ; dry_run : Boolean.t option
  }

let make
    ~client_vpn_endpoint_id
    ~target_network_cidr
    ?access_group_id
    ?authorize_all_groups
    ?description
    ?client_token
    ?dry_run
    () =
  { client_vpn_endpoint_id
  ; target_network_cidr
  ; access_group_id
  ; authorize_all_groups
  ; description
  ; client_token
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
    ; authorize_all_groups =
        Aws.Util.option_bind (Aws.Xml.member "AuthorizeAllGroups" xml) Boolean.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.authorize_all_groups (fun f ->
             Aws.Query.Pair ("AuthorizeAllGroups", Boolean.to_query f))
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
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.authorize_all_groups (fun f ->
             "AuthorizeAllGroups", Boolean.to_json f)
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
  ; authorize_all_groups =
      Aws.Util.option_map (Aws.Json.lookup j "AuthorizeAllGroups") Boolean.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
