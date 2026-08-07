open Aws.BaseTypes

type t =
  { client_vpn_endpoint_id : String.t
  ; vpc_id : String.t
  ; security_group_ids : ClientVpnSecurityGroupIdSet.t
  ; dry_run : Boolean.t option
  }

let make ~client_vpn_endpoint_id ~vpc_id ~security_group_ids ?dry_run () =
  { client_vpn_endpoint_id; vpc_id; security_group_ids; dry_run }

let parse xml =
  Some
    { client_vpn_endpoint_id =
        Aws.Xml.required
          "ClientVpnEndpointId"
          (Aws.Util.option_bind (Aws.Xml.member "ClientVpnEndpointId" xml) String.parse)
    ; vpc_id =
        Aws.Xml.required
          "VpcId"
          (Aws.Util.option_bind (Aws.Xml.member "VpcId" xml) String.parse)
    ; security_group_ids =
        Aws.Xml.required
          "SecurityGroupId"
          (Aws.Util.option_bind
             (Aws.Xml.member "SecurityGroupId" xml)
             ClientVpnSecurityGroupIdSet.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SecurityGroupId"
              , ClientVpnSecurityGroupIdSet.to_query v.security_group_ids ))
       ; Some (Aws.Query.Pair ("VpcId", String.to_query v.vpc_id))
       ; Some
           (Aws.Query.Pair
              ("ClientVpnEndpointId", String.to_query v.client_vpn_endpoint_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("SecurityGroupId", ClientVpnSecurityGroupIdSet.to_json v.security_group_ids)
       ; Some ("VpcId", String.to_json v.vpc_id)
       ; Some ("ClientVpnEndpointId", String.to_json v.client_vpn_endpoint_id)
       ])

let of_json j =
  { client_vpn_endpoint_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ClientVpnEndpointId"))
  ; vpc_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VpcId"))
  ; security_group_ids =
      ClientVpnSecurityGroupIdSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroupId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
