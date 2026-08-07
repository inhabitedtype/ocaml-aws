open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; service_id : String.t
  ; private_dns_name : String.t option
  ; remove_private_dns_name : Boolean.t option
  ; acceptance_required : Boolean.t option
  ; add_network_load_balancer_arns : ValueStringList.t
  ; remove_network_load_balancer_arns : ValueStringList.t
  ; add_gateway_load_balancer_arns : ValueStringList.t
  ; remove_gateway_load_balancer_arns : ValueStringList.t
  ; add_supported_ip_address_types : ValueStringList.t
  ; remove_supported_ip_address_types : ValueStringList.t
  ; add_supported_regions : ValueStringList.t
  ; remove_supported_regions : ValueStringList.t
  }

let make
    ?dry_run
    ~service_id
    ?private_dns_name
    ?remove_private_dns_name
    ?acceptance_required
    ?(add_network_load_balancer_arns = [])
    ?(remove_network_load_balancer_arns = [])
    ?(add_gateway_load_balancer_arns = [])
    ?(remove_gateway_load_balancer_arns = [])
    ?(add_supported_ip_address_types = [])
    ?(remove_supported_ip_address_types = [])
    ?(add_supported_regions = [])
    ?(remove_supported_regions = [])
    () =
  { dry_run
  ; service_id
  ; private_dns_name
  ; remove_private_dns_name
  ; acceptance_required
  ; add_network_load_balancer_arns
  ; remove_network_load_balancer_arns
  ; add_gateway_load_balancer_arns
  ; remove_gateway_load_balancer_arns
  ; add_supported_ip_address_types
  ; remove_supported_ip_address_types
  ; add_supported_regions
  ; remove_supported_regions
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; service_id =
        Aws.Xml.required
          "ServiceId"
          (Aws.Util.option_bind (Aws.Xml.member "ServiceId" xml) String.parse)
    ; private_dns_name =
        Aws.Util.option_bind (Aws.Xml.member "PrivateDnsName" xml) String.parse
    ; remove_private_dns_name =
        Aws.Util.option_bind (Aws.Xml.member "RemovePrivateDnsName" xml) Boolean.parse
    ; acceptance_required =
        Aws.Util.option_bind (Aws.Xml.member "AcceptanceRequired" xml) Boolean.parse
    ; add_network_load_balancer_arns =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AddNetworkLoadBalancerArn" xml)
             ValueStringList.parse)
    ; remove_network_load_balancer_arns =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "RemoveNetworkLoadBalancerArn" xml)
             ValueStringList.parse)
    ; add_gateway_load_balancer_arns =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AddGatewayLoadBalancerArn" xml)
             ValueStringList.parse)
    ; remove_gateway_load_balancer_arns =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "RemoveGatewayLoadBalancerArn" xml)
             ValueStringList.parse)
    ; add_supported_ip_address_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AddSupportedIpAddressType" xml)
             ValueStringList.parse)
    ; remove_supported_ip_address_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "RemoveSupportedIpAddressType" xml)
             ValueStringList.parse)
    ; add_supported_regions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AddSupportedRegion" xml)
             ValueStringList.parse)
    ; remove_supported_regions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "RemoveSupportedRegion" xml)
             ValueStringList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "RemoveSupportedRegion"
              , ValueStringList.to_query v.remove_supported_regions ))
       ; Some
           (Aws.Query.Pair
              ("AddSupportedRegion", ValueStringList.to_query v.add_supported_regions))
       ; Some
           (Aws.Query.Pair
              ( "RemoveSupportedIpAddressType"
              , ValueStringList.to_query v.remove_supported_ip_address_types ))
       ; Some
           (Aws.Query.Pair
              ( "AddSupportedIpAddressType"
              , ValueStringList.to_query v.add_supported_ip_address_types ))
       ; Some
           (Aws.Query.Pair
              ( "RemoveGatewayLoadBalancerArn"
              , ValueStringList.to_query v.remove_gateway_load_balancer_arns ))
       ; Some
           (Aws.Query.Pair
              ( "AddGatewayLoadBalancerArn"
              , ValueStringList.to_query v.add_gateway_load_balancer_arns ))
       ; Some
           (Aws.Query.Pair
              ( "RemoveNetworkLoadBalancerArn"
              , ValueStringList.to_query v.remove_network_load_balancer_arns ))
       ; Some
           (Aws.Query.Pair
              ( "AddNetworkLoadBalancerArn"
              , ValueStringList.to_query v.add_network_load_balancer_arns ))
       ; Aws.Util.option_map v.acceptance_required (fun f ->
             Aws.Query.Pair ("AcceptanceRequired", Boolean.to_query f))
       ; Aws.Util.option_map v.remove_private_dns_name (fun f ->
             Aws.Query.Pair ("RemovePrivateDnsName", Boolean.to_query f))
       ; Aws.Util.option_map v.private_dns_name (fun f ->
             Aws.Query.Pair ("PrivateDnsName", String.to_query f))
       ; Some (Aws.Query.Pair ("ServiceId", String.to_query v.service_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("RemoveSupportedRegion", ValueStringList.to_json v.remove_supported_regions)
       ; Some ("AddSupportedRegion", ValueStringList.to_json v.add_supported_regions)
       ; Some
           ( "RemoveSupportedIpAddressType"
           , ValueStringList.to_json v.remove_supported_ip_address_types )
       ; Some
           ( "AddSupportedIpAddressType"
           , ValueStringList.to_json v.add_supported_ip_address_types )
       ; Some
           ( "RemoveGatewayLoadBalancerArn"
           , ValueStringList.to_json v.remove_gateway_load_balancer_arns )
       ; Some
           ( "AddGatewayLoadBalancerArn"
           , ValueStringList.to_json v.add_gateway_load_balancer_arns )
       ; Some
           ( "RemoveNetworkLoadBalancerArn"
           , ValueStringList.to_json v.remove_network_load_balancer_arns )
       ; Some
           ( "AddNetworkLoadBalancerArn"
           , ValueStringList.to_json v.add_network_load_balancer_arns )
       ; Aws.Util.option_map v.acceptance_required (fun f ->
             "AcceptanceRequired", Boolean.to_json f)
       ; Aws.Util.option_map v.remove_private_dns_name (fun f ->
             "RemovePrivateDnsName", Boolean.to_json f)
       ; Aws.Util.option_map v.private_dns_name (fun f ->
             "PrivateDnsName", String.to_json f)
       ; Some ("ServiceId", String.to_json v.service_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; service_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ServiceId"))
  ; private_dns_name =
      Aws.Util.option_map (Aws.Json.lookup j "PrivateDnsName") String.of_json
  ; remove_private_dns_name =
      Aws.Util.option_map (Aws.Json.lookup j "RemovePrivateDnsName") Boolean.of_json
  ; acceptance_required =
      Aws.Util.option_map (Aws.Json.lookup j "AcceptanceRequired") Boolean.of_json
  ; add_network_load_balancer_arns =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AddNetworkLoadBalancerArn"))
  ; remove_network_load_balancer_arns =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "RemoveNetworkLoadBalancerArn"))
  ; add_gateway_load_balancer_arns =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AddGatewayLoadBalancerArn"))
  ; remove_gateway_load_balancer_arns =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "RemoveGatewayLoadBalancerArn"))
  ; add_supported_ip_address_types =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AddSupportedIpAddressType"))
  ; remove_supported_ip_address_types =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "RemoveSupportedIpAddressType"))
  ; add_supported_regions =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AddSupportedRegion"))
  ; remove_supported_regions =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "RemoveSupportedRegion"))
  }
