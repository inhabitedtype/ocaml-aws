open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; acceptance_required : Boolean.t option
  ; private_dns_name : String.t option
  ; network_load_balancer_arns : ValueStringList.t
  ; gateway_load_balancer_arns : ValueStringList.t
  ; supported_ip_address_types : ValueStringList.t
  ; supported_regions : ValueStringList.t
  ; client_token : String.t option
  ; tag_specifications : TagSpecificationList.t
  }

let make
    ?dry_run
    ?acceptance_required
    ?private_dns_name
    ?(network_load_balancer_arns = [])
    ?(gateway_load_balancer_arns = [])
    ?(supported_ip_address_types = [])
    ?(supported_regions = [])
    ?client_token
    ?(tag_specifications = [])
    () =
  { dry_run
  ; acceptance_required
  ; private_dns_name
  ; network_load_balancer_arns
  ; gateway_load_balancer_arns
  ; supported_ip_address_types
  ; supported_regions
  ; client_token
  ; tag_specifications
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; acceptance_required =
        Aws.Util.option_bind (Aws.Xml.member "AcceptanceRequired" xml) Boolean.parse
    ; private_dns_name =
        Aws.Util.option_bind (Aws.Xml.member "PrivateDnsName" xml) String.parse
    ; network_load_balancer_arns =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "NetworkLoadBalancerArn" xml)
             ValueStringList.parse)
    ; gateway_load_balancer_arns =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "GatewayLoadBalancerArn" xml)
             ValueStringList.parse)
    ; supported_ip_address_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SupportedIpAddressType" xml)
             ValueStringList.parse)
    ; supported_regions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SupportedRegion" xml)
             ValueStringList.parse)
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("SupportedRegion", ValueStringList.to_query v.supported_regions))
       ; Some
           (Aws.Query.Pair
              ( "SupportedIpAddressType"
              , ValueStringList.to_query v.supported_ip_address_types ))
       ; Some
           (Aws.Query.Pair
              ( "GatewayLoadBalancerArn"
              , ValueStringList.to_query v.gateway_load_balancer_arns ))
       ; Some
           (Aws.Query.Pair
              ( "NetworkLoadBalancerArn"
              , ValueStringList.to_query v.network_load_balancer_arns ))
       ; Aws.Util.option_map v.private_dns_name (fun f ->
             Aws.Query.Pair ("PrivateDnsName", String.to_query f))
       ; Aws.Util.option_map v.acceptance_required (fun f ->
             Aws.Query.Pair ("AcceptanceRequired", Boolean.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Some ("SupportedRegion", ValueStringList.to_json v.supported_regions)
       ; Some
           ("SupportedIpAddressType", ValueStringList.to_json v.supported_ip_address_types)
       ; Some
           ("GatewayLoadBalancerArn", ValueStringList.to_json v.gateway_load_balancer_arns)
       ; Some
           ("NetworkLoadBalancerArn", ValueStringList.to_json v.network_load_balancer_arns)
       ; Aws.Util.option_map v.private_dns_name (fun f ->
             "PrivateDnsName", String.to_json f)
       ; Aws.Util.option_map v.acceptance_required (fun f ->
             "AcceptanceRequired", Boolean.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; acceptance_required =
      Aws.Util.option_map (Aws.Json.lookup j "AcceptanceRequired") Boolean.of_json
  ; private_dns_name =
      Aws.Util.option_map (Aws.Json.lookup j "PrivateDnsName") String.of_json
  ; network_load_balancer_arns =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "NetworkLoadBalancerArn"))
  ; gateway_load_balancer_arns =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "GatewayLoadBalancerArn"))
  ; supported_ip_address_types =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SupportedIpAddressType"))
  ; supported_regions =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SupportedRegion"))
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  }
