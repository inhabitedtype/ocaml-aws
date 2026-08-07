open Aws.BaseTypes

type t =
  { enable_dns_hostnames : AttributeBooleanValue.t option
  ; enable_dns_support : AttributeBooleanValue.t option
  ; enable_network_address_usage_metrics : AttributeBooleanValue.t option
  ; vpc_id : String.t option
  }

let make
    ?enable_dns_hostnames
    ?enable_dns_support
    ?enable_network_address_usage_metrics
    ?vpc_id
    () =
  { enable_dns_hostnames
  ; enable_dns_support
  ; enable_network_address_usage_metrics
  ; vpc_id
  }

let parse xml =
  Some
    { enable_dns_hostnames =
        Aws.Util.option_bind
          (Aws.Xml.member "enableDnsHostnames" xml)
          AttributeBooleanValue.parse
    ; enable_dns_support =
        Aws.Util.option_bind
          (Aws.Xml.member "enableDnsSupport" xml)
          AttributeBooleanValue.parse
    ; enable_network_address_usage_metrics =
        Aws.Util.option_bind
          (Aws.Xml.member "enableNetworkAddressUsageMetrics" xml)
          AttributeBooleanValue.parse
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Aws.Util.option_map v.enable_network_address_usage_metrics (fun f ->
             Aws.Query.Pair
               ("EnableNetworkAddressUsageMetrics", AttributeBooleanValue.to_query f))
       ; Aws.Util.option_map v.enable_dns_support (fun f ->
             Aws.Query.Pair ("EnableDnsSupport", AttributeBooleanValue.to_query f))
       ; Aws.Util.option_map v.enable_dns_hostnames (fun f ->
             Aws.Query.Pair ("EnableDnsHostnames", AttributeBooleanValue.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpc_id (fun f -> "vpcId", String.to_json f)
       ; Aws.Util.option_map v.enable_network_address_usage_metrics (fun f ->
             "enableNetworkAddressUsageMetrics", AttributeBooleanValue.to_json f)
       ; Aws.Util.option_map v.enable_dns_support (fun f ->
             "enableDnsSupport", AttributeBooleanValue.to_json f)
       ; Aws.Util.option_map v.enable_dns_hostnames (fun f ->
             "enableDnsHostnames", AttributeBooleanValue.to_json f)
       ])

let of_json j =
  { enable_dns_hostnames =
      Aws.Util.option_map
        (Aws.Json.lookup j "enableDnsHostnames")
        AttributeBooleanValue.of_json
  ; enable_dns_support =
      Aws.Util.option_map
        (Aws.Json.lookup j "enableDnsSupport")
        AttributeBooleanValue.of_json
  ; enable_network_address_usage_metrics =
      Aws.Util.option_map
        (Aws.Json.lookup j "enableNetworkAddressUsageMetrics")
        AttributeBooleanValue.of_json
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "vpcId") String.of_json
  }
