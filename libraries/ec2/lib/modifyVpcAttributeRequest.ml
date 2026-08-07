open Aws.BaseTypes

type t =
  { enable_dns_hostnames : AttributeBooleanValue.t option
  ; enable_dns_support : AttributeBooleanValue.t option
  ; vpc_id : String.t
  ; enable_network_address_usage_metrics : AttributeBooleanValue.t option
  }

let make
    ?enable_dns_hostnames
    ?enable_dns_support
    ~vpc_id
    ?enable_network_address_usage_metrics
    () =
  { enable_dns_hostnames
  ; enable_dns_support
  ; vpc_id
  ; enable_network_address_usage_metrics
  }

let parse xml =
  Some
    { enable_dns_hostnames =
        Aws.Util.option_bind
          (Aws.Xml.member "EnableDnsHostnames" xml)
          AttributeBooleanValue.parse
    ; enable_dns_support =
        Aws.Util.option_bind
          (Aws.Xml.member "EnableDnsSupport" xml)
          AttributeBooleanValue.parse
    ; vpc_id =
        Aws.Xml.required
          "vpcId"
          (Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse)
    ; enable_network_address_usage_metrics =
        Aws.Util.option_bind
          (Aws.Xml.member "EnableNetworkAddressUsageMetrics" xml)
          AttributeBooleanValue.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.enable_network_address_usage_metrics (fun f ->
             Aws.Query.Pair
               ("EnableNetworkAddressUsageMetrics", AttributeBooleanValue.to_query f))
       ; Some (Aws.Query.Pair ("VpcId", String.to_query v.vpc_id))
       ; Aws.Util.option_map v.enable_dns_support (fun f ->
             Aws.Query.Pair ("EnableDnsSupport", AttributeBooleanValue.to_query f))
       ; Aws.Util.option_map v.enable_dns_hostnames (fun f ->
             Aws.Query.Pair ("EnableDnsHostnames", AttributeBooleanValue.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.enable_network_address_usage_metrics (fun f ->
             "EnableNetworkAddressUsageMetrics", AttributeBooleanValue.to_json f)
       ; Some ("vpcId", String.to_json v.vpc_id)
       ; Aws.Util.option_map v.enable_dns_support (fun f ->
             "EnableDnsSupport", AttributeBooleanValue.to_json f)
       ; Aws.Util.option_map v.enable_dns_hostnames (fun f ->
             "EnableDnsHostnames", AttributeBooleanValue.to_json f)
       ])

let of_json j =
  { enable_dns_hostnames =
      Aws.Util.option_map
        (Aws.Json.lookup j "EnableDnsHostnames")
        AttributeBooleanValue.of_json
  ; enable_dns_support =
      Aws.Util.option_map
        (Aws.Json.lookup j "EnableDnsSupport")
        AttributeBooleanValue.of_json
  ; vpc_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "vpcId"))
  ; enable_network_address_usage_metrics =
      Aws.Util.option_map
        (Aws.Json.lookup j "EnableNetworkAddressUsageMetrics")
        AttributeBooleanValue.of_json
  }
