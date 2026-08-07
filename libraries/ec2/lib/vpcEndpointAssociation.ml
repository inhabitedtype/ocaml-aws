open Aws.BaseTypes

type t =
  { id : String.t option
  ; vpc_endpoint_id : String.t option
  ; service_network_arn : String.t option
  ; service_network_name : String.t option
  ; associated_resource_accessibility : String.t option
  ; failure_reason : String.t option
  ; failure_code : String.t option
  ; dns_entry : DnsEntry.t option
  ; private_dns_entry : DnsEntry.t option
  ; associated_resource_arn : String.t option
  ; resource_configuration_group_arn : String.t option
  ; tags : TagList.t
  }

let make
    ?id
    ?vpc_endpoint_id
    ?service_network_arn
    ?service_network_name
    ?associated_resource_accessibility
    ?failure_reason
    ?failure_code
    ?dns_entry
    ?private_dns_entry
    ?associated_resource_arn
    ?resource_configuration_group_arn
    ?(tags = [])
    () =
  { id
  ; vpc_endpoint_id
  ; service_network_arn
  ; service_network_name
  ; associated_resource_accessibility
  ; failure_reason
  ; failure_code
  ; dns_entry
  ; private_dns_entry
  ; associated_resource_arn
  ; resource_configuration_group_arn
  ; tags
  }

let parse xml =
  Some
    { id = Aws.Util.option_bind (Aws.Xml.member "id" xml) String.parse
    ; vpc_endpoint_id =
        Aws.Util.option_bind (Aws.Xml.member "vpcEndpointId" xml) String.parse
    ; service_network_arn =
        Aws.Util.option_bind (Aws.Xml.member "serviceNetworkArn" xml) String.parse
    ; service_network_name =
        Aws.Util.option_bind (Aws.Xml.member "serviceNetworkName" xml) String.parse
    ; associated_resource_accessibility =
        Aws.Util.option_bind
          (Aws.Xml.member "associatedResourceAccessibility" xml)
          String.parse
    ; failure_reason =
        Aws.Util.option_bind (Aws.Xml.member "failureReason" xml) String.parse
    ; failure_code = Aws.Util.option_bind (Aws.Xml.member "failureCode" xml) String.parse
    ; dns_entry = Aws.Util.option_bind (Aws.Xml.member "dnsEntry" xml) DnsEntry.parse
    ; private_dns_entry =
        Aws.Util.option_bind (Aws.Xml.member "privateDnsEntry" xml) DnsEntry.parse
    ; associated_resource_arn =
        Aws.Util.option_bind (Aws.Xml.member "associatedResourceArn" xml) String.parse
    ; resource_configuration_group_arn =
        Aws.Util.option_bind
          (Aws.Xml.member "resourceConfigurationGroupArn" xml)
          String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.resource_configuration_group_arn (fun f ->
             Aws.Query.Pair ("ResourceConfigurationGroupArn", String.to_query f))
       ; Aws.Util.option_map v.associated_resource_arn (fun f ->
             Aws.Query.Pair ("AssociatedResourceArn", String.to_query f))
       ; Aws.Util.option_map v.private_dns_entry (fun f ->
             Aws.Query.Pair ("PrivateDnsEntry", DnsEntry.to_query f))
       ; Aws.Util.option_map v.dns_entry (fun f ->
             Aws.Query.Pair ("DnsEntry", DnsEntry.to_query f))
       ; Aws.Util.option_map v.failure_code (fun f ->
             Aws.Query.Pair ("FailureCode", String.to_query f))
       ; Aws.Util.option_map v.failure_reason (fun f ->
             Aws.Query.Pair ("FailureReason", String.to_query f))
       ; Aws.Util.option_map v.associated_resource_accessibility (fun f ->
             Aws.Query.Pair ("AssociatedResourceAccessibility", String.to_query f))
       ; Aws.Util.option_map v.service_network_name (fun f ->
             Aws.Query.Pair ("ServiceNetworkName", String.to_query f))
       ; Aws.Util.option_map v.service_network_arn (fun f ->
             Aws.Query.Pair ("ServiceNetworkArn", String.to_query f))
       ; Aws.Util.option_map v.vpc_endpoint_id (fun f ->
             Aws.Query.Pair ("VpcEndpointId", String.to_query f))
       ; Aws.Util.option_map v.id (fun f -> Aws.Query.Pair ("Id", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.resource_configuration_group_arn (fun f ->
             "resourceConfigurationGroupArn", String.to_json f)
       ; Aws.Util.option_map v.associated_resource_arn (fun f ->
             "associatedResourceArn", String.to_json f)
       ; Aws.Util.option_map v.private_dns_entry (fun f ->
             "privateDnsEntry", DnsEntry.to_json f)
       ; Aws.Util.option_map v.dns_entry (fun f -> "dnsEntry", DnsEntry.to_json f)
       ; Aws.Util.option_map v.failure_code (fun f -> "failureCode", String.to_json f)
       ; Aws.Util.option_map v.failure_reason (fun f -> "failureReason", String.to_json f)
       ; Aws.Util.option_map v.associated_resource_accessibility (fun f ->
             "associatedResourceAccessibility", String.to_json f)
       ; Aws.Util.option_map v.service_network_name (fun f ->
             "serviceNetworkName", String.to_json f)
       ; Aws.Util.option_map v.service_network_arn (fun f ->
             "serviceNetworkArn", String.to_json f)
       ; Aws.Util.option_map v.vpc_endpoint_id (fun f ->
             "vpcEndpointId", String.to_json f)
       ; Aws.Util.option_map v.id (fun f -> "id", String.to_json f)
       ])

let of_json j =
  { id = Aws.Util.option_map (Aws.Json.lookup j "id") String.of_json
  ; vpc_endpoint_id =
      Aws.Util.option_map (Aws.Json.lookup j "vpcEndpointId") String.of_json
  ; service_network_arn =
      Aws.Util.option_map (Aws.Json.lookup j "serviceNetworkArn") String.of_json
  ; service_network_name =
      Aws.Util.option_map (Aws.Json.lookup j "serviceNetworkName") String.of_json
  ; associated_resource_accessibility =
      Aws.Util.option_map
        (Aws.Json.lookup j "associatedResourceAccessibility")
        String.of_json
  ; failure_reason =
      Aws.Util.option_map (Aws.Json.lookup j "failureReason") String.of_json
  ; failure_code = Aws.Util.option_map (Aws.Json.lookup j "failureCode") String.of_json
  ; dns_entry = Aws.Util.option_map (Aws.Json.lookup j "dnsEntry") DnsEntry.of_json
  ; private_dns_entry =
      Aws.Util.option_map (Aws.Json.lookup j "privateDnsEntry") DnsEntry.of_json
  ; associated_resource_arn =
      Aws.Util.option_map (Aws.Json.lookup j "associatedResourceArn") String.of_json
  ; resource_configuration_group_arn =
      Aws.Util.option_map
        (Aws.Json.lookup j "resourceConfigurationGroupArn")
        String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
