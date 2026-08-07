open Aws.BaseTypes

type t =
  { assign_ipv6_address_on_creation : AttributeBooleanValue.t option
  ; map_public_ip_on_launch : AttributeBooleanValue.t option
  ; subnet_id : String.t
  ; map_customer_owned_ip_on_launch : AttributeBooleanValue.t option
  ; customer_owned_ipv4_pool : String.t option
  ; enable_dns64 : AttributeBooleanValue.t option
  ; private_dns_hostname_type_on_launch : HostnameType.t option
  ; enable_resource_name_dns_a_record_on_launch : AttributeBooleanValue.t option
  ; enable_resource_name_dns_a_a_a_a_record_on_launch : AttributeBooleanValue.t option
  ; enable_lni_at_device_index : Integer.t option
  ; disable_lni_at_device_index : AttributeBooleanValue.t option
  }

let make
    ?assign_ipv6_address_on_creation
    ?map_public_ip_on_launch
    ~subnet_id
    ?map_customer_owned_ip_on_launch
    ?customer_owned_ipv4_pool
    ?enable_dns64
    ?private_dns_hostname_type_on_launch
    ?enable_resource_name_dns_a_record_on_launch
    ?enable_resource_name_dns_a_a_a_a_record_on_launch
    ?enable_lni_at_device_index
    ?disable_lni_at_device_index
    () =
  { assign_ipv6_address_on_creation
  ; map_public_ip_on_launch
  ; subnet_id
  ; map_customer_owned_ip_on_launch
  ; customer_owned_ipv4_pool
  ; enable_dns64
  ; private_dns_hostname_type_on_launch
  ; enable_resource_name_dns_a_record_on_launch
  ; enable_resource_name_dns_a_a_a_a_record_on_launch
  ; enable_lni_at_device_index
  ; disable_lni_at_device_index
  }

let parse xml =
  Some
    { assign_ipv6_address_on_creation =
        Aws.Util.option_bind
          (Aws.Xml.member "AssignIpv6AddressOnCreation" xml)
          AttributeBooleanValue.parse
    ; map_public_ip_on_launch =
        Aws.Util.option_bind
          (Aws.Xml.member "MapPublicIpOnLaunch" xml)
          AttributeBooleanValue.parse
    ; subnet_id =
        Aws.Xml.required
          "subnetId"
          (Aws.Util.option_bind (Aws.Xml.member "subnetId" xml) String.parse)
    ; map_customer_owned_ip_on_launch =
        Aws.Util.option_bind
          (Aws.Xml.member "MapCustomerOwnedIpOnLaunch" xml)
          AttributeBooleanValue.parse
    ; customer_owned_ipv4_pool =
        Aws.Util.option_bind (Aws.Xml.member "CustomerOwnedIpv4Pool" xml) String.parse
    ; enable_dns64 =
        Aws.Util.option_bind
          (Aws.Xml.member "EnableDns64" xml)
          AttributeBooleanValue.parse
    ; private_dns_hostname_type_on_launch =
        Aws.Util.option_bind
          (Aws.Xml.member "PrivateDnsHostnameTypeOnLaunch" xml)
          HostnameType.parse
    ; enable_resource_name_dns_a_record_on_launch =
        Aws.Util.option_bind
          (Aws.Xml.member "EnableResourceNameDnsARecordOnLaunch" xml)
          AttributeBooleanValue.parse
    ; enable_resource_name_dns_a_a_a_a_record_on_launch =
        Aws.Util.option_bind
          (Aws.Xml.member "EnableResourceNameDnsAAAARecordOnLaunch" xml)
          AttributeBooleanValue.parse
    ; enable_lni_at_device_index =
        Aws.Util.option_bind (Aws.Xml.member "EnableLniAtDeviceIndex" xml) Integer.parse
    ; disable_lni_at_device_index =
        Aws.Util.option_bind
          (Aws.Xml.member "DisableLniAtDeviceIndex" xml)
          AttributeBooleanValue.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.disable_lni_at_device_index (fun f ->
             Aws.Query.Pair ("DisableLniAtDeviceIndex", AttributeBooleanValue.to_query f))
       ; Aws.Util.option_map v.enable_lni_at_device_index (fun f ->
             Aws.Query.Pair ("EnableLniAtDeviceIndex", Integer.to_query f))
       ; Aws.Util.option_map v.enable_resource_name_dns_a_a_a_a_record_on_launch (fun f ->
             Aws.Query.Pair
               ( "EnableResourceNameDnsAAAARecordOnLaunch"
               , AttributeBooleanValue.to_query f ))
       ; Aws.Util.option_map v.enable_resource_name_dns_a_record_on_launch (fun f ->
             Aws.Query.Pair
               ("EnableResourceNameDnsARecordOnLaunch", AttributeBooleanValue.to_query f))
       ; Aws.Util.option_map v.private_dns_hostname_type_on_launch (fun f ->
             Aws.Query.Pair ("PrivateDnsHostnameTypeOnLaunch", HostnameType.to_query f))
       ; Aws.Util.option_map v.enable_dns64 (fun f ->
             Aws.Query.Pair ("EnableDns64", AttributeBooleanValue.to_query f))
       ; Aws.Util.option_map v.customer_owned_ipv4_pool (fun f ->
             Aws.Query.Pair ("CustomerOwnedIpv4Pool", String.to_query f))
       ; Aws.Util.option_map v.map_customer_owned_ip_on_launch (fun f ->
             Aws.Query.Pair
               ("MapCustomerOwnedIpOnLaunch", AttributeBooleanValue.to_query f))
       ; Some (Aws.Query.Pair ("SubnetId", String.to_query v.subnet_id))
       ; Aws.Util.option_map v.map_public_ip_on_launch (fun f ->
             Aws.Query.Pair ("MapPublicIpOnLaunch", AttributeBooleanValue.to_query f))
       ; Aws.Util.option_map v.assign_ipv6_address_on_creation (fun f ->
             Aws.Query.Pair
               ("AssignIpv6AddressOnCreation", AttributeBooleanValue.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.disable_lni_at_device_index (fun f ->
             "DisableLniAtDeviceIndex", AttributeBooleanValue.to_json f)
       ; Aws.Util.option_map v.enable_lni_at_device_index (fun f ->
             "EnableLniAtDeviceIndex", Integer.to_json f)
       ; Aws.Util.option_map v.enable_resource_name_dns_a_a_a_a_record_on_launch (fun f ->
             "EnableResourceNameDnsAAAARecordOnLaunch", AttributeBooleanValue.to_json f)
       ; Aws.Util.option_map v.enable_resource_name_dns_a_record_on_launch (fun f ->
             "EnableResourceNameDnsARecordOnLaunch", AttributeBooleanValue.to_json f)
       ; Aws.Util.option_map v.private_dns_hostname_type_on_launch (fun f ->
             "PrivateDnsHostnameTypeOnLaunch", HostnameType.to_json f)
       ; Aws.Util.option_map v.enable_dns64 (fun f ->
             "EnableDns64", AttributeBooleanValue.to_json f)
       ; Aws.Util.option_map v.customer_owned_ipv4_pool (fun f ->
             "CustomerOwnedIpv4Pool", String.to_json f)
       ; Aws.Util.option_map v.map_customer_owned_ip_on_launch (fun f ->
             "MapCustomerOwnedIpOnLaunch", AttributeBooleanValue.to_json f)
       ; Some ("subnetId", String.to_json v.subnet_id)
       ; Aws.Util.option_map v.map_public_ip_on_launch (fun f ->
             "MapPublicIpOnLaunch", AttributeBooleanValue.to_json f)
       ; Aws.Util.option_map v.assign_ipv6_address_on_creation (fun f ->
             "AssignIpv6AddressOnCreation", AttributeBooleanValue.to_json f)
       ])

let of_json j =
  { assign_ipv6_address_on_creation =
      Aws.Util.option_map
        (Aws.Json.lookup j "AssignIpv6AddressOnCreation")
        AttributeBooleanValue.of_json
  ; map_public_ip_on_launch =
      Aws.Util.option_map
        (Aws.Json.lookup j "MapPublicIpOnLaunch")
        AttributeBooleanValue.of_json
  ; subnet_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "subnetId"))
  ; map_customer_owned_ip_on_launch =
      Aws.Util.option_map
        (Aws.Json.lookup j "MapCustomerOwnedIpOnLaunch")
        AttributeBooleanValue.of_json
  ; customer_owned_ipv4_pool =
      Aws.Util.option_map (Aws.Json.lookup j "CustomerOwnedIpv4Pool") String.of_json
  ; enable_dns64 =
      Aws.Util.option_map (Aws.Json.lookup j "EnableDns64") AttributeBooleanValue.of_json
  ; private_dns_hostname_type_on_launch =
      Aws.Util.option_map
        (Aws.Json.lookup j "PrivateDnsHostnameTypeOnLaunch")
        HostnameType.of_json
  ; enable_resource_name_dns_a_record_on_launch =
      Aws.Util.option_map
        (Aws.Json.lookup j "EnableResourceNameDnsARecordOnLaunch")
        AttributeBooleanValue.of_json
  ; enable_resource_name_dns_a_a_a_a_record_on_launch =
      Aws.Util.option_map
        (Aws.Json.lookup j "EnableResourceNameDnsAAAARecordOnLaunch")
        AttributeBooleanValue.of_json
  ; enable_lni_at_device_index =
      Aws.Util.option_map (Aws.Json.lookup j "EnableLniAtDeviceIndex") Integer.of_json
  ; disable_lni_at_device_index =
      Aws.Util.option_map
        (Aws.Json.lookup j "DisableLniAtDeviceIndex")
        AttributeBooleanValue.of_json
  }
