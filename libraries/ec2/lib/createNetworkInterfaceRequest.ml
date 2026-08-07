open Aws.BaseTypes

type t =
  { ipv4_prefixes : Ipv4PrefixList.t
  ; ipv4_prefix_count : Integer.t option
  ; ipv6_prefixes : Ipv6PrefixList.t
  ; ipv6_prefix_count : Integer.t option
  ; interface_type : NetworkInterfaceCreationType.t option
  ; tag_specifications : TagSpecificationList.t
  ; client_token : String.t option
  ; enable_primary_ipv6 : Boolean.t option
  ; connection_tracking_specification : ConnectionTrackingSpecificationRequest.t option
  ; operator : OperatorRequest.t option
  ; subnet_id : String.t
  ; description : String.t option
  ; private_ip_address : String.t option
  ; groups : SecurityGroupIdStringList.t
  ; private_ip_addresses : PrivateIpAddressSpecificationList.t
  ; secondary_private_ip_address_count : Integer.t option
  ; ipv6_addresses : InstanceIpv6AddressList.t
  ; ipv6_address_count : Integer.t option
  ; dry_run : Boolean.t option
  }

let make
    ?(ipv4_prefixes = [])
    ?ipv4_prefix_count
    ?(ipv6_prefixes = [])
    ?ipv6_prefix_count
    ?interface_type
    ?(tag_specifications = [])
    ?client_token
    ?enable_primary_ipv6
    ?connection_tracking_specification
    ?operator
    ~subnet_id
    ?description
    ?private_ip_address
    ?(groups = [])
    ?(private_ip_addresses = [])
    ?secondary_private_ip_address_count
    ?(ipv6_addresses = [])
    ?ipv6_address_count
    ?dry_run
    () =
  { ipv4_prefixes
  ; ipv4_prefix_count
  ; ipv6_prefixes
  ; ipv6_prefix_count
  ; interface_type
  ; tag_specifications
  ; client_token
  ; enable_primary_ipv6
  ; connection_tracking_specification
  ; operator
  ; subnet_id
  ; description
  ; private_ip_address
  ; groups
  ; private_ip_addresses
  ; secondary_private_ip_address_count
  ; ipv6_addresses
  ; ipv6_address_count
  ; dry_run
  }

let parse xml =
  Some
    { ipv4_prefixes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Ipv4Prefix" xml) Ipv4PrefixList.parse)
    ; ipv4_prefix_count =
        Aws.Util.option_bind (Aws.Xml.member "Ipv4PrefixCount" xml) Integer.parse
    ; ipv6_prefixes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Ipv6Prefix" xml) Ipv6PrefixList.parse)
    ; ipv6_prefix_count =
        Aws.Util.option_bind (Aws.Xml.member "Ipv6PrefixCount" xml) Integer.parse
    ; interface_type =
        Aws.Util.option_bind
          (Aws.Xml.member "InterfaceType" xml)
          NetworkInterfaceCreationType.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; enable_primary_ipv6 =
        Aws.Util.option_bind (Aws.Xml.member "EnablePrimaryIpv6" xml) Boolean.parse
    ; connection_tracking_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "ConnectionTrackingSpecification" xml)
          ConnectionTrackingSpecificationRequest.parse
    ; operator =
        Aws.Util.option_bind (Aws.Xml.member "Operator" xml) OperatorRequest.parse
    ; subnet_id =
        Aws.Xml.required
          "subnetId"
          (Aws.Util.option_bind (Aws.Xml.member "subnetId" xml) String.parse)
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; private_ip_address =
        Aws.Util.option_bind (Aws.Xml.member "privateIpAddress" xml) String.parse
    ; groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SecurityGroupId" xml)
             SecurityGroupIdStringList.parse)
    ; private_ip_addresses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "privateIpAddresses" xml)
             PrivateIpAddressSpecificationList.parse)
    ; secondary_private_ip_address_count =
        Aws.Util.option_bind
          (Aws.Xml.member "secondaryPrivateIpAddressCount" xml)
          Integer.parse
    ; ipv6_addresses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ipv6Addresses" xml)
             InstanceIpv6AddressList.parse)
    ; ipv6_address_count =
        Aws.Util.option_bind (Aws.Xml.member "ipv6AddressCount" xml) Integer.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.ipv6_address_count (fun f ->
             Aws.Query.Pair ("Ipv6AddressCount", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ("Ipv6Addresses", InstanceIpv6AddressList.to_query v.ipv6_addresses))
       ; Aws.Util.option_map v.secondary_private_ip_address_count (fun f ->
             Aws.Query.Pair ("SecondaryPrivateIpAddressCount", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "PrivateIpAddresses"
              , PrivateIpAddressSpecificationList.to_query v.private_ip_addresses ))
       ; Some
           (Aws.Query.Pair ("SecurityGroupId", SecurityGroupIdStringList.to_query v.groups))
       ; Aws.Util.option_map v.private_ip_address (fun f ->
             Aws.Query.Pair ("PrivateIpAddress", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Some (Aws.Query.Pair ("SubnetId", String.to_query v.subnet_id))
       ; Aws.Util.option_map v.operator (fun f ->
             Aws.Query.Pair ("Operator", OperatorRequest.to_query f))
       ; Aws.Util.option_map v.connection_tracking_specification (fun f ->
             Aws.Query.Pair
               ( "ConnectionTrackingSpecification"
               , ConnectionTrackingSpecificationRequest.to_query f ))
       ; Aws.Util.option_map v.enable_primary_ipv6 (fun f ->
             Aws.Query.Pair ("EnablePrimaryIpv6", Boolean.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.interface_type (fun f ->
             Aws.Query.Pair ("InterfaceType", NetworkInterfaceCreationType.to_query f))
       ; Aws.Util.option_map v.ipv6_prefix_count (fun f ->
             Aws.Query.Pair ("Ipv6PrefixCount", Integer.to_query f))
       ; Some (Aws.Query.Pair ("Ipv6Prefix", Ipv6PrefixList.to_query v.ipv6_prefixes))
       ; Aws.Util.option_map v.ipv4_prefix_count (fun f ->
             Aws.Query.Pair ("Ipv4PrefixCount", Integer.to_query f))
       ; Some (Aws.Query.Pair ("Ipv4Prefix", Ipv4PrefixList.to_query v.ipv4_prefixes))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.ipv6_address_count (fun f ->
             "ipv6AddressCount", Integer.to_json f)
       ; Some ("ipv6Addresses", InstanceIpv6AddressList.to_json v.ipv6_addresses)
       ; Aws.Util.option_map v.secondary_private_ip_address_count (fun f ->
             "secondaryPrivateIpAddressCount", Integer.to_json f)
       ; Some
           ( "privateIpAddresses"
           , PrivateIpAddressSpecificationList.to_json v.private_ip_addresses )
       ; Some ("SecurityGroupId", SecurityGroupIdStringList.to_json v.groups)
       ; Aws.Util.option_map v.private_ip_address (fun f ->
             "privateIpAddress", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Some ("subnetId", String.to_json v.subnet_id)
       ; Aws.Util.option_map v.operator (fun f -> "Operator", OperatorRequest.to_json f)
       ; Aws.Util.option_map v.connection_tracking_specification (fun f ->
             ( "ConnectionTrackingSpecification"
             , ConnectionTrackingSpecificationRequest.to_json f ))
       ; Aws.Util.option_map v.enable_primary_ipv6 (fun f ->
             "EnablePrimaryIpv6", Boolean.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.interface_type (fun f ->
             "InterfaceType", NetworkInterfaceCreationType.to_json f)
       ; Aws.Util.option_map v.ipv6_prefix_count (fun f ->
             "Ipv6PrefixCount", Integer.to_json f)
       ; Some ("Ipv6Prefix", Ipv6PrefixList.to_json v.ipv6_prefixes)
       ; Aws.Util.option_map v.ipv4_prefix_count (fun f ->
             "Ipv4PrefixCount", Integer.to_json f)
       ; Some ("Ipv4Prefix", Ipv4PrefixList.to_json v.ipv4_prefixes)
       ])

let of_json j =
  { ipv4_prefixes =
      Ipv4PrefixList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Ipv4Prefix"))
  ; ipv4_prefix_count =
      Aws.Util.option_map (Aws.Json.lookup j "Ipv4PrefixCount") Integer.of_json
  ; ipv6_prefixes =
      Ipv6PrefixList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Ipv6Prefix"))
  ; ipv6_prefix_count =
      Aws.Util.option_map (Aws.Json.lookup j "Ipv6PrefixCount") Integer.of_json
  ; interface_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "InterfaceType")
        NetworkInterfaceCreationType.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; enable_primary_ipv6 =
      Aws.Util.option_map (Aws.Json.lookup j "EnablePrimaryIpv6") Boolean.of_json
  ; connection_tracking_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "ConnectionTrackingSpecification")
        ConnectionTrackingSpecificationRequest.of_json
  ; operator = Aws.Util.option_map (Aws.Json.lookup j "Operator") OperatorRequest.of_json
  ; subnet_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "subnetId"))
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; private_ip_address =
      Aws.Util.option_map (Aws.Json.lookup j "privateIpAddress") String.of_json
  ; groups =
      SecurityGroupIdStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroupId"))
  ; private_ip_addresses =
      PrivateIpAddressSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "privateIpAddresses"))
  ; secondary_private_ip_address_count =
      Aws.Util.option_map
        (Aws.Json.lookup j "secondaryPrivateIpAddressCount")
        Integer.of_json
  ; ipv6_addresses =
      InstanceIpv6AddressList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ipv6Addresses"))
  ; ipv6_address_count =
      Aws.Util.option_map (Aws.Json.lookup j "ipv6AddressCount") Integer.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  }
