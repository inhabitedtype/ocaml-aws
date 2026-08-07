open Aws.BaseTypes

type t =
  { block_device_mappings : BlockDeviceMappingRequestList.t
  ; image_id : String.t option
  ; instance_type : InstanceType.t option
  ; ipv6_address_count : Integer.t option
  ; ipv6_addresses : InstanceIpv6AddressList.t
  ; kernel_id : String.t option
  ; key_name : String.t option
  ; max_count : Integer.t
  ; min_count : Integer.t
  ; monitoring : RunInstancesMonitoringEnabled.t option
  ; placement : Placement.t option
  ; ramdisk_id : String.t option
  ; security_group_ids : SecurityGroupIdStringList.t
  ; security_groups : SecurityGroupStringList.t
  ; subnet_id : String.t option
  ; user_data : String.t option
  ; elastic_gpu_specification : ElasticGpuSpecifications.t
  ; elastic_inference_accelerators : ElasticInferenceAccelerators.t
  ; tag_specifications : TagSpecificationList.t
  ; launch_template : LaunchTemplateSpecification.t option
  ; instance_market_options : InstanceMarketOptionsRequest.t option
  ; credit_specification : CreditSpecificationRequest.t option
  ; cpu_options : CpuOptionsRequest.t option
  ; capacity_reservation_specification : CapacityReservationSpecification.t option
  ; hibernation_options : HibernationOptionsRequest.t option
  ; license_specifications : LicenseSpecificationListRequest.t
  ; metadata_options : InstanceMetadataOptionsRequest.t option
  ; enclave_options : EnclaveOptionsRequest.t option
  ; private_dns_name_options : PrivateDnsNameOptionsRequest.t option
  ; maintenance_options : InstanceMaintenanceOptionsRequest.t option
  ; disable_api_stop : Boolean.t option
  ; enable_primary_ipv6 : Boolean.t option
  ; network_performance_options : InstanceNetworkPerformanceOptionsRequest.t option
  ; operator : OperatorRequest.t option
  ; secondary_interfaces : InstanceSecondaryInterfaceSpecificationListRequest.t
  ; dry_run : Boolean.t option
  ; disable_api_termination : Boolean.t option
  ; instance_initiated_shutdown_behavior : ShutdownBehavior.t option
  ; private_ip_address : String.t option
  ; client_token : String.t option
  ; additional_info : String.t option
  ; network_interfaces : InstanceNetworkInterfaceSpecificationList.t
  ; iam_instance_profile : IamInstanceProfileSpecification.t option
  ; ebs_optimized : Boolean.t option
  }

let make
    ?(block_device_mappings = [])
    ?image_id
    ?instance_type
    ?ipv6_address_count
    ?(ipv6_addresses = [])
    ?kernel_id
    ?key_name
    ~max_count
    ~min_count
    ?monitoring
    ?placement
    ?ramdisk_id
    ?(security_group_ids = [])
    ?(security_groups = [])
    ?subnet_id
    ?user_data
    ?(elastic_gpu_specification = [])
    ?(elastic_inference_accelerators = [])
    ?(tag_specifications = [])
    ?launch_template
    ?instance_market_options
    ?credit_specification
    ?cpu_options
    ?capacity_reservation_specification
    ?hibernation_options
    ?(license_specifications = [])
    ?metadata_options
    ?enclave_options
    ?private_dns_name_options
    ?maintenance_options
    ?disable_api_stop
    ?enable_primary_ipv6
    ?network_performance_options
    ?operator
    ?(secondary_interfaces = [])
    ?dry_run
    ?disable_api_termination
    ?instance_initiated_shutdown_behavior
    ?private_ip_address
    ?client_token
    ?additional_info
    ?(network_interfaces = [])
    ?iam_instance_profile
    ?ebs_optimized
    () =
  { block_device_mappings
  ; image_id
  ; instance_type
  ; ipv6_address_count
  ; ipv6_addresses
  ; kernel_id
  ; key_name
  ; max_count
  ; min_count
  ; monitoring
  ; placement
  ; ramdisk_id
  ; security_group_ids
  ; security_groups
  ; subnet_id
  ; user_data
  ; elastic_gpu_specification
  ; elastic_inference_accelerators
  ; tag_specifications
  ; launch_template
  ; instance_market_options
  ; credit_specification
  ; cpu_options
  ; capacity_reservation_specification
  ; hibernation_options
  ; license_specifications
  ; metadata_options
  ; enclave_options
  ; private_dns_name_options
  ; maintenance_options
  ; disable_api_stop
  ; enable_primary_ipv6
  ; network_performance_options
  ; operator
  ; secondary_interfaces
  ; dry_run
  ; disable_api_termination
  ; instance_initiated_shutdown_behavior
  ; private_ip_address
  ; client_token
  ; additional_info
  ; network_interfaces
  ; iam_instance_profile
  ; ebs_optimized
  }

let parse xml =
  Some
    { block_device_mappings =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "BlockDeviceMapping" xml)
             BlockDeviceMappingRequestList.parse)
    ; image_id = Aws.Util.option_bind (Aws.Xml.member "ImageId" xml) String.parse
    ; instance_type =
        Aws.Util.option_bind (Aws.Xml.member "InstanceType" xml) InstanceType.parse
    ; ipv6_address_count =
        Aws.Util.option_bind (Aws.Xml.member "Ipv6AddressCount" xml) Integer.parse
    ; ipv6_addresses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Ipv6Address" xml)
             InstanceIpv6AddressList.parse)
    ; kernel_id = Aws.Util.option_bind (Aws.Xml.member "KernelId" xml) String.parse
    ; key_name = Aws.Util.option_bind (Aws.Xml.member "KeyName" xml) String.parse
    ; max_count =
        Aws.Xml.required
          "MaxCount"
          (Aws.Util.option_bind (Aws.Xml.member "MaxCount" xml) Integer.parse)
    ; min_count =
        Aws.Xml.required
          "MinCount"
          (Aws.Util.option_bind (Aws.Xml.member "MinCount" xml) Integer.parse)
    ; monitoring =
        Aws.Util.option_bind
          (Aws.Xml.member "Monitoring" xml)
          RunInstancesMonitoringEnabled.parse
    ; placement = Aws.Util.option_bind (Aws.Xml.member "Placement" xml) Placement.parse
    ; ramdisk_id = Aws.Util.option_bind (Aws.Xml.member "RamdiskId" xml) String.parse
    ; security_group_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SecurityGroupId" xml)
             SecurityGroupIdStringList.parse)
    ; security_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SecurityGroup" xml)
             SecurityGroupStringList.parse)
    ; subnet_id = Aws.Util.option_bind (Aws.Xml.member "SubnetId" xml) String.parse
    ; user_data = Aws.Util.option_bind (Aws.Xml.member "UserData" xml) String.parse
    ; elastic_gpu_specification =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ElasticGpuSpecification" xml)
             ElasticGpuSpecifications.parse)
    ; elastic_inference_accelerators =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ElasticInferenceAccelerator" xml)
             ElasticInferenceAccelerators.parse)
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; launch_template =
        Aws.Util.option_bind
          (Aws.Xml.member "LaunchTemplate" xml)
          LaunchTemplateSpecification.parse
    ; instance_market_options =
        Aws.Util.option_bind
          (Aws.Xml.member "InstanceMarketOptions" xml)
          InstanceMarketOptionsRequest.parse
    ; credit_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "CreditSpecification" xml)
          CreditSpecificationRequest.parse
    ; cpu_options =
        Aws.Util.option_bind (Aws.Xml.member "CpuOptions" xml) CpuOptionsRequest.parse
    ; capacity_reservation_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "CapacityReservationSpecification" xml)
          CapacityReservationSpecification.parse
    ; hibernation_options =
        Aws.Util.option_bind
          (Aws.Xml.member "HibernationOptions" xml)
          HibernationOptionsRequest.parse
    ; license_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "LicenseSpecification" xml)
             LicenseSpecificationListRequest.parse)
    ; metadata_options =
        Aws.Util.option_bind
          (Aws.Xml.member "MetadataOptions" xml)
          InstanceMetadataOptionsRequest.parse
    ; enclave_options =
        Aws.Util.option_bind
          (Aws.Xml.member "EnclaveOptions" xml)
          EnclaveOptionsRequest.parse
    ; private_dns_name_options =
        Aws.Util.option_bind
          (Aws.Xml.member "PrivateDnsNameOptions" xml)
          PrivateDnsNameOptionsRequest.parse
    ; maintenance_options =
        Aws.Util.option_bind
          (Aws.Xml.member "MaintenanceOptions" xml)
          InstanceMaintenanceOptionsRequest.parse
    ; disable_api_stop =
        Aws.Util.option_bind (Aws.Xml.member "DisableApiStop" xml) Boolean.parse
    ; enable_primary_ipv6 =
        Aws.Util.option_bind (Aws.Xml.member "EnablePrimaryIpv6" xml) Boolean.parse
    ; network_performance_options =
        Aws.Util.option_bind
          (Aws.Xml.member "NetworkPerformanceOptions" xml)
          InstanceNetworkPerformanceOptionsRequest.parse
    ; operator =
        Aws.Util.option_bind (Aws.Xml.member "Operator" xml) OperatorRequest.parse
    ; secondary_interfaces =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SecondaryInterface" xml)
             InstanceSecondaryInterfaceSpecificationListRequest.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; disable_api_termination =
        Aws.Util.option_bind (Aws.Xml.member "disableApiTermination" xml) Boolean.parse
    ; instance_initiated_shutdown_behavior =
        Aws.Util.option_bind
          (Aws.Xml.member "instanceInitiatedShutdownBehavior" xml)
          ShutdownBehavior.parse
    ; private_ip_address =
        Aws.Util.option_bind (Aws.Xml.member "privateIpAddress" xml) String.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "clientToken" xml) String.parse
    ; additional_info =
        Aws.Util.option_bind (Aws.Xml.member "additionalInfo" xml) String.parse
    ; network_interfaces =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "networkInterface" xml)
             InstanceNetworkInterfaceSpecificationList.parse)
    ; iam_instance_profile =
        Aws.Util.option_bind
          (Aws.Xml.member "iamInstanceProfile" xml)
          IamInstanceProfileSpecification.parse
    ; ebs_optimized =
        Aws.Util.option_bind (Aws.Xml.member "ebsOptimized" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ebs_optimized (fun f ->
             Aws.Query.Pair ("EbsOptimized", Boolean.to_query f))
       ; Aws.Util.option_map v.iam_instance_profile (fun f ->
             Aws.Query.Pair
               ("IamInstanceProfile", IamInstanceProfileSpecification.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "NetworkInterface"
              , InstanceNetworkInterfaceSpecificationList.to_query v.network_interfaces ))
       ; Aws.Util.option_map v.additional_info (fun f ->
             Aws.Query.Pair ("AdditionalInfo", String.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.private_ip_address (fun f ->
             Aws.Query.Pair ("PrivateIpAddress", String.to_query f))
       ; Aws.Util.option_map v.instance_initiated_shutdown_behavior (fun f ->
             Aws.Query.Pair
               ("InstanceInitiatedShutdownBehavior", ShutdownBehavior.to_query f))
       ; Aws.Util.option_map v.disable_api_termination (fun f ->
             Aws.Query.Pair ("DisableApiTermination", Boolean.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SecondaryInterface"
              , InstanceSecondaryInterfaceSpecificationListRequest.to_query
                  v.secondary_interfaces ))
       ; Aws.Util.option_map v.operator (fun f ->
             Aws.Query.Pair ("Operator", OperatorRequest.to_query f))
       ; Aws.Util.option_map v.network_performance_options (fun f ->
             Aws.Query.Pair
               ( "NetworkPerformanceOptions"
               , InstanceNetworkPerformanceOptionsRequest.to_query f ))
       ; Aws.Util.option_map v.enable_primary_ipv6 (fun f ->
             Aws.Query.Pair ("EnablePrimaryIpv6", Boolean.to_query f))
       ; Aws.Util.option_map v.disable_api_stop (fun f ->
             Aws.Query.Pair ("DisableApiStop", Boolean.to_query f))
       ; Aws.Util.option_map v.maintenance_options (fun f ->
             Aws.Query.Pair
               ("MaintenanceOptions", InstanceMaintenanceOptionsRequest.to_query f))
       ; Aws.Util.option_map v.private_dns_name_options (fun f ->
             Aws.Query.Pair
               ("PrivateDnsNameOptions", PrivateDnsNameOptionsRequest.to_query f))
       ; Aws.Util.option_map v.enclave_options (fun f ->
             Aws.Query.Pair ("EnclaveOptions", EnclaveOptionsRequest.to_query f))
       ; Aws.Util.option_map v.metadata_options (fun f ->
             Aws.Query.Pair ("MetadataOptions", InstanceMetadataOptionsRequest.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "LicenseSpecification"
              , LicenseSpecificationListRequest.to_query v.license_specifications ))
       ; Aws.Util.option_map v.hibernation_options (fun f ->
             Aws.Query.Pair ("HibernationOptions", HibernationOptionsRequest.to_query f))
       ; Aws.Util.option_map v.capacity_reservation_specification (fun f ->
             Aws.Query.Pair
               ( "CapacityReservationSpecification"
               , CapacityReservationSpecification.to_query f ))
       ; Aws.Util.option_map v.cpu_options (fun f ->
             Aws.Query.Pair ("CpuOptions", CpuOptionsRequest.to_query f))
       ; Aws.Util.option_map v.credit_specification (fun f ->
             Aws.Query.Pair ("CreditSpecification", CreditSpecificationRequest.to_query f))
       ; Aws.Util.option_map v.instance_market_options (fun f ->
             Aws.Query.Pair
               ("InstanceMarketOptions", InstanceMarketOptionsRequest.to_query f))
       ; Aws.Util.option_map v.launch_template (fun f ->
             Aws.Query.Pair ("LaunchTemplate", LaunchTemplateSpecification.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Some
           (Aws.Query.Pair
              ( "ElasticInferenceAccelerator"
              , ElasticInferenceAccelerators.to_query v.elastic_inference_accelerators ))
       ; Some
           (Aws.Query.Pair
              ( "ElasticGpuSpecification"
              , ElasticGpuSpecifications.to_query v.elastic_gpu_specification ))
       ; Aws.Util.option_map v.user_data (fun f ->
             Aws.Query.Pair ("UserData", String.to_query f))
       ; Aws.Util.option_map v.subnet_id (fun f ->
             Aws.Query.Pair ("SubnetId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("SecurityGroup", SecurityGroupStringList.to_query v.security_groups))
       ; Some
           (Aws.Query.Pair
              ("SecurityGroupId", SecurityGroupIdStringList.to_query v.security_group_ids))
       ; Aws.Util.option_map v.ramdisk_id (fun f ->
             Aws.Query.Pair ("RamdiskId", String.to_query f))
       ; Aws.Util.option_map v.placement (fun f ->
             Aws.Query.Pair ("Placement", Placement.to_query f))
       ; Aws.Util.option_map v.monitoring (fun f ->
             Aws.Query.Pair ("Monitoring", RunInstancesMonitoringEnabled.to_query f))
       ; Some (Aws.Query.Pair ("MinCount", Integer.to_query v.min_count))
       ; Some (Aws.Query.Pair ("MaxCount", Integer.to_query v.max_count))
       ; Aws.Util.option_map v.key_name (fun f ->
             Aws.Query.Pair ("KeyName", String.to_query f))
       ; Aws.Util.option_map v.kernel_id (fun f ->
             Aws.Query.Pair ("KernelId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("Ipv6Address", InstanceIpv6AddressList.to_query v.ipv6_addresses))
       ; Aws.Util.option_map v.ipv6_address_count (fun f ->
             Aws.Query.Pair ("Ipv6AddressCount", Integer.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", InstanceType.to_query f))
       ; Aws.Util.option_map v.image_id (fun f ->
             Aws.Query.Pair ("ImageId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "BlockDeviceMapping"
              , BlockDeviceMappingRequestList.to_query v.block_device_mappings ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ebs_optimized (fun f -> "ebsOptimized", Boolean.to_json f)
       ; Aws.Util.option_map v.iam_instance_profile (fun f ->
             "iamInstanceProfile", IamInstanceProfileSpecification.to_json f)
       ; Some
           ( "networkInterface"
           , InstanceNetworkInterfaceSpecificationList.to_json v.network_interfaces )
       ; Aws.Util.option_map v.additional_info (fun f ->
             "additionalInfo", String.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "clientToken", String.to_json f)
       ; Aws.Util.option_map v.private_ip_address (fun f ->
             "privateIpAddress", String.to_json f)
       ; Aws.Util.option_map v.instance_initiated_shutdown_behavior (fun f ->
             "instanceInitiatedShutdownBehavior", ShutdownBehavior.to_json f)
       ; Aws.Util.option_map v.disable_api_termination (fun f ->
             "disableApiTermination", Boolean.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Some
           ( "SecondaryInterface"
           , InstanceSecondaryInterfaceSpecificationListRequest.to_json
               v.secondary_interfaces )
       ; Aws.Util.option_map v.operator (fun f -> "Operator", OperatorRequest.to_json f)
       ; Aws.Util.option_map v.network_performance_options (fun f ->
             ( "NetworkPerformanceOptions"
             , InstanceNetworkPerformanceOptionsRequest.to_json f ))
       ; Aws.Util.option_map v.enable_primary_ipv6 (fun f ->
             "EnablePrimaryIpv6", Boolean.to_json f)
       ; Aws.Util.option_map v.disable_api_stop (fun f ->
             "DisableApiStop", Boolean.to_json f)
       ; Aws.Util.option_map v.maintenance_options (fun f ->
             "MaintenanceOptions", InstanceMaintenanceOptionsRequest.to_json f)
       ; Aws.Util.option_map v.private_dns_name_options (fun f ->
             "PrivateDnsNameOptions", PrivateDnsNameOptionsRequest.to_json f)
       ; Aws.Util.option_map v.enclave_options (fun f ->
             "EnclaveOptions", EnclaveOptionsRequest.to_json f)
       ; Aws.Util.option_map v.metadata_options (fun f ->
             "MetadataOptions", InstanceMetadataOptionsRequest.to_json f)
       ; Some
           ( "LicenseSpecification"
           , LicenseSpecificationListRequest.to_json v.license_specifications )
       ; Aws.Util.option_map v.hibernation_options (fun f ->
             "HibernationOptions", HibernationOptionsRequest.to_json f)
       ; Aws.Util.option_map v.capacity_reservation_specification (fun f ->
             ( "CapacityReservationSpecification"
             , CapacityReservationSpecification.to_json f ))
       ; Aws.Util.option_map v.cpu_options (fun f ->
             "CpuOptions", CpuOptionsRequest.to_json f)
       ; Aws.Util.option_map v.credit_specification (fun f ->
             "CreditSpecification", CreditSpecificationRequest.to_json f)
       ; Aws.Util.option_map v.instance_market_options (fun f ->
             "InstanceMarketOptions", InstanceMarketOptionsRequest.to_json f)
       ; Aws.Util.option_map v.launch_template (fun f ->
             "LaunchTemplate", LaunchTemplateSpecification.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Some
           ( "ElasticInferenceAccelerator"
           , ElasticInferenceAccelerators.to_json v.elastic_inference_accelerators )
       ; Some
           ( "ElasticGpuSpecification"
           , ElasticGpuSpecifications.to_json v.elastic_gpu_specification )
       ; Aws.Util.option_map v.user_data (fun f -> "UserData", String.to_json f)
       ; Aws.Util.option_map v.subnet_id (fun f -> "SubnetId", String.to_json f)
       ; Some ("SecurityGroup", SecurityGroupStringList.to_json v.security_groups)
       ; Some ("SecurityGroupId", SecurityGroupIdStringList.to_json v.security_group_ids)
       ; Aws.Util.option_map v.ramdisk_id (fun f -> "RamdiskId", String.to_json f)
       ; Aws.Util.option_map v.placement (fun f -> "Placement", Placement.to_json f)
       ; Aws.Util.option_map v.monitoring (fun f ->
             "Monitoring", RunInstancesMonitoringEnabled.to_json f)
       ; Some ("MinCount", Integer.to_json v.min_count)
       ; Some ("MaxCount", Integer.to_json v.max_count)
       ; Aws.Util.option_map v.key_name (fun f -> "KeyName", String.to_json f)
       ; Aws.Util.option_map v.kernel_id (fun f -> "KernelId", String.to_json f)
       ; Some ("Ipv6Address", InstanceIpv6AddressList.to_json v.ipv6_addresses)
       ; Aws.Util.option_map v.ipv6_address_count (fun f ->
             "Ipv6AddressCount", Integer.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f ->
             "InstanceType", InstanceType.to_json f)
       ; Aws.Util.option_map v.image_id (fun f -> "ImageId", String.to_json f)
       ; Some
           ( "BlockDeviceMapping"
           , BlockDeviceMappingRequestList.to_json v.block_device_mappings )
       ])

let of_json j =
  { block_device_mappings =
      BlockDeviceMappingRequestList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "BlockDeviceMapping"))
  ; image_id = Aws.Util.option_map (Aws.Json.lookup j "ImageId") String.of_json
  ; instance_type =
      Aws.Util.option_map (Aws.Json.lookup j "InstanceType") InstanceType.of_json
  ; ipv6_address_count =
      Aws.Util.option_map (Aws.Json.lookup j "Ipv6AddressCount") Integer.of_json
  ; ipv6_addresses =
      InstanceIpv6AddressList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Ipv6Address"))
  ; kernel_id = Aws.Util.option_map (Aws.Json.lookup j "KernelId") String.of_json
  ; key_name = Aws.Util.option_map (Aws.Json.lookup j "KeyName") String.of_json
  ; max_count = Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MaxCount"))
  ; min_count = Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MinCount"))
  ; monitoring =
      Aws.Util.option_map
        (Aws.Json.lookup j "Monitoring")
        RunInstancesMonitoringEnabled.of_json
  ; placement = Aws.Util.option_map (Aws.Json.lookup j "Placement") Placement.of_json
  ; ramdisk_id = Aws.Util.option_map (Aws.Json.lookup j "RamdiskId") String.of_json
  ; security_group_ids =
      SecurityGroupIdStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroupId"))
  ; security_groups =
      SecurityGroupStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroup"))
  ; subnet_id = Aws.Util.option_map (Aws.Json.lookup j "SubnetId") String.of_json
  ; user_data = Aws.Util.option_map (Aws.Json.lookup j "UserData") String.of_json
  ; elastic_gpu_specification =
      ElasticGpuSpecifications.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ElasticGpuSpecification"))
  ; elastic_inference_accelerators =
      ElasticInferenceAccelerators.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ElasticInferenceAccelerator"))
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; launch_template =
      Aws.Util.option_map
        (Aws.Json.lookup j "LaunchTemplate")
        LaunchTemplateSpecification.of_json
  ; instance_market_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "InstanceMarketOptions")
        InstanceMarketOptionsRequest.of_json
  ; credit_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "CreditSpecification")
        CreditSpecificationRequest.of_json
  ; cpu_options =
      Aws.Util.option_map (Aws.Json.lookup j "CpuOptions") CpuOptionsRequest.of_json
  ; capacity_reservation_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "CapacityReservationSpecification")
        CapacityReservationSpecification.of_json
  ; hibernation_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "HibernationOptions")
        HibernationOptionsRequest.of_json
  ; license_specifications =
      LicenseSpecificationListRequest.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LicenseSpecification"))
  ; metadata_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "MetadataOptions")
        InstanceMetadataOptionsRequest.of_json
  ; enclave_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "EnclaveOptions")
        EnclaveOptionsRequest.of_json
  ; private_dns_name_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "PrivateDnsNameOptions")
        PrivateDnsNameOptionsRequest.of_json
  ; maintenance_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "MaintenanceOptions")
        InstanceMaintenanceOptionsRequest.of_json
  ; disable_api_stop =
      Aws.Util.option_map (Aws.Json.lookup j "DisableApiStop") Boolean.of_json
  ; enable_primary_ipv6 =
      Aws.Util.option_map (Aws.Json.lookup j "EnablePrimaryIpv6") Boolean.of_json
  ; network_performance_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "NetworkPerformanceOptions")
        InstanceNetworkPerformanceOptionsRequest.of_json
  ; operator = Aws.Util.option_map (Aws.Json.lookup j "Operator") OperatorRequest.of_json
  ; secondary_interfaces =
      InstanceSecondaryInterfaceSpecificationListRequest.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SecondaryInterface"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; disable_api_termination =
      Aws.Util.option_map (Aws.Json.lookup j "disableApiTermination") Boolean.of_json
  ; instance_initiated_shutdown_behavior =
      Aws.Util.option_map
        (Aws.Json.lookup j "instanceInitiatedShutdownBehavior")
        ShutdownBehavior.of_json
  ; private_ip_address =
      Aws.Util.option_map (Aws.Json.lookup j "privateIpAddress") String.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "clientToken") String.of_json
  ; additional_info =
      Aws.Util.option_map (Aws.Json.lookup j "additionalInfo") String.of_json
  ; network_interfaces =
      InstanceNetworkInterfaceSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "networkInterface"))
  ; iam_instance_profile =
      Aws.Util.option_map
        (Aws.Json.lookup j "iamInstanceProfile")
        IamInstanceProfileSpecification.of_json
  ; ebs_optimized = Aws.Util.option_map (Aws.Json.lookup j "ebsOptimized") Boolean.of_json
  }
