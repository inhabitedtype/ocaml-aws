open Aws.BaseTypes

type t =
  { architecture : ArchitectureValues.t
  ; block_device_mappings : InstanceBlockDeviceMappingList.t
  ; client_token : String.t option
  ; ebs_optimized : Boolean.t option
  ; ena_support : Boolean.t option
  ; hypervisor : HypervisorType.t
  ; iam_instance_profile : IamInstanceProfile.t option
  ; instance_lifecycle : InstanceLifecycleType.t option
  ; elastic_gpu_associations : ElasticGpuAssociationList.t
  ; elastic_inference_accelerator_associations :
      ElasticInferenceAcceleratorAssociationList.t
  ; network_interfaces : InstanceNetworkInterfaceList.t
  ; outpost_arn : String.t option
  ; root_device_name : String.t option
  ; root_device_type : DeviceType.t
  ; security_groups : GroupIdentifierList.t
  ; source_dest_check : Boolean.t option
  ; spot_instance_request_id : String.t option
  ; sriov_net_support : String.t option
  ; state_reason : StateReason.t option
  ; tags : TagList.t
  ; virtualization_type : VirtualizationType.t
  ; cpu_options : CpuOptions.t option
  ; capacity_block_id : String.t option
  ; capacity_reservation_id : String.t option
  ; capacity_reservation_specification : CapacityReservationSpecificationResponse.t option
  ; hibernation_options : HibernationOptions.t option
  ; licenses : LicenseList.t
  ; metadata_options : InstanceMetadataOptionsResponse.t option
  ; enclave_options : EnclaveOptions.t option
  ; boot_mode : BootModeValues.t option
  ; platform_details : String.t option
  ; usage_operation : String.t option
  ; usage_operation_update_time : DateTime.t option
  ; private_dns_name_options : PrivateDnsNameOptionsResponse.t option
  ; ipv6_address : String.t option
  ; tpm_support : String.t option
  ; maintenance_options : InstanceMaintenanceOptions.t option
  ; current_instance_boot_mode : InstanceBootModeValues.t option
  ; network_performance_options : InstanceNetworkPerformanceOptions.t option
  ; operator : OperatorResponse.t option
  ; secondary_interfaces : InstanceSecondaryInterfaceList.t
  ; instance_id : String.t
  ; image_id : String.t
  ; state : InstanceState.t
  ; private_dns_name : String.t option
  ; public_dns_name : String.t option
  ; state_transition_reason : String.t option
  ; key_name : String.t option
  ; ami_launch_index : Integer.t
  ; product_codes : ProductCodeList.t
  ; instance_type : InstanceType.t
  ; launch_time : DateTime.t
  ; placement : Placement.t
  ; kernel_id : String.t option
  ; ramdisk_id : String.t option
  ; platform : PlatformValues.t option
  ; monitoring : Monitoring.t
  ; subnet_id : String.t option
  ; vpc_id : String.t option
  ; private_ip_address : String.t option
  ; public_ip_address : String.t option
  }

let make
    ~architecture
    ?(block_device_mappings = [])
    ?client_token
    ?ebs_optimized
    ?ena_support
    ~hypervisor
    ?iam_instance_profile
    ?instance_lifecycle
    ?(elastic_gpu_associations = [])
    ?(elastic_inference_accelerator_associations = [])
    ?(network_interfaces = [])
    ?outpost_arn
    ?root_device_name
    ~root_device_type
    ?(security_groups = [])
    ?source_dest_check
    ?spot_instance_request_id
    ?sriov_net_support
    ?state_reason
    ?(tags = [])
    ~virtualization_type
    ?cpu_options
    ?capacity_block_id
    ?capacity_reservation_id
    ?capacity_reservation_specification
    ?hibernation_options
    ?(licenses = [])
    ?metadata_options
    ?enclave_options
    ?boot_mode
    ?platform_details
    ?usage_operation
    ?usage_operation_update_time
    ?private_dns_name_options
    ?ipv6_address
    ?tpm_support
    ?maintenance_options
    ?current_instance_boot_mode
    ?network_performance_options
    ?operator
    ?(secondary_interfaces = [])
    ~instance_id
    ~image_id
    ~state
    ?private_dns_name
    ?public_dns_name
    ?state_transition_reason
    ?key_name
    ~ami_launch_index
    ?(product_codes = [])
    ~instance_type
    ~launch_time
    ~placement
    ?kernel_id
    ?ramdisk_id
    ?platform
    ~monitoring
    ?subnet_id
    ?vpc_id
    ?private_ip_address
    ?public_ip_address
    () =
  { architecture
  ; block_device_mappings
  ; client_token
  ; ebs_optimized
  ; ena_support
  ; hypervisor
  ; iam_instance_profile
  ; instance_lifecycle
  ; elastic_gpu_associations
  ; elastic_inference_accelerator_associations
  ; network_interfaces
  ; outpost_arn
  ; root_device_name
  ; root_device_type
  ; security_groups
  ; source_dest_check
  ; spot_instance_request_id
  ; sriov_net_support
  ; state_reason
  ; tags
  ; virtualization_type
  ; cpu_options
  ; capacity_block_id
  ; capacity_reservation_id
  ; capacity_reservation_specification
  ; hibernation_options
  ; licenses
  ; metadata_options
  ; enclave_options
  ; boot_mode
  ; platform_details
  ; usage_operation
  ; usage_operation_update_time
  ; private_dns_name_options
  ; ipv6_address
  ; tpm_support
  ; maintenance_options
  ; current_instance_boot_mode
  ; network_performance_options
  ; operator
  ; secondary_interfaces
  ; instance_id
  ; image_id
  ; state
  ; private_dns_name
  ; public_dns_name
  ; state_transition_reason
  ; key_name
  ; ami_launch_index
  ; product_codes
  ; instance_type
  ; launch_time
  ; placement
  ; kernel_id
  ; ramdisk_id
  ; platform
  ; monitoring
  ; subnet_id
  ; vpc_id
  ; private_ip_address
  ; public_ip_address
  }

let parse xml =
  Some
    { architecture =
        Aws.Xml.required
          "architecture"
          (Aws.Util.option_bind
             (Aws.Xml.member "architecture" xml)
             ArchitectureValues.parse)
    ; block_device_mappings =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "blockDeviceMapping" xml)
             InstanceBlockDeviceMappingList.parse)
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "clientToken" xml) String.parse
    ; ebs_optimized =
        Aws.Util.option_bind (Aws.Xml.member "ebsOptimized" xml) Boolean.parse
    ; ena_support = Aws.Util.option_bind (Aws.Xml.member "enaSupport" xml) Boolean.parse
    ; hypervisor =
        Aws.Xml.required
          "hypervisor"
          (Aws.Util.option_bind (Aws.Xml.member "hypervisor" xml) HypervisorType.parse)
    ; iam_instance_profile =
        Aws.Util.option_bind
          (Aws.Xml.member "iamInstanceProfile" xml)
          IamInstanceProfile.parse
    ; instance_lifecycle =
        Aws.Util.option_bind
          (Aws.Xml.member "instanceLifecycle" xml)
          InstanceLifecycleType.parse
    ; elastic_gpu_associations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "elasticGpuAssociationSet" xml)
             ElasticGpuAssociationList.parse)
    ; elastic_inference_accelerator_associations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "elasticInferenceAcceleratorAssociationSet" xml)
             ElasticInferenceAcceleratorAssociationList.parse)
    ; network_interfaces =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "networkInterfaceSet" xml)
             InstanceNetworkInterfaceList.parse)
    ; outpost_arn = Aws.Util.option_bind (Aws.Xml.member "outpostArn" xml) String.parse
    ; root_device_name =
        Aws.Util.option_bind (Aws.Xml.member "rootDeviceName" xml) String.parse
    ; root_device_type =
        Aws.Xml.required
          "rootDeviceType"
          (Aws.Util.option_bind (Aws.Xml.member "rootDeviceType" xml) DeviceType.parse)
    ; security_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "groupSet" xml) GroupIdentifierList.parse)
    ; source_dest_check =
        Aws.Util.option_bind (Aws.Xml.member "sourceDestCheck" xml) Boolean.parse
    ; spot_instance_request_id =
        Aws.Util.option_bind (Aws.Xml.member "spotInstanceRequestId" xml) String.parse
    ; sriov_net_support =
        Aws.Util.option_bind (Aws.Xml.member "sriovNetSupport" xml) String.parse
    ; state_reason =
        Aws.Util.option_bind (Aws.Xml.member "stateReason" xml) StateReason.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; virtualization_type =
        Aws.Xml.required
          "virtualizationType"
          (Aws.Util.option_bind
             (Aws.Xml.member "virtualizationType" xml)
             VirtualizationType.parse)
    ; cpu_options =
        Aws.Util.option_bind (Aws.Xml.member "cpuOptions" xml) CpuOptions.parse
    ; capacity_block_id =
        Aws.Util.option_bind (Aws.Xml.member "capacityBlockId" xml) String.parse
    ; capacity_reservation_id =
        Aws.Util.option_bind (Aws.Xml.member "capacityReservationId" xml) String.parse
    ; capacity_reservation_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "capacityReservationSpecification" xml)
          CapacityReservationSpecificationResponse.parse
    ; hibernation_options =
        Aws.Util.option_bind
          (Aws.Xml.member "hibernationOptions" xml)
          HibernationOptions.parse
    ; licenses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "licenseSet" xml) LicenseList.parse)
    ; metadata_options =
        Aws.Util.option_bind
          (Aws.Xml.member "metadataOptions" xml)
          InstanceMetadataOptionsResponse.parse
    ; enclave_options =
        Aws.Util.option_bind (Aws.Xml.member "enclaveOptions" xml) EnclaveOptions.parse
    ; boot_mode =
        Aws.Util.option_bind (Aws.Xml.member "bootMode" xml) BootModeValues.parse
    ; platform_details =
        Aws.Util.option_bind (Aws.Xml.member "platformDetails" xml) String.parse
    ; usage_operation =
        Aws.Util.option_bind (Aws.Xml.member "usageOperation" xml) String.parse
    ; usage_operation_update_time =
        Aws.Util.option_bind
          (Aws.Xml.member "usageOperationUpdateTime" xml)
          DateTime.parse
    ; private_dns_name_options =
        Aws.Util.option_bind
          (Aws.Xml.member "privateDnsNameOptions" xml)
          PrivateDnsNameOptionsResponse.parse
    ; ipv6_address = Aws.Util.option_bind (Aws.Xml.member "ipv6Address" xml) String.parse
    ; tpm_support = Aws.Util.option_bind (Aws.Xml.member "tpmSupport" xml) String.parse
    ; maintenance_options =
        Aws.Util.option_bind
          (Aws.Xml.member "maintenanceOptions" xml)
          InstanceMaintenanceOptions.parse
    ; current_instance_boot_mode =
        Aws.Util.option_bind
          (Aws.Xml.member "currentInstanceBootMode" xml)
          InstanceBootModeValues.parse
    ; network_performance_options =
        Aws.Util.option_bind
          (Aws.Xml.member "networkPerformanceOptions" xml)
          InstanceNetworkPerformanceOptions.parse
    ; operator =
        Aws.Util.option_bind (Aws.Xml.member "operator" xml) OperatorResponse.parse
    ; secondary_interfaces =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "secondaryInterfaceSet" xml)
             InstanceSecondaryInterfaceList.parse)
    ; instance_id =
        Aws.Xml.required
          "instanceId"
          (Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse)
    ; image_id =
        Aws.Xml.required
          "imageId"
          (Aws.Util.option_bind (Aws.Xml.member "imageId" xml) String.parse)
    ; state =
        Aws.Xml.required
          "instanceState"
          (Aws.Util.option_bind (Aws.Xml.member "instanceState" xml) InstanceState.parse)
    ; private_dns_name =
        Aws.Util.option_bind (Aws.Xml.member "privateDnsName" xml) String.parse
    ; public_dns_name = Aws.Util.option_bind (Aws.Xml.member "dnsName" xml) String.parse
    ; state_transition_reason =
        Aws.Util.option_bind (Aws.Xml.member "reason" xml) String.parse
    ; key_name = Aws.Util.option_bind (Aws.Xml.member "keyName" xml) String.parse
    ; ami_launch_index =
        Aws.Xml.required
          "amiLaunchIndex"
          (Aws.Util.option_bind (Aws.Xml.member "amiLaunchIndex" xml) Integer.parse)
    ; product_codes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "productCodes" xml) ProductCodeList.parse)
    ; instance_type =
        Aws.Xml.required
          "instanceType"
          (Aws.Util.option_bind (Aws.Xml.member "instanceType" xml) InstanceType.parse)
    ; launch_time =
        Aws.Xml.required
          "launchTime"
          (Aws.Util.option_bind (Aws.Xml.member "launchTime" xml) DateTime.parse)
    ; placement =
        Aws.Xml.required
          "placement"
          (Aws.Util.option_bind (Aws.Xml.member "placement" xml) Placement.parse)
    ; kernel_id = Aws.Util.option_bind (Aws.Xml.member "kernelId" xml) String.parse
    ; ramdisk_id = Aws.Util.option_bind (Aws.Xml.member "ramdiskId" xml) String.parse
    ; platform = Aws.Util.option_bind (Aws.Xml.member "platform" xml) PlatformValues.parse
    ; monitoring =
        Aws.Xml.required
          "monitoring"
          (Aws.Util.option_bind (Aws.Xml.member "monitoring" xml) Monitoring.parse)
    ; subnet_id = Aws.Util.option_bind (Aws.Xml.member "subnetId" xml) String.parse
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse
    ; private_ip_address =
        Aws.Util.option_bind (Aws.Xml.member "privateIpAddress" xml) String.parse
    ; public_ip_address =
        Aws.Util.option_bind (Aws.Xml.member "ipAddress" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.public_ip_address (fun f ->
             Aws.Query.Pair ("IpAddress", String.to_query f))
       ; Aws.Util.option_map v.private_ip_address (fun f ->
             Aws.Query.Pair ("PrivateIpAddress", String.to_query f))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Aws.Util.option_map v.subnet_id (fun f ->
             Aws.Query.Pair ("SubnetId", String.to_query f))
       ; Some (Aws.Query.Pair ("Monitoring", Monitoring.to_query v.monitoring))
       ; Aws.Util.option_map v.platform (fun f ->
             Aws.Query.Pair ("Platform", PlatformValues.to_query f))
       ; Aws.Util.option_map v.ramdisk_id (fun f ->
             Aws.Query.Pair ("RamdiskId", String.to_query f))
       ; Aws.Util.option_map v.kernel_id (fun f ->
             Aws.Query.Pair ("KernelId", String.to_query f))
       ; Some (Aws.Query.Pair ("Placement", Placement.to_query v.placement))
       ; Some (Aws.Query.Pair ("LaunchTime", DateTime.to_query v.launch_time))
       ; Some (Aws.Query.Pair ("InstanceType", InstanceType.to_query v.instance_type))
       ; Some (Aws.Query.Pair ("ProductCodes", ProductCodeList.to_query v.product_codes))
       ; Some (Aws.Query.Pair ("AmiLaunchIndex", Integer.to_query v.ami_launch_index))
       ; Aws.Util.option_map v.key_name (fun f ->
             Aws.Query.Pair ("KeyName", String.to_query f))
       ; Aws.Util.option_map v.state_transition_reason (fun f ->
             Aws.Query.Pair ("Reason", String.to_query f))
       ; Aws.Util.option_map v.public_dns_name (fun f ->
             Aws.Query.Pair ("DnsName", String.to_query f))
       ; Aws.Util.option_map v.private_dns_name (fun f ->
             Aws.Query.Pair ("PrivateDnsName", String.to_query f))
       ; Some (Aws.Query.Pair ("InstanceState", InstanceState.to_query v.state))
       ; Some (Aws.Query.Pair ("ImageId", String.to_query v.image_id))
       ; Some (Aws.Query.Pair ("InstanceId", String.to_query v.instance_id))
       ; Some
           (Aws.Query.Pair
              ( "SecondaryInterfaceSet"
              , InstanceSecondaryInterfaceList.to_query v.secondary_interfaces ))
       ; Aws.Util.option_map v.operator (fun f ->
             Aws.Query.Pair ("Operator", OperatorResponse.to_query f))
       ; Aws.Util.option_map v.network_performance_options (fun f ->
             Aws.Query.Pair
               ("NetworkPerformanceOptions", InstanceNetworkPerformanceOptions.to_query f))
       ; Aws.Util.option_map v.current_instance_boot_mode (fun f ->
             Aws.Query.Pair ("CurrentInstanceBootMode", InstanceBootModeValues.to_query f))
       ; Aws.Util.option_map v.maintenance_options (fun f ->
             Aws.Query.Pair ("MaintenanceOptions", InstanceMaintenanceOptions.to_query f))
       ; Aws.Util.option_map v.tpm_support (fun f ->
             Aws.Query.Pair ("TpmSupport", String.to_query f))
       ; Aws.Util.option_map v.ipv6_address (fun f ->
             Aws.Query.Pair ("Ipv6Address", String.to_query f))
       ; Aws.Util.option_map v.private_dns_name_options (fun f ->
             Aws.Query.Pair
               ("PrivateDnsNameOptions", PrivateDnsNameOptionsResponse.to_query f))
       ; Aws.Util.option_map v.usage_operation_update_time (fun f ->
             Aws.Query.Pair ("UsageOperationUpdateTime", DateTime.to_query f))
       ; Aws.Util.option_map v.usage_operation (fun f ->
             Aws.Query.Pair ("UsageOperation", String.to_query f))
       ; Aws.Util.option_map v.platform_details (fun f ->
             Aws.Query.Pair ("PlatformDetails", String.to_query f))
       ; Aws.Util.option_map v.boot_mode (fun f ->
             Aws.Query.Pair ("BootMode", BootModeValues.to_query f))
       ; Aws.Util.option_map v.enclave_options (fun f ->
             Aws.Query.Pair ("EnclaveOptions", EnclaveOptions.to_query f))
       ; Aws.Util.option_map v.metadata_options (fun f ->
             Aws.Query.Pair ("MetadataOptions", InstanceMetadataOptionsResponse.to_query f))
       ; Some (Aws.Query.Pair ("LicenseSet", LicenseList.to_query v.licenses))
       ; Aws.Util.option_map v.hibernation_options (fun f ->
             Aws.Query.Pair ("HibernationOptions", HibernationOptions.to_query f))
       ; Aws.Util.option_map v.capacity_reservation_specification (fun f ->
             Aws.Query.Pair
               ( "CapacityReservationSpecification"
               , CapacityReservationSpecificationResponse.to_query f ))
       ; Aws.Util.option_map v.capacity_reservation_id (fun f ->
             Aws.Query.Pair ("CapacityReservationId", String.to_query f))
       ; Aws.Util.option_map v.capacity_block_id (fun f ->
             Aws.Query.Pair ("CapacityBlockId", String.to_query f))
       ; Aws.Util.option_map v.cpu_options (fun f ->
             Aws.Query.Pair ("CpuOptions", CpuOptions.to_query f))
       ; Some
           (Aws.Query.Pair
              ("VirtualizationType", VirtualizationType.to_query v.virtualization_type))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.state_reason (fun f ->
             Aws.Query.Pair ("StateReason", StateReason.to_query f))
       ; Aws.Util.option_map v.sriov_net_support (fun f ->
             Aws.Query.Pair ("SriovNetSupport", String.to_query f))
       ; Aws.Util.option_map v.spot_instance_request_id (fun f ->
             Aws.Query.Pair ("SpotInstanceRequestId", String.to_query f))
       ; Aws.Util.option_map v.source_dest_check (fun f ->
             Aws.Query.Pair ("SourceDestCheck", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair ("GroupSet", GroupIdentifierList.to_query v.security_groups))
       ; Some (Aws.Query.Pair ("RootDeviceType", DeviceType.to_query v.root_device_type))
       ; Aws.Util.option_map v.root_device_name (fun f ->
             Aws.Query.Pair ("RootDeviceName", String.to_query f))
       ; Aws.Util.option_map v.outpost_arn (fun f ->
             Aws.Query.Pair ("OutpostArn", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "NetworkInterfaceSet"
              , InstanceNetworkInterfaceList.to_query v.network_interfaces ))
       ; Some
           (Aws.Query.Pair
              ( "ElasticInferenceAcceleratorAssociationSet"
              , ElasticInferenceAcceleratorAssociationList.to_query
                  v.elastic_inference_accelerator_associations ))
       ; Some
           (Aws.Query.Pair
              ( "ElasticGpuAssociationSet"
              , ElasticGpuAssociationList.to_query v.elastic_gpu_associations ))
       ; Aws.Util.option_map v.instance_lifecycle (fun f ->
             Aws.Query.Pair ("InstanceLifecycle", InstanceLifecycleType.to_query f))
       ; Aws.Util.option_map v.iam_instance_profile (fun f ->
             Aws.Query.Pair ("IamInstanceProfile", IamInstanceProfile.to_query f))
       ; Some (Aws.Query.Pair ("Hypervisor", HypervisorType.to_query v.hypervisor))
       ; Aws.Util.option_map v.ena_support (fun f ->
             Aws.Query.Pair ("EnaSupport", Boolean.to_query f))
       ; Aws.Util.option_map v.ebs_optimized (fun f ->
             Aws.Query.Pair ("EbsOptimized", Boolean.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "BlockDeviceMapping"
              , InstanceBlockDeviceMappingList.to_query v.block_device_mappings ))
       ; Some
           (Aws.Query.Pair ("Architecture", ArchitectureValues.to_query v.architecture))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.public_ip_address (fun f -> "ipAddress", String.to_json f)
       ; Aws.Util.option_map v.private_ip_address (fun f ->
             "privateIpAddress", String.to_json f)
       ; Aws.Util.option_map v.vpc_id (fun f -> "vpcId", String.to_json f)
       ; Aws.Util.option_map v.subnet_id (fun f -> "subnetId", String.to_json f)
       ; Some ("monitoring", Monitoring.to_json v.monitoring)
       ; Aws.Util.option_map v.platform (fun f -> "platform", PlatformValues.to_json f)
       ; Aws.Util.option_map v.ramdisk_id (fun f -> "ramdiskId", String.to_json f)
       ; Aws.Util.option_map v.kernel_id (fun f -> "kernelId", String.to_json f)
       ; Some ("placement", Placement.to_json v.placement)
       ; Some ("launchTime", DateTime.to_json v.launch_time)
       ; Some ("instanceType", InstanceType.to_json v.instance_type)
       ; Some ("productCodes", ProductCodeList.to_json v.product_codes)
       ; Some ("amiLaunchIndex", Integer.to_json v.ami_launch_index)
       ; Aws.Util.option_map v.key_name (fun f -> "keyName", String.to_json f)
       ; Aws.Util.option_map v.state_transition_reason (fun f ->
             "reason", String.to_json f)
       ; Aws.Util.option_map v.public_dns_name (fun f -> "dnsName", String.to_json f)
       ; Aws.Util.option_map v.private_dns_name (fun f ->
             "privateDnsName", String.to_json f)
       ; Some ("instanceState", InstanceState.to_json v.state)
       ; Some ("imageId", String.to_json v.image_id)
       ; Some ("instanceId", String.to_json v.instance_id)
       ; Some
           ( "secondaryInterfaceSet"
           , InstanceSecondaryInterfaceList.to_json v.secondary_interfaces )
       ; Aws.Util.option_map v.operator (fun f -> "operator", OperatorResponse.to_json f)
       ; Aws.Util.option_map v.network_performance_options (fun f ->
             "networkPerformanceOptions", InstanceNetworkPerformanceOptions.to_json f)
       ; Aws.Util.option_map v.current_instance_boot_mode (fun f ->
             "currentInstanceBootMode", InstanceBootModeValues.to_json f)
       ; Aws.Util.option_map v.maintenance_options (fun f ->
             "maintenanceOptions", InstanceMaintenanceOptions.to_json f)
       ; Aws.Util.option_map v.tpm_support (fun f -> "tpmSupport", String.to_json f)
       ; Aws.Util.option_map v.ipv6_address (fun f -> "ipv6Address", String.to_json f)
       ; Aws.Util.option_map v.private_dns_name_options (fun f ->
             "privateDnsNameOptions", PrivateDnsNameOptionsResponse.to_json f)
       ; Aws.Util.option_map v.usage_operation_update_time (fun f ->
             "usageOperationUpdateTime", DateTime.to_json f)
       ; Aws.Util.option_map v.usage_operation (fun f ->
             "usageOperation", String.to_json f)
       ; Aws.Util.option_map v.platform_details (fun f ->
             "platformDetails", String.to_json f)
       ; Aws.Util.option_map v.boot_mode (fun f -> "bootMode", BootModeValues.to_json f)
       ; Aws.Util.option_map v.enclave_options (fun f ->
             "enclaveOptions", EnclaveOptions.to_json f)
       ; Aws.Util.option_map v.metadata_options (fun f ->
             "metadataOptions", InstanceMetadataOptionsResponse.to_json f)
       ; Some ("licenseSet", LicenseList.to_json v.licenses)
       ; Aws.Util.option_map v.hibernation_options (fun f ->
             "hibernationOptions", HibernationOptions.to_json f)
       ; Aws.Util.option_map v.capacity_reservation_specification (fun f ->
             ( "capacityReservationSpecification"
             , CapacityReservationSpecificationResponse.to_json f ))
       ; Aws.Util.option_map v.capacity_reservation_id (fun f ->
             "capacityReservationId", String.to_json f)
       ; Aws.Util.option_map v.capacity_block_id (fun f ->
             "capacityBlockId", String.to_json f)
       ; Aws.Util.option_map v.cpu_options (fun f -> "cpuOptions", CpuOptions.to_json f)
       ; Some ("virtualizationType", VirtualizationType.to_json v.virtualization_type)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.state_reason (fun f ->
             "stateReason", StateReason.to_json f)
       ; Aws.Util.option_map v.sriov_net_support (fun f ->
             "sriovNetSupport", String.to_json f)
       ; Aws.Util.option_map v.spot_instance_request_id (fun f ->
             "spotInstanceRequestId", String.to_json f)
       ; Aws.Util.option_map v.source_dest_check (fun f ->
             "sourceDestCheck", Boolean.to_json f)
       ; Some ("groupSet", GroupIdentifierList.to_json v.security_groups)
       ; Some ("rootDeviceType", DeviceType.to_json v.root_device_type)
       ; Aws.Util.option_map v.root_device_name (fun f ->
             "rootDeviceName", String.to_json f)
       ; Aws.Util.option_map v.outpost_arn (fun f -> "outpostArn", String.to_json f)
       ; Some
           ( "networkInterfaceSet"
           , InstanceNetworkInterfaceList.to_json v.network_interfaces )
       ; Some
           ( "elasticInferenceAcceleratorAssociationSet"
           , ElasticInferenceAcceleratorAssociationList.to_json
               v.elastic_inference_accelerator_associations )
       ; Some
           ( "elasticGpuAssociationSet"
           , ElasticGpuAssociationList.to_json v.elastic_gpu_associations )
       ; Aws.Util.option_map v.instance_lifecycle (fun f ->
             "instanceLifecycle", InstanceLifecycleType.to_json f)
       ; Aws.Util.option_map v.iam_instance_profile (fun f ->
             "iamInstanceProfile", IamInstanceProfile.to_json f)
       ; Some ("hypervisor", HypervisorType.to_json v.hypervisor)
       ; Aws.Util.option_map v.ena_support (fun f -> "enaSupport", Boolean.to_json f)
       ; Aws.Util.option_map v.ebs_optimized (fun f -> "ebsOptimized", Boolean.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "clientToken", String.to_json f)
       ; Some
           ( "blockDeviceMapping"
           , InstanceBlockDeviceMappingList.to_json v.block_device_mappings )
       ; Some ("architecture", ArchitectureValues.to_json v.architecture)
       ])

let of_json j =
  { architecture =
      ArchitectureValues.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "architecture"))
  ; block_device_mappings =
      InstanceBlockDeviceMappingList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "blockDeviceMapping"))
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "clientToken") String.of_json
  ; ebs_optimized = Aws.Util.option_map (Aws.Json.lookup j "ebsOptimized") Boolean.of_json
  ; ena_support = Aws.Util.option_map (Aws.Json.lookup j "enaSupport") Boolean.of_json
  ; hypervisor =
      HypervisorType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "hypervisor"))
  ; iam_instance_profile =
      Aws.Util.option_map
        (Aws.Json.lookup j "iamInstanceProfile")
        IamInstanceProfile.of_json
  ; instance_lifecycle =
      Aws.Util.option_map
        (Aws.Json.lookup j "instanceLifecycle")
        InstanceLifecycleType.of_json
  ; elastic_gpu_associations =
      ElasticGpuAssociationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "elasticGpuAssociationSet"))
  ; elastic_inference_accelerator_associations =
      ElasticInferenceAcceleratorAssociationList.of_json
        (Aws.Util.of_option_exn
           (Aws.Json.lookup j "elasticInferenceAcceleratorAssociationSet"))
  ; network_interfaces =
      InstanceNetworkInterfaceList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "networkInterfaceSet"))
  ; outpost_arn = Aws.Util.option_map (Aws.Json.lookup j "outpostArn") String.of_json
  ; root_device_name =
      Aws.Util.option_map (Aws.Json.lookup j "rootDeviceName") String.of_json
  ; root_device_type =
      DeviceType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "rootDeviceType"))
  ; security_groups =
      GroupIdentifierList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "groupSet"))
  ; source_dest_check =
      Aws.Util.option_map (Aws.Json.lookup j "sourceDestCheck") Boolean.of_json
  ; spot_instance_request_id =
      Aws.Util.option_map (Aws.Json.lookup j "spotInstanceRequestId") String.of_json
  ; sriov_net_support =
      Aws.Util.option_map (Aws.Json.lookup j "sriovNetSupport") String.of_json
  ; state_reason =
      Aws.Util.option_map (Aws.Json.lookup j "stateReason") StateReason.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; virtualization_type =
      VirtualizationType.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "virtualizationType"))
  ; cpu_options = Aws.Util.option_map (Aws.Json.lookup j "cpuOptions") CpuOptions.of_json
  ; capacity_block_id =
      Aws.Util.option_map (Aws.Json.lookup j "capacityBlockId") String.of_json
  ; capacity_reservation_id =
      Aws.Util.option_map (Aws.Json.lookup j "capacityReservationId") String.of_json
  ; capacity_reservation_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "capacityReservationSpecification")
        CapacityReservationSpecificationResponse.of_json
  ; hibernation_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "hibernationOptions")
        HibernationOptions.of_json
  ; licenses =
      LicenseList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "licenseSet"))
  ; metadata_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "metadataOptions")
        InstanceMetadataOptionsResponse.of_json
  ; enclave_options =
      Aws.Util.option_map (Aws.Json.lookup j "enclaveOptions") EnclaveOptions.of_json
  ; boot_mode = Aws.Util.option_map (Aws.Json.lookup j "bootMode") BootModeValues.of_json
  ; platform_details =
      Aws.Util.option_map (Aws.Json.lookup j "platformDetails") String.of_json
  ; usage_operation =
      Aws.Util.option_map (Aws.Json.lookup j "usageOperation") String.of_json
  ; usage_operation_update_time =
      Aws.Util.option_map (Aws.Json.lookup j "usageOperationUpdateTime") DateTime.of_json
  ; private_dns_name_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "privateDnsNameOptions")
        PrivateDnsNameOptionsResponse.of_json
  ; ipv6_address = Aws.Util.option_map (Aws.Json.lookup j "ipv6Address") String.of_json
  ; tpm_support = Aws.Util.option_map (Aws.Json.lookup j "tpmSupport") String.of_json
  ; maintenance_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "maintenanceOptions")
        InstanceMaintenanceOptions.of_json
  ; current_instance_boot_mode =
      Aws.Util.option_map
        (Aws.Json.lookup j "currentInstanceBootMode")
        InstanceBootModeValues.of_json
  ; network_performance_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "networkPerformanceOptions")
        InstanceNetworkPerformanceOptions.of_json
  ; operator = Aws.Util.option_map (Aws.Json.lookup j "operator") OperatorResponse.of_json
  ; secondary_interfaces =
      InstanceSecondaryInterfaceList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "secondaryInterfaceSet"))
  ; instance_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "instanceId"))
  ; image_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "imageId"))
  ; state =
      InstanceState.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "instanceState"))
  ; private_dns_name =
      Aws.Util.option_map (Aws.Json.lookup j "privateDnsName") String.of_json
  ; public_dns_name = Aws.Util.option_map (Aws.Json.lookup j "dnsName") String.of_json
  ; state_transition_reason =
      Aws.Util.option_map (Aws.Json.lookup j "reason") String.of_json
  ; key_name = Aws.Util.option_map (Aws.Json.lookup j "keyName") String.of_json
  ; ami_launch_index =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "amiLaunchIndex"))
  ; product_codes =
      ProductCodeList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "productCodes"))
  ; instance_type =
      InstanceType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "instanceType"))
  ; launch_time =
      DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "launchTime"))
  ; placement = Placement.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "placement"))
  ; kernel_id = Aws.Util.option_map (Aws.Json.lookup j "kernelId") String.of_json
  ; ramdisk_id = Aws.Util.option_map (Aws.Json.lookup j "ramdiskId") String.of_json
  ; platform = Aws.Util.option_map (Aws.Json.lookup j "platform") PlatformValues.of_json
  ; monitoring =
      Monitoring.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "monitoring"))
  ; subnet_id = Aws.Util.option_map (Aws.Json.lookup j "subnetId") String.of_json
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "vpcId") String.of_json
  ; private_ip_address =
      Aws.Util.option_map (Aws.Json.lookup j "privateIpAddress") String.of_json
  ; public_ip_address = Aws.Util.option_map (Aws.Json.lookup j "ipAddress") String.of_json
  }
