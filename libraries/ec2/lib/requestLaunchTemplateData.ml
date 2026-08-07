open Aws.BaseTypes

type t =
  { kernel_id : String.t option
  ; ebs_optimized : Boolean.t option
  ; iam_instance_profile : LaunchTemplateIamInstanceProfileSpecificationRequest.t option
  ; block_device_mappings : LaunchTemplateBlockDeviceMappingRequestList.t
  ; network_interfaces : LaunchTemplateInstanceNetworkInterfaceSpecificationRequestList.t
  ; image_id : String.t option
  ; instance_type : InstanceType.t option
  ; key_name : String.t option
  ; monitoring : LaunchTemplatesMonitoringRequest.t option
  ; placement : LaunchTemplatePlacementRequest.t option
  ; ram_disk_id : String.t option
  ; disable_api_termination : Boolean.t option
  ; instance_initiated_shutdown_behavior : ShutdownBehavior.t option
  ; user_data : String.t option
  ; tag_specifications : LaunchTemplateTagSpecificationRequestList.t
  ; elastic_gpu_specifications : ElasticGpuSpecificationList.t
  ; elastic_inference_accelerators : LaunchTemplateElasticInferenceAcceleratorList.t
  ; security_group_ids : SecurityGroupIdStringList.t
  ; security_groups : SecurityGroupStringList.t
  ; instance_market_options : LaunchTemplateInstanceMarketOptionsRequest.t option
  ; credit_specification : CreditSpecificationRequest.t option
  ; cpu_options : LaunchTemplateCpuOptionsRequest.t option
  ; capacity_reservation_specification :
      LaunchTemplateCapacityReservationSpecificationRequest.t option
  ; license_specifications : LaunchTemplateLicenseSpecificationListRequest.t
  ; hibernation_options : LaunchTemplateHibernationOptionsRequest.t option
  ; metadata_options : LaunchTemplateInstanceMetadataOptionsRequest.t option
  ; enclave_options : LaunchTemplateEnclaveOptionsRequest.t option
  ; instance_requirements : InstanceRequirementsRequest.t option
  ; private_dns_name_options : LaunchTemplatePrivateDnsNameOptionsRequest.t option
  ; maintenance_options : LaunchTemplateInstanceMaintenanceOptionsRequest.t option
  ; disable_api_stop : Boolean.t option
  ; operator : OperatorRequest.t option
  ; network_performance_options : LaunchTemplateNetworkPerformanceOptionsRequest.t option
  ; secondary_interfaces :
      LaunchTemplateInstanceSecondaryInterfaceSpecificationRequestList.t
  }

let make
    ?kernel_id
    ?ebs_optimized
    ?iam_instance_profile
    ?(block_device_mappings = [])
    ?(network_interfaces = [])
    ?image_id
    ?instance_type
    ?key_name
    ?monitoring
    ?placement
    ?ram_disk_id
    ?disable_api_termination
    ?instance_initiated_shutdown_behavior
    ?user_data
    ?(tag_specifications = [])
    ?(elastic_gpu_specifications = [])
    ?(elastic_inference_accelerators = [])
    ?(security_group_ids = [])
    ?(security_groups = [])
    ?instance_market_options
    ?credit_specification
    ?cpu_options
    ?capacity_reservation_specification
    ?(license_specifications = [])
    ?hibernation_options
    ?metadata_options
    ?enclave_options
    ?instance_requirements
    ?private_dns_name_options
    ?maintenance_options
    ?disable_api_stop
    ?operator
    ?network_performance_options
    ?(secondary_interfaces = [])
    () =
  { kernel_id
  ; ebs_optimized
  ; iam_instance_profile
  ; block_device_mappings
  ; network_interfaces
  ; image_id
  ; instance_type
  ; key_name
  ; monitoring
  ; placement
  ; ram_disk_id
  ; disable_api_termination
  ; instance_initiated_shutdown_behavior
  ; user_data
  ; tag_specifications
  ; elastic_gpu_specifications
  ; elastic_inference_accelerators
  ; security_group_ids
  ; security_groups
  ; instance_market_options
  ; credit_specification
  ; cpu_options
  ; capacity_reservation_specification
  ; license_specifications
  ; hibernation_options
  ; metadata_options
  ; enclave_options
  ; instance_requirements
  ; private_dns_name_options
  ; maintenance_options
  ; disable_api_stop
  ; operator
  ; network_performance_options
  ; secondary_interfaces
  }

let parse xml =
  Some
    { kernel_id = Aws.Util.option_bind (Aws.Xml.member "KernelId" xml) String.parse
    ; ebs_optimized =
        Aws.Util.option_bind (Aws.Xml.member "EbsOptimized" xml) Boolean.parse
    ; iam_instance_profile =
        Aws.Util.option_bind
          (Aws.Xml.member "IamInstanceProfile" xml)
          LaunchTemplateIamInstanceProfileSpecificationRequest.parse
    ; block_device_mappings =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "BlockDeviceMapping" xml)
             LaunchTemplateBlockDeviceMappingRequestList.parse)
    ; network_interfaces =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "NetworkInterface" xml)
             LaunchTemplateInstanceNetworkInterfaceSpecificationRequestList.parse)
    ; image_id = Aws.Util.option_bind (Aws.Xml.member "ImageId" xml) String.parse
    ; instance_type =
        Aws.Util.option_bind (Aws.Xml.member "InstanceType" xml) InstanceType.parse
    ; key_name = Aws.Util.option_bind (Aws.Xml.member "KeyName" xml) String.parse
    ; monitoring =
        Aws.Util.option_bind
          (Aws.Xml.member "Monitoring" xml)
          LaunchTemplatesMonitoringRequest.parse
    ; placement =
        Aws.Util.option_bind
          (Aws.Xml.member "Placement" xml)
          LaunchTemplatePlacementRequest.parse
    ; ram_disk_id = Aws.Util.option_bind (Aws.Xml.member "RamDiskId" xml) String.parse
    ; disable_api_termination =
        Aws.Util.option_bind (Aws.Xml.member "DisableApiTermination" xml) Boolean.parse
    ; instance_initiated_shutdown_behavior =
        Aws.Util.option_bind
          (Aws.Xml.member "InstanceInitiatedShutdownBehavior" xml)
          ShutdownBehavior.parse
    ; user_data = Aws.Util.option_bind (Aws.Xml.member "UserData" xml) String.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             LaunchTemplateTagSpecificationRequestList.parse)
    ; elastic_gpu_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ElasticGpuSpecification" xml)
             ElasticGpuSpecificationList.parse)
    ; elastic_inference_accelerators =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ElasticInferenceAccelerator" xml)
             LaunchTemplateElasticInferenceAcceleratorList.parse)
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
    ; instance_market_options =
        Aws.Util.option_bind
          (Aws.Xml.member "InstanceMarketOptions" xml)
          LaunchTemplateInstanceMarketOptionsRequest.parse
    ; credit_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "CreditSpecification" xml)
          CreditSpecificationRequest.parse
    ; cpu_options =
        Aws.Util.option_bind
          (Aws.Xml.member "CpuOptions" xml)
          LaunchTemplateCpuOptionsRequest.parse
    ; capacity_reservation_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "CapacityReservationSpecification" xml)
          LaunchTemplateCapacityReservationSpecificationRequest.parse
    ; license_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "LicenseSpecification" xml)
             LaunchTemplateLicenseSpecificationListRequest.parse)
    ; hibernation_options =
        Aws.Util.option_bind
          (Aws.Xml.member "HibernationOptions" xml)
          LaunchTemplateHibernationOptionsRequest.parse
    ; metadata_options =
        Aws.Util.option_bind
          (Aws.Xml.member "MetadataOptions" xml)
          LaunchTemplateInstanceMetadataOptionsRequest.parse
    ; enclave_options =
        Aws.Util.option_bind
          (Aws.Xml.member "EnclaveOptions" xml)
          LaunchTemplateEnclaveOptionsRequest.parse
    ; instance_requirements =
        Aws.Util.option_bind
          (Aws.Xml.member "InstanceRequirements" xml)
          InstanceRequirementsRequest.parse
    ; private_dns_name_options =
        Aws.Util.option_bind
          (Aws.Xml.member "PrivateDnsNameOptions" xml)
          LaunchTemplatePrivateDnsNameOptionsRequest.parse
    ; maintenance_options =
        Aws.Util.option_bind
          (Aws.Xml.member "MaintenanceOptions" xml)
          LaunchTemplateInstanceMaintenanceOptionsRequest.parse
    ; disable_api_stop =
        Aws.Util.option_bind (Aws.Xml.member "DisableApiStop" xml) Boolean.parse
    ; operator =
        Aws.Util.option_bind (Aws.Xml.member "Operator" xml) OperatorRequest.parse
    ; network_performance_options =
        Aws.Util.option_bind
          (Aws.Xml.member "NetworkPerformanceOptions" xml)
          LaunchTemplateNetworkPerformanceOptionsRequest.parse
    ; secondary_interfaces =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SecondaryInterface" xml)
             LaunchTemplateInstanceSecondaryInterfaceSpecificationRequestList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "SecondaryInterface"
              , LaunchTemplateInstanceSecondaryInterfaceSpecificationRequestList.to_query
                  v.secondary_interfaces ))
       ; Aws.Util.option_map v.network_performance_options (fun f ->
             Aws.Query.Pair
               ( "NetworkPerformanceOptions"
               , LaunchTemplateNetworkPerformanceOptionsRequest.to_query f ))
       ; Aws.Util.option_map v.operator (fun f ->
             Aws.Query.Pair ("Operator", OperatorRequest.to_query f))
       ; Aws.Util.option_map v.disable_api_stop (fun f ->
             Aws.Query.Pair ("DisableApiStop", Boolean.to_query f))
       ; Aws.Util.option_map v.maintenance_options (fun f ->
             Aws.Query.Pair
               ( "MaintenanceOptions"
               , LaunchTemplateInstanceMaintenanceOptionsRequest.to_query f ))
       ; Aws.Util.option_map v.private_dns_name_options (fun f ->
             Aws.Query.Pair
               ( "PrivateDnsNameOptions"
               , LaunchTemplatePrivateDnsNameOptionsRequest.to_query f ))
       ; Aws.Util.option_map v.instance_requirements (fun f ->
             Aws.Query.Pair
               ("InstanceRequirements", InstanceRequirementsRequest.to_query f))
       ; Aws.Util.option_map v.enclave_options (fun f ->
             Aws.Query.Pair
               ("EnclaveOptions", LaunchTemplateEnclaveOptionsRequest.to_query f))
       ; Aws.Util.option_map v.metadata_options (fun f ->
             Aws.Query.Pair
               ("MetadataOptions", LaunchTemplateInstanceMetadataOptionsRequest.to_query f))
       ; Aws.Util.option_map v.hibernation_options (fun f ->
             Aws.Query.Pair
               ("HibernationOptions", LaunchTemplateHibernationOptionsRequest.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "LicenseSpecification"
              , LaunchTemplateLicenseSpecificationListRequest.to_query
                  v.license_specifications ))
       ; Aws.Util.option_map v.capacity_reservation_specification (fun f ->
             Aws.Query.Pair
               ( "CapacityReservationSpecification"
               , LaunchTemplateCapacityReservationSpecificationRequest.to_query f ))
       ; Aws.Util.option_map v.cpu_options (fun f ->
             Aws.Query.Pair ("CpuOptions", LaunchTemplateCpuOptionsRequest.to_query f))
       ; Aws.Util.option_map v.credit_specification (fun f ->
             Aws.Query.Pair ("CreditSpecification", CreditSpecificationRequest.to_query f))
       ; Aws.Util.option_map v.instance_market_options (fun f ->
             Aws.Query.Pair
               ( "InstanceMarketOptions"
               , LaunchTemplateInstanceMarketOptionsRequest.to_query f ))
       ; Some
           (Aws.Query.Pair
              ("SecurityGroup", SecurityGroupStringList.to_query v.security_groups))
       ; Some
           (Aws.Query.Pair
              ("SecurityGroupId", SecurityGroupIdStringList.to_query v.security_group_ids))
       ; Some
           (Aws.Query.Pair
              ( "ElasticInferenceAccelerator"
              , LaunchTemplateElasticInferenceAcceleratorList.to_query
                  v.elastic_inference_accelerators ))
       ; Some
           (Aws.Query.Pair
              ( "ElasticGpuSpecification"
              , ElasticGpuSpecificationList.to_query v.elastic_gpu_specifications ))
       ; Some
           (Aws.Query.Pair
              ( "TagSpecification"
              , LaunchTemplateTagSpecificationRequestList.to_query v.tag_specifications ))
       ; Aws.Util.option_map v.user_data (fun f ->
             Aws.Query.Pair ("UserData", String.to_query f))
       ; Aws.Util.option_map v.instance_initiated_shutdown_behavior (fun f ->
             Aws.Query.Pair
               ("InstanceInitiatedShutdownBehavior", ShutdownBehavior.to_query f))
       ; Aws.Util.option_map v.disable_api_termination (fun f ->
             Aws.Query.Pair ("DisableApiTermination", Boolean.to_query f))
       ; Aws.Util.option_map v.ram_disk_id (fun f ->
             Aws.Query.Pair ("RamDiskId", String.to_query f))
       ; Aws.Util.option_map v.placement (fun f ->
             Aws.Query.Pair ("Placement", LaunchTemplatePlacementRequest.to_query f))
       ; Aws.Util.option_map v.monitoring (fun f ->
             Aws.Query.Pair ("Monitoring", LaunchTemplatesMonitoringRequest.to_query f))
       ; Aws.Util.option_map v.key_name (fun f ->
             Aws.Query.Pair ("KeyName", String.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", InstanceType.to_query f))
       ; Aws.Util.option_map v.image_id (fun f ->
             Aws.Query.Pair ("ImageId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "NetworkInterface"
              , LaunchTemplateInstanceNetworkInterfaceSpecificationRequestList.to_query
                  v.network_interfaces ))
       ; Some
           (Aws.Query.Pair
              ( "BlockDeviceMapping"
              , LaunchTemplateBlockDeviceMappingRequestList.to_query
                  v.block_device_mappings ))
       ; Aws.Util.option_map v.iam_instance_profile (fun f ->
             Aws.Query.Pair
               ( "IamInstanceProfile"
               , LaunchTemplateIamInstanceProfileSpecificationRequest.to_query f ))
       ; Aws.Util.option_map v.ebs_optimized (fun f ->
             Aws.Query.Pair ("EbsOptimized", Boolean.to_query f))
       ; Aws.Util.option_map v.kernel_id (fun f ->
             Aws.Query.Pair ("KernelId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "SecondaryInterface"
           , LaunchTemplateInstanceSecondaryInterfaceSpecificationRequestList.to_json
               v.secondary_interfaces )
       ; Aws.Util.option_map v.network_performance_options (fun f ->
             ( "NetworkPerformanceOptions"
             , LaunchTemplateNetworkPerformanceOptionsRequest.to_json f ))
       ; Aws.Util.option_map v.operator (fun f -> "Operator", OperatorRequest.to_json f)
       ; Aws.Util.option_map v.disable_api_stop (fun f ->
             "DisableApiStop", Boolean.to_json f)
       ; Aws.Util.option_map v.maintenance_options (fun f ->
             ( "MaintenanceOptions"
             , LaunchTemplateInstanceMaintenanceOptionsRequest.to_json f ))
       ; Aws.Util.option_map v.private_dns_name_options (fun f ->
             "PrivateDnsNameOptions", LaunchTemplatePrivateDnsNameOptionsRequest.to_json f)
       ; Aws.Util.option_map v.instance_requirements (fun f ->
             "InstanceRequirements", InstanceRequirementsRequest.to_json f)
       ; Aws.Util.option_map v.enclave_options (fun f ->
             "EnclaveOptions", LaunchTemplateEnclaveOptionsRequest.to_json f)
       ; Aws.Util.option_map v.metadata_options (fun f ->
             "MetadataOptions", LaunchTemplateInstanceMetadataOptionsRequest.to_json f)
       ; Aws.Util.option_map v.hibernation_options (fun f ->
             "HibernationOptions", LaunchTemplateHibernationOptionsRequest.to_json f)
       ; Some
           ( "LicenseSpecification"
           , LaunchTemplateLicenseSpecificationListRequest.to_json
               v.license_specifications )
       ; Aws.Util.option_map v.capacity_reservation_specification (fun f ->
             ( "CapacityReservationSpecification"
             , LaunchTemplateCapacityReservationSpecificationRequest.to_json f ))
       ; Aws.Util.option_map v.cpu_options (fun f ->
             "CpuOptions", LaunchTemplateCpuOptionsRequest.to_json f)
       ; Aws.Util.option_map v.credit_specification (fun f ->
             "CreditSpecification", CreditSpecificationRequest.to_json f)
       ; Aws.Util.option_map v.instance_market_options (fun f ->
             "InstanceMarketOptions", LaunchTemplateInstanceMarketOptionsRequest.to_json f)
       ; Some ("SecurityGroup", SecurityGroupStringList.to_json v.security_groups)
       ; Some ("SecurityGroupId", SecurityGroupIdStringList.to_json v.security_group_ids)
       ; Some
           ( "ElasticInferenceAccelerator"
           , LaunchTemplateElasticInferenceAcceleratorList.to_json
               v.elastic_inference_accelerators )
       ; Some
           ( "ElasticGpuSpecification"
           , ElasticGpuSpecificationList.to_json v.elastic_gpu_specifications )
       ; Some
           ( "TagSpecification"
           , LaunchTemplateTagSpecificationRequestList.to_json v.tag_specifications )
       ; Aws.Util.option_map v.user_data (fun f -> "UserData", String.to_json f)
       ; Aws.Util.option_map v.instance_initiated_shutdown_behavior (fun f ->
             "InstanceInitiatedShutdownBehavior", ShutdownBehavior.to_json f)
       ; Aws.Util.option_map v.disable_api_termination (fun f ->
             "DisableApiTermination", Boolean.to_json f)
       ; Aws.Util.option_map v.ram_disk_id (fun f -> "RamDiskId", String.to_json f)
       ; Aws.Util.option_map v.placement (fun f ->
             "Placement", LaunchTemplatePlacementRequest.to_json f)
       ; Aws.Util.option_map v.monitoring (fun f ->
             "Monitoring", LaunchTemplatesMonitoringRequest.to_json f)
       ; Aws.Util.option_map v.key_name (fun f -> "KeyName", String.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f ->
             "InstanceType", InstanceType.to_json f)
       ; Aws.Util.option_map v.image_id (fun f -> "ImageId", String.to_json f)
       ; Some
           ( "NetworkInterface"
           , LaunchTemplateInstanceNetworkInterfaceSpecificationRequestList.to_json
               v.network_interfaces )
       ; Some
           ( "BlockDeviceMapping"
           , LaunchTemplateBlockDeviceMappingRequestList.to_json v.block_device_mappings
           )
       ; Aws.Util.option_map v.iam_instance_profile (fun f ->
             ( "IamInstanceProfile"
             , LaunchTemplateIamInstanceProfileSpecificationRequest.to_json f ))
       ; Aws.Util.option_map v.ebs_optimized (fun f -> "EbsOptimized", Boolean.to_json f)
       ; Aws.Util.option_map v.kernel_id (fun f -> "KernelId", String.to_json f)
       ])

let of_json j =
  { kernel_id = Aws.Util.option_map (Aws.Json.lookup j "KernelId") String.of_json
  ; ebs_optimized = Aws.Util.option_map (Aws.Json.lookup j "EbsOptimized") Boolean.of_json
  ; iam_instance_profile =
      Aws.Util.option_map
        (Aws.Json.lookup j "IamInstanceProfile")
        LaunchTemplateIamInstanceProfileSpecificationRequest.of_json
  ; block_device_mappings =
      LaunchTemplateBlockDeviceMappingRequestList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "BlockDeviceMapping"))
  ; network_interfaces =
      LaunchTemplateInstanceNetworkInterfaceSpecificationRequestList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "NetworkInterface"))
  ; image_id = Aws.Util.option_map (Aws.Json.lookup j "ImageId") String.of_json
  ; instance_type =
      Aws.Util.option_map (Aws.Json.lookup j "InstanceType") InstanceType.of_json
  ; key_name = Aws.Util.option_map (Aws.Json.lookup j "KeyName") String.of_json
  ; monitoring =
      Aws.Util.option_map
        (Aws.Json.lookup j "Monitoring")
        LaunchTemplatesMonitoringRequest.of_json
  ; placement =
      Aws.Util.option_map
        (Aws.Json.lookup j "Placement")
        LaunchTemplatePlacementRequest.of_json
  ; ram_disk_id = Aws.Util.option_map (Aws.Json.lookup j "RamDiskId") String.of_json
  ; disable_api_termination =
      Aws.Util.option_map (Aws.Json.lookup j "DisableApiTermination") Boolean.of_json
  ; instance_initiated_shutdown_behavior =
      Aws.Util.option_map
        (Aws.Json.lookup j "InstanceInitiatedShutdownBehavior")
        ShutdownBehavior.of_json
  ; user_data = Aws.Util.option_map (Aws.Json.lookup j "UserData") String.of_json
  ; tag_specifications =
      LaunchTemplateTagSpecificationRequestList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; elastic_gpu_specifications =
      ElasticGpuSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ElasticGpuSpecification"))
  ; elastic_inference_accelerators =
      LaunchTemplateElasticInferenceAcceleratorList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ElasticInferenceAccelerator"))
  ; security_group_ids =
      SecurityGroupIdStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroupId"))
  ; security_groups =
      SecurityGroupStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroup"))
  ; instance_market_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "InstanceMarketOptions")
        LaunchTemplateInstanceMarketOptionsRequest.of_json
  ; credit_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "CreditSpecification")
        CreditSpecificationRequest.of_json
  ; cpu_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "CpuOptions")
        LaunchTemplateCpuOptionsRequest.of_json
  ; capacity_reservation_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "CapacityReservationSpecification")
        LaunchTemplateCapacityReservationSpecificationRequest.of_json
  ; license_specifications =
      LaunchTemplateLicenseSpecificationListRequest.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LicenseSpecification"))
  ; hibernation_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "HibernationOptions")
        LaunchTemplateHibernationOptionsRequest.of_json
  ; metadata_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "MetadataOptions")
        LaunchTemplateInstanceMetadataOptionsRequest.of_json
  ; enclave_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "EnclaveOptions")
        LaunchTemplateEnclaveOptionsRequest.of_json
  ; instance_requirements =
      Aws.Util.option_map
        (Aws.Json.lookup j "InstanceRequirements")
        InstanceRequirementsRequest.of_json
  ; private_dns_name_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "PrivateDnsNameOptions")
        LaunchTemplatePrivateDnsNameOptionsRequest.of_json
  ; maintenance_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "MaintenanceOptions")
        LaunchTemplateInstanceMaintenanceOptionsRequest.of_json
  ; disable_api_stop =
      Aws.Util.option_map (Aws.Json.lookup j "DisableApiStop") Boolean.of_json
  ; operator = Aws.Util.option_map (Aws.Json.lookup j "Operator") OperatorRequest.of_json
  ; network_performance_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "NetworkPerformanceOptions")
        LaunchTemplateNetworkPerformanceOptionsRequest.of_json
  ; secondary_interfaces =
      LaunchTemplateInstanceSecondaryInterfaceSpecificationRequestList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SecondaryInterface"))
  }
