open Aws.BaseTypes

type t =
  { kernel_id : String.t option
  ; ebs_optimized : Boolean.t option
  ; iam_instance_profile : LaunchTemplateIamInstanceProfileSpecification.t option
  ; block_device_mappings : LaunchTemplateBlockDeviceMappingList.t
  ; network_interfaces : LaunchTemplateInstanceNetworkInterfaceSpecificationList.t
  ; image_id : String.t option
  ; instance_type : InstanceType.t option
  ; key_name : String.t option
  ; monitoring : LaunchTemplatesMonitoring.t option
  ; placement : LaunchTemplatePlacement.t option
  ; ram_disk_id : String.t option
  ; disable_api_termination : Boolean.t option
  ; instance_initiated_shutdown_behavior : ShutdownBehavior.t option
  ; user_data : String.t option
  ; tag_specifications : LaunchTemplateTagSpecificationList.t
  ; elastic_gpu_specifications : ElasticGpuSpecificationResponseList.t
  ; elastic_inference_accelerators :
      LaunchTemplateElasticInferenceAcceleratorResponseList.t
  ; security_group_ids : ValueStringList.t
  ; security_groups : ValueStringList.t
  ; instance_market_options : LaunchTemplateInstanceMarketOptions.t option
  ; credit_specification : CreditSpecification.t option
  ; cpu_options : LaunchTemplateCpuOptions.t option
  ; capacity_reservation_specification :
      LaunchTemplateCapacityReservationSpecificationResponse.t option
  ; license_specifications : LaunchTemplateLicenseList.t
  ; hibernation_options : LaunchTemplateHibernationOptions.t option
  ; metadata_options : LaunchTemplateInstanceMetadataOptions.t option
  ; enclave_options : LaunchTemplateEnclaveOptions.t option
  ; instance_requirements : InstanceRequirements.t option
  ; private_dns_name_options : LaunchTemplatePrivateDnsNameOptions.t option
  ; maintenance_options : LaunchTemplateInstanceMaintenanceOptions.t option
  ; disable_api_stop : Boolean.t option
  ; operator : OperatorResponse.t option
  ; network_performance_options : LaunchTemplateNetworkPerformanceOptions.t option
  ; secondary_interfaces : LaunchTemplateInstanceSecondaryInterfaceSpecificationList.t
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
    { kernel_id = Aws.Util.option_bind (Aws.Xml.member "kernelId" xml) String.parse
    ; ebs_optimized =
        Aws.Util.option_bind (Aws.Xml.member "ebsOptimized" xml) Boolean.parse
    ; iam_instance_profile =
        Aws.Util.option_bind
          (Aws.Xml.member "iamInstanceProfile" xml)
          LaunchTemplateIamInstanceProfileSpecification.parse
    ; block_device_mappings =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "blockDeviceMappingSet" xml)
             LaunchTemplateBlockDeviceMappingList.parse)
    ; network_interfaces =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "networkInterfaceSet" xml)
             LaunchTemplateInstanceNetworkInterfaceSpecificationList.parse)
    ; image_id = Aws.Util.option_bind (Aws.Xml.member "imageId" xml) String.parse
    ; instance_type =
        Aws.Util.option_bind (Aws.Xml.member "instanceType" xml) InstanceType.parse
    ; key_name = Aws.Util.option_bind (Aws.Xml.member "keyName" xml) String.parse
    ; monitoring =
        Aws.Util.option_bind
          (Aws.Xml.member "monitoring" xml)
          LaunchTemplatesMonitoring.parse
    ; placement =
        Aws.Util.option_bind
          (Aws.Xml.member "placement" xml)
          LaunchTemplatePlacement.parse
    ; ram_disk_id = Aws.Util.option_bind (Aws.Xml.member "ramDiskId" xml) String.parse
    ; disable_api_termination =
        Aws.Util.option_bind (Aws.Xml.member "disableApiTermination" xml) Boolean.parse
    ; instance_initiated_shutdown_behavior =
        Aws.Util.option_bind
          (Aws.Xml.member "instanceInitiatedShutdownBehavior" xml)
          ShutdownBehavior.parse
    ; user_data = Aws.Util.option_bind (Aws.Xml.member "userData" xml) String.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "tagSpecificationSet" xml)
             LaunchTemplateTagSpecificationList.parse)
    ; elastic_gpu_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "elasticGpuSpecificationSet" xml)
             ElasticGpuSpecificationResponseList.parse)
    ; elastic_inference_accelerators =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "elasticInferenceAcceleratorSet" xml)
             LaunchTemplateElasticInferenceAcceleratorResponseList.parse)
    ; security_group_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "securityGroupIdSet" xml)
             ValueStringList.parse)
    ; security_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "securityGroupSet" xml)
             ValueStringList.parse)
    ; instance_market_options =
        Aws.Util.option_bind
          (Aws.Xml.member "instanceMarketOptions" xml)
          LaunchTemplateInstanceMarketOptions.parse
    ; credit_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "creditSpecification" xml)
          CreditSpecification.parse
    ; cpu_options =
        Aws.Util.option_bind
          (Aws.Xml.member "cpuOptions" xml)
          LaunchTemplateCpuOptions.parse
    ; capacity_reservation_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "capacityReservationSpecification" xml)
          LaunchTemplateCapacityReservationSpecificationResponse.parse
    ; license_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "licenseSet" xml)
             LaunchTemplateLicenseList.parse)
    ; hibernation_options =
        Aws.Util.option_bind
          (Aws.Xml.member "hibernationOptions" xml)
          LaunchTemplateHibernationOptions.parse
    ; metadata_options =
        Aws.Util.option_bind
          (Aws.Xml.member "metadataOptions" xml)
          LaunchTemplateInstanceMetadataOptions.parse
    ; enclave_options =
        Aws.Util.option_bind
          (Aws.Xml.member "enclaveOptions" xml)
          LaunchTemplateEnclaveOptions.parse
    ; instance_requirements =
        Aws.Util.option_bind
          (Aws.Xml.member "instanceRequirements" xml)
          InstanceRequirements.parse
    ; private_dns_name_options =
        Aws.Util.option_bind
          (Aws.Xml.member "privateDnsNameOptions" xml)
          LaunchTemplatePrivateDnsNameOptions.parse
    ; maintenance_options =
        Aws.Util.option_bind
          (Aws.Xml.member "maintenanceOptions" xml)
          LaunchTemplateInstanceMaintenanceOptions.parse
    ; disable_api_stop =
        Aws.Util.option_bind (Aws.Xml.member "disableApiStop" xml) Boolean.parse
    ; operator =
        Aws.Util.option_bind (Aws.Xml.member "operator" xml) OperatorResponse.parse
    ; network_performance_options =
        Aws.Util.option_bind
          (Aws.Xml.member "networkPerformanceOptions" xml)
          LaunchTemplateNetworkPerformanceOptions.parse
    ; secondary_interfaces =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "secondaryInterfaceSet" xml)
             LaunchTemplateInstanceSecondaryInterfaceSpecificationList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "SecondaryInterfaceSet"
              , LaunchTemplateInstanceSecondaryInterfaceSpecificationList.to_query
                  v.secondary_interfaces ))
       ; Aws.Util.option_map v.network_performance_options (fun f ->
             Aws.Query.Pair
               ( "NetworkPerformanceOptions"
               , LaunchTemplateNetworkPerformanceOptions.to_query f ))
       ; Aws.Util.option_map v.operator (fun f ->
             Aws.Query.Pair ("Operator", OperatorResponse.to_query f))
       ; Aws.Util.option_map v.disable_api_stop (fun f ->
             Aws.Query.Pair ("DisableApiStop", Boolean.to_query f))
       ; Aws.Util.option_map v.maintenance_options (fun f ->
             Aws.Query.Pair
               ("MaintenanceOptions", LaunchTemplateInstanceMaintenanceOptions.to_query f))
       ; Aws.Util.option_map v.private_dns_name_options (fun f ->
             Aws.Query.Pair
               ("PrivateDnsNameOptions", LaunchTemplatePrivateDnsNameOptions.to_query f))
       ; Aws.Util.option_map v.instance_requirements (fun f ->
             Aws.Query.Pair ("InstanceRequirements", InstanceRequirements.to_query f))
       ; Aws.Util.option_map v.enclave_options (fun f ->
             Aws.Query.Pair ("EnclaveOptions", LaunchTemplateEnclaveOptions.to_query f))
       ; Aws.Util.option_map v.metadata_options (fun f ->
             Aws.Query.Pair
               ("MetadataOptions", LaunchTemplateInstanceMetadataOptions.to_query f))
       ; Aws.Util.option_map v.hibernation_options (fun f ->
             Aws.Query.Pair
               ("HibernationOptions", LaunchTemplateHibernationOptions.to_query f))
       ; Some
           (Aws.Query.Pair
              ("LicenseSet", LaunchTemplateLicenseList.to_query v.license_specifications))
       ; Aws.Util.option_map v.capacity_reservation_specification (fun f ->
             Aws.Query.Pair
               ( "CapacityReservationSpecification"
               , LaunchTemplateCapacityReservationSpecificationResponse.to_query f ))
       ; Aws.Util.option_map v.cpu_options (fun f ->
             Aws.Query.Pair ("CpuOptions", LaunchTemplateCpuOptions.to_query f))
       ; Aws.Util.option_map v.credit_specification (fun f ->
             Aws.Query.Pair ("CreditSpecification", CreditSpecification.to_query f))
       ; Aws.Util.option_map v.instance_market_options (fun f ->
             Aws.Query.Pair
               ("InstanceMarketOptions", LaunchTemplateInstanceMarketOptions.to_query f))
       ; Some
           (Aws.Query.Pair ("SecurityGroupSet", ValueStringList.to_query v.security_groups))
       ; Some
           (Aws.Query.Pair
              ("SecurityGroupIdSet", ValueStringList.to_query v.security_group_ids))
       ; Some
           (Aws.Query.Pair
              ( "ElasticInferenceAcceleratorSet"
              , LaunchTemplateElasticInferenceAcceleratorResponseList.to_query
                  v.elastic_inference_accelerators ))
       ; Some
           (Aws.Query.Pair
              ( "ElasticGpuSpecificationSet"
              , ElasticGpuSpecificationResponseList.to_query v.elastic_gpu_specifications
              ))
       ; Some
           (Aws.Query.Pair
              ( "TagSpecificationSet"
              , LaunchTemplateTagSpecificationList.to_query v.tag_specifications ))
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
             Aws.Query.Pair ("Placement", LaunchTemplatePlacement.to_query f))
       ; Aws.Util.option_map v.monitoring (fun f ->
             Aws.Query.Pair ("Monitoring", LaunchTemplatesMonitoring.to_query f))
       ; Aws.Util.option_map v.key_name (fun f ->
             Aws.Query.Pair ("KeyName", String.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", InstanceType.to_query f))
       ; Aws.Util.option_map v.image_id (fun f ->
             Aws.Query.Pair ("ImageId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "NetworkInterfaceSet"
              , LaunchTemplateInstanceNetworkInterfaceSpecificationList.to_query
                  v.network_interfaces ))
       ; Some
           (Aws.Query.Pair
              ( "BlockDeviceMappingSet"
              , LaunchTemplateBlockDeviceMappingList.to_query v.block_device_mappings ))
       ; Aws.Util.option_map v.iam_instance_profile (fun f ->
             Aws.Query.Pair
               ( "IamInstanceProfile"
               , LaunchTemplateIamInstanceProfileSpecification.to_query f ))
       ; Aws.Util.option_map v.ebs_optimized (fun f ->
             Aws.Query.Pair ("EbsOptimized", Boolean.to_query f))
       ; Aws.Util.option_map v.kernel_id (fun f ->
             Aws.Query.Pair ("KernelId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "secondaryInterfaceSet"
           , LaunchTemplateInstanceSecondaryInterfaceSpecificationList.to_json
               v.secondary_interfaces )
       ; Aws.Util.option_map v.network_performance_options (fun f ->
             ( "networkPerformanceOptions"
             , LaunchTemplateNetworkPerformanceOptions.to_json f ))
       ; Aws.Util.option_map v.operator (fun f -> "operator", OperatorResponse.to_json f)
       ; Aws.Util.option_map v.disable_api_stop (fun f ->
             "disableApiStop", Boolean.to_json f)
       ; Aws.Util.option_map v.maintenance_options (fun f ->
             "maintenanceOptions", LaunchTemplateInstanceMaintenanceOptions.to_json f)
       ; Aws.Util.option_map v.private_dns_name_options (fun f ->
             "privateDnsNameOptions", LaunchTemplatePrivateDnsNameOptions.to_json f)
       ; Aws.Util.option_map v.instance_requirements (fun f ->
             "instanceRequirements", InstanceRequirements.to_json f)
       ; Aws.Util.option_map v.enclave_options (fun f ->
             "enclaveOptions", LaunchTemplateEnclaveOptions.to_json f)
       ; Aws.Util.option_map v.metadata_options (fun f ->
             "metadataOptions", LaunchTemplateInstanceMetadataOptions.to_json f)
       ; Aws.Util.option_map v.hibernation_options (fun f ->
             "hibernationOptions", LaunchTemplateHibernationOptions.to_json f)
       ; Some ("licenseSet", LaunchTemplateLicenseList.to_json v.license_specifications)
       ; Aws.Util.option_map v.capacity_reservation_specification (fun f ->
             ( "capacityReservationSpecification"
             , LaunchTemplateCapacityReservationSpecificationResponse.to_json f ))
       ; Aws.Util.option_map v.cpu_options (fun f ->
             "cpuOptions", LaunchTemplateCpuOptions.to_json f)
       ; Aws.Util.option_map v.credit_specification (fun f ->
             "creditSpecification", CreditSpecification.to_json f)
       ; Aws.Util.option_map v.instance_market_options (fun f ->
             "instanceMarketOptions", LaunchTemplateInstanceMarketOptions.to_json f)
       ; Some ("securityGroupSet", ValueStringList.to_json v.security_groups)
       ; Some ("securityGroupIdSet", ValueStringList.to_json v.security_group_ids)
       ; Some
           ( "elasticInferenceAcceleratorSet"
           , LaunchTemplateElasticInferenceAcceleratorResponseList.to_json
               v.elastic_inference_accelerators )
       ; Some
           ( "elasticGpuSpecificationSet"
           , ElasticGpuSpecificationResponseList.to_json v.elastic_gpu_specifications )
       ; Some
           ( "tagSpecificationSet"
           , LaunchTemplateTagSpecificationList.to_json v.tag_specifications )
       ; Aws.Util.option_map v.user_data (fun f -> "userData", String.to_json f)
       ; Aws.Util.option_map v.instance_initiated_shutdown_behavior (fun f ->
             "instanceInitiatedShutdownBehavior", ShutdownBehavior.to_json f)
       ; Aws.Util.option_map v.disable_api_termination (fun f ->
             "disableApiTermination", Boolean.to_json f)
       ; Aws.Util.option_map v.ram_disk_id (fun f -> "ramDiskId", String.to_json f)
       ; Aws.Util.option_map v.placement (fun f ->
             "placement", LaunchTemplatePlacement.to_json f)
       ; Aws.Util.option_map v.monitoring (fun f ->
             "monitoring", LaunchTemplatesMonitoring.to_json f)
       ; Aws.Util.option_map v.key_name (fun f -> "keyName", String.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f ->
             "instanceType", InstanceType.to_json f)
       ; Aws.Util.option_map v.image_id (fun f -> "imageId", String.to_json f)
       ; Some
           ( "networkInterfaceSet"
           , LaunchTemplateInstanceNetworkInterfaceSpecificationList.to_json
               v.network_interfaces )
       ; Some
           ( "blockDeviceMappingSet"
           , LaunchTemplateBlockDeviceMappingList.to_json v.block_device_mappings )
       ; Aws.Util.option_map v.iam_instance_profile (fun f ->
             "iamInstanceProfile", LaunchTemplateIamInstanceProfileSpecification.to_json f)
       ; Aws.Util.option_map v.ebs_optimized (fun f -> "ebsOptimized", Boolean.to_json f)
       ; Aws.Util.option_map v.kernel_id (fun f -> "kernelId", String.to_json f)
       ])

let of_json j =
  { kernel_id = Aws.Util.option_map (Aws.Json.lookup j "kernelId") String.of_json
  ; ebs_optimized = Aws.Util.option_map (Aws.Json.lookup j "ebsOptimized") Boolean.of_json
  ; iam_instance_profile =
      Aws.Util.option_map
        (Aws.Json.lookup j "iamInstanceProfile")
        LaunchTemplateIamInstanceProfileSpecification.of_json
  ; block_device_mappings =
      LaunchTemplateBlockDeviceMappingList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "blockDeviceMappingSet"))
  ; network_interfaces =
      LaunchTemplateInstanceNetworkInterfaceSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "networkInterfaceSet"))
  ; image_id = Aws.Util.option_map (Aws.Json.lookup j "imageId") String.of_json
  ; instance_type =
      Aws.Util.option_map (Aws.Json.lookup j "instanceType") InstanceType.of_json
  ; key_name = Aws.Util.option_map (Aws.Json.lookup j "keyName") String.of_json
  ; monitoring =
      Aws.Util.option_map
        (Aws.Json.lookup j "monitoring")
        LaunchTemplatesMonitoring.of_json
  ; placement =
      Aws.Util.option_map (Aws.Json.lookup j "placement") LaunchTemplatePlacement.of_json
  ; ram_disk_id = Aws.Util.option_map (Aws.Json.lookup j "ramDiskId") String.of_json
  ; disable_api_termination =
      Aws.Util.option_map (Aws.Json.lookup j "disableApiTermination") Boolean.of_json
  ; instance_initiated_shutdown_behavior =
      Aws.Util.option_map
        (Aws.Json.lookup j "instanceInitiatedShutdownBehavior")
        ShutdownBehavior.of_json
  ; user_data = Aws.Util.option_map (Aws.Json.lookup j "userData") String.of_json
  ; tag_specifications =
      LaunchTemplateTagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSpecificationSet"))
  ; elastic_gpu_specifications =
      ElasticGpuSpecificationResponseList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "elasticGpuSpecificationSet"))
  ; elastic_inference_accelerators =
      LaunchTemplateElasticInferenceAcceleratorResponseList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "elasticInferenceAcceleratorSet"))
  ; security_group_ids =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "securityGroupIdSet"))
  ; security_groups =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "securityGroupSet"))
  ; instance_market_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "instanceMarketOptions")
        LaunchTemplateInstanceMarketOptions.of_json
  ; credit_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "creditSpecification")
        CreditSpecification.of_json
  ; cpu_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "cpuOptions")
        LaunchTemplateCpuOptions.of_json
  ; capacity_reservation_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "capacityReservationSpecification")
        LaunchTemplateCapacityReservationSpecificationResponse.of_json
  ; license_specifications =
      LaunchTemplateLicenseList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "licenseSet"))
  ; hibernation_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "hibernationOptions")
        LaunchTemplateHibernationOptions.of_json
  ; metadata_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "metadataOptions")
        LaunchTemplateInstanceMetadataOptions.of_json
  ; enclave_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "enclaveOptions")
        LaunchTemplateEnclaveOptions.of_json
  ; instance_requirements =
      Aws.Util.option_map
        (Aws.Json.lookup j "instanceRequirements")
        InstanceRequirements.of_json
  ; private_dns_name_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "privateDnsNameOptions")
        LaunchTemplatePrivateDnsNameOptions.of_json
  ; maintenance_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "maintenanceOptions")
        LaunchTemplateInstanceMaintenanceOptions.of_json
  ; disable_api_stop =
      Aws.Util.option_map (Aws.Json.lookup j "disableApiStop") Boolean.of_json
  ; operator = Aws.Util.option_map (Aws.Json.lookup j "operator") OperatorResponse.of_json
  ; network_performance_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "networkPerformanceOptions")
        LaunchTemplateNetworkPerformanceOptions.of_json
  ; secondary_interfaces =
      LaunchTemplateInstanceSecondaryInterfaceSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "secondaryInterfaceSet"))
  }
