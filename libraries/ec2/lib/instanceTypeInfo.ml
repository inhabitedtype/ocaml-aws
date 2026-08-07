open Aws.BaseTypes

type t =
  { instance_type : InstanceType.t option
  ; current_generation : Boolean.t option
  ; free_tier_eligible : Boolean.t option
  ; supported_usage_classes : UsageClassTypeList.t
  ; supported_root_device_types : RootDeviceTypeList.t
  ; supported_virtualization_types : VirtualizationTypeList.t
  ; bare_metal : Boolean.t option
  ; hypervisor : InstanceTypeHypervisor.t option
  ; processor_info : ProcessorInfo.t option
  ; v_cpu_info : VCpuInfo.t option
  ; memory_info : MemoryInfo.t option
  ; instance_storage_supported : Boolean.t option
  ; instance_storage_info : InstanceStorageInfo.t option
  ; ebs_info : EbsInfo.t option
  ; network_info : NetworkInfo.t option
  ; gpu_info : GpuInfo.t option
  ; fpga_info : FpgaInfo.t option
  ; placement_group_info : PlacementGroupInfo.t option
  ; inference_accelerator_info : InferenceAcceleratorInfo.t option
  ; hibernation_supported : Boolean.t option
  ; burstable_performance_supported : Boolean.t option
  ; dedicated_hosts_supported : Boolean.t option
  ; auto_recovery_supported : Boolean.t option
  ; supported_boot_modes : BootModeTypeList.t
  ; nitro_enclaves_support : NitroEnclavesSupport.t option
  ; nitro_tpm_support : NitroTpmSupport.t option
  ; nitro_tpm_info : NitroTpmInfo.t option
  ; media_accelerator_info : MediaAcceleratorInfo.t option
  ; neuron_info : NeuronInfo.t option
  ; phc_support : PhcSupport.t option
  ; reboot_migration_support : RebootMigrationSupport.t option
  ; supported_in_region : Boolean.t option
  }

let make
    ?instance_type
    ?current_generation
    ?free_tier_eligible
    ?(supported_usage_classes = [])
    ?(supported_root_device_types = [])
    ?(supported_virtualization_types = [])
    ?bare_metal
    ?hypervisor
    ?processor_info
    ?v_cpu_info
    ?memory_info
    ?instance_storage_supported
    ?instance_storage_info
    ?ebs_info
    ?network_info
    ?gpu_info
    ?fpga_info
    ?placement_group_info
    ?inference_accelerator_info
    ?hibernation_supported
    ?burstable_performance_supported
    ?dedicated_hosts_supported
    ?auto_recovery_supported
    ?(supported_boot_modes = [])
    ?nitro_enclaves_support
    ?nitro_tpm_support
    ?nitro_tpm_info
    ?media_accelerator_info
    ?neuron_info
    ?phc_support
    ?reboot_migration_support
    ?supported_in_region
    () =
  { instance_type
  ; current_generation
  ; free_tier_eligible
  ; supported_usage_classes
  ; supported_root_device_types
  ; supported_virtualization_types
  ; bare_metal
  ; hypervisor
  ; processor_info
  ; v_cpu_info
  ; memory_info
  ; instance_storage_supported
  ; instance_storage_info
  ; ebs_info
  ; network_info
  ; gpu_info
  ; fpga_info
  ; placement_group_info
  ; inference_accelerator_info
  ; hibernation_supported
  ; burstable_performance_supported
  ; dedicated_hosts_supported
  ; auto_recovery_supported
  ; supported_boot_modes
  ; nitro_enclaves_support
  ; nitro_tpm_support
  ; nitro_tpm_info
  ; media_accelerator_info
  ; neuron_info
  ; phc_support
  ; reboot_migration_support
  ; supported_in_region
  }

let parse xml =
  Some
    { instance_type =
        Aws.Util.option_bind (Aws.Xml.member "instanceType" xml) InstanceType.parse
    ; current_generation =
        Aws.Util.option_bind (Aws.Xml.member "currentGeneration" xml) Boolean.parse
    ; free_tier_eligible =
        Aws.Util.option_bind (Aws.Xml.member "freeTierEligible" xml) Boolean.parse
    ; supported_usage_classes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "supportedUsageClasses" xml)
             UsageClassTypeList.parse)
    ; supported_root_device_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "supportedRootDeviceTypes" xml)
             RootDeviceTypeList.parse)
    ; supported_virtualization_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "supportedVirtualizationTypes" xml)
             VirtualizationTypeList.parse)
    ; bare_metal = Aws.Util.option_bind (Aws.Xml.member "bareMetal" xml) Boolean.parse
    ; hypervisor =
        Aws.Util.option_bind
          (Aws.Xml.member "hypervisor" xml)
          InstanceTypeHypervisor.parse
    ; processor_info =
        Aws.Util.option_bind (Aws.Xml.member "processorInfo" xml) ProcessorInfo.parse
    ; v_cpu_info = Aws.Util.option_bind (Aws.Xml.member "vCpuInfo" xml) VCpuInfo.parse
    ; memory_info =
        Aws.Util.option_bind (Aws.Xml.member "memoryInfo" xml) MemoryInfo.parse
    ; instance_storage_supported =
        Aws.Util.option_bind (Aws.Xml.member "instanceStorageSupported" xml) Boolean.parse
    ; instance_storage_info =
        Aws.Util.option_bind
          (Aws.Xml.member "instanceStorageInfo" xml)
          InstanceStorageInfo.parse
    ; ebs_info = Aws.Util.option_bind (Aws.Xml.member "ebsInfo" xml) EbsInfo.parse
    ; network_info =
        Aws.Util.option_bind (Aws.Xml.member "networkInfo" xml) NetworkInfo.parse
    ; gpu_info = Aws.Util.option_bind (Aws.Xml.member "gpuInfo" xml) GpuInfo.parse
    ; fpga_info = Aws.Util.option_bind (Aws.Xml.member "fpgaInfo" xml) FpgaInfo.parse
    ; placement_group_info =
        Aws.Util.option_bind
          (Aws.Xml.member "placementGroupInfo" xml)
          PlacementGroupInfo.parse
    ; inference_accelerator_info =
        Aws.Util.option_bind
          (Aws.Xml.member "inferenceAcceleratorInfo" xml)
          InferenceAcceleratorInfo.parse
    ; hibernation_supported =
        Aws.Util.option_bind (Aws.Xml.member "hibernationSupported" xml) Boolean.parse
    ; burstable_performance_supported =
        Aws.Util.option_bind
          (Aws.Xml.member "burstablePerformanceSupported" xml)
          Boolean.parse
    ; dedicated_hosts_supported =
        Aws.Util.option_bind (Aws.Xml.member "dedicatedHostsSupported" xml) Boolean.parse
    ; auto_recovery_supported =
        Aws.Util.option_bind (Aws.Xml.member "autoRecoverySupported" xml) Boolean.parse
    ; supported_boot_modes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "supportedBootModes" xml)
             BootModeTypeList.parse)
    ; nitro_enclaves_support =
        Aws.Util.option_bind
          (Aws.Xml.member "nitroEnclavesSupport" xml)
          NitroEnclavesSupport.parse
    ; nitro_tpm_support =
        Aws.Util.option_bind (Aws.Xml.member "nitroTpmSupport" xml) NitroTpmSupport.parse
    ; nitro_tpm_info =
        Aws.Util.option_bind (Aws.Xml.member "nitroTpmInfo" xml) NitroTpmInfo.parse
    ; media_accelerator_info =
        Aws.Util.option_bind
          (Aws.Xml.member "mediaAcceleratorInfo" xml)
          MediaAcceleratorInfo.parse
    ; neuron_info =
        Aws.Util.option_bind (Aws.Xml.member "neuronInfo" xml) NeuronInfo.parse
    ; phc_support =
        Aws.Util.option_bind (Aws.Xml.member "phcSupport" xml) PhcSupport.parse
    ; reboot_migration_support =
        Aws.Util.option_bind
          (Aws.Xml.member "rebootMigrationSupport" xml)
          RebootMigrationSupport.parse
    ; supported_in_region =
        Aws.Util.option_bind (Aws.Xml.member "supportedInRegion" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.supported_in_region (fun f ->
             Aws.Query.Pair ("SupportedInRegion", Boolean.to_query f))
       ; Aws.Util.option_map v.reboot_migration_support (fun f ->
             Aws.Query.Pair ("RebootMigrationSupport", RebootMigrationSupport.to_query f))
       ; Aws.Util.option_map v.phc_support (fun f ->
             Aws.Query.Pair ("PhcSupport", PhcSupport.to_query f))
       ; Aws.Util.option_map v.neuron_info (fun f ->
             Aws.Query.Pair ("NeuronInfo", NeuronInfo.to_query f))
       ; Aws.Util.option_map v.media_accelerator_info (fun f ->
             Aws.Query.Pair ("MediaAcceleratorInfo", MediaAcceleratorInfo.to_query f))
       ; Aws.Util.option_map v.nitro_tpm_info (fun f ->
             Aws.Query.Pair ("NitroTpmInfo", NitroTpmInfo.to_query f))
       ; Aws.Util.option_map v.nitro_tpm_support (fun f ->
             Aws.Query.Pair ("NitroTpmSupport", NitroTpmSupport.to_query f))
       ; Aws.Util.option_map v.nitro_enclaves_support (fun f ->
             Aws.Query.Pair ("NitroEnclavesSupport", NitroEnclavesSupport.to_query f))
       ; Some
           (Aws.Query.Pair
              ("SupportedBootModes", BootModeTypeList.to_query v.supported_boot_modes))
       ; Aws.Util.option_map v.auto_recovery_supported (fun f ->
             Aws.Query.Pair ("AutoRecoverySupported", Boolean.to_query f))
       ; Aws.Util.option_map v.dedicated_hosts_supported (fun f ->
             Aws.Query.Pair ("DedicatedHostsSupported", Boolean.to_query f))
       ; Aws.Util.option_map v.burstable_performance_supported (fun f ->
             Aws.Query.Pair ("BurstablePerformanceSupported", Boolean.to_query f))
       ; Aws.Util.option_map v.hibernation_supported (fun f ->
             Aws.Query.Pair ("HibernationSupported", Boolean.to_query f))
       ; Aws.Util.option_map v.inference_accelerator_info (fun f ->
             Aws.Query.Pair
               ("InferenceAcceleratorInfo", InferenceAcceleratorInfo.to_query f))
       ; Aws.Util.option_map v.placement_group_info (fun f ->
             Aws.Query.Pair ("PlacementGroupInfo", PlacementGroupInfo.to_query f))
       ; Aws.Util.option_map v.fpga_info (fun f ->
             Aws.Query.Pair ("FpgaInfo", FpgaInfo.to_query f))
       ; Aws.Util.option_map v.gpu_info (fun f ->
             Aws.Query.Pair ("GpuInfo", GpuInfo.to_query f))
       ; Aws.Util.option_map v.network_info (fun f ->
             Aws.Query.Pair ("NetworkInfo", NetworkInfo.to_query f))
       ; Aws.Util.option_map v.ebs_info (fun f ->
             Aws.Query.Pair ("EbsInfo", EbsInfo.to_query f))
       ; Aws.Util.option_map v.instance_storage_info (fun f ->
             Aws.Query.Pair ("InstanceStorageInfo", InstanceStorageInfo.to_query f))
       ; Aws.Util.option_map v.instance_storage_supported (fun f ->
             Aws.Query.Pair ("InstanceStorageSupported", Boolean.to_query f))
       ; Aws.Util.option_map v.memory_info (fun f ->
             Aws.Query.Pair ("MemoryInfo", MemoryInfo.to_query f))
       ; Aws.Util.option_map v.v_cpu_info (fun f ->
             Aws.Query.Pair ("VCpuInfo", VCpuInfo.to_query f))
       ; Aws.Util.option_map v.processor_info (fun f ->
             Aws.Query.Pair ("ProcessorInfo", ProcessorInfo.to_query f))
       ; Aws.Util.option_map v.hypervisor (fun f ->
             Aws.Query.Pair ("Hypervisor", InstanceTypeHypervisor.to_query f))
       ; Aws.Util.option_map v.bare_metal (fun f ->
             Aws.Query.Pair ("BareMetal", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SupportedVirtualizationTypes"
              , VirtualizationTypeList.to_query v.supported_virtualization_types ))
       ; Some
           (Aws.Query.Pair
              ( "SupportedRootDeviceTypes"
              , RootDeviceTypeList.to_query v.supported_root_device_types ))
       ; Some
           (Aws.Query.Pair
              ( "SupportedUsageClasses"
              , UsageClassTypeList.to_query v.supported_usage_classes ))
       ; Aws.Util.option_map v.free_tier_eligible (fun f ->
             Aws.Query.Pair ("FreeTierEligible", Boolean.to_query f))
       ; Aws.Util.option_map v.current_generation (fun f ->
             Aws.Query.Pair ("CurrentGeneration", Boolean.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", InstanceType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.supported_in_region (fun f ->
             "supportedInRegion", Boolean.to_json f)
       ; Aws.Util.option_map v.reboot_migration_support (fun f ->
             "rebootMigrationSupport", RebootMigrationSupport.to_json f)
       ; Aws.Util.option_map v.phc_support (fun f -> "phcSupport", PhcSupport.to_json f)
       ; Aws.Util.option_map v.neuron_info (fun f -> "neuronInfo", NeuronInfo.to_json f)
       ; Aws.Util.option_map v.media_accelerator_info (fun f ->
             "mediaAcceleratorInfo", MediaAcceleratorInfo.to_json f)
       ; Aws.Util.option_map v.nitro_tpm_info (fun f ->
             "nitroTpmInfo", NitroTpmInfo.to_json f)
       ; Aws.Util.option_map v.nitro_tpm_support (fun f ->
             "nitroTpmSupport", NitroTpmSupport.to_json f)
       ; Aws.Util.option_map v.nitro_enclaves_support (fun f ->
             "nitroEnclavesSupport", NitroEnclavesSupport.to_json f)
       ; Some ("supportedBootModes", BootModeTypeList.to_json v.supported_boot_modes)
       ; Aws.Util.option_map v.auto_recovery_supported (fun f ->
             "autoRecoverySupported", Boolean.to_json f)
       ; Aws.Util.option_map v.dedicated_hosts_supported (fun f ->
             "dedicatedHostsSupported", Boolean.to_json f)
       ; Aws.Util.option_map v.burstable_performance_supported (fun f ->
             "burstablePerformanceSupported", Boolean.to_json f)
       ; Aws.Util.option_map v.hibernation_supported (fun f ->
             "hibernationSupported", Boolean.to_json f)
       ; Aws.Util.option_map v.inference_accelerator_info (fun f ->
             "inferenceAcceleratorInfo", InferenceAcceleratorInfo.to_json f)
       ; Aws.Util.option_map v.placement_group_info (fun f ->
             "placementGroupInfo", PlacementGroupInfo.to_json f)
       ; Aws.Util.option_map v.fpga_info (fun f -> "fpgaInfo", FpgaInfo.to_json f)
       ; Aws.Util.option_map v.gpu_info (fun f -> "gpuInfo", GpuInfo.to_json f)
       ; Aws.Util.option_map v.network_info (fun f ->
             "networkInfo", NetworkInfo.to_json f)
       ; Aws.Util.option_map v.ebs_info (fun f -> "ebsInfo", EbsInfo.to_json f)
       ; Aws.Util.option_map v.instance_storage_info (fun f ->
             "instanceStorageInfo", InstanceStorageInfo.to_json f)
       ; Aws.Util.option_map v.instance_storage_supported (fun f ->
             "instanceStorageSupported", Boolean.to_json f)
       ; Aws.Util.option_map v.memory_info (fun f -> "memoryInfo", MemoryInfo.to_json f)
       ; Aws.Util.option_map v.v_cpu_info (fun f -> "vCpuInfo", VCpuInfo.to_json f)
       ; Aws.Util.option_map v.processor_info (fun f ->
             "processorInfo", ProcessorInfo.to_json f)
       ; Aws.Util.option_map v.hypervisor (fun f ->
             "hypervisor", InstanceTypeHypervisor.to_json f)
       ; Aws.Util.option_map v.bare_metal (fun f -> "bareMetal", Boolean.to_json f)
       ; Some
           ( "supportedVirtualizationTypes"
           , VirtualizationTypeList.to_json v.supported_virtualization_types )
       ; Some
           ( "supportedRootDeviceTypes"
           , RootDeviceTypeList.to_json v.supported_root_device_types )
       ; Some
           ("supportedUsageClasses", UsageClassTypeList.to_json v.supported_usage_classes)
       ; Aws.Util.option_map v.free_tier_eligible (fun f ->
             "freeTierEligible", Boolean.to_json f)
       ; Aws.Util.option_map v.current_generation (fun f ->
             "currentGeneration", Boolean.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f ->
             "instanceType", InstanceType.to_json f)
       ])

let of_json j =
  { instance_type =
      Aws.Util.option_map (Aws.Json.lookup j "instanceType") InstanceType.of_json
  ; current_generation =
      Aws.Util.option_map (Aws.Json.lookup j "currentGeneration") Boolean.of_json
  ; free_tier_eligible =
      Aws.Util.option_map (Aws.Json.lookup j "freeTierEligible") Boolean.of_json
  ; supported_usage_classes =
      UsageClassTypeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "supportedUsageClasses"))
  ; supported_root_device_types =
      RootDeviceTypeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "supportedRootDeviceTypes"))
  ; supported_virtualization_types =
      VirtualizationTypeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "supportedVirtualizationTypes"))
  ; bare_metal = Aws.Util.option_map (Aws.Json.lookup j "bareMetal") Boolean.of_json
  ; hypervisor =
      Aws.Util.option_map (Aws.Json.lookup j "hypervisor") InstanceTypeHypervisor.of_json
  ; processor_info =
      Aws.Util.option_map (Aws.Json.lookup j "processorInfo") ProcessorInfo.of_json
  ; v_cpu_info = Aws.Util.option_map (Aws.Json.lookup j "vCpuInfo") VCpuInfo.of_json
  ; memory_info = Aws.Util.option_map (Aws.Json.lookup j "memoryInfo") MemoryInfo.of_json
  ; instance_storage_supported =
      Aws.Util.option_map (Aws.Json.lookup j "instanceStorageSupported") Boolean.of_json
  ; instance_storage_info =
      Aws.Util.option_map
        (Aws.Json.lookup j "instanceStorageInfo")
        InstanceStorageInfo.of_json
  ; ebs_info = Aws.Util.option_map (Aws.Json.lookup j "ebsInfo") EbsInfo.of_json
  ; network_info =
      Aws.Util.option_map (Aws.Json.lookup j "networkInfo") NetworkInfo.of_json
  ; gpu_info = Aws.Util.option_map (Aws.Json.lookup j "gpuInfo") GpuInfo.of_json
  ; fpga_info = Aws.Util.option_map (Aws.Json.lookup j "fpgaInfo") FpgaInfo.of_json
  ; placement_group_info =
      Aws.Util.option_map
        (Aws.Json.lookup j "placementGroupInfo")
        PlacementGroupInfo.of_json
  ; inference_accelerator_info =
      Aws.Util.option_map
        (Aws.Json.lookup j "inferenceAcceleratorInfo")
        InferenceAcceleratorInfo.of_json
  ; hibernation_supported =
      Aws.Util.option_map (Aws.Json.lookup j "hibernationSupported") Boolean.of_json
  ; burstable_performance_supported =
      Aws.Util.option_map
        (Aws.Json.lookup j "burstablePerformanceSupported")
        Boolean.of_json
  ; dedicated_hosts_supported =
      Aws.Util.option_map (Aws.Json.lookup j "dedicatedHostsSupported") Boolean.of_json
  ; auto_recovery_supported =
      Aws.Util.option_map (Aws.Json.lookup j "autoRecoverySupported") Boolean.of_json
  ; supported_boot_modes =
      BootModeTypeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "supportedBootModes"))
  ; nitro_enclaves_support =
      Aws.Util.option_map
        (Aws.Json.lookup j "nitroEnclavesSupport")
        NitroEnclavesSupport.of_json
  ; nitro_tpm_support =
      Aws.Util.option_map (Aws.Json.lookup j "nitroTpmSupport") NitroTpmSupport.of_json
  ; nitro_tpm_info =
      Aws.Util.option_map (Aws.Json.lookup j "nitroTpmInfo") NitroTpmInfo.of_json
  ; media_accelerator_info =
      Aws.Util.option_map
        (Aws.Json.lookup j "mediaAcceleratorInfo")
        MediaAcceleratorInfo.of_json
  ; neuron_info = Aws.Util.option_map (Aws.Json.lookup j "neuronInfo") NeuronInfo.of_json
  ; phc_support = Aws.Util.option_map (Aws.Json.lookup j "phcSupport") PhcSupport.of_json
  ; reboot_migration_support =
      Aws.Util.option_map
        (Aws.Json.lookup j "rebootMigrationSupport")
        RebootMigrationSupport.of_json
  ; supported_in_region =
      Aws.Util.option_map (Aws.Json.lookup j "supportedInRegion") Boolean.of_json
  }
