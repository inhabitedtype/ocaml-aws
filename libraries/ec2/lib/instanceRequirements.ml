open Aws.BaseTypes

type t =
  { v_cpu_count : VCpuCountRange.t option
  ; memory_mi_b : MemoryMiB.t option
  ; cpu_manufacturers : CpuManufacturerSet.t
  ; memory_gi_b_per_v_cpu : MemoryGiBPerVCpu.t option
  ; excluded_instance_types : ExcludedInstanceTypeSet.t
  ; instance_generations : InstanceGenerationSet.t
  ; spot_max_price_percentage_over_lowest_price : Integer.t option
  ; on_demand_max_price_percentage_over_lowest_price : Integer.t option
  ; bare_metal : BareMetal.t option
  ; burstable_performance : BurstablePerformance.t option
  ; require_hibernate_support : Boolean.t option
  ; network_interface_count : NetworkInterfaceCount.t option
  ; local_storage : LocalStorage.t option
  ; local_storage_types : LocalStorageTypeSet.t
  ; total_local_storage_g_b : TotalLocalStorageGB.t option
  ; baseline_ebs_bandwidth_mbps : BaselineEbsBandwidthMbps.t option
  ; accelerator_types : AcceleratorTypeSet.t
  ; accelerator_count : AcceleratorCount.t option
  ; accelerator_manufacturers : AcceleratorManufacturerSet.t
  ; accelerator_names : AcceleratorNameSet.t
  ; accelerator_total_memory_mi_b : AcceleratorTotalMemoryMiB.t option
  ; network_bandwidth_gbps : NetworkBandwidthGbps.t option
  ; allowed_instance_types : AllowedInstanceTypeSet.t
  ; max_spot_price_as_percentage_of_optimal_on_demand_price : Integer.t option
  ; baseline_performance_factors : BaselinePerformanceFactors.t option
  ; require_encryption_in_transit : Boolean.t option
  }

let make
    ?v_cpu_count
    ?memory_mi_b
    ?(cpu_manufacturers = [])
    ?memory_gi_b_per_v_cpu
    ?(excluded_instance_types = [])
    ?(instance_generations = [])
    ?spot_max_price_percentage_over_lowest_price
    ?on_demand_max_price_percentage_over_lowest_price
    ?bare_metal
    ?burstable_performance
    ?require_hibernate_support
    ?network_interface_count
    ?local_storage
    ?(local_storage_types = [])
    ?total_local_storage_g_b
    ?baseline_ebs_bandwidth_mbps
    ?(accelerator_types = [])
    ?accelerator_count
    ?(accelerator_manufacturers = [])
    ?(accelerator_names = [])
    ?accelerator_total_memory_mi_b
    ?network_bandwidth_gbps
    ?(allowed_instance_types = [])
    ?max_spot_price_as_percentage_of_optimal_on_demand_price
    ?baseline_performance_factors
    ?require_encryption_in_transit
    () =
  { v_cpu_count
  ; memory_mi_b
  ; cpu_manufacturers
  ; memory_gi_b_per_v_cpu
  ; excluded_instance_types
  ; instance_generations
  ; spot_max_price_percentage_over_lowest_price
  ; on_demand_max_price_percentage_over_lowest_price
  ; bare_metal
  ; burstable_performance
  ; require_hibernate_support
  ; network_interface_count
  ; local_storage
  ; local_storage_types
  ; total_local_storage_g_b
  ; baseline_ebs_bandwidth_mbps
  ; accelerator_types
  ; accelerator_count
  ; accelerator_manufacturers
  ; accelerator_names
  ; accelerator_total_memory_mi_b
  ; network_bandwidth_gbps
  ; allowed_instance_types
  ; max_spot_price_as_percentage_of_optimal_on_demand_price
  ; baseline_performance_factors
  ; require_encryption_in_transit
  }

let parse xml =
  Some
    { v_cpu_count =
        Aws.Util.option_bind (Aws.Xml.member "vCpuCount" xml) VCpuCountRange.parse
    ; memory_mi_b = Aws.Util.option_bind (Aws.Xml.member "memoryMiB" xml) MemoryMiB.parse
    ; cpu_manufacturers =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "cpuManufacturerSet" xml)
             CpuManufacturerSet.parse)
    ; memory_gi_b_per_v_cpu =
        Aws.Util.option_bind
          (Aws.Xml.member "memoryGiBPerVCpu" xml)
          MemoryGiBPerVCpu.parse
    ; excluded_instance_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "excludedInstanceTypeSet" xml)
             ExcludedInstanceTypeSet.parse)
    ; instance_generations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "instanceGenerationSet" xml)
             InstanceGenerationSet.parse)
    ; spot_max_price_percentage_over_lowest_price =
        Aws.Util.option_bind
          (Aws.Xml.member "spotMaxPricePercentageOverLowestPrice" xml)
          Integer.parse
    ; on_demand_max_price_percentage_over_lowest_price =
        Aws.Util.option_bind
          (Aws.Xml.member "onDemandMaxPricePercentageOverLowestPrice" xml)
          Integer.parse
    ; bare_metal = Aws.Util.option_bind (Aws.Xml.member "bareMetal" xml) BareMetal.parse
    ; burstable_performance =
        Aws.Util.option_bind
          (Aws.Xml.member "burstablePerformance" xml)
          BurstablePerformance.parse
    ; require_hibernate_support =
        Aws.Util.option_bind (Aws.Xml.member "requireHibernateSupport" xml) Boolean.parse
    ; network_interface_count =
        Aws.Util.option_bind
          (Aws.Xml.member "networkInterfaceCount" xml)
          NetworkInterfaceCount.parse
    ; local_storage =
        Aws.Util.option_bind (Aws.Xml.member "localStorage" xml) LocalStorage.parse
    ; local_storage_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "localStorageTypeSet" xml)
             LocalStorageTypeSet.parse)
    ; total_local_storage_g_b =
        Aws.Util.option_bind
          (Aws.Xml.member "totalLocalStorageGB" xml)
          TotalLocalStorageGB.parse
    ; baseline_ebs_bandwidth_mbps =
        Aws.Util.option_bind
          (Aws.Xml.member "baselineEbsBandwidthMbps" xml)
          BaselineEbsBandwidthMbps.parse
    ; accelerator_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "acceleratorTypeSet" xml)
             AcceleratorTypeSet.parse)
    ; accelerator_count =
        Aws.Util.option_bind
          (Aws.Xml.member "acceleratorCount" xml)
          AcceleratorCount.parse
    ; accelerator_manufacturers =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "acceleratorManufacturerSet" xml)
             AcceleratorManufacturerSet.parse)
    ; accelerator_names =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "acceleratorNameSet" xml)
             AcceleratorNameSet.parse)
    ; accelerator_total_memory_mi_b =
        Aws.Util.option_bind
          (Aws.Xml.member "acceleratorTotalMemoryMiB" xml)
          AcceleratorTotalMemoryMiB.parse
    ; network_bandwidth_gbps =
        Aws.Util.option_bind
          (Aws.Xml.member "networkBandwidthGbps" xml)
          NetworkBandwidthGbps.parse
    ; allowed_instance_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "allowedInstanceTypeSet" xml)
             AllowedInstanceTypeSet.parse)
    ; max_spot_price_as_percentage_of_optimal_on_demand_price =
        Aws.Util.option_bind
          (Aws.Xml.member "maxSpotPriceAsPercentageOfOptimalOnDemandPrice" xml)
          Integer.parse
    ; baseline_performance_factors =
        Aws.Util.option_bind
          (Aws.Xml.member "baselinePerformanceFactors" xml)
          BaselinePerformanceFactors.parse
    ; require_encryption_in_transit =
        Aws.Util.option_bind
          (Aws.Xml.member "requireEncryptionInTransit" xml)
          Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.require_encryption_in_transit (fun f ->
             Aws.Query.Pair ("RequireEncryptionInTransit", Boolean.to_query f))
       ; Aws.Util.option_map v.baseline_performance_factors (fun f ->
             Aws.Query.Pair
               ("BaselinePerformanceFactors", BaselinePerformanceFactors.to_query f))
       ; Aws.Util.option_map
           v.max_spot_price_as_percentage_of_optimal_on_demand_price
           (fun f ->
             Aws.Query.Pair
               ("MaxSpotPriceAsPercentageOfOptimalOnDemandPrice", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "AllowedInstanceTypeSet"
              , AllowedInstanceTypeSet.to_query v.allowed_instance_types ))
       ; Aws.Util.option_map v.network_bandwidth_gbps (fun f ->
             Aws.Query.Pair ("NetworkBandwidthGbps", NetworkBandwidthGbps.to_query f))
       ; Aws.Util.option_map v.accelerator_total_memory_mi_b (fun f ->
             Aws.Query.Pair
               ("AcceleratorTotalMemoryMiB", AcceleratorTotalMemoryMiB.to_query f))
       ; Some
           (Aws.Query.Pair
              ("AcceleratorNameSet", AcceleratorNameSet.to_query v.accelerator_names))
       ; Some
           (Aws.Query.Pair
              ( "AcceleratorManufacturerSet"
              , AcceleratorManufacturerSet.to_query v.accelerator_manufacturers ))
       ; Aws.Util.option_map v.accelerator_count (fun f ->
             Aws.Query.Pair ("AcceleratorCount", AcceleratorCount.to_query f))
       ; Some
           (Aws.Query.Pair
              ("AcceleratorTypeSet", AcceleratorTypeSet.to_query v.accelerator_types))
       ; Aws.Util.option_map v.baseline_ebs_bandwidth_mbps (fun f ->
             Aws.Query.Pair
               ("BaselineEbsBandwidthMbps", BaselineEbsBandwidthMbps.to_query f))
       ; Aws.Util.option_map v.total_local_storage_g_b (fun f ->
             Aws.Query.Pair ("TotalLocalStorageGB", TotalLocalStorageGB.to_query f))
       ; Some
           (Aws.Query.Pair
              ("LocalStorageTypeSet", LocalStorageTypeSet.to_query v.local_storage_types))
       ; Aws.Util.option_map v.local_storage (fun f ->
             Aws.Query.Pair ("LocalStorage", LocalStorage.to_query f))
       ; Aws.Util.option_map v.network_interface_count (fun f ->
             Aws.Query.Pair ("NetworkInterfaceCount", NetworkInterfaceCount.to_query f))
       ; Aws.Util.option_map v.require_hibernate_support (fun f ->
             Aws.Query.Pair ("RequireHibernateSupport", Boolean.to_query f))
       ; Aws.Util.option_map v.burstable_performance (fun f ->
             Aws.Query.Pair ("BurstablePerformance", BurstablePerformance.to_query f))
       ; Aws.Util.option_map v.bare_metal (fun f ->
             Aws.Query.Pair ("BareMetal", BareMetal.to_query f))
       ; Aws.Util.option_map v.on_demand_max_price_percentage_over_lowest_price (fun f ->
             Aws.Query.Pair
               ("OnDemandMaxPricePercentageOverLowestPrice", Integer.to_query f))
       ; Aws.Util.option_map v.spot_max_price_percentage_over_lowest_price (fun f ->
             Aws.Query.Pair ("SpotMaxPricePercentageOverLowestPrice", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "InstanceGenerationSet"
              , InstanceGenerationSet.to_query v.instance_generations ))
       ; Some
           (Aws.Query.Pair
              ( "ExcludedInstanceTypeSet"
              , ExcludedInstanceTypeSet.to_query v.excluded_instance_types ))
       ; Aws.Util.option_map v.memory_gi_b_per_v_cpu (fun f ->
             Aws.Query.Pair ("MemoryGiBPerVCpu", MemoryGiBPerVCpu.to_query f))
       ; Some
           (Aws.Query.Pair
              ("CpuManufacturerSet", CpuManufacturerSet.to_query v.cpu_manufacturers))
       ; Aws.Util.option_map v.memory_mi_b (fun f ->
             Aws.Query.Pair ("MemoryMiB", MemoryMiB.to_query f))
       ; Aws.Util.option_map v.v_cpu_count (fun f ->
             Aws.Query.Pair ("VCpuCount", VCpuCountRange.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.require_encryption_in_transit (fun f ->
             "requireEncryptionInTransit", Boolean.to_json f)
       ; Aws.Util.option_map v.baseline_performance_factors (fun f ->
             "baselinePerformanceFactors", BaselinePerformanceFactors.to_json f)
       ; Aws.Util.option_map
           v.max_spot_price_as_percentage_of_optimal_on_demand_price
           (fun f -> "maxSpotPriceAsPercentageOfOptimalOnDemandPrice", Integer.to_json f)
       ; Some
           ( "allowedInstanceTypeSet"
           , AllowedInstanceTypeSet.to_json v.allowed_instance_types )
       ; Aws.Util.option_map v.network_bandwidth_gbps (fun f ->
             "networkBandwidthGbps", NetworkBandwidthGbps.to_json f)
       ; Aws.Util.option_map v.accelerator_total_memory_mi_b (fun f ->
             "acceleratorTotalMemoryMiB", AcceleratorTotalMemoryMiB.to_json f)
       ; Some ("acceleratorNameSet", AcceleratorNameSet.to_json v.accelerator_names)
       ; Some
           ( "acceleratorManufacturerSet"
           , AcceleratorManufacturerSet.to_json v.accelerator_manufacturers )
       ; Aws.Util.option_map v.accelerator_count (fun f ->
             "acceleratorCount", AcceleratorCount.to_json f)
       ; Some ("acceleratorTypeSet", AcceleratorTypeSet.to_json v.accelerator_types)
       ; Aws.Util.option_map v.baseline_ebs_bandwidth_mbps (fun f ->
             "baselineEbsBandwidthMbps", BaselineEbsBandwidthMbps.to_json f)
       ; Aws.Util.option_map v.total_local_storage_g_b (fun f ->
             "totalLocalStorageGB", TotalLocalStorageGB.to_json f)
       ; Some ("localStorageTypeSet", LocalStorageTypeSet.to_json v.local_storage_types)
       ; Aws.Util.option_map v.local_storage (fun f ->
             "localStorage", LocalStorage.to_json f)
       ; Aws.Util.option_map v.network_interface_count (fun f ->
             "networkInterfaceCount", NetworkInterfaceCount.to_json f)
       ; Aws.Util.option_map v.require_hibernate_support (fun f ->
             "requireHibernateSupport", Boolean.to_json f)
       ; Aws.Util.option_map v.burstable_performance (fun f ->
             "burstablePerformance", BurstablePerformance.to_json f)
       ; Aws.Util.option_map v.bare_metal (fun f -> "bareMetal", BareMetal.to_json f)
       ; Aws.Util.option_map v.on_demand_max_price_percentage_over_lowest_price (fun f ->
             "onDemandMaxPricePercentageOverLowestPrice", Integer.to_json f)
       ; Aws.Util.option_map v.spot_max_price_percentage_over_lowest_price (fun f ->
             "spotMaxPricePercentageOverLowestPrice", Integer.to_json f)
       ; Some
           ("instanceGenerationSet", InstanceGenerationSet.to_json v.instance_generations)
       ; Some
           ( "excludedInstanceTypeSet"
           , ExcludedInstanceTypeSet.to_json v.excluded_instance_types )
       ; Aws.Util.option_map v.memory_gi_b_per_v_cpu (fun f ->
             "memoryGiBPerVCpu", MemoryGiBPerVCpu.to_json f)
       ; Some ("cpuManufacturerSet", CpuManufacturerSet.to_json v.cpu_manufacturers)
       ; Aws.Util.option_map v.memory_mi_b (fun f -> "memoryMiB", MemoryMiB.to_json f)
       ; Aws.Util.option_map v.v_cpu_count (fun f ->
             "vCpuCount", VCpuCountRange.to_json f)
       ])

let of_json j =
  { v_cpu_count =
      Aws.Util.option_map (Aws.Json.lookup j "vCpuCount") VCpuCountRange.of_json
  ; memory_mi_b = Aws.Util.option_map (Aws.Json.lookup j "memoryMiB") MemoryMiB.of_json
  ; cpu_manufacturers =
      CpuManufacturerSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "cpuManufacturerSet"))
  ; memory_gi_b_per_v_cpu =
      Aws.Util.option_map (Aws.Json.lookup j "memoryGiBPerVCpu") MemoryGiBPerVCpu.of_json
  ; excluded_instance_types =
      ExcludedInstanceTypeSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "excludedInstanceTypeSet"))
  ; instance_generations =
      InstanceGenerationSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "instanceGenerationSet"))
  ; spot_max_price_percentage_over_lowest_price =
      Aws.Util.option_map
        (Aws.Json.lookup j "spotMaxPricePercentageOverLowestPrice")
        Integer.of_json
  ; on_demand_max_price_percentage_over_lowest_price =
      Aws.Util.option_map
        (Aws.Json.lookup j "onDemandMaxPricePercentageOverLowestPrice")
        Integer.of_json
  ; bare_metal = Aws.Util.option_map (Aws.Json.lookup j "bareMetal") BareMetal.of_json
  ; burstable_performance =
      Aws.Util.option_map
        (Aws.Json.lookup j "burstablePerformance")
        BurstablePerformance.of_json
  ; require_hibernate_support =
      Aws.Util.option_map (Aws.Json.lookup j "requireHibernateSupport") Boolean.of_json
  ; network_interface_count =
      Aws.Util.option_map
        (Aws.Json.lookup j "networkInterfaceCount")
        NetworkInterfaceCount.of_json
  ; local_storage =
      Aws.Util.option_map (Aws.Json.lookup j "localStorage") LocalStorage.of_json
  ; local_storage_types =
      LocalStorageTypeSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "localStorageTypeSet"))
  ; total_local_storage_g_b =
      Aws.Util.option_map
        (Aws.Json.lookup j "totalLocalStorageGB")
        TotalLocalStorageGB.of_json
  ; baseline_ebs_bandwidth_mbps =
      Aws.Util.option_map
        (Aws.Json.lookup j "baselineEbsBandwidthMbps")
        BaselineEbsBandwidthMbps.of_json
  ; accelerator_types =
      AcceleratorTypeSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "acceleratorTypeSet"))
  ; accelerator_count =
      Aws.Util.option_map (Aws.Json.lookup j "acceleratorCount") AcceleratorCount.of_json
  ; accelerator_manufacturers =
      AcceleratorManufacturerSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "acceleratorManufacturerSet"))
  ; accelerator_names =
      AcceleratorNameSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "acceleratorNameSet"))
  ; accelerator_total_memory_mi_b =
      Aws.Util.option_map
        (Aws.Json.lookup j "acceleratorTotalMemoryMiB")
        AcceleratorTotalMemoryMiB.of_json
  ; network_bandwidth_gbps =
      Aws.Util.option_map
        (Aws.Json.lookup j "networkBandwidthGbps")
        NetworkBandwidthGbps.of_json
  ; allowed_instance_types =
      AllowedInstanceTypeSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "allowedInstanceTypeSet"))
  ; max_spot_price_as_percentage_of_optimal_on_demand_price =
      Aws.Util.option_map
        (Aws.Json.lookup j "maxSpotPriceAsPercentageOfOptimalOnDemandPrice")
        Integer.of_json
  ; baseline_performance_factors =
      Aws.Util.option_map
        (Aws.Json.lookup j "baselinePerformanceFactors")
        BaselinePerformanceFactors.of_json
  ; require_encryption_in_transit =
      Aws.Util.option_map (Aws.Json.lookup j "requireEncryptionInTransit") Boolean.of_json
  }
