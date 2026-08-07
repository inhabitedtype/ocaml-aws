open Aws.BaseTypes

type t =
  { v_cpu_count : VCpuCountRequest.t
  ; memory_mi_b : MemoryMiBRequest.t
  ; cpu_manufacturers : CpuManufacturers.t
  ; memory_gi_b_per_v_cpu : MemoryGiBPerVCpuRequest.t option
  ; excluded_instance_types : ExcludedInstanceTypes.t
  ; instance_generations : InstanceGenerations.t
  ; spot_max_price_percentage_over_lowest_price : Integer.t option
  ; max_spot_price_as_percentage_of_optimal_on_demand_price : Integer.t option
  ; on_demand_max_price_percentage_over_lowest_price : Integer.t option
  ; bare_metal : BareMetal.t option
  ; burstable_performance : BurstablePerformance.t option
  ; require_hibernate_support : Boolean.t option
  ; network_interface_count : NetworkInterfaceCountRequest.t option
  ; local_storage : LocalStorage.t option
  ; local_storage_types : LocalStorageTypes.t
  ; total_local_storage_g_b : TotalLocalStorageGBRequest.t option
  ; baseline_ebs_bandwidth_mbps : BaselineEbsBandwidthMbpsRequest.t option
  ; accelerator_types : AcceleratorTypes.t
  ; accelerator_count : AcceleratorCountRequest.t option
  ; accelerator_manufacturers : AcceleratorManufacturers.t
  ; accelerator_names : AcceleratorNames.t
  ; accelerator_total_memory_mi_b : AcceleratorTotalMemoryMiBRequest.t option
  ; network_bandwidth_gbps : NetworkBandwidthGbpsRequest.t option
  ; allowed_instance_types : AllowedInstanceTypes.t
  ; baseline_performance_factors : BaselinePerformanceFactorsRequest.t option
  }

let make
    ~v_cpu_count
    ~memory_mi_b
    ?(cpu_manufacturers = [])
    ?memory_gi_b_per_v_cpu
    ?(excluded_instance_types = [])
    ?(instance_generations = [])
    ?spot_max_price_percentage_over_lowest_price
    ?max_spot_price_as_percentage_of_optimal_on_demand_price
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
    ?baseline_performance_factors
    () =
  { v_cpu_count
  ; memory_mi_b
  ; cpu_manufacturers
  ; memory_gi_b_per_v_cpu
  ; excluded_instance_types
  ; instance_generations
  ; spot_max_price_percentage_over_lowest_price
  ; max_spot_price_as_percentage_of_optimal_on_demand_price
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
  ; baseline_performance_factors
  }

let parse xml =
  Some
    { v_cpu_count =
        Aws.Xml.required
          "VCpuCount"
          (Aws.Util.option_bind (Aws.Xml.member "VCpuCount" xml) VCpuCountRequest.parse)
    ; memory_mi_b =
        Aws.Xml.required
          "MemoryMiB"
          (Aws.Util.option_bind (Aws.Xml.member "MemoryMiB" xml) MemoryMiBRequest.parse)
    ; cpu_manufacturers =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "CpuManufacturers" xml)
             CpuManufacturers.parse)
    ; memory_gi_b_per_v_cpu =
        Aws.Util.option_bind
          (Aws.Xml.member "MemoryGiBPerVCpu" xml)
          MemoryGiBPerVCpuRequest.parse
    ; excluded_instance_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ExcludedInstanceTypes" xml)
             ExcludedInstanceTypes.parse)
    ; instance_generations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "InstanceGenerations" xml)
             InstanceGenerations.parse)
    ; spot_max_price_percentage_over_lowest_price =
        Aws.Util.option_bind
          (Aws.Xml.member "SpotMaxPricePercentageOverLowestPrice" xml)
          Integer.parse
    ; max_spot_price_as_percentage_of_optimal_on_demand_price =
        Aws.Util.option_bind
          (Aws.Xml.member "MaxSpotPriceAsPercentageOfOptimalOnDemandPrice" xml)
          Integer.parse
    ; on_demand_max_price_percentage_over_lowest_price =
        Aws.Util.option_bind
          (Aws.Xml.member "OnDemandMaxPricePercentageOverLowestPrice" xml)
          Integer.parse
    ; bare_metal = Aws.Util.option_bind (Aws.Xml.member "BareMetal" xml) BareMetal.parse
    ; burstable_performance =
        Aws.Util.option_bind
          (Aws.Xml.member "BurstablePerformance" xml)
          BurstablePerformance.parse
    ; require_hibernate_support =
        Aws.Util.option_bind (Aws.Xml.member "RequireHibernateSupport" xml) Boolean.parse
    ; network_interface_count =
        Aws.Util.option_bind
          (Aws.Xml.member "NetworkInterfaceCount" xml)
          NetworkInterfaceCountRequest.parse
    ; local_storage =
        Aws.Util.option_bind (Aws.Xml.member "LocalStorage" xml) LocalStorage.parse
    ; local_storage_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "LocalStorageTypes" xml)
             LocalStorageTypes.parse)
    ; total_local_storage_g_b =
        Aws.Util.option_bind
          (Aws.Xml.member "TotalLocalStorageGB" xml)
          TotalLocalStorageGBRequest.parse
    ; baseline_ebs_bandwidth_mbps =
        Aws.Util.option_bind
          (Aws.Xml.member "BaselineEbsBandwidthMbps" xml)
          BaselineEbsBandwidthMbpsRequest.parse
    ; accelerator_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AcceleratorTypes" xml)
             AcceleratorTypes.parse)
    ; accelerator_count =
        Aws.Util.option_bind
          (Aws.Xml.member "AcceleratorCount" xml)
          AcceleratorCountRequest.parse
    ; accelerator_manufacturers =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AcceleratorManufacturers" xml)
             AcceleratorManufacturers.parse)
    ; accelerator_names =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AcceleratorNames" xml)
             AcceleratorNames.parse)
    ; accelerator_total_memory_mi_b =
        Aws.Util.option_bind
          (Aws.Xml.member "AcceleratorTotalMemoryMiB" xml)
          AcceleratorTotalMemoryMiBRequest.parse
    ; network_bandwidth_gbps =
        Aws.Util.option_bind
          (Aws.Xml.member "NetworkBandwidthGbps" xml)
          NetworkBandwidthGbpsRequest.parse
    ; allowed_instance_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AllowedInstanceTypes" xml)
             AllowedInstanceTypes.parse)
    ; baseline_performance_factors =
        Aws.Util.option_bind
          (Aws.Xml.member "BaselinePerformanceFactors" xml)
          BaselinePerformanceFactorsRequest.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.baseline_performance_factors (fun f ->
             Aws.Query.Pair
               ("BaselinePerformanceFactors", BaselinePerformanceFactorsRequest.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "AllowedInstanceTypes.member"
              , AllowedInstanceTypes.to_query v.allowed_instance_types ))
       ; Aws.Util.option_map v.network_bandwidth_gbps (fun f ->
             Aws.Query.Pair
               ("NetworkBandwidthGbps", NetworkBandwidthGbpsRequest.to_query f))
       ; Aws.Util.option_map v.accelerator_total_memory_mi_b (fun f ->
             Aws.Query.Pair
               ("AcceleratorTotalMemoryMiB", AcceleratorTotalMemoryMiBRequest.to_query f))
       ; Some
           (Aws.Query.Pair
              ("AcceleratorNames.member", AcceleratorNames.to_query v.accelerator_names))
       ; Some
           (Aws.Query.Pair
              ( "AcceleratorManufacturers.member"
              , AcceleratorManufacturers.to_query v.accelerator_manufacturers ))
       ; Aws.Util.option_map v.accelerator_count (fun f ->
             Aws.Query.Pair ("AcceleratorCount", AcceleratorCountRequest.to_query f))
       ; Some
           (Aws.Query.Pair
              ("AcceleratorTypes.member", AcceleratorTypes.to_query v.accelerator_types))
       ; Aws.Util.option_map v.baseline_ebs_bandwidth_mbps (fun f ->
             Aws.Query.Pair
               ("BaselineEbsBandwidthMbps", BaselineEbsBandwidthMbpsRequest.to_query f))
       ; Aws.Util.option_map v.total_local_storage_g_b (fun f ->
             Aws.Query.Pair ("TotalLocalStorageGB", TotalLocalStorageGBRequest.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "LocalStorageTypes.member"
              , LocalStorageTypes.to_query v.local_storage_types ))
       ; Aws.Util.option_map v.local_storage (fun f ->
             Aws.Query.Pair ("LocalStorage", LocalStorage.to_query f))
       ; Aws.Util.option_map v.network_interface_count (fun f ->
             Aws.Query.Pair
               ("NetworkInterfaceCount", NetworkInterfaceCountRequest.to_query f))
       ; Aws.Util.option_map v.require_hibernate_support (fun f ->
             Aws.Query.Pair ("RequireHibernateSupport", Boolean.to_query f))
       ; Aws.Util.option_map v.burstable_performance (fun f ->
             Aws.Query.Pair ("BurstablePerformance", BurstablePerformance.to_query f))
       ; Aws.Util.option_map v.bare_metal (fun f ->
             Aws.Query.Pair ("BareMetal", BareMetal.to_query f))
       ; Aws.Util.option_map v.on_demand_max_price_percentage_over_lowest_price (fun f ->
             Aws.Query.Pair
               ("OnDemandMaxPricePercentageOverLowestPrice", Integer.to_query f))
       ; Aws.Util.option_map
           v.max_spot_price_as_percentage_of_optimal_on_demand_price
           (fun f ->
             Aws.Query.Pair
               ("MaxSpotPriceAsPercentageOfOptimalOnDemandPrice", Integer.to_query f))
       ; Aws.Util.option_map v.spot_max_price_percentage_over_lowest_price (fun f ->
             Aws.Query.Pair ("SpotMaxPricePercentageOverLowestPrice", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "InstanceGenerations.member"
              , InstanceGenerations.to_query v.instance_generations ))
       ; Some
           (Aws.Query.Pair
              ( "ExcludedInstanceTypes.member"
              , ExcludedInstanceTypes.to_query v.excluded_instance_types ))
       ; Aws.Util.option_map v.memory_gi_b_per_v_cpu (fun f ->
             Aws.Query.Pair ("MemoryGiBPerVCpu", MemoryGiBPerVCpuRequest.to_query f))
       ; Some
           (Aws.Query.Pair
              ("CpuManufacturers.member", CpuManufacturers.to_query v.cpu_manufacturers))
       ; Some (Aws.Query.Pair ("MemoryMiB", MemoryMiBRequest.to_query v.memory_mi_b))
       ; Some (Aws.Query.Pair ("VCpuCount", VCpuCountRequest.to_query v.v_cpu_count))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.baseline_performance_factors (fun f ->
             "BaselinePerformanceFactors", BaselinePerformanceFactorsRequest.to_json f)
       ; Some
           ("AllowedInstanceTypes", AllowedInstanceTypes.to_json v.allowed_instance_types)
       ; Aws.Util.option_map v.network_bandwidth_gbps (fun f ->
             "NetworkBandwidthGbps", NetworkBandwidthGbpsRequest.to_json f)
       ; Aws.Util.option_map v.accelerator_total_memory_mi_b (fun f ->
             "AcceleratorTotalMemoryMiB", AcceleratorTotalMemoryMiBRequest.to_json f)
       ; Some ("AcceleratorNames", AcceleratorNames.to_json v.accelerator_names)
       ; Some
           ( "AcceleratorManufacturers"
           , AcceleratorManufacturers.to_json v.accelerator_manufacturers )
       ; Aws.Util.option_map v.accelerator_count (fun f ->
             "AcceleratorCount", AcceleratorCountRequest.to_json f)
       ; Some ("AcceleratorTypes", AcceleratorTypes.to_json v.accelerator_types)
       ; Aws.Util.option_map v.baseline_ebs_bandwidth_mbps (fun f ->
             "BaselineEbsBandwidthMbps", BaselineEbsBandwidthMbpsRequest.to_json f)
       ; Aws.Util.option_map v.total_local_storage_g_b (fun f ->
             "TotalLocalStorageGB", TotalLocalStorageGBRequest.to_json f)
       ; Some ("LocalStorageTypes", LocalStorageTypes.to_json v.local_storage_types)
       ; Aws.Util.option_map v.local_storage (fun f ->
             "LocalStorage", LocalStorage.to_json f)
       ; Aws.Util.option_map v.network_interface_count (fun f ->
             "NetworkInterfaceCount", NetworkInterfaceCountRequest.to_json f)
       ; Aws.Util.option_map v.require_hibernate_support (fun f ->
             "RequireHibernateSupport", Boolean.to_json f)
       ; Aws.Util.option_map v.burstable_performance (fun f ->
             "BurstablePerformance", BurstablePerformance.to_json f)
       ; Aws.Util.option_map v.bare_metal (fun f -> "BareMetal", BareMetal.to_json f)
       ; Aws.Util.option_map v.on_demand_max_price_percentage_over_lowest_price (fun f ->
             "OnDemandMaxPricePercentageOverLowestPrice", Integer.to_json f)
       ; Aws.Util.option_map
           v.max_spot_price_as_percentage_of_optimal_on_demand_price
           (fun f -> "MaxSpotPriceAsPercentageOfOptimalOnDemandPrice", Integer.to_json f)
       ; Aws.Util.option_map v.spot_max_price_percentage_over_lowest_price (fun f ->
             "SpotMaxPricePercentageOverLowestPrice", Integer.to_json f)
       ; Some ("InstanceGenerations", InstanceGenerations.to_json v.instance_generations)
       ; Some
           ( "ExcludedInstanceTypes"
           , ExcludedInstanceTypes.to_json v.excluded_instance_types )
       ; Aws.Util.option_map v.memory_gi_b_per_v_cpu (fun f ->
             "MemoryGiBPerVCpu", MemoryGiBPerVCpuRequest.to_json f)
       ; Some ("CpuManufacturers", CpuManufacturers.to_json v.cpu_manufacturers)
       ; Some ("MemoryMiB", MemoryMiBRequest.to_json v.memory_mi_b)
       ; Some ("VCpuCount", VCpuCountRequest.to_json v.v_cpu_count)
       ])

let of_json j =
  { v_cpu_count =
      VCpuCountRequest.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VCpuCount"))
  ; memory_mi_b =
      MemoryMiBRequest.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MemoryMiB"))
  ; cpu_manufacturers =
      CpuManufacturers.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CpuManufacturers"))
  ; memory_gi_b_per_v_cpu =
      Aws.Util.option_map
        (Aws.Json.lookup j "MemoryGiBPerVCpu")
        MemoryGiBPerVCpuRequest.of_json
  ; excluded_instance_types =
      ExcludedInstanceTypes.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ExcludedInstanceTypes"))
  ; instance_generations =
      InstanceGenerations.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceGenerations"))
  ; spot_max_price_percentage_over_lowest_price =
      Aws.Util.option_map
        (Aws.Json.lookup j "SpotMaxPricePercentageOverLowestPrice")
        Integer.of_json
  ; max_spot_price_as_percentage_of_optimal_on_demand_price =
      Aws.Util.option_map
        (Aws.Json.lookup j "MaxSpotPriceAsPercentageOfOptimalOnDemandPrice")
        Integer.of_json
  ; on_demand_max_price_percentage_over_lowest_price =
      Aws.Util.option_map
        (Aws.Json.lookup j "OnDemandMaxPricePercentageOverLowestPrice")
        Integer.of_json
  ; bare_metal = Aws.Util.option_map (Aws.Json.lookup j "BareMetal") BareMetal.of_json
  ; burstable_performance =
      Aws.Util.option_map
        (Aws.Json.lookup j "BurstablePerformance")
        BurstablePerformance.of_json
  ; require_hibernate_support =
      Aws.Util.option_map (Aws.Json.lookup j "RequireHibernateSupport") Boolean.of_json
  ; network_interface_count =
      Aws.Util.option_map
        (Aws.Json.lookup j "NetworkInterfaceCount")
        NetworkInterfaceCountRequest.of_json
  ; local_storage =
      Aws.Util.option_map (Aws.Json.lookup j "LocalStorage") LocalStorage.of_json
  ; local_storage_types =
      LocalStorageTypes.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LocalStorageTypes"))
  ; total_local_storage_g_b =
      Aws.Util.option_map
        (Aws.Json.lookup j "TotalLocalStorageGB")
        TotalLocalStorageGBRequest.of_json
  ; baseline_ebs_bandwidth_mbps =
      Aws.Util.option_map
        (Aws.Json.lookup j "BaselineEbsBandwidthMbps")
        BaselineEbsBandwidthMbpsRequest.of_json
  ; accelerator_types =
      AcceleratorTypes.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AcceleratorTypes"))
  ; accelerator_count =
      Aws.Util.option_map
        (Aws.Json.lookup j "AcceleratorCount")
        AcceleratorCountRequest.of_json
  ; accelerator_manufacturers =
      AcceleratorManufacturers.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AcceleratorManufacturers"))
  ; accelerator_names =
      AcceleratorNames.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AcceleratorNames"))
  ; accelerator_total_memory_mi_b =
      Aws.Util.option_map
        (Aws.Json.lookup j "AcceleratorTotalMemoryMiB")
        AcceleratorTotalMemoryMiBRequest.of_json
  ; network_bandwidth_gbps =
      Aws.Util.option_map
        (Aws.Json.lookup j "NetworkBandwidthGbps")
        NetworkBandwidthGbpsRequest.of_json
  ; allowed_instance_types =
      AllowedInstanceTypes.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AllowedInstanceTypes"))
  ; baseline_performance_factors =
      Aws.Util.option_map
        (Aws.Json.lookup j "BaselinePerformanceFactors")
        BaselinePerformanceFactorsRequest.of_json
  }
