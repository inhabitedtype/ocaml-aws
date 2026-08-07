open Aws.BaseTypes

type t =
  { storage_type : String.t option
  ; storage_size : RangeList.t
  ; provisioned_iops : RangeList.t
  ; iops_to_storage_ratio : DoubleRangeList.t
  ; provisioned_storage_throughput : RangeList.t
  ; storage_throughput_to_iops_ratio : DoubleRangeList.t
  ; supports_storage_autoscaling : Boolean.t option
  }

let make
    ?storage_type
    ?(storage_size = [])
    ?(provisioned_iops = [])
    ?(iops_to_storage_ratio = [])
    ?(provisioned_storage_throughput = [])
    ?(storage_throughput_to_iops_ratio = [])
    ?supports_storage_autoscaling
    () =
  { storage_type
  ; storage_size
  ; provisioned_iops
  ; iops_to_storage_ratio
  ; provisioned_storage_throughput
  ; storage_throughput_to_iops_ratio
  ; supports_storage_autoscaling
  }

let parse xml =
  Some
    { storage_type = Aws.Util.option_bind (Aws.Xml.member "StorageType" xml) String.parse
    ; storage_size =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "StorageSize" xml) RangeList.parse)
    ; provisioned_iops =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "ProvisionedIops" xml) RangeList.parse)
    ; iops_to_storage_ratio =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "IopsToStorageRatio" xml)
             DoubleRangeList.parse)
    ; provisioned_storage_throughput =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ProvisionedStorageThroughput" xml)
             RangeList.parse)
    ; storage_throughput_to_iops_ratio =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "StorageThroughputToIopsRatio" xml)
             DoubleRangeList.parse)
    ; supports_storage_autoscaling =
        Aws.Util.option_bind
          (Aws.Xml.member "SupportsStorageAutoscaling" xml)
          Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.supports_storage_autoscaling (fun f ->
             Aws.Query.Pair ("SupportsStorageAutoscaling", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "StorageThroughputToIopsRatio.member"
              , DoubleRangeList.to_query v.storage_throughput_to_iops_ratio ))
       ; Some
           (Aws.Query.Pair
              ( "ProvisionedStorageThroughput.member"
              , RangeList.to_query v.provisioned_storage_throughput ))
       ; Some
           (Aws.Query.Pair
              ( "IopsToStorageRatio.member"
              , DoubleRangeList.to_query v.iops_to_storage_ratio ))
       ; Some
           (Aws.Query.Pair
              ("ProvisionedIops.member", RangeList.to_query v.provisioned_iops))
       ; Some (Aws.Query.Pair ("StorageSize.member", RangeList.to_query v.storage_size))
       ; Aws.Util.option_map v.storage_type (fun f ->
             Aws.Query.Pair ("StorageType", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.supports_storage_autoscaling (fun f ->
             "SupportsStorageAutoscaling", Boolean.to_json f)
       ; Some
           ( "StorageThroughputToIopsRatio"
           , DoubleRangeList.to_json v.storage_throughput_to_iops_ratio )
       ; Some
           ( "ProvisionedStorageThroughput"
           , RangeList.to_json v.provisioned_storage_throughput )
       ; Some ("IopsToStorageRatio", DoubleRangeList.to_json v.iops_to_storage_ratio)
       ; Some ("ProvisionedIops", RangeList.to_json v.provisioned_iops)
       ; Some ("StorageSize", RangeList.to_json v.storage_size)
       ; Aws.Util.option_map v.storage_type (fun f -> "StorageType", String.to_json f)
       ])

let of_json j =
  { storage_type = Aws.Util.option_map (Aws.Json.lookup j "StorageType") String.of_json
  ; storage_size =
      RangeList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StorageSize"))
  ; provisioned_iops =
      RangeList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ProvisionedIops"))
  ; iops_to_storage_ratio =
      DoubleRangeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "IopsToStorageRatio"))
  ; provisioned_storage_throughput =
      RangeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ProvisionedStorageThroughput"))
  ; storage_throughput_to_iops_ratio =
      DoubleRangeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "StorageThroughputToIopsRatio"))
  ; supports_storage_autoscaling =
      Aws.Util.option_map (Aws.Json.lookup j "SupportsStorageAutoscaling") Boolean.of_json
  }
