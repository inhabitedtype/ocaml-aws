open Aws.BaseTypes

type t =
  { supports_storage_autoscaling : Boolean.t option
  ; supports_storage_throughput : Boolean.t option
  ; supports_iops : Boolean.t option
  ; storage_type : String.t option
  ; min_storage_size : Integer.t option
  ; max_storage_size : Integer.t option
  ; min_iops : Integer.t option
  ; max_iops : Integer.t option
  ; min_iops_per_gib : Double.t option
  ; max_iops_per_gib : Double.t option
  ; min_storage_throughput : Integer.t option
  ; max_storage_throughput : Integer.t option
  }

let make
    ?supports_storage_autoscaling
    ?supports_storage_throughput
    ?supports_iops
    ?storage_type
    ?min_storage_size
    ?max_storage_size
    ?min_iops
    ?max_iops
    ?min_iops_per_gib
    ?max_iops_per_gib
    ?min_storage_throughput
    ?max_storage_throughput
    () =
  { supports_storage_autoscaling
  ; supports_storage_throughput
  ; supports_iops
  ; storage_type
  ; min_storage_size
  ; max_storage_size
  ; min_iops
  ; max_iops
  ; min_iops_per_gib
  ; max_iops_per_gib
  ; min_storage_throughput
  ; max_storage_throughput
  }

let parse xml =
  Some
    { supports_storage_autoscaling =
        Aws.Util.option_bind
          (Aws.Xml.member "SupportsStorageAutoscaling" xml)
          Boolean.parse
    ; supports_storage_throughput =
        Aws.Util.option_bind
          (Aws.Xml.member "SupportsStorageThroughput" xml)
          Boolean.parse
    ; supports_iops =
        Aws.Util.option_bind (Aws.Xml.member "SupportsIops" xml) Boolean.parse
    ; storage_type = Aws.Util.option_bind (Aws.Xml.member "StorageType" xml) String.parse
    ; min_storage_size =
        Aws.Util.option_bind (Aws.Xml.member "MinStorageSize" xml) Integer.parse
    ; max_storage_size =
        Aws.Util.option_bind (Aws.Xml.member "MaxStorageSize" xml) Integer.parse
    ; min_iops = Aws.Util.option_bind (Aws.Xml.member "MinIops" xml) Integer.parse
    ; max_iops = Aws.Util.option_bind (Aws.Xml.member "MaxIops" xml) Integer.parse
    ; min_iops_per_gib =
        Aws.Util.option_bind (Aws.Xml.member "MinIopsPerGib" xml) Double.parse
    ; max_iops_per_gib =
        Aws.Util.option_bind (Aws.Xml.member "MaxIopsPerGib" xml) Double.parse
    ; min_storage_throughput =
        Aws.Util.option_bind (Aws.Xml.member "MinStorageThroughput" xml) Integer.parse
    ; max_storage_throughput =
        Aws.Util.option_bind (Aws.Xml.member "MaxStorageThroughput" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_storage_throughput (fun f ->
             Aws.Query.Pair ("MaxStorageThroughput", Integer.to_query f))
       ; Aws.Util.option_map v.min_storage_throughput (fun f ->
             Aws.Query.Pair ("MinStorageThroughput", Integer.to_query f))
       ; Aws.Util.option_map v.max_iops_per_gib (fun f ->
             Aws.Query.Pair ("MaxIopsPerGib", Double.to_query f))
       ; Aws.Util.option_map v.min_iops_per_gib (fun f ->
             Aws.Query.Pair ("MinIopsPerGib", Double.to_query f))
       ; Aws.Util.option_map v.max_iops (fun f ->
             Aws.Query.Pair ("MaxIops", Integer.to_query f))
       ; Aws.Util.option_map v.min_iops (fun f ->
             Aws.Query.Pair ("MinIops", Integer.to_query f))
       ; Aws.Util.option_map v.max_storage_size (fun f ->
             Aws.Query.Pair ("MaxStorageSize", Integer.to_query f))
       ; Aws.Util.option_map v.min_storage_size (fun f ->
             Aws.Query.Pair ("MinStorageSize", Integer.to_query f))
       ; Aws.Util.option_map v.storage_type (fun f ->
             Aws.Query.Pair ("StorageType", String.to_query f))
       ; Aws.Util.option_map v.supports_iops (fun f ->
             Aws.Query.Pair ("SupportsIops", Boolean.to_query f))
       ; Aws.Util.option_map v.supports_storage_throughput (fun f ->
             Aws.Query.Pair ("SupportsStorageThroughput", Boolean.to_query f))
       ; Aws.Util.option_map v.supports_storage_autoscaling (fun f ->
             Aws.Query.Pair ("SupportsStorageAutoscaling", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_storage_throughput (fun f ->
             "MaxStorageThroughput", Integer.to_json f)
       ; Aws.Util.option_map v.min_storage_throughput (fun f ->
             "MinStorageThroughput", Integer.to_json f)
       ; Aws.Util.option_map v.max_iops_per_gib (fun f ->
             "MaxIopsPerGib", Double.to_json f)
       ; Aws.Util.option_map v.min_iops_per_gib (fun f ->
             "MinIopsPerGib", Double.to_json f)
       ; Aws.Util.option_map v.max_iops (fun f -> "MaxIops", Integer.to_json f)
       ; Aws.Util.option_map v.min_iops (fun f -> "MinIops", Integer.to_json f)
       ; Aws.Util.option_map v.max_storage_size (fun f ->
             "MaxStorageSize", Integer.to_json f)
       ; Aws.Util.option_map v.min_storage_size (fun f ->
             "MinStorageSize", Integer.to_json f)
       ; Aws.Util.option_map v.storage_type (fun f -> "StorageType", String.to_json f)
       ; Aws.Util.option_map v.supports_iops (fun f -> "SupportsIops", Boolean.to_json f)
       ; Aws.Util.option_map v.supports_storage_throughput (fun f ->
             "SupportsStorageThroughput", Boolean.to_json f)
       ; Aws.Util.option_map v.supports_storage_autoscaling (fun f ->
             "SupportsStorageAutoscaling", Boolean.to_json f)
       ])

let of_json j =
  { supports_storage_autoscaling =
      Aws.Util.option_map (Aws.Json.lookup j "SupportsStorageAutoscaling") Boolean.of_json
  ; supports_storage_throughput =
      Aws.Util.option_map (Aws.Json.lookup j "SupportsStorageThroughput") Boolean.of_json
  ; supports_iops = Aws.Util.option_map (Aws.Json.lookup j "SupportsIops") Boolean.of_json
  ; storage_type = Aws.Util.option_map (Aws.Json.lookup j "StorageType") String.of_json
  ; min_storage_size =
      Aws.Util.option_map (Aws.Json.lookup j "MinStorageSize") Integer.of_json
  ; max_storage_size =
      Aws.Util.option_map (Aws.Json.lookup j "MaxStorageSize") Integer.of_json
  ; min_iops = Aws.Util.option_map (Aws.Json.lookup j "MinIops") Integer.of_json
  ; max_iops = Aws.Util.option_map (Aws.Json.lookup j "MaxIops") Integer.of_json
  ; min_iops_per_gib =
      Aws.Util.option_map (Aws.Json.lookup j "MinIopsPerGib") Double.of_json
  ; max_iops_per_gib =
      Aws.Util.option_map (Aws.Json.lookup j "MaxIopsPerGib") Double.of_json
  ; min_storage_throughput =
      Aws.Util.option_map (Aws.Json.lookup j "MinStorageThroughput") Integer.of_json
  ; max_storage_throughput =
      Aws.Util.option_map (Aws.Json.lookup j "MaxStorageThroughput") Integer.of_json
  }
