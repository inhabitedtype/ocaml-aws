open Aws.BaseTypes

type t =
  { name : String.t option
  ; manufacturer : String.t option
  ; count : Integer.t option
  ; logical_gpu_count : Integer.t option
  ; gpu_partition_size : Double.t option
  ; workloads : WorkloadsList.t
  ; memory_info : GpuDeviceMemoryInfo.t option
  }

let make
    ?name
    ?manufacturer
    ?count
    ?logical_gpu_count
    ?gpu_partition_size
    ?(workloads = [])
    ?memory_info
    () =
  { name
  ; manufacturer
  ; count
  ; logical_gpu_count
  ; gpu_partition_size
  ; workloads
  ; memory_info
  }

let parse xml =
  Some
    { name = Aws.Util.option_bind (Aws.Xml.member "name" xml) String.parse
    ; manufacturer = Aws.Util.option_bind (Aws.Xml.member "manufacturer" xml) String.parse
    ; count = Aws.Util.option_bind (Aws.Xml.member "count" xml) Integer.parse
    ; logical_gpu_count =
        Aws.Util.option_bind (Aws.Xml.member "logicalGpuCount" xml) Integer.parse
    ; gpu_partition_size =
        Aws.Util.option_bind (Aws.Xml.member "gpuPartitionSize" xml) Double.parse
    ; workloads =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "workloadSet" xml) WorkloadsList.parse)
    ; memory_info =
        Aws.Util.option_bind (Aws.Xml.member "memoryInfo" xml) GpuDeviceMemoryInfo.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.memory_info (fun f ->
             Aws.Query.Pair ("MemoryInfo", GpuDeviceMemoryInfo.to_query f))
       ; Some (Aws.Query.Pair ("WorkloadSet", WorkloadsList.to_query v.workloads))
       ; Aws.Util.option_map v.gpu_partition_size (fun f ->
             Aws.Query.Pair ("GpuPartitionSize", Double.to_query f))
       ; Aws.Util.option_map v.logical_gpu_count (fun f ->
             Aws.Query.Pair ("LogicalGpuCount", Integer.to_query f))
       ; Aws.Util.option_map v.count (fun f ->
             Aws.Query.Pair ("Count", Integer.to_query f))
       ; Aws.Util.option_map v.manufacturer (fun f ->
             Aws.Query.Pair ("Manufacturer", String.to_query f))
       ; Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.memory_info (fun f ->
             "memoryInfo", GpuDeviceMemoryInfo.to_json f)
       ; Some ("workloadSet", WorkloadsList.to_json v.workloads)
       ; Aws.Util.option_map v.gpu_partition_size (fun f ->
             "gpuPartitionSize", Double.to_json f)
       ; Aws.Util.option_map v.logical_gpu_count (fun f ->
             "logicalGpuCount", Integer.to_json f)
       ; Aws.Util.option_map v.count (fun f -> "count", Integer.to_json f)
       ; Aws.Util.option_map v.manufacturer (fun f -> "manufacturer", String.to_json f)
       ; Aws.Util.option_map v.name (fun f -> "name", String.to_json f)
       ])

let of_json j =
  { name = Aws.Util.option_map (Aws.Json.lookup j "name") String.of_json
  ; manufacturer = Aws.Util.option_map (Aws.Json.lookup j "manufacturer") String.of_json
  ; count = Aws.Util.option_map (Aws.Json.lookup j "count") Integer.of_json
  ; logical_gpu_count =
      Aws.Util.option_map (Aws.Json.lookup j "logicalGpuCount") Integer.of_json
  ; gpu_partition_size =
      Aws.Util.option_map (Aws.Json.lookup j "gpuPartitionSize") Double.of_json
  ; workloads =
      WorkloadsList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "workloadSet"))
  ; memory_info =
      Aws.Util.option_map (Aws.Json.lookup j "memoryInfo") GpuDeviceMemoryInfo.of_json
  }
