open Aws.BaseTypes

type t =
  { gpus : GpuDeviceInfoList.t
  ; total_gpu_memory_in_mi_b : Integer.t option
  }

let make ?(gpus = []) ?total_gpu_memory_in_mi_b () = { gpus; total_gpu_memory_in_mi_b }

let parse xml =
  Some
    { gpus =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "gpus" xml) GpuDeviceInfoList.parse)
    ; total_gpu_memory_in_mi_b =
        Aws.Util.option_bind (Aws.Xml.member "totalGpuMemoryInMiB" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.total_gpu_memory_in_mi_b (fun f ->
             Aws.Query.Pair ("TotalGpuMemoryInMiB", Integer.to_query f))
       ; Some (Aws.Query.Pair ("Gpus", GpuDeviceInfoList.to_query v.gpus))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.total_gpu_memory_in_mi_b (fun f ->
             "totalGpuMemoryInMiB", Integer.to_json f)
       ; Some ("gpus", GpuDeviceInfoList.to_json v.gpus)
       ])

let of_json j =
  { gpus = GpuDeviceInfoList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "gpus"))
  ; total_gpu_memory_in_mi_b =
      Aws.Util.option_map (Aws.Json.lookup j "totalGpuMemoryInMiB") Integer.of_json
  }
