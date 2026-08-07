open Aws.BaseTypes

type t =
  { accelerators : InferenceDeviceInfoList.t
  ; total_inference_memory_in_mi_b : Integer.t option
  }

let make ?(accelerators = []) ?total_inference_memory_in_mi_b () =
  { accelerators; total_inference_memory_in_mi_b }

let parse xml =
  Some
    { accelerators =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "accelerators" xml)
             InferenceDeviceInfoList.parse)
    ; total_inference_memory_in_mi_b =
        Aws.Util.option_bind
          (Aws.Xml.member "totalInferenceMemoryInMiB" xml)
          Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.total_inference_memory_in_mi_b (fun f ->
             Aws.Query.Pair ("TotalInferenceMemoryInMiB", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ("Accelerators", InferenceDeviceInfoList.to_query v.accelerators))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.total_inference_memory_in_mi_b (fun f ->
             "totalInferenceMemoryInMiB", Integer.to_json f)
       ; Some ("accelerators", InferenceDeviceInfoList.to_json v.accelerators)
       ])

let of_json j =
  { accelerators =
      InferenceDeviceInfoList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "accelerators"))
  ; total_inference_memory_in_mi_b =
      Aws.Util.option_map (Aws.Json.lookup j "totalInferenceMemoryInMiB") Integer.of_json
  }
