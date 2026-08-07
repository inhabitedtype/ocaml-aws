open Aws.BaseTypes

type t =
  { accelerators : MediaDeviceInfoList.t
  ; total_media_memory_in_mi_b : Integer.t option
  }

let make ?(accelerators = []) ?total_media_memory_in_mi_b () =
  { accelerators; total_media_memory_in_mi_b }

let parse xml =
  Some
    { accelerators =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "accelerators" xml)
             MediaDeviceInfoList.parse)
    ; total_media_memory_in_mi_b =
        Aws.Util.option_bind (Aws.Xml.member "totalMediaMemoryInMiB" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.total_media_memory_in_mi_b (fun f ->
             Aws.Query.Pair ("TotalMediaMemoryInMiB", Integer.to_query f))
       ; Some
           (Aws.Query.Pair ("Accelerators", MediaDeviceInfoList.to_query v.accelerators))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.total_media_memory_in_mi_b (fun f ->
             "totalMediaMemoryInMiB", Integer.to_json f)
       ; Some ("accelerators", MediaDeviceInfoList.to_json v.accelerators)
       ])

let of_json j =
  { accelerators =
      MediaDeviceInfoList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "accelerators"))
  ; total_media_memory_in_mi_b =
      Aws.Util.option_map (Aws.Json.lookup j "totalMediaMemoryInMiB") Integer.of_json
  }
