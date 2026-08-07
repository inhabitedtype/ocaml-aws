open Aws.BaseTypes

type t =
  { count : Integer.t option
  ; name : String.t option
  ; core_info : NeuronDeviceCoreInfo.t option
  ; memory_info : NeuronDeviceMemoryInfo.t option
  }

let make ?count ?name ?core_info ?memory_info () = { count; name; core_info; memory_info }

let parse xml =
  Some
    { count = Aws.Util.option_bind (Aws.Xml.member "count" xml) Integer.parse
    ; name = Aws.Util.option_bind (Aws.Xml.member "name" xml) String.parse
    ; core_info =
        Aws.Util.option_bind (Aws.Xml.member "coreInfo" xml) NeuronDeviceCoreInfo.parse
    ; memory_info =
        Aws.Util.option_bind
          (Aws.Xml.member "memoryInfo" xml)
          NeuronDeviceMemoryInfo.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.memory_info (fun f ->
             Aws.Query.Pair ("MemoryInfo", NeuronDeviceMemoryInfo.to_query f))
       ; Aws.Util.option_map v.core_info (fun f ->
             Aws.Query.Pair ("CoreInfo", NeuronDeviceCoreInfo.to_query f))
       ; Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ; Aws.Util.option_map v.count (fun f ->
             Aws.Query.Pair ("Count", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.memory_info (fun f ->
             "memoryInfo", NeuronDeviceMemoryInfo.to_json f)
       ; Aws.Util.option_map v.core_info (fun f ->
             "coreInfo", NeuronDeviceCoreInfo.to_json f)
       ; Aws.Util.option_map v.name (fun f -> "name", String.to_json f)
       ; Aws.Util.option_map v.count (fun f -> "count", Integer.to_json f)
       ])

let of_json j =
  { count = Aws.Util.option_map (Aws.Json.lookup j "count") Integer.of_json
  ; name = Aws.Util.option_map (Aws.Json.lookup j "name") String.of_json
  ; core_info =
      Aws.Util.option_map (Aws.Json.lookup j "coreInfo") NeuronDeviceCoreInfo.of_json
  ; memory_info =
      Aws.Util.option_map (Aws.Json.lookup j "memoryInfo") NeuronDeviceMemoryInfo.of_json
  }
