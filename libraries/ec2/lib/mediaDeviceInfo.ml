open Aws.BaseTypes

type t =
  { count : Integer.t option
  ; name : String.t option
  ; manufacturer : String.t option
  ; memory_info : MediaDeviceMemoryInfo.t option
  }

let make ?count ?name ?manufacturer ?memory_info () =
  { count; name; manufacturer; memory_info }

let parse xml =
  Some
    { count = Aws.Util.option_bind (Aws.Xml.member "count" xml) Integer.parse
    ; name = Aws.Util.option_bind (Aws.Xml.member "name" xml) String.parse
    ; manufacturer = Aws.Util.option_bind (Aws.Xml.member "manufacturer" xml) String.parse
    ; memory_info =
        Aws.Util.option_bind (Aws.Xml.member "memoryInfo" xml) MediaDeviceMemoryInfo.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.memory_info (fun f ->
             Aws.Query.Pair ("MemoryInfo", MediaDeviceMemoryInfo.to_query f))
       ; Aws.Util.option_map v.manufacturer (fun f ->
             Aws.Query.Pair ("Manufacturer", String.to_query f))
       ; Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ; Aws.Util.option_map v.count (fun f ->
             Aws.Query.Pair ("Count", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.memory_info (fun f ->
             "memoryInfo", MediaDeviceMemoryInfo.to_json f)
       ; Aws.Util.option_map v.manufacturer (fun f -> "manufacturer", String.to_json f)
       ; Aws.Util.option_map v.name (fun f -> "name", String.to_json f)
       ; Aws.Util.option_map v.count (fun f -> "count", Integer.to_json f)
       ])

let of_json j =
  { count = Aws.Util.option_map (Aws.Json.lookup j "count") Integer.of_json
  ; name = Aws.Util.option_map (Aws.Json.lookup j "name") String.of_json
  ; manufacturer = Aws.Util.option_map (Aws.Json.lookup j "manufacturer") String.of_json
  ; memory_info =
      Aws.Util.option_map (Aws.Json.lookup j "memoryInfo") MediaDeviceMemoryInfo.of_json
  }
