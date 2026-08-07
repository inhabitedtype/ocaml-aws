open Aws.BaseTypes

type t =
  { volume_name : String.t
  ; allocated_storage : Integer.t option
  ; i_o_p_s : Integer.t option
  ; max_allocated_storage : Integer.t option
  ; storage_throughput : Integer.t option
  ; storage_type : String.t option
  }

let make
    ~volume_name
    ?allocated_storage
    ?i_o_p_s
    ?max_allocated_storage
    ?storage_throughput
    ?storage_type
    () =
  { volume_name
  ; allocated_storage
  ; i_o_p_s
  ; max_allocated_storage
  ; storage_throughput
  ; storage_type
  }

let parse xml =
  Some
    { volume_name =
        Aws.Xml.required
          "VolumeName"
          (Aws.Util.option_bind (Aws.Xml.member "VolumeName" xml) String.parse)
    ; allocated_storage =
        Aws.Util.option_bind (Aws.Xml.member "AllocatedStorage" xml) Integer.parse
    ; i_o_p_s = Aws.Util.option_bind (Aws.Xml.member "IOPS" xml) Integer.parse
    ; max_allocated_storage =
        Aws.Util.option_bind (Aws.Xml.member "MaxAllocatedStorage" xml) Integer.parse
    ; storage_throughput =
        Aws.Util.option_bind (Aws.Xml.member "StorageThroughput" xml) Integer.parse
    ; storage_type = Aws.Util.option_bind (Aws.Xml.member "StorageType" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.storage_type (fun f ->
             Aws.Query.Pair ("StorageType", String.to_query f))
       ; Aws.Util.option_map v.storage_throughput (fun f ->
             Aws.Query.Pair ("StorageThroughput", Integer.to_query f))
       ; Aws.Util.option_map v.max_allocated_storage (fun f ->
             Aws.Query.Pair ("MaxAllocatedStorage", Integer.to_query f))
       ; Aws.Util.option_map v.i_o_p_s (fun f ->
             Aws.Query.Pair ("IOPS", Integer.to_query f))
       ; Aws.Util.option_map v.allocated_storage (fun f ->
             Aws.Query.Pair ("AllocatedStorage", Integer.to_query f))
       ; Some (Aws.Query.Pair ("VolumeName", String.to_query v.volume_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.storage_type (fun f -> "StorageType", String.to_json f)
       ; Aws.Util.option_map v.storage_throughput (fun f ->
             "StorageThroughput", Integer.to_json f)
       ; Aws.Util.option_map v.max_allocated_storage (fun f ->
             "MaxAllocatedStorage", Integer.to_json f)
       ; Aws.Util.option_map v.i_o_p_s (fun f -> "IOPS", Integer.to_json f)
       ; Aws.Util.option_map v.allocated_storage (fun f ->
             "AllocatedStorage", Integer.to_json f)
       ; Some ("VolumeName", String.to_json v.volume_name)
       ])

let of_json j =
  { volume_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "VolumeName"))
  ; allocated_storage =
      Aws.Util.option_map (Aws.Json.lookup j "AllocatedStorage") Integer.of_json
  ; i_o_p_s = Aws.Util.option_map (Aws.Json.lookup j "IOPS") Integer.of_json
  ; max_allocated_storage =
      Aws.Util.option_map (Aws.Json.lookup j "MaxAllocatedStorage") Integer.of_json
  ; storage_throughput =
      Aws.Util.option_map (Aws.Json.lookup j "StorageThroughput") Integer.of_json
  ; storage_type = Aws.Util.option_map (Aws.Json.lookup j "StorageType") String.of_json
  }
