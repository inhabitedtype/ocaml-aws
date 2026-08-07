open Aws.BaseTypes

type t =
  { instance_id : String.t option
  ; core_count : Integer.t option
  ; threads_per_core : Integer.t option
  ; nested_virtualization : NestedVirtualizationSpecification.t option
  }

let make ?instance_id ?core_count ?threads_per_core ?nested_virtualization () =
  { instance_id; core_count; threads_per_core; nested_virtualization }

let parse xml =
  Some
    { instance_id = Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse
    ; core_count = Aws.Util.option_bind (Aws.Xml.member "coreCount" xml) Integer.parse
    ; threads_per_core =
        Aws.Util.option_bind (Aws.Xml.member "threadsPerCore" xml) Integer.parse
    ; nested_virtualization =
        Aws.Util.option_bind
          (Aws.Xml.member "nestedVirtualization" xml)
          NestedVirtualizationSpecification.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.nested_virtualization (fun f ->
             Aws.Query.Pair
               ("NestedVirtualization", NestedVirtualizationSpecification.to_query f))
       ; Aws.Util.option_map v.threads_per_core (fun f ->
             Aws.Query.Pair ("ThreadsPerCore", Integer.to_query f))
       ; Aws.Util.option_map v.core_count (fun f ->
             Aws.Query.Pair ("CoreCount", Integer.to_query f))
       ; Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.nested_virtualization (fun f ->
             "nestedVirtualization", NestedVirtualizationSpecification.to_json f)
       ; Aws.Util.option_map v.threads_per_core (fun f ->
             "threadsPerCore", Integer.to_json f)
       ; Aws.Util.option_map v.core_count (fun f -> "coreCount", Integer.to_json f)
       ; Aws.Util.option_map v.instance_id (fun f -> "instanceId", String.to_json f)
       ])

let of_json j =
  { instance_id = Aws.Util.option_map (Aws.Json.lookup j "instanceId") String.of_json
  ; core_count = Aws.Util.option_map (Aws.Json.lookup j "coreCount") Integer.of_json
  ; threads_per_core =
      Aws.Util.option_map (Aws.Json.lookup j "threadsPerCore") Integer.of_json
  ; nested_virtualization =
      Aws.Util.option_map
        (Aws.Json.lookup j "nestedVirtualization")
        NestedVirtualizationSpecification.of_json
  }
