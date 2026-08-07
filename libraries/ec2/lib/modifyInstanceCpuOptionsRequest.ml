open Aws.BaseTypes

type t =
  { instance_id : String.t
  ; core_count : Integer.t option
  ; threads_per_core : Integer.t option
  ; nested_virtualization : NestedVirtualizationSpecification.t option
  ; dry_run : Boolean.t option
  }

let make ~instance_id ?core_count ?threads_per_core ?nested_virtualization ?dry_run () =
  { instance_id; core_count; threads_per_core; nested_virtualization; dry_run }

let parse xml =
  Some
    { instance_id =
        Aws.Xml.required
          "InstanceId"
          (Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse)
    ; core_count = Aws.Util.option_bind (Aws.Xml.member "CoreCount" xml) Integer.parse
    ; threads_per_core =
        Aws.Util.option_bind (Aws.Xml.member "ThreadsPerCore" xml) Integer.parse
    ; nested_virtualization =
        Aws.Util.option_bind
          (Aws.Xml.member "NestedVirtualization" xml)
          NestedVirtualizationSpecification.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.nested_virtualization (fun f ->
             Aws.Query.Pair
               ("NestedVirtualization", NestedVirtualizationSpecification.to_query f))
       ; Aws.Util.option_map v.threads_per_core (fun f ->
             Aws.Query.Pair ("ThreadsPerCore", Integer.to_query f))
       ; Aws.Util.option_map v.core_count (fun f ->
             Aws.Query.Pair ("CoreCount", Integer.to_query f))
       ; Some (Aws.Query.Pair ("InstanceId", String.to_query v.instance_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.nested_virtualization (fun f ->
             "NestedVirtualization", NestedVirtualizationSpecification.to_json f)
       ; Aws.Util.option_map v.threads_per_core (fun f ->
             "ThreadsPerCore", Integer.to_json f)
       ; Aws.Util.option_map v.core_count (fun f -> "CoreCount", Integer.to_json f)
       ; Some ("InstanceId", String.to_json v.instance_id)
       ])

let of_json j =
  { instance_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceId"))
  ; core_count = Aws.Util.option_map (Aws.Json.lookup j "CoreCount") Integer.of_json
  ; threads_per_core =
      Aws.Util.option_map (Aws.Json.lookup j "ThreadsPerCore") Integer.of_json
  ; nested_virtualization =
      Aws.Util.option_map
        (Aws.Json.lookup j "NestedVirtualization")
        NestedVirtualizationSpecification.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
