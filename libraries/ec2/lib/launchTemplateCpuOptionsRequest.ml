open Aws.BaseTypes

type t =
  { core_count : Integer.t option
  ; threads_per_core : Integer.t option
  ; amd_sev_snp : AmdSevSnpSpecification.t option
  ; nested_virtualization : NestedVirtualizationSpecification.t option
  }

let make ?core_count ?threads_per_core ?amd_sev_snp ?nested_virtualization () =
  { core_count; threads_per_core; amd_sev_snp; nested_virtualization }

let parse xml =
  Some
    { core_count = Aws.Util.option_bind (Aws.Xml.member "CoreCount" xml) Integer.parse
    ; threads_per_core =
        Aws.Util.option_bind (Aws.Xml.member "ThreadsPerCore" xml) Integer.parse
    ; amd_sev_snp =
        Aws.Util.option_bind (Aws.Xml.member "AmdSevSnp" xml) AmdSevSnpSpecification.parse
    ; nested_virtualization =
        Aws.Util.option_bind
          (Aws.Xml.member "NestedVirtualization" xml)
          NestedVirtualizationSpecification.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.nested_virtualization (fun f ->
             Aws.Query.Pair
               ("NestedVirtualization", NestedVirtualizationSpecification.to_query f))
       ; Aws.Util.option_map v.amd_sev_snp (fun f ->
             Aws.Query.Pair ("AmdSevSnp", AmdSevSnpSpecification.to_query f))
       ; Aws.Util.option_map v.threads_per_core (fun f ->
             Aws.Query.Pair ("ThreadsPerCore", Integer.to_query f))
       ; Aws.Util.option_map v.core_count (fun f ->
             Aws.Query.Pair ("CoreCount", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.nested_virtualization (fun f ->
             "NestedVirtualization", NestedVirtualizationSpecification.to_json f)
       ; Aws.Util.option_map v.amd_sev_snp (fun f ->
             "AmdSevSnp", AmdSevSnpSpecification.to_json f)
       ; Aws.Util.option_map v.threads_per_core (fun f ->
             "ThreadsPerCore", Integer.to_json f)
       ; Aws.Util.option_map v.core_count (fun f -> "CoreCount", Integer.to_json f)
       ])

let of_json j =
  { core_count = Aws.Util.option_map (Aws.Json.lookup j "CoreCount") Integer.of_json
  ; threads_per_core =
      Aws.Util.option_map (Aws.Json.lookup j "ThreadsPerCore") Integer.of_json
  ; amd_sev_snp =
      Aws.Util.option_map (Aws.Json.lookup j "AmdSevSnp") AmdSevSnpSpecification.of_json
  ; nested_virtualization =
      Aws.Util.option_map
        (Aws.Json.lookup j "NestedVirtualization")
        NestedVirtualizationSpecification.of_json
  }
