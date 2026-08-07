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
    { core_count = Aws.Util.option_bind (Aws.Xml.member "coreCount" xml) Integer.parse
    ; threads_per_core =
        Aws.Util.option_bind (Aws.Xml.member "threadsPerCore" xml) Integer.parse
    ; amd_sev_snp =
        Aws.Util.option_bind (Aws.Xml.member "amdSevSnp" xml) AmdSevSnpSpecification.parse
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
             "nestedVirtualization", NestedVirtualizationSpecification.to_json f)
       ; Aws.Util.option_map v.amd_sev_snp (fun f ->
             "amdSevSnp", AmdSevSnpSpecification.to_json f)
       ; Aws.Util.option_map v.threads_per_core (fun f ->
             "threadsPerCore", Integer.to_json f)
       ; Aws.Util.option_map v.core_count (fun f -> "coreCount", Integer.to_json f)
       ])

let of_json j =
  { core_count = Aws.Util.option_map (Aws.Json.lookup j "coreCount") Integer.of_json
  ; threads_per_core =
      Aws.Util.option_map (Aws.Json.lookup j "threadsPerCore") Integer.of_json
  ; amd_sev_snp =
      Aws.Util.option_map (Aws.Json.lookup j "amdSevSnp") AmdSevSnpSpecification.of_json
  ; nested_virtualization =
      Aws.Util.option_map
        (Aws.Json.lookup j "nestedVirtualization")
        NestedVirtualizationSpecification.of_json
  }
