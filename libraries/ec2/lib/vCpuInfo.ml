open Aws.BaseTypes

type t =
  { default_v_cpus : Integer.t option
  ; default_cores : Integer.t option
  ; default_threads_per_core : Integer.t option
  ; valid_cores : CoreCountList.t
  ; valid_threads_per_core : ThreadsPerCoreList.t
  }

let make
    ?default_v_cpus
    ?default_cores
    ?default_threads_per_core
    ?(valid_cores = [])
    ?(valid_threads_per_core = [])
    () =
  { default_v_cpus
  ; default_cores
  ; default_threads_per_core
  ; valid_cores
  ; valid_threads_per_core
  }

let parse xml =
  Some
    { default_v_cpus =
        Aws.Util.option_bind (Aws.Xml.member "defaultVCpus" xml) Integer.parse
    ; default_cores =
        Aws.Util.option_bind (Aws.Xml.member "defaultCores" xml) Integer.parse
    ; default_threads_per_core =
        Aws.Util.option_bind (Aws.Xml.member "defaultThreadsPerCore" xml) Integer.parse
    ; valid_cores =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "validCores" xml) CoreCountList.parse)
    ; valid_threads_per_core =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "validThreadsPerCore" xml)
             ThreadsPerCoreList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("ValidThreadsPerCore", ThreadsPerCoreList.to_query v.valid_threads_per_core))
       ; Some (Aws.Query.Pair ("ValidCores", CoreCountList.to_query v.valid_cores))
       ; Aws.Util.option_map v.default_threads_per_core (fun f ->
             Aws.Query.Pair ("DefaultThreadsPerCore", Integer.to_query f))
       ; Aws.Util.option_map v.default_cores (fun f ->
             Aws.Query.Pair ("DefaultCores", Integer.to_query f))
       ; Aws.Util.option_map v.default_v_cpus (fun f ->
             Aws.Query.Pair ("DefaultVCpus", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("validThreadsPerCore", ThreadsPerCoreList.to_json v.valid_threads_per_core)
       ; Some ("validCores", CoreCountList.to_json v.valid_cores)
       ; Aws.Util.option_map v.default_threads_per_core (fun f ->
             "defaultThreadsPerCore", Integer.to_json f)
       ; Aws.Util.option_map v.default_cores (fun f -> "defaultCores", Integer.to_json f)
       ; Aws.Util.option_map v.default_v_cpus (fun f -> "defaultVCpus", Integer.to_json f)
       ])

let of_json j =
  { default_v_cpus =
      Aws.Util.option_map (Aws.Json.lookup j "defaultVCpus") Integer.of_json
  ; default_cores = Aws.Util.option_map (Aws.Json.lookup j "defaultCores") Integer.of_json
  ; default_threads_per_core =
      Aws.Util.option_map (Aws.Json.lookup j "defaultThreadsPerCore") Integer.of_json
  ; valid_cores =
      CoreCountList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "validCores"))
  ; valid_threads_per_core =
      ThreadsPerCoreList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "validThreadsPerCore"))
  }
