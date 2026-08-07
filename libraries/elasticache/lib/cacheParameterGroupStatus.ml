open Aws.BaseTypes

type t =
  { cache_parameter_group_name : String.t option
  ; parameter_apply_status : String.t option
  ; cache_node_ids_to_reboot : CacheNodeIdsList.t
  }

let make
    ?cache_parameter_group_name
    ?parameter_apply_status
    ?(cache_node_ids_to_reboot = [])
    () =
  { cache_parameter_group_name; parameter_apply_status; cache_node_ids_to_reboot }

let parse xml =
  Some
    { cache_parameter_group_name =
        Aws.Util.option_bind (Aws.Xml.member "CacheParameterGroupName" xml) String.parse
    ; parameter_apply_status =
        Aws.Util.option_bind (Aws.Xml.member "ParameterApplyStatus" xml) String.parse
    ; cache_node_ids_to_reboot =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "CacheNodeIdsToReboot" xml)
             CacheNodeIdsList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "CacheNodeIdsToReboot.member"
              , CacheNodeIdsList.to_query v.cache_node_ids_to_reboot ))
       ; Aws.Util.option_map v.parameter_apply_status (fun f ->
             Aws.Query.Pair ("ParameterApplyStatus", String.to_query f))
       ; Aws.Util.option_map v.cache_parameter_group_name (fun f ->
             Aws.Query.Pair ("CacheParameterGroupName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("CacheNodeIdsToReboot", CacheNodeIdsList.to_json v.cache_node_ids_to_reboot)
       ; Aws.Util.option_map v.parameter_apply_status (fun f ->
             "ParameterApplyStatus", String.to_json f)
       ; Aws.Util.option_map v.cache_parameter_group_name (fun f ->
             "CacheParameterGroupName", String.to_json f)
       ])

let of_json j =
  { cache_parameter_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "CacheParameterGroupName") String.of_json
  ; parameter_apply_status =
      Aws.Util.option_map (Aws.Json.lookup j "ParameterApplyStatus") String.of_json
  ; cache_node_ids_to_reboot =
      CacheNodeIdsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheNodeIdsToReboot"))
  }
