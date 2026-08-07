open Aws.BaseTypes

type t =
  { instance_id : String.t option
  ; mac_modification_task_id : String.t option
  ; mac_system_integrity_protection_config :
      MacSystemIntegrityProtectionConfiguration.t option
  ; start_time : DateTime.t option
  ; tags : TagList.t
  ; task_state : MacModificationTaskState.t option
  ; task_type : MacModificationTaskType.t option
  }

let make
    ?instance_id
    ?mac_modification_task_id
    ?mac_system_integrity_protection_config
    ?start_time
    ?(tags = [])
    ?task_state
    ?task_type
    () =
  { instance_id
  ; mac_modification_task_id
  ; mac_system_integrity_protection_config
  ; start_time
  ; tags
  ; task_state
  ; task_type
  }

let parse xml =
  Some
    { instance_id = Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse
    ; mac_modification_task_id =
        Aws.Util.option_bind (Aws.Xml.member "macModificationTaskId" xml) String.parse
    ; mac_system_integrity_protection_config =
        Aws.Util.option_bind
          (Aws.Xml.member "macSystemIntegrityProtectionConfig" xml)
          MacSystemIntegrityProtectionConfiguration.parse
    ; start_time = Aws.Util.option_bind (Aws.Xml.member "startTime" xml) DateTime.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; task_state =
        Aws.Util.option_bind
          (Aws.Xml.member "taskState" xml)
          MacModificationTaskState.parse
    ; task_type =
        Aws.Util.option_bind (Aws.Xml.member "taskType" xml) MacModificationTaskType.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.task_type (fun f ->
             Aws.Query.Pair ("TaskType", MacModificationTaskType.to_query f))
       ; Aws.Util.option_map v.task_state (fun f ->
             Aws.Query.Pair ("TaskState", MacModificationTaskState.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.start_time (fun f ->
             Aws.Query.Pair ("StartTime", DateTime.to_query f))
       ; Aws.Util.option_map v.mac_system_integrity_protection_config (fun f ->
             Aws.Query.Pair
               ( "MacSystemIntegrityProtectionConfig"
               , MacSystemIntegrityProtectionConfiguration.to_query f ))
       ; Aws.Util.option_map v.mac_modification_task_id (fun f ->
             Aws.Query.Pair ("MacModificationTaskId", String.to_query f))
       ; Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.task_type (fun f ->
             "taskType", MacModificationTaskType.to_json f)
       ; Aws.Util.option_map v.task_state (fun f ->
             "taskState", MacModificationTaskState.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.start_time (fun f -> "startTime", DateTime.to_json f)
       ; Aws.Util.option_map v.mac_system_integrity_protection_config (fun f ->
             ( "macSystemIntegrityProtectionConfig"
             , MacSystemIntegrityProtectionConfiguration.to_json f ))
       ; Aws.Util.option_map v.mac_modification_task_id (fun f ->
             "macModificationTaskId", String.to_json f)
       ; Aws.Util.option_map v.instance_id (fun f -> "instanceId", String.to_json f)
       ])

let of_json j =
  { instance_id = Aws.Util.option_map (Aws.Json.lookup j "instanceId") String.of_json
  ; mac_modification_task_id =
      Aws.Util.option_map (Aws.Json.lookup j "macModificationTaskId") String.of_json
  ; mac_system_integrity_protection_config =
      Aws.Util.option_map
        (Aws.Json.lookup j "macSystemIntegrityProtectionConfig")
        MacSystemIntegrityProtectionConfiguration.of_json
  ; start_time = Aws.Util.option_map (Aws.Json.lookup j "startTime") DateTime.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; task_state =
      Aws.Util.option_map (Aws.Json.lookup j "taskState") MacModificationTaskState.of_json
  ; task_type =
      Aws.Util.option_map (Aws.Json.lookup j "taskType") MacModificationTaskType.of_json
  }
