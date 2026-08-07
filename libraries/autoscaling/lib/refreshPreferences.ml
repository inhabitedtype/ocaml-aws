open Aws.BaseTypes

type t =
  { min_healthy_percentage : Integer.t option
  ; instance_warmup : Integer.t option
  ; checkpoint_percentages : CheckpointPercentages.t
  ; checkpoint_delay : Integer.t option
  ; skip_matching : Boolean.t option
  ; auto_rollback : Boolean.t option
  ; scale_in_protected_instances : ScaleInProtectedInstances.t option
  ; standby_instances : StandbyInstances.t option
  ; alarm_specification : AlarmSpecification.t option
  ; max_healthy_percentage : Integer.t option
  ; bake_time : Integer.t option
  }

let make
    ?min_healthy_percentage
    ?instance_warmup
    ?(checkpoint_percentages = [])
    ?checkpoint_delay
    ?skip_matching
    ?auto_rollback
    ?scale_in_protected_instances
    ?standby_instances
    ?alarm_specification
    ?max_healthy_percentage
    ?bake_time
    () =
  { min_healthy_percentage
  ; instance_warmup
  ; checkpoint_percentages
  ; checkpoint_delay
  ; skip_matching
  ; auto_rollback
  ; scale_in_protected_instances
  ; standby_instances
  ; alarm_specification
  ; max_healthy_percentage
  ; bake_time
  }

let parse xml =
  Some
    { min_healthy_percentage =
        Aws.Util.option_bind (Aws.Xml.member "MinHealthyPercentage" xml) Integer.parse
    ; instance_warmup =
        Aws.Util.option_bind (Aws.Xml.member "InstanceWarmup" xml) Integer.parse
    ; checkpoint_percentages =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "CheckpointPercentages" xml)
             CheckpointPercentages.parse)
    ; checkpoint_delay =
        Aws.Util.option_bind (Aws.Xml.member "CheckpointDelay" xml) Integer.parse
    ; skip_matching =
        Aws.Util.option_bind (Aws.Xml.member "SkipMatching" xml) Boolean.parse
    ; auto_rollback =
        Aws.Util.option_bind (Aws.Xml.member "AutoRollback" xml) Boolean.parse
    ; scale_in_protected_instances =
        Aws.Util.option_bind
          (Aws.Xml.member "ScaleInProtectedInstances" xml)
          ScaleInProtectedInstances.parse
    ; standby_instances =
        Aws.Util.option_bind
          (Aws.Xml.member "StandbyInstances" xml)
          StandbyInstances.parse
    ; alarm_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "AlarmSpecification" xml)
          AlarmSpecification.parse
    ; max_healthy_percentage =
        Aws.Util.option_bind (Aws.Xml.member "MaxHealthyPercentage" xml) Integer.parse
    ; bake_time = Aws.Util.option_bind (Aws.Xml.member "BakeTime" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.bake_time (fun f ->
             Aws.Query.Pair ("BakeTime", Integer.to_query f))
       ; Aws.Util.option_map v.max_healthy_percentage (fun f ->
             Aws.Query.Pair ("MaxHealthyPercentage", Integer.to_query f))
       ; Aws.Util.option_map v.alarm_specification (fun f ->
             Aws.Query.Pair ("AlarmSpecification", AlarmSpecification.to_query f))
       ; Aws.Util.option_map v.standby_instances (fun f ->
             Aws.Query.Pair ("StandbyInstances", StandbyInstances.to_query f))
       ; Aws.Util.option_map v.scale_in_protected_instances (fun f ->
             Aws.Query.Pair
               ("ScaleInProtectedInstances", ScaleInProtectedInstances.to_query f))
       ; Aws.Util.option_map v.auto_rollback (fun f ->
             Aws.Query.Pair ("AutoRollback", Boolean.to_query f))
       ; Aws.Util.option_map v.skip_matching (fun f ->
             Aws.Query.Pair ("SkipMatching", Boolean.to_query f))
       ; Aws.Util.option_map v.checkpoint_delay (fun f ->
             Aws.Query.Pair ("CheckpointDelay", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "CheckpointPercentages.member"
              , CheckpointPercentages.to_query v.checkpoint_percentages ))
       ; Aws.Util.option_map v.instance_warmup (fun f ->
             Aws.Query.Pair ("InstanceWarmup", Integer.to_query f))
       ; Aws.Util.option_map v.min_healthy_percentage (fun f ->
             Aws.Query.Pair ("MinHealthyPercentage", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.bake_time (fun f -> "BakeTime", Integer.to_json f)
       ; Aws.Util.option_map v.max_healthy_percentage (fun f ->
             "MaxHealthyPercentage", Integer.to_json f)
       ; Aws.Util.option_map v.alarm_specification (fun f ->
             "AlarmSpecification", AlarmSpecification.to_json f)
       ; Aws.Util.option_map v.standby_instances (fun f ->
             "StandbyInstances", StandbyInstances.to_json f)
       ; Aws.Util.option_map v.scale_in_protected_instances (fun f ->
             "ScaleInProtectedInstances", ScaleInProtectedInstances.to_json f)
       ; Aws.Util.option_map v.auto_rollback (fun f -> "AutoRollback", Boolean.to_json f)
       ; Aws.Util.option_map v.skip_matching (fun f -> "SkipMatching", Boolean.to_json f)
       ; Aws.Util.option_map v.checkpoint_delay (fun f ->
             "CheckpointDelay", Integer.to_json f)
       ; Some
           ( "CheckpointPercentages"
           , CheckpointPercentages.to_json v.checkpoint_percentages )
       ; Aws.Util.option_map v.instance_warmup (fun f ->
             "InstanceWarmup", Integer.to_json f)
       ; Aws.Util.option_map v.min_healthy_percentage (fun f ->
             "MinHealthyPercentage", Integer.to_json f)
       ])

let of_json j =
  { min_healthy_percentage =
      Aws.Util.option_map (Aws.Json.lookup j "MinHealthyPercentage") Integer.of_json
  ; instance_warmup =
      Aws.Util.option_map (Aws.Json.lookup j "InstanceWarmup") Integer.of_json
  ; checkpoint_percentages =
      CheckpointPercentages.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CheckpointPercentages"))
  ; checkpoint_delay =
      Aws.Util.option_map (Aws.Json.lookup j "CheckpointDelay") Integer.of_json
  ; skip_matching = Aws.Util.option_map (Aws.Json.lookup j "SkipMatching") Boolean.of_json
  ; auto_rollback = Aws.Util.option_map (Aws.Json.lookup j "AutoRollback") Boolean.of_json
  ; scale_in_protected_instances =
      Aws.Util.option_map
        (Aws.Json.lookup j "ScaleInProtectedInstances")
        ScaleInProtectedInstances.of_json
  ; standby_instances =
      Aws.Util.option_map (Aws.Json.lookup j "StandbyInstances") StandbyInstances.of_json
  ; alarm_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "AlarmSpecification")
        AlarmSpecification.of_json
  ; max_healthy_percentage =
      Aws.Util.option_map (Aws.Json.lookup j "MaxHealthyPercentage") Integer.of_json
  ; bake_time = Aws.Util.option_map (Aws.Json.lookup j "BakeTime") Integer.of_json
  }
