open Aws.BaseTypes

type t =
  { name : String.t option
  ; alarm_mute_rule_arn : String.t option
  ; description : String.t option
  ; rule : Rule.t option
  ; mute_targets : MuteTargets.t option
  ; start_date : DateTime.t option
  ; expire_date : DateTime.t option
  ; status : AlarmMuteRuleStatus.t option
  ; last_updated_timestamp : DateTime.t option
  ; mute_type : String.t option
  }

let make
    ?name
    ?alarm_mute_rule_arn
    ?description
    ?rule
    ?mute_targets
    ?start_date
    ?expire_date
    ?status
    ?last_updated_timestamp
    ?mute_type
    () =
  { name
  ; alarm_mute_rule_arn
  ; description
  ; rule
  ; mute_targets
  ; start_date
  ; expire_date
  ; status
  ; last_updated_timestamp
  ; mute_type
  }

let parse xml =
  Some
    { name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
    ; alarm_mute_rule_arn =
        Aws.Util.option_bind (Aws.Xml.member "AlarmMuteRuleArn" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; rule = Aws.Util.option_bind (Aws.Xml.member "Rule" xml) Rule.parse
    ; mute_targets =
        Aws.Util.option_bind (Aws.Xml.member "MuteTargets" xml) MuteTargets.parse
    ; start_date = Aws.Util.option_bind (Aws.Xml.member "StartDate" xml) DateTime.parse
    ; expire_date = Aws.Util.option_bind (Aws.Xml.member "ExpireDate" xml) DateTime.parse
    ; status =
        Aws.Util.option_bind (Aws.Xml.member "Status" xml) AlarmMuteRuleStatus.parse
    ; last_updated_timestamp =
        Aws.Util.option_bind (Aws.Xml.member "LastUpdatedTimestamp" xml) DateTime.parse
    ; mute_type = Aws.Util.option_bind (Aws.Xml.member "MuteType" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.mute_type (fun f ->
             Aws.Query.Pair ("MuteType", String.to_query f))
       ; Aws.Util.option_map v.last_updated_timestamp (fun f ->
             Aws.Query.Pair ("LastUpdatedTimestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", AlarmMuteRuleStatus.to_query f))
       ; Aws.Util.option_map v.expire_date (fun f ->
             Aws.Query.Pair ("ExpireDate", DateTime.to_query f))
       ; Aws.Util.option_map v.start_date (fun f ->
             Aws.Query.Pair ("StartDate", DateTime.to_query f))
       ; Aws.Util.option_map v.mute_targets (fun f ->
             Aws.Query.Pair ("MuteTargets", MuteTargets.to_query f))
       ; Aws.Util.option_map v.rule (fun f -> Aws.Query.Pair ("Rule", Rule.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.alarm_mute_rule_arn (fun f ->
             Aws.Query.Pair ("AlarmMuteRuleArn", String.to_query f))
       ; Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.mute_type (fun f -> "MuteType", String.to_json f)
       ; Aws.Util.option_map v.last_updated_timestamp (fun f ->
             "LastUpdatedTimestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", AlarmMuteRuleStatus.to_json f)
       ; Aws.Util.option_map v.expire_date (fun f -> "ExpireDate", DateTime.to_json f)
       ; Aws.Util.option_map v.start_date (fun f -> "StartDate", DateTime.to_json f)
       ; Aws.Util.option_map v.mute_targets (fun f ->
             "MuteTargets", MuteTargets.to_json f)
       ; Aws.Util.option_map v.rule (fun f -> "Rule", Rule.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.alarm_mute_rule_arn (fun f ->
             "AlarmMuteRuleArn", String.to_json f)
       ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
       ])

let of_json j =
  { name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
  ; alarm_mute_rule_arn =
      Aws.Util.option_map (Aws.Json.lookup j "AlarmMuteRuleArn") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; rule = Aws.Util.option_map (Aws.Json.lookup j "Rule") Rule.of_json
  ; mute_targets =
      Aws.Util.option_map (Aws.Json.lookup j "MuteTargets") MuteTargets.of_json
  ; start_date = Aws.Util.option_map (Aws.Json.lookup j "StartDate") DateTime.of_json
  ; expire_date = Aws.Util.option_map (Aws.Json.lookup j "ExpireDate") DateTime.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") AlarmMuteRuleStatus.of_json
  ; last_updated_timestamp =
      Aws.Util.option_map (Aws.Json.lookup j "LastUpdatedTimestamp") DateTime.of_json
  ; mute_type = Aws.Util.option_map (Aws.Json.lookup j "MuteType") String.of_json
  }
