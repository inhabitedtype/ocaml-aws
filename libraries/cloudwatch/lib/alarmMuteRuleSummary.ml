open Aws.BaseTypes

type t =
  { alarm_mute_rule_arn : String.t option
  ; expire_date : DateTime.t option
  ; status : AlarmMuteRuleStatus.t option
  ; mute_type : String.t option
  ; last_updated_timestamp : DateTime.t option
  }

let make ?alarm_mute_rule_arn ?expire_date ?status ?mute_type ?last_updated_timestamp () =
  { alarm_mute_rule_arn; expire_date; status; mute_type; last_updated_timestamp }

let parse xml =
  Some
    { alarm_mute_rule_arn =
        Aws.Util.option_bind (Aws.Xml.member "AlarmMuteRuleArn" xml) String.parse
    ; expire_date = Aws.Util.option_bind (Aws.Xml.member "ExpireDate" xml) DateTime.parse
    ; status =
        Aws.Util.option_bind (Aws.Xml.member "Status" xml) AlarmMuteRuleStatus.parse
    ; mute_type = Aws.Util.option_bind (Aws.Xml.member "MuteType" xml) String.parse
    ; last_updated_timestamp =
        Aws.Util.option_bind (Aws.Xml.member "LastUpdatedTimestamp" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.last_updated_timestamp (fun f ->
             Aws.Query.Pair ("LastUpdatedTimestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.mute_type (fun f ->
             Aws.Query.Pair ("MuteType", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", AlarmMuteRuleStatus.to_query f))
       ; Aws.Util.option_map v.expire_date (fun f ->
             Aws.Query.Pair ("ExpireDate", DateTime.to_query f))
       ; Aws.Util.option_map v.alarm_mute_rule_arn (fun f ->
             Aws.Query.Pair ("AlarmMuteRuleArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.last_updated_timestamp (fun f ->
             "LastUpdatedTimestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.mute_type (fun f -> "MuteType", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", AlarmMuteRuleStatus.to_json f)
       ; Aws.Util.option_map v.expire_date (fun f -> "ExpireDate", DateTime.to_json f)
       ; Aws.Util.option_map v.alarm_mute_rule_arn (fun f ->
             "AlarmMuteRuleArn", String.to_json f)
       ])

let of_json j =
  { alarm_mute_rule_arn =
      Aws.Util.option_map (Aws.Json.lookup j "AlarmMuteRuleArn") String.of_json
  ; expire_date = Aws.Util.option_map (Aws.Json.lookup j "ExpireDate") DateTime.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") AlarmMuteRuleStatus.of_json
  ; mute_type = Aws.Util.option_map (Aws.Json.lookup j "MuteType") String.of_json
  ; last_updated_timestamp =
      Aws.Util.option_map (Aws.Json.lookup j "LastUpdatedTimestamp") DateTime.of_json
  }
