open Aws.BaseTypes

type t =
  { alarm_name : String.t option
  ; statuses : AlarmMuteRuleStatuses.t
  ; max_records : Integer.t option
  ; next_token : String.t option
  }

let make ?alarm_name ?(statuses = []) ?max_records ?next_token () =
  { alarm_name; statuses; max_records; next_token }

let parse xml =
  Some
    { alarm_name = Aws.Util.option_bind (Aws.Xml.member "AlarmName" xml) String.parse
    ; statuses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Statuses" xml)
             AlarmMuteRuleStatuses.parse)
    ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.max_records (fun f ->
             Aws.Query.Pair ("MaxRecords", Integer.to_query f))
       ; Some
           (Aws.Query.Pair ("Statuses.member", AlarmMuteRuleStatuses.to_query v.statuses))
       ; Aws.Util.option_map v.alarm_name (fun f ->
             Aws.Query.Pair ("AlarmName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
       ; Some ("Statuses", AlarmMuteRuleStatuses.to_json v.statuses)
       ; Aws.Util.option_map v.alarm_name (fun f -> "AlarmName", String.to_json f)
       ])

let of_json j =
  { alarm_name = Aws.Util.option_map (Aws.Json.lookup j "AlarmName") String.of_json
  ; statuses =
      AlarmMuteRuleStatuses.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Statuses"))
  ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
