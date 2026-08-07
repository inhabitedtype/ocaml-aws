open Aws.BaseTypes

type t =
  { alarm_mute_rule_summaries : AlarmMuteRuleSummaries.t
  ; next_token : String.t option
  }

let make ?(alarm_mute_rule_summaries = []) ?next_token () =
  { alarm_mute_rule_summaries; next_token }

let parse xml =
  Some
    { alarm_mute_rule_summaries =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AlarmMuteRuleSummaries" xml)
             AlarmMuteRuleSummaries.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "AlarmMuteRuleSummaries.member"
              , AlarmMuteRuleSummaries.to_query v.alarm_mute_rule_summaries ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some
           ( "AlarmMuteRuleSummaries"
           , AlarmMuteRuleSummaries.to_json v.alarm_mute_rule_summaries )
       ])

let of_json j =
  { alarm_mute_rule_summaries =
      AlarmMuteRuleSummaries.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AlarmMuteRuleSummaries"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
