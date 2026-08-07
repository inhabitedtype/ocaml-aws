open Aws.BaseTypes

type t = { alarm_mute_rule_name : String.t }

let make ~alarm_mute_rule_name () = { alarm_mute_rule_name }

let parse xml =
  Some
    { alarm_mute_rule_name =
        Aws.Xml.required
          "AlarmMuteRuleName"
          (Aws.Util.option_bind (Aws.Xml.member "AlarmMuteRuleName" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("AlarmMuteRuleName", String.to_query v.alarm_mute_rule_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("AlarmMuteRuleName", String.to_json v.alarm_mute_rule_name) ])

let of_json j =
  { alarm_mute_rule_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AlarmMuteRuleName"))
  }
