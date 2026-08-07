type t = AlarmMuteRuleSummary.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map AlarmMuteRuleSummary.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list AlarmMuteRuleSummary.to_query v
let to_json v = `List (List.map AlarmMuteRuleSummary.to_json v)
let of_json j = Aws.Json.to_list AlarmMuteRuleSummary.of_json j
