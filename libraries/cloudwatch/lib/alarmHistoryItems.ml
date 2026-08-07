type t = AlarmHistoryItem.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map AlarmHistoryItem.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list AlarmHistoryItem.to_query v
let to_json v = `List (List.map AlarmHistoryItem.to_json v)
let of_json j = Aws.Json.to_list AlarmHistoryItem.of_json j
