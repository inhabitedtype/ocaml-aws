type t = Alarm.t list

let make elems () = elems
let parse xml = Aws.Util.option_all (List.map Alarm.parse (Aws.Xml.members "member" xml))
let to_query v = Aws.Query.to_query_list Alarm.to_query v
let to_json v = `List (List.map Alarm.to_json v)
let of_json j = Aws.Json.to_list Alarm.of_json j
