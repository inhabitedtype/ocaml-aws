type t = CompositeAlarm.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map CompositeAlarm.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list CompositeAlarm.to_query v
let to_json v = `List (List.map CompositeAlarm.to_json v)
let of_json j = Aws.Json.to_list CompositeAlarm.of_json j
