type t = FlowLog.t list

let make elems () = elems
let parse xml = Aws.Util.option_all (List.map FlowLog.parse (Aws.Xml.members "item" xml))
let to_query v = Aws.Query.to_query_list FlowLog.to_query v
let to_json v = `List (List.map FlowLog.to_json v)
let of_json j = Aws.Json.to_list FlowLog.of_json j
