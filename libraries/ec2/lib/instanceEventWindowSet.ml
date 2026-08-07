type t = InstanceEventWindow.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map InstanceEventWindow.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list InstanceEventWindow.to_query v
let to_json v = `List (List.map InstanceEventWindow.to_json v)
let of_json j = Aws.Json.to_list InstanceEventWindow.of_json j
