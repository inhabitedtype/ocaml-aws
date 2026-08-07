type t = NodeGroup.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map NodeGroup.parse (Aws.Xml.members "NodeGroup" xml))

let to_query v = Aws.Query.to_query_list NodeGroup.to_query v
let to_json v = `List (List.map NodeGroup.to_json v)
let of_json j = Aws.Json.to_list NodeGroup.of_json j
