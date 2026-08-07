type t = GlobalNodeGroup.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map GlobalNodeGroup.parse (Aws.Xml.members "GlobalNodeGroup" xml))

let to_query v = Aws.Query.to_query_list GlobalNodeGroup.to_query v
let to_json v = `List (List.map GlobalNodeGroup.to_json v)
let of_json j = Aws.Json.to_list GlobalNodeGroup.of_json j
