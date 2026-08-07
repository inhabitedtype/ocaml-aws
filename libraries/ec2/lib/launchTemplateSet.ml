type t = LaunchTemplate.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map LaunchTemplate.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list LaunchTemplate.to_query v
let to_json v = `List (List.map LaunchTemplate.to_json v)
let of_json j = Aws.Json.to_list LaunchTemplate.of_json j
