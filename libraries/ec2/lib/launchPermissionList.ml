type t = LaunchPermission.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map LaunchPermission.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list LaunchPermission.to_query v
let to_json v = `List (List.map LaunchPermission.to_json v)
let of_json j = Aws.Json.to_list LaunchPermission.of_json j
