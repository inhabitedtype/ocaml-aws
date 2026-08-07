type t = UpdateAction.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map UpdateAction.parse (Aws.Xml.members "UpdateAction" xml))

let to_query v = Aws.Query.to_query_list UpdateAction.to_query v
let to_json v = `List (List.map UpdateAction.to_json v)
let of_json j = Aws.Json.to_list UpdateAction.of_json j
