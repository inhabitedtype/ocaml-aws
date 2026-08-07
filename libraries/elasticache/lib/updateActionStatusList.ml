type t = UpdateActionStatus.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map UpdateActionStatus.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list UpdateActionStatus.to_query v
let to_json v = `List (List.map UpdateActionStatus.to_json v)
let of_json j = Aws.Json.to_list UpdateActionStatus.of_json j
