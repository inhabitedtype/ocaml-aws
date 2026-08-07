type t = PolicyDescription.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map PolicyDescription.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list PolicyDescription.to_query v
let to_json v = `List (List.map PolicyDescription.to_json v)
let of_json j = Aws.Json.to_list PolicyDescription.of_json j
