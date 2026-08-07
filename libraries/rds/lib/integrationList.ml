type t = Integration.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map Integration.parse (Aws.Xml.members "Integration" xml))

let to_query v = Aws.Query.to_query_list Integration.to_query v
let to_json v = `List (List.map Integration.to_json v)
let of_json j = Aws.Json.to_list Integration.of_json j
