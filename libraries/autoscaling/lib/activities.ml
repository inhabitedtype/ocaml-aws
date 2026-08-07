type t = Activity.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map Activity.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list Activity.to_query v
let to_json v = `List (List.map Activity.to_json v)
let of_json j = Aws.Json.to_list Activity.of_json j
