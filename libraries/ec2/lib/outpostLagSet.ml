type t = OutpostLag.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map OutpostLag.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list OutpostLag.to_query v
let to_json v = `List (List.map OutpostLag.to_json v)
let of_json j = Aws.Json.to_list OutpostLag.of_json j
