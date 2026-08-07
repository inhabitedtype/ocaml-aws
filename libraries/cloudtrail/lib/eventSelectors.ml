type t = EventSelector.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map EventSelector.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list EventSelector.to_query v
let to_json v = `List (List.map EventSelector.to_json v)
let of_json j = Aws.Json.to_list EventSelector.of_json j
