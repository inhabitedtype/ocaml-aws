type t = Destination.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map Destination.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list Destination.to_query v
let to_json v = `List (List.map Destination.to_json v)
let of_json j = Aws.Json.to_list Destination.of_json j
