type t = ListenerDescription.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ListenerDescription.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list ListenerDescription.to_query v
let to_json v = `List (List.map ListenerDescription.to_json v)
let of_json j = Aws.Json.to_list ListenerDescription.of_json j
