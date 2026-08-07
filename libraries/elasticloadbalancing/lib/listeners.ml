type t = Listener.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map Listener.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list Listener.to_query v
let to_json v = `List (List.map Listener.to_json v)
let of_json j = Aws.Json.to_list Listener.of_json j
