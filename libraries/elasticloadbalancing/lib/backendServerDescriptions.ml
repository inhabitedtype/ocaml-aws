type t = BackendServerDescription.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map BackendServerDescription.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list BackendServerDescription.to_query v
let to_json v = `List (List.map BackendServerDescription.to_json v)
let of_json j = Aws.Json.to_list BackendServerDescription.of_json j
