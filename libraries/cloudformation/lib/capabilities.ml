type t = Capability.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map Capability.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list Capability.to_query v
let to_json v = `List (List.map Capability.to_json v)
let of_json j = Aws.Json.to_list Capability.of_json j
