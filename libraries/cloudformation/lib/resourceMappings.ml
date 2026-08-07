type t = ResourceMapping.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ResourceMapping.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list ResourceMapping.to_query v
let to_json v = `List (List.map ResourceMapping.to_json v)
let of_json j = Aws.Json.to_list ResourceMapping.of_json j
