type t = ResourceDefinition.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ResourceDefinition.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list ResourceDefinition.to_query v
let to_json v = `List (List.map ResourceDefinition.to_json v)
let of_json j = Aws.Json.to_list ResourceDefinition.of_json j
