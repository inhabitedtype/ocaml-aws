type t = ResourceTypeRequest.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ResourceTypeRequest.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list ResourceTypeRequest.to_query v
let to_json v = `List (List.map ResourceTypeRequest.to_json v)
let of_json j = Aws.Json.to_list ResourceTypeRequest.of_json j
