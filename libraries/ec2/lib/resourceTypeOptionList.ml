type t = ResourceTypeOption.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ResourceTypeOption.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list ResourceTypeOption.to_query v
let to_json v = `List (List.map ResourceTypeOption.to_json v)
let of_json j = Aws.Json.to_list ResourceTypeOption.of_json j
