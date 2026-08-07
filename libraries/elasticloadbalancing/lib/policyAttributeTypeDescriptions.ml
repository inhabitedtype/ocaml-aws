type t = PolicyAttributeTypeDescription.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map PolicyAttributeTypeDescription.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list PolicyAttributeTypeDescription.to_query v
let to_json v = `List (List.map PolicyAttributeTypeDescription.to_json v)
let of_json j = Aws.Json.to_list PolicyAttributeTypeDescription.of_json j
