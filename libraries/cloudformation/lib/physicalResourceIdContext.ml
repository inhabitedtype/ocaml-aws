type t = PhysicalResourceIdContextKeyValuePair.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map PhysicalResourceIdContextKeyValuePair.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list PhysicalResourceIdContextKeyValuePair.to_query v
let to_json v = `List (List.map PhysicalResourceIdContextKeyValuePair.to_json v)
let of_json j = Aws.Json.to_list PhysicalResourceIdContextKeyValuePair.of_json j
