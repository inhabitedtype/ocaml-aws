type t = ManagedPrefixList.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ManagedPrefixList.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ManagedPrefixList.to_query v
let to_json v = `List (List.map ManagedPrefixList.to_json v)
let of_json j = Aws.Json.to_list ManagedPrefixList.of_json j
