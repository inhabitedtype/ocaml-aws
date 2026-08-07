type t = DelegationSet.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map DelegationSet.parse (Aws.Xml.members "DelegationSet" xml))

let to_query v = Aws.Query.to_query_list DelegationSet.to_query v
let to_json v = `List (List.map DelegationSet.to_json v)
let of_json j = Aws.Json.to_list DelegationSet.of_json j
