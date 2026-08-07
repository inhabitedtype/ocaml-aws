type t = CidrCollectionChange.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map CidrCollectionChange.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list CidrCollectionChange.to_query v
let to_json v = `List (List.map CidrCollectionChange.to_json v)
let of_json j = Aws.Json.to_list CidrCollectionChange.of_json j
