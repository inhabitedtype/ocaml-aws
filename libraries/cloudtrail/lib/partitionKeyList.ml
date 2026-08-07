type t = PartitionKey.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map PartitionKey.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list PartitionKey.to_query v
let to_json v = `List (List.map PartitionKey.to_json v)
let of_json j = Aws.Json.to_list PartitionKey.of_json j
