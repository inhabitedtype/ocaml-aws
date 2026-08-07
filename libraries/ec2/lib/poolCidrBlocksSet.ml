type t = PoolCidrBlock.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map PoolCidrBlock.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list PoolCidrBlock.to_query v
let to_json v = `List (List.map PoolCidrBlock.to_json v)
let of_json j = Aws.Json.to_list PoolCidrBlock.of_json j
