type t = KeyPairInfo.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map KeyPairInfo.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list KeyPairInfo.to_query v
let to_json v = `List (List.map KeyPairInfo.to_json v)
let of_json j = Aws.Json.to_list KeyPairInfo.of_json j
