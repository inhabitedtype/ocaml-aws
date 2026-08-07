type t = AddressAttribute.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map AddressAttribute.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list AddressAttribute.to_query v
let to_json v = `List (List.map AddressAttribute.to_json v)
let of_json j = Aws.Json.to_list AddressAttribute.of_json j
