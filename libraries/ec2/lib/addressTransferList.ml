type t = AddressTransfer.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map AddressTransfer.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list AddressTransfer.to_query v
let to_json v = `List (List.map AddressTransfer.to_json v)
let of_json j = Aws.Json.to_list AddressTransfer.of_json j
