type t = MovingAddressStatus.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map MovingAddressStatus.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list MovingAddressStatus.to_query v
let to_json v = `List (List.map MovingAddressStatus.to_json v)
let of_json j = Aws.Json.to_list MovingAddressStatus.of_json j
