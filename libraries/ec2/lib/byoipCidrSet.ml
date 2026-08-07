type t = ByoipCidr.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ByoipCidr.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ByoipCidr.to_query v
let to_json v = `List (List.map ByoipCidr.to_json v)
let of_json j = Aws.Json.to_list ByoipCidr.of_json j
