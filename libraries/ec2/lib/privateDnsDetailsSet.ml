type t = PrivateDnsDetails.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map PrivateDnsDetails.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list PrivateDnsDetails.to_query v
let to_json v = `List (List.map PrivateDnsDetails.to_json v)
let of_json j = Aws.Json.to_list PrivateDnsDetails.of_json j
