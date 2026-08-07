type t = SubnetCidrReservation.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map SubnetCidrReservation.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list SubnetCidrReservation.to_query v
let to_json v = `List (List.map SubnetCidrReservation.to_json v)
let of_json j = Aws.Json.to_list SubnetCidrReservation.of_json j
