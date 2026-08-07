type t = HostReservation.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map HostReservation.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list HostReservation.to_query v
let to_json v = `List (List.map HostReservation.to_json v)
let of_json j = Aws.Json.to_list HostReservation.of_json j
