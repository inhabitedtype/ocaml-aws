type t = Reservation.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map Reservation.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list Reservation.to_query v
let to_json v = `List (List.map Reservation.to_json v)
let of_json j = Aws.Json.to_list Reservation.of_json j
