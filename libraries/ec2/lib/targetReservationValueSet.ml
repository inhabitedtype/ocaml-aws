type t = TargetReservationValue.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map TargetReservationValue.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list TargetReservationValue.to_query v
let to_json v = `List (List.map TargetReservationValue.to_json v)
let of_json j = Aws.Json.to_list TargetReservationValue.of_json j
