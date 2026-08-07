type t = CapacityReservationFleet.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map CapacityReservationFleet.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list CapacityReservationFleet.to_query v
let to_json v = `List (List.map CapacityReservationFleet.to_json v)
let of_json j = Aws.Json.to_list CapacityReservationFleet.of_json j
