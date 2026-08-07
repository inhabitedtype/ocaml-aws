type t = FleetCapacityReservation.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map FleetCapacityReservation.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list FleetCapacityReservation.to_query v
let to_json v = `List (List.map FleetCapacityReservation.to_json v)
let of_json j = Aws.Json.to_list FleetCapacityReservation.of_json j
