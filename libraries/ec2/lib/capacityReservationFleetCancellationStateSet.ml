type t = CapacityReservationFleetCancellationState.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       CapacityReservationFleetCancellationState.parse
       (Aws.Xml.members "item" xml))

let to_query v =
  Aws.Query.to_query_list CapacityReservationFleetCancellationState.to_query v

let to_json v = `List (List.map CapacityReservationFleetCancellationState.to_json v)
let of_json j = Aws.Json.to_list CapacityReservationFleetCancellationState.of_json j
