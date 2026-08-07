type t = CapacityReservationBillingRequest.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map CapacityReservationBillingRequest.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list CapacityReservationBillingRequest.to_query v
let to_json v = `List (List.map CapacityReservationBillingRequest.to_json v)
let of_json j = Aws.Json.to_list CapacityReservationBillingRequest.of_json j
