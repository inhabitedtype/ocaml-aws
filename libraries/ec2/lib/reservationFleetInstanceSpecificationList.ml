type t = ReservationFleetInstanceSpecification.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ReservationFleetInstanceSpecification.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list ReservationFleetInstanceSpecification.to_query v
let to_json v = `List (List.map ReservationFleetInstanceSpecification.to_json v)
let of_json j = Aws.Json.to_list ReservationFleetInstanceSpecification.of_json j
