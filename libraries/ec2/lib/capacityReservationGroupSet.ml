type t = CapacityReservationGroup.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map CapacityReservationGroup.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list CapacityReservationGroup.to_query v
let to_json v = `List (List.map CapacityReservationGroup.to_json v)
let of_json j = Aws.Json.to_list CapacityReservationGroup.of_json j
