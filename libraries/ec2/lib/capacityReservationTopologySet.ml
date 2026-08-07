type t = CapacityReservationTopology.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map CapacityReservationTopology.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list CapacityReservationTopology.to_query v
let to_json v = `List (List.map CapacityReservationTopology.to_json v)
let of_json j = Aws.Json.to_list CapacityReservationTopology.of_json j
