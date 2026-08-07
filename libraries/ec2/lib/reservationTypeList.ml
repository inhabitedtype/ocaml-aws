type t = FleetReservationType.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map FleetReservationType.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list FleetReservationType.to_query v
let to_json v = `List (List.map FleetReservationType.to_json v)
let of_json j = Aws.Json.to_list FleetReservationType.of_json j
