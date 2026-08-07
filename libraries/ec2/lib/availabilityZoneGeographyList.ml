type t = AvailabilityZoneGeography.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map AvailabilityZoneGeography.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list AvailabilityZoneGeography.to_query v
let to_json v = `List (List.map AvailabilityZoneGeography.to_json v)
let of_json j = Aws.Json.to_list AvailabilityZoneGeography.of_json j
