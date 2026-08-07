type t = AvailabilityZoneSubGeography.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map AvailabilityZoneSubGeography.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list AvailabilityZoneSubGeography.to_query v
let to_json v = `List (List.map AvailabilityZoneSubGeography.to_json v)
let of_json j = Aws.Json.to_list AvailabilityZoneSubGeography.of_json j
