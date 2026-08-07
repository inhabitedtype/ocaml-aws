type t = AvailabilityZoneMessage.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map AvailabilityZoneMessage.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list AvailabilityZoneMessage.to_query v
let to_json v = `List (List.map AvailabilityZoneMessage.to_json v)
let of_json j = Aws.Json.to_list AvailabilityZoneMessage.of_json j
