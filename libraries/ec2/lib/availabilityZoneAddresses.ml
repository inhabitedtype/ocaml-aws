type t = AvailabilityZoneAddress.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       AvailabilityZoneAddress.parse
       (Aws.Xml.members "AvailabilityZoneAddress" xml))

let to_query v = Aws.Query.to_query_list AvailabilityZoneAddress.to_query v
let to_json v = `List (List.map AvailabilityZoneAddress.to_json v)
let of_json j = Aws.Json.to_list AvailabilityZoneAddress.of_json j
