type t = AvailabilityZone.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map AvailabilityZone.parse (Aws.Xml.members "AvailabilityZone" xml))

let to_query v = Aws.Query.to_query_list AvailabilityZone.to_query v
let to_json v = `List (List.map AvailabilityZone.to_json v)
let of_json j = Aws.Json.to_list AvailabilityZone.of_json j
