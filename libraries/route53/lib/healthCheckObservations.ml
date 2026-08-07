type t = HealthCheckObservation.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map HealthCheckObservation.parse (Aws.Xml.members "HealthCheckObservation" xml))

let to_query v = Aws.Query.to_query_list HealthCheckObservation.to_query v
let to_json v = `List (List.map HealthCheckObservation.to_json v)
let of_json j = Aws.Json.to_list HealthCheckObservation.of_json j
