type t = HealthCheckRegion.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map HealthCheckRegion.parse (Aws.Xml.members "Region" xml))

let to_query v = Aws.Query.to_query_list HealthCheckRegion.to_query v
let to_json v = `List (List.map HealthCheckRegion.to_json v)
let of_json j = Aws.Json.to_list HealthCheckRegion.of_json j
