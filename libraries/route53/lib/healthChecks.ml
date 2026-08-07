type t = HealthCheck.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map HealthCheck.parse (Aws.Xml.members "HealthCheck" xml))

let to_query v = Aws.Query.to_query_list HealthCheck.to_query v
let to_json v = `List (List.map HealthCheck.to_json v)
let of_json j = Aws.Json.to_list HealthCheck.of_json j
