type t = VgwTelemetry.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map VgwTelemetry.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list VgwTelemetry.to_query v
let to_json v = `List (List.map VgwTelemetry.to_json v)
let of_json j = Aws.Json.to_list VgwTelemetry.of_json j
