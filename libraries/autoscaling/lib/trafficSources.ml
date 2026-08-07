type t = TrafficSourceIdentifier.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map TrafficSourceIdentifier.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list TrafficSourceIdentifier.to_query v
let to_json v = `List (List.map TrafficSourceIdentifier.to_json v)
let of_json j = Aws.Json.to_list TrafficSourceIdentifier.of_json j
