type t = LocationSummary.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map LocationSummary.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list LocationSummary.to_query v
let to_json v = `List (List.map LocationSummary.to_json v)
let of_json j = Aws.Json.to_list LocationSummary.of_json j
