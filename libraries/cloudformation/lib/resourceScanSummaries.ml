type t = ResourceScanSummary.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ResourceScanSummary.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list ResourceScanSummary.to_query v
let to_json v = `List (List.map ResourceScanSummary.to_json v)
let of_json j = Aws.Json.to_list ResourceScanSummary.of_json j
