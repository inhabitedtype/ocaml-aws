type t = ImageUsageReport.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ImageUsageReport.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ImageUsageReport.to_query v
let to_json v = `List (List.map ImageUsageReport.to_json v)
let of_json j = Aws.Json.to_list ImageUsageReport.of_json j
