type t = ImageUsageReportEntry.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ImageUsageReportEntry.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ImageUsageReportEntry.to_query v
let to_json v = `List (List.map ImageUsageReportEntry.to_json v)
let of_json j = Aws.Json.to_list ImageUsageReportEntry.of_json j
