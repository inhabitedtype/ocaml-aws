type t = ReportInstanceReasonCodes.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ReportInstanceReasonCodes.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ReportInstanceReasonCodes.to_query v
let to_json v = `List (List.map ReportInstanceReasonCodes.to_json v)
let of_json j = Aws.Json.to_list ReportInstanceReasonCodes.of_json j
