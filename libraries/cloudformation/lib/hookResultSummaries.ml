type t = HookResultSummary.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map HookResultSummary.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list HookResultSummary.to_query v
let to_json v = `List (List.map HookResultSummary.to_json v)
let of_json j = Aws.Json.to_list HookResultSummary.of_json j
