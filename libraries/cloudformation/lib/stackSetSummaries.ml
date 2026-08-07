type t = StackSetSummary.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map StackSetSummary.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list StackSetSummary.to_query v
let to_json v = `List (List.map StackSetSummary.to_json v)
let of_json j = Aws.Json.to_list StackSetSummary.of_json j
