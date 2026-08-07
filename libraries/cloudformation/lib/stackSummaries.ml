type t = StackSummary.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map StackSummary.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list StackSummary.to_query v
let to_json v = `List (List.map StackSummary.to_json v)
let of_json j = Aws.Json.to_list StackSummary.of_json j
