type t = StackInstanceSummary.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map StackInstanceSummary.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list StackInstanceSummary.to_query v
let to_json v = `List (List.map StackInstanceSummary.to_json v)
let of_json j = Aws.Json.to_list StackInstanceSummary.of_json j
