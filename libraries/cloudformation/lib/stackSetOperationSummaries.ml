type t = StackSetOperationSummary.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map StackSetOperationSummary.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list StackSetOperationSummary.to_query v
let to_json v = `List (List.map StackSetOperationSummary.to_json v)
let of_json j = Aws.Json.to_list StackSetOperationSummary.of_json j
