type t = StackSetOperationResultSummary.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map StackSetOperationResultSummary.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list StackSetOperationResultSummary.to_query v
let to_json v = `List (List.map StackSetOperationResultSummary.to_json v)
let of_json j = Aws.Json.to_list StackSetOperationResultSummary.of_json j
