type t = StackRefactorExecutionStatus.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map StackRefactorExecutionStatus.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list StackRefactorExecutionStatus.to_query v
let to_json v = `List (List.map StackRefactorExecutionStatus.to_json v)
let of_json j = Aws.Json.to_list StackRefactorExecutionStatus.of_json j
