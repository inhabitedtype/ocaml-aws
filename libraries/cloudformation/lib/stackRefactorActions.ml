type t = StackRefactorAction.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map StackRefactorAction.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list StackRefactorAction.to_query v
let to_json v = `List (List.map StackRefactorAction.to_json v)
let of_json j = Aws.Json.to_list StackRefactorAction.of_json j
