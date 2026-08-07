type t = OperationResultFilter.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map OperationResultFilter.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list OperationResultFilter.to_query v
let to_json v = `List (List.map OperationResultFilter.to_json v)
let of_json j = Aws.Json.to_list OperationResultFilter.of_json j
