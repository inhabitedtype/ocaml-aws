type t = OperationEvent.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map OperationEvent.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list OperationEvent.to_query v
let to_json v = `List (List.map OperationEvent.to_json v)
let of_json j = Aws.Json.to_list OperationEvent.of_json j
