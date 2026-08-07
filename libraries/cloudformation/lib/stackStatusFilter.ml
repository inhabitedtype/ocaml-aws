type t = StackStatus.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map StackStatus.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list StackStatus.to_query v
let to_json v = `List (List.map StackStatus.to_json v)
let of_json j = Aws.Json.to_list StackStatus.of_json j
