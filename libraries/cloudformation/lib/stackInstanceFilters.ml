type t = StackInstanceFilter.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map StackInstanceFilter.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list StackInstanceFilter.to_query v
let to_json v = `List (List.map StackInstanceFilter.to_json v)
let of_json j = Aws.Json.to_list StackInstanceFilter.of_json j
