type t = StackResource.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map StackResource.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list StackResource.to_query v
let to_json v = `List (List.map StackResource.to_json v)
let of_json j = Aws.Json.to_list StackResource.of_json j
