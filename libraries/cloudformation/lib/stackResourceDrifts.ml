type t = StackResourceDrift.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map StackResourceDrift.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list StackResourceDrift.to_query v
let to_json v = `List (List.map StackResourceDrift.to_json v)
let of_json j = Aws.Json.to_list StackResourceDrift.of_json j
