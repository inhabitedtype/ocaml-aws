type t = InstanceRefresh.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map InstanceRefresh.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list InstanceRefresh.to_query v
let to_json v = `List (List.map InstanceRefresh.to_json v)
let of_json j = Aws.Json.to_list InstanceRefresh.of_json j
