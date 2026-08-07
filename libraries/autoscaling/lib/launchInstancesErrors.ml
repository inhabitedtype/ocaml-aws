type t = LaunchInstancesError.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map LaunchInstancesError.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list LaunchInstancesError.to_query v
let to_json v = `List (List.map LaunchInstancesError.to_json v)
let of_json j = Aws.Json.to_list LaunchInstancesError.of_json j
