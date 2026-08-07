type t = SuspendedProcess.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map SuspendedProcess.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list SuspendedProcess.to_query v
let to_json v = `List (List.map SuspendedProcess.to_json v)
let of_json j = Aws.Json.to_list SuspendedProcess.of_json j
