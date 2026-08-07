type t = BlueGreenDeployment.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map BlueGreenDeployment.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list BlueGreenDeployment.to_query v
let to_json v = `List (List.map BlueGreenDeployment.to_json v)
let of_json j = Aws.Json.to_list BlueGreenDeployment.of_json j
