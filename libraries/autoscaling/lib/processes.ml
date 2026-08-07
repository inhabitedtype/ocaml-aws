type t = ProcessType.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ProcessType.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list ProcessType.to_query v
let to_json v = `List (List.map ProcessType.to_json v)
let of_json j = Aws.Json.to_list ProcessType.of_json j
