type t = ActiveInstance.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ActiveInstance.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ActiveInstance.to_query v
let to_json v = `List (List.map ActiveInstance.to_json v)
let of_json j = Aws.Json.to_list ActiveInstance.of_json j
