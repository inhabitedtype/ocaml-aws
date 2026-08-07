type t = ResourceToImport.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ResourceToImport.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list ResourceToImport.to_query v
let to_json v = `List (List.map ResourceToImport.to_json v)
let of_json j = Aws.Json.to_list ResourceToImport.of_json j
