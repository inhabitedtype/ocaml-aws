type t = ValidationError.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ValidationError.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ValidationError.to_query v
let to_json v = `List (List.map ValidationError.to_json v)
let of_json j = Aws.Json.to_list ValidationError.of_json j
