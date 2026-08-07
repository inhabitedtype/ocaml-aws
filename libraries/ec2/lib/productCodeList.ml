type t = ProductCode.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ProductCode.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list ProductCode.to_query v
let to_json v = `List (List.map ProductCode.to_json v)
let of_json j = Aws.Json.to_list ProductCode.of_json j
