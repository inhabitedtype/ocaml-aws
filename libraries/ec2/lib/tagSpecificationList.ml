type t = TagSpecification.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map TagSpecification.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list TagSpecification.to_query v
let to_json v = `List (List.map TagSpecification.to_json v)
let of_json j = Aws.Json.to_list TagSpecification.of_json j
