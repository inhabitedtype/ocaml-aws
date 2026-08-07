type t = AttributeSummary.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map AttributeSummary.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list AttributeSummary.to_query v
let to_json v = `List (List.map AttributeSummary.to_json v)
let of_json j = Aws.Json.to_list AttributeSummary.of_json j
