type t = TemplateSummary.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map TemplateSummary.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list TemplateSummary.to_query v
let to_json v = `List (List.map TemplateSummary.to_json v)
let of_json j = Aws.Json.to_list TemplateSummary.of_json j
