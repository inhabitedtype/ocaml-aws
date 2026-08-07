type t = TemplateParameter.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map TemplateParameter.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list TemplateParameter.to_query v
let to_json v = `List (List.map TemplateParameter.to_json v)
let of_json j = Aws.Json.to_list TemplateParameter.of_json j
