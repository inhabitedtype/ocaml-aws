type t = LaunchTemplateTagSpecification.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map LaunchTemplateTagSpecification.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list LaunchTemplateTagSpecification.to_query v
let to_json v = `List (List.map LaunchTemplateTagSpecification.to_json v)
let of_json j = Aws.Json.to_list LaunchTemplateTagSpecification.of_json j
