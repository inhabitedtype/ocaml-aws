type t = LaunchTemplateTagSpecificationRequest.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       LaunchTemplateTagSpecificationRequest.parse
       (Aws.Xml.members "LaunchTemplateTagSpecificationRequest" xml))

let to_query v = Aws.Query.to_query_list LaunchTemplateTagSpecificationRequest.to_query v
let to_json v = `List (List.map LaunchTemplateTagSpecificationRequest.to_json v)
let of_json j = Aws.Json.to_list LaunchTemplateTagSpecificationRequest.of_json j
