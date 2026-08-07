type t = DeleteLaunchTemplateVersionsResponseErrorItem.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       DeleteLaunchTemplateVersionsResponseErrorItem.parse
       (Aws.Xml.members "item" xml))

let to_query v =
  Aws.Query.to_query_list DeleteLaunchTemplateVersionsResponseErrorItem.to_query v

let to_json v = `List (List.map DeleteLaunchTemplateVersionsResponseErrorItem.to_json v)
let of_json j = Aws.Json.to_list DeleteLaunchTemplateVersionsResponseErrorItem.of_json j
