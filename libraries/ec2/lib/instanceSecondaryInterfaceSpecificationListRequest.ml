type t = InstanceSecondaryInterfaceSpecificationRequest.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       InstanceSecondaryInterfaceSpecificationRequest.parse
       (Aws.Xml.members "item" xml))

let to_query v =
  Aws.Query.to_query_list InstanceSecondaryInterfaceSpecificationRequest.to_query v

let to_json v = `List (List.map InstanceSecondaryInterfaceSpecificationRequest.to_json v)
let of_json j = Aws.Json.to_list InstanceSecondaryInterfaceSpecificationRequest.of_json j
