type t = SecurityGroupVpcAssociation.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map SecurityGroupVpcAssociation.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list SecurityGroupVpcAssociation.to_query v
let to_json v = `List (List.map SecurityGroupVpcAssociation.to_json v)
let of_json j = Aws.Json.to_list SecurityGroupVpcAssociation.of_json j
