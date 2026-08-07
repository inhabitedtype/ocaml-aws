type t = InstanceBlockDeviceMappingSpecification.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map InstanceBlockDeviceMappingSpecification.parse (Aws.Xml.members "item" xml))

let to_query v =
  Aws.Query.to_query_list InstanceBlockDeviceMappingSpecification.to_query v

let to_json v = `List (List.map InstanceBlockDeviceMappingSpecification.to_json v)
let of_json j = Aws.Json.to_list InstanceBlockDeviceMappingSpecification.of_json j
