type t = CapacityBlockExtensionOffering.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map CapacityBlockExtensionOffering.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list CapacityBlockExtensionOffering.to_query v
let to_json v = `List (List.map CapacityBlockExtensionOffering.to_json v)
let of_json j = Aws.Json.to_list CapacityBlockExtensionOffering.of_json j
