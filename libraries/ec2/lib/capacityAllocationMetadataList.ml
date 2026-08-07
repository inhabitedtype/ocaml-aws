type t = CapacityAllocationMetadataEntry.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map CapacityAllocationMetadataEntry.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list CapacityAllocationMetadataEntry.to_query v
let to_json v = `List (List.map CapacityAllocationMetadataEntry.to_json v)
let of_json j = Aws.Json.to_list CapacityAllocationMetadataEntry.of_json j
