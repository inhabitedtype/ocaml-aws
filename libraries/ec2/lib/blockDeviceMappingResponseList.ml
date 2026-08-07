type t = BlockDeviceMappingResponse.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map BlockDeviceMappingResponse.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list BlockDeviceMappingResponse.to_query v
let to_json v = `List (List.map BlockDeviceMappingResponse.to_json v)
let of_json j = Aws.Json.to_list BlockDeviceMappingResponse.of_json j
