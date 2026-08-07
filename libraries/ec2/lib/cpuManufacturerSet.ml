type t = CpuManufacturer.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map CpuManufacturer.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list CpuManufacturer.to_query v
let to_json v = `List (List.map CpuManufacturer.to_json v)
let of_json j = Aws.Json.to_list CpuManufacturer.of_json j
