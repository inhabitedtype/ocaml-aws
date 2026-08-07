type t = FpgaDeviceInfo.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map FpgaDeviceInfo.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list FpgaDeviceInfo.to_query v
let to_json v = `List (List.map FpgaDeviceInfo.to_json v)
let of_json j = Aws.Json.to_list FpgaDeviceInfo.of_json j
