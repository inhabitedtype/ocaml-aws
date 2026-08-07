type t = BandwidthWeightingType.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map BandwidthWeightingType.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list BandwidthWeightingType.to_query v
let to_json v = `List (List.map BandwidthWeightingType.to_json v)
let of_json j = Aws.Json.to_list BandwidthWeightingType.of_json j
