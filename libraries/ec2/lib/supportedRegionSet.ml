type t = SupportedRegionDetail.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map SupportedRegionDetail.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list SupportedRegionDetail.to_query v
let to_json v = `List (List.map SupportedRegionDetail.to_json v)
let of_json j = Aws.Json.to_list SupportedRegionDetail.of_json j
