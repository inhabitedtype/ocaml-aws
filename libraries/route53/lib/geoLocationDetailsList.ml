type t = GeoLocationDetails.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map GeoLocationDetails.parse (Aws.Xml.members "GeoLocationDetails" xml))

let to_query v = Aws.Query.to_query_list GeoLocationDetails.to_query v
let to_json v = `List (List.map GeoLocationDetails.to_json v)
let of_json j = Aws.Json.to_list GeoLocationDetails.of_json j
