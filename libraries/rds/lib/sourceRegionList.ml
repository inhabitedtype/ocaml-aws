type t = SourceRegion.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map SourceRegion.parse (Aws.Xml.members "SourceRegion" xml))

let to_query v = Aws.Query.to_query_list SourceRegion.to_query v
let to_json v = `List (List.map SourceRegion.to_json v)
let of_json j = Aws.Json.to_list SourceRegion.of_json j
