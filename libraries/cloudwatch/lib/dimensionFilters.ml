type t = DimensionFilter.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map DimensionFilter.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list DimensionFilter.to_query v
let to_json v = `List (List.map DimensionFilter.to_json v)
let of_json j = Aws.Json.to_list DimensionFilter.of_json j
