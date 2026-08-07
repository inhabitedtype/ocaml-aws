type t = ScannedResource.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ScannedResource.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list ScannedResource.to_query v
let to_json v = `List (List.map ScannedResource.to_json v)
let of_json j = Aws.Json.to_list ScannedResource.of_json j
