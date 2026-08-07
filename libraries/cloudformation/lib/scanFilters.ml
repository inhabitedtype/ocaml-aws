type t = ScanFilter.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map ScanFilter.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list ScanFilter.to_query v
let to_json v = `List (List.map ScanFilter.to_json v)
let of_json j = Aws.Json.to_list ScanFilter.of_json j
