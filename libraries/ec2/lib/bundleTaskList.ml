type t = BundleTask.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map BundleTask.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list BundleTask.to_query v
let to_json v = `List (List.map BundleTask.to_json v)
let of_json j = Aws.Json.to_list BundleTask.of_json j
