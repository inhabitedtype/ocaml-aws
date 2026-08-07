type t = LicenseConfiguration.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map LicenseConfiguration.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list LicenseConfiguration.to_query v
let to_json v = `List (List.map LicenseConfiguration.to_json v)
let of_json j = Aws.Json.to_list LicenseConfiguration.of_json j
