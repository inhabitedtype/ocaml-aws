type t = ValidStorageOptions.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ValidStorageOptions.parse (Aws.Xml.members "ValidStorageOptions" xml))

let to_query v = Aws.Query.to_query_list ValidStorageOptions.to_query v
let to_json v = `List (List.map ValidStorageOptions.to_json v)
let of_json j = Aws.Json.to_list ValidStorageOptions.of_json j
