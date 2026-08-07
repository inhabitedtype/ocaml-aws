type t = S3ObjectTag.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map S3ObjectTag.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list S3ObjectTag.to_query v
let to_json v = `List (List.map S3ObjectTag.to_json v)
let of_json j = Aws.Json.to_list S3ObjectTag.of_json j
