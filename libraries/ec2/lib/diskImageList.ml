type t = DiskImage.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map DiskImage.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list DiskImage.to_query v
let to_json v = `List (List.map DiskImage.to_json v)
let of_json j = Aws.Json.to_list DiskImage.of_json j
