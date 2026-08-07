type t = ResourceRecord.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ResourceRecord.parse (Aws.Xml.members "ResourceRecord" xml))

let to_query v = Aws.Query.to_query_list ResourceRecord.to_query v
let to_json v = `List (List.map ResourceRecord.to_json v)
let of_json j = Aws.Json.to_list ResourceRecord.of_json j
