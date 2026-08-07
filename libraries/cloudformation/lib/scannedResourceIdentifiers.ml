type t = ScannedResourceIdentifier.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map ScannedResourceIdentifier.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list ScannedResourceIdentifier.to_query v
let to_json v = `List (List.map ScannedResourceIdentifier.to_json v)
let of_json j = Aws.Json.to_list ScannedResourceIdentifier.of_json j
