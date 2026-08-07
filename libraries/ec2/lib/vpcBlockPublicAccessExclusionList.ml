type t = VpcBlockPublicAccessExclusion.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map VpcBlockPublicAccessExclusion.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list VpcBlockPublicAccessExclusion.to_query v
let to_json v = `List (List.map VpcBlockPublicAccessExclusion.to_json v)
let of_json j = Aws.Json.to_list VpcBlockPublicAccessExclusion.of_json j
