type t = CidrBlockSummary.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map CidrBlockSummary.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list CidrBlockSummary.to_query v
let to_json v = `List (List.map CidrBlockSummary.to_json v)
let of_json j = Aws.Json.to_list CidrBlockSummary.of_json j
