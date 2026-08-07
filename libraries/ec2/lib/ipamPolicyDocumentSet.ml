type t = IpamPolicyDocument.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map IpamPolicyDocument.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list IpamPolicyDocument.to_query v
let to_json v = `List (List.map IpamPolicyDocument.to_json v)
let of_json j = Aws.Json.to_list IpamPolicyDocument.of_json j
