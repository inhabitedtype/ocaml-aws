type t = IpamExternalResourceVerificationToken.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map IpamExternalResourceVerificationToken.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list IpamExternalResourceVerificationToken.to_query v
let to_json v = `List (List.map IpamExternalResourceVerificationToken.to_json v)
let of_json j = Aws.Json.to_list IpamExternalResourceVerificationToken.of_json j
