type t = AppCookieStickinessPolicy.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map AppCookieStickinessPolicy.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list AppCookieStickinessPolicy.to_query v
let to_json v = `List (List.map AppCookieStickinessPolicy.to_json v)
let of_json j = Aws.Json.to_list AppCookieStickinessPolicy.of_json j
