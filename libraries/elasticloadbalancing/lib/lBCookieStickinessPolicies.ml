type t = LBCookieStickinessPolicy.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map LBCookieStickinessPolicy.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list LBCookieStickinessPolicy.to_query v
let to_json v = `List (List.map LBCookieStickinessPolicy.to_json v)
let of_json j = Aws.Json.to_list LBCookieStickinessPolicy.of_json j
