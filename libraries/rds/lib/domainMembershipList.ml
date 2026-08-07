type t = DomainMembership.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map DomainMembership.parse (Aws.Xml.members "DomainMembership" xml))

let to_query v = Aws.Query.to_query_list DomainMembership.to_query v
let to_json v = `List (List.map DomainMembership.to_json v)
let of_json j = Aws.Json.to_list DomainMembership.of_json j
