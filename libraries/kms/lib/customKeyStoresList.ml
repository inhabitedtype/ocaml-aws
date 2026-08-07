type t = CustomKeyStoresListEntry.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map CustomKeyStoresListEntry.parse (Aws.Xml.members "member" xml))

let to_query v = Aws.Query.to_query_list CustomKeyStoresListEntry.to_query v
let to_json v = `List (List.map CustomKeyStoresListEntry.to_json v)
let of_json j = Aws.Json.to_list CustomKeyStoresListEntry.of_json j
