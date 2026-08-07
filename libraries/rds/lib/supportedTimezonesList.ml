type t = Timezone.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map Timezone.parse (Aws.Xml.members "Timezone" xml))

let to_query v = Aws.Query.to_query_list Timezone.to_query v
let to_json v = `List (List.map Timezone.to_json v)
let of_json j = Aws.Json.to_list Timezone.of_json j
