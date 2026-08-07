type t = FpgaImage.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all (List.map FpgaImage.parse (Aws.Xml.members "item" xml))

let to_query v = Aws.Query.to_query_list FpgaImage.to_query v
let to_json v = `List (List.map FpgaImage.to_json v)
let of_json j = Aws.Json.to_list FpgaImage.of_json j
