type t = OptionGroupOptionSetting.t list

let make elems () = elems

let parse xml =
  Aws.Util.option_all
    (List.map
       OptionGroupOptionSetting.parse
       (Aws.Xml.members "OptionGroupOptionSetting" xml))

let to_query v = Aws.Query.to_query_list OptionGroupOptionSetting.to_query v
let to_json v = `List (List.map OptionGroupOptionSetting.to_json v)
let of_json j = Aws.Json.to_list OptionGroupOptionSetting.of_json j
