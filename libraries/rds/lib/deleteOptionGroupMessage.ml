open Aws.BaseTypes

type t = { option_group_name : String.t }

let make ~option_group_name () = { option_group_name }

let parse xml =
  Some
    { option_group_name =
        Aws.Xml.required
          "OptionGroupName"
          (Aws.Util.option_bind (Aws.Xml.member "OptionGroupName" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("OptionGroupName", String.to_query v.option_group_name)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("OptionGroupName", String.to_json v.option_group_name) ])

let of_json j =
  { option_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "OptionGroupName"))
  }
