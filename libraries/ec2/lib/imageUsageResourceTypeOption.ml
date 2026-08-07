open Aws.BaseTypes

type t =
  { option_name : String.t option
  ; option_values : ImageUsageResourceTypeOptionValuesList.t
  }

let make ?option_name ?(option_values = []) () = { option_name; option_values }

let parse xml =
  Some
    { option_name = Aws.Util.option_bind (Aws.Xml.member "optionName" xml) String.parse
    ; option_values =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "optionValueSet" xml)
             ImageUsageResourceTypeOptionValuesList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "OptionValueSet"
              , ImageUsageResourceTypeOptionValuesList.to_query v.option_values ))
       ; Aws.Util.option_map v.option_name (fun f ->
             Aws.Query.Pair ("OptionName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "optionValueSet"
           , ImageUsageResourceTypeOptionValuesList.to_json v.option_values )
       ; Aws.Util.option_map v.option_name (fun f -> "optionName", String.to_json f)
       ])

let of_json j =
  { option_name = Aws.Util.option_map (Aws.Json.lookup j "optionName") String.of_json
  ; option_values =
      ImageUsageResourceTypeOptionValuesList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "optionValueSet"))
  }
