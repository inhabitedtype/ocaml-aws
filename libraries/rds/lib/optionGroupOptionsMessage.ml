open Aws.BaseTypes

type t =
  { option_group_options : OptionGroupOptionsList.t
  ; marker : String.t option
  }

let make ?(option_group_options = []) ?marker () = { option_group_options; marker }

let parse xml =
  Some
    { option_group_options =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "OptionGroupOptions" xml)
             OptionGroupOptionsList.parse)
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "OptionGroupOptions.member"
              , OptionGroupOptionsList.to_query v.option_group_options ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Some ("OptionGroupOptions", OptionGroupOptionsList.to_json v.option_group_options)
       ])

let of_json j =
  { option_group_options =
      OptionGroupOptionsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "OptionGroupOptions"))
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  }
