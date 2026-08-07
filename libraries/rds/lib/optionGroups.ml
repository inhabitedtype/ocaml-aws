open Aws.BaseTypes

type t =
  { option_groups_list : OptionGroupsList.t
  ; marker : String.t option
  }

let make ?(option_groups_list = []) ?marker () = { option_groups_list; marker }

let parse xml =
  Some
    { option_groups_list =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "OptionGroupsList" xml)
             OptionGroupsList.parse)
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("OptionGroupsList.member", OptionGroupsList.to_query v.option_groups_list))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Some ("OptionGroupsList", OptionGroupsList.to_json v.option_groups_list)
       ])

let of_json j =
  { option_groups_list =
      OptionGroupsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "OptionGroupsList"))
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  }
