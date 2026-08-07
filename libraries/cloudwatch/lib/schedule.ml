open Aws.BaseTypes

type t =
  { expression : String.t
  ; duration : String.t
  ; timezone : String.t option
  }

let make ~expression ~duration ?timezone () = { expression; duration; timezone }

let parse xml =
  Some
    { expression =
        Aws.Xml.required
          "Expression"
          (Aws.Util.option_bind (Aws.Xml.member "Expression" xml) String.parse)
    ; duration =
        Aws.Xml.required
          "Duration"
          (Aws.Util.option_bind (Aws.Xml.member "Duration" xml) String.parse)
    ; timezone = Aws.Util.option_bind (Aws.Xml.member "Timezone" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.timezone (fun f ->
             Aws.Query.Pair ("Timezone", String.to_query f))
       ; Some (Aws.Query.Pair ("Duration", String.to_query v.duration))
       ; Some (Aws.Query.Pair ("Expression", String.to_query v.expression))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.timezone (fun f -> "Timezone", String.to_json f)
       ; Some ("Duration", String.to_json v.duration)
       ; Some ("Expression", String.to_json v.expression)
       ])

let of_json j =
  { expression = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Expression"))
  ; duration = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Duration"))
  ; timezone = Aws.Util.option_map (Aws.Json.lookup j "Timezone") String.of_json
  }
