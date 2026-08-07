open Aws.BaseTypes

type t =
  { maximum : Integer.t option
  ; minimum : Integer.t option
  }

let make ?maximum ?minimum () = { maximum; minimum }

let parse xml =
  Some
    { maximum = Aws.Util.option_bind (Aws.Xml.member "Maximum" xml) Integer.parse
    ; minimum = Aws.Util.option_bind (Aws.Xml.member "Minimum" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.minimum (fun f ->
             Aws.Query.Pair ("Minimum", Integer.to_query f))
       ; Aws.Util.option_map v.maximum (fun f ->
             Aws.Query.Pair ("Maximum", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.minimum (fun f -> "Minimum", Integer.to_json f)
       ; Aws.Util.option_map v.maximum (fun f -> "Maximum", Integer.to_json f)
       ])

let of_json j =
  { maximum = Aws.Util.option_map (Aws.Json.lookup j "Maximum") Integer.of_json
  ; minimum = Aws.Util.option_map (Aws.Json.lookup j "Minimum") Integer.of_json
  }
