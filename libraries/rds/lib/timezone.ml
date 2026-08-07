open Aws.BaseTypes

type t = { timezone_name : String.t option }

let make ?timezone_name () = { timezone_name }

let parse xml =
  Some
    { timezone_name =
        Aws.Util.option_bind (Aws.Xml.member "TimezoneName" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.timezone_name (fun f ->
             Aws.Query.Pair ("TimezoneName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.timezone_name (fun f -> "TimezoneName", String.to_json f) ])

let of_json j =
  { timezone_name = Aws.Util.option_map (Aws.Json.lookup j "TimezoneName") String.of_json
  }
