open Aws.BaseTypes

type t = { timezone : String.t option }

let make ?timezone () = { timezone }

let parse xml =
  Some { timezone = Aws.Util.option_bind (Aws.Xml.member "Timezone" xml) String.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.timezone (fun f ->
             Aws.Query.Pair ("Timezone", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.timezone (fun f -> "Timezone", String.to_json f) ])

let of_json j =
  { timezone = Aws.Util.option_map (Aws.Json.lookup j "Timezone") String.of_json }
