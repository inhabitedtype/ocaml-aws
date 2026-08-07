open Aws.BaseTypes

type t = { periodic_spikes : Boolean.t option }

let make ?periodic_spikes () = { periodic_spikes }

let parse xml =
  Some
    { periodic_spikes =
        Aws.Util.option_bind (Aws.Xml.member "PeriodicSpikes" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.periodic_spikes (fun f ->
             Aws.Query.Pair ("PeriodicSpikes", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.periodic_spikes (fun f ->
             "PeriodicSpikes", Boolean.to_json f)
       ])

let of_json j =
  { periodic_spikes =
      Aws.Util.option_map (Aws.Json.lookup j "PeriodicSpikes") Boolean.of_json
  }
