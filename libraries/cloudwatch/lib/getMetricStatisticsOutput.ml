open Aws.BaseTypes

type t =
  { label : String.t option
  ; datapoints : Datapoints.t
  }

let make ?label ?(datapoints = []) () = { label; datapoints }

let parse xml =
  Some
    { label = Aws.Util.option_bind (Aws.Xml.member "Label" xml) String.parse
    ; datapoints =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Datapoints" xml) Datapoints.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Datapoints.member", Datapoints.to_query v.datapoints))
       ; Aws.Util.option_map v.label (fun f ->
             Aws.Query.Pair ("Label", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Datapoints", Datapoints.to_json v.datapoints)
       ; Aws.Util.option_map v.label (fun f -> "Label", String.to_json f)
       ])

let of_json j =
  { label = Aws.Util.option_map (Aws.Json.lookup j "Label") String.of_json
  ; datapoints =
      Datapoints.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Datapoints"))
  }
