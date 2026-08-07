open Aws.BaseTypes

type t =
  { channel : String.t
  ; destinations : Destinations.t
  ; name : String.t option
  }

let make ~channel ?(destinations = []) ?name () = { channel; destinations; name }

let parse xml =
  Some
    { channel =
        Aws.Xml.required
          "Channel"
          (Aws.Util.option_bind (Aws.Xml.member "Channel" xml) String.parse)
    ; destinations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Destinations" xml) Destinations.parse)
    ; name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ; Some
           (Aws.Query.Pair ("Destinations.member", Destinations.to_query v.destinations))
       ; Some (Aws.Query.Pair ("Channel", String.to_query v.channel))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
       ; Some ("Destinations", Destinations.to_json v.destinations)
       ; Some ("Channel", String.to_json v.channel)
       ])

let of_json j =
  { channel = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Channel"))
  ; destinations =
      Destinations.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Destinations"))
  ; name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
  }
