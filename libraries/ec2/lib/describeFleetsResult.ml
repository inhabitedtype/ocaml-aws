open Aws.BaseTypes

type t =
  { next_token : String.t option
  ; fleets : FleetSet.t
  }

let make ?next_token ?(fleets = []) () = { next_token; fleets }

let parse xml =
  Some
    { next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    ; fleets =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "fleetSet" xml) FleetSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("FleetSet", FleetSet.to_query v.fleets))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("fleetSet", FleetSet.to_json v.fleets)
       ; Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ])

let of_json j =
  { next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  ; fleets = FleetSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "fleetSet"))
  }
