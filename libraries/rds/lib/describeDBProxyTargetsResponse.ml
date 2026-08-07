open Aws.BaseTypes

type t =
  { targets : TargetList.t
  ; marker : String.t option
  }

let make ?(targets = []) ?marker () = { targets; marker }

let parse xml =
  Some
    { targets =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Targets" xml) TargetList.parse)
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Some (Aws.Query.Pair ("Targets.member", TargetList.to_query v.targets))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Some ("Targets", TargetList.to_json v.targets)
       ])

let of_json j =
  { targets = TargetList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Targets"))
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  }
