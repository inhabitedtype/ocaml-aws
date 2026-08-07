open Aws.BaseTypes

type t =
  { parameters : ParametersList.t
  ; marker : String.t option
  }

let make ?(parameters = []) ?marker () = { parameters; marker }

let parse xml =
  Some
    { parameters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Parameters" xml) ParametersList.parse)
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Some (Aws.Query.Pair ("Parameters.member", ParametersList.to_query v.parameters))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Some ("Parameters", ParametersList.to_json v.parameters)
       ])

let of_json j =
  { parameters =
      ParametersList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Parameters"))
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  }
