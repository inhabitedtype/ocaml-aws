open Aws.BaseTypes

type t =
  { d_b_parameter_group_family : String.t option
  ; marker : String.t option
  ; parameters : ParametersList.t
  }

let make ?d_b_parameter_group_family ?marker ?(parameters = []) () =
  { d_b_parameter_group_family; marker; parameters }

let parse xml =
  Some
    { d_b_parameter_group_family =
        Aws.Util.option_bind (Aws.Xml.member "DBParameterGroupFamily" xml) String.parse
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; parameters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Parameters" xml) ParametersList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Parameters.member", ParametersList.to_query v.parameters))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Aws.Util.option_map v.d_b_parameter_group_family (fun f ->
             Aws.Query.Pair ("DBParameterGroupFamily", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Parameters", ParametersList.to_json v.parameters)
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Aws.Util.option_map v.d_b_parameter_group_family (fun f ->
             "DBParameterGroupFamily", String.to_json f)
       ])

let of_json j =
  { d_b_parameter_group_family =
      Aws.Util.option_map (Aws.Json.lookup j "DBParameterGroupFamily") String.of_json
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; parameters =
      ParametersList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Parameters"))
  }
