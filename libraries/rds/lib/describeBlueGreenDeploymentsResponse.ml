open Aws.BaseTypes

type t =
  { blue_green_deployments : BlueGreenDeploymentList.t
  ; marker : String.t option
  }

let make ?(blue_green_deployments = []) ?marker () = { blue_green_deployments; marker }

let parse xml =
  Some
    { blue_green_deployments =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "BlueGreenDeployments" xml)
             BlueGreenDeploymentList.parse)
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "BlueGreenDeployments.member"
              , BlueGreenDeploymentList.to_query v.blue_green_deployments ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Some
           ( "BlueGreenDeployments"
           , BlueGreenDeploymentList.to_json v.blue_green_deployments )
       ])

let of_json j =
  { blue_green_deployments =
      BlueGreenDeploymentList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "BlueGreenDeployments"))
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  }
