open Aws.BaseTypes

type t =
  { blue_green_deployment_identifier : String.t
  ; switchover_timeout : Integer.t option
  }

let make ~blue_green_deployment_identifier ?switchover_timeout () =
  { blue_green_deployment_identifier; switchover_timeout }

let parse xml =
  Some
    { blue_green_deployment_identifier =
        Aws.Xml.required
          "BlueGreenDeploymentIdentifier"
          (Aws.Util.option_bind
             (Aws.Xml.member "BlueGreenDeploymentIdentifier" xml)
             String.parse)
    ; switchover_timeout =
        Aws.Util.option_bind (Aws.Xml.member "SwitchoverTimeout" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.switchover_timeout (fun f ->
             Aws.Query.Pair ("SwitchoverTimeout", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "BlueGreenDeploymentIdentifier"
              , String.to_query v.blue_green_deployment_identifier ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.switchover_timeout (fun f ->
             "SwitchoverTimeout", Integer.to_json f)
       ; Some
           ( "BlueGreenDeploymentIdentifier"
           , String.to_json v.blue_green_deployment_identifier )
       ])

let of_json j =
  { blue_green_deployment_identifier =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "BlueGreenDeploymentIdentifier"))
  ; switchover_timeout =
      Aws.Util.option_map (Aws.Json.lookup j "SwitchoverTimeout") Integer.of_json
  }
