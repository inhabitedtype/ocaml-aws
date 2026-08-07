open Aws.BaseTypes

type t =
  { blue_green_deployment_identifier : String.t
  ; delete_target : Boolean.t option
  }

let make ~blue_green_deployment_identifier ?delete_target () =
  { blue_green_deployment_identifier; delete_target }

let parse xml =
  Some
    { blue_green_deployment_identifier =
        Aws.Xml.required
          "BlueGreenDeploymentIdentifier"
          (Aws.Util.option_bind
             (Aws.Xml.member "BlueGreenDeploymentIdentifier" xml)
             String.parse)
    ; delete_target =
        Aws.Util.option_bind (Aws.Xml.member "DeleteTarget" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.delete_target (fun f ->
             Aws.Query.Pair ("DeleteTarget", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "BlueGreenDeploymentIdentifier"
              , String.to_query v.blue_green_deployment_identifier ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.delete_target (fun f -> "DeleteTarget", Boolean.to_json f)
       ; Some
           ( "BlueGreenDeploymentIdentifier"
           , String.to_json v.blue_green_deployment_identifier )
       ])

let of_json j =
  { blue_green_deployment_identifier =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "BlueGreenDeploymentIdentifier"))
  ; delete_target = Aws.Util.option_map (Aws.Json.lookup j "DeleteTarget") Boolean.of_json
  }
