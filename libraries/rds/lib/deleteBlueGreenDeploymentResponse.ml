type t = { blue_green_deployment : BlueGreenDeployment.t option }

let make ?blue_green_deployment () = { blue_green_deployment }

let parse xml =
  Some
    { blue_green_deployment =
        Aws.Util.option_bind
          (Aws.Xml.member "BlueGreenDeployment" xml)
          BlueGreenDeployment.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.blue_green_deployment (fun f ->
             Aws.Query.Pair ("BlueGreenDeployment", BlueGreenDeployment.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.blue_green_deployment (fun f ->
             "BlueGreenDeployment", BlueGreenDeployment.to_json f)
       ])

let of_json j =
  { blue_green_deployment =
      Aws.Util.option_map
        (Aws.Json.lookup j "BlueGreenDeployment")
        BlueGreenDeployment.of_json
  }
