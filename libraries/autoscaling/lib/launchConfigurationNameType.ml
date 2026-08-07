open Aws.BaseTypes

type t = { launch_configuration_name : String.t }

let make ~launch_configuration_name () = { launch_configuration_name }

let parse xml =
  Some
    { launch_configuration_name =
        Aws.Xml.required
          "LaunchConfigurationName"
          (Aws.Util.option_bind
             (Aws.Xml.member "LaunchConfigurationName" xml)
             String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("LaunchConfigurationName", String.to_query v.launch_configuration_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("LaunchConfigurationName", String.to_json v.launch_configuration_name) ])

let of_json j =
  { launch_configuration_name =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LaunchConfigurationName"))
  }
