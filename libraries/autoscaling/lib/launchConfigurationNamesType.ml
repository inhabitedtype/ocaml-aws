open Aws.BaseTypes

type t =
  { launch_configuration_names : LaunchConfigurationNames.t
  ; next_token : String.t option
  ; max_records : Integer.t option
  }

let make ?(launch_configuration_names = []) ?next_token ?max_records () =
  { launch_configuration_names; next_token; max_records }

let parse xml =
  Some
    { launch_configuration_names =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "LaunchConfigurationNames" xml)
             LaunchConfigurationNames.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_records (fun f ->
             Aws.Query.Pair ("MaxRecords", Integer.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "LaunchConfigurationNames.member"
              , LaunchConfigurationNames.to_query v.launch_configuration_names ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some
           ( "LaunchConfigurationNames"
           , LaunchConfigurationNames.to_json v.launch_configuration_names )
       ])

let of_json j =
  { launch_configuration_names =
      LaunchConfigurationNames.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LaunchConfigurationNames"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
  }
