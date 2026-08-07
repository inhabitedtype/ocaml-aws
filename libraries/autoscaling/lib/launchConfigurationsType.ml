open Aws.BaseTypes

type t =
  { launch_configurations : LaunchConfigurations.t
  ; next_token : String.t option
  }

let make ~launch_configurations ?next_token () = { launch_configurations; next_token }

let parse xml =
  Some
    { launch_configurations =
        Aws.Xml.required
          "LaunchConfigurations"
          (Aws.Util.option_bind
             (Aws.Xml.member "LaunchConfigurations" xml)
             LaunchConfigurations.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "LaunchConfigurations.member"
              , LaunchConfigurations.to_query v.launch_configurations ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some
           ("LaunchConfigurations", LaunchConfigurations.to_json v.launch_configurations)
       ])

let of_json j =
  { launch_configurations =
      LaunchConfigurations.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LaunchConfigurations"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
