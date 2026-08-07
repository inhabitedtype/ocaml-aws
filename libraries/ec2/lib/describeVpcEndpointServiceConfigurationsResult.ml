open Aws.BaseTypes

type t =
  { service_configurations : ServiceConfigurationSet.t
  ; next_token : String.t option
  }

let make ?(service_configurations = []) ?next_token () =
  { service_configurations; next_token }

let parse xml =
  Some
    { service_configurations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "serviceConfigurationSet" xml)
             ServiceConfigurationSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ServiceConfigurationSet"
              , ServiceConfigurationSet.to_query v.service_configurations ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "serviceConfigurationSet"
           , ServiceConfigurationSet.to_json v.service_configurations )
       ])

let of_json j =
  { service_configurations =
      ServiceConfigurationSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "serviceConfigurationSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
