open Aws.BaseTypes

type t =
  { instance_connect_endpoint : Ec2InstanceConnectEndpoint.t option
  ; client_token : String.t option
  }

let make ?instance_connect_endpoint ?client_token () =
  { instance_connect_endpoint; client_token }

let parse xml =
  Some
    { instance_connect_endpoint =
        Aws.Util.option_bind
          (Aws.Xml.member "instanceConnectEndpoint" xml)
          Ec2InstanceConnectEndpoint.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "clientToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.instance_connect_endpoint (fun f ->
             Aws.Query.Pair
               ("InstanceConnectEndpoint", Ec2InstanceConnectEndpoint.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_token (fun f -> "clientToken", String.to_json f)
       ; Aws.Util.option_map v.instance_connect_endpoint (fun f ->
             "instanceConnectEndpoint", Ec2InstanceConnectEndpoint.to_json f)
       ])

let of_json j =
  { instance_connect_endpoint =
      Aws.Util.option_map
        (Aws.Json.lookup j "instanceConnectEndpoint")
        Ec2InstanceConnectEndpoint.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "clientToken") String.of_json
  }
