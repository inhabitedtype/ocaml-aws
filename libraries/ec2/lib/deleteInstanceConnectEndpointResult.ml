type t = { instance_connect_endpoint : Ec2InstanceConnectEndpoint.t option }

let make ?instance_connect_endpoint () = { instance_connect_endpoint }

let parse xml =
  Some
    { instance_connect_endpoint =
        Aws.Util.option_bind
          (Aws.Xml.member "instanceConnectEndpoint" xml)
          Ec2InstanceConnectEndpoint.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_connect_endpoint (fun f ->
             Aws.Query.Pair
               ("InstanceConnectEndpoint", Ec2InstanceConnectEndpoint.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_connect_endpoint (fun f ->
             "instanceConnectEndpoint", Ec2InstanceConnectEndpoint.to_json f)
       ])

let of_json j =
  { instance_connect_endpoint =
      Aws.Util.option_map
        (Aws.Json.lookup j "instanceConnectEndpoint")
        Ec2InstanceConnectEndpoint.of_json
  }
