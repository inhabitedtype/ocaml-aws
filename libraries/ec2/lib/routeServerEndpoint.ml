open Aws.BaseTypes

type t =
  { route_server_id : String.t option
  ; route_server_endpoint_id : String.t option
  ; vpc_id : String.t option
  ; subnet_id : String.t option
  ; eni_id : String.t option
  ; eni_address : String.t option
  ; state : RouteServerEndpointState.t option
  ; failure_reason : String.t option
  ; tags : TagList.t
  }

let make
    ?route_server_id
    ?route_server_endpoint_id
    ?vpc_id
    ?subnet_id
    ?eni_id
    ?eni_address
    ?state
    ?failure_reason
    ?(tags = [])
    () =
  { route_server_id
  ; route_server_endpoint_id
  ; vpc_id
  ; subnet_id
  ; eni_id
  ; eni_address
  ; state
  ; failure_reason
  ; tags
  }

let parse xml =
  Some
    { route_server_id =
        Aws.Util.option_bind (Aws.Xml.member "routeServerId" xml) String.parse
    ; route_server_endpoint_id =
        Aws.Util.option_bind (Aws.Xml.member "routeServerEndpointId" xml) String.parse
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "vpcId" xml) String.parse
    ; subnet_id = Aws.Util.option_bind (Aws.Xml.member "subnetId" xml) String.parse
    ; eni_id = Aws.Util.option_bind (Aws.Xml.member "eniId" xml) String.parse
    ; eni_address = Aws.Util.option_bind (Aws.Xml.member "eniAddress" xml) String.parse
    ; state =
        Aws.Util.option_bind (Aws.Xml.member "state" xml) RouteServerEndpointState.parse
    ; failure_reason =
        Aws.Util.option_bind (Aws.Xml.member "failureReason" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.failure_reason (fun f ->
             Aws.Query.Pair ("FailureReason", String.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", RouteServerEndpointState.to_query f))
       ; Aws.Util.option_map v.eni_address (fun f ->
             Aws.Query.Pair ("EniAddress", String.to_query f))
       ; Aws.Util.option_map v.eni_id (fun f ->
             Aws.Query.Pair ("EniId", String.to_query f))
       ; Aws.Util.option_map v.subnet_id (fun f ->
             Aws.Query.Pair ("SubnetId", String.to_query f))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Aws.Util.option_map v.route_server_endpoint_id (fun f ->
             Aws.Query.Pair ("RouteServerEndpointId", String.to_query f))
       ; Aws.Util.option_map v.route_server_id (fun f ->
             Aws.Query.Pair ("RouteServerId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.failure_reason (fun f -> "failureReason", String.to_json f)
       ; Aws.Util.option_map v.state (fun f ->
             "state", RouteServerEndpointState.to_json f)
       ; Aws.Util.option_map v.eni_address (fun f -> "eniAddress", String.to_json f)
       ; Aws.Util.option_map v.eni_id (fun f -> "eniId", String.to_json f)
       ; Aws.Util.option_map v.subnet_id (fun f -> "subnetId", String.to_json f)
       ; Aws.Util.option_map v.vpc_id (fun f -> "vpcId", String.to_json f)
       ; Aws.Util.option_map v.route_server_endpoint_id (fun f ->
             "routeServerEndpointId", String.to_json f)
       ; Aws.Util.option_map v.route_server_id (fun f ->
             "routeServerId", String.to_json f)
       ])

let of_json j =
  { route_server_id =
      Aws.Util.option_map (Aws.Json.lookup j "routeServerId") String.of_json
  ; route_server_endpoint_id =
      Aws.Util.option_map (Aws.Json.lookup j "routeServerEndpointId") String.of_json
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "vpcId") String.of_json
  ; subnet_id = Aws.Util.option_map (Aws.Json.lookup j "subnetId") String.of_json
  ; eni_id = Aws.Util.option_map (Aws.Json.lookup j "eniId") String.of_json
  ; eni_address = Aws.Util.option_map (Aws.Json.lookup j "eniAddress") String.of_json
  ; state =
      Aws.Util.option_map (Aws.Json.lookup j "state") RouteServerEndpointState.of_json
  ; failure_reason =
      Aws.Util.option_map (Aws.Json.lookup j "failureReason") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
