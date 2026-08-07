open Aws.BaseTypes

type t =
  { subnet_ids : ModifyVerifiedAccessEndpointSubnetIdList.t
  ; port : Integer.t option
  ; rds_endpoint : String.t option
  }

let make ?(subnet_ids = []) ?port ?rds_endpoint () = { subnet_ids; port; rds_endpoint }

let parse xml =
  Some
    { subnet_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SubnetId" xml)
             ModifyVerifiedAccessEndpointSubnetIdList.parse)
    ; port = Aws.Util.option_bind (Aws.Xml.member "Port" xml) Integer.parse
    ; rds_endpoint = Aws.Util.option_bind (Aws.Xml.member "RdsEndpoint" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.rds_endpoint (fun f ->
             Aws.Query.Pair ("RdsEndpoint", String.to_query f))
       ; Aws.Util.option_map v.port (fun f -> Aws.Query.Pair ("Port", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ("SubnetId", ModifyVerifiedAccessEndpointSubnetIdList.to_query v.subnet_ids))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.rds_endpoint (fun f -> "RdsEndpoint", String.to_json f)
       ; Aws.Util.option_map v.port (fun f -> "Port", Integer.to_json f)
       ; Some ("SubnetId", ModifyVerifiedAccessEndpointSubnetIdList.to_json v.subnet_ids)
       ])

let of_json j =
  { subnet_ids =
      ModifyVerifiedAccessEndpointSubnetIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SubnetId"))
  ; port = Aws.Util.option_map (Aws.Json.lookup j "Port") Integer.of_json
  ; rds_endpoint = Aws.Util.option_map (Aws.Json.lookup j "RdsEndpoint") String.of_json
  }
