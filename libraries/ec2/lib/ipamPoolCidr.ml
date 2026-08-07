open Aws.BaseTypes

type t =
  { cidr : String.t option
  ; state : IpamPoolCidrState.t option
  ; failure_reason : IpamPoolCidrFailureReason.t option
  ; ipam_pool_cidr_id : String.t option
  ; netmask_length : Integer.t option
  }

let make ?cidr ?state ?failure_reason ?ipam_pool_cidr_id ?netmask_length () =
  { cidr; state; failure_reason; ipam_pool_cidr_id; netmask_length }

let parse xml =
  Some
    { cidr = Aws.Util.option_bind (Aws.Xml.member "cidr" xml) String.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "state" xml) IpamPoolCidrState.parse
    ; failure_reason =
        Aws.Util.option_bind
          (Aws.Xml.member "failureReason" xml)
          IpamPoolCidrFailureReason.parse
    ; ipam_pool_cidr_id =
        Aws.Util.option_bind (Aws.Xml.member "ipamPoolCidrId" xml) String.parse
    ; netmask_length =
        Aws.Util.option_bind (Aws.Xml.member "netmaskLength" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.netmask_length (fun f ->
             Aws.Query.Pair ("NetmaskLength", Integer.to_query f))
       ; Aws.Util.option_map v.ipam_pool_cidr_id (fun f ->
             Aws.Query.Pair ("IpamPoolCidrId", String.to_query f))
       ; Aws.Util.option_map v.failure_reason (fun f ->
             Aws.Query.Pair ("FailureReason", IpamPoolCidrFailureReason.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", IpamPoolCidrState.to_query f))
       ; Aws.Util.option_map v.cidr (fun f -> Aws.Query.Pair ("Cidr", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.netmask_length (fun f ->
             "netmaskLength", Integer.to_json f)
       ; Aws.Util.option_map v.ipam_pool_cidr_id (fun f ->
             "ipamPoolCidrId", String.to_json f)
       ; Aws.Util.option_map v.failure_reason (fun f ->
             "failureReason", IpamPoolCidrFailureReason.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "state", IpamPoolCidrState.to_json f)
       ; Aws.Util.option_map v.cidr (fun f -> "cidr", String.to_json f)
       ])

let of_json j =
  { cidr = Aws.Util.option_map (Aws.Json.lookup j "cidr") String.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") IpamPoolCidrState.of_json
  ; failure_reason =
      Aws.Util.option_map
        (Aws.Json.lookup j "failureReason")
        IpamPoolCidrFailureReason.of_json
  ; ipam_pool_cidr_id =
      Aws.Util.option_map (Aws.Json.lookup j "ipamPoolCidrId") String.of_json
  ; netmask_length =
      Aws.Util.option_map (Aws.Json.lookup j "netmaskLength") Integer.of_json
  }
