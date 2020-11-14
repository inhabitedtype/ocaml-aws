open Types
open Aws

type input = ModifyReplicationGroupMessage.t

type output = ModifyReplicationGroupResult.t

type error = Errors_internal.t

let service = "elasticache"

let to_http service region req =
  let uri =
    Uri.add_query_params
      (Uri.of_string (Aws.Util.of_option_exn (Endpoints.url_of service region)))
      (List.append
         [ "Version", [ "2015-02-02" ]; "Action", [ "ModifyReplicationGroup" ] ]
         (Util.drop_empty
            (Uri.query_of_encoded
               (Query.render (ModifyReplicationGroupMessage.to_query req)))))
  in
  `POST, uri, []

let of_http body =
  try
    let xml = Ezxmlm.from_string body in
    let resp =
      Util.option_bind
        (Xml.member "ModifyReplicationGroupResponse" (snd xml))
        (Xml.member "ModifyReplicationGroupResult")
    in
    try
      Util.or_error
        (Util.option_bind resp ModifyReplicationGroupResult.parse)
        (let open Error in
        BadResponse
          { body; message = "Could not find well formed ModifyReplicationGroupResult." })
    with Xml.RequiredFieldMissing msg ->
      let open Error in
      `Error
        (BadResponse
           { body
           ; message =
               "Error parsing ModifyReplicationGroupResult - missing field in body or \
                children: "
               ^ msg
           })
  with Failure msg ->
    `Error
      (let open Error in
      BadResponse { body; message = "Error parsing xml: " ^ msg })

let parse_error code err =
  let errors =
    [ Errors_internal.InvalidParameterCombination
    ; Errors_internal.InvalidParameterValue
    ; Errors_internal.InvalidVPCNetworkStateFault
    ; Errors_internal.CacheParameterGroupNotFound
    ; Errors_internal.CacheSecurityGroupNotFound
    ; Errors_internal.NodeQuotaForCustomerExceeded
    ; Errors_internal.NodeQuotaForClusterExceeded
    ; Errors_internal.CacheClusterNotFound
    ; Errors_internal.InsufficientCacheClusterCapacity
    ; Errors_internal.InvalidCacheSecurityGroupState
    ; Errors_internal.InvalidCacheClusterState
    ; Errors_internal.InvalidReplicationGroupState
    ; Errors_internal.ReplicationGroupNotFoundFault
    ]
    @ Errors_internal.common
  in
  match Errors_internal.of_string err with
  | Some var ->
      if List.mem var errors
         &&
         match Errors_internal.to_http_code var with
         | Some var -> var = code
         | None -> true
      then Some var
      else None
  | None -> None
