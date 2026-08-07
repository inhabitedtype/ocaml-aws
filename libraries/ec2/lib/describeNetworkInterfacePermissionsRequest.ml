open Aws.BaseTypes

type t =
  { network_interface_permission_ids : NetworkInterfacePermissionIdList.t
  ; filters : FilterList.t
  ; next_token : String.t option
  ; max_results : Integer.t option
  }

let make
    ?(network_interface_permission_ids = [])
    ?(filters = [])
    ?next_token
    ?max_results
    () =
  { network_interface_permission_ids; filters; next_token; max_results }

let parse xml =
  Some
    { network_interface_permission_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "NetworkInterfacePermissionId" xml)
             NetworkInterfacePermissionIdList.parse)
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Some
           (Aws.Query.Pair
              ( "NetworkInterfacePermissionId"
              , NetworkInterfacePermissionIdList.to_query
                  v.network_interface_permission_ids ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some ("Filter", FilterList.to_json v.filters)
       ; Some
           ( "NetworkInterfacePermissionId"
           , NetworkInterfacePermissionIdList.to_json v.network_interface_permission_ids
           )
       ])

let of_json j =
  { network_interface_permission_ids =
      NetworkInterfacePermissionIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "NetworkInterfacePermissionId"))
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  }
