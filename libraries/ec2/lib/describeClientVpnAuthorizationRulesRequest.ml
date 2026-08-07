open Aws.BaseTypes

type t =
  { client_vpn_endpoint_id : String.t
  ; dry_run : Boolean.t option
  ; next_token : String.t option
  ; filters : FilterList.t
  ; max_results : Integer.t option
  }

let make ~client_vpn_endpoint_id ?dry_run ?next_token ?(filters = []) ?max_results () =
  { client_vpn_endpoint_id; dry_run; next_token; filters; max_results }

let parse xml =
  Some
    { client_vpn_endpoint_id =
        Aws.Xml.required
          "ClientVpnEndpointId"
          (Aws.Util.option_bind (Aws.Xml.member "ClientVpnEndpointId" xml) String.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Filter" xml) FilterList.parse)
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Some (Aws.Query.Pair ("Filter", FilterList.to_query v.filters))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("ClientVpnEndpointId", String.to_query v.client_vpn_endpoint_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Some ("Filter", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("ClientVpnEndpointId", String.to_json v.client_vpn_endpoint_id)
       ])

let of_json j =
  { client_vpn_endpoint_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ClientVpnEndpointId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filter"))
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  }
