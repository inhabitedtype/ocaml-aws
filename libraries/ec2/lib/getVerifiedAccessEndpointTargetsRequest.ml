open Aws.BaseTypes

type t =
  { verified_access_endpoint_id : String.t
  ; max_results : Integer.t option
  ; next_token : String.t option
  ; dry_run : Boolean.t option
  }

let make ~verified_access_endpoint_id ?max_results ?next_token ?dry_run () =
  { verified_access_endpoint_id; max_results; next_token; dry_run }

let parse xml =
  Some
    { verified_access_endpoint_id =
        Aws.Xml.required
          "VerifiedAccessEndpointId"
          (Aws.Util.option_bind
             (Aws.Xml.member "VerifiedAccessEndpointId" xml)
             String.parse)
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ("VerifiedAccessEndpointId", String.to_query v.verified_access_endpoint_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Some ("VerifiedAccessEndpointId", String.to_json v.verified_access_endpoint_id)
       ])

let of_json j =
  { verified_access_endpoint_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "VerifiedAccessEndpointId"))
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
