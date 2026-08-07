open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; spot_fleet_request_ids : SpotFleetRequestIdList.t
  ; next_token : String.t option
  ; max_results : Integer.t option
  }

let make ?dry_run ?(spot_fleet_request_ids = []) ?next_token ?max_results () =
  { dry_run; spot_fleet_request_ids; next_token; max_results }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; spot_fleet_request_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "spotFleetRequestId" xml)
             SpotFleetRequestIdList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "maxResults" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SpotFleetRequestId"
              , SpotFleetRequestIdList.to_query v.spot_fleet_request_ids ))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f -> "maxResults", Integer.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ("spotFleetRequestId", SpotFleetRequestIdList.to_json v.spot_fleet_request_ids)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; spot_fleet_request_ids =
      SpotFleetRequestIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "spotFleetRequestId"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "maxResults") Integer.of_json
  }
