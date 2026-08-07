open Aws.BaseTypes

type t =
  { event_data_store_arn : String.t option
  ; federation_status : FederationStatus.t option
  }

let make ?event_data_store_arn ?federation_status () =
  { event_data_store_arn; federation_status }

let parse xml =
  Some
    { event_data_store_arn =
        Aws.Util.option_bind (Aws.Xml.member "EventDataStoreArn" xml) String.parse
    ; federation_status =
        Aws.Util.option_bind
          (Aws.Xml.member "FederationStatus" xml)
          FederationStatus.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.federation_status (fun f ->
             Aws.Query.Pair ("FederationStatus", FederationStatus.to_query f))
       ; Aws.Util.option_map v.event_data_store_arn (fun f ->
             Aws.Query.Pair ("EventDataStoreArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.federation_status (fun f ->
             "FederationStatus", FederationStatus.to_json f)
       ; Aws.Util.option_map v.event_data_store_arn (fun f ->
             "EventDataStoreArn", String.to_json f)
       ])

let of_json j =
  { event_data_store_arn =
      Aws.Util.option_map (Aws.Json.lookup j "EventDataStoreArn") String.of_json
  ; federation_status =
      Aws.Util.option_map (Aws.Json.lookup j "FederationStatus") FederationStatus.of_json
  }
