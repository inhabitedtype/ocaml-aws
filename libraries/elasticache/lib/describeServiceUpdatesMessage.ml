open Aws.BaseTypes

type t =
  { service_update_name : String.t option
  ; service_update_status : ServiceUpdateStatusList.t
  ; max_records : Integer.t option
  ; marker : String.t option
  }

let make ?service_update_name ?(service_update_status = []) ?max_records ?marker () =
  { service_update_name; service_update_status; max_records; marker }

let parse xml =
  Some
    { service_update_name =
        Aws.Util.option_bind (Aws.Xml.member "ServiceUpdateName" xml) String.parse
    ; service_update_status =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ServiceUpdateStatus" xml)
             ServiceUpdateStatusList.parse)
    ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Aws.Util.option_map v.max_records (fun f ->
             Aws.Query.Pair ("MaxRecords", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ServiceUpdateStatus.member"
              , ServiceUpdateStatusList.to_query v.service_update_status ))
       ; Aws.Util.option_map v.service_update_name (fun f ->
             Aws.Query.Pair ("ServiceUpdateName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
       ; Some
           ("ServiceUpdateStatus", ServiceUpdateStatusList.to_json v.service_update_status)
       ; Aws.Util.option_map v.service_update_name (fun f ->
             "ServiceUpdateName", String.to_json f)
       ])

let of_json j =
  { service_update_name =
      Aws.Util.option_map (Aws.Json.lookup j "ServiceUpdateName") String.of_json
  ; service_update_status =
      ServiceUpdateStatusList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ServiceUpdateStatus"))
  ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  }
