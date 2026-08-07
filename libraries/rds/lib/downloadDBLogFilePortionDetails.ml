open Aws.BaseTypes

type t =
  { log_file_data : String.t option
  ; marker : String.t option
  ; additional_data_pending : Boolean.t option
  }

let make ?log_file_data ?marker ?additional_data_pending () =
  { log_file_data; marker; additional_data_pending }

let parse xml =
  Some
    { log_file_data = Aws.Util.option_bind (Aws.Xml.member "LogFileData" xml) String.parse
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; additional_data_pending =
        Aws.Util.option_bind (Aws.Xml.member "AdditionalDataPending" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.additional_data_pending (fun f ->
             Aws.Query.Pair ("AdditionalDataPending", Boolean.to_query f))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Aws.Util.option_map v.log_file_data (fun f ->
             Aws.Query.Pair ("LogFileData", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.additional_data_pending (fun f ->
             "AdditionalDataPending", Boolean.to_json f)
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Aws.Util.option_map v.log_file_data (fun f -> "LogFileData", String.to_json f)
       ])

let of_json j =
  { log_file_data = Aws.Util.option_map (Aws.Json.lookup j "LogFileData") String.of_json
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; additional_data_pending =
      Aws.Util.option_map (Aws.Json.lookup j "AdditionalDataPending") Boolean.of_json
  }
