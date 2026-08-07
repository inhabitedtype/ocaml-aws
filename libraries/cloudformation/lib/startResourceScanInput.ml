open Aws.BaseTypes

type t =
  { client_request_token : String.t option
  ; scan_filters : ScanFilters.t
  }

let make ?client_request_token ?(scan_filters = []) () =
  { client_request_token; scan_filters }

let parse xml =
  Some
    { client_request_token =
        Aws.Util.option_bind (Aws.Xml.member "ClientRequestToken" xml) String.parse
    ; scan_filters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "ScanFilters" xml) ScanFilters.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("ScanFilters.member", ScanFilters.to_query v.scan_filters))
       ; Aws.Util.option_map v.client_request_token (fun f ->
             Aws.Query.Pair ("ClientRequestToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ScanFilters", ScanFilters.to_json v.scan_filters)
       ; Aws.Util.option_map v.client_request_token (fun f ->
             "ClientRequestToken", String.to_json f)
       ])

let of_json j =
  { client_request_token =
      Aws.Util.option_map (Aws.Json.lookup j "ClientRequestToken") String.of_json
  ; scan_filters =
      ScanFilters.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ScanFilters"))
  }
