open Aws.BaseTypes

type t =
  { provision_time : DateTime.t option
  ; provisioned : String.t option
  ; request_time : DateTime.t option
  ; requested : String.t option
  ; status : String.t option
  }

let make ?provision_time ?provisioned ?request_time ?requested ?status () =
  { provision_time; provisioned; request_time; requested; status }

let parse xml =
  Some
    { provision_time =
        Aws.Util.option_bind (Aws.Xml.member "provisionTime" xml) DateTime.parse
    ; provisioned = Aws.Util.option_bind (Aws.Xml.member "provisioned" xml) String.parse
    ; request_time =
        Aws.Util.option_bind (Aws.Xml.member "requestTime" xml) DateTime.parse
    ; requested = Aws.Util.option_bind (Aws.Xml.member "requested" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "status" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.requested (fun f ->
             Aws.Query.Pair ("Requested", String.to_query f))
       ; Aws.Util.option_map v.request_time (fun f ->
             Aws.Query.Pair ("RequestTime", DateTime.to_query f))
       ; Aws.Util.option_map v.provisioned (fun f ->
             Aws.Query.Pair ("Provisioned", String.to_query f))
       ; Aws.Util.option_map v.provision_time (fun f ->
             Aws.Query.Pair ("ProvisionTime", DateTime.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.status (fun f -> "status", String.to_json f)
       ; Aws.Util.option_map v.requested (fun f -> "requested", String.to_json f)
       ; Aws.Util.option_map v.request_time (fun f -> "requestTime", DateTime.to_json f)
       ; Aws.Util.option_map v.provisioned (fun f -> "provisioned", String.to_json f)
       ; Aws.Util.option_map v.provision_time (fun f ->
             "provisionTime", DateTime.to_json f)
       ])

let of_json j =
  { provision_time =
      Aws.Util.option_map (Aws.Json.lookup j "provisionTime") DateTime.of_json
  ; provisioned = Aws.Util.option_map (Aws.Json.lookup j "provisioned") String.of_json
  ; request_time = Aws.Util.option_map (Aws.Json.lookup j "requestTime") DateTime.of_json
  ; requested = Aws.Util.option_map (Aws.Json.lookup j "requested") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "status") String.of_json
  }
