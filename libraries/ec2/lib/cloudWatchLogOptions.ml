open Aws.BaseTypes

type t =
  { log_enabled : Boolean.t option
  ; log_group_arn : String.t option
  ; log_output_format : String.t option
  ; bgp_log_enabled : Boolean.t option
  ; bgp_log_group_arn : String.t option
  ; bgp_log_output_format : String.t option
  }

let make
    ?log_enabled
    ?log_group_arn
    ?log_output_format
    ?bgp_log_enabled
    ?bgp_log_group_arn
    ?bgp_log_output_format
    () =
  { log_enabled
  ; log_group_arn
  ; log_output_format
  ; bgp_log_enabled
  ; bgp_log_group_arn
  ; bgp_log_output_format
  }

let parse xml =
  Some
    { log_enabled = Aws.Util.option_bind (Aws.Xml.member "logEnabled" xml) Boolean.parse
    ; log_group_arn = Aws.Util.option_bind (Aws.Xml.member "logGroupArn" xml) String.parse
    ; log_output_format =
        Aws.Util.option_bind (Aws.Xml.member "logOutputFormat" xml) String.parse
    ; bgp_log_enabled =
        Aws.Util.option_bind (Aws.Xml.member "bgpLogEnabled" xml) Boolean.parse
    ; bgp_log_group_arn =
        Aws.Util.option_bind (Aws.Xml.member "bgpLogGroupArn" xml) String.parse
    ; bgp_log_output_format =
        Aws.Util.option_bind (Aws.Xml.member "bgpLogOutputFormat" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.bgp_log_output_format (fun f ->
             Aws.Query.Pair ("BgpLogOutputFormat", String.to_query f))
       ; Aws.Util.option_map v.bgp_log_group_arn (fun f ->
             Aws.Query.Pair ("BgpLogGroupArn", String.to_query f))
       ; Aws.Util.option_map v.bgp_log_enabled (fun f ->
             Aws.Query.Pair ("BgpLogEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.log_output_format (fun f ->
             Aws.Query.Pair ("LogOutputFormat", String.to_query f))
       ; Aws.Util.option_map v.log_group_arn (fun f ->
             Aws.Query.Pair ("LogGroupArn", String.to_query f))
       ; Aws.Util.option_map v.log_enabled (fun f ->
             Aws.Query.Pair ("LogEnabled", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.bgp_log_output_format (fun f ->
             "bgpLogOutputFormat", String.to_json f)
       ; Aws.Util.option_map v.bgp_log_group_arn (fun f ->
             "bgpLogGroupArn", String.to_json f)
       ; Aws.Util.option_map v.bgp_log_enabled (fun f ->
             "bgpLogEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.log_output_format (fun f ->
             "logOutputFormat", String.to_json f)
       ; Aws.Util.option_map v.log_group_arn (fun f -> "logGroupArn", String.to_json f)
       ; Aws.Util.option_map v.log_enabled (fun f -> "logEnabled", Boolean.to_json f)
       ])

let of_json j =
  { log_enabled = Aws.Util.option_map (Aws.Json.lookup j "logEnabled") Boolean.of_json
  ; log_group_arn = Aws.Util.option_map (Aws.Json.lookup j "logGroupArn") String.of_json
  ; log_output_format =
      Aws.Util.option_map (Aws.Json.lookup j "logOutputFormat") String.of_json
  ; bgp_log_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "bgpLogEnabled") Boolean.of_json
  ; bgp_log_group_arn =
      Aws.Util.option_map (Aws.Json.lookup j "bgpLogGroupArn") String.of_json
  ; bgp_log_output_format =
      Aws.Util.option_map (Aws.Json.lookup j "bgpLogOutputFormat") String.of_json
  }
