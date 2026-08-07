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
    { log_enabled = Aws.Util.option_bind (Aws.Xml.member "LogEnabled" xml) Boolean.parse
    ; log_group_arn = Aws.Util.option_bind (Aws.Xml.member "LogGroupArn" xml) String.parse
    ; log_output_format =
        Aws.Util.option_bind (Aws.Xml.member "LogOutputFormat" xml) String.parse
    ; bgp_log_enabled =
        Aws.Util.option_bind (Aws.Xml.member "BgpLogEnabled" xml) Boolean.parse
    ; bgp_log_group_arn =
        Aws.Util.option_bind (Aws.Xml.member "BgpLogGroupArn" xml) String.parse
    ; bgp_log_output_format =
        Aws.Util.option_bind (Aws.Xml.member "BgpLogOutputFormat" xml) String.parse
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
             "BgpLogOutputFormat", String.to_json f)
       ; Aws.Util.option_map v.bgp_log_group_arn (fun f ->
             "BgpLogGroupArn", String.to_json f)
       ; Aws.Util.option_map v.bgp_log_enabled (fun f ->
             "BgpLogEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.log_output_format (fun f ->
             "LogOutputFormat", String.to_json f)
       ; Aws.Util.option_map v.log_group_arn (fun f -> "LogGroupArn", String.to_json f)
       ; Aws.Util.option_map v.log_enabled (fun f -> "LogEnabled", Boolean.to_json f)
       ])

let of_json j =
  { log_enabled = Aws.Util.option_map (Aws.Json.lookup j "LogEnabled") Boolean.of_json
  ; log_group_arn = Aws.Util.option_map (Aws.Json.lookup j "LogGroupArn") String.of_json
  ; log_output_format =
      Aws.Util.option_map (Aws.Json.lookup j "LogOutputFormat") String.of_json
  ; bgp_log_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "BgpLogEnabled") Boolean.of_json
  ; bgp_log_group_arn =
      Aws.Util.option_map (Aws.Json.lookup j "BgpLogGroupArn") String.of_json
  ; bgp_log_output_format =
      Aws.Util.option_map (Aws.Json.lookup j "BgpLogOutputFormat") String.of_json
  }
