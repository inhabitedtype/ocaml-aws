open Aws.BaseTypes

type t =
  { default_tcp_established_timeout : Integer.t option
  ; default_udp_timeout : Integer.t option
  ; default_udp_stream_timeout : Integer.t option
  }

let make
    ?default_tcp_established_timeout
    ?default_udp_timeout
    ?default_udp_stream_timeout
    () =
  { default_tcp_established_timeout; default_udp_timeout; default_udp_stream_timeout }

let parse xml =
  Some
    { default_tcp_established_timeout =
        Aws.Util.option_bind
          (Aws.Xml.member "defaultTcpEstablishedTimeout" xml)
          Integer.parse
    ; default_udp_timeout =
        Aws.Util.option_bind (Aws.Xml.member "defaultUdpTimeout" xml) Integer.parse
    ; default_udp_stream_timeout =
        Aws.Util.option_bind (Aws.Xml.member "defaultUdpStreamTimeout" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.default_udp_stream_timeout (fun f ->
             Aws.Query.Pair ("DefaultUdpStreamTimeout", Integer.to_query f))
       ; Aws.Util.option_map v.default_udp_timeout (fun f ->
             Aws.Query.Pair ("DefaultUdpTimeout", Integer.to_query f))
       ; Aws.Util.option_map v.default_tcp_established_timeout (fun f ->
             Aws.Query.Pair ("DefaultTcpEstablishedTimeout", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.default_udp_stream_timeout (fun f ->
             "defaultUdpStreamTimeout", Integer.to_json f)
       ; Aws.Util.option_map v.default_udp_timeout (fun f ->
             "defaultUdpTimeout", Integer.to_json f)
       ; Aws.Util.option_map v.default_tcp_established_timeout (fun f ->
             "defaultTcpEstablishedTimeout", Integer.to_json f)
       ])

let of_json j =
  { default_tcp_established_timeout =
      Aws.Util.option_map
        (Aws.Json.lookup j "defaultTcpEstablishedTimeout")
        Integer.of_json
  ; default_udp_timeout =
      Aws.Util.option_map (Aws.Json.lookup j "defaultUdpTimeout") Integer.of_json
  ; default_udp_stream_timeout =
      Aws.Util.option_map (Aws.Json.lookup j "defaultUdpStreamTimeout") Integer.of_json
  }
