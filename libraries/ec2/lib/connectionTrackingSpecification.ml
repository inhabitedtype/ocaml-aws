open Aws.BaseTypes

type t =
  { tcp_established_timeout : Integer.t option
  ; udp_timeout : Integer.t option
  ; udp_stream_timeout : Integer.t option
  }

let make ?tcp_established_timeout ?udp_timeout ?udp_stream_timeout () =
  { tcp_established_timeout; udp_timeout; udp_stream_timeout }

let parse xml =
  Some
    { tcp_established_timeout =
        Aws.Util.option_bind (Aws.Xml.member "tcpEstablishedTimeout" xml) Integer.parse
    ; udp_timeout = Aws.Util.option_bind (Aws.Xml.member "udpTimeout" xml) Integer.parse
    ; udp_stream_timeout =
        Aws.Util.option_bind (Aws.Xml.member "udpStreamTimeout" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.udp_stream_timeout (fun f ->
             Aws.Query.Pair ("UdpStreamTimeout", Integer.to_query f))
       ; Aws.Util.option_map v.udp_timeout (fun f ->
             Aws.Query.Pair ("UdpTimeout", Integer.to_query f))
       ; Aws.Util.option_map v.tcp_established_timeout (fun f ->
             Aws.Query.Pair ("TcpEstablishedTimeout", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.udp_stream_timeout (fun f ->
             "udpStreamTimeout", Integer.to_json f)
       ; Aws.Util.option_map v.udp_timeout (fun f -> "udpTimeout", Integer.to_json f)
       ; Aws.Util.option_map v.tcp_established_timeout (fun f ->
             "tcpEstablishedTimeout", Integer.to_json f)
       ])

let of_json j =
  { tcp_established_timeout =
      Aws.Util.option_map (Aws.Json.lookup j "tcpEstablishedTimeout") Integer.of_json
  ; udp_timeout = Aws.Util.option_map (Aws.Json.lookup j "udpTimeout") Integer.of_json
  ; udp_stream_timeout =
      Aws.Util.option_map (Aws.Json.lookup j "udpStreamTimeout") Integer.of_json
  }
