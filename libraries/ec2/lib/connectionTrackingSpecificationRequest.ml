open Aws.BaseTypes

type t =
  { tcp_established_timeout : Integer.t option
  ; udp_stream_timeout : Integer.t option
  ; udp_timeout : Integer.t option
  }

let make ?tcp_established_timeout ?udp_stream_timeout ?udp_timeout () =
  { tcp_established_timeout; udp_stream_timeout; udp_timeout }

let parse xml =
  Some
    { tcp_established_timeout =
        Aws.Util.option_bind (Aws.Xml.member "TcpEstablishedTimeout" xml) Integer.parse
    ; udp_stream_timeout =
        Aws.Util.option_bind (Aws.Xml.member "UdpStreamTimeout" xml) Integer.parse
    ; udp_timeout = Aws.Util.option_bind (Aws.Xml.member "UdpTimeout" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.udp_timeout (fun f ->
             Aws.Query.Pair ("UdpTimeout", Integer.to_query f))
       ; Aws.Util.option_map v.udp_stream_timeout (fun f ->
             Aws.Query.Pair ("UdpStreamTimeout", Integer.to_query f))
       ; Aws.Util.option_map v.tcp_established_timeout (fun f ->
             Aws.Query.Pair ("TcpEstablishedTimeout", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.udp_timeout (fun f -> "UdpTimeout", Integer.to_json f)
       ; Aws.Util.option_map v.udp_stream_timeout (fun f ->
             "UdpStreamTimeout", Integer.to_json f)
       ; Aws.Util.option_map v.tcp_established_timeout (fun f ->
             "TcpEstablishedTimeout", Integer.to_json f)
       ])

let of_json j =
  { tcp_established_timeout =
      Aws.Util.option_map (Aws.Json.lookup j "TcpEstablishedTimeout") Integer.of_json
  ; udp_stream_timeout =
      Aws.Util.option_map (Aws.Json.lookup j "UdpStreamTimeout") Integer.of_json
  ; udp_timeout = Aws.Util.option_map (Aws.Json.lookup j "UdpTimeout") Integer.of_json
  }
