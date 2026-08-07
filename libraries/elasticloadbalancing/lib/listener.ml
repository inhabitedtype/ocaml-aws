open Aws.BaseTypes

type t =
  { protocol : String.t
  ; load_balancer_port : Integer.t
  ; instance_protocol : String.t option
  ; instance_port : Integer.t
  ; s_s_l_certificate_id : String.t option
  }

let make
    ~protocol
    ~load_balancer_port
    ?instance_protocol
    ~instance_port
    ?s_s_l_certificate_id
    () =
  { protocol; load_balancer_port; instance_protocol; instance_port; s_s_l_certificate_id }

let parse xml =
  Some
    { protocol =
        Aws.Xml.required
          "Protocol"
          (Aws.Util.option_bind (Aws.Xml.member "Protocol" xml) String.parse)
    ; load_balancer_port =
        Aws.Xml.required
          "LoadBalancerPort"
          (Aws.Util.option_bind (Aws.Xml.member "LoadBalancerPort" xml) Integer.parse)
    ; instance_protocol =
        Aws.Util.option_bind (Aws.Xml.member "InstanceProtocol" xml) String.parse
    ; instance_port =
        Aws.Xml.required
          "InstancePort"
          (Aws.Util.option_bind (Aws.Xml.member "InstancePort" xml) Integer.parse)
    ; s_s_l_certificate_id =
        Aws.Util.option_bind (Aws.Xml.member "SSLCertificateId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.s_s_l_certificate_id (fun f ->
             Aws.Query.Pair ("SSLCertificateId", String.to_query f))
       ; Some (Aws.Query.Pair ("InstancePort", Integer.to_query v.instance_port))
       ; Aws.Util.option_map v.instance_protocol (fun f ->
             Aws.Query.Pair ("InstanceProtocol", String.to_query f))
       ; Some (Aws.Query.Pair ("LoadBalancerPort", Integer.to_query v.load_balancer_port))
       ; Some (Aws.Query.Pair ("Protocol", String.to_query v.protocol))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.s_s_l_certificate_id (fun f ->
             "SSLCertificateId", String.to_json f)
       ; Some ("InstancePort", Integer.to_json v.instance_port)
       ; Aws.Util.option_map v.instance_protocol (fun f ->
             "InstanceProtocol", String.to_json f)
       ; Some ("LoadBalancerPort", Integer.to_json v.load_balancer_port)
       ; Some ("Protocol", String.to_json v.protocol)
       ])

let of_json j =
  { protocol = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Protocol"))
  ; load_balancer_port =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerPort"))
  ; instance_protocol =
      Aws.Util.option_map (Aws.Json.lookup j "InstanceProtocol") String.of_json
  ; instance_port =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstancePort"))
  ; s_s_l_certificate_id =
      Aws.Util.option_map (Aws.Json.lookup j "SSLCertificateId") String.of_json
  }
