open Aws.BaseTypes

type t =
  { load_balancer_name : String.t
  ; load_balancer_port : Integer.t
  ; s_s_l_certificate_id : String.t
  }

let make ~load_balancer_name ~load_balancer_port ~s_s_l_certificate_id () =
  { load_balancer_name; load_balancer_port; s_s_l_certificate_id }

let parse xml =
  Some
    { load_balancer_name =
        Aws.Xml.required
          "LoadBalancerName"
          (Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse)
    ; load_balancer_port =
        Aws.Xml.required
          "LoadBalancerPort"
          (Aws.Util.option_bind (Aws.Xml.member "LoadBalancerPort" xml) Integer.parse)
    ; s_s_l_certificate_id =
        Aws.Xml.required
          "SSLCertificateId"
          (Aws.Util.option_bind (Aws.Xml.member "SSLCertificateId" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("SSLCertificateId", String.to_query v.s_s_l_certificate_id))
       ; Some (Aws.Query.Pair ("LoadBalancerPort", Integer.to_query v.load_balancer_port))
       ; Some (Aws.Query.Pair ("LoadBalancerName", String.to_query v.load_balancer_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("SSLCertificateId", String.to_json v.s_s_l_certificate_id)
       ; Some ("LoadBalancerPort", Integer.to_json v.load_balancer_port)
       ; Some ("LoadBalancerName", String.to_json v.load_balancer_name)
       ])

let of_json j =
  { load_balancer_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerName"))
  ; load_balancer_port =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerPort"))
  ; s_s_l_certificate_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SSLCertificateId"))
  }
