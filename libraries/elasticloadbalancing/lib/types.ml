open Aws.BaseTypes

type calendar = CalendarLib.Calendar.t

module PolicyNotFoundException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module InvalidConfigurationRequestException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module Subnets = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module DetachLoadBalancerFromSubnetsInput = struct
  type t =
    { load_balancer_name : String.t
    ; subnets : Subnets.t
    }

  let make ~load_balancer_name ~subnets () = { load_balancer_name; subnets }

  let parse xml =
    Some
      { load_balancer_name =
          Aws.Xml.required
            "LoadBalancerName"
            (Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse)
      ; subnets =
          Aws.Xml.required
            "Subnets"
            (Aws.Util.option_bind (Aws.Xml.member "Subnets" xml) Subnets.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Subnets.member", Subnets.to_query v.subnets))
         ; Some
             (Aws.Query.Pair ("LoadBalancerName", String.to_query v.load_balancer_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Subnets", Subnets.to_json v.subnets)
         ; Some ("LoadBalancerName", String.to_json v.load_balancer_name)
         ])

  let of_json j =
    { load_balancer_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerName"))
    ; subnets = Subnets.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Subnets"))
    }
end

module AccessPointNotFoundException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module LBCookieStickinessPolicy = struct
  type t =
    { policy_name : String.t option
    ; cookie_expiration_period : Long.t option
    }

  let make ?policy_name ?cookie_expiration_period () =
    { policy_name; cookie_expiration_period }

  let parse xml =
    Some
      { policy_name = Aws.Util.option_bind (Aws.Xml.member "PolicyName" xml) String.parse
      ; cookie_expiration_period =
          Aws.Util.option_bind (Aws.Xml.member "CookieExpirationPeriod" xml) Long.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cookie_expiration_period (fun f ->
               Aws.Query.Pair ("CookieExpirationPeriod", Long.to_query f))
         ; Aws.Util.option_map v.policy_name (fun f ->
               Aws.Query.Pair ("PolicyName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cookie_expiration_period (fun f ->
               "CookieExpirationPeriod", Long.to_json f)
         ; Aws.Util.option_map v.policy_name (fun f -> "PolicyName", String.to_json f)
         ])

  let of_json j =
    { policy_name = Aws.Util.option_map (Aws.Json.lookup j "PolicyName") String.of_json
    ; cookie_expiration_period =
        Aws.Util.option_map (Aws.Json.lookup j "CookieExpirationPeriod") Long.of_json
    }
end

module LBCookieStickinessPolicies = struct
  type t = LBCookieStickinessPolicy.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map LBCookieStickinessPolicy.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list LBCookieStickinessPolicy.to_query v

  let to_json v = `List (List.map LBCookieStickinessPolicy.to_json v)

  let of_json j = Aws.Json.to_list LBCookieStickinessPolicy.of_json j
end

module PolicyAttributeDescription = struct
  type t =
    { attribute_name : String.t option
    ; attribute_value : String.t option
    }

  let make ?attribute_name ?attribute_value () = { attribute_name; attribute_value }

  let parse xml =
    Some
      { attribute_name =
          Aws.Util.option_bind (Aws.Xml.member "AttributeName" xml) String.parse
      ; attribute_value =
          Aws.Util.option_bind (Aws.Xml.member "AttributeValue" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.attribute_value (fun f ->
               Aws.Query.Pair ("AttributeValue", String.to_query f))
         ; Aws.Util.option_map v.attribute_name (fun f ->
               Aws.Query.Pair ("AttributeName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.attribute_value (fun f ->
               "AttributeValue", String.to_json f)
         ; Aws.Util.option_map v.attribute_name (fun f ->
               "AttributeName", String.to_json f)
         ])

  let of_json j =
    { attribute_name =
        Aws.Util.option_map (Aws.Json.lookup j "AttributeName") String.of_json
    ; attribute_value =
        Aws.Util.option_map (Aws.Json.lookup j "AttributeValue") String.of_json
    }
end

module PolicyAttributeDescriptions = struct
  type t = PolicyAttributeDescription.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map PolicyAttributeDescription.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list PolicyAttributeDescription.to_query v

  let to_json v = `List (List.map PolicyAttributeDescription.to_json v)

  let of_json j = Aws.Json.to_list PolicyAttributeDescription.of_json j
end

module PolicyDescription = struct
  type t =
    { policy_name : String.t option
    ; policy_type_name : String.t option
    ; policy_attribute_descriptions : PolicyAttributeDescriptions.t
    }

  let make ?policy_name ?policy_type_name ?(policy_attribute_descriptions = []) () =
    { policy_name; policy_type_name; policy_attribute_descriptions }

  let parse xml =
    Some
      { policy_name = Aws.Util.option_bind (Aws.Xml.member "PolicyName" xml) String.parse
      ; policy_type_name =
          Aws.Util.option_bind (Aws.Xml.member "PolicyTypeName" xml) String.parse
      ; policy_attribute_descriptions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "PolicyAttributeDescriptions" xml)
               PolicyAttributeDescriptions.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "PolicyAttributeDescriptions.member"
                , PolicyAttributeDescriptions.to_query v.policy_attribute_descriptions ))
         ; Aws.Util.option_map v.policy_type_name (fun f ->
               Aws.Query.Pair ("PolicyTypeName", String.to_query f))
         ; Aws.Util.option_map v.policy_name (fun f ->
               Aws.Query.Pair ("PolicyName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "PolicyAttributeDescriptions"
             , PolicyAttributeDescriptions.to_json v.policy_attribute_descriptions )
         ; Aws.Util.option_map v.policy_type_name (fun f ->
               "PolicyTypeName", String.to_json f)
         ; Aws.Util.option_map v.policy_name (fun f -> "PolicyName", String.to_json f)
         ])

  let of_json j =
    { policy_name = Aws.Util.option_map (Aws.Json.lookup j "PolicyName") String.of_json
    ; policy_type_name =
        Aws.Util.option_map (Aws.Json.lookup j "PolicyTypeName") String.of_json
    ; policy_attribute_descriptions =
        PolicyAttributeDescriptions.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyAttributeDescriptions"))
    }
end

module CrossZoneLoadBalancing = struct
  type t = { enabled : Boolean.t }

  let make ~enabled () = { enabled }

  let parse xml =
    Some
      { enabled =
          Aws.Xml.required
            "Enabled"
            (Aws.Util.option_bind (Aws.Xml.member "Enabled" xml) Boolean.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Enabled", Boolean.to_query v.enabled)) ])

  let to_json v =
    `Assoc (Aws.Util.list_filter_opt [ Some ("Enabled", Boolean.to_json v.enabled) ])

  let of_json j =
    { enabled = Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Enabled")) }
end

module SourceSecurityGroup = struct
  type t =
    { owner_alias : String.t option
    ; group_name : String.t option
    }

  let make ?owner_alias ?group_name () = { owner_alias; group_name }

  let parse xml =
    Some
      { owner_alias = Aws.Util.option_bind (Aws.Xml.member "OwnerAlias" xml) String.parse
      ; group_name = Aws.Util.option_bind (Aws.Xml.member "GroupName" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.group_name (fun f ->
               Aws.Query.Pair ("GroupName", String.to_query f))
         ; Aws.Util.option_map v.owner_alias (fun f ->
               Aws.Query.Pair ("OwnerAlias", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.group_name (fun f -> "GroupName", String.to_json f)
         ; Aws.Util.option_map v.owner_alias (fun f -> "OwnerAlias", String.to_json f)
         ])

  let of_json j =
    { owner_alias = Aws.Util.option_map (Aws.Json.lookup j "OwnerAlias") String.of_json
    ; group_name = Aws.Util.option_map (Aws.Json.lookup j "GroupName") String.of_json
    }
end

module SecurityGroups = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module PolicyNames = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module AppCookieStickinessPolicy = struct
  type t =
    { policy_name : String.t option
    ; cookie_name : String.t option
    }

  let make ?policy_name ?cookie_name () = { policy_name; cookie_name }

  let parse xml =
    Some
      { policy_name = Aws.Util.option_bind (Aws.Xml.member "PolicyName" xml) String.parse
      ; cookie_name = Aws.Util.option_bind (Aws.Xml.member "CookieName" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cookie_name (fun f ->
               Aws.Query.Pair ("CookieName", String.to_query f))
         ; Aws.Util.option_map v.policy_name (fun f ->
               Aws.Query.Pair ("PolicyName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cookie_name (fun f -> "CookieName", String.to_json f)
         ; Aws.Util.option_map v.policy_name (fun f -> "PolicyName", String.to_json f)
         ])

  let of_json j =
    { policy_name = Aws.Util.option_map (Aws.Json.lookup j "PolicyName") String.of_json
    ; cookie_name = Aws.Util.option_map (Aws.Json.lookup j "CookieName") String.of_json
    }
end

module AppCookieStickinessPolicies = struct
  type t = AppCookieStickinessPolicy.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map AppCookieStickinessPolicy.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list AppCookieStickinessPolicy.to_query v

  let to_json v = `List (List.map AppCookieStickinessPolicy.to_json v)

  let of_json j = Aws.Json.to_list AppCookieStickinessPolicy.of_json j
end

module Policies = struct
  type t =
    { app_cookie_stickiness_policies : AppCookieStickinessPolicies.t
    ; l_b_cookie_stickiness_policies : LBCookieStickinessPolicies.t
    ; other_policies : PolicyNames.t
    }

  let make
      ?(app_cookie_stickiness_policies = [])
      ?(l_b_cookie_stickiness_policies = [])
      ?(other_policies = [])
      () =
    { app_cookie_stickiness_policies; l_b_cookie_stickiness_policies; other_policies }

  let parse xml =
    Some
      { app_cookie_stickiness_policies =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "AppCookieStickinessPolicies" xml)
               AppCookieStickinessPolicies.parse)
      ; l_b_cookie_stickiness_policies =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "LBCookieStickinessPolicies" xml)
               LBCookieStickinessPolicies.parse)
      ; other_policies =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "OtherPolicies" xml) PolicyNames.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("OtherPolicies.member", PolicyNames.to_query v.other_policies))
         ; Some
             (Aws.Query.Pair
                ( "LBCookieStickinessPolicies.member"
                , LBCookieStickinessPolicies.to_query v.l_b_cookie_stickiness_policies ))
         ; Some
             (Aws.Query.Pair
                ( "AppCookieStickinessPolicies.member"
                , AppCookieStickinessPolicies.to_query v.app_cookie_stickiness_policies ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("OtherPolicies", PolicyNames.to_json v.other_policies)
         ; Some
             ( "LBCookieStickinessPolicies"
             , LBCookieStickinessPolicies.to_json v.l_b_cookie_stickiness_policies )
         ; Some
             ( "AppCookieStickinessPolicies"
             , AppCookieStickinessPolicies.to_json v.app_cookie_stickiness_policies )
         ])

  let of_json j =
    { app_cookie_stickiness_policies =
        AppCookieStickinessPolicies.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "AppCookieStickinessPolicies"))
    ; l_b_cookie_stickiness_policies =
        LBCookieStickinessPolicies.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "LBCookieStickinessPolicies"))
    ; other_policies =
        PolicyNames.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "OtherPolicies"))
    }
end

module Listener = struct
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
    { protocol
    ; load_balancer_port
    ; instance_protocol
    ; instance_port
    ; s_s_l_certificate_id
    }

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
         ; Some
             (Aws.Query.Pair ("LoadBalancerPort", Integer.to_query v.load_balancer_port))
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
end

module ListenerDescription = struct
  type t =
    { listener : Listener.t option
    ; policy_names : PolicyNames.t
    }

  let make ?listener ?(policy_names = []) () = { listener; policy_names }

  let parse xml =
    Some
      { listener = Aws.Util.option_bind (Aws.Xml.member "Listener" xml) Listener.parse
      ; policy_names =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "PolicyNames" xml) PolicyNames.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("PolicyNames.member", PolicyNames.to_query v.policy_names))
         ; Aws.Util.option_map v.listener (fun f ->
               Aws.Query.Pair ("Listener", Listener.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("PolicyNames", PolicyNames.to_json v.policy_names)
         ; Aws.Util.option_map v.listener (fun f -> "Listener", Listener.to_json f)
         ])

  let of_json j =
    { listener = Aws.Util.option_map (Aws.Json.lookup j "Listener") Listener.of_json
    ; policy_names =
        PolicyNames.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyNames"))
    }
end

module ListenerDescriptions = struct
  type t = ListenerDescription.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map ListenerDescription.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list ListenerDescription.to_query v

  let to_json v = `List (List.map ListenerDescription.to_json v)

  let of_json j = Aws.Json.to_list ListenerDescription.of_json j
end

module Instance = struct
  type t = { instance_id : String.t option }

  let make ?instance_id () = { instance_id }

  let parse xml =
    Some
      { instance_id = Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.instance_id (fun f ->
               Aws.Query.Pair ("InstanceId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.instance_id (fun f -> "InstanceId", String.to_json f) ])

  let of_json j =
    { instance_id = Aws.Util.option_map (Aws.Json.lookup j "InstanceId") String.of_json }
end

module Instances = struct
  type t = Instance.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map Instance.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list Instance.to_query v

  let to_json v = `List (List.map Instance.to_json v)

  let of_json j = Aws.Json.to_list Instance.of_json j
end

module HealthCheck = struct
  type t =
    { target : String.t
    ; interval : Integer.t
    ; timeout : Integer.t
    ; unhealthy_threshold : Integer.t
    ; healthy_threshold : Integer.t
    }

  let make ~target ~interval ~timeout ~unhealthy_threshold ~healthy_threshold () =
    { target; interval; timeout; unhealthy_threshold; healthy_threshold }

  let parse xml =
    Some
      { target =
          Aws.Xml.required
            "Target"
            (Aws.Util.option_bind (Aws.Xml.member "Target" xml) String.parse)
      ; interval =
          Aws.Xml.required
            "Interval"
            (Aws.Util.option_bind (Aws.Xml.member "Interval" xml) Integer.parse)
      ; timeout =
          Aws.Xml.required
            "Timeout"
            (Aws.Util.option_bind (Aws.Xml.member "Timeout" xml) Integer.parse)
      ; unhealthy_threshold =
          Aws.Xml.required
            "UnhealthyThreshold"
            (Aws.Util.option_bind (Aws.Xml.member "UnhealthyThreshold" xml) Integer.parse)
      ; healthy_threshold =
          Aws.Xml.required
            "HealthyThreshold"
            (Aws.Util.option_bind (Aws.Xml.member "HealthyThreshold" xml) Integer.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("HealthyThreshold", Integer.to_query v.healthy_threshold))
         ; Some
             (Aws.Query.Pair ("UnhealthyThreshold", Integer.to_query v.unhealthy_threshold))
         ; Some (Aws.Query.Pair ("Timeout", Integer.to_query v.timeout))
         ; Some (Aws.Query.Pair ("Interval", Integer.to_query v.interval))
         ; Some (Aws.Query.Pair ("Target", String.to_query v.target))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("HealthyThreshold", Integer.to_json v.healthy_threshold)
         ; Some ("UnhealthyThreshold", Integer.to_json v.unhealthy_threshold)
         ; Some ("Timeout", Integer.to_json v.timeout)
         ; Some ("Interval", Integer.to_json v.interval)
         ; Some ("Target", String.to_json v.target)
         ])

  let of_json j =
    { target = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Target"))
    ; interval = Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Interval"))
    ; timeout = Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Timeout"))
    ; unhealthy_threshold =
        Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UnhealthyThreshold"))
    ; healthy_threshold =
        Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HealthyThreshold"))
    }
end

module BackendServerDescription = struct
  type t =
    { instance_port : Integer.t option
    ; policy_names : PolicyNames.t
    }

  let make ?instance_port ?(policy_names = []) () = { instance_port; policy_names }

  let parse xml =
    Some
      { instance_port =
          Aws.Util.option_bind (Aws.Xml.member "InstancePort" xml) Integer.parse
      ; policy_names =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "PolicyNames" xml) PolicyNames.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("PolicyNames.member", PolicyNames.to_query v.policy_names))
         ; Aws.Util.option_map v.instance_port (fun f ->
               Aws.Query.Pair ("InstancePort", Integer.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("PolicyNames", PolicyNames.to_json v.policy_names)
         ; Aws.Util.option_map v.instance_port (fun f ->
               "InstancePort", Integer.to_json f)
         ])

  let of_json j =
    { instance_port =
        Aws.Util.option_map (Aws.Json.lookup j "InstancePort") Integer.of_json
    ; policy_names =
        PolicyNames.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyNames"))
    }
end

module BackendServerDescriptions = struct
  type t = BackendServerDescription.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map BackendServerDescription.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list BackendServerDescription.to_query v

  let to_json v = `List (List.map BackendServerDescription.to_json v)

  let of_json j = Aws.Json.to_list BackendServerDescription.of_json j
end

module AvailabilityZones = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module LoadBalancerDescription = struct
  type t =
    { load_balancer_name : String.t option
    ; d_n_s_name : String.t option
    ; canonical_hosted_zone_name : String.t option
    ; canonical_hosted_zone_name_i_d : String.t option
    ; listener_descriptions : ListenerDescriptions.t
    ; policies : Policies.t option
    ; backend_server_descriptions : BackendServerDescriptions.t
    ; availability_zones : AvailabilityZones.t
    ; subnets : Subnets.t
    ; v_p_c_id : String.t option
    ; instances : Instances.t
    ; health_check : HealthCheck.t option
    ; source_security_group : SourceSecurityGroup.t option
    ; security_groups : SecurityGroups.t
    ; created_time : DateTime.t option
    ; scheme : String.t option
    }

  let make
      ?load_balancer_name
      ?d_n_s_name
      ?canonical_hosted_zone_name
      ?canonical_hosted_zone_name_i_d
      ?(listener_descriptions = [])
      ?policies
      ?(backend_server_descriptions = [])
      ?(availability_zones = [])
      ?(subnets = [])
      ?v_p_c_id
      ?(instances = [])
      ?health_check
      ?source_security_group
      ?(security_groups = [])
      ?created_time
      ?scheme
      () =
    { load_balancer_name
    ; d_n_s_name
    ; canonical_hosted_zone_name
    ; canonical_hosted_zone_name_i_d
    ; listener_descriptions
    ; policies
    ; backend_server_descriptions
    ; availability_zones
    ; subnets
    ; v_p_c_id
    ; instances
    ; health_check
    ; source_security_group
    ; security_groups
    ; created_time
    ; scheme
    }

  let parse xml =
    Some
      { load_balancer_name =
          Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse
      ; d_n_s_name = Aws.Util.option_bind (Aws.Xml.member "DNSName" xml) String.parse
      ; canonical_hosted_zone_name =
          Aws.Util.option_bind (Aws.Xml.member "CanonicalHostedZoneName" xml) String.parse
      ; canonical_hosted_zone_name_i_d =
          Aws.Util.option_bind
            (Aws.Xml.member "CanonicalHostedZoneNameID" xml)
            String.parse
      ; listener_descriptions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ListenerDescriptions" xml)
               ListenerDescriptions.parse)
      ; policies = Aws.Util.option_bind (Aws.Xml.member "Policies" xml) Policies.parse
      ; backend_server_descriptions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "BackendServerDescriptions" xml)
               BackendServerDescriptions.parse)
      ; availability_zones =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "AvailabilityZones" xml)
               AvailabilityZones.parse)
      ; subnets =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Subnets" xml) Subnets.parse)
      ; v_p_c_id = Aws.Util.option_bind (Aws.Xml.member "VPCId" xml) String.parse
      ; instances =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Instances" xml) Instances.parse)
      ; health_check =
          Aws.Util.option_bind (Aws.Xml.member "HealthCheck" xml) HealthCheck.parse
      ; source_security_group =
          Aws.Util.option_bind
            (Aws.Xml.member "SourceSecurityGroup" xml)
            SourceSecurityGroup.parse
      ; security_groups =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "SecurityGroups" xml)
               SecurityGroups.parse)
      ; created_time =
          Aws.Util.option_bind (Aws.Xml.member "CreatedTime" xml) DateTime.parse
      ; scheme = Aws.Util.option_bind (Aws.Xml.member "Scheme" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.scheme (fun f ->
               Aws.Query.Pair ("Scheme", String.to_query f))
         ; Aws.Util.option_map v.created_time (fun f ->
               Aws.Query.Pair ("CreatedTime", DateTime.to_query f))
         ; Some
             (Aws.Query.Pair
                ("SecurityGroups.member", SecurityGroups.to_query v.security_groups))
         ; Aws.Util.option_map v.source_security_group (fun f ->
               Aws.Query.Pair ("SourceSecurityGroup", SourceSecurityGroup.to_query f))
         ; Aws.Util.option_map v.health_check (fun f ->
               Aws.Query.Pair ("HealthCheck", HealthCheck.to_query f))
         ; Some (Aws.Query.Pair ("Instances.member", Instances.to_query v.instances))
         ; Aws.Util.option_map v.v_p_c_id (fun f ->
               Aws.Query.Pair ("VPCId", String.to_query f))
         ; Some (Aws.Query.Pair ("Subnets.member", Subnets.to_query v.subnets))
         ; Some
             (Aws.Query.Pair
                ( "AvailabilityZones.member"
                , AvailabilityZones.to_query v.availability_zones ))
         ; Some
             (Aws.Query.Pair
                ( "BackendServerDescriptions.member"
                , BackendServerDescriptions.to_query v.backend_server_descriptions ))
         ; Aws.Util.option_map v.policies (fun f ->
               Aws.Query.Pair ("Policies", Policies.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "ListenerDescriptions.member"
                , ListenerDescriptions.to_query v.listener_descriptions ))
         ; Aws.Util.option_map v.canonical_hosted_zone_name_i_d (fun f ->
               Aws.Query.Pair ("CanonicalHostedZoneNameID", String.to_query f))
         ; Aws.Util.option_map v.canonical_hosted_zone_name (fun f ->
               Aws.Query.Pair ("CanonicalHostedZoneName", String.to_query f))
         ; Aws.Util.option_map v.d_n_s_name (fun f ->
               Aws.Query.Pair ("DNSName", String.to_query f))
         ; Aws.Util.option_map v.load_balancer_name (fun f ->
               Aws.Query.Pair ("LoadBalancerName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.scheme (fun f -> "Scheme", String.to_json f)
         ; Aws.Util.option_map v.created_time (fun f -> "CreatedTime", DateTime.to_json f)
         ; Some ("SecurityGroups", SecurityGroups.to_json v.security_groups)
         ; Aws.Util.option_map v.source_security_group (fun f ->
               "SourceSecurityGroup", SourceSecurityGroup.to_json f)
         ; Aws.Util.option_map v.health_check (fun f ->
               "HealthCheck", HealthCheck.to_json f)
         ; Some ("Instances", Instances.to_json v.instances)
         ; Aws.Util.option_map v.v_p_c_id (fun f -> "VPCId", String.to_json f)
         ; Some ("Subnets", Subnets.to_json v.subnets)
         ; Some ("AvailabilityZones", AvailabilityZones.to_json v.availability_zones)
         ; Some
             ( "BackendServerDescriptions"
             , BackendServerDescriptions.to_json v.backend_server_descriptions )
         ; Aws.Util.option_map v.policies (fun f -> "Policies", Policies.to_json f)
         ; Some
             ("ListenerDescriptions", ListenerDescriptions.to_json v.listener_descriptions)
         ; Aws.Util.option_map v.canonical_hosted_zone_name_i_d (fun f ->
               "CanonicalHostedZoneNameID", String.to_json f)
         ; Aws.Util.option_map v.canonical_hosted_zone_name (fun f ->
               "CanonicalHostedZoneName", String.to_json f)
         ; Aws.Util.option_map v.d_n_s_name (fun f -> "DNSName", String.to_json f)
         ; Aws.Util.option_map v.load_balancer_name (fun f ->
               "LoadBalancerName", String.to_json f)
         ])

  let of_json j =
    { load_balancer_name =
        Aws.Util.option_map (Aws.Json.lookup j "LoadBalancerName") String.of_json
    ; d_n_s_name = Aws.Util.option_map (Aws.Json.lookup j "DNSName") String.of_json
    ; canonical_hosted_zone_name =
        Aws.Util.option_map (Aws.Json.lookup j "CanonicalHostedZoneName") String.of_json
    ; canonical_hosted_zone_name_i_d =
        Aws.Util.option_map (Aws.Json.lookup j "CanonicalHostedZoneNameID") String.of_json
    ; listener_descriptions =
        ListenerDescriptions.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ListenerDescriptions"))
    ; policies = Aws.Util.option_map (Aws.Json.lookup j "Policies") Policies.of_json
    ; backend_server_descriptions =
        BackendServerDescriptions.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "BackendServerDescriptions"))
    ; availability_zones =
        AvailabilityZones.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "AvailabilityZones"))
    ; subnets = Subnets.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Subnets"))
    ; v_p_c_id = Aws.Util.option_map (Aws.Json.lookup j "VPCId") String.of_json
    ; instances =
        Instances.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Instances"))
    ; health_check =
        Aws.Util.option_map (Aws.Json.lookup j "HealthCheck") HealthCheck.of_json
    ; source_security_group =
        Aws.Util.option_map
          (Aws.Json.lookup j "SourceSecurityGroup")
          SourceSecurityGroup.of_json
    ; security_groups =
        SecurityGroups.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroups"))
    ; created_time =
        Aws.Util.option_map (Aws.Json.lookup j "CreatedTime") DateTime.of_json
    ; scheme = Aws.Util.option_map (Aws.Json.lookup j "Scheme") String.of_json
    }
end

module LoadBalancerDescriptions = struct
  type t = LoadBalancerDescription.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map LoadBalancerDescription.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list LoadBalancerDescription.to_query v

  let to_json v = `List (List.map LoadBalancerDescription.to_json v)

  let of_json j = Aws.Json.to_list LoadBalancerDescription.of_json j
end

module DescribeAccessPointsOutput = struct
  type t =
    { load_balancer_descriptions : LoadBalancerDescriptions.t
    ; next_marker : String.t option
    }

  let make ?(load_balancer_descriptions = []) ?next_marker () =
    { load_balancer_descriptions; next_marker }

  let parse xml =
    Some
      { load_balancer_descriptions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "LoadBalancerDescriptions" xml)
               LoadBalancerDescriptions.parse)
      ; next_marker = Aws.Util.option_bind (Aws.Xml.member "NextMarker" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_marker (fun f ->
               Aws.Query.Pair ("NextMarker", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "LoadBalancerDescriptions.member"
                , LoadBalancerDescriptions.to_query v.load_balancer_descriptions ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_marker (fun f -> "NextMarker", String.to_json f)
         ; Some
             ( "LoadBalancerDescriptions"
             , LoadBalancerDescriptions.to_json v.load_balancer_descriptions )
         ])

  let of_json j =
    { load_balancer_descriptions =
        LoadBalancerDescriptions.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerDescriptions"))
    ; next_marker = Aws.Util.option_map (Aws.Json.lookup j "NextMarker") String.of_json
    }
end

module DeleteAccessPointInput = struct
  type t = { load_balancer_name : String.t }

  let make ~load_balancer_name () = { load_balancer_name }

  let parse xml =
    Some
      { load_balancer_name =
          Aws.Xml.required
            "LoadBalancerName"
            (Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("LoadBalancerName", String.to_query v.load_balancer_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("LoadBalancerName", String.to_json v.load_balancer_name) ])

  let of_json j =
    { load_balancer_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerName"))
    }
end

module PolicyAttributeTypeDescription = struct
  type t =
    { attribute_name : String.t option
    ; attribute_type : String.t option
    ; description : String.t option
    ; default_value : String.t option
    ; cardinality : String.t option
    }

  let make ?attribute_name ?attribute_type ?description ?default_value ?cardinality () =
    { attribute_name; attribute_type; description; default_value; cardinality }

  let parse xml =
    Some
      { attribute_name =
          Aws.Util.option_bind (Aws.Xml.member "AttributeName" xml) String.parse
      ; attribute_type =
          Aws.Util.option_bind (Aws.Xml.member "AttributeType" xml) String.parse
      ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
      ; default_value =
          Aws.Util.option_bind (Aws.Xml.member "DefaultValue" xml) String.parse
      ; cardinality = Aws.Util.option_bind (Aws.Xml.member "Cardinality" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cardinality (fun f ->
               Aws.Query.Pair ("Cardinality", String.to_query f))
         ; Aws.Util.option_map v.default_value (fun f ->
               Aws.Query.Pair ("DefaultValue", String.to_query f))
         ; Aws.Util.option_map v.description (fun f ->
               Aws.Query.Pair ("Description", String.to_query f))
         ; Aws.Util.option_map v.attribute_type (fun f ->
               Aws.Query.Pair ("AttributeType", String.to_query f))
         ; Aws.Util.option_map v.attribute_name (fun f ->
               Aws.Query.Pair ("AttributeName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cardinality (fun f -> "Cardinality", String.to_json f)
         ; Aws.Util.option_map v.default_value (fun f -> "DefaultValue", String.to_json f)
         ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
         ; Aws.Util.option_map v.attribute_type (fun f ->
               "AttributeType", String.to_json f)
         ; Aws.Util.option_map v.attribute_name (fun f ->
               "AttributeName", String.to_json f)
         ])

  let of_json j =
    { attribute_name =
        Aws.Util.option_map (Aws.Json.lookup j "AttributeName") String.of_json
    ; attribute_type =
        Aws.Util.option_map (Aws.Json.lookup j "AttributeType") String.of_json
    ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
    ; default_value =
        Aws.Util.option_map (Aws.Json.lookup j "DefaultValue") String.of_json
    ; cardinality = Aws.Util.option_map (Aws.Json.lookup j "Cardinality") String.of_json
    }
end

module PolicyAttributeTypeDescriptions = struct
  type t = PolicyAttributeTypeDescription.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map PolicyAttributeTypeDescription.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list PolicyAttributeTypeDescription.to_query v

  let to_json v = `List (List.map PolicyAttributeTypeDescription.to_json v)

  let of_json j = Aws.Json.to_list PolicyAttributeTypeDescription.of_json j
end

module PolicyTypeDescription = struct
  type t =
    { policy_type_name : String.t option
    ; description : String.t option
    ; policy_attribute_type_descriptions : PolicyAttributeTypeDescriptions.t
    }

  let make ?policy_type_name ?description ?(policy_attribute_type_descriptions = []) () =
    { policy_type_name; description; policy_attribute_type_descriptions }

  let parse xml =
    Some
      { policy_type_name =
          Aws.Util.option_bind (Aws.Xml.member "PolicyTypeName" xml) String.parse
      ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
      ; policy_attribute_type_descriptions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "PolicyAttributeTypeDescriptions" xml)
               PolicyAttributeTypeDescriptions.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "PolicyAttributeTypeDescriptions.member"
                , PolicyAttributeTypeDescriptions.to_query
                    v.policy_attribute_type_descriptions ))
         ; Aws.Util.option_map v.description (fun f ->
               Aws.Query.Pair ("Description", String.to_query f))
         ; Aws.Util.option_map v.policy_type_name (fun f ->
               Aws.Query.Pair ("PolicyTypeName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "PolicyAttributeTypeDescriptions"
             , PolicyAttributeTypeDescriptions.to_json
                 v.policy_attribute_type_descriptions )
         ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
         ; Aws.Util.option_map v.policy_type_name (fun f ->
               "PolicyTypeName", String.to_json f)
         ])

  let of_json j =
    { policy_type_name =
        Aws.Util.option_map (Aws.Json.lookup j "PolicyTypeName") String.of_json
    ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
    ; policy_attribute_type_descriptions =
        PolicyAttributeTypeDescriptions.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyAttributeTypeDescriptions"))
    }
end

module PolicyTypeDescriptions = struct
  type t = PolicyTypeDescription.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map PolicyTypeDescription.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list PolicyTypeDescription.to_query v

  let to_json v = `List (List.map PolicyTypeDescription.to_json v)

  let of_json j = Aws.Json.to_list PolicyTypeDescription.of_json j
end

module DescribeLoadBalancerPolicyTypesOutput = struct
  type t = { policy_type_descriptions : PolicyTypeDescriptions.t }

  let make ?(policy_type_descriptions = []) () = { policy_type_descriptions }

  let parse xml =
    Some
      { policy_type_descriptions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "PolicyTypeDescriptions" xml)
               PolicyTypeDescriptions.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "PolicyTypeDescriptions.member"
                , PolicyTypeDescriptions.to_query v.policy_type_descriptions ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "PolicyTypeDescriptions"
             , PolicyTypeDescriptions.to_json v.policy_type_descriptions )
         ])

  let of_json j =
    { policy_type_descriptions =
        PolicyTypeDescriptions.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyTypeDescriptions"))
    }
end

module AddAvailabilityZonesInput = struct
  type t =
    { load_balancer_name : String.t
    ; availability_zones : AvailabilityZones.t
    }

  let make ~load_balancer_name ~availability_zones () =
    { load_balancer_name; availability_zones }

  let parse xml =
    Some
      { load_balancer_name =
          Aws.Xml.required
            "LoadBalancerName"
            (Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse)
      ; availability_zones =
          Aws.Xml.required
            "AvailabilityZones"
            (Aws.Util.option_bind
               (Aws.Xml.member "AvailabilityZones" xml)
               AvailabilityZones.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "AvailabilityZones.member"
                , AvailabilityZones.to_query v.availability_zones ))
         ; Some
             (Aws.Query.Pair ("LoadBalancerName", String.to_query v.load_balancer_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("AvailabilityZones", AvailabilityZones.to_json v.availability_zones)
         ; Some ("LoadBalancerName", String.to_json v.load_balancer_name)
         ])

  let of_json j =
    { load_balancer_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerName"))
    ; availability_zones =
        AvailabilityZones.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "AvailabilityZones"))
    }
end

module RegisterEndPointsOutput = struct
  type t = { instances : Instances.t }

  let make ?(instances = []) () = { instances }

  let parse xml =
    Some
      { instances =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Instances" xml) Instances.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Instances.member", Instances.to_query v.instances)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt [ Some ("Instances", Instances.to_json v.instances) ])

  let of_json j =
    { instances =
        Instances.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Instances"))
    }
end

module Ports = struct
  type t = Integer.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map Integer.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list Integer.to_query v

  let to_json v = `List (List.map Integer.to_json v)

  let of_json j = Aws.Json.to_list Integer.of_json j
end

module DeleteLoadBalancerListenerInput = struct
  type t =
    { load_balancer_name : String.t
    ; load_balancer_ports : Ports.t
    }

  let make ~load_balancer_name ~load_balancer_ports () =
    { load_balancer_name; load_balancer_ports }

  let parse xml =
    Some
      { load_balancer_name =
          Aws.Xml.required
            "LoadBalancerName"
            (Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse)
      ; load_balancer_ports =
          Aws.Xml.required
            "LoadBalancerPorts"
            (Aws.Util.option_bind (Aws.Xml.member "LoadBalancerPorts" xml) Ports.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("LoadBalancerPorts.member", Ports.to_query v.load_balancer_ports))
         ; Some
             (Aws.Query.Pair ("LoadBalancerName", String.to_query v.load_balancer_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("LoadBalancerPorts", Ports.to_json v.load_balancer_ports)
         ; Some ("LoadBalancerName", String.to_json v.load_balancer_name)
         ])

  let of_json j =
    { load_balancer_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerName"))
    ; load_balancer_ports =
        Ports.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerPorts"))
    }
end

module RegisterEndPointsInput = struct
  type t =
    { load_balancer_name : String.t
    ; instances : Instances.t
    }

  let make ~load_balancer_name ~instances () = { load_balancer_name; instances }

  let parse xml =
    Some
      { load_balancer_name =
          Aws.Xml.required
            "LoadBalancerName"
            (Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse)
      ; instances =
          Aws.Xml.required
            "Instances"
            (Aws.Util.option_bind (Aws.Xml.member "Instances" xml) Instances.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Instances.member", Instances.to_query v.instances))
         ; Some
             (Aws.Query.Pair ("LoadBalancerName", String.to_query v.load_balancer_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Instances", Instances.to_json v.instances)
         ; Some ("LoadBalancerName", String.to_json v.load_balancer_name)
         ])

  let of_json j =
    { load_balancer_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerName"))
    ; instances =
        Instances.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Instances"))
    }
end

module CreateLBCookieStickinessPolicyOutput = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module SetLoadBalancerListenerSSLCertificateInput = struct
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
         ; Some
             (Aws.Query.Pair ("LoadBalancerPort", Integer.to_query v.load_balancer_port))
         ; Some
             (Aws.Query.Pair ("LoadBalancerName", String.to_query v.load_balancer_name))
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
end

module ConnectionSettings = struct
  type t = { idle_timeout : Integer.t }

  let make ~idle_timeout () = { idle_timeout }

  let parse xml =
    Some
      { idle_timeout =
          Aws.Xml.required
            "IdleTimeout"
            (Aws.Util.option_bind (Aws.Xml.member "IdleTimeout" xml) Integer.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("IdleTimeout", Integer.to_query v.idle_timeout)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt [ Some ("IdleTimeout", Integer.to_json v.idle_timeout) ])

  let of_json j =
    { idle_timeout =
        Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IdleTimeout"))
    }
end

module ConnectionDraining = struct
  type t =
    { enabled : Boolean.t
    ; timeout : Integer.t option
    }

  let make ~enabled ?timeout () = { enabled; timeout }

  let parse xml =
    Some
      { enabled =
          Aws.Xml.required
            "Enabled"
            (Aws.Util.option_bind (Aws.Xml.member "Enabled" xml) Boolean.parse)
      ; timeout = Aws.Util.option_bind (Aws.Xml.member "Timeout" xml) Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.timeout (fun f ->
               Aws.Query.Pair ("Timeout", Integer.to_query f))
         ; Some (Aws.Query.Pair ("Enabled", Boolean.to_query v.enabled))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.timeout (fun f -> "Timeout", Integer.to_json f)
         ; Some ("Enabled", Boolean.to_json v.enabled)
         ])

  let of_json j =
    { enabled = Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Enabled"))
    ; timeout = Aws.Util.option_map (Aws.Json.lookup j "Timeout") Integer.of_json
    }
end

module AdditionalAttribute = struct
  type t =
    { key : String.t option
    ; value : String.t option
    }

  let make ?key ?value () = { key; value }

  let parse xml =
    Some
      { key = Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse
      ; value = Aws.Util.option_bind (Aws.Xml.member "Value" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.value (fun f ->
               Aws.Query.Pair ("Value", String.to_query f))
         ; Aws.Util.option_map v.key (fun f -> Aws.Query.Pair ("Key", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.value (fun f -> "Value", String.to_json f)
         ; Aws.Util.option_map v.key (fun f -> "Key", String.to_json f)
         ])

  let of_json j =
    { key = Aws.Util.option_map (Aws.Json.lookup j "Key") String.of_json
    ; value = Aws.Util.option_map (Aws.Json.lookup j "Value") String.of_json
    }
end

module AdditionalAttributes = struct
  type t = AdditionalAttribute.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map AdditionalAttribute.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list AdditionalAttribute.to_query v

  let to_json v = `List (List.map AdditionalAttribute.to_json v)

  let of_json j = Aws.Json.to_list AdditionalAttribute.of_json j
end

module AccessLog = struct
  type t =
    { enabled : Boolean.t
    ; s3_bucket_name : String.t option
    ; emit_interval : Integer.t option
    ; s3_bucket_prefix : String.t option
    }

  let make ~enabled ?s3_bucket_name ?emit_interval ?s3_bucket_prefix () =
    { enabled; s3_bucket_name; emit_interval; s3_bucket_prefix }

  let parse xml =
    Some
      { enabled =
          Aws.Xml.required
            "Enabled"
            (Aws.Util.option_bind (Aws.Xml.member "Enabled" xml) Boolean.parse)
      ; s3_bucket_name =
          Aws.Util.option_bind (Aws.Xml.member "S3BucketName" xml) String.parse
      ; emit_interval =
          Aws.Util.option_bind (Aws.Xml.member "EmitInterval" xml) Integer.parse
      ; s3_bucket_prefix =
          Aws.Util.option_bind (Aws.Xml.member "S3BucketPrefix" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.s3_bucket_prefix (fun f ->
               Aws.Query.Pair ("S3BucketPrefix", String.to_query f))
         ; Aws.Util.option_map v.emit_interval (fun f ->
               Aws.Query.Pair ("EmitInterval", Integer.to_query f))
         ; Aws.Util.option_map v.s3_bucket_name (fun f ->
               Aws.Query.Pair ("S3BucketName", String.to_query f))
         ; Some (Aws.Query.Pair ("Enabled", Boolean.to_query v.enabled))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.s3_bucket_prefix (fun f ->
               "S3BucketPrefix", String.to_json f)
         ; Aws.Util.option_map v.emit_interval (fun f ->
               "EmitInterval", Integer.to_json f)
         ; Aws.Util.option_map v.s3_bucket_name (fun f ->
               "S3BucketName", String.to_json f)
         ; Some ("Enabled", Boolean.to_json v.enabled)
         ])

  let of_json j =
    { enabled = Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Enabled"))
    ; s3_bucket_name =
        Aws.Util.option_map (Aws.Json.lookup j "S3BucketName") String.of_json
    ; emit_interval =
        Aws.Util.option_map (Aws.Json.lookup j "EmitInterval") Integer.of_json
    ; s3_bucket_prefix =
        Aws.Util.option_map (Aws.Json.lookup j "S3BucketPrefix") String.of_json
    }
end

module LoadBalancerAttributes = struct
  type t =
    { cross_zone_load_balancing : CrossZoneLoadBalancing.t option
    ; access_log : AccessLog.t option
    ; connection_draining : ConnectionDraining.t option
    ; connection_settings : ConnectionSettings.t option
    ; additional_attributes : AdditionalAttributes.t
    }

  let make
      ?cross_zone_load_balancing
      ?access_log
      ?connection_draining
      ?connection_settings
      ?(additional_attributes = [])
      () =
    { cross_zone_load_balancing
    ; access_log
    ; connection_draining
    ; connection_settings
    ; additional_attributes
    }

  let parse xml =
    Some
      { cross_zone_load_balancing =
          Aws.Util.option_bind
            (Aws.Xml.member "CrossZoneLoadBalancing" xml)
            CrossZoneLoadBalancing.parse
      ; access_log = Aws.Util.option_bind (Aws.Xml.member "AccessLog" xml) AccessLog.parse
      ; connection_draining =
          Aws.Util.option_bind
            (Aws.Xml.member "ConnectionDraining" xml)
            ConnectionDraining.parse
      ; connection_settings =
          Aws.Util.option_bind
            (Aws.Xml.member "ConnectionSettings" xml)
            ConnectionSettings.parse
      ; additional_attributes =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "AdditionalAttributes" xml)
               AdditionalAttributes.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "AdditionalAttributes.member"
                , AdditionalAttributes.to_query v.additional_attributes ))
         ; Aws.Util.option_map v.connection_settings (fun f ->
               Aws.Query.Pair ("ConnectionSettings", ConnectionSettings.to_query f))
         ; Aws.Util.option_map v.connection_draining (fun f ->
               Aws.Query.Pair ("ConnectionDraining", ConnectionDraining.to_query f))
         ; Aws.Util.option_map v.access_log (fun f ->
               Aws.Query.Pair ("AccessLog", AccessLog.to_query f))
         ; Aws.Util.option_map v.cross_zone_load_balancing (fun f ->
               Aws.Query.Pair ("CrossZoneLoadBalancing", CrossZoneLoadBalancing.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ("AdditionalAttributes", AdditionalAttributes.to_json v.additional_attributes)
         ; Aws.Util.option_map v.connection_settings (fun f ->
               "ConnectionSettings", ConnectionSettings.to_json f)
         ; Aws.Util.option_map v.connection_draining (fun f ->
               "ConnectionDraining", ConnectionDraining.to_json f)
         ; Aws.Util.option_map v.access_log (fun f -> "AccessLog", AccessLog.to_json f)
         ; Aws.Util.option_map v.cross_zone_load_balancing (fun f ->
               "CrossZoneLoadBalancing", CrossZoneLoadBalancing.to_json f)
         ])

  let of_json j =
    { cross_zone_load_balancing =
        Aws.Util.option_map
          (Aws.Json.lookup j "CrossZoneLoadBalancing")
          CrossZoneLoadBalancing.of_json
    ; access_log = Aws.Util.option_map (Aws.Json.lookup j "AccessLog") AccessLog.of_json
    ; connection_draining =
        Aws.Util.option_map
          (Aws.Json.lookup j "ConnectionDraining")
          ConnectionDraining.of_json
    ; connection_settings =
        Aws.Util.option_map
          (Aws.Json.lookup j "ConnectionSettings")
          ConnectionSettings.of_json
    ; additional_attributes =
        AdditionalAttributes.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "AdditionalAttributes"))
    }
end

module DescribeLoadBalancerAttributesOutput = struct
  type t = { load_balancer_attributes : LoadBalancerAttributes.t option }

  let make ?load_balancer_attributes () = { load_balancer_attributes }

  let parse xml =
    Some
      { load_balancer_attributes =
          Aws.Util.option_bind
            (Aws.Xml.member "LoadBalancerAttributes" xml)
            LoadBalancerAttributes.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.load_balancer_attributes (fun f ->
               Aws.Query.Pair ("LoadBalancerAttributes", LoadBalancerAttributes.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.load_balancer_attributes (fun f ->
               "LoadBalancerAttributes", LoadBalancerAttributes.to_json f)
         ])

  let of_json j =
    { load_balancer_attributes =
        Aws.Util.option_map
          (Aws.Json.lookup j "LoadBalancerAttributes")
          LoadBalancerAttributes.of_json
    }
end

module DeregisterEndPointsOutput = struct
  type t = { instances : Instances.t }

  let make ?(instances = []) () = { instances }

  let parse xml =
    Some
      { instances =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Instances" xml) Instances.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Instances.member", Instances.to_query v.instances)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt [ Some ("Instances", Instances.to_json v.instances) ])

  let of_json j =
    { instances =
        Instances.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Instances"))
    }
end

module PolicyTypeNames = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module DescribeLoadBalancerPolicyTypesInput = struct
  type t = { policy_type_names : PolicyTypeNames.t }

  let make ?(policy_type_names = []) () = { policy_type_names }

  let parse xml =
    Some
      { policy_type_names =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "PolicyTypeNames" xml)
               PolicyTypeNames.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("PolicyTypeNames.member", PolicyTypeNames.to_query v.policy_type_names))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("PolicyTypeNames", PolicyTypeNames.to_json v.policy_type_names) ])

  let of_json j =
    { policy_type_names =
        PolicyTypeNames.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyTypeNames"))
    }
end

module DescribeEndPointStateInput = struct
  type t =
    { load_balancer_name : String.t
    ; instances : Instances.t
    }

  let make ~load_balancer_name ?(instances = []) () = { load_balancer_name; instances }

  let parse xml =
    Some
      { load_balancer_name =
          Aws.Xml.required
            "LoadBalancerName"
            (Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse)
      ; instances =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Instances" xml) Instances.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Instances.member", Instances.to_query v.instances))
         ; Some
             (Aws.Query.Pair ("LoadBalancerName", String.to_query v.load_balancer_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Instances", Instances.to_json v.instances)
         ; Some ("LoadBalancerName", String.to_json v.load_balancer_name)
         ])

  let of_json j =
    { load_balancer_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerName"))
    ; instances =
        Instances.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Instances"))
    }
end

module DuplicateAccessPointNameException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ConfigureHealthCheckInput = struct
  type t =
    { load_balancer_name : String.t
    ; health_check : HealthCheck.t
    }

  let make ~load_balancer_name ~health_check () = { load_balancer_name; health_check }

  let parse xml =
    Some
      { load_balancer_name =
          Aws.Xml.required
            "LoadBalancerName"
            (Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse)
      ; health_check =
          Aws.Xml.required
            "HealthCheck"
            (Aws.Util.option_bind (Aws.Xml.member "HealthCheck" xml) HealthCheck.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("HealthCheck", HealthCheck.to_query v.health_check))
         ; Some
             (Aws.Query.Pair ("LoadBalancerName", String.to_query v.load_balancer_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("HealthCheck", HealthCheck.to_json v.health_check)
         ; Some ("LoadBalancerName", String.to_json v.load_balancer_name)
         ])

  let of_json j =
    { load_balancer_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerName"))
    ; health_check =
        HealthCheck.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "HealthCheck"))
    }
end

module ApplySecurityGroupsToLoadBalancerOutput = struct
  type t = { security_groups : SecurityGroups.t }

  let make ?(security_groups = []) () = { security_groups }

  let parse xml =
    Some
      { security_groups =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "SecurityGroups" xml)
               SecurityGroups.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("SecurityGroups.member", SecurityGroups.to_query v.security_groups))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("SecurityGroups", SecurityGroups.to_json v.security_groups) ])

  let of_json j =
    { security_groups =
        SecurityGroups.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroups"))
    }
end

module CreateLBCookieStickinessPolicyInput = struct
  type t =
    { load_balancer_name : String.t
    ; policy_name : String.t
    ; cookie_expiration_period : Long.t option
    }

  let make ~load_balancer_name ~policy_name ?cookie_expiration_period () =
    { load_balancer_name; policy_name; cookie_expiration_period }

  let parse xml =
    Some
      { load_balancer_name =
          Aws.Xml.required
            "LoadBalancerName"
            (Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse)
      ; policy_name =
          Aws.Xml.required
            "PolicyName"
            (Aws.Util.option_bind (Aws.Xml.member "PolicyName" xml) String.parse)
      ; cookie_expiration_period =
          Aws.Util.option_bind (Aws.Xml.member "CookieExpirationPeriod" xml) Long.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cookie_expiration_period (fun f ->
               Aws.Query.Pair ("CookieExpirationPeriod", Long.to_query f))
         ; Some (Aws.Query.Pair ("PolicyName", String.to_query v.policy_name))
         ; Some
             (Aws.Query.Pair ("LoadBalancerName", String.to_query v.load_balancer_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cookie_expiration_period (fun f ->
               "CookieExpirationPeriod", Long.to_json f)
         ; Some ("PolicyName", String.to_json v.policy_name)
         ; Some ("LoadBalancerName", String.to_json v.load_balancer_name)
         ])

  let of_json j =
    { load_balancer_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerName"))
    ; policy_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyName"))
    ; cookie_expiration_period =
        Aws.Util.option_map (Aws.Json.lookup j "CookieExpirationPeriod") Long.of_json
    }
end

module ModifyLoadBalancerAttributesInput = struct
  type t =
    { load_balancer_name : String.t
    ; load_balancer_attributes : LoadBalancerAttributes.t
    }

  let make ~load_balancer_name ~load_balancer_attributes () =
    { load_balancer_name; load_balancer_attributes }

  let parse xml =
    Some
      { load_balancer_name =
          Aws.Xml.required
            "LoadBalancerName"
            (Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse)
      ; load_balancer_attributes =
          Aws.Xml.required
            "LoadBalancerAttributes"
            (Aws.Util.option_bind
               (Aws.Xml.member "LoadBalancerAttributes" xml)
               LoadBalancerAttributes.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "LoadBalancerAttributes"
                , LoadBalancerAttributes.to_query v.load_balancer_attributes ))
         ; Some
             (Aws.Query.Pair ("LoadBalancerName", String.to_query v.load_balancer_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "LoadBalancerAttributes"
             , LoadBalancerAttributes.to_json v.load_balancer_attributes )
         ; Some ("LoadBalancerName", String.to_json v.load_balancer_name)
         ])

  let of_json j =
    { load_balancer_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerName"))
    ; load_balancer_attributes =
        LoadBalancerAttributes.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerAttributes"))
    }
end

module TooManyPoliciesException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module DeleteLoadBalancerListenerOutput = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module DuplicatePolicyNameException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module DeleteLoadBalancerPolicyOutput = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module LoadBalancerNamesMax20 = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module DescribeTagsInput = struct
  type t = { load_balancer_names : LoadBalancerNamesMax20.t }

  let make ~load_balancer_names () = { load_balancer_names }

  let parse xml =
    Some
      { load_balancer_names =
          Aws.Xml.required
            "LoadBalancerNames"
            (Aws.Util.option_bind
               (Aws.Xml.member "LoadBalancerNames" xml)
               LoadBalancerNamesMax20.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "LoadBalancerNames.member"
                , LoadBalancerNamesMax20.to_query v.load_balancer_names ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("LoadBalancerNames", LoadBalancerNamesMax20.to_json v.load_balancer_names)
         ])

  let of_json j =
    { load_balancer_names =
        LoadBalancerNamesMax20.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerNames"))
    }
end

module CreateAppCookieStickinessPolicyInput = struct
  type t =
    { load_balancer_name : String.t
    ; policy_name : String.t
    ; cookie_name : String.t
    }

  let make ~load_balancer_name ~policy_name ~cookie_name () =
    { load_balancer_name; policy_name; cookie_name }

  let parse xml =
    Some
      { load_balancer_name =
          Aws.Xml.required
            "LoadBalancerName"
            (Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse)
      ; policy_name =
          Aws.Xml.required
            "PolicyName"
            (Aws.Util.option_bind (Aws.Xml.member "PolicyName" xml) String.parse)
      ; cookie_name =
          Aws.Xml.required
            "CookieName"
            (Aws.Util.option_bind (Aws.Xml.member "CookieName" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("CookieName", String.to_query v.cookie_name))
         ; Some (Aws.Query.Pair ("PolicyName", String.to_query v.policy_name))
         ; Some
             (Aws.Query.Pair ("LoadBalancerName", String.to_query v.load_balancer_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("CookieName", String.to_json v.cookie_name)
         ; Some ("PolicyName", String.to_json v.policy_name)
         ; Some ("LoadBalancerName", String.to_json v.load_balancer_name)
         ])

  let of_json j =
    { load_balancer_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerName"))
    ; policy_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyName"))
    ; cookie_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CookieName"))
    }
end

module SetLoadBalancerPoliciesOfListenerInput = struct
  type t =
    { load_balancer_name : String.t
    ; load_balancer_port : Integer.t
    ; policy_names : PolicyNames.t
    }

  let make ~load_balancer_name ~load_balancer_port ~policy_names () =
    { load_balancer_name; load_balancer_port; policy_names }

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
      ; policy_names =
          Aws.Xml.required
            "PolicyNames"
            (Aws.Util.option_bind (Aws.Xml.member "PolicyNames" xml) PolicyNames.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("PolicyNames.member", PolicyNames.to_query v.policy_names))
         ; Some
             (Aws.Query.Pair ("LoadBalancerPort", Integer.to_query v.load_balancer_port))
         ; Some
             (Aws.Query.Pair ("LoadBalancerName", String.to_query v.load_balancer_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("PolicyNames", PolicyNames.to_json v.policy_names)
         ; Some ("LoadBalancerPort", Integer.to_json v.load_balancer_port)
         ; Some ("LoadBalancerName", String.to_json v.load_balancer_name)
         ])

  let of_json j =
    { load_balancer_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerName"))
    ; load_balancer_port =
        Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerPort"))
    ; policy_names =
        PolicyNames.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyNames"))
    }
end

module ApplySecurityGroupsToLoadBalancerInput = struct
  type t =
    { load_balancer_name : String.t
    ; security_groups : SecurityGroups.t
    }

  let make ~load_balancer_name ~security_groups () =
    { load_balancer_name; security_groups }

  let parse xml =
    Some
      { load_balancer_name =
          Aws.Xml.required
            "LoadBalancerName"
            (Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse)
      ; security_groups =
          Aws.Xml.required
            "SecurityGroups"
            (Aws.Util.option_bind
               (Aws.Xml.member "SecurityGroups" xml)
               SecurityGroups.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("SecurityGroups.member", SecurityGroups.to_query v.security_groups))
         ; Some
             (Aws.Query.Pair ("LoadBalancerName", String.to_query v.load_balancer_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("SecurityGroups", SecurityGroups.to_json v.security_groups)
         ; Some ("LoadBalancerName", String.to_json v.load_balancer_name)
         ])

  let of_json j =
    { load_balancer_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerName"))
    ; security_groups =
        SecurityGroups.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroups"))
    }
end

module RemoveAvailabilityZonesOutput = struct
  type t = { availability_zones : AvailabilityZones.t }

  let make ?(availability_zones = []) () = { availability_zones }

  let parse xml =
    Some
      { availability_zones =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "AvailabilityZones" xml)
               AvailabilityZones.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "AvailabilityZones.member"
                , AvailabilityZones.to_query v.availability_zones ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("AvailabilityZones", AvailabilityZones.to_json v.availability_zones) ])

  let of_json j =
    { availability_zones =
        AvailabilityZones.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "AvailabilityZones"))
    }
end

module Listeners = struct
  type t = Listener.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map Listener.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list Listener.to_query v

  let to_json v = `List (List.map Listener.to_json v)

  let of_json j = Aws.Json.to_list Listener.of_json j
end

module Tag = struct
  type t =
    { key : String.t
    ; value : String.t option
    }

  let make ~key ?value () = { key; value }

  let parse xml =
    Some
      { key =
          Aws.Xml.required
            "Key"
            (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse)
      ; value = Aws.Util.option_bind (Aws.Xml.member "Value" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.value (fun f ->
               Aws.Query.Pair ("Value", String.to_query f))
         ; Some (Aws.Query.Pair ("Key", String.to_query v.key))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.value (fun f -> "Value", String.to_json f)
         ; Some ("Key", String.to_json v.key)
         ])

  let of_json j =
    { key = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key"))
    ; value = Aws.Util.option_map (Aws.Json.lookup j "Value") String.of_json
    }
end

module TagList = struct
  type t = Tag.t list

  let make elems () = elems

  let parse xml = Aws.Util.option_all (List.map Tag.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list Tag.to_query v

  let to_json v = `List (List.map Tag.to_json v)

  let of_json j = Aws.Json.to_list Tag.of_json j
end

module RemoveTagsOutput = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module DeleteAccessPointOutput = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module AddTagsOutput = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module InstanceState = struct
  type t =
    { instance_id : String.t option
    ; state : String.t option
    ; reason_code : String.t option
    ; description : String.t option
    }

  let make ?instance_id ?state ?reason_code ?description () =
    { instance_id; state; reason_code; description }

  let parse xml =
    Some
      { instance_id = Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse
      ; state = Aws.Util.option_bind (Aws.Xml.member "State" xml) String.parse
      ; reason_code = Aws.Util.option_bind (Aws.Xml.member "ReasonCode" xml) String.parse
      ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.description (fun f ->
               Aws.Query.Pair ("Description", String.to_query f))
         ; Aws.Util.option_map v.reason_code (fun f ->
               Aws.Query.Pair ("ReasonCode", String.to_query f))
         ; Aws.Util.option_map v.state (fun f ->
               Aws.Query.Pair ("State", String.to_query f))
         ; Aws.Util.option_map v.instance_id (fun f ->
               Aws.Query.Pair ("InstanceId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
         ; Aws.Util.option_map v.reason_code (fun f -> "ReasonCode", String.to_json f)
         ; Aws.Util.option_map v.state (fun f -> "State", String.to_json f)
         ; Aws.Util.option_map v.instance_id (fun f -> "InstanceId", String.to_json f)
         ])

  let of_json j =
    { instance_id = Aws.Util.option_map (Aws.Json.lookup j "InstanceId") String.of_json
    ; state = Aws.Util.option_map (Aws.Json.lookup j "State") String.of_json
    ; reason_code = Aws.Util.option_map (Aws.Json.lookup j "ReasonCode") String.of_json
    ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
    }
end

module InstanceStates = struct
  type t = InstanceState.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map InstanceState.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list InstanceState.to_query v

  let to_json v = `List (List.map InstanceState.to_json v)

  let of_json j = Aws.Json.to_list InstanceState.of_json j
end

module DescribeEndPointStateOutput = struct
  type t = { instance_states : InstanceStates.t }

  let make ?(instance_states = []) () = { instance_states }

  let parse xml =
    Some
      { instance_states =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "InstanceStates" xml)
               InstanceStates.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("InstanceStates.member", InstanceStates.to_query v.instance_states))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("InstanceStates", InstanceStates.to_json v.instance_states) ])

  let of_json j =
    { instance_states =
        InstanceStates.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "InstanceStates"))
    }
end

module CreateAppCookieStickinessPolicyOutput = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module AttachLoadBalancerToSubnetsOutput = struct
  type t = { subnets : Subnets.t }

  let make ?(subnets = []) () = { subnets }

  let parse xml =
    Some
      { subnets =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Subnets" xml) Subnets.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Subnets.member", Subnets.to_query v.subnets)) ])

  let to_json v =
    `Assoc (Aws.Util.list_filter_opt [ Some ("Subnets", Subnets.to_json v.subnets) ])

  let of_json j =
    { subnets = Subnets.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Subnets")) }
end

module DeleteLoadBalancerPolicyInput = struct
  type t =
    { load_balancer_name : String.t
    ; policy_name : String.t
    }

  let make ~load_balancer_name ~policy_name () = { load_balancer_name; policy_name }

  let parse xml =
    Some
      { load_balancer_name =
          Aws.Xml.required
            "LoadBalancerName"
            (Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse)
      ; policy_name =
          Aws.Xml.required
            "PolicyName"
            (Aws.Util.option_bind (Aws.Xml.member "PolicyName" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("PolicyName", String.to_query v.policy_name))
         ; Some
             (Aws.Query.Pair ("LoadBalancerName", String.to_query v.load_balancer_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("PolicyName", String.to_json v.policy_name)
         ; Some ("LoadBalancerName", String.to_json v.load_balancer_name)
         ])

  let of_json j =
    { load_balancer_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerName"))
    ; policy_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyName"))
    }
end

module PolicyAttribute = struct
  type t =
    { attribute_name : String.t option
    ; attribute_value : String.t option
    }

  let make ?attribute_name ?attribute_value () = { attribute_name; attribute_value }

  let parse xml =
    Some
      { attribute_name =
          Aws.Util.option_bind (Aws.Xml.member "AttributeName" xml) String.parse
      ; attribute_value =
          Aws.Util.option_bind (Aws.Xml.member "AttributeValue" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.attribute_value (fun f ->
               Aws.Query.Pair ("AttributeValue", String.to_query f))
         ; Aws.Util.option_map v.attribute_name (fun f ->
               Aws.Query.Pair ("AttributeName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.attribute_value (fun f ->
               "AttributeValue", String.to_json f)
         ; Aws.Util.option_map v.attribute_name (fun f ->
               "AttributeName", String.to_json f)
         ])

  let of_json j =
    { attribute_name =
        Aws.Util.option_map (Aws.Json.lookup j "AttributeName") String.of_json
    ; attribute_value =
        Aws.Util.option_map (Aws.Json.lookup j "AttributeValue") String.of_json
    }
end

module InvalidSubnetException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module RemoveAvailabilityZonesInput = struct
  type t =
    { load_balancer_name : String.t
    ; availability_zones : AvailabilityZones.t
    }

  let make ~load_balancer_name ~availability_zones () =
    { load_balancer_name; availability_zones }

  let parse xml =
    Some
      { load_balancer_name =
          Aws.Xml.required
            "LoadBalancerName"
            (Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse)
      ; availability_zones =
          Aws.Xml.required
            "AvailabilityZones"
            (Aws.Util.option_bind
               (Aws.Xml.member "AvailabilityZones" xml)
               AvailabilityZones.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "AvailabilityZones.member"
                , AvailabilityZones.to_query v.availability_zones ))
         ; Some
             (Aws.Query.Pair ("LoadBalancerName", String.to_query v.load_balancer_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("AvailabilityZones", AvailabilityZones.to_json v.availability_zones)
         ; Some ("LoadBalancerName", String.to_json v.load_balancer_name)
         ])

  let of_json j =
    { load_balancer_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerName"))
    ; availability_zones =
        AvailabilityZones.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "AvailabilityZones"))
    }
end

module PolicyTypeNotFoundException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module PolicyDescriptions = struct
  type t = PolicyDescription.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map PolicyDescription.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list PolicyDescription.to_query v

  let to_json v = `List (List.map PolicyDescription.to_json v)

  let of_json j = Aws.Json.to_list PolicyDescription.of_json j
end

module DescribeLoadBalancerPoliciesOutput = struct
  type t = { policy_descriptions : PolicyDescriptions.t }

  let make ?(policy_descriptions = []) () = { policy_descriptions }

  let parse xml =
    Some
      { policy_descriptions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "PolicyDescriptions" xml)
               PolicyDescriptions.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "PolicyDescriptions.member"
                , PolicyDescriptions.to_query v.policy_descriptions ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("PolicyDescriptions", PolicyDescriptions.to_json v.policy_descriptions) ])

  let of_json j =
    { policy_descriptions =
        PolicyDescriptions.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyDescriptions"))
    }
end

module TagKeyOnly = struct
  type t = { key : String.t option }

  let make ?key () = { key }

  let parse xml =
    Some { key = Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.key (fun f -> Aws.Query.Pair ("Key", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.key (fun f -> "Key", String.to_json f) ])

  let of_json j = { key = Aws.Util.option_map (Aws.Json.lookup j "Key") String.of_json }
end

module TagKeyList = struct
  type t = TagKeyOnly.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map TagKeyOnly.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list TagKeyOnly.to_query v

  let to_json v = `List (List.map TagKeyOnly.to_json v)

  let of_json j = Aws.Json.to_list TagKeyOnly.of_json j
end

module CertificateNotFoundException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module DuplicateListenerException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module LoadBalancerNames = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module DescribeLoadBalancerAttributesInput = struct
  type t = { load_balancer_name : String.t }

  let make ~load_balancer_name () = { load_balancer_name }

  let parse xml =
    Some
      { load_balancer_name =
          Aws.Xml.required
            "LoadBalancerName"
            (Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("LoadBalancerName", String.to_query v.load_balancer_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("LoadBalancerName", String.to_json v.load_balancer_name) ])

  let of_json j =
    { load_balancer_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerName"))
    }
end

module DetachLoadBalancerFromSubnetsOutput = struct
  type t = { subnets : Subnets.t }

  let make ?(subnets = []) () = { subnets }

  let parse xml =
    Some
      { subnets =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Subnets" xml) Subnets.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Subnets.member", Subnets.to_query v.subnets)) ])

  let to_json v =
    `Assoc (Aws.Util.list_filter_opt [ Some ("Subnets", Subnets.to_json v.subnets) ])

  let of_json j =
    { subnets = Subnets.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Subnets")) }
end

module CreateLoadBalancerListenerOutput = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module InvalidSecurityGroupException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module SetLoadBalancerPoliciesOfListenerOutput = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module InvalidSchemeException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module DescribeAccessPointsInput = struct
  type t =
    { load_balancer_names : LoadBalancerNames.t
    ; marker : String.t option
    ; page_size : Integer.t option
    }

  let make ?(load_balancer_names = []) ?marker ?page_size () =
    { load_balancer_names; marker; page_size }

  let parse xml =
    Some
      { load_balancer_names =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "LoadBalancerNames" xml)
               LoadBalancerNames.parse)
      ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      ; page_size = Aws.Util.option_bind (Aws.Xml.member "PageSize" xml) Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.page_size (fun f ->
               Aws.Query.Pair ("PageSize", Integer.to_query f))
         ; Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "LoadBalancerNames.member"
                , LoadBalancerNames.to_query v.load_balancer_names ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.page_size (fun f -> "PageSize", Integer.to_json f)
         ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ; Some ("LoadBalancerNames", LoadBalancerNames.to_json v.load_balancer_names)
         ])

  let of_json j =
    { load_balancer_names =
        LoadBalancerNames.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerNames"))
    ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    ; page_size = Aws.Util.option_map (Aws.Json.lookup j "PageSize") Integer.of_json
    }
end

module CreateAccessPointInput = struct
  type t =
    { load_balancer_name : String.t
    ; listeners : Listeners.t
    ; availability_zones : AvailabilityZones.t
    ; subnets : Subnets.t
    ; security_groups : SecurityGroups.t
    ; scheme : String.t option
    ; tags : TagList.t
    }

  let make
      ~load_balancer_name
      ~listeners
      ?(availability_zones = [])
      ?(subnets = [])
      ?(security_groups = [])
      ?scheme
      ?(tags = [])
      () =
    { load_balancer_name
    ; listeners
    ; availability_zones
    ; subnets
    ; security_groups
    ; scheme
    ; tags
    }

  let parse xml =
    Some
      { load_balancer_name =
          Aws.Xml.required
            "LoadBalancerName"
            (Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse)
      ; listeners =
          Aws.Xml.required
            "Listeners"
            (Aws.Util.option_bind (Aws.Xml.member "Listeners" xml) Listeners.parse)
      ; availability_zones =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "AvailabilityZones" xml)
               AvailabilityZones.parse)
      ; subnets =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Subnets" xml) Subnets.parse)
      ; security_groups =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "SecurityGroups" xml)
               SecurityGroups.parse)
      ; scheme = Aws.Util.option_bind (Aws.Xml.member "Scheme" xml) String.parse
      ; tags =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Aws.Util.option_map v.scheme (fun f ->
               Aws.Query.Pair ("Scheme", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("SecurityGroups.member", SecurityGroups.to_query v.security_groups))
         ; Some (Aws.Query.Pair ("Subnets.member", Subnets.to_query v.subnets))
         ; Some
             (Aws.Query.Pair
                ( "AvailabilityZones.member"
                , AvailabilityZones.to_query v.availability_zones ))
         ; Some (Aws.Query.Pair ("Listeners.member", Listeners.to_query v.listeners))
         ; Some
             (Aws.Query.Pair ("LoadBalancerName", String.to_query v.load_balancer_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Tags", TagList.to_json v.tags)
         ; Aws.Util.option_map v.scheme (fun f -> "Scheme", String.to_json f)
         ; Some ("SecurityGroups", SecurityGroups.to_json v.security_groups)
         ; Some ("Subnets", Subnets.to_json v.subnets)
         ; Some ("AvailabilityZones", AvailabilityZones.to_json v.availability_zones)
         ; Some ("Listeners", Listeners.to_json v.listeners)
         ; Some ("LoadBalancerName", String.to_json v.load_balancer_name)
         ])

  let of_json j =
    { load_balancer_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerName"))
    ; listeners =
        Listeners.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Listeners"))
    ; availability_zones =
        AvailabilityZones.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "AvailabilityZones"))
    ; subnets = Subnets.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Subnets"))
    ; security_groups =
        SecurityGroups.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroups"))
    ; scheme = Aws.Util.option_map (Aws.Json.lookup j "Scheme") String.of_json
    ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
    }
end

module TooManyAccessPointsException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module SetLoadBalancerListenerSSLCertificateOutput = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module SetLoadBalancerPoliciesForBackendServerInput = struct
  type t =
    { load_balancer_name : String.t
    ; instance_port : Integer.t
    ; policy_names : PolicyNames.t
    }

  let make ~load_balancer_name ~instance_port ~policy_names () =
    { load_balancer_name; instance_port; policy_names }

  let parse xml =
    Some
      { load_balancer_name =
          Aws.Xml.required
            "LoadBalancerName"
            (Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse)
      ; instance_port =
          Aws.Xml.required
            "InstancePort"
            (Aws.Util.option_bind (Aws.Xml.member "InstancePort" xml) Integer.parse)
      ; policy_names =
          Aws.Xml.required
            "PolicyNames"
            (Aws.Util.option_bind (Aws.Xml.member "PolicyNames" xml) PolicyNames.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("PolicyNames.member", PolicyNames.to_query v.policy_names))
         ; Some (Aws.Query.Pair ("InstancePort", Integer.to_query v.instance_port))
         ; Some
             (Aws.Query.Pair ("LoadBalancerName", String.to_query v.load_balancer_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("PolicyNames", PolicyNames.to_json v.policy_names)
         ; Some ("InstancePort", Integer.to_json v.instance_port)
         ; Some ("LoadBalancerName", String.to_json v.load_balancer_name)
         ])

  let of_json j =
    { load_balancer_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerName"))
    ; instance_port =
        Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "InstancePort"))
    ; policy_names =
        PolicyNames.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyNames"))
    }
end

module RemoveTagsInput = struct
  type t =
    { load_balancer_names : LoadBalancerNames.t
    ; tags : TagKeyList.t
    }

  let make ~load_balancer_names ~tags () = { load_balancer_names; tags }

  let parse xml =
    Some
      { load_balancer_names =
          Aws.Xml.required
            "LoadBalancerNames"
            (Aws.Util.option_bind
               (Aws.Xml.member "LoadBalancerNames" xml)
               LoadBalancerNames.parse)
      ; tags =
          Aws.Xml.required
            "Tags"
            (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagKeyList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Tags.member", TagKeyList.to_query v.tags))
         ; Some
             (Aws.Query.Pair
                ( "LoadBalancerNames.member"
                , LoadBalancerNames.to_query v.load_balancer_names ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Tags", TagKeyList.to_json v.tags)
         ; Some ("LoadBalancerNames", LoadBalancerNames.to_json v.load_balancer_names)
         ])

  let of_json j =
    { load_balancer_names =
        LoadBalancerNames.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerNames"))
    ; tags = TagKeyList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
    }
end

module SubnetNotFoundException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module InvalidEndPointException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module CreateLoadBalancerPolicyOutput = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ListenerNotFoundException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module AddAvailabilityZonesOutput = struct
  type t = { availability_zones : AvailabilityZones.t }

  let make ?(availability_zones = []) () = { availability_zones }

  let parse xml =
    Some
      { availability_zones =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "AvailabilityZones" xml)
               AvailabilityZones.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "AvailabilityZones.member"
                , AvailabilityZones.to_query v.availability_zones ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("AvailabilityZones", AvailabilityZones.to_json v.availability_zones) ])

  let of_json j =
    { availability_zones =
        AvailabilityZones.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "AvailabilityZones"))
    }
end

module DeregisterEndPointsInput = struct
  type t =
    { load_balancer_name : String.t
    ; instances : Instances.t
    }

  let make ~load_balancer_name ~instances () = { load_balancer_name; instances }

  let parse xml =
    Some
      { load_balancer_name =
          Aws.Xml.required
            "LoadBalancerName"
            (Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse)
      ; instances =
          Aws.Xml.required
            "Instances"
            (Aws.Util.option_bind (Aws.Xml.member "Instances" xml) Instances.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Instances.member", Instances.to_query v.instances))
         ; Some
             (Aws.Query.Pair ("LoadBalancerName", String.to_query v.load_balancer_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Instances", Instances.to_json v.instances)
         ; Some ("LoadBalancerName", String.to_json v.load_balancer_name)
         ])

  let of_json j =
    { load_balancer_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerName"))
    ; instances =
        Instances.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Instances"))
    }
end

module AttachLoadBalancerToSubnetsInput = struct
  type t =
    { load_balancer_name : String.t
    ; subnets : Subnets.t
    }

  let make ~load_balancer_name ~subnets () = { load_balancer_name; subnets }

  let parse xml =
    Some
      { load_balancer_name =
          Aws.Xml.required
            "LoadBalancerName"
            (Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse)
      ; subnets =
          Aws.Xml.required
            "Subnets"
            (Aws.Util.option_bind (Aws.Xml.member "Subnets" xml) Subnets.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Subnets.member", Subnets.to_query v.subnets))
         ; Some
             (Aws.Query.Pair ("LoadBalancerName", String.to_query v.load_balancer_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Subnets", Subnets.to_json v.subnets)
         ; Some ("LoadBalancerName", String.to_json v.load_balancer_name)
         ])

  let of_json j =
    { load_balancer_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerName"))
    ; subnets = Subnets.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Subnets"))
    }
end

module TooManyTagsException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module LoadBalancerAttributeNotFoundException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module DescribeLoadBalancerPoliciesInput = struct
  type t =
    { load_balancer_name : String.t option
    ; policy_names : PolicyNames.t
    }

  let make ?load_balancer_name ?(policy_names = []) () =
    { load_balancer_name; policy_names }

  let parse xml =
    Some
      { load_balancer_name =
          Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse
      ; policy_names =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "PolicyNames" xml) PolicyNames.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("PolicyNames.member", PolicyNames.to_query v.policy_names))
         ; Aws.Util.option_map v.load_balancer_name (fun f ->
               Aws.Query.Pair ("LoadBalancerName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("PolicyNames", PolicyNames.to_json v.policy_names)
         ; Aws.Util.option_map v.load_balancer_name (fun f ->
               "LoadBalancerName", String.to_json f)
         ])

  let of_json j =
    { load_balancer_name =
        Aws.Util.option_map (Aws.Json.lookup j "LoadBalancerName") String.of_json
    ; policy_names =
        PolicyNames.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyNames"))
    }
end

module TagDescription = struct
  type t =
    { load_balancer_name : String.t option
    ; tags : TagList.t
    }

  let make ?load_balancer_name ?(tags = []) () = { load_balancer_name; tags }

  let parse xml =
    Some
      { load_balancer_name =
          Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse
      ; tags =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Aws.Util.option_map v.load_balancer_name (fun f ->
               Aws.Query.Pair ("LoadBalancerName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Tags", TagList.to_json v.tags)
         ; Aws.Util.option_map v.load_balancer_name (fun f ->
               "LoadBalancerName", String.to_json f)
         ])

  let of_json j =
    { load_balancer_name =
        Aws.Util.option_map (Aws.Json.lookup j "LoadBalancerName") String.of_json
    ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
    }
end

module TagDescriptions = struct
  type t = TagDescription.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map TagDescription.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list TagDescription.to_query v

  let to_json v = `List (List.map TagDescription.to_json v)

  let of_json j = Aws.Json.to_list TagDescription.of_json j
end

module CreateAccessPointOutput = struct
  type t = { d_n_s_name : String.t option }

  let make ?d_n_s_name () = { d_n_s_name }

  let parse xml =
    Some { d_n_s_name = Aws.Util.option_bind (Aws.Xml.member "DNSName" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.d_n_s_name (fun f ->
               Aws.Query.Pair ("DNSName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.d_n_s_name (fun f -> "DNSName", String.to_json f) ])

  let of_json j =
    { d_n_s_name = Aws.Util.option_map (Aws.Json.lookup j "DNSName") String.of_json }
end

module DuplicateTagKeysException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module PolicyAttributes = struct
  type t = PolicyAttribute.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map PolicyAttribute.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list PolicyAttribute.to_query v

  let to_json v = `List (List.map PolicyAttribute.to_json v)

  let of_json j = Aws.Json.to_list PolicyAttribute.of_json j
end

module CreateLoadBalancerPolicyInput = struct
  type t =
    { load_balancer_name : String.t
    ; policy_name : String.t
    ; policy_type_name : String.t
    ; policy_attributes : PolicyAttributes.t
    }

  let make ~load_balancer_name ~policy_name ~policy_type_name ?(policy_attributes = []) ()
      =
    { load_balancer_name; policy_name; policy_type_name; policy_attributes }

  let parse xml =
    Some
      { load_balancer_name =
          Aws.Xml.required
            "LoadBalancerName"
            (Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse)
      ; policy_name =
          Aws.Xml.required
            "PolicyName"
            (Aws.Util.option_bind (Aws.Xml.member "PolicyName" xml) String.parse)
      ; policy_type_name =
          Aws.Xml.required
            "PolicyTypeName"
            (Aws.Util.option_bind (Aws.Xml.member "PolicyTypeName" xml) String.parse)
      ; policy_attributes =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "PolicyAttributes" xml)
               PolicyAttributes.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("PolicyAttributes.member", PolicyAttributes.to_query v.policy_attributes))
         ; Some (Aws.Query.Pair ("PolicyTypeName", String.to_query v.policy_type_name))
         ; Some (Aws.Query.Pair ("PolicyName", String.to_query v.policy_name))
         ; Some
             (Aws.Query.Pair ("LoadBalancerName", String.to_query v.load_balancer_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("PolicyAttributes", PolicyAttributes.to_json v.policy_attributes)
         ; Some ("PolicyTypeName", String.to_json v.policy_type_name)
         ; Some ("PolicyName", String.to_json v.policy_name)
         ; Some ("LoadBalancerName", String.to_json v.load_balancer_name)
         ])

  let of_json j =
    { load_balancer_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerName"))
    ; policy_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyName"))
    ; policy_type_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyTypeName"))
    ; policy_attributes =
        PolicyAttributes.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyAttributes"))
    }
end

module CreateLoadBalancerListenerInput = struct
  type t =
    { load_balancer_name : String.t
    ; listeners : Listeners.t
    }

  let make ~load_balancer_name ~listeners () = { load_balancer_name; listeners }

  let parse xml =
    Some
      { load_balancer_name =
          Aws.Xml.required
            "LoadBalancerName"
            (Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse)
      ; listeners =
          Aws.Xml.required
            "Listeners"
            (Aws.Util.option_bind (Aws.Xml.member "Listeners" xml) Listeners.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Listeners.member", Listeners.to_query v.listeners))
         ; Some
             (Aws.Query.Pair ("LoadBalancerName", String.to_query v.load_balancer_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Listeners", Listeners.to_json v.listeners)
         ; Some ("LoadBalancerName", String.to_json v.load_balancer_name)
         ])

  let of_json j =
    { load_balancer_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerName"))
    ; listeners =
        Listeners.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Listeners"))
    }
end

module AddTagsInput = struct
  type t =
    { load_balancer_names : LoadBalancerNames.t
    ; tags : TagList.t
    }

  let make ~load_balancer_names ~tags () = { load_balancer_names; tags }

  let parse xml =
    Some
      { load_balancer_names =
          Aws.Xml.required
            "LoadBalancerNames"
            (Aws.Util.option_bind
               (Aws.Xml.member "LoadBalancerNames" xml)
               LoadBalancerNames.parse)
      ; tags =
          Aws.Xml.required
            "Tags"
            (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Some
             (Aws.Query.Pair
                ( "LoadBalancerNames.member"
                , LoadBalancerNames.to_query v.load_balancer_names ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Tags", TagList.to_json v.tags)
         ; Some ("LoadBalancerNames", LoadBalancerNames.to_json v.load_balancer_names)
         ])

  let of_json j =
    { load_balancer_names =
        LoadBalancerNames.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "LoadBalancerNames"))
    ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
    }
end

module SetLoadBalancerPoliciesForBackendServerOutput = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ModifyLoadBalancerAttributesOutput = struct
  type t =
    { load_balancer_name : String.t option
    ; load_balancer_attributes : LoadBalancerAttributes.t option
    }

  let make ?load_balancer_name ?load_balancer_attributes () =
    { load_balancer_name; load_balancer_attributes }

  let parse xml =
    Some
      { load_balancer_name =
          Aws.Util.option_bind (Aws.Xml.member "LoadBalancerName" xml) String.parse
      ; load_balancer_attributes =
          Aws.Util.option_bind
            (Aws.Xml.member "LoadBalancerAttributes" xml)
            LoadBalancerAttributes.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.load_balancer_attributes (fun f ->
               Aws.Query.Pair ("LoadBalancerAttributes", LoadBalancerAttributes.to_query f))
         ; Aws.Util.option_map v.load_balancer_name (fun f ->
               Aws.Query.Pair ("LoadBalancerName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.load_balancer_attributes (fun f ->
               "LoadBalancerAttributes", LoadBalancerAttributes.to_json f)
         ; Aws.Util.option_map v.load_balancer_name (fun f ->
               "LoadBalancerName", String.to_json f)
         ])

  let of_json j =
    { load_balancer_name =
        Aws.Util.option_map (Aws.Json.lookup j "LoadBalancerName") String.of_json
    ; load_balancer_attributes =
        Aws.Util.option_map
          (Aws.Json.lookup j "LoadBalancerAttributes")
          LoadBalancerAttributes.of_json
    }
end

module ConfigureHealthCheckOutput = struct
  type t = { health_check : HealthCheck.t option }

  let make ?health_check () = { health_check }

  let parse xml =
    Some
      { health_check =
          Aws.Util.option_bind (Aws.Xml.member "HealthCheck" xml) HealthCheck.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.health_check (fun f ->
               Aws.Query.Pair ("HealthCheck", HealthCheck.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.health_check (fun f ->
               "HealthCheck", HealthCheck.to_json f)
         ])

  let of_json j =
    { health_check =
        Aws.Util.option_map (Aws.Json.lookup j "HealthCheck") HealthCheck.of_json
    }
end

module DescribeTagsOutput = struct
  type t = { tag_descriptions : TagDescriptions.t }

  let make ?(tag_descriptions = []) () = { tag_descriptions }

  let parse xml =
    Some
      { tag_descriptions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "TagDescriptions" xml)
               TagDescriptions.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("TagDescriptions.member", TagDescriptions.to_query v.tag_descriptions))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("TagDescriptions", TagDescriptions.to_json v.tag_descriptions) ])

  let of_json j =
    { tag_descriptions =
        TagDescriptions.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "TagDescriptions"))
    }
end
