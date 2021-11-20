open Aws
open Aws.BaseTypes
open CalendarLib
type calendar = Calendar.t
module PolicyNames =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module Listener =
  struct
    type t =
      {
      protocol: String.t ;
      load_balancer_port: Integer.t ;
      instance_protocol: String.t option ;
      instance_port: Integer.t ;
      s_s_l_certificate_id: String.t option }
    let make ~protocol  ~load_balancer_port  ?instance_protocol 
      ~instance_port  ?s_s_l_certificate_id  () =
      {
        protocol;
        load_balancer_port;
        instance_protocol;
        instance_port;
        s_s_l_certificate_id
      }
    let parse xml =
      Some
        {
          protocol =
            (Xml.required "Protocol"
               (Util.option_bind (Xml.member "Protocol" xml) String.parse));
          load_balancer_port =
            (Xml.required "LoadBalancerPort"
               (Util.option_bind (Xml.member "LoadBalancerPort" xml)
                  Integer.parse));
          instance_protocol =
            (Util.option_bind (Xml.member "InstanceProtocol" xml)
               String.parse);
          instance_port =
            (Xml.required "InstancePort"
               (Util.option_bind (Xml.member "InstancePort" xml)
                  Integer.parse));
          s_s_l_certificate_id =
            (Util.option_bind (Xml.member "SSLCertificateId" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.s_s_l_certificate_id
              (fun f -> Query.Pair ("SSLCertificateId", (String.to_query f)));
           Some
             (Query.Pair ("InstancePort", (Integer.to_query v.instance_port)));
           Util.option_map v.instance_protocol
             (fun f -> Query.Pair ("InstanceProtocol", (String.to_query f)));
           Some
             (Query.Pair
                ("LoadBalancerPort", (Integer.to_query v.load_balancer_port)));
           Some (Query.Pair ("Protocol", (String.to_query v.protocol)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.s_s_l_certificate_id
              (fun f -> ("s_s_l_certificate_id", (String.to_json f)));
           Some ("instance_port", (Integer.to_json v.instance_port));
           Util.option_map v.instance_protocol
             (fun f -> ("instance_protocol", (String.to_json f)));
           Some
             ("load_balancer_port", (Integer.to_json v.load_balancer_port));
           Some ("protocol", (String.to_json v.protocol))])
    let of_json j =
      {
        protocol =
          (String.of_json (Util.of_option_exn (Json.lookup j "protocol")));
        load_balancer_port =
          (Integer.of_json
             (Util.of_option_exn (Json.lookup j "load_balancer_port")));
        instance_protocol =
          (Util.option_map (Json.lookup j "instance_protocol") String.of_json);
        instance_port =
          (Integer.of_json
             (Util.of_option_exn (Json.lookup j "instance_port")));
        s_s_l_certificate_id =
          (Util.option_map (Json.lookup j "s_s_l_certificate_id")
             String.of_json)
      }
  end
module AppCookieStickinessPolicy =
  struct
    type t = {
      policy_name: String.t option ;
      cookie_name: String.t option }
    let make ?policy_name  ?cookie_name  () = { policy_name; cookie_name }
    let parse xml =
      Some
        {
          policy_name =
            (Util.option_bind (Xml.member "PolicyName" xml) String.parse);
          cookie_name =
            (Util.option_bind (Xml.member "CookieName" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.cookie_name
              (fun f -> Query.Pair ("CookieName", (String.to_query f)));
           Util.option_map v.policy_name
             (fun f -> Query.Pair ("PolicyName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cookie_name
              (fun f -> ("cookie_name", (String.to_json f)));
           Util.option_map v.policy_name
             (fun f -> ("policy_name", (String.to_json f)))])
    let of_json j =
      {
        policy_name =
          (Util.option_map (Json.lookup j "policy_name") String.of_json);
        cookie_name =
          (Util.option_map (Json.lookup j "cookie_name") String.of_json)
      }
  end
module LBCookieStickinessPolicy =
  struct
    type t =
      {
      policy_name: String.t option ;
      cookie_expiration_period: Long.t option }
    let make ?policy_name  ?cookie_expiration_period  () =
      { policy_name; cookie_expiration_period }
    let parse xml =
      Some
        {
          policy_name =
            (Util.option_bind (Xml.member "PolicyName" xml) String.parse);
          cookie_expiration_period =
            (Util.option_bind (Xml.member "CookieExpirationPeriod" xml)
               Long.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.cookie_expiration_period
              (fun f ->
                 Query.Pair ("CookieExpirationPeriod", (Long.to_query f)));
           Util.option_map v.policy_name
             (fun f -> Query.Pair ("PolicyName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cookie_expiration_period
              (fun f -> ("cookie_expiration_period", (Long.to_json f)));
           Util.option_map v.policy_name
             (fun f -> ("policy_name", (String.to_json f)))])
    let of_json j =
      {
        policy_name =
          (Util.option_map (Json.lookup j "policy_name") String.of_json);
        cookie_expiration_period =
          (Util.option_map (Json.lookup j "cookie_expiration_period")
             Long.of_json)
      }
  end
module Tag =
  struct
    type t = {
      key: String.t ;
      value: String.t option }
    let make ~key  ?value  () = { key; value }
    let parse xml =
      Some
        {
          key =
            (Xml.required "Key"
               (Util.option_bind (Xml.member "Key" xml) String.parse));
          value = (Util.option_bind (Xml.member "Value" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.value
              (fun f -> Query.Pair ("Value", (String.to_query f)));
           Some (Query.Pair ("Key", (String.to_query v.key)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.value (fun f -> ("value", (String.to_json f)));
           Some ("key", (String.to_json v.key))])
    let of_json j =
      {
        key = (String.of_json (Util.of_option_exn (Json.lookup j "key")));
        value = (Util.option_map (Json.lookup j "value") String.of_json)
      }
  end
module PolicyAttributeDescription =
  struct
    type t =
      {
      attribute_name: String.t option ;
      attribute_value: String.t option }
    let make ?attribute_name  ?attribute_value  () =
      { attribute_name; attribute_value }
    let parse xml =
      Some
        {
          attribute_name =
            (Util.option_bind (Xml.member "AttributeName" xml) String.parse);
          attribute_value =
            (Util.option_bind (Xml.member "AttributeValue" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.attribute_value
              (fun f -> Query.Pair ("AttributeValue", (String.to_query f)));
           Util.option_map v.attribute_name
             (fun f -> Query.Pair ("AttributeName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.attribute_value
              (fun f -> ("attribute_value", (String.to_json f)));
           Util.option_map v.attribute_name
             (fun f -> ("attribute_name", (String.to_json f)))])
    let of_json j =
      {
        attribute_name =
          (Util.option_map (Json.lookup j "attribute_name") String.of_json);
        attribute_value =
          (Util.option_map (Json.lookup j "attribute_value") String.of_json)
      }
  end
module PolicyAttributeTypeDescription =
  struct
    type t =
      {
      attribute_name: String.t option ;
      attribute_type: String.t option ;
      description: String.t option ;
      default_value: String.t option ;
      cardinality: String.t option }
    let make ?attribute_name  ?attribute_type  ?description  ?default_value 
      ?cardinality  () =
      {
        attribute_name;
        attribute_type;
        description;
        default_value;
        cardinality
      }
    let parse xml =
      Some
        {
          attribute_name =
            (Util.option_bind (Xml.member "AttributeName" xml) String.parse);
          attribute_type =
            (Util.option_bind (Xml.member "AttributeType" xml) String.parse);
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse);
          default_value =
            (Util.option_bind (Xml.member "DefaultValue" xml) String.parse);
          cardinality =
            (Util.option_bind (Xml.member "Cardinality" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.cardinality
              (fun f -> Query.Pair ("Cardinality", (String.to_query f)));
           Util.option_map v.default_value
             (fun f -> Query.Pair ("DefaultValue", (String.to_query f)));
           Util.option_map v.description
             (fun f -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.attribute_type
             (fun f -> Query.Pair ("AttributeType", (String.to_query f)));
           Util.option_map v.attribute_name
             (fun f -> Query.Pair ("AttributeName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cardinality
              (fun f -> ("cardinality", (String.to_json f)));
           Util.option_map v.default_value
             (fun f -> ("default_value", (String.to_json f)));
           Util.option_map v.description
             (fun f -> ("description", (String.to_json f)));
           Util.option_map v.attribute_type
             (fun f -> ("attribute_type", (String.to_json f)));
           Util.option_map v.attribute_name
             (fun f -> ("attribute_name", (String.to_json f)))])
    let of_json j =
      {
        attribute_name =
          (Util.option_map (Json.lookup j "attribute_name") String.of_json);
        attribute_type =
          (Util.option_map (Json.lookup j "attribute_type") String.of_json);
        description =
          (Util.option_map (Json.lookup j "description") String.of_json);
        default_value =
          (Util.option_map (Json.lookup j "default_value") String.of_json);
        cardinality =
          (Util.option_map (Json.lookup j "cardinality") String.of_json)
      }
  end
module BackendServerDescription =
  struct
    type t = {
      instance_port: Integer.t option ;
      policy_names: PolicyNames.t }
    let make ?instance_port  ?(policy_names= [])  () =
      { instance_port; policy_names }
    let parse xml =
      Some
        {
          instance_port =
            (Util.option_bind (Xml.member "InstancePort" xml) Integer.parse);
          policy_names =
            (Util.of_option []
               (Util.option_bind (Xml.member "PolicyNames" xml)
                  PolicyNames.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("PolicyNames.member",
                   (PolicyNames.to_query v.policy_names)));
           Util.option_map v.instance_port
             (fun f -> Query.Pair ("InstancePort", (Integer.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("policy_names", (PolicyNames.to_json v.policy_names));
           Util.option_map v.instance_port
             (fun f -> ("instance_port", (Integer.to_json f)))])
    let of_json j =
      {
        instance_port =
          (Util.option_map (Json.lookup j "instance_port") Integer.of_json);
        policy_names =
          (PolicyNames.of_json
             (Util.of_option_exn (Json.lookup j "policy_names")))
      }
  end
module Instance =
  struct
    type t = {
      instance_id: String.t option }
    let make ?instance_id  () = { instance_id }
    let parse xml =
      Some
        {
          instance_id =
            (Util.option_bind (Xml.member "InstanceId" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.instance_id
              (fun f -> Query.Pair ("InstanceId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.instance_id
              (fun f -> ("instance_id", (String.to_json f)))])
    let of_json j =
      {
        instance_id =
          (Util.option_map (Json.lookup j "instance_id") String.of_json)
      }
  end
module ListenerDescription =
  struct
    type t = {
      listener: Listener.t option ;
      policy_names: PolicyNames.t }
    let make ?listener  ?(policy_names= [])  () = { listener; policy_names }
    let parse xml =
      Some
        {
          listener =
            (Util.option_bind (Xml.member "Listener" xml) Listener.parse);
          policy_names =
            (Util.of_option []
               (Util.option_bind (Xml.member "PolicyNames" xml)
                  PolicyNames.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("PolicyNames.member",
                   (PolicyNames.to_query v.policy_names)));
           Util.option_map v.listener
             (fun f -> Query.Pair ("Listener", (Listener.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("policy_names", (PolicyNames.to_json v.policy_names));
           Util.option_map v.listener
             (fun f -> ("listener", (Listener.to_json f)))])
    let of_json j =
      {
        listener =
          (Util.option_map (Json.lookup j "listener") Listener.of_json);
        policy_names =
          (PolicyNames.of_json
             (Util.of_option_exn (Json.lookup j "policy_names")))
      }
  end
module AppCookieStickinessPolicies =
  struct
    type t = AppCookieStickinessPolicy.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map AppCookieStickinessPolicy.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list AppCookieStickinessPolicy.to_query v
    let to_json v = `List (List.map AppCookieStickinessPolicy.to_json v)
    let of_json j = Json.to_list AppCookieStickinessPolicy.of_json j
  end
module LBCookieStickinessPolicies =
  struct
    type t = LBCookieStickinessPolicy.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map LBCookieStickinessPolicy.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list LBCookieStickinessPolicy.to_query v
    let to_json v = `List (List.map LBCookieStickinessPolicy.to_json v)
    let of_json j = Json.to_list LBCookieStickinessPolicy.of_json j
  end
module TagList =
  struct
    type t = Tag.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Tag.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Tag.to_query v
    let to_json v = `List (List.map Tag.to_json v)
    let of_json j = Json.to_list Tag.of_json j
  end
module AdditionalAttribute =
  struct
    type t = {
      key: String.t option ;
      value: String.t option }
    let make ?key  ?value  () = { key; value }
    let parse xml =
      Some
        {
          key = (Util.option_bind (Xml.member "Key" xml) String.parse);
          value = (Util.option_bind (Xml.member "Value" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.value
              (fun f -> Query.Pair ("Value", (String.to_query f)));
           Util.option_map v.key
             (fun f -> Query.Pair ("Key", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.value (fun f -> ("value", (String.to_json f)));
           Util.option_map v.key (fun f -> ("key", (String.to_json f)))])
    let of_json j =
      {
        key = (Util.option_map (Json.lookup j "key") String.of_json);
        value = (Util.option_map (Json.lookup j "value") String.of_json)
      }
  end
module PolicyAttributeDescriptions =
  struct
    type t = PolicyAttributeDescription.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map PolicyAttributeDescription.parse (Xml.members "member" xml))
    let to_query v =
      Query.to_query_list PolicyAttributeDescription.to_query v
    let to_json v = `List (List.map PolicyAttributeDescription.to_json v)
    let of_json j = Json.to_list PolicyAttributeDescription.of_json j
  end
module PolicyAttributeTypeDescriptions =
  struct
    type t = PolicyAttributeTypeDescription.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map PolicyAttributeTypeDescription.parse
           (Xml.members "member" xml))
    let to_query v =
      Query.to_query_list PolicyAttributeTypeDescription.to_query v
    let to_json v = `List (List.map PolicyAttributeTypeDescription.to_json v)
    let of_json j = Json.to_list PolicyAttributeTypeDescription.of_json j
  end
module AvailabilityZones =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module BackendServerDescriptions =
  struct
    type t = BackendServerDescription.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map BackendServerDescription.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list BackendServerDescription.to_query v
    let to_json v = `List (List.map BackendServerDescription.to_json v)
    let of_json j = Json.to_list BackendServerDescription.of_json j
  end
module HealthCheck =
  struct
    type t =
      {
      target: String.t ;
      interval: Integer.t ;
      timeout: Integer.t ;
      unhealthy_threshold: Integer.t ;
      healthy_threshold: Integer.t }
    let make ~target  ~interval  ~timeout  ~unhealthy_threshold 
      ~healthy_threshold  () =
      { target; interval; timeout; unhealthy_threshold; healthy_threshold }
    let parse xml =
      Some
        {
          target =
            (Xml.required "Target"
               (Util.option_bind (Xml.member "Target" xml) String.parse));
          interval =
            (Xml.required "Interval"
               (Util.option_bind (Xml.member "Interval" xml) Integer.parse));
          timeout =
            (Xml.required "Timeout"
               (Util.option_bind (Xml.member "Timeout" xml) Integer.parse));
          unhealthy_threshold =
            (Xml.required "UnhealthyThreshold"
               (Util.option_bind (Xml.member "UnhealthyThreshold" xml)
                  Integer.parse));
          healthy_threshold =
            (Xml.required "HealthyThreshold"
               (Util.option_bind (Xml.member "HealthyThreshold" xml)
                  Integer.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("HealthyThreshold", (Integer.to_query v.healthy_threshold)));
           Some
             (Query.Pair
                ("UnhealthyThreshold",
                  (Integer.to_query v.unhealthy_threshold)));
           Some (Query.Pair ("Timeout", (Integer.to_query v.timeout)));
           Some (Query.Pair ("Interval", (Integer.to_query v.interval)));
           Some (Query.Pair ("Target", (String.to_query v.target)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("healthy_threshold", (Integer.to_json v.healthy_threshold));
           Some
             ("unhealthy_threshold", (Integer.to_json v.unhealthy_threshold));
           Some ("timeout", (Integer.to_json v.timeout));
           Some ("interval", (Integer.to_json v.interval));
           Some ("target", (String.to_json v.target))])
    let of_json j =
      {
        target =
          (String.of_json (Util.of_option_exn (Json.lookup j "target")));
        interval =
          (Integer.of_json (Util.of_option_exn (Json.lookup j "interval")));
        timeout =
          (Integer.of_json (Util.of_option_exn (Json.lookup j "timeout")));
        unhealthy_threshold =
          (Integer.of_json
             (Util.of_option_exn (Json.lookup j "unhealthy_threshold")));
        healthy_threshold =
          (Integer.of_json
             (Util.of_option_exn (Json.lookup j "healthy_threshold")))
      }
  end
module Instances =
  struct
    type t = Instance.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Instance.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Instance.to_query v
    let to_json v = `List (List.map Instance.to_json v)
    let of_json j = Json.to_list Instance.of_json j
  end
module ListenerDescriptions =
  struct
    type t = ListenerDescription.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map ListenerDescription.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list ListenerDescription.to_query v
    let to_json v = `List (List.map ListenerDescription.to_json v)
    let of_json j = Json.to_list ListenerDescription.of_json j
  end
module Policies =
  struct
    type t =
      {
      app_cookie_stickiness_policies: AppCookieStickinessPolicies.t ;
      l_b_cookie_stickiness_policies: LBCookieStickinessPolicies.t ;
      other_policies: PolicyNames.t }
    let make ?(app_cookie_stickiness_policies= []) 
      ?(l_b_cookie_stickiness_policies= [])  ?(other_policies= [])  () =
      {
        app_cookie_stickiness_policies;
        l_b_cookie_stickiness_policies;
        other_policies
      }
    let parse xml =
      Some
        {
          app_cookie_stickiness_policies =
            (Util.of_option []
               (Util.option_bind
                  (Xml.member "AppCookieStickinessPolicies" xml)
                  AppCookieStickinessPolicies.parse));
          l_b_cookie_stickiness_policies =
            (Util.of_option []
               (Util.option_bind
                  (Xml.member "LBCookieStickinessPolicies" xml)
                  LBCookieStickinessPolicies.parse));
          other_policies =
            (Util.of_option []
               (Util.option_bind (Xml.member "OtherPolicies" xml)
                  PolicyNames.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("OtherPolicies.member",
                   (PolicyNames.to_query v.other_policies)));
           Some
             (Query.Pair
                ("LBCookieStickinessPolicies.member",
                  (LBCookieStickinessPolicies.to_query
                     v.l_b_cookie_stickiness_policies)));
           Some
             (Query.Pair
                ("AppCookieStickinessPolicies.member",
                  (AppCookieStickinessPolicies.to_query
                     v.app_cookie_stickiness_policies)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("other_policies", (PolicyNames.to_json v.other_policies));
           Some
             ("l_b_cookie_stickiness_policies",
               (LBCookieStickinessPolicies.to_json
                  v.l_b_cookie_stickiness_policies));
           Some
             ("app_cookie_stickiness_policies",
               (AppCookieStickinessPolicies.to_json
                  v.app_cookie_stickiness_policies))])
    let of_json j =
      {
        app_cookie_stickiness_policies =
          (AppCookieStickinessPolicies.of_json
             (Util.of_option_exn
                (Json.lookup j "app_cookie_stickiness_policies")));
        l_b_cookie_stickiness_policies =
          (LBCookieStickinessPolicies.of_json
             (Util.of_option_exn
                (Json.lookup j "l_b_cookie_stickiness_policies")));
        other_policies =
          (PolicyNames.of_json
             (Util.of_option_exn (Json.lookup j "other_policies")))
      }
  end
module SecurityGroups =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module SourceSecurityGroup =
  struct
    type t = {
      owner_alias: String.t option ;
      group_name: String.t option }
    let make ?owner_alias  ?group_name  () = { owner_alias; group_name }
    let parse xml =
      Some
        {
          owner_alias =
            (Util.option_bind (Xml.member "OwnerAlias" xml) String.parse);
          group_name =
            (Util.option_bind (Xml.member "GroupName" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.group_name
              (fun f -> Query.Pair ("GroupName", (String.to_query f)));
           Util.option_map v.owner_alias
             (fun f -> Query.Pair ("OwnerAlias", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.group_name
              (fun f -> ("group_name", (String.to_json f)));
           Util.option_map v.owner_alias
             (fun f -> ("owner_alias", (String.to_json f)))])
    let of_json j =
      {
        owner_alias =
          (Util.option_map (Json.lookup j "owner_alias") String.of_json);
        group_name =
          (Util.option_map (Json.lookup j "group_name") String.of_json)
      }
  end
module Subnets =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module TagDescription =
  struct
    type t = {
      load_balancer_name: String.t option ;
      tags: TagList.t }
    let make ?load_balancer_name  ?(tags= [])  () =
      { load_balancer_name; tags }
    let parse xml =
      Some
        {
          load_balancer_name =
            (Util.option_bind (Xml.member "LoadBalancerName" xml)
               String.parse);
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) TagList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Tags.member", (TagList.to_query v.tags)));
           Util.option_map v.load_balancer_name
             (fun f -> Query.Pair ("LoadBalancerName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tags", (TagList.to_json v.tags));
           Util.option_map v.load_balancer_name
             (fun f -> ("load_balancer_name", (String.to_json f)))])
    let of_json j =
      {
        load_balancer_name =
          (Util.option_map (Json.lookup j "load_balancer_name")
             String.of_json);
        tags = (TagList.of_json (Util.of_option_exn (Json.lookup j "tags")))
      }
  end
module AccessLog =
  struct
    type t =
      {
      enabled: Boolean.t ;
      s3_bucket_name: String.t option ;
      emit_interval: Integer.t option ;
      s3_bucket_prefix: String.t option }
    let make ~enabled  ?s3_bucket_name  ?emit_interval  ?s3_bucket_prefix  ()
      = { enabled; s3_bucket_name; emit_interval; s3_bucket_prefix }
    let parse xml =
      Some
        {
          enabled =
            (Xml.required "Enabled"
               (Util.option_bind (Xml.member "Enabled" xml) Boolean.parse));
          s3_bucket_name =
            (Util.option_bind (Xml.member "S3BucketName" xml) String.parse);
          emit_interval =
            (Util.option_bind (Xml.member "EmitInterval" xml) Integer.parse);
          s3_bucket_prefix =
            (Util.option_bind (Xml.member "S3BucketPrefix" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.s3_bucket_prefix
              (fun f -> Query.Pair ("S3BucketPrefix", (String.to_query f)));
           Util.option_map v.emit_interval
             (fun f -> Query.Pair ("EmitInterval", (Integer.to_query f)));
           Util.option_map v.s3_bucket_name
             (fun f -> Query.Pair ("S3BucketName", (String.to_query f)));
           Some (Query.Pair ("Enabled", (Boolean.to_query v.enabled)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.s3_bucket_prefix
              (fun f -> ("s3_bucket_prefix", (String.to_json f)));
           Util.option_map v.emit_interval
             (fun f -> ("emit_interval", (Integer.to_json f)));
           Util.option_map v.s3_bucket_name
             (fun f -> ("s3_bucket_name", (String.to_json f)));
           Some ("enabled", (Boolean.to_json v.enabled))])
    let of_json j =
      {
        enabled =
          (Boolean.of_json (Util.of_option_exn (Json.lookup j "enabled")));
        s3_bucket_name =
          (Util.option_map (Json.lookup j "s3_bucket_name") String.of_json);
        emit_interval =
          (Util.option_map (Json.lookup j "emit_interval") Integer.of_json);
        s3_bucket_prefix =
          (Util.option_map (Json.lookup j "s3_bucket_prefix") String.of_json)
      }
  end
module AdditionalAttributes =
  struct
    type t = AdditionalAttribute.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map AdditionalAttribute.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list AdditionalAttribute.to_query v
    let to_json v = `List (List.map AdditionalAttribute.to_json v)
    let of_json j = Json.to_list AdditionalAttribute.of_json j
  end
module ConnectionDraining =
  struct
    type t = {
      enabled: Boolean.t ;
      timeout: Integer.t option }
    let make ~enabled  ?timeout  () = { enabled; timeout }
    let parse xml =
      Some
        {
          enabled =
            (Xml.required "Enabled"
               (Util.option_bind (Xml.member "Enabled" xml) Boolean.parse));
          timeout =
            (Util.option_bind (Xml.member "Timeout" xml) Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.timeout
              (fun f -> Query.Pair ("Timeout", (Integer.to_query f)));
           Some (Query.Pair ("Enabled", (Boolean.to_query v.enabled)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.timeout
              (fun f -> ("timeout", (Integer.to_json f)));
           Some ("enabled", (Boolean.to_json v.enabled))])
    let of_json j =
      {
        enabled =
          (Boolean.of_json (Util.of_option_exn (Json.lookup j "enabled")));
        timeout = (Util.option_map (Json.lookup j "timeout") Integer.of_json)
      }
  end
module ConnectionSettings =
  struct
    type t = {
      idle_timeout: Integer.t }
    let make ~idle_timeout  () = { idle_timeout }
    let parse xml =
      Some
        {
          idle_timeout =
            (Xml.required "IdleTimeout"
               (Util.option_bind (Xml.member "IdleTimeout" xml) Integer.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("IdleTimeout", (Integer.to_query v.idle_timeout)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("idle_timeout", (Integer.to_json v.idle_timeout))])
    let of_json j =
      {
        idle_timeout =
          (Integer.of_json
             (Util.of_option_exn (Json.lookup j "idle_timeout")))
      }
  end
module CrossZoneLoadBalancing =
  struct
    type t = {
      enabled: Boolean.t }
    let make ~enabled  () = { enabled }
    let parse xml =
      Some
        {
          enabled =
            (Xml.required "Enabled"
               (Util.option_bind (Xml.member "Enabled" xml) Boolean.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Enabled", (Boolean.to_query v.enabled)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("enabled", (Boolean.to_json v.enabled))])
    let of_json j =
      {
        enabled =
          (Boolean.of_json (Util.of_option_exn (Json.lookup j "enabled")))
      }
  end
module PolicyAttribute =
  struct
    type t =
      {
      attribute_name: String.t option ;
      attribute_value: String.t option }
    let make ?attribute_name  ?attribute_value  () =
      { attribute_name; attribute_value }
    let parse xml =
      Some
        {
          attribute_name =
            (Util.option_bind (Xml.member "AttributeName" xml) String.parse);
          attribute_value =
            (Util.option_bind (Xml.member "AttributeValue" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.attribute_value
              (fun f -> Query.Pair ("AttributeValue", (String.to_query f)));
           Util.option_map v.attribute_name
             (fun f -> Query.Pair ("AttributeName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.attribute_value
              (fun f -> ("attribute_value", (String.to_json f)));
           Util.option_map v.attribute_name
             (fun f -> ("attribute_name", (String.to_json f)))])
    let of_json j =
      {
        attribute_name =
          (Util.option_map (Json.lookup j "attribute_name") String.of_json);
        attribute_value =
          (Util.option_map (Json.lookup j "attribute_value") String.of_json)
      }
  end
module TagKeyOnly =
  struct
    type t = {
      key: String.t option }
    let make ?key  () = { key }
    let parse xml =
      Some { key = (Util.option_bind (Xml.member "Key" xml) String.parse) }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.key
              (fun f -> Query.Pair ("Key", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.key (fun f -> ("key", (String.to_json f)))])
    let of_json j =
      { key = (Util.option_map (Json.lookup j "key") String.of_json) }
  end
module PolicyDescription =
  struct
    type t =
      {
      policy_name: String.t option ;
      policy_type_name: String.t option ;
      policy_attribute_descriptions: PolicyAttributeDescriptions.t }
    let make ?policy_name  ?policy_type_name 
      ?(policy_attribute_descriptions= [])  () =
      { policy_name; policy_type_name; policy_attribute_descriptions }
    let parse xml =
      Some
        {
          policy_name =
            (Util.option_bind (Xml.member "PolicyName" xml) String.parse);
          policy_type_name =
            (Util.option_bind (Xml.member "PolicyTypeName" xml) String.parse);
          policy_attribute_descriptions =
            (Util.of_option []
               (Util.option_bind
                  (Xml.member "PolicyAttributeDescriptions" xml)
                  PolicyAttributeDescriptions.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("PolicyAttributeDescriptions.member",
                   (PolicyAttributeDescriptions.to_query
                      v.policy_attribute_descriptions)));
           Util.option_map v.policy_type_name
             (fun f -> Query.Pair ("PolicyTypeName", (String.to_query f)));
           Util.option_map v.policy_name
             (fun f -> Query.Pair ("PolicyName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("policy_attribute_descriptions",
                (PolicyAttributeDescriptions.to_json
                   v.policy_attribute_descriptions));
           Util.option_map v.policy_type_name
             (fun f -> ("policy_type_name", (String.to_json f)));
           Util.option_map v.policy_name
             (fun f -> ("policy_name", (String.to_json f)))])
    let of_json j =
      {
        policy_name =
          (Util.option_map (Json.lookup j "policy_name") String.of_json);
        policy_type_name =
          (Util.option_map (Json.lookup j "policy_type_name") String.of_json);
        policy_attribute_descriptions =
          (PolicyAttributeDescriptions.of_json
             (Util.of_option_exn
                (Json.lookup j "policy_attribute_descriptions")))
      }
  end
module InstanceState =
  struct
    type t =
      {
      instance_id: String.t option ;
      state: String.t option ;
      reason_code: String.t option ;
      description: String.t option }
    let make ?instance_id  ?state  ?reason_code  ?description  () =
      { instance_id; state; reason_code; description }
    let parse xml =
      Some
        {
          instance_id =
            (Util.option_bind (Xml.member "InstanceId" xml) String.parse);
          state = (Util.option_bind (Xml.member "State" xml) String.parse);
          reason_code =
            (Util.option_bind (Xml.member "ReasonCode" xml) String.parse);
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.description
              (fun f -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.reason_code
             (fun f -> Query.Pair ("ReasonCode", (String.to_query f)));
           Util.option_map v.state
             (fun f -> Query.Pair ("State", (String.to_query f)));
           Util.option_map v.instance_id
             (fun f -> Query.Pair ("InstanceId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.description
              (fun f -> ("description", (String.to_json f)));
           Util.option_map v.reason_code
             (fun f -> ("reason_code", (String.to_json f)));
           Util.option_map v.state (fun f -> ("state", (String.to_json f)));
           Util.option_map v.instance_id
             (fun f -> ("instance_id", (String.to_json f)))])
    let of_json j =
      {
        instance_id =
          (Util.option_map (Json.lookup j "instance_id") String.of_json);
        state = (Util.option_map (Json.lookup j "state") String.of_json);
        reason_code =
          (Util.option_map (Json.lookup j "reason_code") String.of_json);
        description =
          (Util.option_map (Json.lookup j "description") String.of_json)
      }
  end
module PolicyTypeDescription =
  struct
    type t =
      {
      policy_type_name: String.t option ;
      description: String.t option ;
      policy_attribute_type_descriptions: PolicyAttributeTypeDescriptions.t }
    let make ?policy_type_name  ?description 
      ?(policy_attribute_type_descriptions= [])  () =
      { policy_type_name; description; policy_attribute_type_descriptions }
    let parse xml =
      Some
        {
          policy_type_name =
            (Util.option_bind (Xml.member "PolicyTypeName" xml) String.parse);
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse);
          policy_attribute_type_descriptions =
            (Util.of_option []
               (Util.option_bind
                  (Xml.member "PolicyAttributeTypeDescriptions" xml)
                  PolicyAttributeTypeDescriptions.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("PolicyAttributeTypeDescriptions.member",
                   (PolicyAttributeTypeDescriptions.to_query
                      v.policy_attribute_type_descriptions)));
           Util.option_map v.description
             (fun f -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.policy_type_name
             (fun f -> Query.Pair ("PolicyTypeName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("policy_attribute_type_descriptions",
                (PolicyAttributeTypeDescriptions.to_json
                   v.policy_attribute_type_descriptions));
           Util.option_map v.description
             (fun f -> ("description", (String.to_json f)));
           Util.option_map v.policy_type_name
             (fun f -> ("policy_type_name", (String.to_json f)))])
    let of_json j =
      {
        policy_type_name =
          (Util.option_map (Json.lookup j "policy_type_name") String.of_json);
        description =
          (Util.option_map (Json.lookup j "description") String.of_json);
        policy_attribute_type_descriptions =
          (PolicyAttributeTypeDescriptions.of_json
             (Util.of_option_exn
                (Json.lookup j "policy_attribute_type_descriptions")))
      }
  end
module LoadBalancerDescription =
  struct
    type t =
      {
      load_balancer_name: String.t option ;
      d_n_s_name: String.t option ;
      canonical_hosted_zone_name: String.t option ;
      canonical_hosted_zone_name_i_d: String.t option ;
      listener_descriptions: ListenerDescriptions.t ;
      policies: Policies.t option ;
      backend_server_descriptions: BackendServerDescriptions.t ;
      availability_zones: AvailabilityZones.t ;
      subnets: Subnets.t ;
      v_p_c_id: String.t option ;
      instances: Instances.t ;
      health_check: HealthCheck.t option ;
      source_security_group: SourceSecurityGroup.t option ;
      security_groups: SecurityGroups.t ;
      created_time: DateTime.t option ;
      scheme: String.t option }
    let make ?load_balancer_name  ?d_n_s_name  ?canonical_hosted_zone_name 
      ?canonical_hosted_zone_name_i_d  ?(listener_descriptions= []) 
      ?policies  ?(backend_server_descriptions= [])  ?(availability_zones=
      [])  ?(subnets= [])  ?v_p_c_id  ?(instances= [])  ?health_check 
      ?source_security_group  ?(security_groups= [])  ?created_time  ?scheme 
      () =
      {
        load_balancer_name;
        d_n_s_name;
        canonical_hosted_zone_name;
        canonical_hosted_zone_name_i_d;
        listener_descriptions;
        policies;
        backend_server_descriptions;
        availability_zones;
        subnets;
        v_p_c_id;
        instances;
        health_check;
        source_security_group;
        security_groups;
        created_time;
        scheme
      }
    let parse xml =
      Some
        {
          load_balancer_name =
            (Util.option_bind (Xml.member "LoadBalancerName" xml)
               String.parse);
          d_n_s_name =
            (Util.option_bind (Xml.member "DNSName" xml) String.parse);
          canonical_hosted_zone_name =
            (Util.option_bind (Xml.member "CanonicalHostedZoneName" xml)
               String.parse);
          canonical_hosted_zone_name_i_d =
            (Util.option_bind (Xml.member "CanonicalHostedZoneNameID" xml)
               String.parse);
          listener_descriptions =
            (Util.of_option []
               (Util.option_bind (Xml.member "ListenerDescriptions" xml)
                  ListenerDescriptions.parse));
          policies =
            (Util.option_bind (Xml.member "Policies" xml) Policies.parse);
          backend_server_descriptions =
            (Util.of_option []
               (Util.option_bind (Xml.member "BackendServerDescriptions" xml)
                  BackendServerDescriptions.parse));
          availability_zones =
            (Util.of_option []
               (Util.option_bind (Xml.member "AvailabilityZones" xml)
                  AvailabilityZones.parse));
          subnets =
            (Util.of_option []
               (Util.option_bind (Xml.member "Subnets" xml) Subnets.parse));
          v_p_c_id = (Util.option_bind (Xml.member "VPCId" xml) String.parse);
          instances =
            (Util.of_option []
               (Util.option_bind (Xml.member "Instances" xml) Instances.parse));
          health_check =
            (Util.option_bind (Xml.member "HealthCheck" xml)
               HealthCheck.parse);
          source_security_group =
            (Util.option_bind (Xml.member "SourceSecurityGroup" xml)
               SourceSecurityGroup.parse);
          security_groups =
            (Util.of_option []
               (Util.option_bind (Xml.member "SecurityGroups" xml)
                  SecurityGroups.parse));
          created_time =
            (Util.option_bind (Xml.member "CreatedTime" xml) DateTime.parse);
          scheme = (Util.option_bind (Xml.member "Scheme" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.scheme
              (fun f -> Query.Pair ("Scheme", (String.to_query f)));
           Util.option_map v.created_time
             (fun f -> Query.Pair ("CreatedTime", (DateTime.to_query f)));
           Some
             (Query.Pair
                ("SecurityGroups.member",
                  (SecurityGroups.to_query v.security_groups)));
           Util.option_map v.source_security_group
             (fun f ->
                Query.Pair
                  ("SourceSecurityGroup", (SourceSecurityGroup.to_query f)));
           Util.option_map v.health_check
             (fun f -> Query.Pair ("HealthCheck", (HealthCheck.to_query f)));
           Some
             (Query.Pair
                ("Instances.member", (Instances.to_query v.instances)));
           Util.option_map v.v_p_c_id
             (fun f -> Query.Pair ("VPCId", (String.to_query f)));
           Some (Query.Pair ("Subnets.member", (Subnets.to_query v.subnets)));
           Some
             (Query.Pair
                ("AvailabilityZones.member",
                  (AvailabilityZones.to_query v.availability_zones)));
           Some
             (Query.Pair
                ("BackendServerDescriptions.member",
                  (BackendServerDescriptions.to_query
                     v.backend_server_descriptions)));
           Util.option_map v.policies
             (fun f -> Query.Pair ("Policies", (Policies.to_query f)));
           Some
             (Query.Pair
                ("ListenerDescriptions.member",
                  (ListenerDescriptions.to_query v.listener_descriptions)));
           Util.option_map v.canonical_hosted_zone_name_i_d
             (fun f ->
                Query.Pair ("CanonicalHostedZoneNameID", (String.to_query f)));
           Util.option_map v.canonical_hosted_zone_name
             (fun f ->
                Query.Pair ("CanonicalHostedZoneName", (String.to_query f)));
           Util.option_map v.d_n_s_name
             (fun f -> Query.Pair ("DNSName", (String.to_query f)));
           Util.option_map v.load_balancer_name
             (fun f -> Query.Pair ("LoadBalancerName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.scheme
              (fun f -> ("scheme", (String.to_json f)));
           Util.option_map v.created_time
             (fun f -> ("created_time", (DateTime.to_json f)));
           Some
             ("security_groups", (SecurityGroups.to_json v.security_groups));
           Util.option_map v.source_security_group
             (fun f ->
                ("source_security_group", (SourceSecurityGroup.to_json f)));
           Util.option_map v.health_check
             (fun f -> ("health_check", (HealthCheck.to_json f)));
           Some ("instances", (Instances.to_json v.instances));
           Util.option_map v.v_p_c_id
             (fun f -> ("v_p_c_id", (String.to_json f)));
           Some ("subnets", (Subnets.to_json v.subnets));
           Some
             ("availability_zones",
               (AvailabilityZones.to_json v.availability_zones));
           Some
             ("backend_server_descriptions",
               (BackendServerDescriptions.to_json
                  v.backend_server_descriptions));
           Util.option_map v.policies
             (fun f -> ("policies", (Policies.to_json f)));
           Some
             ("listener_descriptions",
               (ListenerDescriptions.to_json v.listener_descriptions));
           Util.option_map v.canonical_hosted_zone_name_i_d
             (fun f -> ("canonical_hosted_zone_name_i_d", (String.to_json f)));
           Util.option_map v.canonical_hosted_zone_name
             (fun f -> ("canonical_hosted_zone_name", (String.to_json f)));
           Util.option_map v.d_n_s_name
             (fun f -> ("d_n_s_name", (String.to_json f)));
           Util.option_map v.load_balancer_name
             (fun f -> ("load_balancer_name", (String.to_json f)))])
    let of_json j =
      {
        load_balancer_name =
          (Util.option_map (Json.lookup j "load_balancer_name")
             String.of_json);
        d_n_s_name =
          (Util.option_map (Json.lookup j "d_n_s_name") String.of_json);
        canonical_hosted_zone_name =
          (Util.option_map (Json.lookup j "canonical_hosted_zone_name")
             String.of_json);
        canonical_hosted_zone_name_i_d =
          (Util.option_map (Json.lookup j "canonical_hosted_zone_name_i_d")
             String.of_json);
        listener_descriptions =
          (ListenerDescriptions.of_json
             (Util.of_option_exn (Json.lookup j "listener_descriptions")));
        policies =
          (Util.option_map (Json.lookup j "policies") Policies.of_json);
        backend_server_descriptions =
          (BackendServerDescriptions.of_json
             (Util.of_option_exn
                (Json.lookup j "backend_server_descriptions")));
        availability_zones =
          (AvailabilityZones.of_json
             (Util.of_option_exn (Json.lookup j "availability_zones")));
        subnets =
          (Subnets.of_json (Util.of_option_exn (Json.lookup j "subnets")));
        v_p_c_id =
          (Util.option_map (Json.lookup j "v_p_c_id") String.of_json);
        instances =
          (Instances.of_json (Util.of_option_exn (Json.lookup j "instances")));
        health_check =
          (Util.option_map (Json.lookup j "health_check") HealthCheck.of_json);
        source_security_group =
          (Util.option_map (Json.lookup j "source_security_group")
             SourceSecurityGroup.of_json);
        security_groups =
          (SecurityGroups.of_json
             (Util.of_option_exn (Json.lookup j "security_groups")));
        created_time =
          (Util.option_map (Json.lookup j "created_time") DateTime.of_json);
        scheme = (Util.option_map (Json.lookup j "scheme") String.of_json)
      }
  end
module TagDescriptions =
  struct
    type t = TagDescription.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map TagDescription.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list TagDescription.to_query v
    let to_json v = `List (List.map TagDescription.to_json v)
    let of_json j = Json.to_list TagDescription.of_json j
  end
module LoadBalancerAttributes =
  struct
    type t =
      {
      cross_zone_load_balancing: CrossZoneLoadBalancing.t option ;
      access_log: AccessLog.t option ;
      connection_draining: ConnectionDraining.t option ;
      connection_settings: ConnectionSettings.t option ;
      additional_attributes: AdditionalAttributes.t }
    let make ?cross_zone_load_balancing  ?access_log  ?connection_draining 
      ?connection_settings  ?(additional_attributes= [])  () =
      {
        cross_zone_load_balancing;
        access_log;
        connection_draining;
        connection_settings;
        additional_attributes
      }
    let parse xml =
      Some
        {
          cross_zone_load_balancing =
            (Util.option_bind (Xml.member "CrossZoneLoadBalancing" xml)
               CrossZoneLoadBalancing.parse);
          access_log =
            (Util.option_bind (Xml.member "AccessLog" xml) AccessLog.parse);
          connection_draining =
            (Util.option_bind (Xml.member "ConnectionDraining" xml)
               ConnectionDraining.parse);
          connection_settings =
            (Util.option_bind (Xml.member "ConnectionSettings" xml)
               ConnectionSettings.parse);
          additional_attributes =
            (Util.of_option []
               (Util.option_bind (Xml.member "AdditionalAttributes" xml)
                  AdditionalAttributes.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("AdditionalAttributes.member",
                   (AdditionalAttributes.to_query v.additional_attributes)));
           Util.option_map v.connection_settings
             (fun f ->
                Query.Pair
                  ("ConnectionSettings", (ConnectionSettings.to_query f)));
           Util.option_map v.connection_draining
             (fun f ->
                Query.Pair
                  ("ConnectionDraining", (ConnectionDraining.to_query f)));
           Util.option_map v.access_log
             (fun f -> Query.Pair ("AccessLog", (AccessLog.to_query f)));
           Util.option_map v.cross_zone_load_balancing
             (fun f ->
                Query.Pair
                  ("CrossZoneLoadBalancing",
                    (CrossZoneLoadBalancing.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("additional_attributes",
                (AdditionalAttributes.to_json v.additional_attributes));
           Util.option_map v.connection_settings
             (fun f ->
                ("connection_settings", (ConnectionSettings.to_json f)));
           Util.option_map v.connection_draining
             (fun f ->
                ("connection_draining", (ConnectionDraining.to_json f)));
           Util.option_map v.access_log
             (fun f -> ("access_log", (AccessLog.to_json f)));
           Util.option_map v.cross_zone_load_balancing
             (fun f ->
                ("cross_zone_load_balancing",
                  (CrossZoneLoadBalancing.to_json f)))])
    let of_json j =
      {
        cross_zone_load_balancing =
          (Util.option_map (Json.lookup j "cross_zone_load_balancing")
             CrossZoneLoadBalancing.of_json);
        access_log =
          (Util.option_map (Json.lookup j "access_log") AccessLog.of_json);
        connection_draining =
          (Util.option_map (Json.lookup j "connection_draining")
             ConnectionDraining.of_json);
        connection_settings =
          (Util.option_map (Json.lookup j "connection_settings")
             ConnectionSettings.of_json);
        additional_attributes =
          (AdditionalAttributes.of_json
             (Util.of_option_exn (Json.lookup j "additional_attributes")))
      }
  end
module LoadBalancerNames =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module Listeners =
  struct
    type t = Listener.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Listener.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Listener.to_query v
    let to_json v = `List (List.map Listener.to_json v)
    let of_json j = Json.to_list Listener.of_json j
  end
module PolicyAttributes =
  struct
    type t = PolicyAttribute.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map PolicyAttribute.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list PolicyAttribute.to_query v
    let to_json v = `List (List.map PolicyAttribute.to_json v)
    let of_json j = Json.to_list PolicyAttribute.of_json j
  end
module TagKeyList =
  struct
    type t = TagKeyOnly.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map TagKeyOnly.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list TagKeyOnly.to_query v
    let to_json v = `List (List.map TagKeyOnly.to_json v)
    let of_json j = Json.to_list TagKeyOnly.of_json j
  end
module PolicyDescriptions =
  struct
    type t = PolicyDescription.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map PolicyDescription.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list PolicyDescription.to_query v
    let to_json v = `List (List.map PolicyDescription.to_json v)
    let of_json j = Json.to_list PolicyDescription.of_json j
  end
module InstanceStates =
  struct
    type t = InstanceState.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map InstanceState.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list InstanceState.to_query v
    let to_json v = `List (List.map InstanceState.to_json v)
    let of_json j = Json.to_list InstanceState.of_json j
  end
module LoadBalancerNamesMax20 =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module PolicyTypeNames =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module Ports =
  struct
    type t = Integer.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Integer.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Integer.to_query v
    let to_json v = `List (List.map Integer.to_json v)
    let of_json j = Json.to_list Integer.of_json j
  end
module PolicyTypeDescriptions =
  struct
    type t = PolicyTypeDescription.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map PolicyTypeDescription.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list PolicyTypeDescription.to_query v
    let to_json v = `List (List.map PolicyTypeDescription.to_json v)
    let of_json j = Json.to_list PolicyTypeDescription.of_json j
  end
module LoadBalancerDescriptions =
  struct
    type t = LoadBalancerDescription.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map LoadBalancerDescription.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list LoadBalancerDescription.to_query v
    let to_json v = `List (List.map LoadBalancerDescription.to_json v)
    let of_json j = Json.to_list LoadBalancerDescription.of_json j
  end
module DescribeTagsOutput =
  struct
    type t = {
      tag_descriptions: TagDescriptions.t }
    let make ?(tag_descriptions= [])  () = { tag_descriptions }
    let parse xml =
      Some
        {
          tag_descriptions =
            (Util.of_option []
               (Util.option_bind (Xml.member "TagDescriptions" xml)
                  TagDescriptions.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("TagDescriptions.member",
                   (TagDescriptions.to_query v.tag_descriptions)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("tag_descriptions",
                (TagDescriptions.to_json v.tag_descriptions))])
    let of_json j =
      {
        tag_descriptions =
          (TagDescriptions.of_json
             (Util.of_option_exn (Json.lookup j "tag_descriptions")))
      }
  end
module ConfigureHealthCheckOutput =
  struct
    type t = {
      health_check: HealthCheck.t option }
    let make ?health_check  () = { health_check }
    let parse xml =
      Some
        {
          health_check =
            (Util.option_bind (Xml.member "HealthCheck" xml)
               HealthCheck.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.health_check
              (fun f -> Query.Pair ("HealthCheck", (HealthCheck.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.health_check
              (fun f -> ("health_check", (HealthCheck.to_json f)))])
    let of_json j =
      {
        health_check =
          (Util.option_map (Json.lookup j "health_check") HealthCheck.of_json)
      }
  end
module ModifyLoadBalancerAttributesOutput =
  struct
    type t =
      {
      load_balancer_name: String.t option ;
      load_balancer_attributes: LoadBalancerAttributes.t option }
    let make ?load_balancer_name  ?load_balancer_attributes  () =
      { load_balancer_name; load_balancer_attributes }
    let parse xml =
      Some
        {
          load_balancer_name =
            (Util.option_bind (Xml.member "LoadBalancerName" xml)
               String.parse);
          load_balancer_attributes =
            (Util.option_bind (Xml.member "LoadBalancerAttributes" xml)
               LoadBalancerAttributes.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.load_balancer_attributes
              (fun f ->
                 Query.Pair
                   ("LoadBalancerAttributes",
                     (LoadBalancerAttributes.to_query f)));
           Util.option_map v.load_balancer_name
             (fun f -> Query.Pair ("LoadBalancerName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.load_balancer_attributes
              (fun f ->
                 ("load_balancer_attributes",
                   (LoadBalancerAttributes.to_json f)));
           Util.option_map v.load_balancer_name
             (fun f -> ("load_balancer_name", (String.to_json f)))])
    let of_json j =
      {
        load_balancer_name =
          (Util.option_map (Json.lookup j "load_balancer_name")
             String.of_json);
        load_balancer_attributes =
          (Util.option_map (Json.lookup j "load_balancer_attributes")
             LoadBalancerAttributes.of_json)
      }
  end
module SetLoadBalancerPoliciesForBackendServerOutput =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module AddTagsInput =
  struct
    type t = {
      load_balancer_names: LoadBalancerNames.t ;
      tags: TagList.t }
    let make ~load_balancer_names  ~tags  () = { load_balancer_names; tags }
    let parse xml =
      Some
        {
          load_balancer_names =
            (Xml.required "LoadBalancerNames"
               (Util.option_bind (Xml.member "LoadBalancerNames" xml)
                  LoadBalancerNames.parse));
          tags =
            (Xml.required "Tags"
               (Util.option_bind (Xml.member "Tags" xml) TagList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Tags.member", (TagList.to_query v.tags)));
           Some
             (Query.Pair
                ("LoadBalancerNames.member",
                  (LoadBalancerNames.to_query v.load_balancer_names)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tags", (TagList.to_json v.tags));
           Some
             ("load_balancer_names",
               (LoadBalancerNames.to_json v.load_balancer_names))])
    let of_json j =
      {
        load_balancer_names =
          (LoadBalancerNames.of_json
             (Util.of_option_exn (Json.lookup j "load_balancer_names")));
        tags = (TagList.of_json (Util.of_option_exn (Json.lookup j "tags")))
      }
  end
module CreateLoadBalancerListenerInput =
  struct
    type t = {
      load_balancer_name: String.t ;
      listeners: Listeners.t }
    let make ~load_balancer_name  ~listeners  () =
      { load_balancer_name; listeners }
    let parse xml =
      Some
        {
          load_balancer_name =
            (Xml.required "LoadBalancerName"
               (Util.option_bind (Xml.member "LoadBalancerName" xml)
                  String.parse));
          listeners =
            (Xml.required "Listeners"
               (Util.option_bind (Xml.member "Listeners" xml) Listeners.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Listeners.member", (Listeners.to_query v.listeners)));
           Some
             (Query.Pair
                ("LoadBalancerName", (String.to_query v.load_balancer_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("listeners", (Listeners.to_json v.listeners));
           Some ("load_balancer_name", (String.to_json v.load_balancer_name))])
    let of_json j =
      {
        load_balancer_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "load_balancer_name")));
        listeners =
          (Listeners.of_json (Util.of_option_exn (Json.lookup j "listeners")))
      }
  end
module CreateLoadBalancerPolicyInput =
  struct
    type t =
      {
      load_balancer_name: String.t ;
      policy_name: String.t ;
      policy_type_name: String.t ;
      policy_attributes: PolicyAttributes.t }
    let make ~load_balancer_name  ~policy_name  ~policy_type_name 
      ?(policy_attributes= [])  () =
      { load_balancer_name; policy_name; policy_type_name; policy_attributes
      }
    let parse xml =
      Some
        {
          load_balancer_name =
            (Xml.required "LoadBalancerName"
               (Util.option_bind (Xml.member "LoadBalancerName" xml)
                  String.parse));
          policy_name =
            (Xml.required "PolicyName"
               (Util.option_bind (Xml.member "PolicyName" xml) String.parse));
          policy_type_name =
            (Xml.required "PolicyTypeName"
               (Util.option_bind (Xml.member "PolicyTypeName" xml)
                  String.parse));
          policy_attributes =
            (Util.of_option []
               (Util.option_bind (Xml.member "PolicyAttributes" xml)
                  PolicyAttributes.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("PolicyAttributes.member",
                   (PolicyAttributes.to_query v.policy_attributes)));
           Some
             (Query.Pair
                ("PolicyTypeName", (String.to_query v.policy_type_name)));
           Some (Query.Pair ("PolicyName", (String.to_query v.policy_name)));
           Some
             (Query.Pair
                ("LoadBalancerName", (String.to_query v.load_balancer_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("policy_attributes",
                (PolicyAttributes.to_json v.policy_attributes));
           Some ("policy_type_name", (String.to_json v.policy_type_name));
           Some ("policy_name", (String.to_json v.policy_name));
           Some ("load_balancer_name", (String.to_json v.load_balancer_name))])
    let of_json j =
      {
        load_balancer_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "load_balancer_name")));
        policy_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "policy_name")));
        policy_type_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "policy_type_name")));
        policy_attributes =
          (PolicyAttributes.of_json
             (Util.of_option_exn (Json.lookup j "policy_attributes")))
      }
  end
module DuplicateTagKeysException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CreateAccessPointOutput =
  struct
    type t = {
      d_n_s_name: String.t option }
    let make ?d_n_s_name  () = { d_n_s_name }
    let parse xml =
      Some
        {
          d_n_s_name =
            (Util.option_bind (Xml.member "DNSName" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.d_n_s_name
              (fun f -> Query.Pair ("DNSName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.d_n_s_name
              (fun f -> ("d_n_s_name", (String.to_json f)))])
    let of_json j =
      {
        d_n_s_name =
          (Util.option_map (Json.lookup j "d_n_s_name") String.of_json)
      }
  end
module DescribeLoadBalancerPoliciesInput =
  struct
    type t =
      {
      load_balancer_name: String.t option ;
      policy_names: PolicyNames.t }
    let make ?load_balancer_name  ?(policy_names= [])  () =
      { load_balancer_name; policy_names }
    let parse xml =
      Some
        {
          load_balancer_name =
            (Util.option_bind (Xml.member "LoadBalancerName" xml)
               String.parse);
          policy_names =
            (Util.of_option []
               (Util.option_bind (Xml.member "PolicyNames" xml)
                  PolicyNames.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("PolicyNames.member",
                   (PolicyNames.to_query v.policy_names)));
           Util.option_map v.load_balancer_name
             (fun f -> Query.Pair ("LoadBalancerName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("policy_names", (PolicyNames.to_json v.policy_names));
           Util.option_map v.load_balancer_name
             (fun f -> ("load_balancer_name", (String.to_json f)))])
    let of_json j =
      {
        load_balancer_name =
          (Util.option_map (Json.lookup j "load_balancer_name")
             String.of_json);
        policy_names =
          (PolicyNames.of_json
             (Util.of_option_exn (Json.lookup j "policy_names")))
      }
  end
module LoadBalancerAttributeNotFoundException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module TooManyTagsException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module AttachLoadBalancerToSubnetsInput =
  struct
    type t = {
      load_balancer_name: String.t ;
      subnets: Subnets.t }
    let make ~load_balancer_name  ~subnets  () =
      { load_balancer_name; subnets }
    let parse xml =
      Some
        {
          load_balancer_name =
            (Xml.required "LoadBalancerName"
               (Util.option_bind (Xml.member "LoadBalancerName" xml)
                  String.parse));
          subnets =
            (Xml.required "Subnets"
               (Util.option_bind (Xml.member "Subnets" xml) Subnets.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("Subnets.member", (Subnets.to_query v.subnets)));
           Some
             (Query.Pair
                ("LoadBalancerName", (String.to_query v.load_balancer_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("subnets", (Subnets.to_json v.subnets));
           Some ("load_balancer_name", (String.to_json v.load_balancer_name))])
    let of_json j =
      {
        load_balancer_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "load_balancer_name")));
        subnets =
          (Subnets.of_json (Util.of_option_exn (Json.lookup j "subnets")))
      }
  end
module DeregisterEndPointsInput =
  struct
    type t = {
      load_balancer_name: String.t ;
      instances: Instances.t }
    let make ~load_balancer_name  ~instances  () =
      { load_balancer_name; instances }
    let parse xml =
      Some
        {
          load_balancer_name =
            (Xml.required "LoadBalancerName"
               (Util.option_bind (Xml.member "LoadBalancerName" xml)
                  String.parse));
          instances =
            (Xml.required "Instances"
               (Util.option_bind (Xml.member "Instances" xml) Instances.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Instances.member", (Instances.to_query v.instances)));
           Some
             (Query.Pair
                ("LoadBalancerName", (String.to_query v.load_balancer_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("instances", (Instances.to_json v.instances));
           Some ("load_balancer_name", (String.to_json v.load_balancer_name))])
    let of_json j =
      {
        load_balancer_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "load_balancer_name")));
        instances =
          (Instances.of_json (Util.of_option_exn (Json.lookup j "instances")))
      }
  end
module AddAvailabilityZonesOutput =
  struct
    type t = {
      availability_zones: AvailabilityZones.t }
    let make ?(availability_zones= [])  () = { availability_zones }
    let parse xml =
      Some
        {
          availability_zones =
            (Util.of_option []
               (Util.option_bind (Xml.member "AvailabilityZones" xml)
                  AvailabilityZones.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("AvailabilityZones.member",
                   (AvailabilityZones.to_query v.availability_zones)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("availability_zones",
                (AvailabilityZones.to_json v.availability_zones))])
    let of_json j =
      {
        availability_zones =
          (AvailabilityZones.of_json
             (Util.of_option_exn (Json.lookup j "availability_zones")))
      }
  end
module ListenerNotFoundException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CreateLoadBalancerPolicyOutput =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module InvalidEndPointException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module SubnetNotFoundException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module RemoveTagsInput =
  struct
    type t = {
      load_balancer_names: LoadBalancerNames.t ;
      tags: TagKeyList.t }
    let make ~load_balancer_names  ~tags  () = { load_balancer_names; tags }
    let parse xml =
      Some
        {
          load_balancer_names =
            (Xml.required "LoadBalancerNames"
               (Util.option_bind (Xml.member "LoadBalancerNames" xml)
                  LoadBalancerNames.parse));
          tags =
            (Xml.required "Tags"
               (Util.option_bind (Xml.member "Tags" xml) TagKeyList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Tags.member", (TagKeyList.to_query v.tags)));
           Some
             (Query.Pair
                ("LoadBalancerNames.member",
                  (LoadBalancerNames.to_query v.load_balancer_names)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tags", (TagKeyList.to_json v.tags));
           Some
             ("load_balancer_names",
               (LoadBalancerNames.to_json v.load_balancer_names))])
    let of_json j =
      {
        load_balancer_names =
          (LoadBalancerNames.of_json
             (Util.of_option_exn (Json.lookup j "load_balancer_names")));
        tags =
          (TagKeyList.of_json (Util.of_option_exn (Json.lookup j "tags")))
      }
  end
module SetLoadBalancerPoliciesForBackendServerInput =
  struct
    type t =
      {
      load_balancer_name: String.t ;
      instance_port: Integer.t ;
      policy_names: PolicyNames.t }
    let make ~load_balancer_name  ~instance_port  ~policy_names  () =
      { load_balancer_name; instance_port; policy_names }
    let parse xml =
      Some
        {
          load_balancer_name =
            (Xml.required "LoadBalancerName"
               (Util.option_bind (Xml.member "LoadBalancerName" xml)
                  String.parse));
          instance_port =
            (Xml.required "InstancePort"
               (Util.option_bind (Xml.member "InstancePort" xml)
                  Integer.parse));
          policy_names =
            (Xml.required "PolicyNames"
               (Util.option_bind (Xml.member "PolicyNames" xml)
                  PolicyNames.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("PolicyNames.member",
                   (PolicyNames.to_query v.policy_names)));
           Some
             (Query.Pair ("InstancePort", (Integer.to_query v.instance_port)));
           Some
             (Query.Pair
                ("LoadBalancerName", (String.to_query v.load_balancer_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("policy_names", (PolicyNames.to_json v.policy_names));
           Some ("instance_port", (Integer.to_json v.instance_port));
           Some ("load_balancer_name", (String.to_json v.load_balancer_name))])
    let of_json j =
      {
        load_balancer_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "load_balancer_name")));
        instance_port =
          (Integer.of_json
             (Util.of_option_exn (Json.lookup j "instance_port")));
        policy_names =
          (PolicyNames.of_json
             (Util.of_option_exn (Json.lookup j "policy_names")))
      }
  end
module SetLoadBalancerListenerSSLCertificateOutput =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module TooManyAccessPointsException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CreateAccessPointInput =
  struct
    type t =
      {
      load_balancer_name: String.t ;
      listeners: Listeners.t ;
      availability_zones: AvailabilityZones.t ;
      subnets: Subnets.t ;
      security_groups: SecurityGroups.t ;
      scheme: String.t option ;
      tags: TagList.t }
    let make ~load_balancer_name  ~listeners  ?(availability_zones= []) 
      ?(subnets= [])  ?(security_groups= [])  ?scheme  ?(tags= [])  () =
      {
        load_balancer_name;
        listeners;
        availability_zones;
        subnets;
        security_groups;
        scheme;
        tags
      }
    let parse xml =
      Some
        {
          load_balancer_name =
            (Xml.required "LoadBalancerName"
               (Util.option_bind (Xml.member "LoadBalancerName" xml)
                  String.parse));
          listeners =
            (Xml.required "Listeners"
               (Util.option_bind (Xml.member "Listeners" xml) Listeners.parse));
          availability_zones =
            (Util.of_option []
               (Util.option_bind (Xml.member "AvailabilityZones" xml)
                  AvailabilityZones.parse));
          subnets =
            (Util.of_option []
               (Util.option_bind (Xml.member "Subnets" xml) Subnets.parse));
          security_groups =
            (Util.of_option []
               (Util.option_bind (Xml.member "SecurityGroups" xml)
                  SecurityGroups.parse));
          scheme = (Util.option_bind (Xml.member "Scheme" xml) String.parse);
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) TagList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Tags.member", (TagList.to_query v.tags)));
           Util.option_map v.scheme
             (fun f -> Query.Pair ("Scheme", (String.to_query f)));
           Some
             (Query.Pair
                ("SecurityGroups.member",
                  (SecurityGroups.to_query v.security_groups)));
           Some (Query.Pair ("Subnets.member", (Subnets.to_query v.subnets)));
           Some
             (Query.Pair
                ("AvailabilityZones.member",
                  (AvailabilityZones.to_query v.availability_zones)));
           Some
             (Query.Pair
                ("Listeners.member", (Listeners.to_query v.listeners)));
           Some
             (Query.Pair
                ("LoadBalancerName", (String.to_query v.load_balancer_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tags", (TagList.to_json v.tags));
           Util.option_map v.scheme (fun f -> ("scheme", (String.to_json f)));
           Some
             ("security_groups", (SecurityGroups.to_json v.security_groups));
           Some ("subnets", (Subnets.to_json v.subnets));
           Some
             ("availability_zones",
               (AvailabilityZones.to_json v.availability_zones));
           Some ("listeners", (Listeners.to_json v.listeners));
           Some ("load_balancer_name", (String.to_json v.load_balancer_name))])
    let of_json j =
      {
        load_balancer_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "load_balancer_name")));
        listeners =
          (Listeners.of_json (Util.of_option_exn (Json.lookup j "listeners")));
        availability_zones =
          (AvailabilityZones.of_json
             (Util.of_option_exn (Json.lookup j "availability_zones")));
        subnets =
          (Subnets.of_json (Util.of_option_exn (Json.lookup j "subnets")));
        security_groups =
          (SecurityGroups.of_json
             (Util.of_option_exn (Json.lookup j "security_groups")));
        scheme = (Util.option_map (Json.lookup j "scheme") String.of_json);
        tags = (TagList.of_json (Util.of_option_exn (Json.lookup j "tags")))
      }
  end
module DescribeAccessPointsInput =
  struct
    type t =
      {
      load_balancer_names: LoadBalancerNames.t ;
      marker: String.t option ;
      page_size: Integer.t option }
    let make ?(load_balancer_names= [])  ?marker  ?page_size  () =
      { load_balancer_names; marker; page_size }
    let parse xml =
      Some
        {
          load_balancer_names =
            (Util.of_option []
               (Util.option_bind (Xml.member "LoadBalancerNames" xml)
                  LoadBalancerNames.parse));
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          page_size =
            (Util.option_bind (Xml.member "PageSize" xml) Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.page_size
              (fun f -> Query.Pair ("PageSize", (Integer.to_query f)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Some
             (Query.Pair
                ("LoadBalancerNames.member",
                  (LoadBalancerNames.to_query v.load_balancer_names)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.page_size
              (fun f -> ("page_size", (Integer.to_json f)));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)));
           Some
             ("load_balancer_names",
               (LoadBalancerNames.to_json v.load_balancer_names))])
    let of_json j =
      {
        load_balancer_names =
          (LoadBalancerNames.of_json
             (Util.of_option_exn (Json.lookup j "load_balancer_names")));
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        page_size =
          (Util.option_map (Json.lookup j "page_size") Integer.of_json)
      }
  end
module InvalidSchemeException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module SetLoadBalancerPoliciesOfListenerOutput =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module InvalidSecurityGroupException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CreateLoadBalancerListenerOutput =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DetachLoadBalancerFromSubnetsOutput =
  struct
    type t = {
      subnets: Subnets.t }
    let make ?(subnets= [])  () = { subnets }
    let parse xml =
      Some
        {
          subnets =
            (Util.of_option []
               (Util.option_bind (Xml.member "Subnets" xml) Subnets.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("Subnets.member", (Subnets.to_query v.subnets)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("subnets", (Subnets.to_json v.subnets))])
    let of_json j =
      {
        subnets =
          (Subnets.of_json (Util.of_option_exn (Json.lookup j "subnets")))
      }
  end
module DescribeLoadBalancerAttributesInput =
  struct
    type t = {
      load_balancer_name: String.t }
    let make ~load_balancer_name  () = { load_balancer_name }
    let parse xml =
      Some
        {
          load_balancer_name =
            (Xml.required "LoadBalancerName"
               (Util.option_bind (Xml.member "LoadBalancerName" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("LoadBalancerName", (String.to_query v.load_balancer_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("load_balancer_name", (String.to_json v.load_balancer_name))])
    let of_json j =
      {
        load_balancer_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "load_balancer_name")))
      }
  end
module DuplicateListenerException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CertificateNotFoundException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DescribeLoadBalancerPoliciesOutput =
  struct
    type t = {
      policy_descriptions: PolicyDescriptions.t }
    let make ?(policy_descriptions= [])  () = { policy_descriptions }
    let parse xml =
      Some
        {
          policy_descriptions =
            (Util.of_option []
               (Util.option_bind (Xml.member "PolicyDescriptions" xml)
                  PolicyDescriptions.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("PolicyDescriptions.member",
                   (PolicyDescriptions.to_query v.policy_descriptions)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("policy_descriptions",
                (PolicyDescriptions.to_json v.policy_descriptions))])
    let of_json j =
      {
        policy_descriptions =
          (PolicyDescriptions.of_json
             (Util.of_option_exn (Json.lookup j "policy_descriptions")))
      }
  end
module PolicyTypeNotFoundException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module RemoveAvailabilityZonesInput =
  struct
    type t =
      {
      load_balancer_name: String.t ;
      availability_zones: AvailabilityZones.t }
    let make ~load_balancer_name  ~availability_zones  () =
      { load_balancer_name; availability_zones }
    let parse xml =
      Some
        {
          load_balancer_name =
            (Xml.required "LoadBalancerName"
               (Util.option_bind (Xml.member "LoadBalancerName" xml)
                  String.parse));
          availability_zones =
            (Xml.required "AvailabilityZones"
               (Util.option_bind (Xml.member "AvailabilityZones" xml)
                  AvailabilityZones.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("AvailabilityZones.member",
                   (AvailabilityZones.to_query v.availability_zones)));
           Some
             (Query.Pair
                ("LoadBalancerName", (String.to_query v.load_balancer_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("availability_zones",
                (AvailabilityZones.to_json v.availability_zones));
           Some ("load_balancer_name", (String.to_json v.load_balancer_name))])
    let of_json j =
      {
        load_balancer_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "load_balancer_name")));
        availability_zones =
          (AvailabilityZones.of_json
             (Util.of_option_exn (Json.lookup j "availability_zones")))
      }
  end
module InvalidSubnetException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DeleteLoadBalancerPolicyInput =
  struct
    type t = {
      load_balancer_name: String.t ;
      policy_name: String.t }
    let make ~load_balancer_name  ~policy_name  () =
      { load_balancer_name; policy_name }
    let parse xml =
      Some
        {
          load_balancer_name =
            (Xml.required "LoadBalancerName"
               (Util.option_bind (Xml.member "LoadBalancerName" xml)
                  String.parse));
          policy_name =
            (Xml.required "PolicyName"
               (Util.option_bind (Xml.member "PolicyName" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("PolicyName", (String.to_query v.policy_name)));
           Some
             (Query.Pair
                ("LoadBalancerName", (String.to_query v.load_balancer_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("policy_name", (String.to_json v.policy_name));
           Some ("load_balancer_name", (String.to_json v.load_balancer_name))])
    let of_json j =
      {
        load_balancer_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "load_balancer_name")));
        policy_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "policy_name")))
      }
  end
module AttachLoadBalancerToSubnetsOutput =
  struct
    type t = {
      subnets: Subnets.t }
    let make ?(subnets= [])  () = { subnets }
    let parse xml =
      Some
        {
          subnets =
            (Util.of_option []
               (Util.option_bind (Xml.member "Subnets" xml) Subnets.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("Subnets.member", (Subnets.to_query v.subnets)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("subnets", (Subnets.to_json v.subnets))])
    let of_json j =
      {
        subnets =
          (Subnets.of_json (Util.of_option_exn (Json.lookup j "subnets")))
      }
  end
module CreateAppCookieStickinessPolicyOutput =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DescribeEndPointStateOutput =
  struct
    type t = {
      instance_states: InstanceStates.t }
    let make ?(instance_states= [])  () = { instance_states }
    let parse xml =
      Some
        {
          instance_states =
            (Util.of_option []
               (Util.option_bind (Xml.member "InstanceStates" xml)
                  InstanceStates.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("InstanceStates.member",
                   (InstanceStates.to_query v.instance_states)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("instance_states", (InstanceStates.to_json v.instance_states))])
    let of_json j =
      {
        instance_states =
          (InstanceStates.of_json
             (Util.of_option_exn (Json.lookup j "instance_states")))
      }
  end
module AddTagsOutput =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DeleteAccessPointOutput =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module RemoveTagsOutput =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module RemoveAvailabilityZonesOutput =
  struct
    type t = {
      availability_zones: AvailabilityZones.t }
    let make ?(availability_zones= [])  () = { availability_zones }
    let parse xml =
      Some
        {
          availability_zones =
            (Util.of_option []
               (Util.option_bind (Xml.member "AvailabilityZones" xml)
                  AvailabilityZones.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("AvailabilityZones.member",
                   (AvailabilityZones.to_query v.availability_zones)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("availability_zones",
                (AvailabilityZones.to_json v.availability_zones))])
    let of_json j =
      {
        availability_zones =
          (AvailabilityZones.of_json
             (Util.of_option_exn (Json.lookup j "availability_zones")))
      }
  end
module ApplySecurityGroupsToLoadBalancerInput =
  struct
    type t =
      {
      load_balancer_name: String.t ;
      security_groups: SecurityGroups.t }
    let make ~load_balancer_name  ~security_groups  () =
      { load_balancer_name; security_groups }
    let parse xml =
      Some
        {
          load_balancer_name =
            (Xml.required "LoadBalancerName"
               (Util.option_bind (Xml.member "LoadBalancerName" xml)
                  String.parse));
          security_groups =
            (Xml.required "SecurityGroups"
               (Util.option_bind (Xml.member "SecurityGroups" xml)
                  SecurityGroups.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("SecurityGroups.member",
                   (SecurityGroups.to_query v.security_groups)));
           Some
             (Query.Pair
                ("LoadBalancerName", (String.to_query v.load_balancer_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("security_groups", (SecurityGroups.to_json v.security_groups));
           Some ("load_balancer_name", (String.to_json v.load_balancer_name))])
    let of_json j =
      {
        load_balancer_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "load_balancer_name")));
        security_groups =
          (SecurityGroups.of_json
             (Util.of_option_exn (Json.lookup j "security_groups")))
      }
  end
module SetLoadBalancerPoliciesOfListenerInput =
  struct
    type t =
      {
      load_balancer_name: String.t ;
      load_balancer_port: Integer.t ;
      policy_names: PolicyNames.t }
    let make ~load_balancer_name  ~load_balancer_port  ~policy_names  () =
      { load_balancer_name; load_balancer_port; policy_names }
    let parse xml =
      Some
        {
          load_balancer_name =
            (Xml.required "LoadBalancerName"
               (Util.option_bind (Xml.member "LoadBalancerName" xml)
                  String.parse));
          load_balancer_port =
            (Xml.required "LoadBalancerPort"
               (Util.option_bind (Xml.member "LoadBalancerPort" xml)
                  Integer.parse));
          policy_names =
            (Xml.required "PolicyNames"
               (Util.option_bind (Xml.member "PolicyNames" xml)
                  PolicyNames.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("PolicyNames.member",
                   (PolicyNames.to_query v.policy_names)));
           Some
             (Query.Pair
                ("LoadBalancerPort", (Integer.to_query v.load_balancer_port)));
           Some
             (Query.Pair
                ("LoadBalancerName", (String.to_query v.load_balancer_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("policy_names", (PolicyNames.to_json v.policy_names));
           Some
             ("load_balancer_port", (Integer.to_json v.load_balancer_port));
           Some ("load_balancer_name", (String.to_json v.load_balancer_name))])
    let of_json j =
      {
        load_balancer_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "load_balancer_name")));
        load_balancer_port =
          (Integer.of_json
             (Util.of_option_exn (Json.lookup j "load_balancer_port")));
        policy_names =
          (PolicyNames.of_json
             (Util.of_option_exn (Json.lookup j "policy_names")))
      }
  end
module CreateAppCookieStickinessPolicyInput =
  struct
    type t =
      {
      load_balancer_name: String.t ;
      policy_name: String.t ;
      cookie_name: String.t }
    let make ~load_balancer_name  ~policy_name  ~cookie_name  () =
      { load_balancer_name; policy_name; cookie_name }
    let parse xml =
      Some
        {
          load_balancer_name =
            (Xml.required "LoadBalancerName"
               (Util.option_bind (Xml.member "LoadBalancerName" xml)
                  String.parse));
          policy_name =
            (Xml.required "PolicyName"
               (Util.option_bind (Xml.member "PolicyName" xml) String.parse));
          cookie_name =
            (Xml.required "CookieName"
               (Util.option_bind (Xml.member "CookieName" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("CookieName", (String.to_query v.cookie_name)));
           Some (Query.Pair ("PolicyName", (String.to_query v.policy_name)));
           Some
             (Query.Pair
                ("LoadBalancerName", (String.to_query v.load_balancer_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("cookie_name", (String.to_json v.cookie_name));
           Some ("policy_name", (String.to_json v.policy_name));
           Some ("load_balancer_name", (String.to_json v.load_balancer_name))])
    let of_json j =
      {
        load_balancer_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "load_balancer_name")));
        policy_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "policy_name")));
        cookie_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "cookie_name")))
      }
  end
module DescribeTagsInput =
  struct
    type t = {
      load_balancer_names: LoadBalancerNamesMax20.t }
    let make ~load_balancer_names  () = { load_balancer_names }
    let parse xml =
      Some
        {
          load_balancer_names =
            (Xml.required "LoadBalancerNames"
               (Util.option_bind (Xml.member "LoadBalancerNames" xml)
                  LoadBalancerNamesMax20.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("LoadBalancerNames.member",
                   (LoadBalancerNamesMax20.to_query v.load_balancer_names)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("load_balancer_names",
                (LoadBalancerNamesMax20.to_json v.load_balancer_names))])
    let of_json j =
      {
        load_balancer_names =
          (LoadBalancerNamesMax20.of_json
             (Util.of_option_exn (Json.lookup j "load_balancer_names")))
      }
  end
module DeleteLoadBalancerPolicyOutput =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DuplicatePolicyNameException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DeleteLoadBalancerListenerOutput =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module TooManyPoliciesException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ModifyLoadBalancerAttributesInput =
  struct
    type t =
      {
      load_balancer_name: String.t ;
      load_balancer_attributes: LoadBalancerAttributes.t }
    let make ~load_balancer_name  ~load_balancer_attributes  () =
      { load_balancer_name; load_balancer_attributes }
    let parse xml =
      Some
        {
          load_balancer_name =
            (Xml.required "LoadBalancerName"
               (Util.option_bind (Xml.member "LoadBalancerName" xml)
                  String.parse));
          load_balancer_attributes =
            (Xml.required "LoadBalancerAttributes"
               (Util.option_bind (Xml.member "LoadBalancerAttributes" xml)
                  LoadBalancerAttributes.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("LoadBalancerAttributes",
                   (LoadBalancerAttributes.to_query
                      v.load_balancer_attributes)));
           Some
             (Query.Pair
                ("LoadBalancerName", (String.to_query v.load_balancer_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("load_balancer_attributes",
                (LoadBalancerAttributes.to_json v.load_balancer_attributes));
           Some ("load_balancer_name", (String.to_json v.load_balancer_name))])
    let of_json j =
      {
        load_balancer_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "load_balancer_name")));
        load_balancer_attributes =
          (LoadBalancerAttributes.of_json
             (Util.of_option_exn (Json.lookup j "load_balancer_attributes")))
      }
  end
module CreateLBCookieStickinessPolicyInput =
  struct
    type t =
      {
      load_balancer_name: String.t ;
      policy_name: String.t ;
      cookie_expiration_period: Long.t option }
    let make ~load_balancer_name  ~policy_name  ?cookie_expiration_period  ()
      = { load_balancer_name; policy_name; cookie_expiration_period }
    let parse xml =
      Some
        {
          load_balancer_name =
            (Xml.required "LoadBalancerName"
               (Util.option_bind (Xml.member "LoadBalancerName" xml)
                  String.parse));
          policy_name =
            (Xml.required "PolicyName"
               (Util.option_bind (Xml.member "PolicyName" xml) String.parse));
          cookie_expiration_period =
            (Util.option_bind (Xml.member "CookieExpirationPeriod" xml)
               Long.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.cookie_expiration_period
              (fun f ->
                 Query.Pair ("CookieExpirationPeriod", (Long.to_query f)));
           Some (Query.Pair ("PolicyName", (String.to_query v.policy_name)));
           Some
             (Query.Pair
                ("LoadBalancerName", (String.to_query v.load_balancer_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cookie_expiration_period
              (fun f -> ("cookie_expiration_period", (Long.to_json f)));
           Some ("policy_name", (String.to_json v.policy_name));
           Some ("load_balancer_name", (String.to_json v.load_balancer_name))])
    let of_json j =
      {
        load_balancer_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "load_balancer_name")));
        policy_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "policy_name")));
        cookie_expiration_period =
          (Util.option_map (Json.lookup j "cookie_expiration_period")
             Long.of_json)
      }
  end
module ApplySecurityGroupsToLoadBalancerOutput =
  struct
    type t = {
      security_groups: SecurityGroups.t }
    let make ?(security_groups= [])  () = { security_groups }
    let parse xml =
      Some
        {
          security_groups =
            (Util.of_option []
               (Util.option_bind (Xml.member "SecurityGroups" xml)
                  SecurityGroups.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("SecurityGroups.member",
                   (SecurityGroups.to_query v.security_groups)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("security_groups", (SecurityGroups.to_json v.security_groups))])
    let of_json j =
      {
        security_groups =
          (SecurityGroups.of_json
             (Util.of_option_exn (Json.lookup j "security_groups")))
      }
  end
module ConfigureHealthCheckInput =
  struct
    type t = {
      load_balancer_name: String.t ;
      health_check: HealthCheck.t }
    let make ~load_balancer_name  ~health_check  () =
      { load_balancer_name; health_check }
    let parse xml =
      Some
        {
          load_balancer_name =
            (Xml.required "LoadBalancerName"
               (Util.option_bind (Xml.member "LoadBalancerName" xml)
                  String.parse));
          health_check =
            (Xml.required "HealthCheck"
               (Util.option_bind (Xml.member "HealthCheck" xml)
                  HealthCheck.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("HealthCheck", (HealthCheck.to_query v.health_check)));
           Some
             (Query.Pair
                ("LoadBalancerName", (String.to_query v.load_balancer_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("health_check", (HealthCheck.to_json v.health_check));
           Some ("load_balancer_name", (String.to_json v.load_balancer_name))])
    let of_json j =
      {
        load_balancer_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "load_balancer_name")));
        health_check =
          (HealthCheck.of_json
             (Util.of_option_exn (Json.lookup j "health_check")))
      }
  end
module DuplicateAccessPointNameException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DescribeEndPointStateInput =
  struct
    type t = {
      load_balancer_name: String.t ;
      instances: Instances.t }
    let make ~load_balancer_name  ?(instances= [])  () =
      { load_balancer_name; instances }
    let parse xml =
      Some
        {
          load_balancer_name =
            (Xml.required "LoadBalancerName"
               (Util.option_bind (Xml.member "LoadBalancerName" xml)
                  String.parse));
          instances =
            (Util.of_option []
               (Util.option_bind (Xml.member "Instances" xml) Instances.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Instances.member", (Instances.to_query v.instances)));
           Some
             (Query.Pair
                ("LoadBalancerName", (String.to_query v.load_balancer_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("instances", (Instances.to_json v.instances));
           Some ("load_balancer_name", (String.to_json v.load_balancer_name))])
    let of_json j =
      {
        load_balancer_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "load_balancer_name")));
        instances =
          (Instances.of_json (Util.of_option_exn (Json.lookup j "instances")))
      }
  end
module DescribeLoadBalancerPolicyTypesInput =
  struct
    type t = {
      policy_type_names: PolicyTypeNames.t }
    let make ?(policy_type_names= [])  () = { policy_type_names }
    let parse xml =
      Some
        {
          policy_type_names =
            (Util.of_option []
               (Util.option_bind (Xml.member "PolicyTypeNames" xml)
                  PolicyTypeNames.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("PolicyTypeNames.member",
                   (PolicyTypeNames.to_query v.policy_type_names)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("policy_type_names",
                (PolicyTypeNames.to_json v.policy_type_names))])
    let of_json j =
      {
        policy_type_names =
          (PolicyTypeNames.of_json
             (Util.of_option_exn (Json.lookup j "policy_type_names")))
      }
  end
module DeregisterEndPointsOutput =
  struct
    type t = {
      instances: Instances.t }
    let make ?(instances= [])  () = { instances }
    let parse xml =
      Some
        {
          instances =
            (Util.of_option []
               (Util.option_bind (Xml.member "Instances" xml) Instances.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Instances.member", (Instances.to_query v.instances)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("instances", (Instances.to_json v.instances))])
    let of_json j =
      {
        instances =
          (Instances.of_json (Util.of_option_exn (Json.lookup j "instances")))
      }
  end
module DescribeLoadBalancerAttributesOutput =
  struct
    type t = {
      load_balancer_attributes: LoadBalancerAttributes.t option }
    let make ?load_balancer_attributes  () = { load_balancer_attributes }
    let parse xml =
      Some
        {
          load_balancer_attributes =
            (Util.option_bind (Xml.member "LoadBalancerAttributes" xml)
               LoadBalancerAttributes.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.load_balancer_attributes
              (fun f ->
                 Query.Pair
                   ("LoadBalancerAttributes",
                     (LoadBalancerAttributes.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.load_balancer_attributes
              (fun f ->
                 ("load_balancer_attributes",
                   (LoadBalancerAttributes.to_json f)))])
    let of_json j =
      {
        load_balancer_attributes =
          (Util.option_map (Json.lookup j "load_balancer_attributes")
             LoadBalancerAttributes.of_json)
      }
  end
module SetLoadBalancerListenerSSLCertificateInput =
  struct
    type t =
      {
      load_balancer_name: String.t ;
      load_balancer_port: Integer.t ;
      s_s_l_certificate_id: String.t }
    let make ~load_balancer_name  ~load_balancer_port  ~s_s_l_certificate_id 
      () = { load_balancer_name; load_balancer_port; s_s_l_certificate_id }
    let parse xml =
      Some
        {
          load_balancer_name =
            (Xml.required "LoadBalancerName"
               (Util.option_bind (Xml.member "LoadBalancerName" xml)
                  String.parse));
          load_balancer_port =
            (Xml.required "LoadBalancerPort"
               (Util.option_bind (Xml.member "LoadBalancerPort" xml)
                  Integer.parse));
          s_s_l_certificate_id =
            (Xml.required "SSLCertificateId"
               (Util.option_bind (Xml.member "SSLCertificateId" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("SSLCertificateId",
                   (String.to_query v.s_s_l_certificate_id)));
           Some
             (Query.Pair
                ("LoadBalancerPort", (Integer.to_query v.load_balancer_port)));
           Some
             (Query.Pair
                ("LoadBalancerName", (String.to_query v.load_balancer_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("s_s_l_certificate_id",
                (String.to_json v.s_s_l_certificate_id));
           Some
             ("load_balancer_port", (Integer.to_json v.load_balancer_port));
           Some ("load_balancer_name", (String.to_json v.load_balancer_name))])
    let of_json j =
      {
        load_balancer_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "load_balancer_name")));
        load_balancer_port =
          (Integer.of_json
             (Util.of_option_exn (Json.lookup j "load_balancer_port")));
        s_s_l_certificate_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "s_s_l_certificate_id")))
      }
  end
module CreateLBCookieStickinessPolicyOutput =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module RegisterEndPointsInput =
  struct
    type t = {
      load_balancer_name: String.t ;
      instances: Instances.t }
    let make ~load_balancer_name  ~instances  () =
      { load_balancer_name; instances }
    let parse xml =
      Some
        {
          load_balancer_name =
            (Xml.required "LoadBalancerName"
               (Util.option_bind (Xml.member "LoadBalancerName" xml)
                  String.parse));
          instances =
            (Xml.required "Instances"
               (Util.option_bind (Xml.member "Instances" xml) Instances.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Instances.member", (Instances.to_query v.instances)));
           Some
             (Query.Pair
                ("LoadBalancerName", (String.to_query v.load_balancer_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("instances", (Instances.to_json v.instances));
           Some ("load_balancer_name", (String.to_json v.load_balancer_name))])
    let of_json j =
      {
        load_balancer_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "load_balancer_name")));
        instances =
          (Instances.of_json (Util.of_option_exn (Json.lookup j "instances")))
      }
  end
module DeleteLoadBalancerListenerInput =
  struct
    type t = {
      load_balancer_name: String.t ;
      load_balancer_ports: Ports.t }
    let make ~load_balancer_name  ~load_balancer_ports  () =
      { load_balancer_name; load_balancer_ports }
    let parse xml =
      Some
        {
          load_balancer_name =
            (Xml.required "LoadBalancerName"
               (Util.option_bind (Xml.member "LoadBalancerName" xml)
                  String.parse));
          load_balancer_ports =
            (Xml.required "LoadBalancerPorts"
               (Util.option_bind (Xml.member "LoadBalancerPorts" xml)
                  Ports.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("LoadBalancerPorts.member",
                   (Ports.to_query v.load_balancer_ports)));
           Some
             (Query.Pair
                ("LoadBalancerName", (String.to_query v.load_balancer_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("load_balancer_ports", (Ports.to_json v.load_balancer_ports));
           Some ("load_balancer_name", (String.to_json v.load_balancer_name))])
    let of_json j =
      {
        load_balancer_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "load_balancer_name")));
        load_balancer_ports =
          (Ports.of_json
             (Util.of_option_exn (Json.lookup j "load_balancer_ports")))
      }
  end
module RegisterEndPointsOutput =
  struct
    type t = {
      instances: Instances.t }
    let make ?(instances= [])  () = { instances }
    let parse xml =
      Some
        {
          instances =
            (Util.of_option []
               (Util.option_bind (Xml.member "Instances" xml) Instances.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Instances.member", (Instances.to_query v.instances)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("instances", (Instances.to_json v.instances))])
    let of_json j =
      {
        instances =
          (Instances.of_json (Util.of_option_exn (Json.lookup j "instances")))
      }
  end
module AddAvailabilityZonesInput =
  struct
    type t =
      {
      load_balancer_name: String.t ;
      availability_zones: AvailabilityZones.t }
    let make ~load_balancer_name  ~availability_zones  () =
      { load_balancer_name; availability_zones }
    let parse xml =
      Some
        {
          load_balancer_name =
            (Xml.required "LoadBalancerName"
               (Util.option_bind (Xml.member "LoadBalancerName" xml)
                  String.parse));
          availability_zones =
            (Xml.required "AvailabilityZones"
               (Util.option_bind (Xml.member "AvailabilityZones" xml)
                  AvailabilityZones.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("AvailabilityZones.member",
                   (AvailabilityZones.to_query v.availability_zones)));
           Some
             (Query.Pair
                ("LoadBalancerName", (String.to_query v.load_balancer_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("availability_zones",
                (AvailabilityZones.to_json v.availability_zones));
           Some ("load_balancer_name", (String.to_json v.load_balancer_name))])
    let of_json j =
      {
        load_balancer_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "load_balancer_name")));
        availability_zones =
          (AvailabilityZones.of_json
             (Util.of_option_exn (Json.lookup j "availability_zones")))
      }
  end
module DescribeLoadBalancerPolicyTypesOutput =
  struct
    type t = {
      policy_type_descriptions: PolicyTypeDescriptions.t }
    let make ?(policy_type_descriptions= [])  () =
      { policy_type_descriptions }
    let parse xml =
      Some
        {
          policy_type_descriptions =
            (Util.of_option []
               (Util.option_bind (Xml.member "PolicyTypeDescriptions" xml)
                  PolicyTypeDescriptions.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("PolicyTypeDescriptions.member",
                   (PolicyTypeDescriptions.to_query
                      v.policy_type_descriptions)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("policy_type_descriptions",
                (PolicyTypeDescriptions.to_json v.policy_type_descriptions))])
    let of_json j =
      {
        policy_type_descriptions =
          (PolicyTypeDescriptions.of_json
             (Util.of_option_exn (Json.lookup j "policy_type_descriptions")))
      }
  end
module DeleteAccessPointInput =
  struct
    type t = {
      load_balancer_name: String.t }
    let make ~load_balancer_name  () = { load_balancer_name }
    let parse xml =
      Some
        {
          load_balancer_name =
            (Xml.required "LoadBalancerName"
               (Util.option_bind (Xml.member "LoadBalancerName" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("LoadBalancerName", (String.to_query v.load_balancer_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("load_balancer_name", (String.to_json v.load_balancer_name))])
    let of_json j =
      {
        load_balancer_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "load_balancer_name")))
      }
  end
module DescribeAccessPointsOutput =
  struct
    type t =
      {
      load_balancer_descriptions: LoadBalancerDescriptions.t ;
      next_marker: String.t option }
    let make ?(load_balancer_descriptions= [])  ?next_marker  () =
      { load_balancer_descriptions; next_marker }
    let parse xml =
      Some
        {
          load_balancer_descriptions =
            (Util.of_option []
               (Util.option_bind (Xml.member "LoadBalancerDescriptions" xml)
                  LoadBalancerDescriptions.parse));
          next_marker =
            (Util.option_bind (Xml.member "NextMarker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_marker
              (fun f -> Query.Pair ("NextMarker", (String.to_query f)));
           Some
             (Query.Pair
                ("LoadBalancerDescriptions.member",
                  (LoadBalancerDescriptions.to_query
                     v.load_balancer_descriptions)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_marker
              (fun f -> ("next_marker", (String.to_json f)));
           Some
             ("load_balancer_descriptions",
               (LoadBalancerDescriptions.to_json v.load_balancer_descriptions))])
    let of_json j =
      {
        load_balancer_descriptions =
          (LoadBalancerDescriptions.of_json
             (Util.of_option_exn (Json.lookup j "load_balancer_descriptions")));
        next_marker =
          (Util.option_map (Json.lookup j "next_marker") String.of_json)
      }
  end
module AccessPointNotFoundException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DetachLoadBalancerFromSubnetsInput =
  struct
    type t = {
      load_balancer_name: String.t ;
      subnets: Subnets.t }
    let make ~load_balancer_name  ~subnets  () =
      { load_balancer_name; subnets }
    let parse xml =
      Some
        {
          load_balancer_name =
            (Xml.required "LoadBalancerName"
               (Util.option_bind (Xml.member "LoadBalancerName" xml)
                  String.parse));
          subnets =
            (Xml.required "Subnets"
               (Util.option_bind (Xml.member "Subnets" xml) Subnets.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("Subnets.member", (Subnets.to_query v.subnets)));
           Some
             (Query.Pair
                ("LoadBalancerName", (String.to_query v.load_balancer_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("subnets", (Subnets.to_json v.subnets));
           Some ("load_balancer_name", (String.to_json v.load_balancer_name))])
    let of_json j =
      {
        load_balancer_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "load_balancer_name")));
        subnets =
          (Subnets.of_json (Util.of_option_exn (Json.lookup j "subnets")))
      }
  end
module InvalidConfigurationRequestException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module PolicyNotFoundException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end