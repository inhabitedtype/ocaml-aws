open Aws.BaseTypes

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
              ("AvailabilityZones.member", AvailabilityZones.to_query v.availability_zones))
       ; Some (Aws.Query.Pair ("Listeners.member", Listeners.to_query v.listeners))
       ; Some (Aws.Query.Pair ("LoadBalancerName", String.to_query v.load_balancer_name))
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
  ; listeners = Listeners.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Listeners"))
  ; availability_zones =
      AvailabilityZones.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AvailabilityZones"))
  ; subnets = Subnets.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Subnets"))
  ; security_groups =
      SecurityGroups.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroups"))
  ; scheme = Aws.Util.option_map (Aws.Json.lookup j "Scheme") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  }
