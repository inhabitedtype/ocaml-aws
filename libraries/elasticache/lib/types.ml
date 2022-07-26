open Aws.BaseTypes

type calendar = CalendarLib.Calendar.t

module DisassociateGlobalReplicationGroupMessage = struct
  type t =
    { global_replication_group_id : String.t
    ; replication_group_id : String.t
    ; replication_group_region : String.t
    }

  let make ~global_replication_group_id ~replication_group_id ~replication_group_region ()
      =
    { global_replication_group_id; replication_group_id; replication_group_region }

  let parse xml =
    Some
      { global_replication_group_id =
          Aws.Xml.required
            "GlobalReplicationGroupId"
            (Aws.Util.option_bind
               (Aws.Xml.member "GlobalReplicationGroupId" xml)
               String.parse)
      ; replication_group_id =
          Aws.Xml.required
            "ReplicationGroupId"
            (Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse)
      ; replication_group_region =
          Aws.Xml.required
            "ReplicationGroupRegion"
            (Aws.Util.option_bind
               (Aws.Xml.member "ReplicationGroupRegion" xml)
               String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("ReplicationGroupRegion", String.to_query v.replication_group_region))
         ; Some
             (Aws.Query.Pair ("ReplicationGroupId", String.to_query v.replication_group_id))
         ; Some
             (Aws.Query.Pair
                ("GlobalReplicationGroupId", String.to_query v.global_replication_group_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("ReplicationGroupRegion", String.to_json v.replication_group_region)
         ; Some ("ReplicationGroupId", String.to_json v.replication_group_id)
         ; Some ("GlobalReplicationGroupId", String.to_json v.global_replication_group_id)
         ])

  let of_json j =
    { global_replication_group_id =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "GlobalReplicationGroupId"))
    ; replication_group_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicationGroupId"))
    ; replication_group_region =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicationGroupRegion"))
    }
end

module NoOperationFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module FilterValueList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module Filter = struct
  type t =
    { name : String.t
    ; values : FilterValueList.t
    }

  let make ~name ~values () = { name; values }

  let parse xml =
    Some
      { name =
          Aws.Xml.required
            "Name"
            (Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse)
      ; values =
          Aws.Xml.required
            "Values"
            (Aws.Util.option_bind (Aws.Xml.member "Values" xml) FilterValueList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Values.member", FilterValueList.to_query v.values))
         ; Some (Aws.Query.Pair ("Name", String.to_query v.name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Values", FilterValueList.to_json v.values)
         ; Some ("Name", String.to_json v.name)
         ])

  let of_json j =
    { name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Name"))
    ; values =
        FilterValueList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Values"))
    }
end

module RecurringCharge = struct
  type t =
    { recurring_charge_amount : Double.t option
    ; recurring_charge_frequency : String.t option
    }

  let make ?recurring_charge_amount ?recurring_charge_frequency () =
    { recurring_charge_amount; recurring_charge_frequency }

  let parse xml =
    Some
      { recurring_charge_amount =
          Aws.Util.option_bind (Aws.Xml.member "RecurringChargeAmount" xml) Double.parse
      ; recurring_charge_frequency =
          Aws.Util.option_bind
            (Aws.Xml.member "RecurringChargeFrequency" xml)
            String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.recurring_charge_frequency (fun f ->
               Aws.Query.Pair ("RecurringChargeFrequency", String.to_query f))
         ; Aws.Util.option_map v.recurring_charge_amount (fun f ->
               Aws.Query.Pair ("RecurringChargeAmount", Double.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.recurring_charge_frequency (fun f ->
               "RecurringChargeFrequency", String.to_json f)
         ; Aws.Util.option_map v.recurring_charge_amount (fun f ->
               "RecurringChargeAmount", Double.to_json f)
         ])

  let of_json j =
    { recurring_charge_amount =
        Aws.Util.option_map (Aws.Json.lookup j "RecurringChargeAmount") Double.of_json
    ; recurring_charge_frequency =
        Aws.Util.option_map (Aws.Json.lookup j "RecurringChargeFrequency") String.of_json
    }
end

module RecurringChargeList = struct
  type t = RecurringCharge.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map RecurringCharge.parse (Aws.Xml.members "RecurringCharge" xml))

  let to_query v = Aws.Query.to_query_list RecurringCharge.to_query v

  let to_json v = `List (List.map RecurringCharge.to_json v)

  let of_json j = Aws.Json.to_list RecurringCharge.of_json j
end

module ReservedCacheNode = struct
  type t =
    { reserved_cache_node_id : String.t option
    ; reserved_cache_nodes_offering_id : String.t option
    ; cache_node_type : String.t option
    ; start_time : DateTime.t option
    ; duration : Integer.t option
    ; fixed_price : Double.t option
    ; usage_price : Double.t option
    ; cache_node_count : Integer.t option
    ; product_description : String.t option
    ; offering_type : String.t option
    ; state : String.t option
    ; recurring_charges : RecurringChargeList.t
    ; reservation_a_r_n : String.t option
    }

  let make
      ?reserved_cache_node_id
      ?reserved_cache_nodes_offering_id
      ?cache_node_type
      ?start_time
      ?duration
      ?fixed_price
      ?usage_price
      ?cache_node_count
      ?product_description
      ?offering_type
      ?state
      ?(recurring_charges = [])
      ?reservation_a_r_n
      () =
    { reserved_cache_node_id
    ; reserved_cache_nodes_offering_id
    ; cache_node_type
    ; start_time
    ; duration
    ; fixed_price
    ; usage_price
    ; cache_node_count
    ; product_description
    ; offering_type
    ; state
    ; recurring_charges
    ; reservation_a_r_n
    }

  let parse xml =
    Some
      { reserved_cache_node_id =
          Aws.Util.option_bind (Aws.Xml.member "ReservedCacheNodeId" xml) String.parse
      ; reserved_cache_nodes_offering_id =
          Aws.Util.option_bind
            (Aws.Xml.member "ReservedCacheNodesOfferingId" xml)
            String.parse
      ; cache_node_type =
          Aws.Util.option_bind (Aws.Xml.member "CacheNodeType" xml) String.parse
      ; start_time = Aws.Util.option_bind (Aws.Xml.member "StartTime" xml) DateTime.parse
      ; duration = Aws.Util.option_bind (Aws.Xml.member "Duration" xml) Integer.parse
      ; fixed_price = Aws.Util.option_bind (Aws.Xml.member "FixedPrice" xml) Double.parse
      ; usage_price = Aws.Util.option_bind (Aws.Xml.member "UsagePrice" xml) Double.parse
      ; cache_node_count =
          Aws.Util.option_bind (Aws.Xml.member "CacheNodeCount" xml) Integer.parse
      ; product_description =
          Aws.Util.option_bind (Aws.Xml.member "ProductDescription" xml) String.parse
      ; offering_type =
          Aws.Util.option_bind (Aws.Xml.member "OfferingType" xml) String.parse
      ; state = Aws.Util.option_bind (Aws.Xml.member "State" xml) String.parse
      ; recurring_charges =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "RecurringCharges" xml)
               RecurringChargeList.parse)
      ; reservation_a_r_n =
          Aws.Util.option_bind (Aws.Xml.member "ReservationARN" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.reservation_a_r_n (fun f ->
               Aws.Query.Pair ("ReservationARN", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "RecurringCharges.member"
                , RecurringChargeList.to_query v.recurring_charges ))
         ; Aws.Util.option_map v.state (fun f ->
               Aws.Query.Pair ("State", String.to_query f))
         ; Aws.Util.option_map v.offering_type (fun f ->
               Aws.Query.Pair ("OfferingType", String.to_query f))
         ; Aws.Util.option_map v.product_description (fun f ->
               Aws.Query.Pair ("ProductDescription", String.to_query f))
         ; Aws.Util.option_map v.cache_node_count (fun f ->
               Aws.Query.Pair ("CacheNodeCount", Integer.to_query f))
         ; Aws.Util.option_map v.usage_price (fun f ->
               Aws.Query.Pair ("UsagePrice", Double.to_query f))
         ; Aws.Util.option_map v.fixed_price (fun f ->
               Aws.Query.Pair ("FixedPrice", Double.to_query f))
         ; Aws.Util.option_map v.duration (fun f ->
               Aws.Query.Pair ("Duration", Integer.to_query f))
         ; Aws.Util.option_map v.start_time (fun f ->
               Aws.Query.Pair ("StartTime", DateTime.to_query f))
         ; Aws.Util.option_map v.cache_node_type (fun f ->
               Aws.Query.Pair ("CacheNodeType", String.to_query f))
         ; Aws.Util.option_map v.reserved_cache_nodes_offering_id (fun f ->
               Aws.Query.Pair ("ReservedCacheNodesOfferingId", String.to_query f))
         ; Aws.Util.option_map v.reserved_cache_node_id (fun f ->
               Aws.Query.Pair ("ReservedCacheNodeId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.reservation_a_r_n (fun f ->
               "ReservationARN", String.to_json f)
         ; Some ("RecurringCharges", RecurringChargeList.to_json v.recurring_charges)
         ; Aws.Util.option_map v.state (fun f -> "State", String.to_json f)
         ; Aws.Util.option_map v.offering_type (fun f -> "OfferingType", String.to_json f)
         ; Aws.Util.option_map v.product_description (fun f ->
               "ProductDescription", String.to_json f)
         ; Aws.Util.option_map v.cache_node_count (fun f ->
               "CacheNodeCount", Integer.to_json f)
         ; Aws.Util.option_map v.usage_price (fun f -> "UsagePrice", Double.to_json f)
         ; Aws.Util.option_map v.fixed_price (fun f -> "FixedPrice", Double.to_json f)
         ; Aws.Util.option_map v.duration (fun f -> "Duration", Integer.to_json f)
         ; Aws.Util.option_map v.start_time (fun f -> "StartTime", DateTime.to_json f)
         ; Aws.Util.option_map v.cache_node_type (fun f ->
               "CacheNodeType", String.to_json f)
         ; Aws.Util.option_map v.reserved_cache_nodes_offering_id (fun f ->
               "ReservedCacheNodesOfferingId", String.to_json f)
         ; Aws.Util.option_map v.reserved_cache_node_id (fun f ->
               "ReservedCacheNodeId", String.to_json f)
         ])

  let of_json j =
    { reserved_cache_node_id =
        Aws.Util.option_map (Aws.Json.lookup j "ReservedCacheNodeId") String.of_json
    ; reserved_cache_nodes_offering_id =
        Aws.Util.option_map
          (Aws.Json.lookup j "ReservedCacheNodesOfferingId")
          String.of_json
    ; cache_node_type =
        Aws.Util.option_map (Aws.Json.lookup j "CacheNodeType") String.of_json
    ; start_time = Aws.Util.option_map (Aws.Json.lookup j "StartTime") DateTime.of_json
    ; duration = Aws.Util.option_map (Aws.Json.lookup j "Duration") Integer.of_json
    ; fixed_price = Aws.Util.option_map (Aws.Json.lookup j "FixedPrice") Double.of_json
    ; usage_price = Aws.Util.option_map (Aws.Json.lookup j "UsagePrice") Double.of_json
    ; cache_node_count =
        Aws.Util.option_map (Aws.Json.lookup j "CacheNodeCount") Integer.of_json
    ; product_description =
        Aws.Util.option_map (Aws.Json.lookup j "ProductDescription") String.of_json
    ; offering_type =
        Aws.Util.option_map (Aws.Json.lookup j "OfferingType") String.of_json
    ; state = Aws.Util.option_map (Aws.Json.lookup j "State") String.of_json
    ; recurring_charges =
        RecurringChargeList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "RecurringCharges"))
    ; reservation_a_r_n =
        Aws.Util.option_map (Aws.Json.lookup j "ReservationARN") String.of_json
    }
end

module ReservedCacheNodeList = struct
  type t = ReservedCacheNode.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map ReservedCacheNode.parse (Aws.Xml.members "ReservedCacheNode" xml))

  let to_query v = Aws.Query.to_query_list ReservedCacheNode.to_query v

  let to_json v = `List (List.map ReservedCacheNode.to_json v)

  let of_json j = Aws.Json.to_list ReservedCacheNode.of_json j
end

module ReservedCacheNodeMessage = struct
  type t =
    { marker : String.t option
    ; reserved_cache_nodes : ReservedCacheNodeList.t
    }

  let make ?marker ?(reserved_cache_nodes = []) () = { marker; reserved_cache_nodes }

  let parse xml =
    Some
      { marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      ; reserved_cache_nodes =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ReservedCacheNodes" xml)
               ReservedCacheNodeList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "ReservedCacheNodes.member"
                , ReservedCacheNodeList.to_query v.reserved_cache_nodes ))
         ; Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ("ReservedCacheNodes", ReservedCacheNodeList.to_json v.reserved_cache_nodes)
         ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ])

  let of_json j =
    { marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    ; reserved_cache_nodes =
        ReservedCacheNodeList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ReservedCacheNodes"))
    }
end

module AutomaticFailoverStatus = struct
  type t =
    | Enabled
    | Disabled
    | Enabling
    | Disabling

  let str_to_t =
    [ "disabling", Disabling
    ; "enabling", Enabling
    ; "disabled", Disabled
    ; "enabled", Enabled
    ]

  let t_to_str =
    [ Disabling, "disabling"
    ; Enabling, "enabling"
    ; Disabled, "disabled"
    ; Enabled, "enabled"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module GlobalReplicationGroupMember = struct
  type t =
    { replication_group_id : String.t option
    ; replication_group_region : String.t option
    ; role : String.t option
    ; automatic_failover : AutomaticFailoverStatus.t option
    ; status : String.t option
    }

  let make
      ?replication_group_id
      ?replication_group_region
      ?role
      ?automatic_failover
      ?status
      () =
    { replication_group_id; replication_group_region; role; automatic_failover; status }

  let parse xml =
    Some
      { replication_group_id =
          Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse
      ; replication_group_region =
          Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupRegion" xml) String.parse
      ; role = Aws.Util.option_bind (Aws.Xml.member "Role" xml) String.parse
      ; automatic_failover =
          Aws.Util.option_bind
            (Aws.Xml.member "AutomaticFailover" xml)
            AutomaticFailoverStatus.parse
      ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.status (fun f ->
               Aws.Query.Pair ("Status", String.to_query f))
         ; Aws.Util.option_map v.automatic_failover (fun f ->
               Aws.Query.Pair ("AutomaticFailover", AutomaticFailoverStatus.to_query f))
         ; Aws.Util.option_map v.role (fun f ->
               Aws.Query.Pair ("Role", String.to_query f))
         ; Aws.Util.option_map v.replication_group_region (fun f ->
               Aws.Query.Pair ("ReplicationGroupRegion", String.to_query f))
         ; Aws.Util.option_map v.replication_group_id (fun f ->
               Aws.Query.Pair ("ReplicationGroupId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
         ; Aws.Util.option_map v.automatic_failover (fun f ->
               "AutomaticFailover", AutomaticFailoverStatus.to_json f)
         ; Aws.Util.option_map v.role (fun f -> "Role", String.to_json f)
         ; Aws.Util.option_map v.replication_group_region (fun f ->
               "ReplicationGroupRegion", String.to_json f)
         ; Aws.Util.option_map v.replication_group_id (fun f ->
               "ReplicationGroupId", String.to_json f)
         ])

  let of_json j =
    { replication_group_id =
        Aws.Util.option_map (Aws.Json.lookup j "ReplicationGroupId") String.of_json
    ; replication_group_region =
        Aws.Util.option_map (Aws.Json.lookup j "ReplicationGroupRegion") String.of_json
    ; role = Aws.Util.option_map (Aws.Json.lookup j "Role") String.of_json
    ; automatic_failover =
        Aws.Util.option_map
          (Aws.Json.lookup j "AutomaticFailover")
          AutomaticFailoverStatus.of_json
    ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
    }
end

module GlobalReplicationGroupMemberList = struct
  type t = GlobalReplicationGroupMember.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map
         GlobalReplicationGroupMember.parse
         (Aws.Xml.members "GlobalReplicationGroupMember" xml))

  let to_query v = Aws.Query.to_query_list GlobalReplicationGroupMember.to_query v

  let to_json v = `List (List.map GlobalReplicationGroupMember.to_json v)

  let of_json j = Aws.Json.to_list GlobalReplicationGroupMember.of_json j
end

module GlobalNodeGroup = struct
  type t =
    { global_node_group_id : String.t option
    ; slots : String.t option
    }

  let make ?global_node_group_id ?slots () = { global_node_group_id; slots }

  let parse xml =
    Some
      { global_node_group_id =
          Aws.Util.option_bind (Aws.Xml.member "GlobalNodeGroupId" xml) String.parse
      ; slots = Aws.Util.option_bind (Aws.Xml.member "Slots" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.slots (fun f ->
               Aws.Query.Pair ("Slots", String.to_query f))
         ; Aws.Util.option_map v.global_node_group_id (fun f ->
               Aws.Query.Pair ("GlobalNodeGroupId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.slots (fun f -> "Slots", String.to_json f)
         ; Aws.Util.option_map v.global_node_group_id (fun f ->
               "GlobalNodeGroupId", String.to_json f)
         ])

  let of_json j =
    { global_node_group_id =
        Aws.Util.option_map (Aws.Json.lookup j "GlobalNodeGroupId") String.of_json
    ; slots = Aws.Util.option_map (Aws.Json.lookup j "Slots") String.of_json
    }
end

module GlobalNodeGroupList = struct
  type t = GlobalNodeGroup.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map GlobalNodeGroup.parse (Aws.Xml.members "GlobalNodeGroup" xml))

  let to_query v = Aws.Query.to_query_list GlobalNodeGroup.to_query v

  let to_json v = `List (List.map GlobalNodeGroup.to_json v)

  let of_json j = Aws.Json.to_list GlobalNodeGroup.of_json j
end

module GlobalReplicationGroup = struct
  type t =
    { global_replication_group_id : String.t option
    ; global_replication_group_description : String.t option
    ; status : String.t option
    ; cache_node_type : String.t option
    ; engine : String.t option
    ; engine_version : String.t option
    ; members : GlobalReplicationGroupMemberList.t
    ; cluster_enabled : Boolean.t option
    ; global_node_groups : GlobalNodeGroupList.t
    ; auth_token_enabled : Boolean.t option
    ; transit_encryption_enabled : Boolean.t option
    ; at_rest_encryption_enabled : Boolean.t option
    ; a_r_n : String.t option
    }

  let make
      ?global_replication_group_id
      ?global_replication_group_description
      ?status
      ?cache_node_type
      ?engine
      ?engine_version
      ?(members = [])
      ?cluster_enabled
      ?(global_node_groups = [])
      ?auth_token_enabled
      ?transit_encryption_enabled
      ?at_rest_encryption_enabled
      ?a_r_n
      () =
    { global_replication_group_id
    ; global_replication_group_description
    ; status
    ; cache_node_type
    ; engine
    ; engine_version
    ; members
    ; cluster_enabled
    ; global_node_groups
    ; auth_token_enabled
    ; transit_encryption_enabled
    ; at_rest_encryption_enabled
    ; a_r_n
    }

  let parse xml =
    Some
      { global_replication_group_id =
          Aws.Util.option_bind
            (Aws.Xml.member "GlobalReplicationGroupId" xml)
            String.parse
      ; global_replication_group_description =
          Aws.Util.option_bind
            (Aws.Xml.member "GlobalReplicationGroupDescription" xml)
            String.parse
      ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
      ; cache_node_type =
          Aws.Util.option_bind (Aws.Xml.member "CacheNodeType" xml) String.parse
      ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
      ; engine_version =
          Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
      ; members =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "Members" xml)
               GlobalReplicationGroupMemberList.parse)
      ; cluster_enabled =
          Aws.Util.option_bind (Aws.Xml.member "ClusterEnabled" xml) Boolean.parse
      ; global_node_groups =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "GlobalNodeGroups" xml)
               GlobalNodeGroupList.parse)
      ; auth_token_enabled =
          Aws.Util.option_bind (Aws.Xml.member "AuthTokenEnabled" xml) Boolean.parse
      ; transit_encryption_enabled =
          Aws.Util.option_bind
            (Aws.Xml.member "TransitEncryptionEnabled" xml)
            Boolean.parse
      ; at_rest_encryption_enabled =
          Aws.Util.option_bind
            (Aws.Xml.member "AtRestEncryptionEnabled" xml)
            Boolean.parse
      ; a_r_n = Aws.Util.option_bind (Aws.Xml.member "ARN" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.a_r_n (fun f ->
               Aws.Query.Pair ("ARN", String.to_query f))
         ; Aws.Util.option_map v.at_rest_encryption_enabled (fun f ->
               Aws.Query.Pair ("AtRestEncryptionEnabled", Boolean.to_query f))
         ; Aws.Util.option_map v.transit_encryption_enabled (fun f ->
               Aws.Query.Pair ("TransitEncryptionEnabled", Boolean.to_query f))
         ; Aws.Util.option_map v.auth_token_enabled (fun f ->
               Aws.Query.Pair ("AuthTokenEnabled", Boolean.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "GlobalNodeGroups.member"
                , GlobalNodeGroupList.to_query v.global_node_groups ))
         ; Aws.Util.option_map v.cluster_enabled (fun f ->
               Aws.Query.Pair ("ClusterEnabled", Boolean.to_query f))
         ; Some
             (Aws.Query.Pair
                ("Members.member", GlobalReplicationGroupMemberList.to_query v.members))
         ; Aws.Util.option_map v.engine_version (fun f ->
               Aws.Query.Pair ("EngineVersion", String.to_query f))
         ; Aws.Util.option_map v.engine (fun f ->
               Aws.Query.Pair ("Engine", String.to_query f))
         ; Aws.Util.option_map v.cache_node_type (fun f ->
               Aws.Query.Pair ("CacheNodeType", String.to_query f))
         ; Aws.Util.option_map v.status (fun f ->
               Aws.Query.Pair ("Status", String.to_query f))
         ; Aws.Util.option_map v.global_replication_group_description (fun f ->
               Aws.Query.Pair ("GlobalReplicationGroupDescription", String.to_query f))
         ; Aws.Util.option_map v.global_replication_group_id (fun f ->
               Aws.Query.Pair ("GlobalReplicationGroupId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.a_r_n (fun f -> "ARN", String.to_json f)
         ; Aws.Util.option_map v.at_rest_encryption_enabled (fun f ->
               "AtRestEncryptionEnabled", Boolean.to_json f)
         ; Aws.Util.option_map v.transit_encryption_enabled (fun f ->
               "TransitEncryptionEnabled", Boolean.to_json f)
         ; Aws.Util.option_map v.auth_token_enabled (fun f ->
               "AuthTokenEnabled", Boolean.to_json f)
         ; Some ("GlobalNodeGroups", GlobalNodeGroupList.to_json v.global_node_groups)
         ; Aws.Util.option_map v.cluster_enabled (fun f ->
               "ClusterEnabled", Boolean.to_json f)
         ; Some ("Members", GlobalReplicationGroupMemberList.to_json v.members)
         ; Aws.Util.option_map v.engine_version (fun f ->
               "EngineVersion", String.to_json f)
         ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
         ; Aws.Util.option_map v.cache_node_type (fun f ->
               "CacheNodeType", String.to_json f)
         ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
         ; Aws.Util.option_map v.global_replication_group_description (fun f ->
               "GlobalReplicationGroupDescription", String.to_json f)
         ; Aws.Util.option_map v.global_replication_group_id (fun f ->
               "GlobalReplicationGroupId", String.to_json f)
         ])

  let of_json j =
    { global_replication_group_id =
        Aws.Util.option_map (Aws.Json.lookup j "GlobalReplicationGroupId") String.of_json
    ; global_replication_group_description =
        Aws.Util.option_map
          (Aws.Json.lookup j "GlobalReplicationGroupDescription")
          String.of_json
    ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
    ; cache_node_type =
        Aws.Util.option_map (Aws.Json.lookup j "CacheNodeType") String.of_json
    ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
    ; engine_version =
        Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
    ; members =
        GlobalReplicationGroupMemberList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Members"))
    ; cluster_enabled =
        Aws.Util.option_map (Aws.Json.lookup j "ClusterEnabled") Boolean.of_json
    ; global_node_groups =
        GlobalNodeGroupList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "GlobalNodeGroups"))
    ; auth_token_enabled =
        Aws.Util.option_map (Aws.Json.lookup j "AuthTokenEnabled") Boolean.of_json
    ; transit_encryption_enabled =
        Aws.Util.option_map (Aws.Json.lookup j "TransitEncryptionEnabled") Boolean.of_json
    ; at_rest_encryption_enabled =
        Aws.Util.option_map (Aws.Json.lookup j "AtRestEncryptionEnabled") Boolean.of_json
    ; a_r_n = Aws.Util.option_map (Aws.Json.lookup j "ARN") String.of_json
    }
end

module GlobalReplicationGroupAlreadyExistsFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ParameterNameValue = struct
  type t =
    { parameter_name : String.t option
    ; parameter_value : String.t option
    }

  let make ?parameter_name ?parameter_value () = { parameter_name; parameter_value }

  let parse xml =
    Some
      { parameter_name =
          Aws.Util.option_bind (Aws.Xml.member "ParameterName" xml) String.parse
      ; parameter_value =
          Aws.Util.option_bind (Aws.Xml.member "ParameterValue" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.parameter_value (fun f ->
               Aws.Query.Pair ("ParameterValue", String.to_query f))
         ; Aws.Util.option_map v.parameter_name (fun f ->
               Aws.Query.Pair ("ParameterName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.parameter_value (fun f ->
               "ParameterValue", String.to_json f)
         ; Aws.Util.option_map v.parameter_name (fun f ->
               "ParameterName", String.to_json f)
         ])

  let of_json j =
    { parameter_name =
        Aws.Util.option_map (Aws.Json.lookup j "ParameterName") String.of_json
    ; parameter_value =
        Aws.Util.option_map (Aws.Json.lookup j "ParameterValue") String.of_json
    }
end

module ParameterNameValueList = struct
  type t = ParameterNameValue.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map ParameterNameValue.parse (Aws.Xml.members "ParameterNameValue" xml))

  let to_query v = Aws.Query.to_query_list ParameterNameValue.to_query v

  let to_json v = `List (List.map ParameterNameValue.to_json v)

  let of_json j = Aws.Json.to_list ParameterNameValue.of_json j
end

module ModifyCacheParameterGroupMessage = struct
  type t =
    { cache_parameter_group_name : String.t
    ; parameter_name_values : ParameterNameValueList.t
    }

  let make ~cache_parameter_group_name ~parameter_name_values () =
    { cache_parameter_group_name; parameter_name_values }

  let parse xml =
    Some
      { cache_parameter_group_name =
          Aws.Xml.required
            "CacheParameterGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "CacheParameterGroupName" xml)
               String.parse)
      ; parameter_name_values =
          Aws.Xml.required
            "ParameterNameValues"
            (Aws.Util.option_bind
               (Aws.Xml.member "ParameterNameValues" xml)
               ParameterNameValueList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "ParameterNameValues.member"
                , ParameterNameValueList.to_query v.parameter_name_values ))
         ; Some
             (Aws.Query.Pair
                ("CacheParameterGroupName", String.to_query v.cache_parameter_group_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "ParameterNameValues"
             , ParameterNameValueList.to_json v.parameter_name_values )
         ; Some ("CacheParameterGroupName", String.to_json v.cache_parameter_group_name)
         ])

  let of_json j =
    { cache_parameter_group_name =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheParameterGroupName"))
    ; parameter_name_values =
        ParameterNameValueList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ParameterNameValues"))
    }
end

module NodeUpdateInitiatedBy = struct
  type t =
    | System
    | Customer

  let str_to_t = [ "customer", Customer; "system", System ]

  let t_to_str = [ Customer, "customer"; System, "system" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module PreferredOutpostArnList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map String.parse (Aws.Xml.members "PreferredOutpostArn" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module PreferredAvailabilityZoneList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map String.parse (Aws.Xml.members "PreferredAvailabilityZone" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module ConfigureShard = struct
  type t =
    { node_group_id : String.t
    ; new_replica_count : Integer.t
    ; preferred_availability_zones : PreferredAvailabilityZoneList.t
    ; preferred_outpost_arns : PreferredOutpostArnList.t
    }

  let make
      ~node_group_id
      ~new_replica_count
      ?(preferred_availability_zones = [])
      ?(preferred_outpost_arns = [])
      () =
    { node_group_id
    ; new_replica_count
    ; preferred_availability_zones
    ; preferred_outpost_arns
    }

  let parse xml =
    Some
      { node_group_id =
          Aws.Xml.required
            "NodeGroupId"
            (Aws.Util.option_bind (Aws.Xml.member "NodeGroupId" xml) String.parse)
      ; new_replica_count =
          Aws.Xml.required
            "NewReplicaCount"
            (Aws.Util.option_bind (Aws.Xml.member "NewReplicaCount" xml) Integer.parse)
      ; preferred_availability_zones =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "PreferredAvailabilityZones" xml)
               PreferredAvailabilityZoneList.parse)
      ; preferred_outpost_arns =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "PreferredOutpostArns" xml)
               PreferredOutpostArnList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "PreferredOutpostArns.member"
                , PreferredOutpostArnList.to_query v.preferred_outpost_arns ))
         ; Some
             (Aws.Query.Pair
                ( "PreferredAvailabilityZones.member"
                , PreferredAvailabilityZoneList.to_query v.preferred_availability_zones ))
         ; Some (Aws.Query.Pair ("NewReplicaCount", Integer.to_query v.new_replica_count))
         ; Some (Aws.Query.Pair ("NodeGroupId", String.to_query v.node_group_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "PreferredOutpostArns"
             , PreferredOutpostArnList.to_json v.preferred_outpost_arns )
         ; Some
             ( "PreferredAvailabilityZones"
             , PreferredAvailabilityZoneList.to_json v.preferred_availability_zones )
         ; Some ("NewReplicaCount", Integer.to_json v.new_replica_count)
         ; Some ("NodeGroupId", String.to_json v.node_group_id)
         ])

  let of_json j =
    { node_group_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "NodeGroupId"))
    ; new_replica_count =
        Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "NewReplicaCount"))
    ; preferred_availability_zones =
        PreferredAvailabilityZoneList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "PreferredAvailabilityZones"))
    ; preferred_outpost_arns =
        PreferredOutpostArnList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "PreferredOutpostArns"))
    }
end

module ReplicaConfigurationList = struct
  type t = ConfigureShard.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map ConfigureShard.parse (Aws.Xml.members "ConfigureShard" xml))

  let to_query v = Aws.Query.to_query_list ConfigureShard.to_query v

  let to_json v = `List (List.map ConfigureShard.to_json v)

  let of_json j = Aws.Json.to_list ConfigureShard.of_json j
end

module IncreaseReplicaCountMessage = struct
  type t =
    { replication_group_id : String.t
    ; new_replica_count : Integer.t option
    ; replica_configuration : ReplicaConfigurationList.t
    ; apply_immediately : Boolean.t
    }

  let make
      ~replication_group_id
      ?new_replica_count
      ?(replica_configuration = [])
      ~apply_immediately
      () =
    { replication_group_id; new_replica_count; replica_configuration; apply_immediately }

  let parse xml =
    Some
      { replication_group_id =
          Aws.Xml.required
            "ReplicationGroupId"
            (Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse)
      ; new_replica_count =
          Aws.Util.option_bind (Aws.Xml.member "NewReplicaCount" xml) Integer.parse
      ; replica_configuration =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ReplicaConfiguration" xml)
               ReplicaConfigurationList.parse)
      ; apply_immediately =
          Aws.Xml.required
            "ApplyImmediately"
            (Aws.Util.option_bind (Aws.Xml.member "ApplyImmediately" xml) Boolean.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("ApplyImmediately", Boolean.to_query v.apply_immediately))
         ; Some
             (Aws.Query.Pair
                ( "ReplicaConfiguration.member"
                , ReplicaConfigurationList.to_query v.replica_configuration ))
         ; Aws.Util.option_map v.new_replica_count (fun f ->
               Aws.Query.Pair ("NewReplicaCount", Integer.to_query f))
         ; Some
             (Aws.Query.Pair ("ReplicationGroupId", String.to_query v.replication_group_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("ApplyImmediately", Boolean.to_json v.apply_immediately)
         ; Some
             ( "ReplicaConfiguration"
             , ReplicaConfigurationList.to_json v.replica_configuration )
         ; Aws.Util.option_map v.new_replica_count (fun f ->
               "NewReplicaCount", Integer.to_json f)
         ; Some ("ReplicationGroupId", String.to_json v.replication_group_id)
         ])

  let of_json j =
    { replication_group_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicationGroupId"))
    ; new_replica_count =
        Aws.Util.option_map (Aws.Json.lookup j "NewReplicaCount") Integer.of_json
    ; replica_configuration =
        ReplicaConfigurationList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicaConfiguration"))
    ; apply_immediately =
        Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ApplyImmediately"))
    }
end

module DeleteGlobalReplicationGroupResult = struct
  type t = { global_replication_group : GlobalReplicationGroup.t option }

  let make ?global_replication_group () = { global_replication_group }

  let parse xml =
    Some
      { global_replication_group =
          Aws.Util.option_bind
            (Aws.Xml.member "GlobalReplicationGroup" xml)
            GlobalReplicationGroup.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.global_replication_group (fun f ->
               Aws.Query.Pair ("GlobalReplicationGroup", GlobalReplicationGroup.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.global_replication_group (fun f ->
               "GlobalReplicationGroup", GlobalReplicationGroup.to_json f)
         ])

  let of_json j =
    { global_replication_group =
        Aws.Util.option_map
          (Aws.Json.lookup j "GlobalReplicationGroup")
          GlobalReplicationGroup.of_json
    }
end

module UserGroupIdList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module UserGroupsUpdateStatus = struct
  type t =
    { user_group_ids_to_add : UserGroupIdList.t
    ; user_group_ids_to_remove : UserGroupIdList.t
    }

  let make ?(user_group_ids_to_add = []) ?(user_group_ids_to_remove = []) () =
    { user_group_ids_to_add; user_group_ids_to_remove }

  let parse xml =
    Some
      { user_group_ids_to_add =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "UserGroupIdsToAdd" xml)
               UserGroupIdList.parse)
      ; user_group_ids_to_remove =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "UserGroupIdsToRemove" xml)
               UserGroupIdList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "UserGroupIdsToRemove.member"
                , UserGroupIdList.to_query v.user_group_ids_to_remove ))
         ; Some
             (Aws.Query.Pair
                ( "UserGroupIdsToAdd.member"
                , UserGroupIdList.to_query v.user_group_ids_to_add ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ("UserGroupIdsToRemove", UserGroupIdList.to_json v.user_group_ids_to_remove)
         ; Some ("UserGroupIdsToAdd", UserGroupIdList.to_json v.user_group_ids_to_add)
         ])

  let of_json j =
    { user_group_ids_to_add =
        UserGroupIdList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "UserGroupIdsToAdd"))
    ; user_group_ids_to_remove =
        UserGroupIdList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "UserGroupIdsToRemove"))
    }
end

module SlotMigration = struct
  type t = { progress_percentage : Double.t option }

  let make ?progress_percentage () = { progress_percentage }

  let parse xml =
    Some
      { progress_percentage =
          Aws.Util.option_bind (Aws.Xml.member "ProgressPercentage" xml) Double.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.progress_percentage (fun f ->
               Aws.Query.Pair ("ProgressPercentage", Double.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.progress_percentage (fun f ->
               "ProgressPercentage", Double.to_json f)
         ])

  let of_json j =
    { progress_percentage =
        Aws.Util.option_map (Aws.Json.lookup j "ProgressPercentage") Double.of_json
    }
end

module ReshardingStatus = struct
  type t = { slot_migration : SlotMigration.t option }

  let make ?slot_migration () = { slot_migration }

  let parse xml =
    Some
      { slot_migration =
          Aws.Util.option_bind (Aws.Xml.member "SlotMigration" xml) SlotMigration.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.slot_migration (fun f ->
               Aws.Query.Pair ("SlotMigration", SlotMigration.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.slot_migration (fun f ->
               "SlotMigration", SlotMigration.to_json f)
         ])

  let of_json j =
    { slot_migration =
        Aws.Util.option_map (Aws.Json.lookup j "SlotMigration") SlotMigration.of_json
    }
end

module PendingAutomaticFailoverStatus = struct
  type t =
    | Enabled
    | Disabled

  let str_to_t = [ "disabled", Disabled; "enabled", Enabled ]

  let t_to_str = [ Disabled, "disabled"; Enabled, "enabled" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module AuthTokenUpdateStatus = struct
  type t =
    | SETTING
    | ROTATING

  let str_to_t = [ "ROTATING", ROTATING; "SETTING", SETTING ]

  let t_to_str = [ ROTATING, "ROTATING"; SETTING, "SETTING" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module ReplicationGroupPendingModifiedValues = struct
  type t =
    { primary_cluster_id : String.t option
    ; automatic_failover_status : PendingAutomaticFailoverStatus.t option
    ; resharding : ReshardingStatus.t option
    ; auth_token_status : AuthTokenUpdateStatus.t option
    ; user_groups : UserGroupsUpdateStatus.t option
    }

  let make
      ?primary_cluster_id
      ?automatic_failover_status
      ?resharding
      ?auth_token_status
      ?user_groups
      () =
    { primary_cluster_id
    ; automatic_failover_status
    ; resharding
    ; auth_token_status
    ; user_groups
    }

  let parse xml =
    Some
      { primary_cluster_id =
          Aws.Util.option_bind (Aws.Xml.member "PrimaryClusterId" xml) String.parse
      ; automatic_failover_status =
          Aws.Util.option_bind
            (Aws.Xml.member "AutomaticFailoverStatus" xml)
            PendingAutomaticFailoverStatus.parse
      ; resharding =
          Aws.Util.option_bind (Aws.Xml.member "Resharding" xml) ReshardingStatus.parse
      ; auth_token_status =
          Aws.Util.option_bind
            (Aws.Xml.member "AuthTokenStatus" xml)
            AuthTokenUpdateStatus.parse
      ; user_groups =
          Aws.Util.option_bind
            (Aws.Xml.member "UserGroups" xml)
            UserGroupsUpdateStatus.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.user_groups (fun f ->
               Aws.Query.Pair ("UserGroups", UserGroupsUpdateStatus.to_query f))
         ; Aws.Util.option_map v.auth_token_status (fun f ->
               Aws.Query.Pair ("AuthTokenStatus", AuthTokenUpdateStatus.to_query f))
         ; Aws.Util.option_map v.resharding (fun f ->
               Aws.Query.Pair ("Resharding", ReshardingStatus.to_query f))
         ; Aws.Util.option_map v.automatic_failover_status (fun f ->
               Aws.Query.Pair
                 ("AutomaticFailoverStatus", PendingAutomaticFailoverStatus.to_query f))
         ; Aws.Util.option_map v.primary_cluster_id (fun f ->
               Aws.Query.Pair ("PrimaryClusterId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.user_groups (fun f ->
               "UserGroups", UserGroupsUpdateStatus.to_json f)
         ; Aws.Util.option_map v.auth_token_status (fun f ->
               "AuthTokenStatus", AuthTokenUpdateStatus.to_json f)
         ; Aws.Util.option_map v.resharding (fun f ->
               "Resharding", ReshardingStatus.to_json f)
         ; Aws.Util.option_map v.automatic_failover_status (fun f ->
               "AutomaticFailoverStatus", PendingAutomaticFailoverStatus.to_json f)
         ; Aws.Util.option_map v.primary_cluster_id (fun f ->
               "PrimaryClusterId", String.to_json f)
         ])

  let of_json j =
    { primary_cluster_id =
        Aws.Util.option_map (Aws.Json.lookup j "PrimaryClusterId") String.of_json
    ; automatic_failover_status =
        Aws.Util.option_map
          (Aws.Json.lookup j "AutomaticFailoverStatus")
          PendingAutomaticFailoverStatus.of_json
    ; resharding =
        Aws.Util.option_map (Aws.Json.lookup j "Resharding") ReshardingStatus.of_json
    ; auth_token_status =
        Aws.Util.option_map
          (Aws.Json.lookup j "AuthTokenStatus")
          AuthTokenUpdateStatus.of_json
    ; user_groups =
        Aws.Util.option_map
          (Aws.Json.lookup j "UserGroups")
          UserGroupsUpdateStatus.of_json
    }
end

module ReplicationGroupOutpostArnList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map String.parse (Aws.Xml.members "ReplicationGroupOutpostArn" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module Endpoint = struct
  type t =
    { address : String.t option
    ; port : Integer.t option
    }

  let make ?address ?port () = { address; port }

  let parse xml =
    Some
      { address = Aws.Util.option_bind (Aws.Xml.member "Address" xml) String.parse
      ; port = Aws.Util.option_bind (Aws.Xml.member "Port" xml) Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.port (fun f ->
               Aws.Query.Pair ("Port", Integer.to_query f))
         ; Aws.Util.option_map v.address (fun f ->
               Aws.Query.Pair ("Address", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.port (fun f -> "Port", Integer.to_json f)
         ; Aws.Util.option_map v.address (fun f -> "Address", String.to_json f)
         ])

  let of_json j =
    { address = Aws.Util.option_map (Aws.Json.lookup j "Address") String.of_json
    ; port = Aws.Util.option_map (Aws.Json.lookup j "Port") Integer.of_json
    }
end

module NodeGroupMember = struct
  type t =
    { cache_cluster_id : String.t option
    ; cache_node_id : String.t option
    ; read_endpoint : Endpoint.t option
    ; preferred_availability_zone : String.t option
    ; preferred_outpost_arn : String.t option
    ; current_role : String.t option
    }

  let make
      ?cache_cluster_id
      ?cache_node_id
      ?read_endpoint
      ?preferred_availability_zone
      ?preferred_outpost_arn
      ?current_role
      () =
    { cache_cluster_id
    ; cache_node_id
    ; read_endpoint
    ; preferred_availability_zone
    ; preferred_outpost_arn
    ; current_role
    }

  let parse xml =
    Some
      { cache_cluster_id =
          Aws.Util.option_bind (Aws.Xml.member "CacheClusterId" xml) String.parse
      ; cache_node_id =
          Aws.Util.option_bind (Aws.Xml.member "CacheNodeId" xml) String.parse
      ; read_endpoint =
          Aws.Util.option_bind (Aws.Xml.member "ReadEndpoint" xml) Endpoint.parse
      ; preferred_availability_zone =
          Aws.Util.option_bind
            (Aws.Xml.member "PreferredAvailabilityZone" xml)
            String.parse
      ; preferred_outpost_arn =
          Aws.Util.option_bind (Aws.Xml.member "PreferredOutpostArn" xml) String.parse
      ; current_role =
          Aws.Util.option_bind (Aws.Xml.member "CurrentRole" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.current_role (fun f ->
               Aws.Query.Pair ("CurrentRole", String.to_query f))
         ; Aws.Util.option_map v.preferred_outpost_arn (fun f ->
               Aws.Query.Pair ("PreferredOutpostArn", String.to_query f))
         ; Aws.Util.option_map v.preferred_availability_zone (fun f ->
               Aws.Query.Pair ("PreferredAvailabilityZone", String.to_query f))
         ; Aws.Util.option_map v.read_endpoint (fun f ->
               Aws.Query.Pair ("ReadEndpoint", Endpoint.to_query f))
         ; Aws.Util.option_map v.cache_node_id (fun f ->
               Aws.Query.Pair ("CacheNodeId", String.to_query f))
         ; Aws.Util.option_map v.cache_cluster_id (fun f ->
               Aws.Query.Pair ("CacheClusterId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.current_role (fun f -> "CurrentRole", String.to_json f)
         ; Aws.Util.option_map v.preferred_outpost_arn (fun f ->
               "PreferredOutpostArn", String.to_json f)
         ; Aws.Util.option_map v.preferred_availability_zone (fun f ->
               "PreferredAvailabilityZone", String.to_json f)
         ; Aws.Util.option_map v.read_endpoint (fun f ->
               "ReadEndpoint", Endpoint.to_json f)
         ; Aws.Util.option_map v.cache_node_id (fun f -> "CacheNodeId", String.to_json f)
         ; Aws.Util.option_map v.cache_cluster_id (fun f ->
               "CacheClusterId", String.to_json f)
         ])

  let of_json j =
    { cache_cluster_id =
        Aws.Util.option_map (Aws.Json.lookup j "CacheClusterId") String.of_json
    ; cache_node_id = Aws.Util.option_map (Aws.Json.lookup j "CacheNodeId") String.of_json
    ; read_endpoint =
        Aws.Util.option_map (Aws.Json.lookup j "ReadEndpoint") Endpoint.of_json
    ; preferred_availability_zone =
        Aws.Util.option_map (Aws.Json.lookup j "PreferredAvailabilityZone") String.of_json
    ; preferred_outpost_arn =
        Aws.Util.option_map (Aws.Json.lookup j "PreferredOutpostArn") String.of_json
    ; current_role = Aws.Util.option_map (Aws.Json.lookup j "CurrentRole") String.of_json
    }
end

module NodeGroupMemberList = struct
  type t = NodeGroupMember.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map NodeGroupMember.parse (Aws.Xml.members "NodeGroupMember" xml))

  let to_query v = Aws.Query.to_query_list NodeGroupMember.to_query v

  let to_json v = `List (List.map NodeGroupMember.to_json v)

  let of_json j = Aws.Json.to_list NodeGroupMember.of_json j
end

module NodeGroup = struct
  type t =
    { node_group_id : String.t option
    ; status : String.t option
    ; primary_endpoint : Endpoint.t option
    ; reader_endpoint : Endpoint.t option
    ; slots : String.t option
    ; node_group_members : NodeGroupMemberList.t
    }

  let make
      ?node_group_id
      ?status
      ?primary_endpoint
      ?reader_endpoint
      ?slots
      ?(node_group_members = [])
      () =
    { node_group_id
    ; status
    ; primary_endpoint
    ; reader_endpoint
    ; slots
    ; node_group_members
    }

  let parse xml =
    Some
      { node_group_id =
          Aws.Util.option_bind (Aws.Xml.member "NodeGroupId" xml) String.parse
      ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
      ; primary_endpoint =
          Aws.Util.option_bind (Aws.Xml.member "PrimaryEndpoint" xml) Endpoint.parse
      ; reader_endpoint =
          Aws.Util.option_bind (Aws.Xml.member "ReaderEndpoint" xml) Endpoint.parse
      ; slots = Aws.Util.option_bind (Aws.Xml.member "Slots" xml) String.parse
      ; node_group_members =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "NodeGroupMembers" xml)
               NodeGroupMemberList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "NodeGroupMembers.member"
                , NodeGroupMemberList.to_query v.node_group_members ))
         ; Aws.Util.option_map v.slots (fun f ->
               Aws.Query.Pair ("Slots", String.to_query f))
         ; Aws.Util.option_map v.reader_endpoint (fun f ->
               Aws.Query.Pair ("ReaderEndpoint", Endpoint.to_query f))
         ; Aws.Util.option_map v.primary_endpoint (fun f ->
               Aws.Query.Pair ("PrimaryEndpoint", Endpoint.to_query f))
         ; Aws.Util.option_map v.status (fun f ->
               Aws.Query.Pair ("Status", String.to_query f))
         ; Aws.Util.option_map v.node_group_id (fun f ->
               Aws.Query.Pair ("NodeGroupId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("NodeGroupMembers", NodeGroupMemberList.to_json v.node_group_members)
         ; Aws.Util.option_map v.slots (fun f -> "Slots", String.to_json f)
         ; Aws.Util.option_map v.reader_endpoint (fun f ->
               "ReaderEndpoint", Endpoint.to_json f)
         ; Aws.Util.option_map v.primary_endpoint (fun f ->
               "PrimaryEndpoint", Endpoint.to_json f)
         ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
         ; Aws.Util.option_map v.node_group_id (fun f -> "NodeGroupId", String.to_json f)
         ])

  let of_json j =
    { node_group_id = Aws.Util.option_map (Aws.Json.lookup j "NodeGroupId") String.of_json
    ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
    ; primary_endpoint =
        Aws.Util.option_map (Aws.Json.lookup j "PrimaryEndpoint") Endpoint.of_json
    ; reader_endpoint =
        Aws.Util.option_map (Aws.Json.lookup j "ReaderEndpoint") Endpoint.of_json
    ; slots = Aws.Util.option_map (Aws.Json.lookup j "Slots") String.of_json
    ; node_group_members =
        NodeGroupMemberList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "NodeGroupMembers"))
    }
end

module NodeGroupList = struct
  type t = NodeGroup.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map NodeGroup.parse (Aws.Xml.members "NodeGroup" xml))

  let to_query v = Aws.Query.to_query_list NodeGroup.to_query v

  let to_json v = `List (List.map NodeGroup.to_json v)

  let of_json j = Aws.Json.to_list NodeGroup.of_json j
end

module MultiAZStatus = struct
  type t =
    | Enabled
    | Disabled

  let str_to_t = [ "disabled", Disabled; "enabled", Enabled ]

  let t_to_str = [ Disabled, "disabled"; Enabled, "enabled" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module GlobalReplicationGroupInfo = struct
  type t =
    { global_replication_group_id : String.t option
    ; global_replication_group_member_role : String.t option
    }

  let make ?global_replication_group_id ?global_replication_group_member_role () =
    { global_replication_group_id; global_replication_group_member_role }

  let parse xml =
    Some
      { global_replication_group_id =
          Aws.Util.option_bind
            (Aws.Xml.member "GlobalReplicationGroupId" xml)
            String.parse
      ; global_replication_group_member_role =
          Aws.Util.option_bind
            (Aws.Xml.member "GlobalReplicationGroupMemberRole" xml)
            String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.global_replication_group_member_role (fun f ->
               Aws.Query.Pair ("GlobalReplicationGroupMemberRole", String.to_query f))
         ; Aws.Util.option_map v.global_replication_group_id (fun f ->
               Aws.Query.Pair ("GlobalReplicationGroupId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.global_replication_group_member_role (fun f ->
               "GlobalReplicationGroupMemberRole", String.to_json f)
         ; Aws.Util.option_map v.global_replication_group_id (fun f ->
               "GlobalReplicationGroupId", String.to_json f)
         ])

  let of_json j =
    { global_replication_group_id =
        Aws.Util.option_map (Aws.Json.lookup j "GlobalReplicationGroupId") String.of_json
    ; global_replication_group_member_role =
        Aws.Util.option_map
          (Aws.Json.lookup j "GlobalReplicationGroupMemberRole")
          String.of_json
    }
end

module ClusterIdList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "ClusterId" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module ReplicationGroup = struct
  type t =
    { replication_group_id : String.t option
    ; description : String.t option
    ; global_replication_group_info : GlobalReplicationGroupInfo.t option
    ; status : String.t option
    ; pending_modified_values : ReplicationGroupPendingModifiedValues.t option
    ; member_clusters : ClusterIdList.t
    ; node_groups : NodeGroupList.t
    ; snapshotting_cluster_id : String.t option
    ; automatic_failover : AutomaticFailoverStatus.t option
    ; multi_a_z : MultiAZStatus.t option
    ; configuration_endpoint : Endpoint.t option
    ; snapshot_retention_limit : Integer.t option
    ; snapshot_window : String.t option
    ; cluster_enabled : Boolean.t option
    ; cache_node_type : String.t option
    ; auth_token_enabled : Boolean.t option
    ; auth_token_last_modified_date : DateTime.t option
    ; transit_encryption_enabled : Boolean.t option
    ; at_rest_encryption_enabled : Boolean.t option
    ; member_clusters_outpost_arns : ReplicationGroupOutpostArnList.t
    ; kms_key_id : String.t option
    ; a_r_n : String.t option
    ; user_group_ids : UserGroupIdList.t
    }

  let make
      ?replication_group_id
      ?description
      ?global_replication_group_info
      ?status
      ?pending_modified_values
      ?(member_clusters = [])
      ?(node_groups = [])
      ?snapshotting_cluster_id
      ?automatic_failover
      ?multi_a_z
      ?configuration_endpoint
      ?snapshot_retention_limit
      ?snapshot_window
      ?cluster_enabled
      ?cache_node_type
      ?auth_token_enabled
      ?auth_token_last_modified_date
      ?transit_encryption_enabled
      ?at_rest_encryption_enabled
      ?(member_clusters_outpost_arns = [])
      ?kms_key_id
      ?a_r_n
      ?(user_group_ids = [])
      () =
    { replication_group_id
    ; description
    ; global_replication_group_info
    ; status
    ; pending_modified_values
    ; member_clusters
    ; node_groups
    ; snapshotting_cluster_id
    ; automatic_failover
    ; multi_a_z
    ; configuration_endpoint
    ; snapshot_retention_limit
    ; snapshot_window
    ; cluster_enabled
    ; cache_node_type
    ; auth_token_enabled
    ; auth_token_last_modified_date
    ; transit_encryption_enabled
    ; at_rest_encryption_enabled
    ; member_clusters_outpost_arns
    ; kms_key_id
    ; a_r_n
    ; user_group_ids
    }

  let parse xml =
    Some
      { replication_group_id =
          Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse
      ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
      ; global_replication_group_info =
          Aws.Util.option_bind
            (Aws.Xml.member "GlobalReplicationGroupInfo" xml)
            GlobalReplicationGroupInfo.parse
      ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
      ; pending_modified_values =
          Aws.Util.option_bind
            (Aws.Xml.member "PendingModifiedValues" xml)
            ReplicationGroupPendingModifiedValues.parse
      ; member_clusters =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "MemberClusters" xml)
               ClusterIdList.parse)
      ; node_groups =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "NodeGroups" xml) NodeGroupList.parse)
      ; snapshotting_cluster_id =
          Aws.Util.option_bind (Aws.Xml.member "SnapshottingClusterId" xml) String.parse
      ; automatic_failover =
          Aws.Util.option_bind
            (Aws.Xml.member "AutomaticFailover" xml)
            AutomaticFailoverStatus.parse
      ; multi_a_z =
          Aws.Util.option_bind (Aws.Xml.member "MultiAZ" xml) MultiAZStatus.parse
      ; configuration_endpoint =
          Aws.Util.option_bind (Aws.Xml.member "ConfigurationEndpoint" xml) Endpoint.parse
      ; snapshot_retention_limit =
          Aws.Util.option_bind (Aws.Xml.member "SnapshotRetentionLimit" xml) Integer.parse
      ; snapshot_window =
          Aws.Util.option_bind (Aws.Xml.member "SnapshotWindow" xml) String.parse
      ; cluster_enabled =
          Aws.Util.option_bind (Aws.Xml.member "ClusterEnabled" xml) Boolean.parse
      ; cache_node_type =
          Aws.Util.option_bind (Aws.Xml.member "CacheNodeType" xml) String.parse
      ; auth_token_enabled =
          Aws.Util.option_bind (Aws.Xml.member "AuthTokenEnabled" xml) Boolean.parse
      ; auth_token_last_modified_date =
          Aws.Util.option_bind
            (Aws.Xml.member "AuthTokenLastModifiedDate" xml)
            DateTime.parse
      ; transit_encryption_enabled =
          Aws.Util.option_bind
            (Aws.Xml.member "TransitEncryptionEnabled" xml)
            Boolean.parse
      ; at_rest_encryption_enabled =
          Aws.Util.option_bind
            (Aws.Xml.member "AtRestEncryptionEnabled" xml)
            Boolean.parse
      ; member_clusters_outpost_arns =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "MemberClustersOutpostArns" xml)
               ReplicationGroupOutpostArnList.parse)
      ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse
      ; a_r_n = Aws.Util.option_bind (Aws.Xml.member "ARN" xml) String.parse
      ; user_group_ids =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "UserGroupIds" xml)
               UserGroupIdList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("UserGroupIds.member", UserGroupIdList.to_query v.user_group_ids))
         ; Aws.Util.option_map v.a_r_n (fun f ->
               Aws.Query.Pair ("ARN", String.to_query f))
         ; Aws.Util.option_map v.kms_key_id (fun f ->
               Aws.Query.Pair ("KmsKeyId", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "MemberClustersOutpostArns.member"
                , ReplicationGroupOutpostArnList.to_query v.member_clusters_outpost_arns
                ))
         ; Aws.Util.option_map v.at_rest_encryption_enabled (fun f ->
               Aws.Query.Pair ("AtRestEncryptionEnabled", Boolean.to_query f))
         ; Aws.Util.option_map v.transit_encryption_enabled (fun f ->
               Aws.Query.Pair ("TransitEncryptionEnabled", Boolean.to_query f))
         ; Aws.Util.option_map v.auth_token_last_modified_date (fun f ->
               Aws.Query.Pair ("AuthTokenLastModifiedDate", DateTime.to_query f))
         ; Aws.Util.option_map v.auth_token_enabled (fun f ->
               Aws.Query.Pair ("AuthTokenEnabled", Boolean.to_query f))
         ; Aws.Util.option_map v.cache_node_type (fun f ->
               Aws.Query.Pair ("CacheNodeType", String.to_query f))
         ; Aws.Util.option_map v.cluster_enabled (fun f ->
               Aws.Query.Pair ("ClusterEnabled", Boolean.to_query f))
         ; Aws.Util.option_map v.snapshot_window (fun f ->
               Aws.Query.Pair ("SnapshotWindow", String.to_query f))
         ; Aws.Util.option_map v.snapshot_retention_limit (fun f ->
               Aws.Query.Pair ("SnapshotRetentionLimit", Integer.to_query f))
         ; Aws.Util.option_map v.configuration_endpoint (fun f ->
               Aws.Query.Pair ("ConfigurationEndpoint", Endpoint.to_query f))
         ; Aws.Util.option_map v.multi_a_z (fun f ->
               Aws.Query.Pair ("MultiAZ", MultiAZStatus.to_query f))
         ; Aws.Util.option_map v.automatic_failover (fun f ->
               Aws.Query.Pair ("AutomaticFailover", AutomaticFailoverStatus.to_query f))
         ; Aws.Util.option_map v.snapshotting_cluster_id (fun f ->
               Aws.Query.Pair ("SnapshottingClusterId", String.to_query f))
         ; Some
             (Aws.Query.Pair ("NodeGroups.member", NodeGroupList.to_query v.node_groups))
         ; Some
             (Aws.Query.Pair
                ("MemberClusters.member", ClusterIdList.to_query v.member_clusters))
         ; Aws.Util.option_map v.pending_modified_values (fun f ->
               Aws.Query.Pair
                 ( "PendingModifiedValues"
                 , ReplicationGroupPendingModifiedValues.to_query f ))
         ; Aws.Util.option_map v.status (fun f ->
               Aws.Query.Pair ("Status", String.to_query f))
         ; Aws.Util.option_map v.global_replication_group_info (fun f ->
               Aws.Query.Pair
                 ("GlobalReplicationGroupInfo", GlobalReplicationGroupInfo.to_query f))
         ; Aws.Util.option_map v.description (fun f ->
               Aws.Query.Pair ("Description", String.to_query f))
         ; Aws.Util.option_map v.replication_group_id (fun f ->
               Aws.Query.Pair ("ReplicationGroupId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("UserGroupIds", UserGroupIdList.to_json v.user_group_ids)
         ; Aws.Util.option_map v.a_r_n (fun f -> "ARN", String.to_json f)
         ; Aws.Util.option_map v.kms_key_id (fun f -> "KmsKeyId", String.to_json f)
         ; Some
             ( "MemberClustersOutpostArns"
             , ReplicationGroupOutpostArnList.to_json v.member_clusters_outpost_arns )
         ; Aws.Util.option_map v.at_rest_encryption_enabled (fun f ->
               "AtRestEncryptionEnabled", Boolean.to_json f)
         ; Aws.Util.option_map v.transit_encryption_enabled (fun f ->
               "TransitEncryptionEnabled", Boolean.to_json f)
         ; Aws.Util.option_map v.auth_token_last_modified_date (fun f ->
               "AuthTokenLastModifiedDate", DateTime.to_json f)
         ; Aws.Util.option_map v.auth_token_enabled (fun f ->
               "AuthTokenEnabled", Boolean.to_json f)
         ; Aws.Util.option_map v.cache_node_type (fun f ->
               "CacheNodeType", String.to_json f)
         ; Aws.Util.option_map v.cluster_enabled (fun f ->
               "ClusterEnabled", Boolean.to_json f)
         ; Aws.Util.option_map v.snapshot_window (fun f ->
               "SnapshotWindow", String.to_json f)
         ; Aws.Util.option_map v.snapshot_retention_limit (fun f ->
               "SnapshotRetentionLimit", Integer.to_json f)
         ; Aws.Util.option_map v.configuration_endpoint (fun f ->
               "ConfigurationEndpoint", Endpoint.to_json f)
         ; Aws.Util.option_map v.multi_a_z (fun f -> "MultiAZ", MultiAZStatus.to_json f)
         ; Aws.Util.option_map v.automatic_failover (fun f ->
               "AutomaticFailover", AutomaticFailoverStatus.to_json f)
         ; Aws.Util.option_map v.snapshotting_cluster_id (fun f ->
               "SnapshottingClusterId", String.to_json f)
         ; Some ("NodeGroups", NodeGroupList.to_json v.node_groups)
         ; Some ("MemberClusters", ClusterIdList.to_json v.member_clusters)
         ; Aws.Util.option_map v.pending_modified_values (fun f ->
               "PendingModifiedValues", ReplicationGroupPendingModifiedValues.to_json f)
         ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
         ; Aws.Util.option_map v.global_replication_group_info (fun f ->
               "GlobalReplicationGroupInfo", GlobalReplicationGroupInfo.to_json f)
         ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
         ; Aws.Util.option_map v.replication_group_id (fun f ->
               "ReplicationGroupId", String.to_json f)
         ])

  let of_json j =
    { replication_group_id =
        Aws.Util.option_map (Aws.Json.lookup j "ReplicationGroupId") String.of_json
    ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
    ; global_replication_group_info =
        Aws.Util.option_map
          (Aws.Json.lookup j "GlobalReplicationGroupInfo")
          GlobalReplicationGroupInfo.of_json
    ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
    ; pending_modified_values =
        Aws.Util.option_map
          (Aws.Json.lookup j "PendingModifiedValues")
          ReplicationGroupPendingModifiedValues.of_json
    ; member_clusters =
        ClusterIdList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "MemberClusters"))
    ; node_groups =
        NodeGroupList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "NodeGroups"))
    ; snapshotting_cluster_id =
        Aws.Util.option_map (Aws.Json.lookup j "SnapshottingClusterId") String.of_json
    ; automatic_failover =
        Aws.Util.option_map
          (Aws.Json.lookup j "AutomaticFailover")
          AutomaticFailoverStatus.of_json
    ; multi_a_z = Aws.Util.option_map (Aws.Json.lookup j "MultiAZ") MultiAZStatus.of_json
    ; configuration_endpoint =
        Aws.Util.option_map (Aws.Json.lookup j "ConfigurationEndpoint") Endpoint.of_json
    ; snapshot_retention_limit =
        Aws.Util.option_map (Aws.Json.lookup j "SnapshotRetentionLimit") Integer.of_json
    ; snapshot_window =
        Aws.Util.option_map (Aws.Json.lookup j "SnapshotWindow") String.of_json
    ; cluster_enabled =
        Aws.Util.option_map (Aws.Json.lookup j "ClusterEnabled") Boolean.of_json
    ; cache_node_type =
        Aws.Util.option_map (Aws.Json.lookup j "CacheNodeType") String.of_json
    ; auth_token_enabled =
        Aws.Util.option_map (Aws.Json.lookup j "AuthTokenEnabled") Boolean.of_json
    ; auth_token_last_modified_date =
        Aws.Util.option_map
          (Aws.Json.lookup j "AuthTokenLastModifiedDate")
          DateTime.of_json
    ; transit_encryption_enabled =
        Aws.Util.option_map (Aws.Json.lookup j "TransitEncryptionEnabled") Boolean.of_json
    ; at_rest_encryption_enabled =
        Aws.Util.option_map (Aws.Json.lookup j "AtRestEncryptionEnabled") Boolean.of_json
    ; member_clusters_outpost_arns =
        ReplicationGroupOutpostArnList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "MemberClustersOutpostArns"))
    ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
    ; a_r_n = Aws.Util.option_map (Aws.Json.lookup j "ARN") String.of_json
    ; user_group_ids =
        UserGroupIdList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "UserGroupIds"))
    }
end

module StartMigrationResponse = struct
  type t = { replication_group : ReplicationGroup.t option }

  let make ?replication_group () = { replication_group }

  let parse xml =
    Some
      { replication_group =
          Aws.Util.option_bind
            (Aws.Xml.member "ReplicationGroup" xml)
            ReplicationGroup.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.replication_group (fun f ->
               Aws.Query.Pair ("ReplicationGroup", ReplicationGroup.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.replication_group (fun f ->
               "ReplicationGroup", ReplicationGroup.to_json f)
         ])

  let of_json j =
    { replication_group =
        Aws.Util.option_map
          (Aws.Json.lookup j "ReplicationGroup")
          ReplicationGroup.of_json
    }
end

module DescribeEngineDefaultParametersMessage = struct
  type t =
    { cache_parameter_group_family : String.t
    ; max_records : Integer.t option
    ; marker : String.t option
    }

  let make ~cache_parameter_group_family ?max_records ?marker () =
    { cache_parameter_group_family; max_records; marker }

  let parse xml =
    Some
      { cache_parameter_group_family =
          Aws.Xml.required
            "CacheParameterGroupFamily"
            (Aws.Util.option_bind
               (Aws.Xml.member "CacheParameterGroupFamily" xml)
               String.parse)
      ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ; Aws.Util.option_map v.max_records (fun f ->
               Aws.Query.Pair ("MaxRecords", Integer.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "CacheParameterGroupFamily"
                , String.to_query v.cache_parameter_group_family ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
         ; Some
             ("CacheParameterGroupFamily", String.to_json v.cache_parameter_group_family)
         ])

  let of_json j =
    { cache_parameter_group_family =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheParameterGroupFamily"))
    ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
    ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    }
end

module SecurityGroupMembership = struct
  type t =
    { security_group_id : String.t option
    ; status : String.t option
    }

  let make ?security_group_id ?status () = { security_group_id; status }

  let parse xml =
    Some
      { security_group_id =
          Aws.Util.option_bind (Aws.Xml.member "SecurityGroupId" xml) String.parse
      ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.status (fun f ->
               Aws.Query.Pair ("Status", String.to_query f))
         ; Aws.Util.option_map v.security_group_id (fun f ->
               Aws.Query.Pair ("SecurityGroupId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
         ; Aws.Util.option_map v.security_group_id (fun f ->
               "SecurityGroupId", String.to_json f)
         ])

  let of_json j =
    { security_group_id =
        Aws.Util.option_map (Aws.Json.lookup j "SecurityGroupId") String.of_json
    ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
    }
end

module SecurityGroupMembershipList = struct
  type t = SecurityGroupMembership.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map SecurityGroupMembership.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list SecurityGroupMembership.to_query v

  let to_json v = `List (List.map SecurityGroupMembership.to_json v)

  let of_json j = Aws.Json.to_list SecurityGroupMembership.of_json j
end

module CacheNodeIdsList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "CacheNodeId" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module PendingModifiedValues = struct
  type t =
    { num_cache_nodes : Integer.t option
    ; cache_node_ids_to_remove : CacheNodeIdsList.t
    ; engine_version : String.t option
    ; cache_node_type : String.t option
    ; auth_token_status : AuthTokenUpdateStatus.t option
    }

  let make
      ?num_cache_nodes
      ?(cache_node_ids_to_remove = [])
      ?engine_version
      ?cache_node_type
      ?auth_token_status
      () =
    { num_cache_nodes
    ; cache_node_ids_to_remove
    ; engine_version
    ; cache_node_type
    ; auth_token_status
    }

  let parse xml =
    Some
      { num_cache_nodes =
          Aws.Util.option_bind (Aws.Xml.member "NumCacheNodes" xml) Integer.parse
      ; cache_node_ids_to_remove =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "CacheNodeIdsToRemove" xml)
               CacheNodeIdsList.parse)
      ; engine_version =
          Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
      ; cache_node_type =
          Aws.Util.option_bind (Aws.Xml.member "CacheNodeType" xml) String.parse
      ; auth_token_status =
          Aws.Util.option_bind
            (Aws.Xml.member "AuthTokenStatus" xml)
            AuthTokenUpdateStatus.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.auth_token_status (fun f ->
               Aws.Query.Pair ("AuthTokenStatus", AuthTokenUpdateStatus.to_query f))
         ; Aws.Util.option_map v.cache_node_type (fun f ->
               Aws.Query.Pair ("CacheNodeType", String.to_query f))
         ; Aws.Util.option_map v.engine_version (fun f ->
               Aws.Query.Pair ("EngineVersion", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "CacheNodeIdsToRemove.member"
                , CacheNodeIdsList.to_query v.cache_node_ids_to_remove ))
         ; Aws.Util.option_map v.num_cache_nodes (fun f ->
               Aws.Query.Pair ("NumCacheNodes", Integer.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.auth_token_status (fun f ->
               "AuthTokenStatus", AuthTokenUpdateStatus.to_json f)
         ; Aws.Util.option_map v.cache_node_type (fun f ->
               "CacheNodeType", String.to_json f)
         ; Aws.Util.option_map v.engine_version (fun f ->
               "EngineVersion", String.to_json f)
         ; Some
             ("CacheNodeIdsToRemove", CacheNodeIdsList.to_json v.cache_node_ids_to_remove)
         ; Aws.Util.option_map v.num_cache_nodes (fun f ->
               "NumCacheNodes", Integer.to_json f)
         ])

  let of_json j =
    { num_cache_nodes =
        Aws.Util.option_map (Aws.Json.lookup j "NumCacheNodes") Integer.of_json
    ; cache_node_ids_to_remove =
        CacheNodeIdsList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheNodeIdsToRemove"))
    ; engine_version =
        Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
    ; cache_node_type =
        Aws.Util.option_map (Aws.Json.lookup j "CacheNodeType") String.of_json
    ; auth_token_status =
        Aws.Util.option_map
          (Aws.Json.lookup j "AuthTokenStatus")
          AuthTokenUpdateStatus.of_json
    }
end

module NotificationConfiguration = struct
  type t =
    { topic_arn : String.t option
    ; topic_status : String.t option
    }

  let make ?topic_arn ?topic_status () = { topic_arn; topic_status }

  let parse xml =
    Some
      { topic_arn = Aws.Util.option_bind (Aws.Xml.member "TopicArn" xml) String.parse
      ; topic_status =
          Aws.Util.option_bind (Aws.Xml.member "TopicStatus" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.topic_status (fun f ->
               Aws.Query.Pair ("TopicStatus", String.to_query f))
         ; Aws.Util.option_map v.topic_arn (fun f ->
               Aws.Query.Pair ("TopicArn", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.topic_status (fun f -> "TopicStatus", String.to_json f)
         ; Aws.Util.option_map v.topic_arn (fun f -> "TopicArn", String.to_json f)
         ])

  let of_json j =
    { topic_arn = Aws.Util.option_map (Aws.Json.lookup j "TopicArn") String.of_json
    ; topic_status = Aws.Util.option_map (Aws.Json.lookup j "TopicStatus") String.of_json
    }
end

module CacheSecurityGroupMembership = struct
  type t =
    { cache_security_group_name : String.t option
    ; status : String.t option
    }

  let make ?cache_security_group_name ?status () = { cache_security_group_name; status }

  let parse xml =
    Some
      { cache_security_group_name =
          Aws.Util.option_bind (Aws.Xml.member "CacheSecurityGroupName" xml) String.parse
      ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.status (fun f ->
               Aws.Query.Pair ("Status", String.to_query f))
         ; Aws.Util.option_map v.cache_security_group_name (fun f ->
               Aws.Query.Pair ("CacheSecurityGroupName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
         ; Aws.Util.option_map v.cache_security_group_name (fun f ->
               "CacheSecurityGroupName", String.to_json f)
         ])

  let of_json j =
    { cache_security_group_name =
        Aws.Util.option_map (Aws.Json.lookup j "CacheSecurityGroupName") String.of_json
    ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
    }
end

module CacheSecurityGroupMembershipList = struct
  type t = CacheSecurityGroupMembership.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map
         CacheSecurityGroupMembership.parse
         (Aws.Xml.members "CacheSecurityGroup" xml))

  let to_query v = Aws.Query.to_query_list CacheSecurityGroupMembership.to_query v

  let to_json v = `List (List.map CacheSecurityGroupMembership.to_json v)

  let of_json j = Aws.Json.to_list CacheSecurityGroupMembership.of_json j
end

module CacheParameterGroupStatus = struct
  type t =
    { cache_parameter_group_name : String.t option
    ; parameter_apply_status : String.t option
    ; cache_node_ids_to_reboot : CacheNodeIdsList.t
    }

  let make
      ?cache_parameter_group_name
      ?parameter_apply_status
      ?(cache_node_ids_to_reboot = [])
      () =
    { cache_parameter_group_name; parameter_apply_status; cache_node_ids_to_reboot }

  let parse xml =
    Some
      { cache_parameter_group_name =
          Aws.Util.option_bind (Aws.Xml.member "CacheParameterGroupName" xml) String.parse
      ; parameter_apply_status =
          Aws.Util.option_bind (Aws.Xml.member "ParameterApplyStatus" xml) String.parse
      ; cache_node_ids_to_reboot =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "CacheNodeIdsToReboot" xml)
               CacheNodeIdsList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "CacheNodeIdsToReboot.member"
                , CacheNodeIdsList.to_query v.cache_node_ids_to_reboot ))
         ; Aws.Util.option_map v.parameter_apply_status (fun f ->
               Aws.Query.Pair ("ParameterApplyStatus", String.to_query f))
         ; Aws.Util.option_map v.cache_parameter_group_name (fun f ->
               Aws.Query.Pair ("CacheParameterGroupName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ("CacheNodeIdsToReboot", CacheNodeIdsList.to_json v.cache_node_ids_to_reboot)
         ; Aws.Util.option_map v.parameter_apply_status (fun f ->
               "ParameterApplyStatus", String.to_json f)
         ; Aws.Util.option_map v.cache_parameter_group_name (fun f ->
               "CacheParameterGroupName", String.to_json f)
         ])

  let of_json j =
    { cache_parameter_group_name =
        Aws.Util.option_map (Aws.Json.lookup j "CacheParameterGroupName") String.of_json
    ; parameter_apply_status =
        Aws.Util.option_map (Aws.Json.lookup j "ParameterApplyStatus") String.of_json
    ; cache_node_ids_to_reboot =
        CacheNodeIdsList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheNodeIdsToReboot"))
    }
end

module CacheNode = struct
  type t =
    { cache_node_id : String.t option
    ; cache_node_status : String.t option
    ; cache_node_create_time : DateTime.t option
    ; endpoint : Endpoint.t option
    ; parameter_group_status : String.t option
    ; source_cache_node_id : String.t option
    ; customer_availability_zone : String.t option
    ; customer_outpost_arn : String.t option
    }

  let make
      ?cache_node_id
      ?cache_node_status
      ?cache_node_create_time
      ?endpoint
      ?parameter_group_status
      ?source_cache_node_id
      ?customer_availability_zone
      ?customer_outpost_arn
      () =
    { cache_node_id
    ; cache_node_status
    ; cache_node_create_time
    ; endpoint
    ; parameter_group_status
    ; source_cache_node_id
    ; customer_availability_zone
    ; customer_outpost_arn
    }

  let parse xml =
    Some
      { cache_node_id =
          Aws.Util.option_bind (Aws.Xml.member "CacheNodeId" xml) String.parse
      ; cache_node_status =
          Aws.Util.option_bind (Aws.Xml.member "CacheNodeStatus" xml) String.parse
      ; cache_node_create_time =
          Aws.Util.option_bind (Aws.Xml.member "CacheNodeCreateTime" xml) DateTime.parse
      ; endpoint = Aws.Util.option_bind (Aws.Xml.member "Endpoint" xml) Endpoint.parse
      ; parameter_group_status =
          Aws.Util.option_bind (Aws.Xml.member "ParameterGroupStatus" xml) String.parse
      ; source_cache_node_id =
          Aws.Util.option_bind (Aws.Xml.member "SourceCacheNodeId" xml) String.parse
      ; customer_availability_zone =
          Aws.Util.option_bind
            (Aws.Xml.member "CustomerAvailabilityZone" xml)
            String.parse
      ; customer_outpost_arn =
          Aws.Util.option_bind (Aws.Xml.member "CustomerOutpostArn" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.customer_outpost_arn (fun f ->
               Aws.Query.Pair ("CustomerOutpostArn", String.to_query f))
         ; Aws.Util.option_map v.customer_availability_zone (fun f ->
               Aws.Query.Pair ("CustomerAvailabilityZone", String.to_query f))
         ; Aws.Util.option_map v.source_cache_node_id (fun f ->
               Aws.Query.Pair ("SourceCacheNodeId", String.to_query f))
         ; Aws.Util.option_map v.parameter_group_status (fun f ->
               Aws.Query.Pair ("ParameterGroupStatus", String.to_query f))
         ; Aws.Util.option_map v.endpoint (fun f ->
               Aws.Query.Pair ("Endpoint", Endpoint.to_query f))
         ; Aws.Util.option_map v.cache_node_create_time (fun f ->
               Aws.Query.Pair ("CacheNodeCreateTime", DateTime.to_query f))
         ; Aws.Util.option_map v.cache_node_status (fun f ->
               Aws.Query.Pair ("CacheNodeStatus", String.to_query f))
         ; Aws.Util.option_map v.cache_node_id (fun f ->
               Aws.Query.Pair ("CacheNodeId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.customer_outpost_arn (fun f ->
               "CustomerOutpostArn", String.to_json f)
         ; Aws.Util.option_map v.customer_availability_zone (fun f ->
               "CustomerAvailabilityZone", String.to_json f)
         ; Aws.Util.option_map v.source_cache_node_id (fun f ->
               "SourceCacheNodeId", String.to_json f)
         ; Aws.Util.option_map v.parameter_group_status (fun f ->
               "ParameterGroupStatus", String.to_json f)
         ; Aws.Util.option_map v.endpoint (fun f -> "Endpoint", Endpoint.to_json f)
         ; Aws.Util.option_map v.cache_node_create_time (fun f ->
               "CacheNodeCreateTime", DateTime.to_json f)
         ; Aws.Util.option_map v.cache_node_status (fun f ->
               "CacheNodeStatus", String.to_json f)
         ; Aws.Util.option_map v.cache_node_id (fun f -> "CacheNodeId", String.to_json f)
         ])

  let of_json j =
    { cache_node_id = Aws.Util.option_map (Aws.Json.lookup j "CacheNodeId") String.of_json
    ; cache_node_status =
        Aws.Util.option_map (Aws.Json.lookup j "CacheNodeStatus") String.of_json
    ; cache_node_create_time =
        Aws.Util.option_map (Aws.Json.lookup j "CacheNodeCreateTime") DateTime.of_json
    ; endpoint = Aws.Util.option_map (Aws.Json.lookup j "Endpoint") Endpoint.of_json
    ; parameter_group_status =
        Aws.Util.option_map (Aws.Json.lookup j "ParameterGroupStatus") String.of_json
    ; source_cache_node_id =
        Aws.Util.option_map (Aws.Json.lookup j "SourceCacheNodeId") String.of_json
    ; customer_availability_zone =
        Aws.Util.option_map (Aws.Json.lookup j "CustomerAvailabilityZone") String.of_json
    ; customer_outpost_arn =
        Aws.Util.option_map (Aws.Json.lookup j "CustomerOutpostArn") String.of_json
    }
end

module CacheNodeList = struct
  type t = CacheNode.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map CacheNode.parse (Aws.Xml.members "CacheNode" xml))

  let to_query v = Aws.Query.to_query_list CacheNode.to_query v

  let to_json v = `List (List.map CacheNode.to_json v)

  let of_json j = Aws.Json.to_list CacheNode.of_json j
end

module CacheCluster = struct
  type t =
    { cache_cluster_id : String.t option
    ; configuration_endpoint : Endpoint.t option
    ; client_download_landing_page : String.t option
    ; cache_node_type : String.t option
    ; engine : String.t option
    ; engine_version : String.t option
    ; cache_cluster_status : String.t option
    ; num_cache_nodes : Integer.t option
    ; preferred_availability_zone : String.t option
    ; preferred_outpost_arn : String.t option
    ; cache_cluster_create_time : DateTime.t option
    ; preferred_maintenance_window : String.t option
    ; pending_modified_values : PendingModifiedValues.t option
    ; notification_configuration : NotificationConfiguration.t option
    ; cache_security_groups : CacheSecurityGroupMembershipList.t
    ; cache_parameter_group : CacheParameterGroupStatus.t option
    ; cache_subnet_group_name : String.t option
    ; cache_nodes : CacheNodeList.t
    ; auto_minor_version_upgrade : Boolean.t option
    ; security_groups : SecurityGroupMembershipList.t
    ; replication_group_id : String.t option
    ; snapshot_retention_limit : Integer.t option
    ; snapshot_window : String.t option
    ; auth_token_enabled : Boolean.t option
    ; auth_token_last_modified_date : DateTime.t option
    ; transit_encryption_enabled : Boolean.t option
    ; at_rest_encryption_enabled : Boolean.t option
    ; a_r_n : String.t option
    }

  let make
      ?cache_cluster_id
      ?configuration_endpoint
      ?client_download_landing_page
      ?cache_node_type
      ?engine
      ?engine_version
      ?cache_cluster_status
      ?num_cache_nodes
      ?preferred_availability_zone
      ?preferred_outpost_arn
      ?cache_cluster_create_time
      ?preferred_maintenance_window
      ?pending_modified_values
      ?notification_configuration
      ?(cache_security_groups = [])
      ?cache_parameter_group
      ?cache_subnet_group_name
      ?(cache_nodes = [])
      ?auto_minor_version_upgrade
      ?(security_groups = [])
      ?replication_group_id
      ?snapshot_retention_limit
      ?snapshot_window
      ?auth_token_enabled
      ?auth_token_last_modified_date
      ?transit_encryption_enabled
      ?at_rest_encryption_enabled
      ?a_r_n
      () =
    { cache_cluster_id
    ; configuration_endpoint
    ; client_download_landing_page
    ; cache_node_type
    ; engine
    ; engine_version
    ; cache_cluster_status
    ; num_cache_nodes
    ; preferred_availability_zone
    ; preferred_outpost_arn
    ; cache_cluster_create_time
    ; preferred_maintenance_window
    ; pending_modified_values
    ; notification_configuration
    ; cache_security_groups
    ; cache_parameter_group
    ; cache_subnet_group_name
    ; cache_nodes
    ; auto_minor_version_upgrade
    ; security_groups
    ; replication_group_id
    ; snapshot_retention_limit
    ; snapshot_window
    ; auth_token_enabled
    ; auth_token_last_modified_date
    ; transit_encryption_enabled
    ; at_rest_encryption_enabled
    ; a_r_n
    }

  let parse xml =
    Some
      { cache_cluster_id =
          Aws.Util.option_bind (Aws.Xml.member "CacheClusterId" xml) String.parse
      ; configuration_endpoint =
          Aws.Util.option_bind (Aws.Xml.member "ConfigurationEndpoint" xml) Endpoint.parse
      ; client_download_landing_page =
          Aws.Util.option_bind
            (Aws.Xml.member "ClientDownloadLandingPage" xml)
            String.parse
      ; cache_node_type =
          Aws.Util.option_bind (Aws.Xml.member "CacheNodeType" xml) String.parse
      ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
      ; engine_version =
          Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
      ; cache_cluster_status =
          Aws.Util.option_bind (Aws.Xml.member "CacheClusterStatus" xml) String.parse
      ; num_cache_nodes =
          Aws.Util.option_bind (Aws.Xml.member "NumCacheNodes" xml) Integer.parse
      ; preferred_availability_zone =
          Aws.Util.option_bind
            (Aws.Xml.member "PreferredAvailabilityZone" xml)
            String.parse
      ; preferred_outpost_arn =
          Aws.Util.option_bind (Aws.Xml.member "PreferredOutpostArn" xml) String.parse
      ; cache_cluster_create_time =
          Aws.Util.option_bind
            (Aws.Xml.member "CacheClusterCreateTime" xml)
            DateTime.parse
      ; preferred_maintenance_window =
          Aws.Util.option_bind
            (Aws.Xml.member "PreferredMaintenanceWindow" xml)
            String.parse
      ; pending_modified_values =
          Aws.Util.option_bind
            (Aws.Xml.member "PendingModifiedValues" xml)
            PendingModifiedValues.parse
      ; notification_configuration =
          Aws.Util.option_bind
            (Aws.Xml.member "NotificationConfiguration" xml)
            NotificationConfiguration.parse
      ; cache_security_groups =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "CacheSecurityGroups" xml)
               CacheSecurityGroupMembershipList.parse)
      ; cache_parameter_group =
          Aws.Util.option_bind
            (Aws.Xml.member "CacheParameterGroup" xml)
            CacheParameterGroupStatus.parse
      ; cache_subnet_group_name =
          Aws.Util.option_bind (Aws.Xml.member "CacheSubnetGroupName" xml) String.parse
      ; cache_nodes =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "CacheNodes" xml) CacheNodeList.parse)
      ; auto_minor_version_upgrade =
          Aws.Util.option_bind
            (Aws.Xml.member "AutoMinorVersionUpgrade" xml)
            Boolean.parse
      ; security_groups =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "SecurityGroups" xml)
               SecurityGroupMembershipList.parse)
      ; replication_group_id =
          Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse
      ; snapshot_retention_limit =
          Aws.Util.option_bind (Aws.Xml.member "SnapshotRetentionLimit" xml) Integer.parse
      ; snapshot_window =
          Aws.Util.option_bind (Aws.Xml.member "SnapshotWindow" xml) String.parse
      ; auth_token_enabled =
          Aws.Util.option_bind (Aws.Xml.member "AuthTokenEnabled" xml) Boolean.parse
      ; auth_token_last_modified_date =
          Aws.Util.option_bind
            (Aws.Xml.member "AuthTokenLastModifiedDate" xml)
            DateTime.parse
      ; transit_encryption_enabled =
          Aws.Util.option_bind
            (Aws.Xml.member "TransitEncryptionEnabled" xml)
            Boolean.parse
      ; at_rest_encryption_enabled =
          Aws.Util.option_bind
            (Aws.Xml.member "AtRestEncryptionEnabled" xml)
            Boolean.parse
      ; a_r_n = Aws.Util.option_bind (Aws.Xml.member "ARN" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.a_r_n (fun f ->
               Aws.Query.Pair ("ARN", String.to_query f))
         ; Aws.Util.option_map v.at_rest_encryption_enabled (fun f ->
               Aws.Query.Pair ("AtRestEncryptionEnabled", Boolean.to_query f))
         ; Aws.Util.option_map v.transit_encryption_enabled (fun f ->
               Aws.Query.Pair ("TransitEncryptionEnabled", Boolean.to_query f))
         ; Aws.Util.option_map v.auth_token_last_modified_date (fun f ->
               Aws.Query.Pair ("AuthTokenLastModifiedDate", DateTime.to_query f))
         ; Aws.Util.option_map v.auth_token_enabled (fun f ->
               Aws.Query.Pair ("AuthTokenEnabled", Boolean.to_query f))
         ; Aws.Util.option_map v.snapshot_window (fun f ->
               Aws.Query.Pair ("SnapshotWindow", String.to_query f))
         ; Aws.Util.option_map v.snapshot_retention_limit (fun f ->
               Aws.Query.Pair ("SnapshotRetentionLimit", Integer.to_query f))
         ; Aws.Util.option_map v.replication_group_id (fun f ->
               Aws.Query.Pair ("ReplicationGroupId", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "SecurityGroups.member"
                , SecurityGroupMembershipList.to_query v.security_groups ))
         ; Aws.Util.option_map v.auto_minor_version_upgrade (fun f ->
               Aws.Query.Pair ("AutoMinorVersionUpgrade", Boolean.to_query f))
         ; Some
             (Aws.Query.Pair ("CacheNodes.member", CacheNodeList.to_query v.cache_nodes))
         ; Aws.Util.option_map v.cache_subnet_group_name (fun f ->
               Aws.Query.Pair ("CacheSubnetGroupName", String.to_query f))
         ; Aws.Util.option_map v.cache_parameter_group (fun f ->
               Aws.Query.Pair ("CacheParameterGroup", CacheParameterGroupStatus.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "CacheSecurityGroups.member"
                , CacheSecurityGroupMembershipList.to_query v.cache_security_groups ))
         ; Aws.Util.option_map v.notification_configuration (fun f ->
               Aws.Query.Pair
                 ("NotificationConfiguration", NotificationConfiguration.to_query f))
         ; Aws.Util.option_map v.pending_modified_values (fun f ->
               Aws.Query.Pair ("PendingModifiedValues", PendingModifiedValues.to_query f))
         ; Aws.Util.option_map v.preferred_maintenance_window (fun f ->
               Aws.Query.Pair ("PreferredMaintenanceWindow", String.to_query f))
         ; Aws.Util.option_map v.cache_cluster_create_time (fun f ->
               Aws.Query.Pair ("CacheClusterCreateTime", DateTime.to_query f))
         ; Aws.Util.option_map v.preferred_outpost_arn (fun f ->
               Aws.Query.Pair ("PreferredOutpostArn", String.to_query f))
         ; Aws.Util.option_map v.preferred_availability_zone (fun f ->
               Aws.Query.Pair ("PreferredAvailabilityZone", String.to_query f))
         ; Aws.Util.option_map v.num_cache_nodes (fun f ->
               Aws.Query.Pair ("NumCacheNodes", Integer.to_query f))
         ; Aws.Util.option_map v.cache_cluster_status (fun f ->
               Aws.Query.Pair ("CacheClusterStatus", String.to_query f))
         ; Aws.Util.option_map v.engine_version (fun f ->
               Aws.Query.Pair ("EngineVersion", String.to_query f))
         ; Aws.Util.option_map v.engine (fun f ->
               Aws.Query.Pair ("Engine", String.to_query f))
         ; Aws.Util.option_map v.cache_node_type (fun f ->
               Aws.Query.Pair ("CacheNodeType", String.to_query f))
         ; Aws.Util.option_map v.client_download_landing_page (fun f ->
               Aws.Query.Pair ("ClientDownloadLandingPage", String.to_query f))
         ; Aws.Util.option_map v.configuration_endpoint (fun f ->
               Aws.Query.Pair ("ConfigurationEndpoint", Endpoint.to_query f))
         ; Aws.Util.option_map v.cache_cluster_id (fun f ->
               Aws.Query.Pair ("CacheClusterId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.a_r_n (fun f -> "ARN", String.to_json f)
         ; Aws.Util.option_map v.at_rest_encryption_enabled (fun f ->
               "AtRestEncryptionEnabled", Boolean.to_json f)
         ; Aws.Util.option_map v.transit_encryption_enabled (fun f ->
               "TransitEncryptionEnabled", Boolean.to_json f)
         ; Aws.Util.option_map v.auth_token_last_modified_date (fun f ->
               "AuthTokenLastModifiedDate", DateTime.to_json f)
         ; Aws.Util.option_map v.auth_token_enabled (fun f ->
               "AuthTokenEnabled", Boolean.to_json f)
         ; Aws.Util.option_map v.snapshot_window (fun f ->
               "SnapshotWindow", String.to_json f)
         ; Aws.Util.option_map v.snapshot_retention_limit (fun f ->
               "SnapshotRetentionLimit", Integer.to_json f)
         ; Aws.Util.option_map v.replication_group_id (fun f ->
               "ReplicationGroupId", String.to_json f)
         ; Some ("SecurityGroups", SecurityGroupMembershipList.to_json v.security_groups)
         ; Aws.Util.option_map v.auto_minor_version_upgrade (fun f ->
               "AutoMinorVersionUpgrade", Boolean.to_json f)
         ; Some ("CacheNodes", CacheNodeList.to_json v.cache_nodes)
         ; Aws.Util.option_map v.cache_subnet_group_name (fun f ->
               "CacheSubnetGroupName", String.to_json f)
         ; Aws.Util.option_map v.cache_parameter_group (fun f ->
               "CacheParameterGroup", CacheParameterGroupStatus.to_json f)
         ; Some
             ( "CacheSecurityGroups"
             , CacheSecurityGroupMembershipList.to_json v.cache_security_groups )
         ; Aws.Util.option_map v.notification_configuration (fun f ->
               "NotificationConfiguration", NotificationConfiguration.to_json f)
         ; Aws.Util.option_map v.pending_modified_values (fun f ->
               "PendingModifiedValues", PendingModifiedValues.to_json f)
         ; Aws.Util.option_map v.preferred_maintenance_window (fun f ->
               "PreferredMaintenanceWindow", String.to_json f)
         ; Aws.Util.option_map v.cache_cluster_create_time (fun f ->
               "CacheClusterCreateTime", DateTime.to_json f)
         ; Aws.Util.option_map v.preferred_outpost_arn (fun f ->
               "PreferredOutpostArn", String.to_json f)
         ; Aws.Util.option_map v.preferred_availability_zone (fun f ->
               "PreferredAvailabilityZone", String.to_json f)
         ; Aws.Util.option_map v.num_cache_nodes (fun f ->
               "NumCacheNodes", Integer.to_json f)
         ; Aws.Util.option_map v.cache_cluster_status (fun f ->
               "CacheClusterStatus", String.to_json f)
         ; Aws.Util.option_map v.engine_version (fun f ->
               "EngineVersion", String.to_json f)
         ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
         ; Aws.Util.option_map v.cache_node_type (fun f ->
               "CacheNodeType", String.to_json f)
         ; Aws.Util.option_map v.client_download_landing_page (fun f ->
               "ClientDownloadLandingPage", String.to_json f)
         ; Aws.Util.option_map v.configuration_endpoint (fun f ->
               "ConfigurationEndpoint", Endpoint.to_json f)
         ; Aws.Util.option_map v.cache_cluster_id (fun f ->
               "CacheClusterId", String.to_json f)
         ])

  let of_json j =
    { cache_cluster_id =
        Aws.Util.option_map (Aws.Json.lookup j "CacheClusterId") String.of_json
    ; configuration_endpoint =
        Aws.Util.option_map (Aws.Json.lookup j "ConfigurationEndpoint") Endpoint.of_json
    ; client_download_landing_page =
        Aws.Util.option_map (Aws.Json.lookup j "ClientDownloadLandingPage") String.of_json
    ; cache_node_type =
        Aws.Util.option_map (Aws.Json.lookup j "CacheNodeType") String.of_json
    ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
    ; engine_version =
        Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
    ; cache_cluster_status =
        Aws.Util.option_map (Aws.Json.lookup j "CacheClusterStatus") String.of_json
    ; num_cache_nodes =
        Aws.Util.option_map (Aws.Json.lookup j "NumCacheNodes") Integer.of_json
    ; preferred_availability_zone =
        Aws.Util.option_map (Aws.Json.lookup j "PreferredAvailabilityZone") String.of_json
    ; preferred_outpost_arn =
        Aws.Util.option_map (Aws.Json.lookup j "PreferredOutpostArn") String.of_json
    ; cache_cluster_create_time =
        Aws.Util.option_map (Aws.Json.lookup j "CacheClusterCreateTime") DateTime.of_json
    ; preferred_maintenance_window =
        Aws.Util.option_map
          (Aws.Json.lookup j "PreferredMaintenanceWindow")
          String.of_json
    ; pending_modified_values =
        Aws.Util.option_map
          (Aws.Json.lookup j "PendingModifiedValues")
          PendingModifiedValues.of_json
    ; notification_configuration =
        Aws.Util.option_map
          (Aws.Json.lookup j "NotificationConfiguration")
          NotificationConfiguration.of_json
    ; cache_security_groups =
        CacheSecurityGroupMembershipList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheSecurityGroups"))
    ; cache_parameter_group =
        Aws.Util.option_map
          (Aws.Json.lookup j "CacheParameterGroup")
          CacheParameterGroupStatus.of_json
    ; cache_subnet_group_name =
        Aws.Util.option_map (Aws.Json.lookup j "CacheSubnetGroupName") String.of_json
    ; cache_nodes =
        CacheNodeList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheNodes"))
    ; auto_minor_version_upgrade =
        Aws.Util.option_map (Aws.Json.lookup j "AutoMinorVersionUpgrade") Boolean.of_json
    ; security_groups =
        SecurityGroupMembershipList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroups"))
    ; replication_group_id =
        Aws.Util.option_map (Aws.Json.lookup j "ReplicationGroupId") String.of_json
    ; snapshot_retention_limit =
        Aws.Util.option_map (Aws.Json.lookup j "SnapshotRetentionLimit") Integer.of_json
    ; snapshot_window =
        Aws.Util.option_map (Aws.Json.lookup j "SnapshotWindow") String.of_json
    ; auth_token_enabled =
        Aws.Util.option_map (Aws.Json.lookup j "AuthTokenEnabled") Boolean.of_json
    ; auth_token_last_modified_date =
        Aws.Util.option_map
          (Aws.Json.lookup j "AuthTokenLastModifiedDate")
          DateTime.of_json
    ; transit_encryption_enabled =
        Aws.Util.option_map (Aws.Json.lookup j "TransitEncryptionEnabled") Boolean.of_json
    ; at_rest_encryption_enabled =
        Aws.Util.option_map (Aws.Json.lookup j "AtRestEncryptionEnabled") Boolean.of_json
    ; a_r_n = Aws.Util.option_map (Aws.Json.lookup j "ARN") String.of_json
    }
end

module CacheClusterList = struct
  type t = CacheCluster.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map CacheCluster.parse (Aws.Xml.members "CacheCluster" xml))

  let to_query v = Aws.Query.to_query_list CacheCluster.to_query v

  let to_json v = `List (List.map CacheCluster.to_json v)

  let of_json j = Aws.Json.to_list CacheCluster.of_json j
end

module NodeUpdateStatus = struct
  type t =
    | Not_applied
    | Waiting_to_start
    | In_progress
    | Stopping
    | Stopped
    | Complete

  let str_to_t =
    [ "complete", Complete
    ; "stopped", Stopped
    ; "stopping", Stopping
    ; "in-progress", In_progress
    ; "waiting-to-start", Waiting_to_start
    ; "not-applied", Not_applied
    ]

  let t_to_str =
    [ Complete, "complete"
    ; Stopped, "stopped"
    ; Stopping, "stopping"
    ; In_progress, "in-progress"
    ; Waiting_to_start, "waiting-to-start"
    ; Not_applied, "not-applied"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module NodeGroupMemberUpdateStatus = struct
  type t =
    { cache_cluster_id : String.t option
    ; cache_node_id : String.t option
    ; node_update_status : NodeUpdateStatus.t option
    ; node_deletion_date : DateTime.t option
    ; node_update_start_date : DateTime.t option
    ; node_update_end_date : DateTime.t option
    ; node_update_initiated_by : NodeUpdateInitiatedBy.t option
    ; node_update_initiated_date : DateTime.t option
    ; node_update_status_modified_date : DateTime.t option
    }

  let make
      ?cache_cluster_id
      ?cache_node_id
      ?node_update_status
      ?node_deletion_date
      ?node_update_start_date
      ?node_update_end_date
      ?node_update_initiated_by
      ?node_update_initiated_date
      ?node_update_status_modified_date
      () =
    { cache_cluster_id
    ; cache_node_id
    ; node_update_status
    ; node_deletion_date
    ; node_update_start_date
    ; node_update_end_date
    ; node_update_initiated_by
    ; node_update_initiated_date
    ; node_update_status_modified_date
    }

  let parse xml =
    Some
      { cache_cluster_id =
          Aws.Util.option_bind (Aws.Xml.member "CacheClusterId" xml) String.parse
      ; cache_node_id =
          Aws.Util.option_bind (Aws.Xml.member "CacheNodeId" xml) String.parse
      ; node_update_status =
          Aws.Util.option_bind
            (Aws.Xml.member "NodeUpdateStatus" xml)
            NodeUpdateStatus.parse
      ; node_deletion_date =
          Aws.Util.option_bind (Aws.Xml.member "NodeDeletionDate" xml) DateTime.parse
      ; node_update_start_date =
          Aws.Util.option_bind (Aws.Xml.member "NodeUpdateStartDate" xml) DateTime.parse
      ; node_update_end_date =
          Aws.Util.option_bind (Aws.Xml.member "NodeUpdateEndDate" xml) DateTime.parse
      ; node_update_initiated_by =
          Aws.Util.option_bind
            (Aws.Xml.member "NodeUpdateInitiatedBy" xml)
            NodeUpdateInitiatedBy.parse
      ; node_update_initiated_date =
          Aws.Util.option_bind
            (Aws.Xml.member "NodeUpdateInitiatedDate" xml)
            DateTime.parse
      ; node_update_status_modified_date =
          Aws.Util.option_bind
            (Aws.Xml.member "NodeUpdateStatusModifiedDate" xml)
            DateTime.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.node_update_status_modified_date (fun f ->
               Aws.Query.Pair ("NodeUpdateStatusModifiedDate", DateTime.to_query f))
         ; Aws.Util.option_map v.node_update_initiated_date (fun f ->
               Aws.Query.Pair ("NodeUpdateInitiatedDate", DateTime.to_query f))
         ; Aws.Util.option_map v.node_update_initiated_by (fun f ->
               Aws.Query.Pair ("NodeUpdateInitiatedBy", NodeUpdateInitiatedBy.to_query f))
         ; Aws.Util.option_map v.node_update_end_date (fun f ->
               Aws.Query.Pair ("NodeUpdateEndDate", DateTime.to_query f))
         ; Aws.Util.option_map v.node_update_start_date (fun f ->
               Aws.Query.Pair ("NodeUpdateStartDate", DateTime.to_query f))
         ; Aws.Util.option_map v.node_deletion_date (fun f ->
               Aws.Query.Pair ("NodeDeletionDate", DateTime.to_query f))
         ; Aws.Util.option_map v.node_update_status (fun f ->
               Aws.Query.Pair ("NodeUpdateStatus", NodeUpdateStatus.to_query f))
         ; Aws.Util.option_map v.cache_node_id (fun f ->
               Aws.Query.Pair ("CacheNodeId", String.to_query f))
         ; Aws.Util.option_map v.cache_cluster_id (fun f ->
               Aws.Query.Pair ("CacheClusterId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.node_update_status_modified_date (fun f ->
               "NodeUpdateStatusModifiedDate", DateTime.to_json f)
         ; Aws.Util.option_map v.node_update_initiated_date (fun f ->
               "NodeUpdateInitiatedDate", DateTime.to_json f)
         ; Aws.Util.option_map v.node_update_initiated_by (fun f ->
               "NodeUpdateInitiatedBy", NodeUpdateInitiatedBy.to_json f)
         ; Aws.Util.option_map v.node_update_end_date (fun f ->
               "NodeUpdateEndDate", DateTime.to_json f)
         ; Aws.Util.option_map v.node_update_start_date (fun f ->
               "NodeUpdateStartDate", DateTime.to_json f)
         ; Aws.Util.option_map v.node_deletion_date (fun f ->
               "NodeDeletionDate", DateTime.to_json f)
         ; Aws.Util.option_map v.node_update_status (fun f ->
               "NodeUpdateStatus", NodeUpdateStatus.to_json f)
         ; Aws.Util.option_map v.cache_node_id (fun f -> "CacheNodeId", String.to_json f)
         ; Aws.Util.option_map v.cache_cluster_id (fun f ->
               "CacheClusterId", String.to_json f)
         ])

  let of_json j =
    { cache_cluster_id =
        Aws.Util.option_map (Aws.Json.lookup j "CacheClusterId") String.of_json
    ; cache_node_id = Aws.Util.option_map (Aws.Json.lookup j "CacheNodeId") String.of_json
    ; node_update_status =
        Aws.Util.option_map
          (Aws.Json.lookup j "NodeUpdateStatus")
          NodeUpdateStatus.of_json
    ; node_deletion_date =
        Aws.Util.option_map (Aws.Json.lookup j "NodeDeletionDate") DateTime.of_json
    ; node_update_start_date =
        Aws.Util.option_map (Aws.Json.lookup j "NodeUpdateStartDate") DateTime.of_json
    ; node_update_end_date =
        Aws.Util.option_map (Aws.Json.lookup j "NodeUpdateEndDate") DateTime.of_json
    ; node_update_initiated_by =
        Aws.Util.option_map
          (Aws.Json.lookup j "NodeUpdateInitiatedBy")
          NodeUpdateInitiatedBy.of_json
    ; node_update_initiated_date =
        Aws.Util.option_map (Aws.Json.lookup j "NodeUpdateInitiatedDate") DateTime.of_json
    ; node_update_status_modified_date =
        Aws.Util.option_map
          (Aws.Json.lookup j "NodeUpdateStatusModifiedDate")
          DateTime.of_json
    }
end

module NodeGroupMemberUpdateStatusList = struct
  type t = NodeGroupMemberUpdateStatus.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map
         NodeGroupMemberUpdateStatus.parse
         (Aws.Xml.members "NodeGroupMemberUpdateStatus" xml))

  let to_query v = Aws.Query.to_query_list NodeGroupMemberUpdateStatus.to_query v

  let to_json v = `List (List.map NodeGroupMemberUpdateStatus.to_json v)

  let of_json j = Aws.Json.to_list NodeGroupMemberUpdateStatus.of_json j
end

module UserIdListInput = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module ModifyUserGroupMessage = struct
  type t =
    { user_group_id : String.t
    ; user_ids_to_add : UserIdListInput.t
    ; user_ids_to_remove : UserIdListInput.t
    }

  let make ~user_group_id ?(user_ids_to_add = []) ?(user_ids_to_remove = []) () =
    { user_group_id; user_ids_to_add; user_ids_to_remove }

  let parse xml =
    Some
      { user_group_id =
          Aws.Xml.required
            "UserGroupId"
            (Aws.Util.option_bind (Aws.Xml.member "UserGroupId" xml) String.parse)
      ; user_ids_to_add =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "UserIdsToAdd" xml)
               UserIdListInput.parse)
      ; user_ids_to_remove =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "UserIdsToRemove" xml)
               UserIdListInput.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("UserIdsToRemove.member", UserIdListInput.to_query v.user_ids_to_remove))
         ; Some
             (Aws.Query.Pair
                ("UserIdsToAdd.member", UserIdListInput.to_query v.user_ids_to_add))
         ; Some (Aws.Query.Pair ("UserGroupId", String.to_query v.user_group_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("UserIdsToRemove", UserIdListInput.to_json v.user_ids_to_remove)
         ; Some ("UserIdsToAdd", UserIdListInput.to_json v.user_ids_to_add)
         ; Some ("UserGroupId", String.to_json v.user_group_id)
         ])

  let of_json j =
    { user_group_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UserGroupId"))
    ; user_ids_to_add =
        UserIdListInput.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "UserIdsToAdd"))
    ; user_ids_to_remove =
        UserIdListInput.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "UserIdsToRemove"))
    }
end

module ListTagsForResourceMessage = struct
  type t = { resource_name : String.t }

  let make ~resource_name () = { resource_name }

  let parse xml =
    Some
      { resource_name =
          Aws.Xml.required
            "ResourceName"
            (Aws.Util.option_bind (Aws.Xml.member "ResourceName" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("ResourceName", String.to_query v.resource_name)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt [ Some ("ResourceName", String.to_json v.resource_name) ])

  let of_json j =
    { resource_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceName"))
    }
end

module InvalidSnapshotStateFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module PurchaseReservedCacheNodesOfferingResult = struct
  type t = { reserved_cache_node : ReservedCacheNode.t option }

  let make ?reserved_cache_node () = { reserved_cache_node }

  let parse xml =
    Some
      { reserved_cache_node =
          Aws.Util.option_bind
            (Aws.Xml.member "ReservedCacheNode" xml)
            ReservedCacheNode.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.reserved_cache_node (fun f ->
               Aws.Query.Pair ("ReservedCacheNode", ReservedCacheNode.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.reserved_cache_node (fun f ->
               "ReservedCacheNode", ReservedCacheNode.to_json f)
         ])

  let of_json j =
    { reserved_cache_node =
        Aws.Util.option_map
          (Aws.Json.lookup j "ReservedCacheNode")
          ReservedCacheNode.of_json
    }
end

module CacheEngineVersion = struct
  type t =
    { engine : String.t option
    ; engine_version : String.t option
    ; cache_parameter_group_family : String.t option
    ; cache_engine_description : String.t option
    ; cache_engine_version_description : String.t option
    }

  let make
      ?engine
      ?engine_version
      ?cache_parameter_group_family
      ?cache_engine_description
      ?cache_engine_version_description
      () =
    { engine
    ; engine_version
    ; cache_parameter_group_family
    ; cache_engine_description
    ; cache_engine_version_description
    }

  let parse xml =
    Some
      { engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
      ; engine_version =
          Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
      ; cache_parameter_group_family =
          Aws.Util.option_bind
            (Aws.Xml.member "CacheParameterGroupFamily" xml)
            String.parse
      ; cache_engine_description =
          Aws.Util.option_bind (Aws.Xml.member "CacheEngineDescription" xml) String.parse
      ; cache_engine_version_description =
          Aws.Util.option_bind
            (Aws.Xml.member "CacheEngineVersionDescription" xml)
            String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cache_engine_version_description (fun f ->
               Aws.Query.Pair ("CacheEngineVersionDescription", String.to_query f))
         ; Aws.Util.option_map v.cache_engine_description (fun f ->
               Aws.Query.Pair ("CacheEngineDescription", String.to_query f))
         ; Aws.Util.option_map v.cache_parameter_group_family (fun f ->
               Aws.Query.Pair ("CacheParameterGroupFamily", String.to_query f))
         ; Aws.Util.option_map v.engine_version (fun f ->
               Aws.Query.Pair ("EngineVersion", String.to_query f))
         ; Aws.Util.option_map v.engine (fun f ->
               Aws.Query.Pair ("Engine", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cache_engine_version_description (fun f ->
               "CacheEngineVersionDescription", String.to_json f)
         ; Aws.Util.option_map v.cache_engine_description (fun f ->
               "CacheEngineDescription", String.to_json f)
         ; Aws.Util.option_map v.cache_parameter_group_family (fun f ->
               "CacheParameterGroupFamily", String.to_json f)
         ; Aws.Util.option_map v.engine_version (fun f ->
               "EngineVersion", String.to_json f)
         ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
         ])

  let of_json j =
    { engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
    ; engine_version =
        Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
    ; cache_parameter_group_family =
        Aws.Util.option_map (Aws.Json.lookup j "CacheParameterGroupFamily") String.of_json
    ; cache_engine_description =
        Aws.Util.option_map (Aws.Json.lookup j "CacheEngineDescription") String.of_json
    ; cache_engine_version_description =
        Aws.Util.option_map
          (Aws.Json.lookup j "CacheEngineVersionDescription")
          String.of_json
    }
end

module CacheEngineVersionList = struct
  type t = CacheEngineVersion.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map CacheEngineVersion.parse (Aws.Xml.members "CacheEngineVersion" xml))

  let to_query v = Aws.Query.to_query_list CacheEngineVersion.to_query v

  let to_json v = `List (List.map CacheEngineVersion.to_json v)

  let of_json j = Aws.Json.to_list CacheEngineVersion.of_json j
end

module CacheEngineVersionMessage = struct
  type t =
    { marker : String.t option
    ; cache_engine_versions : CacheEngineVersionList.t
    }

  let make ?marker ?(cache_engine_versions = []) () = { marker; cache_engine_versions }

  let parse xml =
    Some
      { marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      ; cache_engine_versions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "CacheEngineVersions" xml)
               CacheEngineVersionList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "CacheEngineVersions.member"
                , CacheEngineVersionList.to_query v.cache_engine_versions ))
         ; Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "CacheEngineVersions"
             , CacheEngineVersionList.to_json v.cache_engine_versions )
         ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ])

  let of_json j =
    { marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    ; cache_engine_versions =
        CacheEngineVersionList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheEngineVersions"))
    }
end

module CacheClusterIdList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module NodeGroupUpdateStatus = struct
  type t =
    { node_group_id : String.t option
    ; node_group_member_update_status : NodeGroupMemberUpdateStatusList.t
    }

  let make ?node_group_id ?(node_group_member_update_status = []) () =
    { node_group_id; node_group_member_update_status }

  let parse xml =
    Some
      { node_group_id =
          Aws.Util.option_bind (Aws.Xml.member "NodeGroupId" xml) String.parse
      ; node_group_member_update_status =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "NodeGroupMemberUpdateStatus" xml)
               NodeGroupMemberUpdateStatusList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "NodeGroupMemberUpdateStatus.member"
                , NodeGroupMemberUpdateStatusList.to_query
                    v.node_group_member_update_status ))
         ; Aws.Util.option_map v.node_group_id (fun f ->
               Aws.Query.Pair ("NodeGroupId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "NodeGroupMemberUpdateStatus"
             , NodeGroupMemberUpdateStatusList.to_json v.node_group_member_update_status
             )
         ; Aws.Util.option_map v.node_group_id (fun f -> "NodeGroupId", String.to_json f)
         ])

  let of_json j =
    { node_group_id = Aws.Util.option_map (Aws.Json.lookup j "NodeGroupId") String.of_json
    ; node_group_member_update_status =
        NodeGroupMemberUpdateStatusList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "NodeGroupMemberUpdateStatus"))
    }
end

module NodeGroupUpdateStatusList = struct
  type t = NodeGroupUpdateStatus.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map NodeGroupUpdateStatus.parse (Aws.Xml.members "NodeGroupUpdateStatus" xml))

  let to_query v = Aws.Query.to_query_list NodeGroupUpdateStatus.to_query v

  let to_json v = `List (List.map NodeGroupUpdateStatus.to_json v)

  let of_json j = Aws.Json.to_list NodeGroupUpdateStatus.of_json j
end

module CreateCacheParameterGroupMessage = struct
  type t =
    { cache_parameter_group_name : String.t
    ; cache_parameter_group_family : String.t
    ; description : String.t
    }

  let make ~cache_parameter_group_name ~cache_parameter_group_family ~description () =
    { cache_parameter_group_name; cache_parameter_group_family; description }

  let parse xml =
    Some
      { cache_parameter_group_name =
          Aws.Xml.required
            "CacheParameterGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "CacheParameterGroupName" xml)
               String.parse)
      ; cache_parameter_group_family =
          Aws.Xml.required
            "CacheParameterGroupFamily"
            (Aws.Util.option_bind
               (Aws.Xml.member "CacheParameterGroupFamily" xml)
               String.parse)
      ; description =
          Aws.Xml.required
            "Description"
            (Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Description", String.to_query v.description))
         ; Some
             (Aws.Query.Pair
                ( "CacheParameterGroupFamily"
                , String.to_query v.cache_parameter_group_family ))
         ; Some
             (Aws.Query.Pair
                ("CacheParameterGroupName", String.to_query v.cache_parameter_group_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Description", String.to_json v.description)
         ; Some
             ("CacheParameterGroupFamily", String.to_json v.cache_parameter_group_family)
         ; Some ("CacheParameterGroupName", String.to_json v.cache_parameter_group_name)
         ])

  let of_json j =
    { cache_parameter_group_name =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheParameterGroupName"))
    ; cache_parameter_group_family =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheParameterGroupFamily"))
    ; description =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Description"))
    }
end

module SlaMet = struct
  type t =
    | Yes
    | No
    | N_a

  let str_to_t = [ "n/a", N_a; "no", No; "yes", Yes ]

  let t_to_str = [ N_a, "n/a"; No, "no"; Yes, "yes" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module CacheClusterAlreadyExistsFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module CacheSubnetGroupQuotaExceededFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ChangeType = struct
  type t =
    | Immediate
    | Requires_reboot

  let str_to_t = [ "requires-reboot", Requires_reboot; "immediate", Immediate ]

  let t_to_str = [ Requires_reboot, "requires-reboot"; Immediate, "immediate" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module CacheNodeTypeSpecificValue = struct
  type t =
    { cache_node_type : String.t option
    ; value : String.t option
    }

  let make ?cache_node_type ?value () = { cache_node_type; value }

  let parse xml =
    Some
      { cache_node_type =
          Aws.Util.option_bind (Aws.Xml.member "CacheNodeType" xml) String.parse
      ; value = Aws.Util.option_bind (Aws.Xml.member "Value" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.value (fun f ->
               Aws.Query.Pair ("Value", String.to_query f))
         ; Aws.Util.option_map v.cache_node_type (fun f ->
               Aws.Query.Pair ("CacheNodeType", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.value (fun f -> "Value", String.to_json f)
         ; Aws.Util.option_map v.cache_node_type (fun f ->
               "CacheNodeType", String.to_json f)
         ])

  let of_json j =
    { cache_node_type =
        Aws.Util.option_map (Aws.Json.lookup j "CacheNodeType") String.of_json
    ; value = Aws.Util.option_map (Aws.Json.lookup j "Value") String.of_json
    }
end

module CacheNodeTypeSpecificValueList = struct
  type t = CacheNodeTypeSpecificValue.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map
         CacheNodeTypeSpecificValue.parse
         (Aws.Xml.members "CacheNodeTypeSpecificValue" xml))

  let to_query v = Aws.Query.to_query_list CacheNodeTypeSpecificValue.to_query v

  let to_json v = `List (List.map CacheNodeTypeSpecificValue.to_json v)

  let of_json j = Aws.Json.to_list CacheNodeTypeSpecificValue.of_json j
end

module CacheNodeTypeSpecificParameter = struct
  type t =
    { parameter_name : String.t option
    ; description : String.t option
    ; source : String.t option
    ; data_type : String.t option
    ; allowed_values : String.t option
    ; is_modifiable : Boolean.t option
    ; minimum_engine_version : String.t option
    ; cache_node_type_specific_values : CacheNodeTypeSpecificValueList.t
    ; change_type : ChangeType.t option
    }

  let make
      ?parameter_name
      ?description
      ?source
      ?data_type
      ?allowed_values
      ?is_modifiable
      ?minimum_engine_version
      ?(cache_node_type_specific_values = [])
      ?change_type
      () =
    { parameter_name
    ; description
    ; source
    ; data_type
    ; allowed_values
    ; is_modifiable
    ; minimum_engine_version
    ; cache_node_type_specific_values
    ; change_type
    }

  let parse xml =
    Some
      { parameter_name =
          Aws.Util.option_bind (Aws.Xml.member "ParameterName" xml) String.parse
      ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
      ; source = Aws.Util.option_bind (Aws.Xml.member "Source" xml) String.parse
      ; data_type = Aws.Util.option_bind (Aws.Xml.member "DataType" xml) String.parse
      ; allowed_values =
          Aws.Util.option_bind (Aws.Xml.member "AllowedValues" xml) String.parse
      ; is_modifiable =
          Aws.Util.option_bind (Aws.Xml.member "IsModifiable" xml) Boolean.parse
      ; minimum_engine_version =
          Aws.Util.option_bind (Aws.Xml.member "MinimumEngineVersion" xml) String.parse
      ; cache_node_type_specific_values =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "CacheNodeTypeSpecificValues" xml)
               CacheNodeTypeSpecificValueList.parse)
      ; change_type =
          Aws.Util.option_bind (Aws.Xml.member "ChangeType" xml) ChangeType.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.change_type (fun f ->
               Aws.Query.Pair ("ChangeType", ChangeType.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "CacheNodeTypeSpecificValues.member"
                , CacheNodeTypeSpecificValueList.to_query
                    v.cache_node_type_specific_values ))
         ; Aws.Util.option_map v.minimum_engine_version (fun f ->
               Aws.Query.Pair ("MinimumEngineVersion", String.to_query f))
         ; Aws.Util.option_map v.is_modifiable (fun f ->
               Aws.Query.Pair ("IsModifiable", Boolean.to_query f))
         ; Aws.Util.option_map v.allowed_values (fun f ->
               Aws.Query.Pair ("AllowedValues", String.to_query f))
         ; Aws.Util.option_map v.data_type (fun f ->
               Aws.Query.Pair ("DataType", String.to_query f))
         ; Aws.Util.option_map v.source (fun f ->
               Aws.Query.Pair ("Source", String.to_query f))
         ; Aws.Util.option_map v.description (fun f ->
               Aws.Query.Pair ("Description", String.to_query f))
         ; Aws.Util.option_map v.parameter_name (fun f ->
               Aws.Query.Pair ("ParameterName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.change_type (fun f -> "ChangeType", ChangeType.to_json f)
         ; Some
             ( "CacheNodeTypeSpecificValues"
             , CacheNodeTypeSpecificValueList.to_json v.cache_node_type_specific_values )
         ; Aws.Util.option_map v.minimum_engine_version (fun f ->
               "MinimumEngineVersion", String.to_json f)
         ; Aws.Util.option_map v.is_modifiable (fun f ->
               "IsModifiable", Boolean.to_json f)
         ; Aws.Util.option_map v.allowed_values (fun f ->
               "AllowedValues", String.to_json f)
         ; Aws.Util.option_map v.data_type (fun f -> "DataType", String.to_json f)
         ; Aws.Util.option_map v.source (fun f -> "Source", String.to_json f)
         ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
         ; Aws.Util.option_map v.parameter_name (fun f ->
               "ParameterName", String.to_json f)
         ])

  let of_json j =
    { parameter_name =
        Aws.Util.option_map (Aws.Json.lookup j "ParameterName") String.of_json
    ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
    ; source = Aws.Util.option_map (Aws.Json.lookup j "Source") String.of_json
    ; data_type = Aws.Util.option_map (Aws.Json.lookup j "DataType") String.of_json
    ; allowed_values =
        Aws.Util.option_map (Aws.Json.lookup j "AllowedValues") String.of_json
    ; is_modifiable =
        Aws.Util.option_map (Aws.Json.lookup j "IsModifiable") Boolean.of_json
    ; minimum_engine_version =
        Aws.Util.option_map (Aws.Json.lookup j "MinimumEngineVersion") String.of_json
    ; cache_node_type_specific_values =
        CacheNodeTypeSpecificValueList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheNodeTypeSpecificValues"))
    ; change_type =
        Aws.Util.option_map (Aws.Json.lookup j "ChangeType") ChangeType.of_json
    }
end

module CacheNodeTypeSpecificParametersList = struct
  type t = CacheNodeTypeSpecificParameter.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map
         CacheNodeTypeSpecificParameter.parse
         (Aws.Xml.members "CacheNodeTypeSpecificParameter" xml))

  let to_query v = Aws.Query.to_query_list CacheNodeTypeSpecificParameter.to_query v

  let to_json v = `List (List.map CacheNodeTypeSpecificParameter.to_json v)

  let of_json j = Aws.Json.to_list CacheNodeTypeSpecificParameter.of_json j
end

module NodeTypeList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module DescribeCacheParametersMessage = struct
  type t =
    { cache_parameter_group_name : String.t
    ; source : String.t option
    ; max_records : Integer.t option
    ; marker : String.t option
    }

  let make ~cache_parameter_group_name ?source ?max_records ?marker () =
    { cache_parameter_group_name; source; max_records; marker }

  let parse xml =
    Some
      { cache_parameter_group_name =
          Aws.Xml.required
            "CacheParameterGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "CacheParameterGroupName" xml)
               String.parse)
      ; source = Aws.Util.option_bind (Aws.Xml.member "Source" xml) String.parse
      ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ; Aws.Util.option_map v.max_records (fun f ->
               Aws.Query.Pair ("MaxRecords", Integer.to_query f))
         ; Aws.Util.option_map v.source (fun f ->
               Aws.Query.Pair ("Source", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("CacheParameterGroupName", String.to_query v.cache_parameter_group_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
         ; Aws.Util.option_map v.source (fun f -> "Source", String.to_json f)
         ; Some ("CacheParameterGroupName", String.to_json v.cache_parameter_group_name)
         ])

  let of_json j =
    { cache_parameter_group_name =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheParameterGroupName"))
    ; source = Aws.Util.option_map (Aws.Json.lookup j "Source") String.of_json
    ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
    ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    }
end

module ReplicationGroupList = struct
  type t = ReplicationGroup.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map ReplicationGroup.parse (Aws.Xml.members "ReplicationGroup" xml))

  let to_query v = Aws.Query.to_query_list ReplicationGroup.to_query v

  let to_json v = `List (List.map ReplicationGroup.to_json v)

  let of_json j = Aws.Json.to_list ReplicationGroup.of_json j
end

module ReplicationGroupMessage = struct
  type t =
    { marker : String.t option
    ; replication_groups : ReplicationGroupList.t
    }

  let make ?marker ?(replication_groups = []) () = { marker; replication_groups }

  let parse xml =
    Some
      { marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      ; replication_groups =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ReplicationGroups" xml)
               ReplicationGroupList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "ReplicationGroups.member"
                , ReplicationGroupList.to_query v.replication_groups ))
         ; Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("ReplicationGroups", ReplicationGroupList.to_json v.replication_groups)
         ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ])

  let of_json j =
    { marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    ; replication_groups =
        ReplicationGroupList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicationGroups"))
    }
end

module KeyList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module RemoveTagsFromResourceMessage = struct
  type t =
    { resource_name : String.t
    ; tag_keys : KeyList.t
    }

  let make ~resource_name ~tag_keys () = { resource_name; tag_keys }

  let parse xml =
    Some
      { resource_name =
          Aws.Xml.required
            "ResourceName"
            (Aws.Util.option_bind (Aws.Xml.member "ResourceName" xml) String.parse)
      ; tag_keys =
          Aws.Xml.required
            "TagKeys"
            (Aws.Util.option_bind (Aws.Xml.member "TagKeys" xml) KeyList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("TagKeys.member", KeyList.to_query v.tag_keys))
         ; Some (Aws.Query.Pair ("ResourceName", String.to_query v.resource_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("TagKeys", KeyList.to_json v.tag_keys)
         ; Some ("ResourceName", String.to_json v.resource_name)
         ])

  let of_json j =
    { resource_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceName"))
    ; tag_keys = KeyList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TagKeys"))
    }
end

module UnprocessedUpdateAction = struct
  type t =
    { replication_group_id : String.t option
    ; cache_cluster_id : String.t option
    ; service_update_name : String.t option
    ; error_type : String.t option
    ; error_message : String.t option
    }

  let make
      ?replication_group_id
      ?cache_cluster_id
      ?service_update_name
      ?error_type
      ?error_message
      () =
    { replication_group_id
    ; cache_cluster_id
    ; service_update_name
    ; error_type
    ; error_message
    }

  let parse xml =
    Some
      { replication_group_id =
          Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse
      ; cache_cluster_id =
          Aws.Util.option_bind (Aws.Xml.member "CacheClusterId" xml) String.parse
      ; service_update_name =
          Aws.Util.option_bind (Aws.Xml.member "ServiceUpdateName" xml) String.parse
      ; error_type = Aws.Util.option_bind (Aws.Xml.member "ErrorType" xml) String.parse
      ; error_message =
          Aws.Util.option_bind (Aws.Xml.member "ErrorMessage" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.error_message (fun f ->
               Aws.Query.Pair ("ErrorMessage", String.to_query f))
         ; Aws.Util.option_map v.error_type (fun f ->
               Aws.Query.Pair ("ErrorType", String.to_query f))
         ; Aws.Util.option_map v.service_update_name (fun f ->
               Aws.Query.Pair ("ServiceUpdateName", String.to_query f))
         ; Aws.Util.option_map v.cache_cluster_id (fun f ->
               Aws.Query.Pair ("CacheClusterId", String.to_query f))
         ; Aws.Util.option_map v.replication_group_id (fun f ->
               Aws.Query.Pair ("ReplicationGroupId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.error_message (fun f -> "ErrorMessage", String.to_json f)
         ; Aws.Util.option_map v.error_type (fun f -> "ErrorType", String.to_json f)
         ; Aws.Util.option_map v.service_update_name (fun f ->
               "ServiceUpdateName", String.to_json f)
         ; Aws.Util.option_map v.cache_cluster_id (fun f ->
               "CacheClusterId", String.to_json f)
         ; Aws.Util.option_map v.replication_group_id (fun f ->
               "ReplicationGroupId", String.to_json f)
         ])

  let of_json j =
    { replication_group_id =
        Aws.Util.option_map (Aws.Json.lookup j "ReplicationGroupId") String.of_json
    ; cache_cluster_id =
        Aws.Util.option_map (Aws.Json.lookup j "CacheClusterId") String.of_json
    ; service_update_name =
        Aws.Util.option_map (Aws.Json.lookup j "ServiceUpdateName") String.of_json
    ; error_type = Aws.Util.option_map (Aws.Json.lookup j "ErrorType") String.of_json
    ; error_message =
        Aws.Util.option_map (Aws.Json.lookup j "ErrorMessage") String.of_json
    }
end

module UnprocessedUpdateActionList = struct
  type t = UnprocessedUpdateAction.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map
         UnprocessedUpdateAction.parse
         (Aws.Xml.members "UnprocessedUpdateAction" xml))

  let to_query v = Aws.Query.to_query_list UnprocessedUpdateAction.to_query v

  let to_json v = `List (List.map UnprocessedUpdateAction.to_json v)

  let of_json j = Aws.Json.to_list UnprocessedUpdateAction.of_json j
end

module UpdateActionStatus = struct
  type t =
    | Not_applied
    | Waiting_to_start
    | In_progress
    | Stopping
    | Stopped
    | Complete
    | Scheduling
    | Scheduled
    | Not_applicable

  let str_to_t =
    [ "not-applicable", Not_applicable
    ; "scheduled", Scheduled
    ; "scheduling", Scheduling
    ; "complete", Complete
    ; "stopped", Stopped
    ; "stopping", Stopping
    ; "in-progress", In_progress
    ; "waiting-to-start", Waiting_to_start
    ; "not-applied", Not_applied
    ]

  let t_to_str =
    [ Not_applicable, "not-applicable"
    ; Scheduled, "scheduled"
    ; Scheduling, "scheduling"
    ; Complete, "complete"
    ; Stopped, "stopped"
    ; Stopping, "stopping"
    ; In_progress, "in-progress"
    ; Waiting_to_start, "waiting-to-start"
    ; Not_applied, "not-applied"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module ProcessedUpdateAction = struct
  type t =
    { replication_group_id : String.t option
    ; cache_cluster_id : String.t option
    ; service_update_name : String.t option
    ; update_action_status : UpdateActionStatus.t option
    }

  let make
      ?replication_group_id
      ?cache_cluster_id
      ?service_update_name
      ?update_action_status
      () =
    { replication_group_id; cache_cluster_id; service_update_name; update_action_status }

  let parse xml =
    Some
      { replication_group_id =
          Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse
      ; cache_cluster_id =
          Aws.Util.option_bind (Aws.Xml.member "CacheClusterId" xml) String.parse
      ; service_update_name =
          Aws.Util.option_bind (Aws.Xml.member "ServiceUpdateName" xml) String.parse
      ; update_action_status =
          Aws.Util.option_bind
            (Aws.Xml.member "UpdateActionStatus" xml)
            UpdateActionStatus.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.update_action_status (fun f ->
               Aws.Query.Pair ("UpdateActionStatus", UpdateActionStatus.to_query f))
         ; Aws.Util.option_map v.service_update_name (fun f ->
               Aws.Query.Pair ("ServiceUpdateName", String.to_query f))
         ; Aws.Util.option_map v.cache_cluster_id (fun f ->
               Aws.Query.Pair ("CacheClusterId", String.to_query f))
         ; Aws.Util.option_map v.replication_group_id (fun f ->
               Aws.Query.Pair ("ReplicationGroupId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.update_action_status (fun f ->
               "UpdateActionStatus", UpdateActionStatus.to_json f)
         ; Aws.Util.option_map v.service_update_name (fun f ->
               "ServiceUpdateName", String.to_json f)
         ; Aws.Util.option_map v.cache_cluster_id (fun f ->
               "CacheClusterId", String.to_json f)
         ; Aws.Util.option_map v.replication_group_id (fun f ->
               "ReplicationGroupId", String.to_json f)
         ])

  let of_json j =
    { replication_group_id =
        Aws.Util.option_map (Aws.Json.lookup j "ReplicationGroupId") String.of_json
    ; cache_cluster_id =
        Aws.Util.option_map (Aws.Json.lookup j "CacheClusterId") String.of_json
    ; service_update_name =
        Aws.Util.option_map (Aws.Json.lookup j "ServiceUpdateName") String.of_json
    ; update_action_status =
        Aws.Util.option_map
          (Aws.Json.lookup j "UpdateActionStatus")
          UpdateActionStatus.of_json
    }
end

module ProcessedUpdateActionList = struct
  type t = ProcessedUpdateAction.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map ProcessedUpdateAction.parse (Aws.Xml.members "ProcessedUpdateAction" xml))

  let to_query v = Aws.Query.to_query_list ProcessedUpdateAction.to_query v

  let to_json v = `List (List.map ProcessedUpdateAction.to_json v)

  let of_json j = Aws.Json.to_list ProcessedUpdateAction.of_json j
end

module UpdateActionResultsMessage = struct
  type t =
    { processed_update_actions : ProcessedUpdateActionList.t
    ; unprocessed_update_actions : UnprocessedUpdateActionList.t
    }

  let make ?(processed_update_actions = []) ?(unprocessed_update_actions = []) () =
    { processed_update_actions; unprocessed_update_actions }

  let parse xml =
    Some
      { processed_update_actions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ProcessedUpdateActions" xml)
               ProcessedUpdateActionList.parse)
      ; unprocessed_update_actions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "UnprocessedUpdateActions" xml)
               UnprocessedUpdateActionList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "UnprocessedUpdateActions.member"
                , UnprocessedUpdateActionList.to_query v.unprocessed_update_actions ))
         ; Some
             (Aws.Query.Pair
                ( "ProcessedUpdateActions.member"
                , ProcessedUpdateActionList.to_query v.processed_update_actions ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "UnprocessedUpdateActions"
             , UnprocessedUpdateActionList.to_json v.unprocessed_update_actions )
         ; Some
             ( "ProcessedUpdateActions"
             , ProcessedUpdateActionList.to_json v.processed_update_actions )
         ])

  let of_json j =
    { processed_update_actions =
        ProcessedUpdateActionList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ProcessedUpdateActions"))
    ; unprocessed_update_actions =
        UnprocessedUpdateActionList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "UnprocessedUpdateActions"))
    }
end

module OutpostArnsList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "OutpostArn" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module AvailabilityZonesList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "AvailabilityZone" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module NodeGroupConfiguration = struct
  type t =
    { node_group_id : String.t option
    ; slots : String.t option
    ; replica_count : Integer.t option
    ; primary_availability_zone : String.t option
    ; replica_availability_zones : AvailabilityZonesList.t
    ; primary_outpost_arn : String.t option
    ; replica_outpost_arns : OutpostArnsList.t
    }

  let make
      ?node_group_id
      ?slots
      ?replica_count
      ?primary_availability_zone
      ?(replica_availability_zones = [])
      ?primary_outpost_arn
      ?(replica_outpost_arns = [])
      () =
    { node_group_id
    ; slots
    ; replica_count
    ; primary_availability_zone
    ; replica_availability_zones
    ; primary_outpost_arn
    ; replica_outpost_arns
    }

  let parse xml =
    Some
      { node_group_id =
          Aws.Util.option_bind (Aws.Xml.member "NodeGroupId" xml) String.parse
      ; slots = Aws.Util.option_bind (Aws.Xml.member "Slots" xml) String.parse
      ; replica_count =
          Aws.Util.option_bind (Aws.Xml.member "ReplicaCount" xml) Integer.parse
      ; primary_availability_zone =
          Aws.Util.option_bind (Aws.Xml.member "PrimaryAvailabilityZone" xml) String.parse
      ; replica_availability_zones =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ReplicaAvailabilityZones" xml)
               AvailabilityZonesList.parse)
      ; primary_outpost_arn =
          Aws.Util.option_bind (Aws.Xml.member "PrimaryOutpostArn" xml) String.parse
      ; replica_outpost_arns =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ReplicaOutpostArns" xml)
               OutpostArnsList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "ReplicaOutpostArns.member"
                , OutpostArnsList.to_query v.replica_outpost_arns ))
         ; Aws.Util.option_map v.primary_outpost_arn (fun f ->
               Aws.Query.Pair ("PrimaryOutpostArn", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "ReplicaAvailabilityZones.member"
                , AvailabilityZonesList.to_query v.replica_availability_zones ))
         ; Aws.Util.option_map v.primary_availability_zone (fun f ->
               Aws.Query.Pair ("PrimaryAvailabilityZone", String.to_query f))
         ; Aws.Util.option_map v.replica_count (fun f ->
               Aws.Query.Pair ("ReplicaCount", Integer.to_query f))
         ; Aws.Util.option_map v.slots (fun f ->
               Aws.Query.Pair ("Slots", String.to_query f))
         ; Aws.Util.option_map v.node_group_id (fun f ->
               Aws.Query.Pair ("NodeGroupId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("ReplicaOutpostArns", OutpostArnsList.to_json v.replica_outpost_arns)
         ; Aws.Util.option_map v.primary_outpost_arn (fun f ->
               "PrimaryOutpostArn", String.to_json f)
         ; Some
             ( "ReplicaAvailabilityZones"
             , AvailabilityZonesList.to_json v.replica_availability_zones )
         ; Aws.Util.option_map v.primary_availability_zone (fun f ->
               "PrimaryAvailabilityZone", String.to_json f)
         ; Aws.Util.option_map v.replica_count (fun f ->
               "ReplicaCount", Integer.to_json f)
         ; Aws.Util.option_map v.slots (fun f -> "Slots", String.to_json f)
         ; Aws.Util.option_map v.node_group_id (fun f -> "NodeGroupId", String.to_json f)
         ])

  let of_json j =
    { node_group_id = Aws.Util.option_map (Aws.Json.lookup j "NodeGroupId") String.of_json
    ; slots = Aws.Util.option_map (Aws.Json.lookup j "Slots") String.of_json
    ; replica_count =
        Aws.Util.option_map (Aws.Json.lookup j "ReplicaCount") Integer.of_json
    ; primary_availability_zone =
        Aws.Util.option_map (Aws.Json.lookup j "PrimaryAvailabilityZone") String.of_json
    ; replica_availability_zones =
        AvailabilityZonesList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicaAvailabilityZones"))
    ; primary_outpost_arn =
        Aws.Util.option_map (Aws.Json.lookup j "PrimaryOutpostArn") String.of_json
    ; replica_outpost_arns =
        OutpostArnsList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicaOutpostArns"))
    }
end

module NodeSnapshot = struct
  type t =
    { cache_cluster_id : String.t option
    ; node_group_id : String.t option
    ; cache_node_id : String.t option
    ; node_group_configuration : NodeGroupConfiguration.t option
    ; cache_size : String.t option
    ; cache_node_create_time : DateTime.t option
    ; snapshot_create_time : DateTime.t option
    }

  let make
      ?cache_cluster_id
      ?node_group_id
      ?cache_node_id
      ?node_group_configuration
      ?cache_size
      ?cache_node_create_time
      ?snapshot_create_time
      () =
    { cache_cluster_id
    ; node_group_id
    ; cache_node_id
    ; node_group_configuration
    ; cache_size
    ; cache_node_create_time
    ; snapshot_create_time
    }

  let parse xml =
    Some
      { cache_cluster_id =
          Aws.Util.option_bind (Aws.Xml.member "CacheClusterId" xml) String.parse
      ; node_group_id =
          Aws.Util.option_bind (Aws.Xml.member "NodeGroupId" xml) String.parse
      ; cache_node_id =
          Aws.Util.option_bind (Aws.Xml.member "CacheNodeId" xml) String.parse
      ; node_group_configuration =
          Aws.Util.option_bind
            (Aws.Xml.member "NodeGroupConfiguration" xml)
            NodeGroupConfiguration.parse
      ; cache_size = Aws.Util.option_bind (Aws.Xml.member "CacheSize" xml) String.parse
      ; cache_node_create_time =
          Aws.Util.option_bind (Aws.Xml.member "CacheNodeCreateTime" xml) DateTime.parse
      ; snapshot_create_time =
          Aws.Util.option_bind (Aws.Xml.member "SnapshotCreateTime" xml) DateTime.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.snapshot_create_time (fun f ->
               Aws.Query.Pair ("SnapshotCreateTime", DateTime.to_query f))
         ; Aws.Util.option_map v.cache_node_create_time (fun f ->
               Aws.Query.Pair ("CacheNodeCreateTime", DateTime.to_query f))
         ; Aws.Util.option_map v.cache_size (fun f ->
               Aws.Query.Pair ("CacheSize", String.to_query f))
         ; Aws.Util.option_map v.node_group_configuration (fun f ->
               Aws.Query.Pair ("NodeGroupConfiguration", NodeGroupConfiguration.to_query f))
         ; Aws.Util.option_map v.cache_node_id (fun f ->
               Aws.Query.Pair ("CacheNodeId", String.to_query f))
         ; Aws.Util.option_map v.node_group_id (fun f ->
               Aws.Query.Pair ("NodeGroupId", String.to_query f))
         ; Aws.Util.option_map v.cache_cluster_id (fun f ->
               Aws.Query.Pair ("CacheClusterId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.snapshot_create_time (fun f ->
               "SnapshotCreateTime", DateTime.to_json f)
         ; Aws.Util.option_map v.cache_node_create_time (fun f ->
               "CacheNodeCreateTime", DateTime.to_json f)
         ; Aws.Util.option_map v.cache_size (fun f -> "CacheSize", String.to_json f)
         ; Aws.Util.option_map v.node_group_configuration (fun f ->
               "NodeGroupConfiguration", NodeGroupConfiguration.to_json f)
         ; Aws.Util.option_map v.cache_node_id (fun f -> "CacheNodeId", String.to_json f)
         ; Aws.Util.option_map v.node_group_id (fun f -> "NodeGroupId", String.to_json f)
         ; Aws.Util.option_map v.cache_cluster_id (fun f ->
               "CacheClusterId", String.to_json f)
         ])

  let of_json j =
    { cache_cluster_id =
        Aws.Util.option_map (Aws.Json.lookup j "CacheClusterId") String.of_json
    ; node_group_id = Aws.Util.option_map (Aws.Json.lookup j "NodeGroupId") String.of_json
    ; cache_node_id = Aws.Util.option_map (Aws.Json.lookup j "CacheNodeId") String.of_json
    ; node_group_configuration =
        Aws.Util.option_map
          (Aws.Json.lookup j "NodeGroupConfiguration")
          NodeGroupConfiguration.of_json
    ; cache_size = Aws.Util.option_map (Aws.Json.lookup j "CacheSize") String.of_json
    ; cache_node_create_time =
        Aws.Util.option_map (Aws.Json.lookup j "CacheNodeCreateTime") DateTime.of_json
    ; snapshot_create_time =
        Aws.Util.option_map (Aws.Json.lookup j "SnapshotCreateTime") DateTime.of_json
    }
end

module NodeSnapshotList = struct
  type t = NodeSnapshot.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map NodeSnapshot.parse (Aws.Xml.members "NodeSnapshot" xml))

  let to_query v = Aws.Query.to_query_list NodeSnapshot.to_query v

  let to_json v = `List (List.map NodeSnapshot.to_json v)

  let of_json j = Aws.Json.to_list NodeSnapshot.of_json j
end

module DescribeCacheEngineVersionsMessage = struct
  type t =
    { engine : String.t option
    ; engine_version : String.t option
    ; cache_parameter_group_family : String.t option
    ; max_records : Integer.t option
    ; marker : String.t option
    ; default_only : Boolean.t option
    }

  let make
      ?engine
      ?engine_version
      ?cache_parameter_group_family
      ?max_records
      ?marker
      ?default_only
      () =
    { engine
    ; engine_version
    ; cache_parameter_group_family
    ; max_records
    ; marker
    ; default_only
    }

  let parse xml =
    Some
      { engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
      ; engine_version =
          Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
      ; cache_parameter_group_family =
          Aws.Util.option_bind
            (Aws.Xml.member "CacheParameterGroupFamily" xml)
            String.parse
      ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      ; default_only =
          Aws.Util.option_bind (Aws.Xml.member "DefaultOnly" xml) Boolean.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.default_only (fun f ->
               Aws.Query.Pair ("DefaultOnly", Boolean.to_query f))
         ; Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ; Aws.Util.option_map v.max_records (fun f ->
               Aws.Query.Pair ("MaxRecords", Integer.to_query f))
         ; Aws.Util.option_map v.cache_parameter_group_family (fun f ->
               Aws.Query.Pair ("CacheParameterGroupFamily", String.to_query f))
         ; Aws.Util.option_map v.engine_version (fun f ->
               Aws.Query.Pair ("EngineVersion", String.to_query f))
         ; Aws.Util.option_map v.engine (fun f ->
               Aws.Query.Pair ("Engine", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.default_only (fun f -> "DefaultOnly", Boolean.to_json f)
         ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
         ; Aws.Util.option_map v.cache_parameter_group_family (fun f ->
               "CacheParameterGroupFamily", String.to_json f)
         ; Aws.Util.option_map v.engine_version (fun f ->
               "EngineVersion", String.to_json f)
         ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
         ])

  let of_json j =
    { engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
    ; engine_version =
        Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
    ; cache_parameter_group_family =
        Aws.Util.option_map (Aws.Json.lookup j "CacheParameterGroupFamily") String.of_json
    ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
    ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    ; default_only = Aws.Util.option_map (Aws.Json.lookup j "DefaultOnly") Boolean.of_json
    }
end

module SubnetIdentifierList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "SubnetIdentifier" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module Tag = struct
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

module TagList = struct
  type t = Tag.t list

  let make elems () = elems

  let parse xml = Aws.Util.option_all (List.map Tag.parse (Aws.Xml.members "Tag" xml))

  let to_query v = Aws.Query.to_query_list Tag.to_query v

  let to_json v = `List (List.map Tag.to_json v)

  let of_json j = Aws.Json.to_list Tag.of_json j
end

module SnapshotArnsList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "SnapshotArn" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module SecurityGroupIdsList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "SecurityGroupId" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module OutpostMode = struct
  type t =
    | Single_outpost
    | Cross_outpost

  let str_to_t = [ "cross-outpost", Cross_outpost; "single-outpost", Single_outpost ]

  let t_to_str = [ Cross_outpost, "cross-outpost"; Single_outpost, "single-outpost" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module CacheSecurityGroupNameList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map String.parse (Aws.Xml.members "CacheSecurityGroupName" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module AZMode = struct
  type t =
    | Single_az
    | Cross_az

  let str_to_t = [ "cross-az", Cross_az; "single-az", Single_az ]

  let t_to_str = [ Cross_az, "cross-az"; Single_az, "single-az" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module CreateCacheClusterMessage = struct
  type t =
    { cache_cluster_id : String.t
    ; replication_group_id : String.t option
    ; a_z_mode : AZMode.t option
    ; preferred_availability_zone : String.t option
    ; preferred_availability_zones : PreferredAvailabilityZoneList.t
    ; num_cache_nodes : Integer.t option
    ; cache_node_type : String.t option
    ; engine : String.t option
    ; engine_version : String.t option
    ; cache_parameter_group_name : String.t option
    ; cache_subnet_group_name : String.t option
    ; cache_security_group_names : CacheSecurityGroupNameList.t
    ; security_group_ids : SecurityGroupIdsList.t
    ; tags : TagList.t
    ; snapshot_arns : SnapshotArnsList.t
    ; snapshot_name : String.t option
    ; preferred_maintenance_window : String.t option
    ; port : Integer.t option
    ; notification_topic_arn : String.t option
    ; auto_minor_version_upgrade : Boolean.t option
    ; snapshot_retention_limit : Integer.t option
    ; snapshot_window : String.t option
    ; auth_token : String.t option
    ; outpost_mode : OutpostMode.t option
    ; preferred_outpost_arn : String.t option
    ; preferred_outpost_arns : PreferredOutpostArnList.t
    }

  let make
      ~cache_cluster_id
      ?replication_group_id
      ?a_z_mode
      ?preferred_availability_zone
      ?(preferred_availability_zones = [])
      ?num_cache_nodes
      ?cache_node_type
      ?engine
      ?engine_version
      ?cache_parameter_group_name
      ?cache_subnet_group_name
      ?(cache_security_group_names = [])
      ?(security_group_ids = [])
      ?(tags = [])
      ?(snapshot_arns = [])
      ?snapshot_name
      ?preferred_maintenance_window
      ?port
      ?notification_topic_arn
      ?auto_minor_version_upgrade
      ?snapshot_retention_limit
      ?snapshot_window
      ?auth_token
      ?outpost_mode
      ?preferred_outpost_arn
      ?(preferred_outpost_arns = [])
      () =
    { cache_cluster_id
    ; replication_group_id
    ; a_z_mode
    ; preferred_availability_zone
    ; preferred_availability_zones
    ; num_cache_nodes
    ; cache_node_type
    ; engine
    ; engine_version
    ; cache_parameter_group_name
    ; cache_subnet_group_name
    ; cache_security_group_names
    ; security_group_ids
    ; tags
    ; snapshot_arns
    ; snapshot_name
    ; preferred_maintenance_window
    ; port
    ; notification_topic_arn
    ; auto_minor_version_upgrade
    ; snapshot_retention_limit
    ; snapshot_window
    ; auth_token
    ; outpost_mode
    ; preferred_outpost_arn
    ; preferred_outpost_arns
    }

  let parse xml =
    Some
      { cache_cluster_id =
          Aws.Xml.required
            "CacheClusterId"
            (Aws.Util.option_bind (Aws.Xml.member "CacheClusterId" xml) String.parse)
      ; replication_group_id =
          Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse
      ; a_z_mode = Aws.Util.option_bind (Aws.Xml.member "AZMode" xml) AZMode.parse
      ; preferred_availability_zone =
          Aws.Util.option_bind
            (Aws.Xml.member "PreferredAvailabilityZone" xml)
            String.parse
      ; preferred_availability_zones =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "PreferredAvailabilityZones" xml)
               PreferredAvailabilityZoneList.parse)
      ; num_cache_nodes =
          Aws.Util.option_bind (Aws.Xml.member "NumCacheNodes" xml) Integer.parse
      ; cache_node_type =
          Aws.Util.option_bind (Aws.Xml.member "CacheNodeType" xml) String.parse
      ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
      ; engine_version =
          Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
      ; cache_parameter_group_name =
          Aws.Util.option_bind (Aws.Xml.member "CacheParameterGroupName" xml) String.parse
      ; cache_subnet_group_name =
          Aws.Util.option_bind (Aws.Xml.member "CacheSubnetGroupName" xml) String.parse
      ; cache_security_group_names =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "CacheSecurityGroupNames" xml)
               CacheSecurityGroupNameList.parse)
      ; security_group_ids =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "SecurityGroupIds" xml)
               SecurityGroupIdsList.parse)
      ; tags =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
      ; snapshot_arns =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "SnapshotArns" xml)
               SnapshotArnsList.parse)
      ; snapshot_name =
          Aws.Util.option_bind (Aws.Xml.member "SnapshotName" xml) String.parse
      ; preferred_maintenance_window =
          Aws.Util.option_bind
            (Aws.Xml.member "PreferredMaintenanceWindow" xml)
            String.parse
      ; port = Aws.Util.option_bind (Aws.Xml.member "Port" xml) Integer.parse
      ; notification_topic_arn =
          Aws.Util.option_bind (Aws.Xml.member "NotificationTopicArn" xml) String.parse
      ; auto_minor_version_upgrade =
          Aws.Util.option_bind
            (Aws.Xml.member "AutoMinorVersionUpgrade" xml)
            Boolean.parse
      ; snapshot_retention_limit =
          Aws.Util.option_bind (Aws.Xml.member "SnapshotRetentionLimit" xml) Integer.parse
      ; snapshot_window =
          Aws.Util.option_bind (Aws.Xml.member "SnapshotWindow" xml) String.parse
      ; auth_token = Aws.Util.option_bind (Aws.Xml.member "AuthToken" xml) String.parse
      ; outpost_mode =
          Aws.Util.option_bind (Aws.Xml.member "OutpostMode" xml) OutpostMode.parse
      ; preferred_outpost_arn =
          Aws.Util.option_bind (Aws.Xml.member "PreferredOutpostArn" xml) String.parse
      ; preferred_outpost_arns =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "PreferredOutpostArns" xml)
               PreferredOutpostArnList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "PreferredOutpostArns.member"
                , PreferredOutpostArnList.to_query v.preferred_outpost_arns ))
         ; Aws.Util.option_map v.preferred_outpost_arn (fun f ->
               Aws.Query.Pair ("PreferredOutpostArn", String.to_query f))
         ; Aws.Util.option_map v.outpost_mode (fun f ->
               Aws.Query.Pair ("OutpostMode", OutpostMode.to_query f))
         ; Aws.Util.option_map v.auth_token (fun f ->
               Aws.Query.Pair ("AuthToken", String.to_query f))
         ; Aws.Util.option_map v.snapshot_window (fun f ->
               Aws.Query.Pair ("SnapshotWindow", String.to_query f))
         ; Aws.Util.option_map v.snapshot_retention_limit (fun f ->
               Aws.Query.Pair ("SnapshotRetentionLimit", Integer.to_query f))
         ; Aws.Util.option_map v.auto_minor_version_upgrade (fun f ->
               Aws.Query.Pair ("AutoMinorVersionUpgrade", Boolean.to_query f))
         ; Aws.Util.option_map v.notification_topic_arn (fun f ->
               Aws.Query.Pair ("NotificationTopicArn", String.to_query f))
         ; Aws.Util.option_map v.port (fun f ->
               Aws.Query.Pair ("Port", Integer.to_query f))
         ; Aws.Util.option_map v.preferred_maintenance_window (fun f ->
               Aws.Query.Pair ("PreferredMaintenanceWindow", String.to_query f))
         ; Aws.Util.option_map v.snapshot_name (fun f ->
               Aws.Query.Pair ("SnapshotName", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("SnapshotArns.member", SnapshotArnsList.to_query v.snapshot_arns))
         ; Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Some
             (Aws.Query.Pair
                ( "SecurityGroupIds.member"
                , SecurityGroupIdsList.to_query v.security_group_ids ))
         ; Some
             (Aws.Query.Pair
                ( "CacheSecurityGroupNames.member"
                , CacheSecurityGroupNameList.to_query v.cache_security_group_names ))
         ; Aws.Util.option_map v.cache_subnet_group_name (fun f ->
               Aws.Query.Pair ("CacheSubnetGroupName", String.to_query f))
         ; Aws.Util.option_map v.cache_parameter_group_name (fun f ->
               Aws.Query.Pair ("CacheParameterGroupName", String.to_query f))
         ; Aws.Util.option_map v.engine_version (fun f ->
               Aws.Query.Pair ("EngineVersion", String.to_query f))
         ; Aws.Util.option_map v.engine (fun f ->
               Aws.Query.Pair ("Engine", String.to_query f))
         ; Aws.Util.option_map v.cache_node_type (fun f ->
               Aws.Query.Pair ("CacheNodeType", String.to_query f))
         ; Aws.Util.option_map v.num_cache_nodes (fun f ->
               Aws.Query.Pair ("NumCacheNodes", Integer.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "PreferredAvailabilityZones.member"
                , PreferredAvailabilityZoneList.to_query v.preferred_availability_zones ))
         ; Aws.Util.option_map v.preferred_availability_zone (fun f ->
               Aws.Query.Pair ("PreferredAvailabilityZone", String.to_query f))
         ; Aws.Util.option_map v.a_z_mode (fun f ->
               Aws.Query.Pair ("AZMode", AZMode.to_query f))
         ; Aws.Util.option_map v.replication_group_id (fun f ->
               Aws.Query.Pair ("ReplicationGroupId", String.to_query f))
         ; Some (Aws.Query.Pair ("CacheClusterId", String.to_query v.cache_cluster_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "PreferredOutpostArns"
             , PreferredOutpostArnList.to_json v.preferred_outpost_arns )
         ; Aws.Util.option_map v.preferred_outpost_arn (fun f ->
               "PreferredOutpostArn", String.to_json f)
         ; Aws.Util.option_map v.outpost_mode (fun f ->
               "OutpostMode", OutpostMode.to_json f)
         ; Aws.Util.option_map v.auth_token (fun f -> "AuthToken", String.to_json f)
         ; Aws.Util.option_map v.snapshot_window (fun f ->
               "SnapshotWindow", String.to_json f)
         ; Aws.Util.option_map v.snapshot_retention_limit (fun f ->
               "SnapshotRetentionLimit", Integer.to_json f)
         ; Aws.Util.option_map v.auto_minor_version_upgrade (fun f ->
               "AutoMinorVersionUpgrade", Boolean.to_json f)
         ; Aws.Util.option_map v.notification_topic_arn (fun f ->
               "NotificationTopicArn", String.to_json f)
         ; Aws.Util.option_map v.port (fun f -> "Port", Integer.to_json f)
         ; Aws.Util.option_map v.preferred_maintenance_window (fun f ->
               "PreferredMaintenanceWindow", String.to_json f)
         ; Aws.Util.option_map v.snapshot_name (fun f -> "SnapshotName", String.to_json f)
         ; Some ("SnapshotArns", SnapshotArnsList.to_json v.snapshot_arns)
         ; Some ("Tags", TagList.to_json v.tags)
         ; Some ("SecurityGroupIds", SecurityGroupIdsList.to_json v.security_group_ids)
         ; Some
             ( "CacheSecurityGroupNames"
             , CacheSecurityGroupNameList.to_json v.cache_security_group_names )
         ; Aws.Util.option_map v.cache_subnet_group_name (fun f ->
               "CacheSubnetGroupName", String.to_json f)
         ; Aws.Util.option_map v.cache_parameter_group_name (fun f ->
               "CacheParameterGroupName", String.to_json f)
         ; Aws.Util.option_map v.engine_version (fun f ->
               "EngineVersion", String.to_json f)
         ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
         ; Aws.Util.option_map v.cache_node_type (fun f ->
               "CacheNodeType", String.to_json f)
         ; Aws.Util.option_map v.num_cache_nodes (fun f ->
               "NumCacheNodes", Integer.to_json f)
         ; Some
             ( "PreferredAvailabilityZones"
             , PreferredAvailabilityZoneList.to_json v.preferred_availability_zones )
         ; Aws.Util.option_map v.preferred_availability_zone (fun f ->
               "PreferredAvailabilityZone", String.to_json f)
         ; Aws.Util.option_map v.a_z_mode (fun f -> "AZMode", AZMode.to_json f)
         ; Aws.Util.option_map v.replication_group_id (fun f ->
               "ReplicationGroupId", String.to_json f)
         ; Some ("CacheClusterId", String.to_json v.cache_cluster_id)
         ])

  let of_json j =
    { cache_cluster_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheClusterId"))
    ; replication_group_id =
        Aws.Util.option_map (Aws.Json.lookup j "ReplicationGroupId") String.of_json
    ; a_z_mode = Aws.Util.option_map (Aws.Json.lookup j "AZMode") AZMode.of_json
    ; preferred_availability_zone =
        Aws.Util.option_map (Aws.Json.lookup j "PreferredAvailabilityZone") String.of_json
    ; preferred_availability_zones =
        PreferredAvailabilityZoneList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "PreferredAvailabilityZones"))
    ; num_cache_nodes =
        Aws.Util.option_map (Aws.Json.lookup j "NumCacheNodes") Integer.of_json
    ; cache_node_type =
        Aws.Util.option_map (Aws.Json.lookup j "CacheNodeType") String.of_json
    ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
    ; engine_version =
        Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
    ; cache_parameter_group_name =
        Aws.Util.option_map (Aws.Json.lookup j "CacheParameterGroupName") String.of_json
    ; cache_subnet_group_name =
        Aws.Util.option_map (Aws.Json.lookup j "CacheSubnetGroupName") String.of_json
    ; cache_security_group_names =
        CacheSecurityGroupNameList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheSecurityGroupNames"))
    ; security_group_ids =
        SecurityGroupIdsList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroupIds"))
    ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
    ; snapshot_arns =
        SnapshotArnsList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "SnapshotArns"))
    ; snapshot_name =
        Aws.Util.option_map (Aws.Json.lookup j "SnapshotName") String.of_json
    ; preferred_maintenance_window =
        Aws.Util.option_map
          (Aws.Json.lookup j "PreferredMaintenanceWindow")
          String.of_json
    ; port = Aws.Util.option_map (Aws.Json.lookup j "Port") Integer.of_json
    ; notification_topic_arn =
        Aws.Util.option_map (Aws.Json.lookup j "NotificationTopicArn") String.of_json
    ; auto_minor_version_upgrade =
        Aws.Util.option_map (Aws.Json.lookup j "AutoMinorVersionUpgrade") Boolean.of_json
    ; snapshot_retention_limit =
        Aws.Util.option_map (Aws.Json.lookup j "SnapshotRetentionLimit") Integer.of_json
    ; snapshot_window =
        Aws.Util.option_map (Aws.Json.lookup j "SnapshotWindow") String.of_json
    ; auth_token = Aws.Util.option_map (Aws.Json.lookup j "AuthToken") String.of_json
    ; outpost_mode =
        Aws.Util.option_map (Aws.Json.lookup j "OutpostMode") OutpostMode.of_json
    ; preferred_outpost_arn =
        Aws.Util.option_map (Aws.Json.lookup j "PreferredOutpostArn") String.of_json
    ; preferred_outpost_arns =
        PreferredOutpostArnList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "PreferredOutpostArns"))
    }
end

module Snapshot = struct
  type t =
    { snapshot_name : String.t option
    ; replication_group_id : String.t option
    ; replication_group_description : String.t option
    ; cache_cluster_id : String.t option
    ; snapshot_status : String.t option
    ; snapshot_source : String.t option
    ; cache_node_type : String.t option
    ; engine : String.t option
    ; engine_version : String.t option
    ; num_cache_nodes : Integer.t option
    ; preferred_availability_zone : String.t option
    ; preferred_outpost_arn : String.t option
    ; cache_cluster_create_time : DateTime.t option
    ; preferred_maintenance_window : String.t option
    ; topic_arn : String.t option
    ; port : Integer.t option
    ; cache_parameter_group_name : String.t option
    ; cache_subnet_group_name : String.t option
    ; vpc_id : String.t option
    ; auto_minor_version_upgrade : Boolean.t option
    ; snapshot_retention_limit : Integer.t option
    ; snapshot_window : String.t option
    ; num_node_groups : Integer.t option
    ; automatic_failover : AutomaticFailoverStatus.t option
    ; node_snapshots : NodeSnapshotList.t
    ; kms_key_id : String.t option
    ; a_r_n : String.t option
    }

  let make
      ?snapshot_name
      ?replication_group_id
      ?replication_group_description
      ?cache_cluster_id
      ?snapshot_status
      ?snapshot_source
      ?cache_node_type
      ?engine
      ?engine_version
      ?num_cache_nodes
      ?preferred_availability_zone
      ?preferred_outpost_arn
      ?cache_cluster_create_time
      ?preferred_maintenance_window
      ?topic_arn
      ?port
      ?cache_parameter_group_name
      ?cache_subnet_group_name
      ?vpc_id
      ?auto_minor_version_upgrade
      ?snapshot_retention_limit
      ?snapshot_window
      ?num_node_groups
      ?automatic_failover
      ?(node_snapshots = [])
      ?kms_key_id
      ?a_r_n
      () =
    { snapshot_name
    ; replication_group_id
    ; replication_group_description
    ; cache_cluster_id
    ; snapshot_status
    ; snapshot_source
    ; cache_node_type
    ; engine
    ; engine_version
    ; num_cache_nodes
    ; preferred_availability_zone
    ; preferred_outpost_arn
    ; cache_cluster_create_time
    ; preferred_maintenance_window
    ; topic_arn
    ; port
    ; cache_parameter_group_name
    ; cache_subnet_group_name
    ; vpc_id
    ; auto_minor_version_upgrade
    ; snapshot_retention_limit
    ; snapshot_window
    ; num_node_groups
    ; automatic_failover
    ; node_snapshots
    ; kms_key_id
    ; a_r_n
    }

  let parse xml =
    Some
      { snapshot_name =
          Aws.Util.option_bind (Aws.Xml.member "SnapshotName" xml) String.parse
      ; replication_group_id =
          Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse
      ; replication_group_description =
          Aws.Util.option_bind
            (Aws.Xml.member "ReplicationGroupDescription" xml)
            String.parse
      ; cache_cluster_id =
          Aws.Util.option_bind (Aws.Xml.member "CacheClusterId" xml) String.parse
      ; snapshot_status =
          Aws.Util.option_bind (Aws.Xml.member "SnapshotStatus" xml) String.parse
      ; snapshot_source =
          Aws.Util.option_bind (Aws.Xml.member "SnapshotSource" xml) String.parse
      ; cache_node_type =
          Aws.Util.option_bind (Aws.Xml.member "CacheNodeType" xml) String.parse
      ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
      ; engine_version =
          Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
      ; num_cache_nodes =
          Aws.Util.option_bind (Aws.Xml.member "NumCacheNodes" xml) Integer.parse
      ; preferred_availability_zone =
          Aws.Util.option_bind
            (Aws.Xml.member "PreferredAvailabilityZone" xml)
            String.parse
      ; preferred_outpost_arn =
          Aws.Util.option_bind (Aws.Xml.member "PreferredOutpostArn" xml) String.parse
      ; cache_cluster_create_time =
          Aws.Util.option_bind
            (Aws.Xml.member "CacheClusterCreateTime" xml)
            DateTime.parse
      ; preferred_maintenance_window =
          Aws.Util.option_bind
            (Aws.Xml.member "PreferredMaintenanceWindow" xml)
            String.parse
      ; topic_arn = Aws.Util.option_bind (Aws.Xml.member "TopicArn" xml) String.parse
      ; port = Aws.Util.option_bind (Aws.Xml.member "Port" xml) Integer.parse
      ; cache_parameter_group_name =
          Aws.Util.option_bind (Aws.Xml.member "CacheParameterGroupName" xml) String.parse
      ; cache_subnet_group_name =
          Aws.Util.option_bind (Aws.Xml.member "CacheSubnetGroupName" xml) String.parse
      ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "VpcId" xml) String.parse
      ; auto_minor_version_upgrade =
          Aws.Util.option_bind
            (Aws.Xml.member "AutoMinorVersionUpgrade" xml)
            Boolean.parse
      ; snapshot_retention_limit =
          Aws.Util.option_bind (Aws.Xml.member "SnapshotRetentionLimit" xml) Integer.parse
      ; snapshot_window =
          Aws.Util.option_bind (Aws.Xml.member "SnapshotWindow" xml) String.parse
      ; num_node_groups =
          Aws.Util.option_bind (Aws.Xml.member "NumNodeGroups" xml) Integer.parse
      ; automatic_failover =
          Aws.Util.option_bind
            (Aws.Xml.member "AutomaticFailover" xml)
            AutomaticFailoverStatus.parse
      ; node_snapshots =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "NodeSnapshots" xml)
               NodeSnapshotList.parse)
      ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse
      ; a_r_n = Aws.Util.option_bind (Aws.Xml.member "ARN" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.a_r_n (fun f ->
               Aws.Query.Pair ("ARN", String.to_query f))
         ; Aws.Util.option_map v.kms_key_id (fun f ->
               Aws.Query.Pair ("KmsKeyId", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("NodeSnapshots.member", NodeSnapshotList.to_query v.node_snapshots))
         ; Aws.Util.option_map v.automatic_failover (fun f ->
               Aws.Query.Pair ("AutomaticFailover", AutomaticFailoverStatus.to_query f))
         ; Aws.Util.option_map v.num_node_groups (fun f ->
               Aws.Query.Pair ("NumNodeGroups", Integer.to_query f))
         ; Aws.Util.option_map v.snapshot_window (fun f ->
               Aws.Query.Pair ("SnapshotWindow", String.to_query f))
         ; Aws.Util.option_map v.snapshot_retention_limit (fun f ->
               Aws.Query.Pair ("SnapshotRetentionLimit", Integer.to_query f))
         ; Aws.Util.option_map v.auto_minor_version_upgrade (fun f ->
               Aws.Query.Pair ("AutoMinorVersionUpgrade", Boolean.to_query f))
         ; Aws.Util.option_map v.vpc_id (fun f ->
               Aws.Query.Pair ("VpcId", String.to_query f))
         ; Aws.Util.option_map v.cache_subnet_group_name (fun f ->
               Aws.Query.Pair ("CacheSubnetGroupName", String.to_query f))
         ; Aws.Util.option_map v.cache_parameter_group_name (fun f ->
               Aws.Query.Pair ("CacheParameterGroupName", String.to_query f))
         ; Aws.Util.option_map v.port (fun f ->
               Aws.Query.Pair ("Port", Integer.to_query f))
         ; Aws.Util.option_map v.topic_arn (fun f ->
               Aws.Query.Pair ("TopicArn", String.to_query f))
         ; Aws.Util.option_map v.preferred_maintenance_window (fun f ->
               Aws.Query.Pair ("PreferredMaintenanceWindow", String.to_query f))
         ; Aws.Util.option_map v.cache_cluster_create_time (fun f ->
               Aws.Query.Pair ("CacheClusterCreateTime", DateTime.to_query f))
         ; Aws.Util.option_map v.preferred_outpost_arn (fun f ->
               Aws.Query.Pair ("PreferredOutpostArn", String.to_query f))
         ; Aws.Util.option_map v.preferred_availability_zone (fun f ->
               Aws.Query.Pair ("PreferredAvailabilityZone", String.to_query f))
         ; Aws.Util.option_map v.num_cache_nodes (fun f ->
               Aws.Query.Pair ("NumCacheNodes", Integer.to_query f))
         ; Aws.Util.option_map v.engine_version (fun f ->
               Aws.Query.Pair ("EngineVersion", String.to_query f))
         ; Aws.Util.option_map v.engine (fun f ->
               Aws.Query.Pair ("Engine", String.to_query f))
         ; Aws.Util.option_map v.cache_node_type (fun f ->
               Aws.Query.Pair ("CacheNodeType", String.to_query f))
         ; Aws.Util.option_map v.snapshot_source (fun f ->
               Aws.Query.Pair ("SnapshotSource", String.to_query f))
         ; Aws.Util.option_map v.snapshot_status (fun f ->
               Aws.Query.Pair ("SnapshotStatus", String.to_query f))
         ; Aws.Util.option_map v.cache_cluster_id (fun f ->
               Aws.Query.Pair ("CacheClusterId", String.to_query f))
         ; Aws.Util.option_map v.replication_group_description (fun f ->
               Aws.Query.Pair ("ReplicationGroupDescription", String.to_query f))
         ; Aws.Util.option_map v.replication_group_id (fun f ->
               Aws.Query.Pair ("ReplicationGroupId", String.to_query f))
         ; Aws.Util.option_map v.snapshot_name (fun f ->
               Aws.Query.Pair ("SnapshotName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.a_r_n (fun f -> "ARN", String.to_json f)
         ; Aws.Util.option_map v.kms_key_id (fun f -> "KmsKeyId", String.to_json f)
         ; Some ("NodeSnapshots", NodeSnapshotList.to_json v.node_snapshots)
         ; Aws.Util.option_map v.automatic_failover (fun f ->
               "AutomaticFailover", AutomaticFailoverStatus.to_json f)
         ; Aws.Util.option_map v.num_node_groups (fun f ->
               "NumNodeGroups", Integer.to_json f)
         ; Aws.Util.option_map v.snapshot_window (fun f ->
               "SnapshotWindow", String.to_json f)
         ; Aws.Util.option_map v.snapshot_retention_limit (fun f ->
               "SnapshotRetentionLimit", Integer.to_json f)
         ; Aws.Util.option_map v.auto_minor_version_upgrade (fun f ->
               "AutoMinorVersionUpgrade", Boolean.to_json f)
         ; Aws.Util.option_map v.vpc_id (fun f -> "VpcId", String.to_json f)
         ; Aws.Util.option_map v.cache_subnet_group_name (fun f ->
               "CacheSubnetGroupName", String.to_json f)
         ; Aws.Util.option_map v.cache_parameter_group_name (fun f ->
               "CacheParameterGroupName", String.to_json f)
         ; Aws.Util.option_map v.port (fun f -> "Port", Integer.to_json f)
         ; Aws.Util.option_map v.topic_arn (fun f -> "TopicArn", String.to_json f)
         ; Aws.Util.option_map v.preferred_maintenance_window (fun f ->
               "PreferredMaintenanceWindow", String.to_json f)
         ; Aws.Util.option_map v.cache_cluster_create_time (fun f ->
               "CacheClusterCreateTime", DateTime.to_json f)
         ; Aws.Util.option_map v.preferred_outpost_arn (fun f ->
               "PreferredOutpostArn", String.to_json f)
         ; Aws.Util.option_map v.preferred_availability_zone (fun f ->
               "PreferredAvailabilityZone", String.to_json f)
         ; Aws.Util.option_map v.num_cache_nodes (fun f ->
               "NumCacheNodes", Integer.to_json f)
         ; Aws.Util.option_map v.engine_version (fun f ->
               "EngineVersion", String.to_json f)
         ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
         ; Aws.Util.option_map v.cache_node_type (fun f ->
               "CacheNodeType", String.to_json f)
         ; Aws.Util.option_map v.snapshot_source (fun f ->
               "SnapshotSource", String.to_json f)
         ; Aws.Util.option_map v.snapshot_status (fun f ->
               "SnapshotStatus", String.to_json f)
         ; Aws.Util.option_map v.cache_cluster_id (fun f ->
               "CacheClusterId", String.to_json f)
         ; Aws.Util.option_map v.replication_group_description (fun f ->
               "ReplicationGroupDescription", String.to_json f)
         ; Aws.Util.option_map v.replication_group_id (fun f ->
               "ReplicationGroupId", String.to_json f)
         ; Aws.Util.option_map v.snapshot_name (fun f -> "SnapshotName", String.to_json f)
         ])

  let of_json j =
    { snapshot_name =
        Aws.Util.option_map (Aws.Json.lookup j "SnapshotName") String.of_json
    ; replication_group_id =
        Aws.Util.option_map (Aws.Json.lookup j "ReplicationGroupId") String.of_json
    ; replication_group_description =
        Aws.Util.option_map
          (Aws.Json.lookup j "ReplicationGroupDescription")
          String.of_json
    ; cache_cluster_id =
        Aws.Util.option_map (Aws.Json.lookup j "CacheClusterId") String.of_json
    ; snapshot_status =
        Aws.Util.option_map (Aws.Json.lookup j "SnapshotStatus") String.of_json
    ; snapshot_source =
        Aws.Util.option_map (Aws.Json.lookup j "SnapshotSource") String.of_json
    ; cache_node_type =
        Aws.Util.option_map (Aws.Json.lookup j "CacheNodeType") String.of_json
    ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
    ; engine_version =
        Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
    ; num_cache_nodes =
        Aws.Util.option_map (Aws.Json.lookup j "NumCacheNodes") Integer.of_json
    ; preferred_availability_zone =
        Aws.Util.option_map (Aws.Json.lookup j "PreferredAvailabilityZone") String.of_json
    ; preferred_outpost_arn =
        Aws.Util.option_map (Aws.Json.lookup j "PreferredOutpostArn") String.of_json
    ; cache_cluster_create_time =
        Aws.Util.option_map (Aws.Json.lookup j "CacheClusterCreateTime") DateTime.of_json
    ; preferred_maintenance_window =
        Aws.Util.option_map
          (Aws.Json.lookup j "PreferredMaintenanceWindow")
          String.of_json
    ; topic_arn = Aws.Util.option_map (Aws.Json.lookup j "TopicArn") String.of_json
    ; port = Aws.Util.option_map (Aws.Json.lookup j "Port") Integer.of_json
    ; cache_parameter_group_name =
        Aws.Util.option_map (Aws.Json.lookup j "CacheParameterGroupName") String.of_json
    ; cache_subnet_group_name =
        Aws.Util.option_map (Aws.Json.lookup j "CacheSubnetGroupName") String.of_json
    ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "VpcId") String.of_json
    ; auto_minor_version_upgrade =
        Aws.Util.option_map (Aws.Json.lookup j "AutoMinorVersionUpgrade") Boolean.of_json
    ; snapshot_retention_limit =
        Aws.Util.option_map (Aws.Json.lookup j "SnapshotRetentionLimit") Integer.of_json
    ; snapshot_window =
        Aws.Util.option_map (Aws.Json.lookup j "SnapshotWindow") String.of_json
    ; num_node_groups =
        Aws.Util.option_map (Aws.Json.lookup j "NumNodeGroups") Integer.of_json
    ; automatic_failover =
        Aws.Util.option_map
          (Aws.Json.lookup j "AutomaticFailover")
          AutomaticFailoverStatus.of_json
    ; node_snapshots =
        NodeSnapshotList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "NodeSnapshots"))
    ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
    ; a_r_n = Aws.Util.option_map (Aws.Json.lookup j "ARN") String.of_json
    }
end

module CreateSnapshotResult = struct
  type t = { snapshot : Snapshot.t option }

  let make ?snapshot () = { snapshot }

  let parse xml =
    Some
      { snapshot = Aws.Util.option_bind (Aws.Xml.member "Snapshot" xml) Snapshot.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.snapshot (fun f ->
               Aws.Query.Pair ("Snapshot", Snapshot.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.snapshot (fun f -> "Snapshot", Snapshot.to_json f) ])

  let of_json j =
    { snapshot = Aws.Util.option_map (Aws.Json.lookup j "Snapshot") Snapshot.of_json }
end

module CacheNodeUpdateStatus = struct
  type t =
    { cache_node_id : String.t option
    ; node_update_status : NodeUpdateStatus.t option
    ; node_deletion_date : DateTime.t option
    ; node_update_start_date : DateTime.t option
    ; node_update_end_date : DateTime.t option
    ; node_update_initiated_by : NodeUpdateInitiatedBy.t option
    ; node_update_initiated_date : DateTime.t option
    ; node_update_status_modified_date : DateTime.t option
    }

  let make
      ?cache_node_id
      ?node_update_status
      ?node_deletion_date
      ?node_update_start_date
      ?node_update_end_date
      ?node_update_initiated_by
      ?node_update_initiated_date
      ?node_update_status_modified_date
      () =
    { cache_node_id
    ; node_update_status
    ; node_deletion_date
    ; node_update_start_date
    ; node_update_end_date
    ; node_update_initiated_by
    ; node_update_initiated_date
    ; node_update_status_modified_date
    }

  let parse xml =
    Some
      { cache_node_id =
          Aws.Util.option_bind (Aws.Xml.member "CacheNodeId" xml) String.parse
      ; node_update_status =
          Aws.Util.option_bind
            (Aws.Xml.member "NodeUpdateStatus" xml)
            NodeUpdateStatus.parse
      ; node_deletion_date =
          Aws.Util.option_bind (Aws.Xml.member "NodeDeletionDate" xml) DateTime.parse
      ; node_update_start_date =
          Aws.Util.option_bind (Aws.Xml.member "NodeUpdateStartDate" xml) DateTime.parse
      ; node_update_end_date =
          Aws.Util.option_bind (Aws.Xml.member "NodeUpdateEndDate" xml) DateTime.parse
      ; node_update_initiated_by =
          Aws.Util.option_bind
            (Aws.Xml.member "NodeUpdateInitiatedBy" xml)
            NodeUpdateInitiatedBy.parse
      ; node_update_initiated_date =
          Aws.Util.option_bind
            (Aws.Xml.member "NodeUpdateInitiatedDate" xml)
            DateTime.parse
      ; node_update_status_modified_date =
          Aws.Util.option_bind
            (Aws.Xml.member "NodeUpdateStatusModifiedDate" xml)
            DateTime.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.node_update_status_modified_date (fun f ->
               Aws.Query.Pair ("NodeUpdateStatusModifiedDate", DateTime.to_query f))
         ; Aws.Util.option_map v.node_update_initiated_date (fun f ->
               Aws.Query.Pair ("NodeUpdateInitiatedDate", DateTime.to_query f))
         ; Aws.Util.option_map v.node_update_initiated_by (fun f ->
               Aws.Query.Pair ("NodeUpdateInitiatedBy", NodeUpdateInitiatedBy.to_query f))
         ; Aws.Util.option_map v.node_update_end_date (fun f ->
               Aws.Query.Pair ("NodeUpdateEndDate", DateTime.to_query f))
         ; Aws.Util.option_map v.node_update_start_date (fun f ->
               Aws.Query.Pair ("NodeUpdateStartDate", DateTime.to_query f))
         ; Aws.Util.option_map v.node_deletion_date (fun f ->
               Aws.Query.Pair ("NodeDeletionDate", DateTime.to_query f))
         ; Aws.Util.option_map v.node_update_status (fun f ->
               Aws.Query.Pair ("NodeUpdateStatus", NodeUpdateStatus.to_query f))
         ; Aws.Util.option_map v.cache_node_id (fun f ->
               Aws.Query.Pair ("CacheNodeId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.node_update_status_modified_date (fun f ->
               "NodeUpdateStatusModifiedDate", DateTime.to_json f)
         ; Aws.Util.option_map v.node_update_initiated_date (fun f ->
               "NodeUpdateInitiatedDate", DateTime.to_json f)
         ; Aws.Util.option_map v.node_update_initiated_by (fun f ->
               "NodeUpdateInitiatedBy", NodeUpdateInitiatedBy.to_json f)
         ; Aws.Util.option_map v.node_update_end_date (fun f ->
               "NodeUpdateEndDate", DateTime.to_json f)
         ; Aws.Util.option_map v.node_update_start_date (fun f ->
               "NodeUpdateStartDate", DateTime.to_json f)
         ; Aws.Util.option_map v.node_deletion_date (fun f ->
               "NodeDeletionDate", DateTime.to_json f)
         ; Aws.Util.option_map v.node_update_status (fun f ->
               "NodeUpdateStatus", NodeUpdateStatus.to_json f)
         ; Aws.Util.option_map v.cache_node_id (fun f -> "CacheNodeId", String.to_json f)
         ])

  let of_json j =
    { cache_node_id = Aws.Util.option_map (Aws.Json.lookup j "CacheNodeId") String.of_json
    ; node_update_status =
        Aws.Util.option_map
          (Aws.Json.lookup j "NodeUpdateStatus")
          NodeUpdateStatus.of_json
    ; node_deletion_date =
        Aws.Util.option_map (Aws.Json.lookup j "NodeDeletionDate") DateTime.of_json
    ; node_update_start_date =
        Aws.Util.option_map (Aws.Json.lookup j "NodeUpdateStartDate") DateTime.of_json
    ; node_update_end_date =
        Aws.Util.option_map (Aws.Json.lookup j "NodeUpdateEndDate") DateTime.of_json
    ; node_update_initiated_by =
        Aws.Util.option_map
          (Aws.Json.lookup j "NodeUpdateInitiatedBy")
          NodeUpdateInitiatedBy.of_json
    ; node_update_initiated_date =
        Aws.Util.option_map (Aws.Json.lookup j "NodeUpdateInitiatedDate") DateTime.of_json
    ; node_update_status_modified_date =
        Aws.Util.option_map
          (Aws.Json.lookup j "NodeUpdateStatusModifiedDate")
          DateTime.of_json
    }
end

module CopySnapshotMessage = struct
  type t =
    { source_snapshot_name : String.t
    ; target_snapshot_name : String.t
    ; target_bucket : String.t option
    ; kms_key_id : String.t option
    }

  let make ~source_snapshot_name ~target_snapshot_name ?target_bucket ?kms_key_id () =
    { source_snapshot_name; target_snapshot_name; target_bucket; kms_key_id }

  let parse xml =
    Some
      { source_snapshot_name =
          Aws.Xml.required
            "SourceSnapshotName"
            (Aws.Util.option_bind (Aws.Xml.member "SourceSnapshotName" xml) String.parse)
      ; target_snapshot_name =
          Aws.Xml.required
            "TargetSnapshotName"
            (Aws.Util.option_bind (Aws.Xml.member "TargetSnapshotName" xml) String.parse)
      ; target_bucket =
          Aws.Util.option_bind (Aws.Xml.member "TargetBucket" xml) String.parse
      ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.kms_key_id (fun f ->
               Aws.Query.Pair ("KmsKeyId", String.to_query f))
         ; Aws.Util.option_map v.target_bucket (fun f ->
               Aws.Query.Pair ("TargetBucket", String.to_query f))
         ; Some
             (Aws.Query.Pair ("TargetSnapshotName", String.to_query v.target_snapshot_name))
         ; Some
             (Aws.Query.Pair ("SourceSnapshotName", String.to_query v.source_snapshot_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.kms_key_id (fun f -> "KmsKeyId", String.to_json f)
         ; Aws.Util.option_map v.target_bucket (fun f -> "TargetBucket", String.to_json f)
         ; Some ("TargetSnapshotName", String.to_json v.target_snapshot_name)
         ; Some ("SourceSnapshotName", String.to_json v.source_snapshot_name)
         ])

  let of_json j =
    { source_snapshot_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SourceSnapshotName"))
    ; target_snapshot_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TargetSnapshotName"))
    ; target_bucket =
        Aws.Util.option_map (Aws.Json.lookup j "TargetBucket") String.of_json
    ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
    }
end

module SubnetOutpost = struct
  type t = { subnet_outpost_arn : String.t option }

  let make ?subnet_outpost_arn () = { subnet_outpost_arn }

  let parse xml =
    Some
      { subnet_outpost_arn =
          Aws.Util.option_bind (Aws.Xml.member "SubnetOutpostArn" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.subnet_outpost_arn (fun f ->
               Aws.Query.Pair ("SubnetOutpostArn", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.subnet_outpost_arn (fun f ->
               "SubnetOutpostArn", String.to_json f)
         ])

  let of_json j =
    { subnet_outpost_arn =
        Aws.Util.option_map (Aws.Json.lookup j "SubnetOutpostArn") String.of_json
    }
end

module AvailabilityZone = struct
  type t = { name : String.t option }

  let make ?name () = { name }

  let parse xml =
    Some { name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.name (fun f ->
               Aws.Query.Pair ("Name", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.name (fun f -> "Name", String.to_json f) ])

  let of_json j = { name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json }
end

module Subnet = struct
  type t =
    { subnet_identifier : String.t option
    ; subnet_availability_zone : AvailabilityZone.t option
    ; subnet_outpost : SubnetOutpost.t option
    }

  let make ?subnet_identifier ?subnet_availability_zone ?subnet_outpost () =
    { subnet_identifier; subnet_availability_zone; subnet_outpost }

  let parse xml =
    Some
      { subnet_identifier =
          Aws.Util.option_bind (Aws.Xml.member "SubnetIdentifier" xml) String.parse
      ; subnet_availability_zone =
          Aws.Util.option_bind
            (Aws.Xml.member "SubnetAvailabilityZone" xml)
            AvailabilityZone.parse
      ; subnet_outpost =
          Aws.Util.option_bind (Aws.Xml.member "SubnetOutpost" xml) SubnetOutpost.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.subnet_outpost (fun f ->
               Aws.Query.Pair ("SubnetOutpost", SubnetOutpost.to_query f))
         ; Aws.Util.option_map v.subnet_availability_zone (fun f ->
               Aws.Query.Pair ("SubnetAvailabilityZone", AvailabilityZone.to_query f))
         ; Aws.Util.option_map v.subnet_identifier (fun f ->
               Aws.Query.Pair ("SubnetIdentifier", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.subnet_outpost (fun f ->
               "SubnetOutpost", SubnetOutpost.to_json f)
         ; Aws.Util.option_map v.subnet_availability_zone (fun f ->
               "SubnetAvailabilityZone", AvailabilityZone.to_json f)
         ; Aws.Util.option_map v.subnet_identifier (fun f ->
               "SubnetIdentifier", String.to_json f)
         ])

  let of_json j =
    { subnet_identifier =
        Aws.Util.option_map (Aws.Json.lookup j "SubnetIdentifier") String.of_json
    ; subnet_availability_zone =
        Aws.Util.option_map
          (Aws.Json.lookup j "SubnetAvailabilityZone")
          AvailabilityZone.of_json
    ; subnet_outpost =
        Aws.Util.option_map (Aws.Json.lookup j "SubnetOutpost") SubnetOutpost.of_json
    }
end

module SubnetList = struct
  type t = Subnet.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map Subnet.parse (Aws.Xml.members "Subnet" xml))

  let to_query v = Aws.Query.to_query_list Subnet.to_query v

  let to_json v = `List (List.map Subnet.to_json v)

  let of_json j = Aws.Json.to_list Subnet.of_json j
end

module CacheSubnetGroup = struct
  type t =
    { cache_subnet_group_name : String.t option
    ; cache_subnet_group_description : String.t option
    ; vpc_id : String.t option
    ; subnets : SubnetList.t
    ; a_r_n : String.t option
    }

  let make
      ?cache_subnet_group_name
      ?cache_subnet_group_description
      ?vpc_id
      ?(subnets = [])
      ?a_r_n
      () =
    { cache_subnet_group_name; cache_subnet_group_description; vpc_id; subnets; a_r_n }

  let parse xml =
    Some
      { cache_subnet_group_name =
          Aws.Util.option_bind (Aws.Xml.member "CacheSubnetGroupName" xml) String.parse
      ; cache_subnet_group_description =
          Aws.Util.option_bind
            (Aws.Xml.member "CacheSubnetGroupDescription" xml)
            String.parse
      ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "VpcId" xml) String.parse
      ; subnets =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Subnets" xml) SubnetList.parse)
      ; a_r_n = Aws.Util.option_bind (Aws.Xml.member "ARN" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.a_r_n (fun f ->
               Aws.Query.Pair ("ARN", String.to_query f))
         ; Some (Aws.Query.Pair ("Subnets.member", SubnetList.to_query v.subnets))
         ; Aws.Util.option_map v.vpc_id (fun f ->
               Aws.Query.Pair ("VpcId", String.to_query f))
         ; Aws.Util.option_map v.cache_subnet_group_description (fun f ->
               Aws.Query.Pair ("CacheSubnetGroupDescription", String.to_query f))
         ; Aws.Util.option_map v.cache_subnet_group_name (fun f ->
               Aws.Query.Pair ("CacheSubnetGroupName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.a_r_n (fun f -> "ARN", String.to_json f)
         ; Some ("Subnets", SubnetList.to_json v.subnets)
         ; Aws.Util.option_map v.vpc_id (fun f -> "VpcId", String.to_json f)
         ; Aws.Util.option_map v.cache_subnet_group_description (fun f ->
               "CacheSubnetGroupDescription", String.to_json f)
         ; Aws.Util.option_map v.cache_subnet_group_name (fun f ->
               "CacheSubnetGroupName", String.to_json f)
         ])

  let of_json j =
    { cache_subnet_group_name =
        Aws.Util.option_map (Aws.Json.lookup j "CacheSubnetGroupName") String.of_json
    ; cache_subnet_group_description =
        Aws.Util.option_map
          (Aws.Json.lookup j "CacheSubnetGroupDescription")
          String.of_json
    ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "VpcId") String.of_json
    ; subnets = SubnetList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Subnets"))
    ; a_r_n = Aws.Util.option_map (Aws.Json.lookup j "ARN") String.of_json
    }
end

module CacheSubnetGroups = struct
  type t = CacheSubnetGroup.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map CacheSubnetGroup.parse (Aws.Xml.members "CacheSubnetGroup" xml))

  let to_query v = Aws.Query.to_query_list CacheSubnetGroup.to_query v

  let to_json v = `List (List.map CacheSubnetGroup.to_json v)

  let of_json j = Aws.Json.to_list CacheSubnetGroup.of_json j
end

module SourceType = struct
  type t =
    | Cache_cluster
    | Cache_parameter_group
    | Cache_security_group
    | Cache_subnet_group
    | Replication_group
    | User
    | User_group

  let str_to_t =
    [ "user-group", User_group
    ; "user", User
    ; "replication-group", Replication_group
    ; "cache-subnet-group", Cache_subnet_group
    ; "cache-security-group", Cache_security_group
    ; "cache-parameter-group", Cache_parameter_group
    ; "cache-cluster", Cache_cluster
    ]

  let t_to_str =
    [ User_group, "user-group"
    ; User, "user"
    ; Replication_group, "replication-group"
    ; Cache_subnet_group, "cache-subnet-group"
    ; Cache_security_group, "cache-security-group"
    ; Cache_parameter_group, "cache-parameter-group"
    ; Cache_cluster, "cache-cluster"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module Event = struct
  type t =
    { source_identifier : String.t option
    ; source_type : SourceType.t option
    ; message : String.t option
    ; date : DateTime.t option
    }

  let make ?source_identifier ?source_type ?message ?date () =
    { source_identifier; source_type; message; date }

  let parse xml =
    Some
      { source_identifier =
          Aws.Util.option_bind (Aws.Xml.member "SourceIdentifier" xml) String.parse
      ; source_type =
          Aws.Util.option_bind (Aws.Xml.member "SourceType" xml) SourceType.parse
      ; message = Aws.Util.option_bind (Aws.Xml.member "Message" xml) String.parse
      ; date = Aws.Util.option_bind (Aws.Xml.member "Date" xml) DateTime.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.date (fun f ->
               Aws.Query.Pair ("Date", DateTime.to_query f))
         ; Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("Message", String.to_query f))
         ; Aws.Util.option_map v.source_type (fun f ->
               Aws.Query.Pair ("SourceType", SourceType.to_query f))
         ; Aws.Util.option_map v.source_identifier (fun f ->
               Aws.Query.Pair ("SourceIdentifier", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.date (fun f -> "Date", DateTime.to_json f)
         ; Aws.Util.option_map v.message (fun f -> "Message", String.to_json f)
         ; Aws.Util.option_map v.source_type (fun f -> "SourceType", SourceType.to_json f)
         ; Aws.Util.option_map v.source_identifier (fun f ->
               "SourceIdentifier", String.to_json f)
         ])

  let of_json j =
    { source_identifier =
        Aws.Util.option_map (Aws.Json.lookup j "SourceIdentifier") String.of_json
    ; source_type =
        Aws.Util.option_map (Aws.Json.lookup j "SourceType") SourceType.of_json
    ; message = Aws.Util.option_map (Aws.Json.lookup j "Message") String.of_json
    ; date = Aws.Util.option_map (Aws.Json.lookup j "Date") DateTime.of_json
    }
end

module EventList = struct
  type t = Event.t list

  let make elems () = elems

  let parse xml = Aws.Util.option_all (List.map Event.parse (Aws.Xml.members "Event" xml))

  let to_query v = Aws.Query.to_query_list Event.to_query v

  let to_json v = `List (List.map Event.to_json v)

  let of_json j = Aws.Json.to_list Event.of_json j
end

module EventsMessage = struct
  type t =
    { marker : String.t option
    ; events : EventList.t
    }

  let make ?marker ?(events = []) () = { marker; events }

  let parse xml =
    Some
      { marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      ; events =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Events" xml) EventList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Events.member", EventList.to_query v.events))
         ; Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Events", EventList.to_json v.events)
         ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ])

  let of_json j =
    { marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    ; events = EventList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Events"))
    }
end

module DeleteCacheSecurityGroupMessage = struct
  type t = { cache_security_group_name : String.t }

  let make ~cache_security_group_name () = { cache_security_group_name }

  let parse xml =
    Some
      { cache_security_group_name =
          Aws.Xml.required
            "CacheSecurityGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "CacheSecurityGroupName" xml)
               String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("CacheSecurityGroupName", String.to_query v.cache_security_group_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("CacheSecurityGroupName", String.to_json v.cache_security_group_name) ])

  let of_json j =
    { cache_security_group_name =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheSecurityGroupName"))
    }
end

module ServiceUpdateType = struct
  type t = Security_update

  let str_to_t = [ "security-update", Security_update ]

  let t_to_str = [ Security_update, "security-update" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module ServiceUpdateStatus = struct
  type t =
    | Available
    | Cancelled
    | Expired

  let str_to_t = [ "expired", Expired; "cancelled", Cancelled; "available", Available ]

  let t_to_str = [ Expired, "expired"; Cancelled, "cancelled"; Available, "available" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module ServiceUpdateSeverity = struct
  type t =
    | Critical
    | Important
    | Medium
    | Low

  let str_to_t =
    [ "low", Low; "medium", Medium; "important", Important; "critical", Critical ]

  let t_to_str =
    [ Low, "low"; Medium, "medium"; Important, "important"; Critical, "critical" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module CacheNodeUpdateStatusList = struct
  type t = CacheNodeUpdateStatus.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map CacheNodeUpdateStatus.parse (Aws.Xml.members "CacheNodeUpdateStatus" xml))

  let to_query v = Aws.Query.to_query_list CacheNodeUpdateStatus.to_query v

  let to_json v = `List (List.map CacheNodeUpdateStatus.to_json v)

  let of_json j = Aws.Json.to_list CacheNodeUpdateStatus.of_json j
end

module UpdateAction = struct
  type t =
    { replication_group_id : String.t option
    ; cache_cluster_id : String.t option
    ; service_update_name : String.t option
    ; service_update_release_date : DateTime.t option
    ; service_update_severity : ServiceUpdateSeverity.t option
    ; service_update_status : ServiceUpdateStatus.t option
    ; service_update_recommended_apply_by_date : DateTime.t option
    ; service_update_type : ServiceUpdateType.t option
    ; update_action_available_date : DateTime.t option
    ; update_action_status : UpdateActionStatus.t option
    ; nodes_updated : String.t option
    ; update_action_status_modified_date : DateTime.t option
    ; sla_met : SlaMet.t option
    ; node_group_update_status : NodeGroupUpdateStatusList.t
    ; cache_node_update_status : CacheNodeUpdateStatusList.t
    ; estimated_update_time : String.t option
    ; engine : String.t option
    }

  let make
      ?replication_group_id
      ?cache_cluster_id
      ?service_update_name
      ?service_update_release_date
      ?service_update_severity
      ?service_update_status
      ?service_update_recommended_apply_by_date
      ?service_update_type
      ?update_action_available_date
      ?update_action_status
      ?nodes_updated
      ?update_action_status_modified_date
      ?sla_met
      ?(node_group_update_status = [])
      ?(cache_node_update_status = [])
      ?estimated_update_time
      ?engine
      () =
    { replication_group_id
    ; cache_cluster_id
    ; service_update_name
    ; service_update_release_date
    ; service_update_severity
    ; service_update_status
    ; service_update_recommended_apply_by_date
    ; service_update_type
    ; update_action_available_date
    ; update_action_status
    ; nodes_updated
    ; update_action_status_modified_date
    ; sla_met
    ; node_group_update_status
    ; cache_node_update_status
    ; estimated_update_time
    ; engine
    }

  let parse xml =
    Some
      { replication_group_id =
          Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse
      ; cache_cluster_id =
          Aws.Util.option_bind (Aws.Xml.member "CacheClusterId" xml) String.parse
      ; service_update_name =
          Aws.Util.option_bind (Aws.Xml.member "ServiceUpdateName" xml) String.parse
      ; service_update_release_date =
          Aws.Util.option_bind
            (Aws.Xml.member "ServiceUpdateReleaseDate" xml)
            DateTime.parse
      ; service_update_severity =
          Aws.Util.option_bind
            (Aws.Xml.member "ServiceUpdateSeverity" xml)
            ServiceUpdateSeverity.parse
      ; service_update_status =
          Aws.Util.option_bind
            (Aws.Xml.member "ServiceUpdateStatus" xml)
            ServiceUpdateStatus.parse
      ; service_update_recommended_apply_by_date =
          Aws.Util.option_bind
            (Aws.Xml.member "ServiceUpdateRecommendedApplyByDate" xml)
            DateTime.parse
      ; service_update_type =
          Aws.Util.option_bind
            (Aws.Xml.member "ServiceUpdateType" xml)
            ServiceUpdateType.parse
      ; update_action_available_date =
          Aws.Util.option_bind
            (Aws.Xml.member "UpdateActionAvailableDate" xml)
            DateTime.parse
      ; update_action_status =
          Aws.Util.option_bind
            (Aws.Xml.member "UpdateActionStatus" xml)
            UpdateActionStatus.parse
      ; nodes_updated =
          Aws.Util.option_bind (Aws.Xml.member "NodesUpdated" xml) String.parse
      ; update_action_status_modified_date =
          Aws.Util.option_bind
            (Aws.Xml.member "UpdateActionStatusModifiedDate" xml)
            DateTime.parse
      ; sla_met = Aws.Util.option_bind (Aws.Xml.member "SlaMet" xml) SlaMet.parse
      ; node_group_update_status =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "NodeGroupUpdateStatus" xml)
               NodeGroupUpdateStatusList.parse)
      ; cache_node_update_status =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "CacheNodeUpdateStatus" xml)
               CacheNodeUpdateStatusList.parse)
      ; estimated_update_time =
          Aws.Util.option_bind (Aws.Xml.member "EstimatedUpdateTime" xml) String.parse
      ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.engine (fun f ->
               Aws.Query.Pair ("Engine", String.to_query f))
         ; Aws.Util.option_map v.estimated_update_time (fun f ->
               Aws.Query.Pair ("EstimatedUpdateTime", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "CacheNodeUpdateStatus.member"
                , CacheNodeUpdateStatusList.to_query v.cache_node_update_status ))
         ; Some
             (Aws.Query.Pair
                ( "NodeGroupUpdateStatus.member"
                , NodeGroupUpdateStatusList.to_query v.node_group_update_status ))
         ; Aws.Util.option_map v.sla_met (fun f ->
               Aws.Query.Pair ("SlaMet", SlaMet.to_query f))
         ; Aws.Util.option_map v.update_action_status_modified_date (fun f ->
               Aws.Query.Pair ("UpdateActionStatusModifiedDate", DateTime.to_query f))
         ; Aws.Util.option_map v.nodes_updated (fun f ->
               Aws.Query.Pair ("NodesUpdated", String.to_query f))
         ; Aws.Util.option_map v.update_action_status (fun f ->
               Aws.Query.Pair ("UpdateActionStatus", UpdateActionStatus.to_query f))
         ; Aws.Util.option_map v.update_action_available_date (fun f ->
               Aws.Query.Pair ("UpdateActionAvailableDate", DateTime.to_query f))
         ; Aws.Util.option_map v.service_update_type (fun f ->
               Aws.Query.Pair ("ServiceUpdateType", ServiceUpdateType.to_query f))
         ; Aws.Util.option_map v.service_update_recommended_apply_by_date (fun f ->
               Aws.Query.Pair ("ServiceUpdateRecommendedApplyByDate", DateTime.to_query f))
         ; Aws.Util.option_map v.service_update_status (fun f ->
               Aws.Query.Pair ("ServiceUpdateStatus", ServiceUpdateStatus.to_query f))
         ; Aws.Util.option_map v.service_update_severity (fun f ->
               Aws.Query.Pair ("ServiceUpdateSeverity", ServiceUpdateSeverity.to_query f))
         ; Aws.Util.option_map v.service_update_release_date (fun f ->
               Aws.Query.Pair ("ServiceUpdateReleaseDate", DateTime.to_query f))
         ; Aws.Util.option_map v.service_update_name (fun f ->
               Aws.Query.Pair ("ServiceUpdateName", String.to_query f))
         ; Aws.Util.option_map v.cache_cluster_id (fun f ->
               Aws.Query.Pair ("CacheClusterId", String.to_query f))
         ; Aws.Util.option_map v.replication_group_id (fun f ->
               Aws.Query.Pair ("ReplicationGroupId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
         ; Aws.Util.option_map v.estimated_update_time (fun f ->
               "EstimatedUpdateTime", String.to_json f)
         ; Some
             ( "CacheNodeUpdateStatus"
             , CacheNodeUpdateStatusList.to_json v.cache_node_update_status )
         ; Some
             ( "NodeGroupUpdateStatus"
             , NodeGroupUpdateStatusList.to_json v.node_group_update_status )
         ; Aws.Util.option_map v.sla_met (fun f -> "SlaMet", SlaMet.to_json f)
         ; Aws.Util.option_map v.update_action_status_modified_date (fun f ->
               "UpdateActionStatusModifiedDate", DateTime.to_json f)
         ; Aws.Util.option_map v.nodes_updated (fun f -> "NodesUpdated", String.to_json f)
         ; Aws.Util.option_map v.update_action_status (fun f ->
               "UpdateActionStatus", UpdateActionStatus.to_json f)
         ; Aws.Util.option_map v.update_action_available_date (fun f ->
               "UpdateActionAvailableDate", DateTime.to_json f)
         ; Aws.Util.option_map v.service_update_type (fun f ->
               "ServiceUpdateType", ServiceUpdateType.to_json f)
         ; Aws.Util.option_map v.service_update_recommended_apply_by_date (fun f ->
               "ServiceUpdateRecommendedApplyByDate", DateTime.to_json f)
         ; Aws.Util.option_map v.service_update_status (fun f ->
               "ServiceUpdateStatus", ServiceUpdateStatus.to_json f)
         ; Aws.Util.option_map v.service_update_severity (fun f ->
               "ServiceUpdateSeverity", ServiceUpdateSeverity.to_json f)
         ; Aws.Util.option_map v.service_update_release_date (fun f ->
               "ServiceUpdateReleaseDate", DateTime.to_json f)
         ; Aws.Util.option_map v.service_update_name (fun f ->
               "ServiceUpdateName", String.to_json f)
         ; Aws.Util.option_map v.cache_cluster_id (fun f ->
               "CacheClusterId", String.to_json f)
         ; Aws.Util.option_map v.replication_group_id (fun f ->
               "ReplicationGroupId", String.to_json f)
         ])

  let of_json j =
    { replication_group_id =
        Aws.Util.option_map (Aws.Json.lookup j "ReplicationGroupId") String.of_json
    ; cache_cluster_id =
        Aws.Util.option_map (Aws.Json.lookup j "CacheClusterId") String.of_json
    ; service_update_name =
        Aws.Util.option_map (Aws.Json.lookup j "ServiceUpdateName") String.of_json
    ; service_update_release_date =
        Aws.Util.option_map
          (Aws.Json.lookup j "ServiceUpdateReleaseDate")
          DateTime.of_json
    ; service_update_severity =
        Aws.Util.option_map
          (Aws.Json.lookup j "ServiceUpdateSeverity")
          ServiceUpdateSeverity.of_json
    ; service_update_status =
        Aws.Util.option_map
          (Aws.Json.lookup j "ServiceUpdateStatus")
          ServiceUpdateStatus.of_json
    ; service_update_recommended_apply_by_date =
        Aws.Util.option_map
          (Aws.Json.lookup j "ServiceUpdateRecommendedApplyByDate")
          DateTime.of_json
    ; service_update_type =
        Aws.Util.option_map
          (Aws.Json.lookup j "ServiceUpdateType")
          ServiceUpdateType.of_json
    ; update_action_available_date =
        Aws.Util.option_map
          (Aws.Json.lookup j "UpdateActionAvailableDate")
          DateTime.of_json
    ; update_action_status =
        Aws.Util.option_map
          (Aws.Json.lookup j "UpdateActionStatus")
          UpdateActionStatus.of_json
    ; nodes_updated =
        Aws.Util.option_map (Aws.Json.lookup j "NodesUpdated") String.of_json
    ; update_action_status_modified_date =
        Aws.Util.option_map
          (Aws.Json.lookup j "UpdateActionStatusModifiedDate")
          DateTime.of_json
    ; sla_met = Aws.Util.option_map (Aws.Json.lookup j "SlaMet") SlaMet.of_json
    ; node_group_update_status =
        NodeGroupUpdateStatusList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "NodeGroupUpdateStatus"))
    ; cache_node_update_status =
        CacheNodeUpdateStatusList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheNodeUpdateStatus"))
    ; estimated_update_time =
        Aws.Util.option_map (Aws.Json.lookup j "EstimatedUpdateTime") String.of_json
    ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
    }
end

module UpdateActionList = struct
  type t = UpdateAction.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map UpdateAction.parse (Aws.Xml.members "UpdateAction" xml))

  let to_query v = Aws.Query.to_query_list UpdateAction.to_query v

  let to_json v = `List (List.map UpdateAction.to_json v)

  let of_json j = Aws.Json.to_list UpdateAction.of_json j
end

module UpdateActionsMessage = struct
  type t =
    { marker : String.t option
    ; update_actions : UpdateActionList.t
    }

  let make ?marker ?(update_actions = []) () = { marker; update_actions }

  let parse xml =
    Some
      { marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      ; update_actions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "UpdateActions" xml)
               UpdateActionList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("UpdateActions.member", UpdateActionList.to_query v.update_actions))
         ; Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("UpdateActions", UpdateActionList.to_json v.update_actions)
         ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ])

  let of_json j =
    { marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    ; update_actions =
        UpdateActionList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "UpdateActions"))
    }
end

module ReservedCacheNodesOffering = struct
  type t =
    { reserved_cache_nodes_offering_id : String.t option
    ; cache_node_type : String.t option
    ; duration : Integer.t option
    ; fixed_price : Double.t option
    ; usage_price : Double.t option
    ; product_description : String.t option
    ; offering_type : String.t option
    ; recurring_charges : RecurringChargeList.t
    }

  let make
      ?reserved_cache_nodes_offering_id
      ?cache_node_type
      ?duration
      ?fixed_price
      ?usage_price
      ?product_description
      ?offering_type
      ?(recurring_charges = [])
      () =
    { reserved_cache_nodes_offering_id
    ; cache_node_type
    ; duration
    ; fixed_price
    ; usage_price
    ; product_description
    ; offering_type
    ; recurring_charges
    }

  let parse xml =
    Some
      { reserved_cache_nodes_offering_id =
          Aws.Util.option_bind
            (Aws.Xml.member "ReservedCacheNodesOfferingId" xml)
            String.parse
      ; cache_node_type =
          Aws.Util.option_bind (Aws.Xml.member "CacheNodeType" xml) String.parse
      ; duration = Aws.Util.option_bind (Aws.Xml.member "Duration" xml) Integer.parse
      ; fixed_price = Aws.Util.option_bind (Aws.Xml.member "FixedPrice" xml) Double.parse
      ; usage_price = Aws.Util.option_bind (Aws.Xml.member "UsagePrice" xml) Double.parse
      ; product_description =
          Aws.Util.option_bind (Aws.Xml.member "ProductDescription" xml) String.parse
      ; offering_type =
          Aws.Util.option_bind (Aws.Xml.member "OfferingType" xml) String.parse
      ; recurring_charges =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "RecurringCharges" xml)
               RecurringChargeList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "RecurringCharges.member"
                , RecurringChargeList.to_query v.recurring_charges ))
         ; Aws.Util.option_map v.offering_type (fun f ->
               Aws.Query.Pair ("OfferingType", String.to_query f))
         ; Aws.Util.option_map v.product_description (fun f ->
               Aws.Query.Pair ("ProductDescription", String.to_query f))
         ; Aws.Util.option_map v.usage_price (fun f ->
               Aws.Query.Pair ("UsagePrice", Double.to_query f))
         ; Aws.Util.option_map v.fixed_price (fun f ->
               Aws.Query.Pair ("FixedPrice", Double.to_query f))
         ; Aws.Util.option_map v.duration (fun f ->
               Aws.Query.Pair ("Duration", Integer.to_query f))
         ; Aws.Util.option_map v.cache_node_type (fun f ->
               Aws.Query.Pair ("CacheNodeType", String.to_query f))
         ; Aws.Util.option_map v.reserved_cache_nodes_offering_id (fun f ->
               Aws.Query.Pair ("ReservedCacheNodesOfferingId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("RecurringCharges", RecurringChargeList.to_json v.recurring_charges)
         ; Aws.Util.option_map v.offering_type (fun f -> "OfferingType", String.to_json f)
         ; Aws.Util.option_map v.product_description (fun f ->
               "ProductDescription", String.to_json f)
         ; Aws.Util.option_map v.usage_price (fun f -> "UsagePrice", Double.to_json f)
         ; Aws.Util.option_map v.fixed_price (fun f -> "FixedPrice", Double.to_json f)
         ; Aws.Util.option_map v.duration (fun f -> "Duration", Integer.to_json f)
         ; Aws.Util.option_map v.cache_node_type (fun f ->
               "CacheNodeType", String.to_json f)
         ; Aws.Util.option_map v.reserved_cache_nodes_offering_id (fun f ->
               "ReservedCacheNodesOfferingId", String.to_json f)
         ])

  let of_json j =
    { reserved_cache_nodes_offering_id =
        Aws.Util.option_map
          (Aws.Json.lookup j "ReservedCacheNodesOfferingId")
          String.of_json
    ; cache_node_type =
        Aws.Util.option_map (Aws.Json.lookup j "CacheNodeType") String.of_json
    ; duration = Aws.Util.option_map (Aws.Json.lookup j "Duration") Integer.of_json
    ; fixed_price = Aws.Util.option_map (Aws.Json.lookup j "FixedPrice") Double.of_json
    ; usage_price = Aws.Util.option_map (Aws.Json.lookup j "UsagePrice") Double.of_json
    ; product_description =
        Aws.Util.option_map (Aws.Json.lookup j "ProductDescription") String.of_json
    ; offering_type =
        Aws.Util.option_map (Aws.Json.lookup j "OfferingType") String.of_json
    ; recurring_charges =
        RecurringChargeList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "RecurringCharges"))
    }
end

module ReservedCacheNodesOfferingList = struct
  type t = ReservedCacheNodesOffering.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map
         ReservedCacheNodesOffering.parse
         (Aws.Xml.members "ReservedCacheNodesOffering" xml))

  let to_query v = Aws.Query.to_query_list ReservedCacheNodesOffering.to_query v

  let to_json v = `List (List.map ReservedCacheNodesOffering.to_json v)

  let of_json j = Aws.Json.to_list ReservedCacheNodesOffering.of_json j
end

module DisassociateGlobalReplicationGroupResult = struct
  type t = { global_replication_group : GlobalReplicationGroup.t option }

  let make ?global_replication_group () = { global_replication_group }

  let parse xml =
    Some
      { global_replication_group =
          Aws.Util.option_bind
            (Aws.Xml.member "GlobalReplicationGroup" xml)
            GlobalReplicationGroup.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.global_replication_group (fun f ->
               Aws.Query.Pair ("GlobalReplicationGroup", GlobalReplicationGroup.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.global_replication_group (fun f ->
               "GlobalReplicationGroup", GlobalReplicationGroup.to_json f)
         ])

  let of_json j =
    { global_replication_group =
        Aws.Util.option_map
          (Aws.Json.lookup j "GlobalReplicationGroup")
          GlobalReplicationGroup.of_json
    }
end

module DescribeCacheSubnetGroupsMessage = struct
  type t =
    { cache_subnet_group_name : String.t option
    ; max_records : Integer.t option
    ; marker : String.t option
    }

  let make ?cache_subnet_group_name ?max_records ?marker () =
    { cache_subnet_group_name; max_records; marker }

  let parse xml =
    Some
      { cache_subnet_group_name =
          Aws.Util.option_bind (Aws.Xml.member "CacheSubnetGroupName" xml) String.parse
      ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ; Aws.Util.option_map v.max_records (fun f ->
               Aws.Query.Pair ("MaxRecords", Integer.to_query f))
         ; Aws.Util.option_map v.cache_subnet_group_name (fun f ->
               Aws.Query.Pair ("CacheSubnetGroupName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
         ; Aws.Util.option_map v.cache_subnet_group_name (fun f ->
               "CacheSubnetGroupName", String.to_json f)
         ])

  let of_json j =
    { cache_subnet_group_name =
        Aws.Util.option_map (Aws.Json.lookup j "CacheSubnetGroupName") String.of_json
    ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
    ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    }
end

module AuthenticationType = struct
  type t =
    | Password
    | No_password

  let str_to_t = [ "no-password", No_password; "password", Password ]

  let t_to_str = [ No_password, "no-password"; Password, "password" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module Authentication = struct
  type t =
    { type_ : AuthenticationType.t option
    ; password_count : Integer.t option
    }

  let make ?type_ ?password_count () = { type_; password_count }

  let parse xml =
    Some
      { type_ = Aws.Util.option_bind (Aws.Xml.member "Type" xml) AuthenticationType.parse
      ; password_count =
          Aws.Util.option_bind (Aws.Xml.member "PasswordCount" xml) Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.password_count (fun f ->
               Aws.Query.Pair ("PasswordCount", Integer.to_query f))
         ; Aws.Util.option_map v.type_ (fun f ->
               Aws.Query.Pair ("Type", AuthenticationType.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.password_count (fun f ->
               "PasswordCount", Integer.to_json f)
         ; Aws.Util.option_map v.type_ (fun f -> "Type", AuthenticationType.to_json f)
         ])

  let of_json j =
    { type_ = Aws.Util.option_map (Aws.Json.lookup j "Type") AuthenticationType.of_json
    ; password_count =
        Aws.Util.option_map (Aws.Json.lookup j "PasswordCount") Integer.of_json
    }
end

module User = struct
  type t =
    { user_id : String.t option
    ; user_name : String.t option
    ; status : String.t option
    ; engine : String.t option
    ; access_string : String.t option
    ; user_group_ids : UserGroupIdList.t
    ; authentication : Authentication.t option
    ; a_r_n : String.t option
    }

  let make
      ?user_id
      ?user_name
      ?status
      ?engine
      ?access_string
      ?(user_group_ids = [])
      ?authentication
      ?a_r_n
      () =
    { user_id
    ; user_name
    ; status
    ; engine
    ; access_string
    ; user_group_ids
    ; authentication
    ; a_r_n
    }

  let parse xml =
    Some
      { user_id = Aws.Util.option_bind (Aws.Xml.member "UserId" xml) String.parse
      ; user_name = Aws.Util.option_bind (Aws.Xml.member "UserName" xml) String.parse
      ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
      ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
      ; access_string =
          Aws.Util.option_bind (Aws.Xml.member "AccessString" xml) String.parse
      ; user_group_ids =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "UserGroupIds" xml)
               UserGroupIdList.parse)
      ; authentication =
          Aws.Util.option_bind (Aws.Xml.member "Authentication" xml) Authentication.parse
      ; a_r_n = Aws.Util.option_bind (Aws.Xml.member "ARN" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.a_r_n (fun f ->
               Aws.Query.Pair ("ARN", String.to_query f))
         ; Aws.Util.option_map v.authentication (fun f ->
               Aws.Query.Pair ("Authentication", Authentication.to_query f))
         ; Some
             (Aws.Query.Pair
                ("UserGroupIds.member", UserGroupIdList.to_query v.user_group_ids))
         ; Aws.Util.option_map v.access_string (fun f ->
               Aws.Query.Pair ("AccessString", String.to_query f))
         ; Aws.Util.option_map v.engine (fun f ->
               Aws.Query.Pair ("Engine", String.to_query f))
         ; Aws.Util.option_map v.status (fun f ->
               Aws.Query.Pair ("Status", String.to_query f))
         ; Aws.Util.option_map v.user_name (fun f ->
               Aws.Query.Pair ("UserName", String.to_query f))
         ; Aws.Util.option_map v.user_id (fun f ->
               Aws.Query.Pair ("UserId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.a_r_n (fun f -> "ARN", String.to_json f)
         ; Aws.Util.option_map v.authentication (fun f ->
               "Authentication", Authentication.to_json f)
         ; Some ("UserGroupIds", UserGroupIdList.to_json v.user_group_ids)
         ; Aws.Util.option_map v.access_string (fun f -> "AccessString", String.to_json f)
         ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
         ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
         ; Aws.Util.option_map v.user_name (fun f -> "UserName", String.to_json f)
         ; Aws.Util.option_map v.user_id (fun f -> "UserId", String.to_json f)
         ])

  let of_json j =
    { user_id = Aws.Util.option_map (Aws.Json.lookup j "UserId") String.of_json
    ; user_name = Aws.Util.option_map (Aws.Json.lookup j "UserName") String.of_json
    ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
    ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
    ; access_string =
        Aws.Util.option_map (Aws.Json.lookup j "AccessString") String.of_json
    ; user_group_ids =
        UserGroupIdList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "UserGroupIds"))
    ; authentication =
        Aws.Util.option_map (Aws.Json.lookup j "Authentication") Authentication.of_json
    ; a_r_n = Aws.Util.option_map (Aws.Json.lookup j "ARN") String.of_json
    }
end

module UserList = struct
  type t = User.t list

  let make elems () = elems

  let parse xml = Aws.Util.option_all (List.map User.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list User.to_query v

  let to_json v = `List (List.map User.to_json v)

  let of_json j = Aws.Json.to_list User.of_json j
end

module UpdateActionStatusList = struct
  type t = UpdateActionStatus.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map UpdateActionStatus.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list UpdateActionStatus.to_query v

  let to_json v = `List (List.map UpdateActionStatus.to_json v)

  let of_json j = Aws.Json.to_list UpdateActionStatus.of_json j
end

module TimeRangeFilter = struct
  type t =
    { start_time : DateTime.t option
    ; end_time : DateTime.t option
    }

  let make ?start_time ?end_time () = { start_time; end_time }

  let parse xml =
    Some
      { start_time = Aws.Util.option_bind (Aws.Xml.member "StartTime" xml) DateTime.parse
      ; end_time = Aws.Util.option_bind (Aws.Xml.member "EndTime" xml) DateTime.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.end_time (fun f ->
               Aws.Query.Pair ("EndTime", DateTime.to_query f))
         ; Aws.Util.option_map v.start_time (fun f ->
               Aws.Query.Pair ("StartTime", DateTime.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.end_time (fun f -> "EndTime", DateTime.to_json f)
         ; Aws.Util.option_map v.start_time (fun f -> "StartTime", DateTime.to_json f)
         ])

  let of_json j =
    { start_time = Aws.Util.option_map (Aws.Json.lookup j "StartTime") DateTime.of_json
    ; end_time = Aws.Util.option_map (Aws.Json.lookup j "EndTime") DateTime.of_json
    }
end

module ServiceUpdateStatusList = struct
  type t = ServiceUpdateStatus.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map ServiceUpdateStatus.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list ServiceUpdateStatus.to_query v

  let to_json v = `List (List.map ServiceUpdateStatus.to_json v)

  let of_json j = Aws.Json.to_list ServiceUpdateStatus.of_json j
end

module ReplicationGroupIdList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module DescribeUpdateActionsMessage = struct
  type t =
    { service_update_name : String.t option
    ; replication_group_ids : ReplicationGroupIdList.t
    ; cache_cluster_ids : CacheClusterIdList.t
    ; engine : String.t option
    ; service_update_status : ServiceUpdateStatusList.t
    ; service_update_time_range : TimeRangeFilter.t option
    ; update_action_status : UpdateActionStatusList.t
    ; show_node_level_update_status : Boolean.t option
    ; max_records : Integer.t option
    ; marker : String.t option
    }

  let make
      ?service_update_name
      ?(replication_group_ids = [])
      ?(cache_cluster_ids = [])
      ?engine
      ?(service_update_status = [])
      ?service_update_time_range
      ?(update_action_status = [])
      ?show_node_level_update_status
      ?max_records
      ?marker
      () =
    { service_update_name
    ; replication_group_ids
    ; cache_cluster_ids
    ; engine
    ; service_update_status
    ; service_update_time_range
    ; update_action_status
    ; show_node_level_update_status
    ; max_records
    ; marker
    }

  let parse xml =
    Some
      { service_update_name =
          Aws.Util.option_bind (Aws.Xml.member "ServiceUpdateName" xml) String.parse
      ; replication_group_ids =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ReplicationGroupIds" xml)
               ReplicationGroupIdList.parse)
      ; cache_cluster_ids =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "CacheClusterIds" xml)
               CacheClusterIdList.parse)
      ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
      ; service_update_status =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ServiceUpdateStatus" xml)
               ServiceUpdateStatusList.parse)
      ; service_update_time_range =
          Aws.Util.option_bind
            (Aws.Xml.member "ServiceUpdateTimeRange" xml)
            TimeRangeFilter.parse
      ; update_action_status =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "UpdateActionStatus" xml)
               UpdateActionStatusList.parse)
      ; show_node_level_update_status =
          Aws.Util.option_bind
            (Aws.Xml.member "ShowNodeLevelUpdateStatus" xml)
            Boolean.parse
      ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ; Aws.Util.option_map v.max_records (fun f ->
               Aws.Query.Pair ("MaxRecords", Integer.to_query f))
         ; Aws.Util.option_map v.show_node_level_update_status (fun f ->
               Aws.Query.Pair ("ShowNodeLevelUpdateStatus", Boolean.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "UpdateActionStatus.member"
                , UpdateActionStatusList.to_query v.update_action_status ))
         ; Aws.Util.option_map v.service_update_time_range (fun f ->
               Aws.Query.Pair ("ServiceUpdateTimeRange", TimeRangeFilter.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "ServiceUpdateStatus.member"
                , ServiceUpdateStatusList.to_query v.service_update_status ))
         ; Aws.Util.option_map v.engine (fun f ->
               Aws.Query.Pair ("Engine", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("CacheClusterIds.member", CacheClusterIdList.to_query v.cache_cluster_ids))
         ; Some
             (Aws.Query.Pair
                ( "ReplicationGroupIds.member"
                , ReplicationGroupIdList.to_query v.replication_group_ids ))
         ; Aws.Util.option_map v.service_update_name (fun f ->
               Aws.Query.Pair ("ServiceUpdateName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
         ; Aws.Util.option_map v.show_node_level_update_status (fun f ->
               "ShowNodeLevelUpdateStatus", Boolean.to_json f)
         ; Some
             ("UpdateActionStatus", UpdateActionStatusList.to_json v.update_action_status)
         ; Aws.Util.option_map v.service_update_time_range (fun f ->
               "ServiceUpdateTimeRange", TimeRangeFilter.to_json f)
         ; Some
             ( "ServiceUpdateStatus"
             , ServiceUpdateStatusList.to_json v.service_update_status )
         ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
         ; Some ("CacheClusterIds", CacheClusterIdList.to_json v.cache_cluster_ids)
         ; Some
             ( "ReplicationGroupIds"
             , ReplicationGroupIdList.to_json v.replication_group_ids )
         ; Aws.Util.option_map v.service_update_name (fun f ->
               "ServiceUpdateName", String.to_json f)
         ])

  let of_json j =
    { service_update_name =
        Aws.Util.option_map (Aws.Json.lookup j "ServiceUpdateName") String.of_json
    ; replication_group_ids =
        ReplicationGroupIdList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicationGroupIds"))
    ; cache_cluster_ids =
        CacheClusterIdList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheClusterIds"))
    ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
    ; service_update_status =
        ServiceUpdateStatusList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ServiceUpdateStatus"))
    ; service_update_time_range =
        Aws.Util.option_map
          (Aws.Json.lookup j "ServiceUpdateTimeRange")
          TimeRangeFilter.of_json
    ; update_action_status =
        UpdateActionStatusList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "UpdateActionStatus"))
    ; show_node_level_update_status =
        Aws.Util.option_map
          (Aws.Json.lookup j "ShowNodeLevelUpdateStatus")
          Boolean.of_json
    ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
    ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    }
end

module ModifyReplicationGroupShardConfigurationResult = struct
  type t = { replication_group : ReplicationGroup.t option }

  let make ?replication_group () = { replication_group }

  let parse xml =
    Some
      { replication_group =
          Aws.Util.option_bind
            (Aws.Xml.member "ReplicationGroup" xml)
            ReplicationGroup.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.replication_group (fun f ->
               Aws.Query.Pair ("ReplicationGroup", ReplicationGroup.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.replication_group (fun f ->
               "ReplicationGroup", ReplicationGroup.to_json f)
         ])

  let of_json j =
    { replication_group =
        Aws.Util.option_map
          (Aws.Json.lookup j "ReplicationGroup")
          ReplicationGroup.of_json
    }
end

module CreateSnapshotMessage = struct
  type t =
    { replication_group_id : String.t option
    ; cache_cluster_id : String.t option
    ; snapshot_name : String.t
    ; kms_key_id : String.t option
    }

  let make ?replication_group_id ?cache_cluster_id ~snapshot_name ?kms_key_id () =
    { replication_group_id; cache_cluster_id; snapshot_name; kms_key_id }

  let parse xml =
    Some
      { replication_group_id =
          Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse
      ; cache_cluster_id =
          Aws.Util.option_bind (Aws.Xml.member "CacheClusterId" xml) String.parse
      ; snapshot_name =
          Aws.Xml.required
            "SnapshotName"
            (Aws.Util.option_bind (Aws.Xml.member "SnapshotName" xml) String.parse)
      ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.kms_key_id (fun f ->
               Aws.Query.Pair ("KmsKeyId", String.to_query f))
         ; Some (Aws.Query.Pair ("SnapshotName", String.to_query v.snapshot_name))
         ; Aws.Util.option_map v.cache_cluster_id (fun f ->
               Aws.Query.Pair ("CacheClusterId", String.to_query f))
         ; Aws.Util.option_map v.replication_group_id (fun f ->
               Aws.Query.Pair ("ReplicationGroupId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.kms_key_id (fun f -> "KmsKeyId", String.to_json f)
         ; Some ("SnapshotName", String.to_json v.snapshot_name)
         ; Aws.Util.option_map v.cache_cluster_id (fun f ->
               "CacheClusterId", String.to_json f)
         ; Aws.Util.option_map v.replication_group_id (fun f ->
               "ReplicationGroupId", String.to_json f)
         ])

  let of_json j =
    { replication_group_id =
        Aws.Util.option_map (Aws.Json.lookup j "ReplicationGroupId") String.of_json
    ; cache_cluster_id =
        Aws.Util.option_map (Aws.Json.lookup j "CacheClusterId") String.of_json
    ; snapshot_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SnapshotName"))
    ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
    }
end

module SnapshotFeatureNotSupportedFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module CreateCacheClusterResult = struct
  type t = { cache_cluster : CacheCluster.t option }

  let make ?cache_cluster () = { cache_cluster }

  let parse xml =
    Some
      { cache_cluster =
          Aws.Util.option_bind (Aws.Xml.member "CacheCluster" xml) CacheCluster.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cache_cluster (fun f ->
               Aws.Query.Pair ("CacheCluster", CacheCluster.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cache_cluster (fun f ->
               "CacheCluster", CacheCluster.to_json f)
         ])

  let of_json j =
    { cache_cluster =
        Aws.Util.option_map (Aws.Json.lookup j "CacheCluster") CacheCluster.of_json
    }
end

module DefaultUserRequired = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module UserGroupNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module InvalidUserStateFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module UserIdList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module UserGroupPendingChanges = struct
  type t =
    { user_ids_to_remove : UserIdList.t
    ; user_ids_to_add : UserIdList.t
    }

  let make ?(user_ids_to_remove = []) ?(user_ids_to_add = []) () =
    { user_ids_to_remove; user_ids_to_add }

  let parse xml =
    Some
      { user_ids_to_remove =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "UserIdsToRemove" xml) UserIdList.parse)
      ; user_ids_to_add =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "UserIdsToAdd" xml) UserIdList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("UserIdsToAdd.member", UserIdList.to_query v.user_ids_to_add))
         ; Some
             (Aws.Query.Pair
                ("UserIdsToRemove.member", UserIdList.to_query v.user_ids_to_remove))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("UserIdsToAdd", UserIdList.to_json v.user_ids_to_add)
         ; Some ("UserIdsToRemove", UserIdList.to_json v.user_ids_to_remove)
         ])

  let of_json j =
    { user_ids_to_remove =
        UserIdList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UserIdsToRemove"))
    ; user_ids_to_add =
        UserIdList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UserIdsToAdd"))
    }
end

module UGReplicationGroupIdList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module UserGroup = struct
  type t =
    { user_group_id : String.t option
    ; status : String.t option
    ; engine : String.t option
    ; user_ids : UserIdList.t
    ; pending_changes : UserGroupPendingChanges.t option
    ; replication_groups : UGReplicationGroupIdList.t
    ; a_r_n : String.t option
    }

  let make
      ?user_group_id
      ?status
      ?engine
      ?(user_ids = [])
      ?pending_changes
      ?(replication_groups = [])
      ?a_r_n
      () =
    { user_group_id
    ; status
    ; engine
    ; user_ids
    ; pending_changes
    ; replication_groups
    ; a_r_n
    }

  let parse xml =
    Some
      { user_group_id =
          Aws.Util.option_bind (Aws.Xml.member "UserGroupId" xml) String.parse
      ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
      ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
      ; user_ids =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "UserIds" xml) UserIdList.parse)
      ; pending_changes =
          Aws.Util.option_bind
            (Aws.Xml.member "PendingChanges" xml)
            UserGroupPendingChanges.parse
      ; replication_groups =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ReplicationGroups" xml)
               UGReplicationGroupIdList.parse)
      ; a_r_n = Aws.Util.option_bind (Aws.Xml.member "ARN" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.a_r_n (fun f ->
               Aws.Query.Pair ("ARN", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "ReplicationGroups.member"
                , UGReplicationGroupIdList.to_query v.replication_groups ))
         ; Aws.Util.option_map v.pending_changes (fun f ->
               Aws.Query.Pair ("PendingChanges", UserGroupPendingChanges.to_query f))
         ; Some (Aws.Query.Pair ("UserIds.member", UserIdList.to_query v.user_ids))
         ; Aws.Util.option_map v.engine (fun f ->
               Aws.Query.Pair ("Engine", String.to_query f))
         ; Aws.Util.option_map v.status (fun f ->
               Aws.Query.Pair ("Status", String.to_query f))
         ; Aws.Util.option_map v.user_group_id (fun f ->
               Aws.Query.Pair ("UserGroupId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.a_r_n (fun f -> "ARN", String.to_json f)
         ; Some
             ("ReplicationGroups", UGReplicationGroupIdList.to_json v.replication_groups)
         ; Aws.Util.option_map v.pending_changes (fun f ->
               "PendingChanges", UserGroupPendingChanges.to_json f)
         ; Some ("UserIds", UserIdList.to_json v.user_ids)
         ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
         ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
         ; Aws.Util.option_map v.user_group_id (fun f -> "UserGroupId", String.to_json f)
         ])

  let of_json j =
    { user_group_id = Aws.Util.option_map (Aws.Json.lookup j "UserGroupId") String.of_json
    ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
    ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
    ; user_ids = UserIdList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UserIds"))
    ; pending_changes =
        Aws.Util.option_map
          (Aws.Json.lookup j "PendingChanges")
          UserGroupPendingChanges.of_json
    ; replication_groups =
        UGReplicationGroupIdList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicationGroups"))
    ; a_r_n = Aws.Util.option_map (Aws.Json.lookup j "ARN") String.of_json
    }
end

module UserGroupList = struct
  type t = UserGroup.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map UserGroup.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list UserGroup.to_query v

  let to_json v = `List (List.map UserGroup.to_json v)

  let of_json j = Aws.Json.to_list UserGroup.of_json j
end

module DescribeReservedCacheNodesMessage = struct
  type t =
    { reserved_cache_node_id : String.t option
    ; reserved_cache_nodes_offering_id : String.t option
    ; cache_node_type : String.t option
    ; duration : String.t option
    ; product_description : String.t option
    ; offering_type : String.t option
    ; max_records : Integer.t option
    ; marker : String.t option
    }

  let make
      ?reserved_cache_node_id
      ?reserved_cache_nodes_offering_id
      ?cache_node_type
      ?duration
      ?product_description
      ?offering_type
      ?max_records
      ?marker
      () =
    { reserved_cache_node_id
    ; reserved_cache_nodes_offering_id
    ; cache_node_type
    ; duration
    ; product_description
    ; offering_type
    ; max_records
    ; marker
    }

  let parse xml =
    Some
      { reserved_cache_node_id =
          Aws.Util.option_bind (Aws.Xml.member "ReservedCacheNodeId" xml) String.parse
      ; reserved_cache_nodes_offering_id =
          Aws.Util.option_bind
            (Aws.Xml.member "ReservedCacheNodesOfferingId" xml)
            String.parse
      ; cache_node_type =
          Aws.Util.option_bind (Aws.Xml.member "CacheNodeType" xml) String.parse
      ; duration = Aws.Util.option_bind (Aws.Xml.member "Duration" xml) String.parse
      ; product_description =
          Aws.Util.option_bind (Aws.Xml.member "ProductDescription" xml) String.parse
      ; offering_type =
          Aws.Util.option_bind (Aws.Xml.member "OfferingType" xml) String.parse
      ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ; Aws.Util.option_map v.max_records (fun f ->
               Aws.Query.Pair ("MaxRecords", Integer.to_query f))
         ; Aws.Util.option_map v.offering_type (fun f ->
               Aws.Query.Pair ("OfferingType", String.to_query f))
         ; Aws.Util.option_map v.product_description (fun f ->
               Aws.Query.Pair ("ProductDescription", String.to_query f))
         ; Aws.Util.option_map v.duration (fun f ->
               Aws.Query.Pair ("Duration", String.to_query f))
         ; Aws.Util.option_map v.cache_node_type (fun f ->
               Aws.Query.Pair ("CacheNodeType", String.to_query f))
         ; Aws.Util.option_map v.reserved_cache_nodes_offering_id (fun f ->
               Aws.Query.Pair ("ReservedCacheNodesOfferingId", String.to_query f))
         ; Aws.Util.option_map v.reserved_cache_node_id (fun f ->
               Aws.Query.Pair ("ReservedCacheNodeId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
         ; Aws.Util.option_map v.offering_type (fun f -> "OfferingType", String.to_json f)
         ; Aws.Util.option_map v.product_description (fun f ->
               "ProductDescription", String.to_json f)
         ; Aws.Util.option_map v.duration (fun f -> "Duration", String.to_json f)
         ; Aws.Util.option_map v.cache_node_type (fun f ->
               "CacheNodeType", String.to_json f)
         ; Aws.Util.option_map v.reserved_cache_nodes_offering_id (fun f ->
               "ReservedCacheNodesOfferingId", String.to_json f)
         ; Aws.Util.option_map v.reserved_cache_node_id (fun f ->
               "ReservedCacheNodeId", String.to_json f)
         ])

  let of_json j =
    { reserved_cache_node_id =
        Aws.Util.option_map (Aws.Json.lookup j "ReservedCacheNodeId") String.of_json
    ; reserved_cache_nodes_offering_id =
        Aws.Util.option_map
          (Aws.Json.lookup j "ReservedCacheNodesOfferingId")
          String.of_json
    ; cache_node_type =
        Aws.Util.option_map (Aws.Json.lookup j "CacheNodeType") String.of_json
    ; duration = Aws.Util.option_map (Aws.Json.lookup j "Duration") String.of_json
    ; product_description =
        Aws.Util.option_map (Aws.Json.lookup j "ProductDescription") String.of_json
    ; offering_type =
        Aws.Util.option_map (Aws.Json.lookup j "OfferingType") String.of_json
    ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
    ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    }
end

module ServiceUpdate = struct
  type t =
    { service_update_name : String.t option
    ; service_update_release_date : DateTime.t option
    ; service_update_end_date : DateTime.t option
    ; service_update_severity : ServiceUpdateSeverity.t option
    ; service_update_recommended_apply_by_date : DateTime.t option
    ; service_update_status : ServiceUpdateStatus.t option
    ; service_update_description : String.t option
    ; service_update_type : ServiceUpdateType.t option
    ; engine : String.t option
    ; engine_version : String.t option
    ; auto_update_after_recommended_apply_by_date : Boolean.t option
    ; estimated_update_time : String.t option
    }

  let make
      ?service_update_name
      ?service_update_release_date
      ?service_update_end_date
      ?service_update_severity
      ?service_update_recommended_apply_by_date
      ?service_update_status
      ?service_update_description
      ?service_update_type
      ?engine
      ?engine_version
      ?auto_update_after_recommended_apply_by_date
      ?estimated_update_time
      () =
    { service_update_name
    ; service_update_release_date
    ; service_update_end_date
    ; service_update_severity
    ; service_update_recommended_apply_by_date
    ; service_update_status
    ; service_update_description
    ; service_update_type
    ; engine
    ; engine_version
    ; auto_update_after_recommended_apply_by_date
    ; estimated_update_time
    }

  let parse xml =
    Some
      { service_update_name =
          Aws.Util.option_bind (Aws.Xml.member "ServiceUpdateName" xml) String.parse
      ; service_update_release_date =
          Aws.Util.option_bind
            (Aws.Xml.member "ServiceUpdateReleaseDate" xml)
            DateTime.parse
      ; service_update_end_date =
          Aws.Util.option_bind (Aws.Xml.member "ServiceUpdateEndDate" xml) DateTime.parse
      ; service_update_severity =
          Aws.Util.option_bind
            (Aws.Xml.member "ServiceUpdateSeverity" xml)
            ServiceUpdateSeverity.parse
      ; service_update_recommended_apply_by_date =
          Aws.Util.option_bind
            (Aws.Xml.member "ServiceUpdateRecommendedApplyByDate" xml)
            DateTime.parse
      ; service_update_status =
          Aws.Util.option_bind
            (Aws.Xml.member "ServiceUpdateStatus" xml)
            ServiceUpdateStatus.parse
      ; service_update_description =
          Aws.Util.option_bind
            (Aws.Xml.member "ServiceUpdateDescription" xml)
            String.parse
      ; service_update_type =
          Aws.Util.option_bind
            (Aws.Xml.member "ServiceUpdateType" xml)
            ServiceUpdateType.parse
      ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
      ; engine_version =
          Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
      ; auto_update_after_recommended_apply_by_date =
          Aws.Util.option_bind
            (Aws.Xml.member "AutoUpdateAfterRecommendedApplyByDate" xml)
            Boolean.parse
      ; estimated_update_time =
          Aws.Util.option_bind (Aws.Xml.member "EstimatedUpdateTime" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.estimated_update_time (fun f ->
               Aws.Query.Pair ("EstimatedUpdateTime", String.to_query f))
         ; Aws.Util.option_map v.auto_update_after_recommended_apply_by_date (fun f ->
               Aws.Query.Pair ("AutoUpdateAfterRecommendedApplyByDate", Boolean.to_query f))
         ; Aws.Util.option_map v.engine_version (fun f ->
               Aws.Query.Pair ("EngineVersion", String.to_query f))
         ; Aws.Util.option_map v.engine (fun f ->
               Aws.Query.Pair ("Engine", String.to_query f))
         ; Aws.Util.option_map v.service_update_type (fun f ->
               Aws.Query.Pair ("ServiceUpdateType", ServiceUpdateType.to_query f))
         ; Aws.Util.option_map v.service_update_description (fun f ->
               Aws.Query.Pair ("ServiceUpdateDescription", String.to_query f))
         ; Aws.Util.option_map v.service_update_status (fun f ->
               Aws.Query.Pair ("ServiceUpdateStatus", ServiceUpdateStatus.to_query f))
         ; Aws.Util.option_map v.service_update_recommended_apply_by_date (fun f ->
               Aws.Query.Pair ("ServiceUpdateRecommendedApplyByDate", DateTime.to_query f))
         ; Aws.Util.option_map v.service_update_severity (fun f ->
               Aws.Query.Pair ("ServiceUpdateSeverity", ServiceUpdateSeverity.to_query f))
         ; Aws.Util.option_map v.service_update_end_date (fun f ->
               Aws.Query.Pair ("ServiceUpdateEndDate", DateTime.to_query f))
         ; Aws.Util.option_map v.service_update_release_date (fun f ->
               Aws.Query.Pair ("ServiceUpdateReleaseDate", DateTime.to_query f))
         ; Aws.Util.option_map v.service_update_name (fun f ->
               Aws.Query.Pair ("ServiceUpdateName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.estimated_update_time (fun f ->
               "EstimatedUpdateTime", String.to_json f)
         ; Aws.Util.option_map v.auto_update_after_recommended_apply_by_date (fun f ->
               "AutoUpdateAfterRecommendedApplyByDate", Boolean.to_json f)
         ; Aws.Util.option_map v.engine_version (fun f ->
               "EngineVersion", String.to_json f)
         ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
         ; Aws.Util.option_map v.service_update_type (fun f ->
               "ServiceUpdateType", ServiceUpdateType.to_json f)
         ; Aws.Util.option_map v.service_update_description (fun f ->
               "ServiceUpdateDescription", String.to_json f)
         ; Aws.Util.option_map v.service_update_status (fun f ->
               "ServiceUpdateStatus", ServiceUpdateStatus.to_json f)
         ; Aws.Util.option_map v.service_update_recommended_apply_by_date (fun f ->
               "ServiceUpdateRecommendedApplyByDate", DateTime.to_json f)
         ; Aws.Util.option_map v.service_update_severity (fun f ->
               "ServiceUpdateSeverity", ServiceUpdateSeverity.to_json f)
         ; Aws.Util.option_map v.service_update_end_date (fun f ->
               "ServiceUpdateEndDate", DateTime.to_json f)
         ; Aws.Util.option_map v.service_update_release_date (fun f ->
               "ServiceUpdateReleaseDate", DateTime.to_json f)
         ; Aws.Util.option_map v.service_update_name (fun f ->
               "ServiceUpdateName", String.to_json f)
         ])

  let of_json j =
    { service_update_name =
        Aws.Util.option_map (Aws.Json.lookup j "ServiceUpdateName") String.of_json
    ; service_update_release_date =
        Aws.Util.option_map
          (Aws.Json.lookup j "ServiceUpdateReleaseDate")
          DateTime.of_json
    ; service_update_end_date =
        Aws.Util.option_map (Aws.Json.lookup j "ServiceUpdateEndDate") DateTime.of_json
    ; service_update_severity =
        Aws.Util.option_map
          (Aws.Json.lookup j "ServiceUpdateSeverity")
          ServiceUpdateSeverity.of_json
    ; service_update_recommended_apply_by_date =
        Aws.Util.option_map
          (Aws.Json.lookup j "ServiceUpdateRecommendedApplyByDate")
          DateTime.of_json
    ; service_update_status =
        Aws.Util.option_map
          (Aws.Json.lookup j "ServiceUpdateStatus")
          ServiceUpdateStatus.of_json
    ; service_update_description =
        Aws.Util.option_map (Aws.Json.lookup j "ServiceUpdateDescription") String.of_json
    ; service_update_type =
        Aws.Util.option_map
          (Aws.Json.lookup j "ServiceUpdateType")
          ServiceUpdateType.of_json
    ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
    ; engine_version =
        Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
    ; auto_update_after_recommended_apply_by_date =
        Aws.Util.option_map
          (Aws.Json.lookup j "AutoUpdateAfterRecommendedApplyByDate")
          Boolean.of_json
    ; estimated_update_time =
        Aws.Util.option_map (Aws.Json.lookup j "EstimatedUpdateTime") String.of_json
    }
end

module ServiceUpdateList = struct
  type t = ServiceUpdate.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map ServiceUpdate.parse (Aws.Xml.members "ServiceUpdate" xml))

  let to_query v = Aws.Query.to_query_list ServiceUpdate.to_query v

  let to_json v = `List (List.map ServiceUpdate.to_json v)

  let of_json j = Aws.Json.to_list ServiceUpdate.of_json j
end

module ModifyGlobalReplicationGroupResult = struct
  type t = { global_replication_group : GlobalReplicationGroup.t option }

  let make ?global_replication_group () = { global_replication_group }

  let parse xml =
    Some
      { global_replication_group =
          Aws.Util.option_bind
            (Aws.Xml.member "GlobalReplicationGroup" xml)
            GlobalReplicationGroup.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.global_replication_group (fun f ->
               Aws.Query.Pair ("GlobalReplicationGroup", GlobalReplicationGroup.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.global_replication_group (fun f ->
               "GlobalReplicationGroup", GlobalReplicationGroup.to_json f)
         ])

  let of_json j =
    { global_replication_group =
        Aws.Util.option_map
          (Aws.Json.lookup j "GlobalReplicationGroup")
          GlobalReplicationGroup.of_json
    }
end

module APICallRateForCustomerExceededFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ReplicationGroupNotUnderMigrationFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module IncreaseNodeGroupsInGlobalReplicationGroupResult = struct
  type t = { global_replication_group : GlobalReplicationGroup.t option }

  let make ?global_replication_group () = { global_replication_group }

  let parse xml =
    Some
      { global_replication_group =
          Aws.Util.option_bind
            (Aws.Xml.member "GlobalReplicationGroup" xml)
            GlobalReplicationGroup.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.global_replication_group (fun f ->
               Aws.Query.Pair ("GlobalReplicationGroup", GlobalReplicationGroup.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.global_replication_group (fun f ->
               "GlobalReplicationGroup", GlobalReplicationGroup.to_json f)
         ])

  let of_json j =
    { global_replication_group =
        Aws.Util.option_map
          (Aws.Json.lookup j "GlobalReplicationGroup")
          GlobalReplicationGroup.of_json
    }
end

module CacheParameterGroup = struct
  type t =
    { cache_parameter_group_name : String.t option
    ; cache_parameter_group_family : String.t option
    ; description : String.t option
    ; is_global : Boolean.t option
    ; a_r_n : String.t option
    }

  let make
      ?cache_parameter_group_name
      ?cache_parameter_group_family
      ?description
      ?is_global
      ?a_r_n
      () =
    { cache_parameter_group_name
    ; cache_parameter_group_family
    ; description
    ; is_global
    ; a_r_n
    }

  let parse xml =
    Some
      { cache_parameter_group_name =
          Aws.Util.option_bind (Aws.Xml.member "CacheParameterGroupName" xml) String.parse
      ; cache_parameter_group_family =
          Aws.Util.option_bind
            (Aws.Xml.member "CacheParameterGroupFamily" xml)
            String.parse
      ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
      ; is_global = Aws.Util.option_bind (Aws.Xml.member "IsGlobal" xml) Boolean.parse
      ; a_r_n = Aws.Util.option_bind (Aws.Xml.member "ARN" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.a_r_n (fun f ->
               Aws.Query.Pair ("ARN", String.to_query f))
         ; Aws.Util.option_map v.is_global (fun f ->
               Aws.Query.Pair ("IsGlobal", Boolean.to_query f))
         ; Aws.Util.option_map v.description (fun f ->
               Aws.Query.Pair ("Description", String.to_query f))
         ; Aws.Util.option_map v.cache_parameter_group_family (fun f ->
               Aws.Query.Pair ("CacheParameterGroupFamily", String.to_query f))
         ; Aws.Util.option_map v.cache_parameter_group_name (fun f ->
               Aws.Query.Pair ("CacheParameterGroupName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.a_r_n (fun f -> "ARN", String.to_json f)
         ; Aws.Util.option_map v.is_global (fun f -> "IsGlobal", Boolean.to_json f)
         ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
         ; Aws.Util.option_map v.cache_parameter_group_family (fun f ->
               "CacheParameterGroupFamily", String.to_json f)
         ; Aws.Util.option_map v.cache_parameter_group_name (fun f ->
               "CacheParameterGroupName", String.to_json f)
         ])

  let of_json j =
    { cache_parameter_group_name =
        Aws.Util.option_map (Aws.Json.lookup j "CacheParameterGroupName") String.of_json
    ; cache_parameter_group_family =
        Aws.Util.option_map (Aws.Json.lookup j "CacheParameterGroupFamily") String.of_json
    ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
    ; is_global = Aws.Util.option_map (Aws.Json.lookup j "IsGlobal") Boolean.of_json
    ; a_r_n = Aws.Util.option_map (Aws.Json.lookup j "ARN") String.of_json
    }
end

module CreateCacheParameterGroupResult = struct
  type t = { cache_parameter_group : CacheParameterGroup.t option }

  let make ?cache_parameter_group () = { cache_parameter_group }

  let parse xml =
    Some
      { cache_parameter_group =
          Aws.Util.option_bind
            (Aws.Xml.member "CacheParameterGroup" xml)
            CacheParameterGroup.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cache_parameter_group (fun f ->
               Aws.Query.Pair ("CacheParameterGroup", CacheParameterGroup.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cache_parameter_group (fun f ->
               "CacheParameterGroup", CacheParameterGroup.to_json f)
         ])

  let of_json j =
    { cache_parameter_group =
        Aws.Util.option_map
          (Aws.Json.lookup j "CacheParameterGroup")
          CacheParameterGroup.of_json
    }
end

module BatchApplyUpdateActionMessage = struct
  type t =
    { replication_group_ids : ReplicationGroupIdList.t
    ; cache_cluster_ids : CacheClusterIdList.t
    ; service_update_name : String.t
    }

  let make ?(replication_group_ids = []) ?(cache_cluster_ids = []) ~service_update_name ()
      =
    { replication_group_ids; cache_cluster_ids; service_update_name }

  let parse xml =
    Some
      { replication_group_ids =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ReplicationGroupIds" xml)
               ReplicationGroupIdList.parse)
      ; cache_cluster_ids =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "CacheClusterIds" xml)
               CacheClusterIdList.parse)
      ; service_update_name =
          Aws.Xml.required
            "ServiceUpdateName"
            (Aws.Util.option_bind (Aws.Xml.member "ServiceUpdateName" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("ServiceUpdateName", String.to_query v.service_update_name))
         ; Some
             (Aws.Query.Pair
                ("CacheClusterIds.member", CacheClusterIdList.to_query v.cache_cluster_ids))
         ; Some
             (Aws.Query.Pair
                ( "ReplicationGroupIds.member"
                , ReplicationGroupIdList.to_query v.replication_group_ids ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("ServiceUpdateName", String.to_json v.service_update_name)
         ; Some ("CacheClusterIds", CacheClusterIdList.to_json v.cache_cluster_ids)
         ; Some
             ( "ReplicationGroupIds"
             , ReplicationGroupIdList.to_json v.replication_group_ids )
         ])

  let of_json j =
    { replication_group_ids =
        ReplicationGroupIdList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicationGroupIds"))
    ; cache_cluster_ids =
        CacheClusterIdList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheClusterIds"))
    ; service_update_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ServiceUpdateName"))
    }
end

module UserGroupIdListInput = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module NodeGroupsToRetainList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "NodeGroupToRetain" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module ReshardingConfiguration = struct
  type t =
    { node_group_id : String.t option
    ; preferred_availability_zones : AvailabilityZonesList.t
    }

  let make ?node_group_id ?(preferred_availability_zones = []) () =
    { node_group_id; preferred_availability_zones }

  let parse xml =
    Some
      { node_group_id =
          Aws.Util.option_bind (Aws.Xml.member "NodeGroupId" xml) String.parse
      ; preferred_availability_zones =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "PreferredAvailabilityZones" xml)
               AvailabilityZonesList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "PreferredAvailabilityZones.member"
                , AvailabilityZonesList.to_query v.preferred_availability_zones ))
         ; Aws.Util.option_map v.node_group_id (fun f ->
               Aws.Query.Pair ("NodeGroupId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "PreferredAvailabilityZones"
             , AvailabilityZonesList.to_json v.preferred_availability_zones )
         ; Aws.Util.option_map v.node_group_id (fun f -> "NodeGroupId", String.to_json f)
         ])

  let of_json j =
    { node_group_id = Aws.Util.option_map (Aws.Json.lookup j "NodeGroupId") String.of_json
    ; preferred_availability_zones =
        AvailabilityZonesList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "PreferredAvailabilityZones"))
    }
end

module ReshardingConfigurationList = struct
  type t = ReshardingConfiguration.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map
         ReshardingConfiguration.parse
         (Aws.Xml.members "ReshardingConfiguration" xml))

  let to_query v = Aws.Query.to_query_list ReshardingConfiguration.to_query v

  let to_json v = `List (List.map ReshardingConfiguration.to_json v)

  let of_json j = Aws.Json.to_list ReshardingConfiguration.of_json j
end

module NodeGroupsToRemoveList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "NodeGroupToRemove" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module ModifyReplicationGroupShardConfigurationMessage = struct
  type t =
    { replication_group_id : String.t
    ; node_group_count : Integer.t
    ; apply_immediately : Boolean.t
    ; resharding_configuration : ReshardingConfigurationList.t
    ; node_groups_to_remove : NodeGroupsToRemoveList.t
    ; node_groups_to_retain : NodeGroupsToRetainList.t
    }

  let make
      ~replication_group_id
      ~node_group_count
      ~apply_immediately
      ?(resharding_configuration = [])
      ?(node_groups_to_remove = [])
      ?(node_groups_to_retain = [])
      () =
    { replication_group_id
    ; node_group_count
    ; apply_immediately
    ; resharding_configuration
    ; node_groups_to_remove
    ; node_groups_to_retain
    }

  let parse xml =
    Some
      { replication_group_id =
          Aws.Xml.required
            "ReplicationGroupId"
            (Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse)
      ; node_group_count =
          Aws.Xml.required
            "NodeGroupCount"
            (Aws.Util.option_bind (Aws.Xml.member "NodeGroupCount" xml) Integer.parse)
      ; apply_immediately =
          Aws.Xml.required
            "ApplyImmediately"
            (Aws.Util.option_bind (Aws.Xml.member "ApplyImmediately" xml) Boolean.parse)
      ; resharding_configuration =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ReshardingConfiguration" xml)
               ReshardingConfigurationList.parse)
      ; node_groups_to_remove =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "NodeGroupsToRemove" xml)
               NodeGroupsToRemoveList.parse)
      ; node_groups_to_retain =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "NodeGroupsToRetain" xml)
               NodeGroupsToRetainList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "NodeGroupsToRetain.member"
                , NodeGroupsToRetainList.to_query v.node_groups_to_retain ))
         ; Some
             (Aws.Query.Pair
                ( "NodeGroupsToRemove.member"
                , NodeGroupsToRemoveList.to_query v.node_groups_to_remove ))
         ; Some
             (Aws.Query.Pair
                ( "ReshardingConfiguration.member"
                , ReshardingConfigurationList.to_query v.resharding_configuration ))
         ; Some
             (Aws.Query.Pair ("ApplyImmediately", Boolean.to_query v.apply_immediately))
         ; Some (Aws.Query.Pair ("NodeGroupCount", Integer.to_query v.node_group_count))
         ; Some
             (Aws.Query.Pair ("ReplicationGroupId", String.to_query v.replication_group_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ("NodeGroupsToRetain", NodeGroupsToRetainList.to_json v.node_groups_to_retain)
         ; Some
             ("NodeGroupsToRemove", NodeGroupsToRemoveList.to_json v.node_groups_to_remove)
         ; Some
             ( "ReshardingConfiguration"
             , ReshardingConfigurationList.to_json v.resharding_configuration )
         ; Some ("ApplyImmediately", Boolean.to_json v.apply_immediately)
         ; Some ("NodeGroupCount", Integer.to_json v.node_group_count)
         ; Some ("ReplicationGroupId", String.to_json v.replication_group_id)
         ])

  let of_json j =
    { replication_group_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicationGroupId"))
    ; node_group_count =
        Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "NodeGroupCount"))
    ; apply_immediately =
        Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ApplyImmediately"))
    ; resharding_configuration =
        ReshardingConfigurationList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ReshardingConfiguration"))
    ; node_groups_to_remove =
        NodeGroupsToRemoveList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "NodeGroupsToRemove"))
    ; node_groups_to_retain =
        NodeGroupsToRetainList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "NodeGroupsToRetain"))
    }
end

module SnapshotQuotaExceededFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module GlobalNodeGroupIdList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "GlobalNodeGroupId" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module UserNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module DescribeUserGroupsResult = struct
  type t =
    { user_groups : UserGroupList.t
    ; marker : String.t option
    }

  let make ?(user_groups = []) ?marker () = { user_groups; marker }

  let parse xml =
    Some
      { user_groups =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "UserGroups" xml) UserGroupList.parse)
      ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ; Some
             (Aws.Query.Pair ("UserGroups.member", UserGroupList.to_query v.user_groups))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ; Some ("UserGroups", UserGroupList.to_json v.user_groups)
         ])

  let of_json j =
    { user_groups =
        UserGroupList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UserGroups"))
    ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    }
end

module EC2SecurityGroup = struct
  type t =
    { status : String.t option
    ; e_c2_security_group_name : String.t option
    ; e_c2_security_group_owner_id : String.t option
    }

  let make ?status ?e_c2_security_group_name ?e_c2_security_group_owner_id () =
    { status; e_c2_security_group_name; e_c2_security_group_owner_id }

  let parse xml =
    Some
      { status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
      ; e_c2_security_group_name =
          Aws.Util.option_bind (Aws.Xml.member "EC2SecurityGroupName" xml) String.parse
      ; e_c2_security_group_owner_id =
          Aws.Util.option_bind (Aws.Xml.member "EC2SecurityGroupOwnerId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.e_c2_security_group_owner_id (fun f ->
               Aws.Query.Pair ("EC2SecurityGroupOwnerId", String.to_query f))
         ; Aws.Util.option_map v.e_c2_security_group_name (fun f ->
               Aws.Query.Pair ("EC2SecurityGroupName", String.to_query f))
         ; Aws.Util.option_map v.status (fun f ->
               Aws.Query.Pair ("Status", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.e_c2_security_group_owner_id (fun f ->
               "EC2SecurityGroupOwnerId", String.to_json f)
         ; Aws.Util.option_map v.e_c2_security_group_name (fun f ->
               "EC2SecurityGroupName", String.to_json f)
         ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
         ])

  let of_json j =
    { status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
    ; e_c2_security_group_name =
        Aws.Util.option_map (Aws.Json.lookup j "EC2SecurityGroupName") String.of_json
    ; e_c2_security_group_owner_id =
        Aws.Util.option_map (Aws.Json.lookup j "EC2SecurityGroupOwnerId") String.of_json
    }
end

module EC2SecurityGroupList = struct
  type t = EC2SecurityGroup.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map EC2SecurityGroup.parse (Aws.Xml.members "EC2SecurityGroup" xml))

  let to_query v = Aws.Query.to_query_list EC2SecurityGroup.to_query v

  let to_json v = `List (List.map EC2SecurityGroup.to_json v)

  let of_json j = Aws.Json.to_list EC2SecurityGroup.of_json j
end

module ReservedCacheNodeQuotaExceededFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module RemoveReplicasList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module RebootCacheClusterMessage = struct
  type t =
    { cache_cluster_id : String.t
    ; cache_node_ids_to_reboot : CacheNodeIdsList.t
    }

  let make ~cache_cluster_id ~cache_node_ids_to_reboot () =
    { cache_cluster_id; cache_node_ids_to_reboot }

  let parse xml =
    Some
      { cache_cluster_id =
          Aws.Xml.required
            "CacheClusterId"
            (Aws.Util.option_bind (Aws.Xml.member "CacheClusterId" xml) String.parse)
      ; cache_node_ids_to_reboot =
          Aws.Xml.required
            "CacheNodeIdsToReboot"
            (Aws.Util.option_bind
               (Aws.Xml.member "CacheNodeIdsToReboot" xml)
               CacheNodeIdsList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "CacheNodeIdsToReboot.member"
                , CacheNodeIdsList.to_query v.cache_node_ids_to_reboot ))
         ; Some (Aws.Query.Pair ("CacheClusterId", String.to_query v.cache_cluster_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ("CacheNodeIdsToReboot", CacheNodeIdsList.to_json v.cache_node_ids_to_reboot)
         ; Some ("CacheClusterId", String.to_json v.cache_cluster_id)
         ])

  let of_json j =
    { cache_cluster_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheClusterId"))
    ; cache_node_ids_to_reboot =
        CacheNodeIdsList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheNodeIdsToReboot"))
    }
end

module CacheSubnetGroupAlreadyExistsFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module CacheClusterMessage = struct
  type t =
    { marker : String.t option
    ; cache_clusters : CacheClusterList.t
    }

  let make ?marker ?(cache_clusters = []) () = { marker; cache_clusters }

  let parse xml =
    Some
      { marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      ; cache_clusters =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "CacheClusters" xml)
               CacheClusterList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("CacheClusters.member", CacheClusterList.to_query v.cache_clusters))
         ; Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("CacheClusters", CacheClusterList.to_json v.cache_clusters)
         ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ])

  let of_json j =
    { marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    ; cache_clusters =
        CacheClusterList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheClusters"))
    }
end

module InvalidCacheClusterStateFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module SnapshotAlreadyExistsFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ServiceLinkedRoleNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module RevokeCacheSecurityGroupIngressMessage = struct
  type t =
    { cache_security_group_name : String.t
    ; e_c2_security_group_name : String.t
    ; e_c2_security_group_owner_id : String.t
    }

  let make
      ~cache_security_group_name
      ~e_c2_security_group_name
      ~e_c2_security_group_owner_id
      () =
    { cache_security_group_name; e_c2_security_group_name; e_c2_security_group_owner_id }

  let parse xml =
    Some
      { cache_security_group_name =
          Aws.Xml.required
            "CacheSecurityGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "CacheSecurityGroupName" xml)
               String.parse)
      ; e_c2_security_group_name =
          Aws.Xml.required
            "EC2SecurityGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "EC2SecurityGroupName" xml)
               String.parse)
      ; e_c2_security_group_owner_id =
          Aws.Xml.required
            "EC2SecurityGroupOwnerId"
            (Aws.Util.option_bind
               (Aws.Xml.member "EC2SecurityGroupOwnerId" xml)
               String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("EC2SecurityGroupOwnerId", String.to_query v.e_c2_security_group_owner_id))
         ; Some
             (Aws.Query.Pair
                ("EC2SecurityGroupName", String.to_query v.e_c2_security_group_name))
         ; Some
             (Aws.Query.Pair
                ("CacheSecurityGroupName", String.to_query v.cache_security_group_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("EC2SecurityGroupOwnerId", String.to_json v.e_c2_security_group_owner_id)
         ; Some ("EC2SecurityGroupName", String.to_json v.e_c2_security_group_name)
         ; Some ("CacheSecurityGroupName", String.to_json v.cache_security_group_name)
         ])

  let of_json j =
    { cache_security_group_name =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheSecurityGroupName"))
    ; e_c2_security_group_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EC2SecurityGroupName"))
    ; e_c2_security_group_owner_id =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "EC2SecurityGroupOwnerId"))
    }
end

module CacheSecurityGroup = struct
  type t =
    { owner_id : String.t option
    ; cache_security_group_name : String.t option
    ; description : String.t option
    ; e_c2_security_groups : EC2SecurityGroupList.t
    ; a_r_n : String.t option
    }

  let make
      ?owner_id
      ?cache_security_group_name
      ?description
      ?(e_c2_security_groups = [])
      ?a_r_n
      () =
    { owner_id; cache_security_group_name; description; e_c2_security_groups; a_r_n }

  let parse xml =
    Some
      { owner_id = Aws.Util.option_bind (Aws.Xml.member "OwnerId" xml) String.parse
      ; cache_security_group_name =
          Aws.Util.option_bind (Aws.Xml.member "CacheSecurityGroupName" xml) String.parse
      ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
      ; e_c2_security_groups =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "EC2SecurityGroups" xml)
               EC2SecurityGroupList.parse)
      ; a_r_n = Aws.Util.option_bind (Aws.Xml.member "ARN" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.a_r_n (fun f ->
               Aws.Query.Pair ("ARN", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "EC2SecurityGroups.member"
                , EC2SecurityGroupList.to_query v.e_c2_security_groups ))
         ; Aws.Util.option_map v.description (fun f ->
               Aws.Query.Pair ("Description", String.to_query f))
         ; Aws.Util.option_map v.cache_security_group_name (fun f ->
               Aws.Query.Pair ("CacheSecurityGroupName", String.to_query f))
         ; Aws.Util.option_map v.owner_id (fun f ->
               Aws.Query.Pair ("OwnerId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.a_r_n (fun f -> "ARN", String.to_json f)
         ; Some ("EC2SecurityGroups", EC2SecurityGroupList.to_json v.e_c2_security_groups)
         ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
         ; Aws.Util.option_map v.cache_security_group_name (fun f ->
               "CacheSecurityGroupName", String.to_json f)
         ; Aws.Util.option_map v.owner_id (fun f -> "OwnerId", String.to_json f)
         ])

  let of_json j =
    { owner_id = Aws.Util.option_map (Aws.Json.lookup j "OwnerId") String.of_json
    ; cache_security_group_name =
        Aws.Util.option_map (Aws.Json.lookup j "CacheSecurityGroupName") String.of_json
    ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
    ; e_c2_security_groups =
        EC2SecurityGroupList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "EC2SecurityGroups"))
    ; a_r_n = Aws.Util.option_map (Aws.Json.lookup j "ARN") String.of_json
    }
end

module DescribeEventsMessage = struct
  type t =
    { source_identifier : String.t option
    ; source_type : SourceType.t option
    ; start_time : DateTime.t option
    ; end_time : DateTime.t option
    ; duration : Integer.t option
    ; max_records : Integer.t option
    ; marker : String.t option
    }

  let make
      ?source_identifier
      ?source_type
      ?start_time
      ?end_time
      ?duration
      ?max_records
      ?marker
      () =
    { source_identifier
    ; source_type
    ; start_time
    ; end_time
    ; duration
    ; max_records
    ; marker
    }

  let parse xml =
    Some
      { source_identifier =
          Aws.Util.option_bind (Aws.Xml.member "SourceIdentifier" xml) String.parse
      ; source_type =
          Aws.Util.option_bind (Aws.Xml.member "SourceType" xml) SourceType.parse
      ; start_time = Aws.Util.option_bind (Aws.Xml.member "StartTime" xml) DateTime.parse
      ; end_time = Aws.Util.option_bind (Aws.Xml.member "EndTime" xml) DateTime.parse
      ; duration = Aws.Util.option_bind (Aws.Xml.member "Duration" xml) Integer.parse
      ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ; Aws.Util.option_map v.max_records (fun f ->
               Aws.Query.Pair ("MaxRecords", Integer.to_query f))
         ; Aws.Util.option_map v.duration (fun f ->
               Aws.Query.Pair ("Duration", Integer.to_query f))
         ; Aws.Util.option_map v.end_time (fun f ->
               Aws.Query.Pair ("EndTime", DateTime.to_query f))
         ; Aws.Util.option_map v.start_time (fun f ->
               Aws.Query.Pair ("StartTime", DateTime.to_query f))
         ; Aws.Util.option_map v.source_type (fun f ->
               Aws.Query.Pair ("SourceType", SourceType.to_query f))
         ; Aws.Util.option_map v.source_identifier (fun f ->
               Aws.Query.Pair ("SourceIdentifier", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
         ; Aws.Util.option_map v.duration (fun f -> "Duration", Integer.to_json f)
         ; Aws.Util.option_map v.end_time (fun f -> "EndTime", DateTime.to_json f)
         ; Aws.Util.option_map v.start_time (fun f -> "StartTime", DateTime.to_json f)
         ; Aws.Util.option_map v.source_type (fun f -> "SourceType", SourceType.to_json f)
         ; Aws.Util.option_map v.source_identifier (fun f ->
               "SourceIdentifier", String.to_json f)
         ])

  let of_json j =
    { source_identifier =
        Aws.Util.option_map (Aws.Json.lookup j "SourceIdentifier") String.of_json
    ; source_type =
        Aws.Util.option_map (Aws.Json.lookup j "SourceType") SourceType.of_json
    ; start_time = Aws.Util.option_map (Aws.Json.lookup j "StartTime") DateTime.of_json
    ; end_time = Aws.Util.option_map (Aws.Json.lookup j "EndTime") DateTime.of_json
    ; duration = Aws.Util.option_map (Aws.Json.lookup j "Duration") Integer.of_json
    ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
    ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    }
end

module CreateGlobalReplicationGroupResult = struct
  type t = { global_replication_group : GlobalReplicationGroup.t option }

  let make ?global_replication_group () = { global_replication_group }

  let parse xml =
    Some
      { global_replication_group =
          Aws.Util.option_bind
            (Aws.Xml.member "GlobalReplicationGroup" xml)
            GlobalReplicationGroup.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.global_replication_group (fun f ->
               Aws.Query.Pair ("GlobalReplicationGroup", GlobalReplicationGroup.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.global_replication_group (fun f ->
               "GlobalReplicationGroup", GlobalReplicationGroup.to_json f)
         ])

  let of_json j =
    { global_replication_group =
        Aws.Util.option_map
          (Aws.Json.lookup j "GlobalReplicationGroup")
          GlobalReplicationGroup.of_json
    }
end

module AuthTokenUpdateStrategyType = struct
  type t =
    | SET
    | ROTATE
    | DELETE

  let str_to_t = [ "DELETE", DELETE; "ROTATE", ROTATE; "SET", SET ]

  let t_to_str = [ DELETE, "DELETE"; ROTATE, "ROTATE"; SET, "SET" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module ResetCacheParameterGroupMessage = struct
  type t =
    { cache_parameter_group_name : String.t
    ; reset_all_parameters : Boolean.t option
    ; parameter_name_values : ParameterNameValueList.t
    }

  let make
      ~cache_parameter_group_name
      ?reset_all_parameters
      ?(parameter_name_values = [])
      () =
    { cache_parameter_group_name; reset_all_parameters; parameter_name_values }

  let parse xml =
    Some
      { cache_parameter_group_name =
          Aws.Xml.required
            "CacheParameterGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "CacheParameterGroupName" xml)
               String.parse)
      ; reset_all_parameters =
          Aws.Util.option_bind (Aws.Xml.member "ResetAllParameters" xml) Boolean.parse
      ; parameter_name_values =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ParameterNameValues" xml)
               ParameterNameValueList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "ParameterNameValues.member"
                , ParameterNameValueList.to_query v.parameter_name_values ))
         ; Aws.Util.option_map v.reset_all_parameters (fun f ->
               Aws.Query.Pair ("ResetAllParameters", Boolean.to_query f))
         ; Some
             (Aws.Query.Pair
                ("CacheParameterGroupName", String.to_query v.cache_parameter_group_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "ParameterNameValues"
             , ParameterNameValueList.to_json v.parameter_name_values )
         ; Aws.Util.option_map v.reset_all_parameters (fun f ->
               "ResetAllParameters", Boolean.to_json f)
         ; Some ("CacheParameterGroupName", String.to_json v.cache_parameter_group_name)
         ])

  let of_json j =
    { cache_parameter_group_name =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheParameterGroupName"))
    ; reset_all_parameters =
        Aws.Util.option_map (Aws.Json.lookup j "ResetAllParameters") Boolean.of_json
    ; parameter_name_values =
        ParameterNameValueList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ParameterNameValues"))
    }
end

module RegionalConfiguration = struct
  type t =
    { replication_group_id : String.t
    ; replication_group_region : String.t
    ; resharding_configuration : ReshardingConfigurationList.t
    }

  let make ~replication_group_id ~replication_group_region ~resharding_configuration () =
    { replication_group_id; replication_group_region; resharding_configuration }

  let parse xml =
    Some
      { replication_group_id =
          Aws.Xml.required
            "ReplicationGroupId"
            (Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse)
      ; replication_group_region =
          Aws.Xml.required
            "ReplicationGroupRegion"
            (Aws.Util.option_bind
               (Aws.Xml.member "ReplicationGroupRegion" xml)
               String.parse)
      ; resharding_configuration =
          Aws.Xml.required
            "ReshardingConfiguration"
            (Aws.Util.option_bind
               (Aws.Xml.member "ReshardingConfiguration" xml)
               ReshardingConfigurationList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "ReshardingConfiguration.member"
                , ReshardingConfigurationList.to_query v.resharding_configuration ))
         ; Some
             (Aws.Query.Pair
                ("ReplicationGroupRegion", String.to_query v.replication_group_region))
         ; Some
             (Aws.Query.Pair ("ReplicationGroupId", String.to_query v.replication_group_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "ReshardingConfiguration"
             , ReshardingConfigurationList.to_json v.resharding_configuration )
         ; Some ("ReplicationGroupRegion", String.to_json v.replication_group_region)
         ; Some ("ReplicationGroupId", String.to_json v.replication_group_id)
         ])

  let of_json j =
    { replication_group_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicationGroupId"))
    ; replication_group_region =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicationGroupRegion"))
    ; resharding_configuration =
        ReshardingConfigurationList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ReshardingConfiguration"))
    }
end

module DecreaseReplicaCountMessage = struct
  type t =
    { replication_group_id : String.t
    ; new_replica_count : Integer.t option
    ; replica_configuration : ReplicaConfigurationList.t
    ; replicas_to_remove : RemoveReplicasList.t
    ; apply_immediately : Boolean.t
    }

  let make
      ~replication_group_id
      ?new_replica_count
      ?(replica_configuration = [])
      ?(replicas_to_remove = [])
      ~apply_immediately
      () =
    { replication_group_id
    ; new_replica_count
    ; replica_configuration
    ; replicas_to_remove
    ; apply_immediately
    }

  let parse xml =
    Some
      { replication_group_id =
          Aws.Xml.required
            "ReplicationGroupId"
            (Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse)
      ; new_replica_count =
          Aws.Util.option_bind (Aws.Xml.member "NewReplicaCount" xml) Integer.parse
      ; replica_configuration =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ReplicaConfiguration" xml)
               ReplicaConfigurationList.parse)
      ; replicas_to_remove =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ReplicasToRemove" xml)
               RemoveReplicasList.parse)
      ; apply_immediately =
          Aws.Xml.required
            "ApplyImmediately"
            (Aws.Util.option_bind (Aws.Xml.member "ApplyImmediately" xml) Boolean.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("ApplyImmediately", Boolean.to_query v.apply_immediately))
         ; Some
             (Aws.Query.Pair
                ( "ReplicasToRemove.member"
                , RemoveReplicasList.to_query v.replicas_to_remove ))
         ; Some
             (Aws.Query.Pair
                ( "ReplicaConfiguration.member"
                , ReplicaConfigurationList.to_query v.replica_configuration ))
         ; Aws.Util.option_map v.new_replica_count (fun f ->
               Aws.Query.Pair ("NewReplicaCount", Integer.to_query f))
         ; Some
             (Aws.Query.Pair ("ReplicationGroupId", String.to_query v.replication_group_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("ApplyImmediately", Boolean.to_json v.apply_immediately)
         ; Some ("ReplicasToRemove", RemoveReplicasList.to_json v.replicas_to_remove)
         ; Some
             ( "ReplicaConfiguration"
             , ReplicaConfigurationList.to_json v.replica_configuration )
         ; Aws.Util.option_map v.new_replica_count (fun f ->
               "NewReplicaCount", Integer.to_json f)
         ; Some ("ReplicationGroupId", String.to_json v.replication_group_id)
         ])

  let of_json j =
    { replication_group_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicationGroupId"))
    ; new_replica_count =
        Aws.Util.option_map (Aws.Json.lookup j "NewReplicaCount") Integer.of_json
    ; replica_configuration =
        ReplicaConfigurationList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicaConfiguration"))
    ; replicas_to_remove =
        RemoveReplicasList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicasToRemove"))
    ; apply_immediately =
        Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ApplyImmediately"))
    }
end

module TestFailoverNotAvailableFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module FailoverGlobalReplicationGroupResult = struct
  type t = { global_replication_group : GlobalReplicationGroup.t option }

  let make ?global_replication_group () = { global_replication_group }

  let parse xml =
    Some
      { global_replication_group =
          Aws.Util.option_bind
            (Aws.Xml.member "GlobalReplicationGroup" xml)
            GlobalReplicationGroup.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.global_replication_group (fun f ->
               Aws.Query.Pair ("GlobalReplicationGroup", GlobalReplicationGroup.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.global_replication_group (fun f ->
               "GlobalReplicationGroup", GlobalReplicationGroup.to_json f)
         ])

  let of_json j =
    { global_replication_group =
        Aws.Util.option_map
          (Aws.Json.lookup j "GlobalReplicationGroup")
          GlobalReplicationGroup.of_json
    }
end

module DeleteUserGroupMessage = struct
  type t = { user_group_id : String.t }

  let make ~user_group_id () = { user_group_id }

  let parse xml =
    Some
      { user_group_id =
          Aws.Xml.required
            "UserGroupId"
            (Aws.Util.option_bind (Aws.Xml.member "UserGroupId" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("UserGroupId", String.to_query v.user_group_id)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt [ Some ("UserGroupId", String.to_json v.user_group_id) ])

  let of_json j =
    { user_group_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UserGroupId"))
    }
end

module CustomerNodeEndpoint = struct
  type t =
    { address : String.t option
    ; port : Integer.t option
    }

  let make ?address ?port () = { address; port }

  let parse xml =
    Some
      { address = Aws.Util.option_bind (Aws.Xml.member "Address" xml) String.parse
      ; port = Aws.Util.option_bind (Aws.Xml.member "Port" xml) Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.port (fun f ->
               Aws.Query.Pair ("Port", Integer.to_query f))
         ; Aws.Util.option_map v.address (fun f ->
               Aws.Query.Pair ("Address", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.port (fun f -> "Port", Integer.to_json f)
         ; Aws.Util.option_map v.address (fun f -> "Address", String.to_json f)
         ])

  let of_json j =
    { address = Aws.Util.option_map (Aws.Json.lookup j "Address") String.of_json
    ; port = Aws.Util.option_map (Aws.Json.lookup j "Port") Integer.of_json
    }
end

module CustomerNodeEndpointList = struct
  type t = CustomerNodeEndpoint.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map CustomerNodeEndpoint.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list CustomerNodeEndpoint.to_query v

  let to_json v = `List (List.map CustomerNodeEndpoint.to_json v)

  let of_json j = Aws.Json.to_list CustomerNodeEndpoint.of_json j
end

module CopySnapshotResult = struct
  type t = { snapshot : Snapshot.t option }

  let make ?snapshot () = { snapshot }

  let parse xml =
    Some
      { snapshot = Aws.Util.option_bind (Aws.Xml.member "Snapshot" xml) Snapshot.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.snapshot (fun f ->
               Aws.Query.Pair ("Snapshot", Snapshot.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.snapshot (fun f -> "Snapshot", Snapshot.to_json f) ])

  let of_json j =
    { snapshot = Aws.Util.option_map (Aws.Json.lookup j "Snapshot") Snapshot.of_json }
end

module TagNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ClusterQuotaForCustomerExceededFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module DeleteReplicationGroupMessage = struct
  type t =
    { replication_group_id : String.t
    ; retain_primary_cluster : Boolean.t option
    ; final_snapshot_identifier : String.t option
    }

  let make ~replication_group_id ?retain_primary_cluster ?final_snapshot_identifier () =
    { replication_group_id; retain_primary_cluster; final_snapshot_identifier }

  let parse xml =
    Some
      { replication_group_id =
          Aws.Xml.required
            "ReplicationGroupId"
            (Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse)
      ; retain_primary_cluster =
          Aws.Util.option_bind (Aws.Xml.member "RetainPrimaryCluster" xml) Boolean.parse
      ; final_snapshot_identifier =
          Aws.Util.option_bind (Aws.Xml.member "FinalSnapshotIdentifier" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.final_snapshot_identifier (fun f ->
               Aws.Query.Pair ("FinalSnapshotIdentifier", String.to_query f))
         ; Aws.Util.option_map v.retain_primary_cluster (fun f ->
               Aws.Query.Pair ("RetainPrimaryCluster", Boolean.to_query f))
         ; Some
             (Aws.Query.Pair ("ReplicationGroupId", String.to_query v.replication_group_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.final_snapshot_identifier (fun f ->
               "FinalSnapshotIdentifier", String.to_json f)
         ; Aws.Util.option_map v.retain_primary_cluster (fun f ->
               "RetainPrimaryCluster", Boolean.to_json f)
         ; Some ("ReplicationGroupId", String.to_json v.replication_group_id)
         ])

  let of_json j =
    { replication_group_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicationGroupId"))
    ; retain_primary_cluster =
        Aws.Util.option_map (Aws.Json.lookup j "RetainPrimaryCluster") Boolean.of_json
    ; final_snapshot_identifier =
        Aws.Util.option_map (Aws.Json.lookup j "FinalSnapshotIdentifier") String.of_json
    }
end

module DescribeUsersResult = struct
  type t =
    { users : UserList.t
    ; marker : String.t option
    }

  let make ?(users = []) ?marker () = { users; marker }

  let parse xml =
    Some
      { users =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Users" xml) UserList.parse)
      ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ; Some (Aws.Query.Pair ("Users.member", UserList.to_query v.users))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ; Some ("Users", UserList.to_json v.users)
         ])

  let of_json j =
    { users = UserList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Users"))
    ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    }
end

module CacheSecurityGroupQuotaExceededFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module CacheParameterGroupList = struct
  type t = CacheParameterGroup.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map CacheParameterGroup.parse (Aws.Xml.members "CacheParameterGroup" xml))

  let to_query v = Aws.Query.to_query_list CacheParameterGroup.to_query v

  let to_json v = `List (List.map CacheParameterGroup.to_json v)

  let of_json j = Aws.Json.to_list CacheParameterGroup.of_json j
end

module DescribeReservedCacheNodesOfferingsMessage = struct
  type t =
    { reserved_cache_nodes_offering_id : String.t option
    ; cache_node_type : String.t option
    ; duration : String.t option
    ; product_description : String.t option
    ; offering_type : String.t option
    ; max_records : Integer.t option
    ; marker : String.t option
    }

  let make
      ?reserved_cache_nodes_offering_id
      ?cache_node_type
      ?duration
      ?product_description
      ?offering_type
      ?max_records
      ?marker
      () =
    { reserved_cache_nodes_offering_id
    ; cache_node_type
    ; duration
    ; product_description
    ; offering_type
    ; max_records
    ; marker
    }

  let parse xml =
    Some
      { reserved_cache_nodes_offering_id =
          Aws.Util.option_bind
            (Aws.Xml.member "ReservedCacheNodesOfferingId" xml)
            String.parse
      ; cache_node_type =
          Aws.Util.option_bind (Aws.Xml.member "CacheNodeType" xml) String.parse
      ; duration = Aws.Util.option_bind (Aws.Xml.member "Duration" xml) String.parse
      ; product_description =
          Aws.Util.option_bind (Aws.Xml.member "ProductDescription" xml) String.parse
      ; offering_type =
          Aws.Util.option_bind (Aws.Xml.member "OfferingType" xml) String.parse
      ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ; Aws.Util.option_map v.max_records (fun f ->
               Aws.Query.Pair ("MaxRecords", Integer.to_query f))
         ; Aws.Util.option_map v.offering_type (fun f ->
               Aws.Query.Pair ("OfferingType", String.to_query f))
         ; Aws.Util.option_map v.product_description (fun f ->
               Aws.Query.Pair ("ProductDescription", String.to_query f))
         ; Aws.Util.option_map v.duration (fun f ->
               Aws.Query.Pair ("Duration", String.to_query f))
         ; Aws.Util.option_map v.cache_node_type (fun f ->
               Aws.Query.Pair ("CacheNodeType", String.to_query f))
         ; Aws.Util.option_map v.reserved_cache_nodes_offering_id (fun f ->
               Aws.Query.Pair ("ReservedCacheNodesOfferingId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
         ; Aws.Util.option_map v.offering_type (fun f -> "OfferingType", String.to_json f)
         ; Aws.Util.option_map v.product_description (fun f ->
               "ProductDescription", String.to_json f)
         ; Aws.Util.option_map v.duration (fun f -> "Duration", String.to_json f)
         ; Aws.Util.option_map v.cache_node_type (fun f ->
               "CacheNodeType", String.to_json f)
         ; Aws.Util.option_map v.reserved_cache_nodes_offering_id (fun f ->
               "ReservedCacheNodesOfferingId", String.to_json f)
         ])

  let of_json j =
    { reserved_cache_nodes_offering_id =
        Aws.Util.option_map
          (Aws.Json.lookup j "ReservedCacheNodesOfferingId")
          String.of_json
    ; cache_node_type =
        Aws.Util.option_map (Aws.Json.lookup j "CacheNodeType") String.of_json
    ; duration = Aws.Util.option_map (Aws.Json.lookup j "Duration") String.of_json
    ; product_description =
        Aws.Util.option_map (Aws.Json.lookup j "ProductDescription") String.of_json
    ; offering_type =
        Aws.Util.option_map (Aws.Json.lookup j "OfferingType") String.of_json
    ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
    ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    }
end

module CompleteMigrationResponse = struct
  type t = { replication_group : ReplicationGroup.t option }

  let make ?replication_group () = { replication_group }

  let parse xml =
    Some
      { replication_group =
          Aws.Util.option_bind
            (Aws.Xml.member "ReplicationGroup" xml)
            ReplicationGroup.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.replication_group (fun f ->
               Aws.Query.Pair ("ReplicationGroup", ReplicationGroup.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.replication_group (fun f ->
               "ReplicationGroup", ReplicationGroup.to_json f)
         ])

  let of_json j =
    { replication_group =
        Aws.Util.option_map
          (Aws.Json.lookup j "ReplicationGroup")
          ReplicationGroup.of_json
    }
end

module AuthorizeCacheSecurityGroupIngressMessage = struct
  type t =
    { cache_security_group_name : String.t
    ; e_c2_security_group_name : String.t
    ; e_c2_security_group_owner_id : String.t
    }

  let make
      ~cache_security_group_name
      ~e_c2_security_group_name
      ~e_c2_security_group_owner_id
      () =
    { cache_security_group_name; e_c2_security_group_name; e_c2_security_group_owner_id }

  let parse xml =
    Some
      { cache_security_group_name =
          Aws.Xml.required
            "CacheSecurityGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "CacheSecurityGroupName" xml)
               String.parse)
      ; e_c2_security_group_name =
          Aws.Xml.required
            "EC2SecurityGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "EC2SecurityGroupName" xml)
               String.parse)
      ; e_c2_security_group_owner_id =
          Aws.Xml.required
            "EC2SecurityGroupOwnerId"
            (Aws.Util.option_bind
               (Aws.Xml.member "EC2SecurityGroupOwnerId" xml)
               String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("EC2SecurityGroupOwnerId", String.to_query v.e_c2_security_group_owner_id))
         ; Some
             (Aws.Query.Pair
                ("EC2SecurityGroupName", String.to_query v.e_c2_security_group_name))
         ; Some
             (Aws.Query.Pair
                ("CacheSecurityGroupName", String.to_query v.cache_security_group_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("EC2SecurityGroupOwnerId", String.to_json v.e_c2_security_group_owner_id)
         ; Some ("EC2SecurityGroupName", String.to_json v.e_c2_security_group_name)
         ; Some ("CacheSecurityGroupName", String.to_json v.cache_security_group_name)
         ])

  let of_json j =
    { cache_security_group_name =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheSecurityGroupName"))
    ; e_c2_security_group_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EC2SecurityGroupName"))
    ; e_c2_security_group_owner_id =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "EC2SecurityGroupOwnerId"))
    }
end

module RebalanceSlotsInGlobalReplicationGroupMessage = struct
  type t =
    { global_replication_group_id : String.t
    ; apply_immediately : Boolean.t
    }

  let make ~global_replication_group_id ~apply_immediately () =
    { global_replication_group_id; apply_immediately }

  let parse xml =
    Some
      { global_replication_group_id =
          Aws.Xml.required
            "GlobalReplicationGroupId"
            (Aws.Util.option_bind
               (Aws.Xml.member "GlobalReplicationGroupId" xml)
               String.parse)
      ; apply_immediately =
          Aws.Xml.required
            "ApplyImmediately"
            (Aws.Util.option_bind (Aws.Xml.member "ApplyImmediately" xml) Boolean.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("ApplyImmediately", Boolean.to_query v.apply_immediately))
         ; Some
             (Aws.Query.Pair
                ("GlobalReplicationGroupId", String.to_query v.global_replication_group_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("ApplyImmediately", Boolean.to_json v.apply_immediately)
         ; Some ("GlobalReplicationGroupId", String.to_json v.global_replication_group_id)
         ])

  let of_json j =
    { global_replication_group_id =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "GlobalReplicationGroupId"))
    ; apply_immediately =
        Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ApplyImmediately"))
    }
end

module NodeGroupNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ReservedCacheNodeAlreadyExistsFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ServiceUpdatesMessage = struct
  type t =
    { marker : String.t option
    ; service_updates : ServiceUpdateList.t
    }

  let make ?marker ?(service_updates = []) () = { marker; service_updates }

  let parse xml =
    Some
      { marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      ; service_updates =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ServiceUpdates" xml)
               ServiceUpdateList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("ServiceUpdates.member", ServiceUpdateList.to_query v.service_updates))
         ; Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("ServiceUpdates", ServiceUpdateList.to_json v.service_updates)
         ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ])

  let of_json j =
    { marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    ; service_updates =
        ServiceUpdateList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ServiceUpdates"))
    }
end

module DecreaseNodeGroupsInGlobalReplicationGroupResult = struct
  type t = { global_replication_group : GlobalReplicationGroup.t option }

  let make ?global_replication_group () = { global_replication_group }

  let parse xml =
    Some
      { global_replication_group =
          Aws.Util.option_bind
            (Aws.Xml.member "GlobalReplicationGroup" xml)
            GlobalReplicationGroup.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.global_replication_group (fun f ->
               Aws.Query.Pair ("GlobalReplicationGroup", GlobalReplicationGroup.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.global_replication_group (fun f ->
               "GlobalReplicationGroup", GlobalReplicationGroup.to_json f)
         ])

  let of_json j =
    { global_replication_group =
        Aws.Util.option_map
          (Aws.Json.lookup j "GlobalReplicationGroup")
          GlobalReplicationGroup.of_json
    }
end

module GlobalReplicationGroupList = struct
  type t = GlobalReplicationGroup.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map
         GlobalReplicationGroup.parse
         (Aws.Xml.members "GlobalReplicationGroup" xml))

  let to_query v = Aws.Query.to_query_list GlobalReplicationGroup.to_query v

  let to_json v = `List (List.map GlobalReplicationGroup.to_json v)

  let of_json j = Aws.Json.to_list GlobalReplicationGroup.of_json j
end

module DescribeGlobalReplicationGroupsResult = struct
  type t =
    { marker : String.t option
    ; global_replication_groups : GlobalReplicationGroupList.t
    }

  let make ?marker ?(global_replication_groups = []) () =
    { marker; global_replication_groups }

  let parse xml =
    Some
      { marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      ; global_replication_groups =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "GlobalReplicationGroups" xml)
               GlobalReplicationGroupList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "GlobalReplicationGroups.member"
                , GlobalReplicationGroupList.to_query v.global_replication_groups ))
         ; Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "GlobalReplicationGroups"
             , GlobalReplicationGroupList.to_json v.global_replication_groups )
         ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ])

  let of_json j =
    { marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    ; global_replication_groups =
        GlobalReplicationGroupList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "GlobalReplicationGroups"))
    }
end

module ReservedCacheNodesOfferingNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module DescribeReplicationGroupsMessage = struct
  type t =
    { replication_group_id : String.t option
    ; max_records : Integer.t option
    ; marker : String.t option
    }

  let make ?replication_group_id ?max_records ?marker () =
    { replication_group_id; max_records; marker }

  let parse xml =
    Some
      { replication_group_id =
          Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse
      ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ; Aws.Util.option_map v.max_records (fun f ->
               Aws.Query.Pair ("MaxRecords", Integer.to_query f))
         ; Aws.Util.option_map v.replication_group_id (fun f ->
               Aws.Query.Pair ("ReplicationGroupId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
         ; Aws.Util.option_map v.replication_group_id (fun f ->
               "ReplicationGroupId", String.to_json f)
         ])

  let of_json j =
    { replication_group_id =
        Aws.Util.option_map (Aws.Json.lookup j "ReplicationGroupId") String.of_json
    ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
    ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    }
end

module SnapshotNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module FailoverGlobalReplicationGroupMessage = struct
  type t =
    { global_replication_group_id : String.t
    ; primary_region : String.t
    ; primary_replication_group_id : String.t
    }

  let make ~global_replication_group_id ~primary_region ~primary_replication_group_id () =
    { global_replication_group_id; primary_region; primary_replication_group_id }

  let parse xml =
    Some
      { global_replication_group_id =
          Aws.Xml.required
            "GlobalReplicationGroupId"
            (Aws.Util.option_bind
               (Aws.Xml.member "GlobalReplicationGroupId" xml)
               String.parse)
      ; primary_region =
          Aws.Xml.required
            "PrimaryRegion"
            (Aws.Util.option_bind (Aws.Xml.member "PrimaryRegion" xml) String.parse)
      ; primary_replication_group_id =
          Aws.Xml.required
            "PrimaryReplicationGroupId"
            (Aws.Util.option_bind
               (Aws.Xml.member "PrimaryReplicationGroupId" xml)
               String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "PrimaryReplicationGroupId"
                , String.to_query v.primary_replication_group_id ))
         ; Some (Aws.Query.Pair ("PrimaryRegion", String.to_query v.primary_region))
         ; Some
             (Aws.Query.Pair
                ("GlobalReplicationGroupId", String.to_query v.global_replication_group_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ("PrimaryReplicationGroupId", String.to_json v.primary_replication_group_id)
         ; Some ("PrimaryRegion", String.to_json v.primary_region)
         ; Some ("GlobalReplicationGroupId", String.to_json v.global_replication_group_id)
         ])

  let of_json j =
    { global_replication_group_id =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "GlobalReplicationGroupId"))
    ; primary_region =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PrimaryRegion"))
    ; primary_replication_group_id =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "PrimaryReplicationGroupId"))
    }
end

module DescribeServiceUpdatesMessage = struct
  type t =
    { service_update_name : String.t option
    ; service_update_status : ServiceUpdateStatusList.t
    ; max_records : Integer.t option
    ; marker : String.t option
    }

  let make ?service_update_name ?(service_update_status = []) ?max_records ?marker () =
    { service_update_name; service_update_status; max_records; marker }

  let parse xml =
    Some
      { service_update_name =
          Aws.Util.option_bind (Aws.Xml.member "ServiceUpdateName" xml) String.parse
      ; service_update_status =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ServiceUpdateStatus" xml)
               ServiceUpdateStatusList.parse)
      ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ; Aws.Util.option_map v.max_records (fun f ->
               Aws.Query.Pair ("MaxRecords", Integer.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "ServiceUpdateStatus.member"
                , ServiceUpdateStatusList.to_query v.service_update_status ))
         ; Aws.Util.option_map v.service_update_name (fun f ->
               Aws.Query.Pair ("ServiceUpdateName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
         ; Some
             ( "ServiceUpdateStatus"
             , ServiceUpdateStatusList.to_json v.service_update_status )
         ; Aws.Util.option_map v.service_update_name (fun f ->
               "ServiceUpdateName", String.to_json f)
         ])

  let of_json j =
    { service_update_name =
        Aws.Util.option_map (Aws.Json.lookup j "ServiceUpdateName") String.of_json
    ; service_update_status =
        ServiceUpdateStatusList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ServiceUpdateStatus"))
    ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
    ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    }
end

module RegionalConfigurationList = struct
  type t = RegionalConfiguration.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map RegionalConfiguration.parse (Aws.Xml.members "RegionalConfiguration" xml))

  let to_query v = Aws.Query.to_query_list RegionalConfiguration.to_query v

  let to_json v = `List (List.map RegionalConfiguration.to_json v)

  let of_json j = Aws.Json.to_list RegionalConfiguration.of_json j
end

module SnapshotList = struct
  type t = Snapshot.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map Snapshot.parse (Aws.Xml.members "Snapshot" xml))

  let to_query v = Aws.Query.to_query_list Snapshot.to_query v

  let to_json v = `List (List.map Snapshot.to_json v)

  let of_json j = Aws.Json.to_list Snapshot.of_json j
end

module DescribeSnapshotsListMessage = struct
  type t =
    { marker : String.t option
    ; snapshots : SnapshotList.t
    }

  let make ?marker ?(snapshots = []) () = { marker; snapshots }

  let parse xml =
    Some
      { marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      ; snapshots =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Snapshots" xml) SnapshotList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Snapshots.member", SnapshotList.to_query v.snapshots))
         ; Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Snapshots", SnapshotList.to_json v.snapshots)
         ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ])

  let of_json j =
    { marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    ; snapshots =
        SnapshotList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Snapshots"))
    }
end

module ServiceUpdateNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module DeleteReplicationGroupResult = struct
  type t = { replication_group : ReplicationGroup.t option }

  let make ?replication_group () = { replication_group }

  let parse xml =
    Some
      { replication_group =
          Aws.Util.option_bind
            (Aws.Xml.member "ReplicationGroup" xml)
            ReplicationGroup.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.replication_group (fun f ->
               Aws.Query.Pair ("ReplicationGroup", ReplicationGroup.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.replication_group (fun f ->
               "ReplicationGroup", ReplicationGroup.to_json f)
         ])

  let of_json j =
    { replication_group =
        Aws.Util.option_map
          (Aws.Json.lookup j "ReplicationGroup")
          ReplicationGroup.of_json
    }
end

module DeleteCacheClusterMessage = struct
  type t =
    { cache_cluster_id : String.t
    ; final_snapshot_identifier : String.t option
    }

  let make ~cache_cluster_id ?final_snapshot_identifier () =
    { cache_cluster_id; final_snapshot_identifier }

  let parse xml =
    Some
      { cache_cluster_id =
          Aws.Xml.required
            "CacheClusterId"
            (Aws.Util.option_bind (Aws.Xml.member "CacheClusterId" xml) String.parse)
      ; final_snapshot_identifier =
          Aws.Util.option_bind (Aws.Xml.member "FinalSnapshotIdentifier" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.final_snapshot_identifier (fun f ->
               Aws.Query.Pair ("FinalSnapshotIdentifier", String.to_query f))
         ; Some (Aws.Query.Pair ("CacheClusterId", String.to_query v.cache_cluster_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.final_snapshot_identifier (fun f ->
               "FinalSnapshotIdentifier", String.to_json f)
         ; Some ("CacheClusterId", String.to_json v.cache_cluster_id)
         ])

  let of_json j =
    { cache_cluster_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheClusterId"))
    ; final_snapshot_identifier =
        Aws.Util.option_map (Aws.Json.lookup j "FinalSnapshotIdentifier") String.of_json
    }
end

module FilterList = struct
  type t = Filter.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map Filter.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list Filter.to_query v

  let to_json v = `List (List.map Filter.to_json v)

  let of_json j = Aws.Json.to_list Filter.of_json j
end

module ReservedCacheNodesOfferingMessage = struct
  type t =
    { marker : String.t option
    ; reserved_cache_nodes_offerings : ReservedCacheNodesOfferingList.t
    }

  let make ?marker ?(reserved_cache_nodes_offerings = []) () =
    { marker; reserved_cache_nodes_offerings }

  let parse xml =
    Some
      { marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      ; reserved_cache_nodes_offerings =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ReservedCacheNodesOfferings" xml)
               ReservedCacheNodesOfferingList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "ReservedCacheNodesOfferings.member"
                , ReservedCacheNodesOfferingList.to_query v.reserved_cache_nodes_offerings
                ))
         ; Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "ReservedCacheNodesOfferings"
             , ReservedCacheNodesOfferingList.to_json v.reserved_cache_nodes_offerings )
         ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ])

  let of_json j =
    { marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    ; reserved_cache_nodes_offerings =
        ReservedCacheNodesOfferingList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ReservedCacheNodesOfferings"))
    }
end

module CreateGlobalReplicationGroupMessage = struct
  type t =
    { global_replication_group_id_suffix : String.t
    ; global_replication_group_description : String.t option
    ; primary_replication_group_id : String.t
    }

  let make
      ~global_replication_group_id_suffix
      ?global_replication_group_description
      ~primary_replication_group_id
      () =
    { global_replication_group_id_suffix
    ; global_replication_group_description
    ; primary_replication_group_id
    }

  let parse xml =
    Some
      { global_replication_group_id_suffix =
          Aws.Xml.required
            "GlobalReplicationGroupIdSuffix"
            (Aws.Util.option_bind
               (Aws.Xml.member "GlobalReplicationGroupIdSuffix" xml)
               String.parse)
      ; global_replication_group_description =
          Aws.Util.option_bind
            (Aws.Xml.member "GlobalReplicationGroupDescription" xml)
            String.parse
      ; primary_replication_group_id =
          Aws.Xml.required
            "PrimaryReplicationGroupId"
            (Aws.Util.option_bind
               (Aws.Xml.member "PrimaryReplicationGroupId" xml)
               String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "PrimaryReplicationGroupId"
                , String.to_query v.primary_replication_group_id ))
         ; Aws.Util.option_map v.global_replication_group_description (fun f ->
               Aws.Query.Pair ("GlobalReplicationGroupDescription", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "GlobalReplicationGroupIdSuffix"
                , String.to_query v.global_replication_group_id_suffix ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ("PrimaryReplicationGroupId", String.to_json v.primary_replication_group_id)
         ; Aws.Util.option_map v.global_replication_group_description (fun f ->
               "GlobalReplicationGroupDescription", String.to_json f)
         ; Some
             ( "GlobalReplicationGroupIdSuffix"
             , String.to_json v.global_replication_group_id_suffix )
         ])

  let of_json j =
    { global_replication_group_id_suffix =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "GlobalReplicationGroupIdSuffix"))
    ; global_replication_group_description =
        Aws.Util.option_map
          (Aws.Json.lookup j "GlobalReplicationGroupDescription")
          String.of_json
    ; primary_replication_group_id =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "PrimaryReplicationGroupId"))
    }
end

module CreateCacheSecurityGroupResult = struct
  type t = { cache_security_group : CacheSecurityGroup.t option }

  let make ?cache_security_group () = { cache_security_group }

  let parse xml =
    Some
      { cache_security_group =
          Aws.Util.option_bind
            (Aws.Xml.member "CacheSecurityGroup" xml)
            CacheSecurityGroup.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cache_security_group (fun f ->
               Aws.Query.Pair ("CacheSecurityGroup", CacheSecurityGroup.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cache_security_group (fun f ->
               "CacheSecurityGroup", CacheSecurityGroup.to_json f)
         ])

  let of_json j =
    { cache_security_group =
        Aws.Util.option_map
          (Aws.Json.lookup j "CacheSecurityGroup")
          CacheSecurityGroup.of_json
    }
end

module StartMigrationMessage = struct
  type t =
    { replication_group_id : String.t
    ; customer_node_endpoint_list : CustomerNodeEndpointList.t
    }

  let make ~replication_group_id ~customer_node_endpoint_list () =
    { replication_group_id; customer_node_endpoint_list }

  let parse xml =
    Some
      { replication_group_id =
          Aws.Xml.required
            "ReplicationGroupId"
            (Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse)
      ; customer_node_endpoint_list =
          Aws.Xml.required
            "CustomerNodeEndpointList"
            (Aws.Util.option_bind
               (Aws.Xml.member "CustomerNodeEndpointList" xml)
               CustomerNodeEndpointList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "CustomerNodeEndpointList.member"
                , CustomerNodeEndpointList.to_query v.customer_node_endpoint_list ))
         ; Some
             (Aws.Query.Pair ("ReplicationGroupId", String.to_query v.replication_group_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "CustomerNodeEndpointList"
             , CustomerNodeEndpointList.to_json v.customer_node_endpoint_list )
         ; Some ("ReplicationGroupId", String.to_json v.replication_group_id)
         ])

  let of_json j =
    { replication_group_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicationGroupId"))
    ; customer_node_endpoint_list =
        CustomerNodeEndpointList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CustomerNodeEndpointList"))
    }
end

module Parameter = struct
  type t =
    { parameter_name : String.t option
    ; parameter_value : String.t option
    ; description : String.t option
    ; source : String.t option
    ; data_type : String.t option
    ; allowed_values : String.t option
    ; is_modifiable : Boolean.t option
    ; minimum_engine_version : String.t option
    ; change_type : ChangeType.t option
    }

  let make
      ?parameter_name
      ?parameter_value
      ?description
      ?source
      ?data_type
      ?allowed_values
      ?is_modifiable
      ?minimum_engine_version
      ?change_type
      () =
    { parameter_name
    ; parameter_value
    ; description
    ; source
    ; data_type
    ; allowed_values
    ; is_modifiable
    ; minimum_engine_version
    ; change_type
    }

  let parse xml =
    Some
      { parameter_name =
          Aws.Util.option_bind (Aws.Xml.member "ParameterName" xml) String.parse
      ; parameter_value =
          Aws.Util.option_bind (Aws.Xml.member "ParameterValue" xml) String.parse
      ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
      ; source = Aws.Util.option_bind (Aws.Xml.member "Source" xml) String.parse
      ; data_type = Aws.Util.option_bind (Aws.Xml.member "DataType" xml) String.parse
      ; allowed_values =
          Aws.Util.option_bind (Aws.Xml.member "AllowedValues" xml) String.parse
      ; is_modifiable =
          Aws.Util.option_bind (Aws.Xml.member "IsModifiable" xml) Boolean.parse
      ; minimum_engine_version =
          Aws.Util.option_bind (Aws.Xml.member "MinimumEngineVersion" xml) String.parse
      ; change_type =
          Aws.Util.option_bind (Aws.Xml.member "ChangeType" xml) ChangeType.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.change_type (fun f ->
               Aws.Query.Pair ("ChangeType", ChangeType.to_query f))
         ; Aws.Util.option_map v.minimum_engine_version (fun f ->
               Aws.Query.Pair ("MinimumEngineVersion", String.to_query f))
         ; Aws.Util.option_map v.is_modifiable (fun f ->
               Aws.Query.Pair ("IsModifiable", Boolean.to_query f))
         ; Aws.Util.option_map v.allowed_values (fun f ->
               Aws.Query.Pair ("AllowedValues", String.to_query f))
         ; Aws.Util.option_map v.data_type (fun f ->
               Aws.Query.Pair ("DataType", String.to_query f))
         ; Aws.Util.option_map v.source (fun f ->
               Aws.Query.Pair ("Source", String.to_query f))
         ; Aws.Util.option_map v.description (fun f ->
               Aws.Query.Pair ("Description", String.to_query f))
         ; Aws.Util.option_map v.parameter_value (fun f ->
               Aws.Query.Pair ("ParameterValue", String.to_query f))
         ; Aws.Util.option_map v.parameter_name (fun f ->
               Aws.Query.Pair ("ParameterName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.change_type (fun f -> "ChangeType", ChangeType.to_json f)
         ; Aws.Util.option_map v.minimum_engine_version (fun f ->
               "MinimumEngineVersion", String.to_json f)
         ; Aws.Util.option_map v.is_modifiable (fun f ->
               "IsModifiable", Boolean.to_json f)
         ; Aws.Util.option_map v.allowed_values (fun f ->
               "AllowedValues", String.to_json f)
         ; Aws.Util.option_map v.data_type (fun f -> "DataType", String.to_json f)
         ; Aws.Util.option_map v.source (fun f -> "Source", String.to_json f)
         ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
         ; Aws.Util.option_map v.parameter_value (fun f ->
               "ParameterValue", String.to_json f)
         ; Aws.Util.option_map v.parameter_name (fun f ->
               "ParameterName", String.to_json f)
         ])

  let of_json j =
    { parameter_name =
        Aws.Util.option_map (Aws.Json.lookup j "ParameterName") String.of_json
    ; parameter_value =
        Aws.Util.option_map (Aws.Json.lookup j "ParameterValue") String.of_json
    ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
    ; source = Aws.Util.option_map (Aws.Json.lookup j "Source") String.of_json
    ; data_type = Aws.Util.option_map (Aws.Json.lookup j "DataType") String.of_json
    ; allowed_values =
        Aws.Util.option_map (Aws.Json.lookup j "AllowedValues") String.of_json
    ; is_modifiable =
        Aws.Util.option_map (Aws.Json.lookup j "IsModifiable") Boolean.of_json
    ; minimum_engine_version =
        Aws.Util.option_map (Aws.Json.lookup j "MinimumEngineVersion") String.of_json
    ; change_type =
        Aws.Util.option_map (Aws.Json.lookup j "ChangeType") ChangeType.of_json
    }
end

module DescribeCacheParameterGroupsMessage = struct
  type t =
    { cache_parameter_group_name : String.t option
    ; max_records : Integer.t option
    ; marker : String.t option
    }

  let make ?cache_parameter_group_name ?max_records ?marker () =
    { cache_parameter_group_name; max_records; marker }

  let parse xml =
    Some
      { cache_parameter_group_name =
          Aws.Util.option_bind (Aws.Xml.member "CacheParameterGroupName" xml) String.parse
      ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ; Aws.Util.option_map v.max_records (fun f ->
               Aws.Query.Pair ("MaxRecords", Integer.to_query f))
         ; Aws.Util.option_map v.cache_parameter_group_name (fun f ->
               Aws.Query.Pair ("CacheParameterGroupName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
         ; Aws.Util.option_map v.cache_parameter_group_name (fun f ->
               "CacheParameterGroupName", String.to_json f)
         ])

  let of_json j =
    { cache_parameter_group_name =
        Aws.Util.option_map (Aws.Json.lookup j "CacheParameterGroupName") String.of_json
    ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
    ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    }
end

module CacheParameterGroupNameMessage = struct
  type t = { cache_parameter_group_name : String.t option }

  let make ?cache_parameter_group_name () = { cache_parameter_group_name }

  let parse xml =
    Some
      { cache_parameter_group_name =
          Aws.Util.option_bind (Aws.Xml.member "CacheParameterGroupName" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cache_parameter_group_name (fun f ->
               Aws.Query.Pair ("CacheParameterGroupName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cache_parameter_group_name (fun f ->
               "CacheParameterGroupName", String.to_json f)
         ])

  let of_json j =
    { cache_parameter_group_name =
        Aws.Util.option_map (Aws.Json.lookup j "CacheParameterGroupName") String.of_json
    }
end

module ModifyCacheClusterMessage = struct
  type t =
    { cache_cluster_id : String.t
    ; num_cache_nodes : Integer.t option
    ; cache_node_ids_to_remove : CacheNodeIdsList.t
    ; a_z_mode : AZMode.t option
    ; new_availability_zones : PreferredAvailabilityZoneList.t
    ; cache_security_group_names : CacheSecurityGroupNameList.t
    ; security_group_ids : SecurityGroupIdsList.t
    ; preferred_maintenance_window : String.t option
    ; notification_topic_arn : String.t option
    ; cache_parameter_group_name : String.t option
    ; notification_topic_status : String.t option
    ; apply_immediately : Boolean.t option
    ; engine_version : String.t option
    ; auto_minor_version_upgrade : Boolean.t option
    ; snapshot_retention_limit : Integer.t option
    ; snapshot_window : String.t option
    ; cache_node_type : String.t option
    ; auth_token : String.t option
    ; auth_token_update_strategy : AuthTokenUpdateStrategyType.t option
    }

  let make
      ~cache_cluster_id
      ?num_cache_nodes
      ?(cache_node_ids_to_remove = [])
      ?a_z_mode
      ?(new_availability_zones = [])
      ?(cache_security_group_names = [])
      ?(security_group_ids = [])
      ?preferred_maintenance_window
      ?notification_topic_arn
      ?cache_parameter_group_name
      ?notification_topic_status
      ?apply_immediately
      ?engine_version
      ?auto_minor_version_upgrade
      ?snapshot_retention_limit
      ?snapshot_window
      ?cache_node_type
      ?auth_token
      ?auth_token_update_strategy
      () =
    { cache_cluster_id
    ; num_cache_nodes
    ; cache_node_ids_to_remove
    ; a_z_mode
    ; new_availability_zones
    ; cache_security_group_names
    ; security_group_ids
    ; preferred_maintenance_window
    ; notification_topic_arn
    ; cache_parameter_group_name
    ; notification_topic_status
    ; apply_immediately
    ; engine_version
    ; auto_minor_version_upgrade
    ; snapshot_retention_limit
    ; snapshot_window
    ; cache_node_type
    ; auth_token
    ; auth_token_update_strategy
    }

  let parse xml =
    Some
      { cache_cluster_id =
          Aws.Xml.required
            "CacheClusterId"
            (Aws.Util.option_bind (Aws.Xml.member "CacheClusterId" xml) String.parse)
      ; num_cache_nodes =
          Aws.Util.option_bind (Aws.Xml.member "NumCacheNodes" xml) Integer.parse
      ; cache_node_ids_to_remove =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "CacheNodeIdsToRemove" xml)
               CacheNodeIdsList.parse)
      ; a_z_mode = Aws.Util.option_bind (Aws.Xml.member "AZMode" xml) AZMode.parse
      ; new_availability_zones =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "NewAvailabilityZones" xml)
               PreferredAvailabilityZoneList.parse)
      ; cache_security_group_names =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "CacheSecurityGroupNames" xml)
               CacheSecurityGroupNameList.parse)
      ; security_group_ids =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "SecurityGroupIds" xml)
               SecurityGroupIdsList.parse)
      ; preferred_maintenance_window =
          Aws.Util.option_bind
            (Aws.Xml.member "PreferredMaintenanceWindow" xml)
            String.parse
      ; notification_topic_arn =
          Aws.Util.option_bind (Aws.Xml.member "NotificationTopicArn" xml) String.parse
      ; cache_parameter_group_name =
          Aws.Util.option_bind (Aws.Xml.member "CacheParameterGroupName" xml) String.parse
      ; notification_topic_status =
          Aws.Util.option_bind (Aws.Xml.member "NotificationTopicStatus" xml) String.parse
      ; apply_immediately =
          Aws.Util.option_bind (Aws.Xml.member "ApplyImmediately" xml) Boolean.parse
      ; engine_version =
          Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
      ; auto_minor_version_upgrade =
          Aws.Util.option_bind
            (Aws.Xml.member "AutoMinorVersionUpgrade" xml)
            Boolean.parse
      ; snapshot_retention_limit =
          Aws.Util.option_bind (Aws.Xml.member "SnapshotRetentionLimit" xml) Integer.parse
      ; snapshot_window =
          Aws.Util.option_bind (Aws.Xml.member "SnapshotWindow" xml) String.parse
      ; cache_node_type =
          Aws.Util.option_bind (Aws.Xml.member "CacheNodeType" xml) String.parse
      ; auth_token = Aws.Util.option_bind (Aws.Xml.member "AuthToken" xml) String.parse
      ; auth_token_update_strategy =
          Aws.Util.option_bind
            (Aws.Xml.member "AuthTokenUpdateStrategy" xml)
            AuthTokenUpdateStrategyType.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.auth_token_update_strategy (fun f ->
               Aws.Query.Pair
                 ("AuthTokenUpdateStrategy", AuthTokenUpdateStrategyType.to_query f))
         ; Aws.Util.option_map v.auth_token (fun f ->
               Aws.Query.Pair ("AuthToken", String.to_query f))
         ; Aws.Util.option_map v.cache_node_type (fun f ->
               Aws.Query.Pair ("CacheNodeType", String.to_query f))
         ; Aws.Util.option_map v.snapshot_window (fun f ->
               Aws.Query.Pair ("SnapshotWindow", String.to_query f))
         ; Aws.Util.option_map v.snapshot_retention_limit (fun f ->
               Aws.Query.Pair ("SnapshotRetentionLimit", Integer.to_query f))
         ; Aws.Util.option_map v.auto_minor_version_upgrade (fun f ->
               Aws.Query.Pair ("AutoMinorVersionUpgrade", Boolean.to_query f))
         ; Aws.Util.option_map v.engine_version (fun f ->
               Aws.Query.Pair ("EngineVersion", String.to_query f))
         ; Aws.Util.option_map v.apply_immediately (fun f ->
               Aws.Query.Pair ("ApplyImmediately", Boolean.to_query f))
         ; Aws.Util.option_map v.notification_topic_status (fun f ->
               Aws.Query.Pair ("NotificationTopicStatus", String.to_query f))
         ; Aws.Util.option_map v.cache_parameter_group_name (fun f ->
               Aws.Query.Pair ("CacheParameterGroupName", String.to_query f))
         ; Aws.Util.option_map v.notification_topic_arn (fun f ->
               Aws.Query.Pair ("NotificationTopicArn", String.to_query f))
         ; Aws.Util.option_map v.preferred_maintenance_window (fun f ->
               Aws.Query.Pair ("PreferredMaintenanceWindow", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "SecurityGroupIds.member"
                , SecurityGroupIdsList.to_query v.security_group_ids ))
         ; Some
             (Aws.Query.Pair
                ( "CacheSecurityGroupNames.member"
                , CacheSecurityGroupNameList.to_query v.cache_security_group_names ))
         ; Some
             (Aws.Query.Pair
                ( "NewAvailabilityZones.member"
                , PreferredAvailabilityZoneList.to_query v.new_availability_zones ))
         ; Aws.Util.option_map v.a_z_mode (fun f ->
               Aws.Query.Pair ("AZMode", AZMode.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "CacheNodeIdsToRemove.member"
                , CacheNodeIdsList.to_query v.cache_node_ids_to_remove ))
         ; Aws.Util.option_map v.num_cache_nodes (fun f ->
               Aws.Query.Pair ("NumCacheNodes", Integer.to_query f))
         ; Some (Aws.Query.Pair ("CacheClusterId", String.to_query v.cache_cluster_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.auth_token_update_strategy (fun f ->
               "AuthTokenUpdateStrategy", AuthTokenUpdateStrategyType.to_json f)
         ; Aws.Util.option_map v.auth_token (fun f -> "AuthToken", String.to_json f)
         ; Aws.Util.option_map v.cache_node_type (fun f ->
               "CacheNodeType", String.to_json f)
         ; Aws.Util.option_map v.snapshot_window (fun f ->
               "SnapshotWindow", String.to_json f)
         ; Aws.Util.option_map v.snapshot_retention_limit (fun f ->
               "SnapshotRetentionLimit", Integer.to_json f)
         ; Aws.Util.option_map v.auto_minor_version_upgrade (fun f ->
               "AutoMinorVersionUpgrade", Boolean.to_json f)
         ; Aws.Util.option_map v.engine_version (fun f ->
               "EngineVersion", String.to_json f)
         ; Aws.Util.option_map v.apply_immediately (fun f ->
               "ApplyImmediately", Boolean.to_json f)
         ; Aws.Util.option_map v.notification_topic_status (fun f ->
               "NotificationTopicStatus", String.to_json f)
         ; Aws.Util.option_map v.cache_parameter_group_name (fun f ->
               "CacheParameterGroupName", String.to_json f)
         ; Aws.Util.option_map v.notification_topic_arn (fun f ->
               "NotificationTopicArn", String.to_json f)
         ; Aws.Util.option_map v.preferred_maintenance_window (fun f ->
               "PreferredMaintenanceWindow", String.to_json f)
         ; Some ("SecurityGroupIds", SecurityGroupIdsList.to_json v.security_group_ids)
         ; Some
             ( "CacheSecurityGroupNames"
             , CacheSecurityGroupNameList.to_json v.cache_security_group_names )
         ; Some
             ( "NewAvailabilityZones"
             , PreferredAvailabilityZoneList.to_json v.new_availability_zones )
         ; Aws.Util.option_map v.a_z_mode (fun f -> "AZMode", AZMode.to_json f)
         ; Some
             ("CacheNodeIdsToRemove", CacheNodeIdsList.to_json v.cache_node_ids_to_remove)
         ; Aws.Util.option_map v.num_cache_nodes (fun f ->
               "NumCacheNodes", Integer.to_json f)
         ; Some ("CacheClusterId", String.to_json v.cache_cluster_id)
         ])

  let of_json j =
    { cache_cluster_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheClusterId"))
    ; num_cache_nodes =
        Aws.Util.option_map (Aws.Json.lookup j "NumCacheNodes") Integer.of_json
    ; cache_node_ids_to_remove =
        CacheNodeIdsList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheNodeIdsToRemove"))
    ; a_z_mode = Aws.Util.option_map (Aws.Json.lookup j "AZMode") AZMode.of_json
    ; new_availability_zones =
        PreferredAvailabilityZoneList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "NewAvailabilityZones"))
    ; cache_security_group_names =
        CacheSecurityGroupNameList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheSecurityGroupNames"))
    ; security_group_ids =
        SecurityGroupIdsList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroupIds"))
    ; preferred_maintenance_window =
        Aws.Util.option_map
          (Aws.Json.lookup j "PreferredMaintenanceWindow")
          String.of_json
    ; notification_topic_arn =
        Aws.Util.option_map (Aws.Json.lookup j "NotificationTopicArn") String.of_json
    ; cache_parameter_group_name =
        Aws.Util.option_map (Aws.Json.lookup j "CacheParameterGroupName") String.of_json
    ; notification_topic_status =
        Aws.Util.option_map (Aws.Json.lookup j "NotificationTopicStatus") String.of_json
    ; apply_immediately =
        Aws.Util.option_map (Aws.Json.lookup j "ApplyImmediately") Boolean.of_json
    ; engine_version =
        Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
    ; auto_minor_version_upgrade =
        Aws.Util.option_map (Aws.Json.lookup j "AutoMinorVersionUpgrade") Boolean.of_json
    ; snapshot_retention_limit =
        Aws.Util.option_map (Aws.Json.lookup j "SnapshotRetentionLimit") Integer.of_json
    ; snapshot_window =
        Aws.Util.option_map (Aws.Json.lookup j "SnapshotWindow") String.of_json
    ; cache_node_type =
        Aws.Util.option_map (Aws.Json.lookup j "CacheNodeType") String.of_json
    ; auth_token = Aws.Util.option_map (Aws.Json.lookup j "AuthToken") String.of_json
    ; auth_token_update_strategy =
        Aws.Util.option_map
          (Aws.Json.lookup j "AuthTokenUpdateStrategy")
          AuthTokenUpdateStrategyType.of_json
    }
end

module CreateCacheSubnetGroupResult = struct
  type t = { cache_subnet_group : CacheSubnetGroup.t option }

  let make ?cache_subnet_group () = { cache_subnet_group }

  let parse xml =
    Some
      { cache_subnet_group =
          Aws.Util.option_bind
            (Aws.Xml.member "CacheSubnetGroup" xml)
            CacheSubnetGroup.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cache_subnet_group (fun f ->
               Aws.Query.Pair ("CacheSubnetGroup", CacheSubnetGroup.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cache_subnet_group (fun f ->
               "CacheSubnetGroup", CacheSubnetGroup.to_json f)
         ])

  let of_json j =
    { cache_subnet_group =
        Aws.Util.option_map
          (Aws.Json.lookup j "CacheSubnetGroup")
          CacheSubnetGroup.of_json
    }
end

module DeleteSnapshotMessage = struct
  type t = { snapshot_name : String.t }

  let make ~snapshot_name () = { snapshot_name }

  let parse xml =
    Some
      { snapshot_name =
          Aws.Xml.required
            "SnapshotName"
            (Aws.Util.option_bind (Aws.Xml.member "SnapshotName" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("SnapshotName", String.to_query v.snapshot_name)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt [ Some ("SnapshotName", String.to_json v.snapshot_name) ])

  let of_json j =
    { snapshot_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SnapshotName"))
    }
end

module RebootCacheClusterResult = struct
  type t = { cache_cluster : CacheCluster.t option }

  let make ?cache_cluster () = { cache_cluster }

  let parse xml =
    Some
      { cache_cluster =
          Aws.Util.option_bind (Aws.Xml.member "CacheCluster" xml) CacheCluster.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cache_cluster (fun f ->
               Aws.Query.Pair ("CacheCluster", CacheCluster.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cache_cluster (fun f ->
               "CacheCluster", CacheCluster.to_json f)
         ])

  let of_json j =
    { cache_cluster =
        Aws.Util.option_map (Aws.Json.lookup j "CacheCluster") CacheCluster.of_json
    }
end

module DeleteCacheSubnetGroupMessage = struct
  type t = { cache_subnet_group_name : String.t }

  let make ~cache_subnet_group_name () = { cache_subnet_group_name }

  let parse xml =
    Some
      { cache_subnet_group_name =
          Aws.Xml.required
            "CacheSubnetGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "CacheSubnetGroupName" xml)
               String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("CacheSubnetGroupName", String.to_query v.cache_subnet_group_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("CacheSubnetGroupName", String.to_json v.cache_subnet_group_name) ])

  let of_json j =
    { cache_subnet_group_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheSubnetGroupName"))
    }
end

module DescribeUsersMessage = struct
  type t =
    { engine : String.t option
    ; user_id : String.t option
    ; filters : FilterList.t
    ; max_records : Integer.t option
    ; marker : String.t option
    }

  let make ?engine ?user_id ?(filters = []) ?max_records ?marker () =
    { engine; user_id; filters; max_records; marker }

  let parse xml =
    Some
      { engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
      ; user_id = Aws.Util.option_bind (Aws.Xml.member "UserId" xml) String.parse
      ; filters =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Filters" xml) FilterList.parse)
      ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ; Aws.Util.option_map v.max_records (fun f ->
               Aws.Query.Pair ("MaxRecords", Integer.to_query f))
         ; Some (Aws.Query.Pair ("Filters.member", FilterList.to_query v.filters))
         ; Aws.Util.option_map v.user_id (fun f ->
               Aws.Query.Pair ("UserId", String.to_query f))
         ; Aws.Util.option_map v.engine (fun f ->
               Aws.Query.Pair ("Engine", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
         ; Some ("Filters", FilterList.to_json v.filters)
         ; Aws.Util.option_map v.user_id (fun f -> "UserId", String.to_json f)
         ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
         ])

  let of_json j =
    { engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
    ; user_id = Aws.Util.option_map (Aws.Json.lookup j "UserId") String.of_json
    ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filters"))
    ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
    ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    }
end

module CacheSecurityGroups = struct
  type t = CacheSecurityGroup.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map CacheSecurityGroup.parse (Aws.Xml.members "CacheSecurityGroup" xml))

  let to_query v = Aws.Query.to_query_list CacheSecurityGroup.to_query v

  let to_json v = `List (List.map CacheSecurityGroup.to_json v)

  let of_json j = Aws.Json.to_list CacheSecurityGroup.of_json j
end

module CacheSecurityGroupMessage = struct
  type t =
    { marker : String.t option
    ; cache_security_groups : CacheSecurityGroups.t
    }

  let make ?marker ?(cache_security_groups = []) () = { marker; cache_security_groups }

  let parse xml =
    Some
      { marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      ; cache_security_groups =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "CacheSecurityGroups" xml)
               CacheSecurityGroups.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "CacheSecurityGroups.member"
                , CacheSecurityGroups.to_query v.cache_security_groups ))
         ; Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ("CacheSecurityGroups", CacheSecurityGroups.to_json v.cache_security_groups)
         ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ])

  let of_json j =
    { marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    ; cache_security_groups =
        CacheSecurityGroups.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheSecurityGroups"))
    }
end

module ModifyReplicationGroupResult = struct
  type t = { replication_group : ReplicationGroup.t option }

  let make ?replication_group () = { replication_group }

  let parse xml =
    Some
      { replication_group =
          Aws.Util.option_bind
            (Aws.Xml.member "ReplicationGroup" xml)
            ReplicationGroup.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.replication_group (fun f ->
               Aws.Query.Pair ("ReplicationGroup", ReplicationGroup.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.replication_group (fun f ->
               "ReplicationGroup", ReplicationGroup.to_json f)
         ])

  let of_json j =
    { replication_group =
        Aws.Util.option_map
          (Aws.Json.lookup j "ReplicationGroup")
          ReplicationGroup.of_json
    }
end

module ParametersList = struct
  type t = Parameter.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map Parameter.parse (Aws.Xml.members "Parameter" xml))

  let to_query v = Aws.Query.to_query_list Parameter.to_query v

  let to_json v = `List (List.map Parameter.to_json v)

  let of_json j = Aws.Json.to_list Parameter.of_json j
end

module GlobalReplicationGroupNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module InvalidCacheSecurityGroupStateFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module TestFailoverResult = struct
  type t = { replication_group : ReplicationGroup.t option }

  let make ?replication_group () = { replication_group }

  let parse xml =
    Some
      { replication_group =
          Aws.Util.option_bind
            (Aws.Xml.member "ReplicationGroup" xml)
            ReplicationGroup.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.replication_group (fun f ->
               Aws.Query.Pair ("ReplicationGroup", ReplicationGroup.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.replication_group (fun f ->
               "ReplicationGroup", ReplicationGroup.to_json f)
         ])

  let of_json j =
    { replication_group =
        Aws.Util.option_map
          (Aws.Json.lookup j "ReplicationGroup")
          ReplicationGroup.of_json
    }
end

module DeleteGlobalReplicationGroupMessage = struct
  type t =
    { global_replication_group_id : String.t
    ; retain_primary_replication_group : Boolean.t
    }

  let make ~global_replication_group_id ~retain_primary_replication_group () =
    { global_replication_group_id; retain_primary_replication_group }

  let parse xml =
    Some
      { global_replication_group_id =
          Aws.Xml.required
            "GlobalReplicationGroupId"
            (Aws.Util.option_bind
               (Aws.Xml.member "GlobalReplicationGroupId" xml)
               String.parse)
      ; retain_primary_replication_group =
          Aws.Xml.required
            "RetainPrimaryReplicationGroup"
            (Aws.Util.option_bind
               (Aws.Xml.member "RetainPrimaryReplicationGroup" xml)
               Boolean.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "RetainPrimaryReplicationGroup"
                , Boolean.to_query v.retain_primary_replication_group ))
         ; Some
             (Aws.Query.Pair
                ("GlobalReplicationGroupId", String.to_query v.global_replication_group_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "RetainPrimaryReplicationGroup"
             , Boolean.to_json v.retain_primary_replication_group )
         ; Some ("GlobalReplicationGroupId", String.to_json v.global_replication_group_id)
         ])

  let of_json j =
    { global_replication_group_id =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "GlobalReplicationGroupId"))
    ; retain_primary_replication_group =
        Boolean.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "RetainPrimaryReplicationGroup"))
    }
end

module UserGroupQuotaExceededFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module RebalanceSlotsInGlobalReplicationGroupResult = struct
  type t = { global_replication_group : GlobalReplicationGroup.t option }

  let make ?global_replication_group () = { global_replication_group }

  let parse xml =
    Some
      { global_replication_group =
          Aws.Util.option_bind
            (Aws.Xml.member "GlobalReplicationGroup" xml)
            GlobalReplicationGroup.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.global_replication_group (fun f ->
               Aws.Query.Pair ("GlobalReplicationGroup", GlobalReplicationGroup.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.global_replication_group (fun f ->
               "GlobalReplicationGroup", GlobalReplicationGroup.to_json f)
         ])

  let of_json j =
    { global_replication_group =
        Aws.Util.option_map
          (Aws.Json.lookup j "GlobalReplicationGroup")
          GlobalReplicationGroup.of_json
    }
end

module ModifyReplicationGroupMessage = struct
  type t =
    { replication_group_id : String.t
    ; replication_group_description : String.t option
    ; primary_cluster_id : String.t option
    ; snapshotting_cluster_id : String.t option
    ; automatic_failover_enabled : Boolean.t option
    ; multi_a_z_enabled : Boolean.t option
    ; node_group_id : String.t option
    ; cache_security_group_names : CacheSecurityGroupNameList.t
    ; security_group_ids : SecurityGroupIdsList.t
    ; preferred_maintenance_window : String.t option
    ; notification_topic_arn : String.t option
    ; cache_parameter_group_name : String.t option
    ; notification_topic_status : String.t option
    ; apply_immediately : Boolean.t option
    ; engine_version : String.t option
    ; auto_minor_version_upgrade : Boolean.t option
    ; snapshot_retention_limit : Integer.t option
    ; snapshot_window : String.t option
    ; cache_node_type : String.t option
    ; auth_token : String.t option
    ; auth_token_update_strategy : AuthTokenUpdateStrategyType.t option
    ; user_group_ids_to_add : UserGroupIdList.t
    ; user_group_ids_to_remove : UserGroupIdList.t
    ; remove_user_groups : Boolean.t option
    }

  let make
      ~replication_group_id
      ?replication_group_description
      ?primary_cluster_id
      ?snapshotting_cluster_id
      ?automatic_failover_enabled
      ?multi_a_z_enabled
      ?node_group_id
      ?(cache_security_group_names = [])
      ?(security_group_ids = [])
      ?preferred_maintenance_window
      ?notification_topic_arn
      ?cache_parameter_group_name
      ?notification_topic_status
      ?apply_immediately
      ?engine_version
      ?auto_minor_version_upgrade
      ?snapshot_retention_limit
      ?snapshot_window
      ?cache_node_type
      ?auth_token
      ?auth_token_update_strategy
      ?(user_group_ids_to_add = [])
      ?(user_group_ids_to_remove = [])
      ?remove_user_groups
      () =
    { replication_group_id
    ; replication_group_description
    ; primary_cluster_id
    ; snapshotting_cluster_id
    ; automatic_failover_enabled
    ; multi_a_z_enabled
    ; node_group_id
    ; cache_security_group_names
    ; security_group_ids
    ; preferred_maintenance_window
    ; notification_topic_arn
    ; cache_parameter_group_name
    ; notification_topic_status
    ; apply_immediately
    ; engine_version
    ; auto_minor_version_upgrade
    ; snapshot_retention_limit
    ; snapshot_window
    ; cache_node_type
    ; auth_token
    ; auth_token_update_strategy
    ; user_group_ids_to_add
    ; user_group_ids_to_remove
    ; remove_user_groups
    }

  let parse xml =
    Some
      { replication_group_id =
          Aws.Xml.required
            "ReplicationGroupId"
            (Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse)
      ; replication_group_description =
          Aws.Util.option_bind
            (Aws.Xml.member "ReplicationGroupDescription" xml)
            String.parse
      ; primary_cluster_id =
          Aws.Util.option_bind (Aws.Xml.member "PrimaryClusterId" xml) String.parse
      ; snapshotting_cluster_id =
          Aws.Util.option_bind (Aws.Xml.member "SnapshottingClusterId" xml) String.parse
      ; automatic_failover_enabled =
          Aws.Util.option_bind
            (Aws.Xml.member "AutomaticFailoverEnabled" xml)
            Boolean.parse
      ; multi_a_z_enabled =
          Aws.Util.option_bind (Aws.Xml.member "MultiAZEnabled" xml) Boolean.parse
      ; node_group_id =
          Aws.Util.option_bind (Aws.Xml.member "NodeGroupId" xml) String.parse
      ; cache_security_group_names =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "CacheSecurityGroupNames" xml)
               CacheSecurityGroupNameList.parse)
      ; security_group_ids =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "SecurityGroupIds" xml)
               SecurityGroupIdsList.parse)
      ; preferred_maintenance_window =
          Aws.Util.option_bind
            (Aws.Xml.member "PreferredMaintenanceWindow" xml)
            String.parse
      ; notification_topic_arn =
          Aws.Util.option_bind (Aws.Xml.member "NotificationTopicArn" xml) String.parse
      ; cache_parameter_group_name =
          Aws.Util.option_bind (Aws.Xml.member "CacheParameterGroupName" xml) String.parse
      ; notification_topic_status =
          Aws.Util.option_bind (Aws.Xml.member "NotificationTopicStatus" xml) String.parse
      ; apply_immediately =
          Aws.Util.option_bind (Aws.Xml.member "ApplyImmediately" xml) Boolean.parse
      ; engine_version =
          Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
      ; auto_minor_version_upgrade =
          Aws.Util.option_bind
            (Aws.Xml.member "AutoMinorVersionUpgrade" xml)
            Boolean.parse
      ; snapshot_retention_limit =
          Aws.Util.option_bind (Aws.Xml.member "SnapshotRetentionLimit" xml) Integer.parse
      ; snapshot_window =
          Aws.Util.option_bind (Aws.Xml.member "SnapshotWindow" xml) String.parse
      ; cache_node_type =
          Aws.Util.option_bind (Aws.Xml.member "CacheNodeType" xml) String.parse
      ; auth_token = Aws.Util.option_bind (Aws.Xml.member "AuthToken" xml) String.parse
      ; auth_token_update_strategy =
          Aws.Util.option_bind
            (Aws.Xml.member "AuthTokenUpdateStrategy" xml)
            AuthTokenUpdateStrategyType.parse
      ; user_group_ids_to_add =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "UserGroupIdsToAdd" xml)
               UserGroupIdList.parse)
      ; user_group_ids_to_remove =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "UserGroupIdsToRemove" xml)
               UserGroupIdList.parse)
      ; remove_user_groups =
          Aws.Util.option_bind (Aws.Xml.member "RemoveUserGroups" xml) Boolean.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.remove_user_groups (fun f ->
               Aws.Query.Pair ("RemoveUserGroups", Boolean.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "UserGroupIdsToRemove.member"
                , UserGroupIdList.to_query v.user_group_ids_to_remove ))
         ; Some
             (Aws.Query.Pair
                ( "UserGroupIdsToAdd.member"
                , UserGroupIdList.to_query v.user_group_ids_to_add ))
         ; Aws.Util.option_map v.auth_token_update_strategy (fun f ->
               Aws.Query.Pair
                 ("AuthTokenUpdateStrategy", AuthTokenUpdateStrategyType.to_query f))
         ; Aws.Util.option_map v.auth_token (fun f ->
               Aws.Query.Pair ("AuthToken", String.to_query f))
         ; Aws.Util.option_map v.cache_node_type (fun f ->
               Aws.Query.Pair ("CacheNodeType", String.to_query f))
         ; Aws.Util.option_map v.snapshot_window (fun f ->
               Aws.Query.Pair ("SnapshotWindow", String.to_query f))
         ; Aws.Util.option_map v.snapshot_retention_limit (fun f ->
               Aws.Query.Pair ("SnapshotRetentionLimit", Integer.to_query f))
         ; Aws.Util.option_map v.auto_minor_version_upgrade (fun f ->
               Aws.Query.Pair ("AutoMinorVersionUpgrade", Boolean.to_query f))
         ; Aws.Util.option_map v.engine_version (fun f ->
               Aws.Query.Pair ("EngineVersion", String.to_query f))
         ; Aws.Util.option_map v.apply_immediately (fun f ->
               Aws.Query.Pair ("ApplyImmediately", Boolean.to_query f))
         ; Aws.Util.option_map v.notification_topic_status (fun f ->
               Aws.Query.Pair ("NotificationTopicStatus", String.to_query f))
         ; Aws.Util.option_map v.cache_parameter_group_name (fun f ->
               Aws.Query.Pair ("CacheParameterGroupName", String.to_query f))
         ; Aws.Util.option_map v.notification_topic_arn (fun f ->
               Aws.Query.Pair ("NotificationTopicArn", String.to_query f))
         ; Aws.Util.option_map v.preferred_maintenance_window (fun f ->
               Aws.Query.Pair ("PreferredMaintenanceWindow", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "SecurityGroupIds.member"
                , SecurityGroupIdsList.to_query v.security_group_ids ))
         ; Some
             (Aws.Query.Pair
                ( "CacheSecurityGroupNames.member"
                , CacheSecurityGroupNameList.to_query v.cache_security_group_names ))
         ; Aws.Util.option_map v.node_group_id (fun f ->
               Aws.Query.Pair ("NodeGroupId", String.to_query f))
         ; Aws.Util.option_map v.multi_a_z_enabled (fun f ->
               Aws.Query.Pair ("MultiAZEnabled", Boolean.to_query f))
         ; Aws.Util.option_map v.automatic_failover_enabled (fun f ->
               Aws.Query.Pair ("AutomaticFailoverEnabled", Boolean.to_query f))
         ; Aws.Util.option_map v.snapshotting_cluster_id (fun f ->
               Aws.Query.Pair ("SnapshottingClusterId", String.to_query f))
         ; Aws.Util.option_map v.primary_cluster_id (fun f ->
               Aws.Query.Pair ("PrimaryClusterId", String.to_query f))
         ; Aws.Util.option_map v.replication_group_description (fun f ->
               Aws.Query.Pair ("ReplicationGroupDescription", String.to_query f))
         ; Some
             (Aws.Query.Pair ("ReplicationGroupId", String.to_query v.replication_group_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.remove_user_groups (fun f ->
               "RemoveUserGroups", Boolean.to_json f)
         ; Some
             ("UserGroupIdsToRemove", UserGroupIdList.to_json v.user_group_ids_to_remove)
         ; Some ("UserGroupIdsToAdd", UserGroupIdList.to_json v.user_group_ids_to_add)
         ; Aws.Util.option_map v.auth_token_update_strategy (fun f ->
               "AuthTokenUpdateStrategy", AuthTokenUpdateStrategyType.to_json f)
         ; Aws.Util.option_map v.auth_token (fun f -> "AuthToken", String.to_json f)
         ; Aws.Util.option_map v.cache_node_type (fun f ->
               "CacheNodeType", String.to_json f)
         ; Aws.Util.option_map v.snapshot_window (fun f ->
               "SnapshotWindow", String.to_json f)
         ; Aws.Util.option_map v.snapshot_retention_limit (fun f ->
               "SnapshotRetentionLimit", Integer.to_json f)
         ; Aws.Util.option_map v.auto_minor_version_upgrade (fun f ->
               "AutoMinorVersionUpgrade", Boolean.to_json f)
         ; Aws.Util.option_map v.engine_version (fun f ->
               "EngineVersion", String.to_json f)
         ; Aws.Util.option_map v.apply_immediately (fun f ->
               "ApplyImmediately", Boolean.to_json f)
         ; Aws.Util.option_map v.notification_topic_status (fun f ->
               "NotificationTopicStatus", String.to_json f)
         ; Aws.Util.option_map v.cache_parameter_group_name (fun f ->
               "CacheParameterGroupName", String.to_json f)
         ; Aws.Util.option_map v.notification_topic_arn (fun f ->
               "NotificationTopicArn", String.to_json f)
         ; Aws.Util.option_map v.preferred_maintenance_window (fun f ->
               "PreferredMaintenanceWindow", String.to_json f)
         ; Some ("SecurityGroupIds", SecurityGroupIdsList.to_json v.security_group_ids)
         ; Some
             ( "CacheSecurityGroupNames"
             , CacheSecurityGroupNameList.to_json v.cache_security_group_names )
         ; Aws.Util.option_map v.node_group_id (fun f -> "NodeGroupId", String.to_json f)
         ; Aws.Util.option_map v.multi_a_z_enabled (fun f ->
               "MultiAZEnabled", Boolean.to_json f)
         ; Aws.Util.option_map v.automatic_failover_enabled (fun f ->
               "AutomaticFailoverEnabled", Boolean.to_json f)
         ; Aws.Util.option_map v.snapshotting_cluster_id (fun f ->
               "SnapshottingClusterId", String.to_json f)
         ; Aws.Util.option_map v.primary_cluster_id (fun f ->
               "PrimaryClusterId", String.to_json f)
         ; Aws.Util.option_map v.replication_group_description (fun f ->
               "ReplicationGroupDescription", String.to_json f)
         ; Some ("ReplicationGroupId", String.to_json v.replication_group_id)
         ])

  let of_json j =
    { replication_group_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicationGroupId"))
    ; replication_group_description =
        Aws.Util.option_map
          (Aws.Json.lookup j "ReplicationGroupDescription")
          String.of_json
    ; primary_cluster_id =
        Aws.Util.option_map (Aws.Json.lookup j "PrimaryClusterId") String.of_json
    ; snapshotting_cluster_id =
        Aws.Util.option_map (Aws.Json.lookup j "SnapshottingClusterId") String.of_json
    ; automatic_failover_enabled =
        Aws.Util.option_map (Aws.Json.lookup j "AutomaticFailoverEnabled") Boolean.of_json
    ; multi_a_z_enabled =
        Aws.Util.option_map (Aws.Json.lookup j "MultiAZEnabled") Boolean.of_json
    ; node_group_id = Aws.Util.option_map (Aws.Json.lookup j "NodeGroupId") String.of_json
    ; cache_security_group_names =
        CacheSecurityGroupNameList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheSecurityGroupNames"))
    ; security_group_ids =
        SecurityGroupIdsList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroupIds"))
    ; preferred_maintenance_window =
        Aws.Util.option_map
          (Aws.Json.lookup j "PreferredMaintenanceWindow")
          String.of_json
    ; notification_topic_arn =
        Aws.Util.option_map (Aws.Json.lookup j "NotificationTopicArn") String.of_json
    ; cache_parameter_group_name =
        Aws.Util.option_map (Aws.Json.lookup j "CacheParameterGroupName") String.of_json
    ; notification_topic_status =
        Aws.Util.option_map (Aws.Json.lookup j "NotificationTopicStatus") String.of_json
    ; apply_immediately =
        Aws.Util.option_map (Aws.Json.lookup j "ApplyImmediately") Boolean.of_json
    ; engine_version =
        Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
    ; auto_minor_version_upgrade =
        Aws.Util.option_map (Aws.Json.lookup j "AutoMinorVersionUpgrade") Boolean.of_json
    ; snapshot_retention_limit =
        Aws.Util.option_map (Aws.Json.lookup j "SnapshotRetentionLimit") Integer.of_json
    ; snapshot_window =
        Aws.Util.option_map (Aws.Json.lookup j "SnapshotWindow") String.of_json
    ; cache_node_type =
        Aws.Util.option_map (Aws.Json.lookup j "CacheNodeType") String.of_json
    ; auth_token = Aws.Util.option_map (Aws.Json.lookup j "AuthToken") String.of_json
    ; auth_token_update_strategy =
        Aws.Util.option_map
          (Aws.Json.lookup j "AuthTokenUpdateStrategy")
          AuthTokenUpdateStrategyType.of_json
    ; user_group_ids_to_add =
        UserGroupIdList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "UserGroupIdsToAdd"))
    ; user_group_ids_to_remove =
        UserGroupIdList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "UserGroupIdsToRemove"))
    ; remove_user_groups =
        Aws.Util.option_map (Aws.Json.lookup j "RemoveUserGroups") Boolean.of_json
    }
end

module CacheParameterGroupDetails = struct
  type t =
    { marker : String.t option
    ; parameters : ParametersList.t
    ; cache_node_type_specific_parameters : CacheNodeTypeSpecificParametersList.t
    }

  let make ?marker ?(parameters = []) ?(cache_node_type_specific_parameters = []) () =
    { marker; parameters; cache_node_type_specific_parameters }

  let parse xml =
    Some
      { marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      ; parameters =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Parameters" xml) ParametersList.parse)
      ; cache_node_type_specific_parameters =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "CacheNodeTypeSpecificParameters" xml)
               CacheNodeTypeSpecificParametersList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "CacheNodeTypeSpecificParameters.member"
                , CacheNodeTypeSpecificParametersList.to_query
                    v.cache_node_type_specific_parameters ))
         ; Some
             (Aws.Query.Pair ("Parameters.member", ParametersList.to_query v.parameters))
         ; Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "CacheNodeTypeSpecificParameters"
             , CacheNodeTypeSpecificParametersList.to_json
                 v.cache_node_type_specific_parameters )
         ; Some ("Parameters", ParametersList.to_json v.parameters)
         ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ])

  let of_json j =
    { marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    ; parameters =
        ParametersList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Parameters"))
    ; cache_node_type_specific_parameters =
        CacheNodeTypeSpecificParametersList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheNodeTypeSpecificParameters"))
    }
end

module EngineDefaults = struct
  type t =
    { cache_parameter_group_family : String.t option
    ; marker : String.t option
    ; parameters : ParametersList.t
    ; cache_node_type_specific_parameters : CacheNodeTypeSpecificParametersList.t
    }

  let make
      ?cache_parameter_group_family
      ?marker
      ?(parameters = [])
      ?(cache_node_type_specific_parameters = [])
      () =
    { cache_parameter_group_family
    ; marker
    ; parameters
    ; cache_node_type_specific_parameters
    }

  let parse xml =
    Some
      { cache_parameter_group_family =
          Aws.Util.option_bind
            (Aws.Xml.member "CacheParameterGroupFamily" xml)
            String.parse
      ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      ; parameters =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Parameters" xml) ParametersList.parse)
      ; cache_node_type_specific_parameters =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "CacheNodeTypeSpecificParameters" xml)
               CacheNodeTypeSpecificParametersList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "CacheNodeTypeSpecificParameters.member"
                , CacheNodeTypeSpecificParametersList.to_query
                    v.cache_node_type_specific_parameters ))
         ; Some
             (Aws.Query.Pair ("Parameters.member", ParametersList.to_query v.parameters))
         ; Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ; Aws.Util.option_map v.cache_parameter_group_family (fun f ->
               Aws.Query.Pair ("CacheParameterGroupFamily", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "CacheNodeTypeSpecificParameters"
             , CacheNodeTypeSpecificParametersList.to_json
                 v.cache_node_type_specific_parameters )
         ; Some ("Parameters", ParametersList.to_json v.parameters)
         ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ; Aws.Util.option_map v.cache_parameter_group_family (fun f ->
               "CacheParameterGroupFamily", String.to_json f)
         ])

  let of_json j =
    { cache_parameter_group_family =
        Aws.Util.option_map (Aws.Json.lookup j "CacheParameterGroupFamily") String.of_json
    ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    ; parameters =
        ParametersList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Parameters"))
    ; cache_node_type_specific_parameters =
        CacheNodeTypeSpecificParametersList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheNodeTypeSpecificParameters"))
    }
end

module CacheSubnetGroupInUse = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ModifyCacheClusterResult = struct
  type t = { cache_cluster : CacheCluster.t option }

  let make ?cache_cluster () = { cache_cluster }

  let parse xml =
    Some
      { cache_cluster =
          Aws.Util.option_bind (Aws.Xml.member "CacheCluster" xml) CacheCluster.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cache_cluster (fun f ->
               Aws.Query.Pair ("CacheCluster", CacheCluster.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cache_cluster (fun f ->
               "CacheCluster", CacheCluster.to_json f)
         ])

  let of_json j =
    { cache_cluster =
        Aws.Util.option_map (Aws.Json.lookup j "CacheCluster") CacheCluster.of_json
    }
end

module NodeQuotaForClusterExceededFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module SubnetNotAllowedFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ReplicationGroupAlreadyExistsFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module AllowedNodeTypeModificationsMessage = struct
  type t =
    { scale_up_modifications : NodeTypeList.t
    ; scale_down_modifications : NodeTypeList.t
    }

  let make ?(scale_up_modifications = []) ?(scale_down_modifications = []) () =
    { scale_up_modifications; scale_down_modifications }

  let parse xml =
    Some
      { scale_up_modifications =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ScaleUpModifications" xml)
               NodeTypeList.parse)
      ; scale_down_modifications =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ScaleDownModifications" xml)
               NodeTypeList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "ScaleDownModifications.member"
                , NodeTypeList.to_query v.scale_down_modifications ))
         ; Some
             (Aws.Query.Pair
                ( "ScaleUpModifications.member"
                , NodeTypeList.to_query v.scale_up_modifications ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("ScaleDownModifications", NodeTypeList.to_json v.scale_down_modifications)
         ; Some ("ScaleUpModifications", NodeTypeList.to_json v.scale_up_modifications)
         ])

  let of_json j =
    { scale_up_modifications =
        NodeTypeList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ScaleUpModifications"))
    ; scale_down_modifications =
        NodeTypeList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ScaleDownModifications"))
    }
end

module IncreaseNodeGroupsInGlobalReplicationGroupMessage = struct
  type t =
    { global_replication_group_id : String.t
    ; node_group_count : Integer.t
    ; regional_configurations : RegionalConfigurationList.t
    ; apply_immediately : Boolean.t
    }

  let make
      ~global_replication_group_id
      ~node_group_count
      ?(regional_configurations = [])
      ~apply_immediately
      () =
    { global_replication_group_id
    ; node_group_count
    ; regional_configurations
    ; apply_immediately
    }

  let parse xml =
    Some
      { global_replication_group_id =
          Aws.Xml.required
            "GlobalReplicationGroupId"
            (Aws.Util.option_bind
               (Aws.Xml.member "GlobalReplicationGroupId" xml)
               String.parse)
      ; node_group_count =
          Aws.Xml.required
            "NodeGroupCount"
            (Aws.Util.option_bind (Aws.Xml.member "NodeGroupCount" xml) Integer.parse)
      ; regional_configurations =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "RegionalConfigurations" xml)
               RegionalConfigurationList.parse)
      ; apply_immediately =
          Aws.Xml.required
            "ApplyImmediately"
            (Aws.Util.option_bind (Aws.Xml.member "ApplyImmediately" xml) Boolean.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("ApplyImmediately", Boolean.to_query v.apply_immediately))
         ; Some
             (Aws.Query.Pair
                ( "RegionalConfigurations.member"
                , RegionalConfigurationList.to_query v.regional_configurations ))
         ; Some (Aws.Query.Pair ("NodeGroupCount", Integer.to_query v.node_group_count))
         ; Some
             (Aws.Query.Pair
                ("GlobalReplicationGroupId", String.to_query v.global_replication_group_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("ApplyImmediately", Boolean.to_json v.apply_immediately)
         ; Some
             ( "RegionalConfigurations"
             , RegionalConfigurationList.to_json v.regional_configurations )
         ; Some ("NodeGroupCount", Integer.to_json v.node_group_count)
         ; Some ("GlobalReplicationGroupId", String.to_json v.global_replication_group_id)
         ])

  let of_json j =
    { global_replication_group_id =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "GlobalReplicationGroupId"))
    ; node_group_count =
        Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "NodeGroupCount"))
    ; regional_configurations =
        RegionalConfigurationList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "RegionalConfigurations"))
    ; apply_immediately =
        Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ApplyImmediately"))
    }
end

module InvalidGlobalReplicationGroupStateFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ModifyGlobalReplicationGroupMessage = struct
  type t =
    { global_replication_group_id : String.t
    ; apply_immediately : Boolean.t
    ; cache_node_type : String.t option
    ; engine_version : String.t option
    ; global_replication_group_description : String.t option
    ; automatic_failover_enabled : Boolean.t option
    }

  let make
      ~global_replication_group_id
      ~apply_immediately
      ?cache_node_type
      ?engine_version
      ?global_replication_group_description
      ?automatic_failover_enabled
      () =
    { global_replication_group_id
    ; apply_immediately
    ; cache_node_type
    ; engine_version
    ; global_replication_group_description
    ; automatic_failover_enabled
    }

  let parse xml =
    Some
      { global_replication_group_id =
          Aws.Xml.required
            "GlobalReplicationGroupId"
            (Aws.Util.option_bind
               (Aws.Xml.member "GlobalReplicationGroupId" xml)
               String.parse)
      ; apply_immediately =
          Aws.Xml.required
            "ApplyImmediately"
            (Aws.Util.option_bind (Aws.Xml.member "ApplyImmediately" xml) Boolean.parse)
      ; cache_node_type =
          Aws.Util.option_bind (Aws.Xml.member "CacheNodeType" xml) String.parse
      ; engine_version =
          Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
      ; global_replication_group_description =
          Aws.Util.option_bind
            (Aws.Xml.member "GlobalReplicationGroupDescription" xml)
            String.parse
      ; automatic_failover_enabled =
          Aws.Util.option_bind
            (Aws.Xml.member "AutomaticFailoverEnabled" xml)
            Boolean.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.automatic_failover_enabled (fun f ->
               Aws.Query.Pair ("AutomaticFailoverEnabled", Boolean.to_query f))
         ; Aws.Util.option_map v.global_replication_group_description (fun f ->
               Aws.Query.Pair ("GlobalReplicationGroupDescription", String.to_query f))
         ; Aws.Util.option_map v.engine_version (fun f ->
               Aws.Query.Pair ("EngineVersion", String.to_query f))
         ; Aws.Util.option_map v.cache_node_type (fun f ->
               Aws.Query.Pair ("CacheNodeType", String.to_query f))
         ; Some
             (Aws.Query.Pair ("ApplyImmediately", Boolean.to_query v.apply_immediately))
         ; Some
             (Aws.Query.Pair
                ("GlobalReplicationGroupId", String.to_query v.global_replication_group_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.automatic_failover_enabled (fun f ->
               "AutomaticFailoverEnabled", Boolean.to_json f)
         ; Aws.Util.option_map v.global_replication_group_description (fun f ->
               "GlobalReplicationGroupDescription", String.to_json f)
         ; Aws.Util.option_map v.engine_version (fun f ->
               "EngineVersion", String.to_json f)
         ; Aws.Util.option_map v.cache_node_type (fun f ->
               "CacheNodeType", String.to_json f)
         ; Some ("ApplyImmediately", Boolean.to_json v.apply_immediately)
         ; Some ("GlobalReplicationGroupId", String.to_json v.global_replication_group_id)
         ])

  let of_json j =
    { global_replication_group_id =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "GlobalReplicationGroupId"))
    ; apply_immediately =
        Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ApplyImmediately"))
    ; cache_node_type =
        Aws.Util.option_map (Aws.Json.lookup j "CacheNodeType") String.of_json
    ; engine_version =
        Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
    ; global_replication_group_description =
        Aws.Util.option_map
          (Aws.Json.lookup j "GlobalReplicationGroupDescription")
          String.of_json
    ; automatic_failover_enabled =
        Aws.Util.option_map (Aws.Json.lookup j "AutomaticFailoverEnabled") Boolean.of_json
    }
end

module DescribeSnapshotsMessage = struct
  type t =
    { replication_group_id : String.t option
    ; cache_cluster_id : String.t option
    ; snapshot_name : String.t option
    ; snapshot_source : String.t option
    ; marker : String.t option
    ; max_records : Integer.t option
    ; show_node_group_config : Boolean.t option
    }

  let make
      ?replication_group_id
      ?cache_cluster_id
      ?snapshot_name
      ?snapshot_source
      ?marker
      ?max_records
      ?show_node_group_config
      () =
    { replication_group_id
    ; cache_cluster_id
    ; snapshot_name
    ; snapshot_source
    ; marker
    ; max_records
    ; show_node_group_config
    }

  let parse xml =
    Some
      { replication_group_id =
          Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse
      ; cache_cluster_id =
          Aws.Util.option_bind (Aws.Xml.member "CacheClusterId" xml) String.parse
      ; snapshot_name =
          Aws.Util.option_bind (Aws.Xml.member "SnapshotName" xml) String.parse
      ; snapshot_source =
          Aws.Util.option_bind (Aws.Xml.member "SnapshotSource" xml) String.parse
      ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
      ; show_node_group_config =
          Aws.Util.option_bind (Aws.Xml.member "ShowNodeGroupConfig" xml) Boolean.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.show_node_group_config (fun f ->
               Aws.Query.Pair ("ShowNodeGroupConfig", Boolean.to_query f))
         ; Aws.Util.option_map v.max_records (fun f ->
               Aws.Query.Pair ("MaxRecords", Integer.to_query f))
         ; Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ; Aws.Util.option_map v.snapshot_source (fun f ->
               Aws.Query.Pair ("SnapshotSource", String.to_query f))
         ; Aws.Util.option_map v.snapshot_name (fun f ->
               Aws.Query.Pair ("SnapshotName", String.to_query f))
         ; Aws.Util.option_map v.cache_cluster_id (fun f ->
               Aws.Query.Pair ("CacheClusterId", String.to_query f))
         ; Aws.Util.option_map v.replication_group_id (fun f ->
               Aws.Query.Pair ("ReplicationGroupId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.show_node_group_config (fun f ->
               "ShowNodeGroupConfig", Boolean.to_json f)
         ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
         ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ; Aws.Util.option_map v.snapshot_source (fun f ->
               "SnapshotSource", String.to_json f)
         ; Aws.Util.option_map v.snapshot_name (fun f -> "SnapshotName", String.to_json f)
         ; Aws.Util.option_map v.cache_cluster_id (fun f ->
               "CacheClusterId", String.to_json f)
         ; Aws.Util.option_map v.replication_group_id (fun f ->
               "ReplicationGroupId", String.to_json f)
         ])

  let of_json j =
    { replication_group_id =
        Aws.Util.option_map (Aws.Json.lookup j "ReplicationGroupId") String.of_json
    ; cache_cluster_id =
        Aws.Util.option_map (Aws.Json.lookup j "CacheClusterId") String.of_json
    ; snapshot_name =
        Aws.Util.option_map (Aws.Json.lookup j "SnapshotName") String.of_json
    ; snapshot_source =
        Aws.Util.option_map (Aws.Json.lookup j "SnapshotSource") String.of_json
    ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
    ; show_node_group_config =
        Aws.Util.option_map (Aws.Json.lookup j "ShowNodeGroupConfig") Boolean.of_json
    }
end

module InvalidCacheParameterGroupStateFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module InvalidARNFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module CacheParameterGroupNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module DescribeUserGroupsMessage = struct
  type t =
    { user_group_id : String.t option
    ; max_records : Integer.t option
    ; marker : String.t option
    }

  let make ?user_group_id ?max_records ?marker () = { user_group_id; max_records; marker }

  let parse xml =
    Some
      { user_group_id =
          Aws.Util.option_bind (Aws.Xml.member "UserGroupId" xml) String.parse
      ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ; Aws.Util.option_map v.max_records (fun f ->
               Aws.Query.Pair ("MaxRecords", Integer.to_query f))
         ; Aws.Util.option_map v.user_group_id (fun f ->
               Aws.Query.Pair ("UserGroupId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
         ; Aws.Util.option_map v.user_group_id (fun f -> "UserGroupId", String.to_json f)
         ])

  let of_json j =
    { user_group_id = Aws.Util.option_map (Aws.Json.lookup j "UserGroupId") String.of_json
    ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
    ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    }
end

module CacheClusterNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module PasswordListInput = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module ModifyUserMessage = struct
  type t =
    { user_id : String.t
    ; access_string : String.t option
    ; append_access_string : String.t option
    ; passwords : PasswordListInput.t
    ; no_password_required : Boolean.t option
    }

  let make
      ~user_id
      ?access_string
      ?append_access_string
      ?(passwords = [])
      ?no_password_required
      () =
    { user_id; access_string; append_access_string; passwords; no_password_required }

  let parse xml =
    Some
      { user_id =
          Aws.Xml.required
            "UserId"
            (Aws.Util.option_bind (Aws.Xml.member "UserId" xml) String.parse)
      ; access_string =
          Aws.Util.option_bind (Aws.Xml.member "AccessString" xml) String.parse
      ; append_access_string =
          Aws.Util.option_bind (Aws.Xml.member "AppendAccessString" xml) String.parse
      ; passwords =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "Passwords" xml)
               PasswordListInput.parse)
      ; no_password_required =
          Aws.Util.option_bind (Aws.Xml.member "NoPasswordRequired" xml) Boolean.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.no_password_required (fun f ->
               Aws.Query.Pair ("NoPasswordRequired", Boolean.to_query f))
         ; Some
             (Aws.Query.Pair ("Passwords.member", PasswordListInput.to_query v.passwords))
         ; Aws.Util.option_map v.append_access_string (fun f ->
               Aws.Query.Pair ("AppendAccessString", String.to_query f))
         ; Aws.Util.option_map v.access_string (fun f ->
               Aws.Query.Pair ("AccessString", String.to_query f))
         ; Some (Aws.Query.Pair ("UserId", String.to_query v.user_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.no_password_required (fun f ->
               "NoPasswordRequired", Boolean.to_json f)
         ; Some ("Passwords", PasswordListInput.to_json v.passwords)
         ; Aws.Util.option_map v.append_access_string (fun f ->
               "AppendAccessString", String.to_json f)
         ; Aws.Util.option_map v.access_string (fun f -> "AccessString", String.to_json f)
         ; Some ("UserId", String.to_json v.user_id)
         ])

  let of_json j =
    { user_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UserId"))
    ; access_string =
        Aws.Util.option_map (Aws.Json.lookup j "AccessString") String.of_json
    ; append_access_string =
        Aws.Util.option_map (Aws.Json.lookup j "AppendAccessString") String.of_json
    ; passwords =
        PasswordListInput.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Passwords"))
    ; no_password_required =
        Aws.Util.option_map (Aws.Json.lookup j "NoPasswordRequired") Boolean.of_json
    }
end

module InvalidParameterCombinationException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module AuthorizeCacheSecurityGroupIngressResult = struct
  type t = { cache_security_group : CacheSecurityGroup.t option }

  let make ?cache_security_group () = { cache_security_group }

  let parse xml =
    Some
      { cache_security_group =
          Aws.Util.option_bind
            (Aws.Xml.member "CacheSecurityGroup" xml)
            CacheSecurityGroup.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cache_security_group (fun f ->
               Aws.Query.Pair ("CacheSecurityGroup", CacheSecurityGroup.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cache_security_group (fun f ->
               "CacheSecurityGroup", CacheSecurityGroup.to_json f)
         ])

  let of_json j =
    { cache_security_group =
        Aws.Util.option_map
          (Aws.Json.lookup j "CacheSecurityGroup")
          CacheSecurityGroup.of_json
    }
end

module InvalidVPCNetworkStateFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module SubnetInUse = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module NodeGroupConfigurationList = struct
  type t = NodeGroupConfiguration.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map
         NodeGroupConfiguration.parse
         (Aws.Xml.members "NodeGroupConfiguration" xml))

  let to_query v = Aws.Query.to_query_list NodeGroupConfiguration.to_query v

  let to_json v = `List (List.map NodeGroupConfiguration.to_json v)

  let of_json j = Aws.Json.to_list NodeGroupConfiguration.of_json j
end

module BatchStopUpdateActionMessage = struct
  type t =
    { replication_group_ids : ReplicationGroupIdList.t
    ; cache_cluster_ids : CacheClusterIdList.t
    ; service_update_name : String.t
    }

  let make ?(replication_group_ids = []) ?(cache_cluster_ids = []) ~service_update_name ()
      =
    { replication_group_ids; cache_cluster_ids; service_update_name }

  let parse xml =
    Some
      { replication_group_ids =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ReplicationGroupIds" xml)
               ReplicationGroupIdList.parse)
      ; cache_cluster_ids =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "CacheClusterIds" xml)
               CacheClusterIdList.parse)
      ; service_update_name =
          Aws.Xml.required
            "ServiceUpdateName"
            (Aws.Util.option_bind (Aws.Xml.member "ServiceUpdateName" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("ServiceUpdateName", String.to_query v.service_update_name))
         ; Some
             (Aws.Query.Pair
                ("CacheClusterIds.member", CacheClusterIdList.to_query v.cache_cluster_ids))
         ; Some
             (Aws.Query.Pair
                ( "ReplicationGroupIds.member"
                , ReplicationGroupIdList.to_query v.replication_group_ids ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("ServiceUpdateName", String.to_json v.service_update_name)
         ; Some ("CacheClusterIds", CacheClusterIdList.to_json v.cache_cluster_ids)
         ; Some
             ( "ReplicationGroupIds"
             , ReplicationGroupIdList.to_json v.replication_group_ids )
         ])

  let of_json j =
    { replication_group_ids =
        ReplicationGroupIdList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicationGroupIds"))
    ; cache_cluster_ids =
        CacheClusterIdList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheClusterIds"))
    ; service_update_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ServiceUpdateName"))
    }
end

module CacheSubnetQuotaExceededFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module AddTagsToResourceMessage = struct
  type t =
    { resource_name : String.t
    ; tags : TagList.t
    }

  let make ~resource_name ~tags () = { resource_name; tags }

  let parse xml =
    Some
      { resource_name =
          Aws.Xml.required
            "ResourceName"
            (Aws.Util.option_bind (Aws.Xml.member "ResourceName" xml) String.parse)
      ; tags =
          Aws.Xml.required
            "Tags"
            (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Some (Aws.Query.Pair ("ResourceName", String.to_query v.resource_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Tags", TagList.to_json v.tags)
         ; Some ("ResourceName", String.to_json v.resource_name)
         ])

  let of_json j =
    { resource_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceName"))
    ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
    }
end

module CreateCacheSecurityGroupMessage = struct
  type t =
    { cache_security_group_name : String.t
    ; description : String.t
    }

  let make ~cache_security_group_name ~description () =
    { cache_security_group_name; description }

  let parse xml =
    Some
      { cache_security_group_name =
          Aws.Xml.required
            "CacheSecurityGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "CacheSecurityGroupName" xml)
               String.parse)
      ; description =
          Aws.Xml.required
            "Description"
            (Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Description", String.to_query v.description))
         ; Some
             (Aws.Query.Pair
                ("CacheSecurityGroupName", String.to_query v.cache_security_group_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Description", String.to_json v.description)
         ; Some ("CacheSecurityGroupName", String.to_json v.cache_security_group_name)
         ])

  let of_json j =
    { cache_security_group_name =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheSecurityGroupName"))
    ; description =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Description"))
    }
end

module UserAlreadyExistsFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module InvalidSubnet = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module InsufficientCacheClusterCapacityFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module InvalidKMSKeyFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module DeleteSnapshotResult = struct
  type t = { snapshot : Snapshot.t option }

  let make ?snapshot () = { snapshot }

  let parse xml =
    Some
      { snapshot = Aws.Util.option_bind (Aws.Xml.member "Snapshot" xml) Snapshot.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.snapshot (fun f ->
               Aws.Query.Pair ("Snapshot", Snapshot.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.snapshot (fun f -> "Snapshot", Snapshot.to_json f) ])

  let of_json j =
    { snapshot = Aws.Util.option_map (Aws.Json.lookup j "Snapshot") Snapshot.of_json }
end

module TagQuotaPerResourceExceeded = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module InvalidParameterValueException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "message") String.of_json }
end

module DescribeEngineDefaultParametersResult = struct
  type t = { engine_defaults : EngineDefaults.t }

  let make ~engine_defaults () = { engine_defaults }

  let parse xml =
    Some
      { engine_defaults =
          Aws.Xml.required
            "EngineDefaults"
            (Aws.Util.option_bind
               (Aws.Xml.member "EngineDefaults" xml)
               EngineDefaults.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("EngineDefaults", EngineDefaults.to_query v.engine_defaults))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("EngineDefaults", EngineDefaults.to_json v.engine_defaults) ])

  let of_json j =
    { engine_defaults =
        EngineDefaults.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "EngineDefaults"))
    }
end

module CreateCacheSubnetGroupMessage = struct
  type t =
    { cache_subnet_group_name : String.t
    ; cache_subnet_group_description : String.t
    ; subnet_ids : SubnetIdentifierList.t
    }

  let make ~cache_subnet_group_name ~cache_subnet_group_description ~subnet_ids () =
    { cache_subnet_group_name; cache_subnet_group_description; subnet_ids }

  let parse xml =
    Some
      { cache_subnet_group_name =
          Aws.Xml.required
            "CacheSubnetGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "CacheSubnetGroupName" xml)
               String.parse)
      ; cache_subnet_group_description =
          Aws.Xml.required
            "CacheSubnetGroupDescription"
            (Aws.Util.option_bind
               (Aws.Xml.member "CacheSubnetGroupDescription" xml)
               String.parse)
      ; subnet_ids =
          Aws.Xml.required
            "SubnetIds"
            (Aws.Util.option_bind
               (Aws.Xml.member "SubnetIds" xml)
               SubnetIdentifierList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("SubnetIds.member", SubnetIdentifierList.to_query v.subnet_ids))
         ; Some
             (Aws.Query.Pair
                ( "CacheSubnetGroupDescription"
                , String.to_query v.cache_subnet_group_description ))
         ; Some
             (Aws.Query.Pair
                ("CacheSubnetGroupName", String.to_query v.cache_subnet_group_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("SubnetIds", SubnetIdentifierList.to_json v.subnet_ids)
         ; Some
             ( "CacheSubnetGroupDescription"
             , String.to_json v.cache_subnet_group_description )
         ; Some ("CacheSubnetGroupName", String.to_json v.cache_subnet_group_name)
         ])

  let of_json j =
    { cache_subnet_group_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheSubnetGroupName"))
    ; cache_subnet_group_description =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheSubnetGroupDescription"))
    ; subnet_ids =
        SubnetIdentifierList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "SubnetIds"))
    }
end

module CacheParameterGroupQuotaExceededFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ReplicationGroupNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module CreateReplicationGroupMessage = struct
  type t =
    { replication_group_id : String.t
    ; replication_group_description : String.t
    ; global_replication_group_id : String.t option
    ; primary_cluster_id : String.t option
    ; automatic_failover_enabled : Boolean.t option
    ; multi_a_z_enabled : Boolean.t option
    ; num_cache_clusters : Integer.t option
    ; preferred_cache_cluster_a_zs : AvailabilityZonesList.t
    ; num_node_groups : Integer.t option
    ; replicas_per_node_group : Integer.t option
    ; node_group_configuration : NodeGroupConfigurationList.t
    ; cache_node_type : String.t option
    ; engine : String.t option
    ; engine_version : String.t option
    ; cache_parameter_group_name : String.t option
    ; cache_subnet_group_name : String.t option
    ; cache_security_group_names : CacheSecurityGroupNameList.t
    ; security_group_ids : SecurityGroupIdsList.t
    ; tags : TagList.t
    ; snapshot_arns : SnapshotArnsList.t
    ; snapshot_name : String.t option
    ; preferred_maintenance_window : String.t option
    ; port : Integer.t option
    ; notification_topic_arn : String.t option
    ; auto_minor_version_upgrade : Boolean.t option
    ; snapshot_retention_limit : Integer.t option
    ; snapshot_window : String.t option
    ; auth_token : String.t option
    ; transit_encryption_enabled : Boolean.t option
    ; at_rest_encryption_enabled : Boolean.t option
    ; kms_key_id : String.t option
    ; user_group_ids : UserGroupIdListInput.t
    }

  let make
      ~replication_group_id
      ~replication_group_description
      ?global_replication_group_id
      ?primary_cluster_id
      ?automatic_failover_enabled
      ?multi_a_z_enabled
      ?num_cache_clusters
      ?(preferred_cache_cluster_a_zs = [])
      ?num_node_groups
      ?replicas_per_node_group
      ?(node_group_configuration = [])
      ?cache_node_type
      ?engine
      ?engine_version
      ?cache_parameter_group_name
      ?cache_subnet_group_name
      ?(cache_security_group_names = [])
      ?(security_group_ids = [])
      ?(tags = [])
      ?(snapshot_arns = [])
      ?snapshot_name
      ?preferred_maintenance_window
      ?port
      ?notification_topic_arn
      ?auto_minor_version_upgrade
      ?snapshot_retention_limit
      ?snapshot_window
      ?auth_token
      ?transit_encryption_enabled
      ?at_rest_encryption_enabled
      ?kms_key_id
      ?(user_group_ids = [])
      () =
    { replication_group_id
    ; replication_group_description
    ; global_replication_group_id
    ; primary_cluster_id
    ; automatic_failover_enabled
    ; multi_a_z_enabled
    ; num_cache_clusters
    ; preferred_cache_cluster_a_zs
    ; num_node_groups
    ; replicas_per_node_group
    ; node_group_configuration
    ; cache_node_type
    ; engine
    ; engine_version
    ; cache_parameter_group_name
    ; cache_subnet_group_name
    ; cache_security_group_names
    ; security_group_ids
    ; tags
    ; snapshot_arns
    ; snapshot_name
    ; preferred_maintenance_window
    ; port
    ; notification_topic_arn
    ; auto_minor_version_upgrade
    ; snapshot_retention_limit
    ; snapshot_window
    ; auth_token
    ; transit_encryption_enabled
    ; at_rest_encryption_enabled
    ; kms_key_id
    ; user_group_ids
    }

  let parse xml =
    Some
      { replication_group_id =
          Aws.Xml.required
            "ReplicationGroupId"
            (Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse)
      ; replication_group_description =
          Aws.Xml.required
            "ReplicationGroupDescription"
            (Aws.Util.option_bind
               (Aws.Xml.member "ReplicationGroupDescription" xml)
               String.parse)
      ; global_replication_group_id =
          Aws.Util.option_bind
            (Aws.Xml.member "GlobalReplicationGroupId" xml)
            String.parse
      ; primary_cluster_id =
          Aws.Util.option_bind (Aws.Xml.member "PrimaryClusterId" xml) String.parse
      ; automatic_failover_enabled =
          Aws.Util.option_bind
            (Aws.Xml.member "AutomaticFailoverEnabled" xml)
            Boolean.parse
      ; multi_a_z_enabled =
          Aws.Util.option_bind (Aws.Xml.member "MultiAZEnabled" xml) Boolean.parse
      ; num_cache_clusters =
          Aws.Util.option_bind (Aws.Xml.member "NumCacheClusters" xml) Integer.parse
      ; preferred_cache_cluster_a_zs =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "PreferredCacheClusterAZs" xml)
               AvailabilityZonesList.parse)
      ; num_node_groups =
          Aws.Util.option_bind (Aws.Xml.member "NumNodeGroups" xml) Integer.parse
      ; replicas_per_node_group =
          Aws.Util.option_bind (Aws.Xml.member "ReplicasPerNodeGroup" xml) Integer.parse
      ; node_group_configuration =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "NodeGroupConfiguration" xml)
               NodeGroupConfigurationList.parse)
      ; cache_node_type =
          Aws.Util.option_bind (Aws.Xml.member "CacheNodeType" xml) String.parse
      ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
      ; engine_version =
          Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
      ; cache_parameter_group_name =
          Aws.Util.option_bind (Aws.Xml.member "CacheParameterGroupName" xml) String.parse
      ; cache_subnet_group_name =
          Aws.Util.option_bind (Aws.Xml.member "CacheSubnetGroupName" xml) String.parse
      ; cache_security_group_names =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "CacheSecurityGroupNames" xml)
               CacheSecurityGroupNameList.parse)
      ; security_group_ids =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "SecurityGroupIds" xml)
               SecurityGroupIdsList.parse)
      ; tags =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
      ; snapshot_arns =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "SnapshotArns" xml)
               SnapshotArnsList.parse)
      ; snapshot_name =
          Aws.Util.option_bind (Aws.Xml.member "SnapshotName" xml) String.parse
      ; preferred_maintenance_window =
          Aws.Util.option_bind
            (Aws.Xml.member "PreferredMaintenanceWindow" xml)
            String.parse
      ; port = Aws.Util.option_bind (Aws.Xml.member "Port" xml) Integer.parse
      ; notification_topic_arn =
          Aws.Util.option_bind (Aws.Xml.member "NotificationTopicArn" xml) String.parse
      ; auto_minor_version_upgrade =
          Aws.Util.option_bind
            (Aws.Xml.member "AutoMinorVersionUpgrade" xml)
            Boolean.parse
      ; snapshot_retention_limit =
          Aws.Util.option_bind (Aws.Xml.member "SnapshotRetentionLimit" xml) Integer.parse
      ; snapshot_window =
          Aws.Util.option_bind (Aws.Xml.member "SnapshotWindow" xml) String.parse
      ; auth_token = Aws.Util.option_bind (Aws.Xml.member "AuthToken" xml) String.parse
      ; transit_encryption_enabled =
          Aws.Util.option_bind
            (Aws.Xml.member "TransitEncryptionEnabled" xml)
            Boolean.parse
      ; at_rest_encryption_enabled =
          Aws.Util.option_bind
            (Aws.Xml.member "AtRestEncryptionEnabled" xml)
            Boolean.parse
      ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse
      ; user_group_ids =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "UserGroupIds" xml)
               UserGroupIdListInput.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("UserGroupIds.member", UserGroupIdListInput.to_query v.user_group_ids))
         ; Aws.Util.option_map v.kms_key_id (fun f ->
               Aws.Query.Pair ("KmsKeyId", String.to_query f))
         ; Aws.Util.option_map v.at_rest_encryption_enabled (fun f ->
               Aws.Query.Pair ("AtRestEncryptionEnabled", Boolean.to_query f))
         ; Aws.Util.option_map v.transit_encryption_enabled (fun f ->
               Aws.Query.Pair ("TransitEncryptionEnabled", Boolean.to_query f))
         ; Aws.Util.option_map v.auth_token (fun f ->
               Aws.Query.Pair ("AuthToken", String.to_query f))
         ; Aws.Util.option_map v.snapshot_window (fun f ->
               Aws.Query.Pair ("SnapshotWindow", String.to_query f))
         ; Aws.Util.option_map v.snapshot_retention_limit (fun f ->
               Aws.Query.Pair ("SnapshotRetentionLimit", Integer.to_query f))
         ; Aws.Util.option_map v.auto_minor_version_upgrade (fun f ->
               Aws.Query.Pair ("AutoMinorVersionUpgrade", Boolean.to_query f))
         ; Aws.Util.option_map v.notification_topic_arn (fun f ->
               Aws.Query.Pair ("NotificationTopicArn", String.to_query f))
         ; Aws.Util.option_map v.port (fun f ->
               Aws.Query.Pair ("Port", Integer.to_query f))
         ; Aws.Util.option_map v.preferred_maintenance_window (fun f ->
               Aws.Query.Pair ("PreferredMaintenanceWindow", String.to_query f))
         ; Aws.Util.option_map v.snapshot_name (fun f ->
               Aws.Query.Pair ("SnapshotName", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("SnapshotArns.member", SnapshotArnsList.to_query v.snapshot_arns))
         ; Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
         ; Some
             (Aws.Query.Pair
                ( "SecurityGroupIds.member"
                , SecurityGroupIdsList.to_query v.security_group_ids ))
         ; Some
             (Aws.Query.Pair
                ( "CacheSecurityGroupNames.member"
                , CacheSecurityGroupNameList.to_query v.cache_security_group_names ))
         ; Aws.Util.option_map v.cache_subnet_group_name (fun f ->
               Aws.Query.Pair ("CacheSubnetGroupName", String.to_query f))
         ; Aws.Util.option_map v.cache_parameter_group_name (fun f ->
               Aws.Query.Pair ("CacheParameterGroupName", String.to_query f))
         ; Aws.Util.option_map v.engine_version (fun f ->
               Aws.Query.Pair ("EngineVersion", String.to_query f))
         ; Aws.Util.option_map v.engine (fun f ->
               Aws.Query.Pair ("Engine", String.to_query f))
         ; Aws.Util.option_map v.cache_node_type (fun f ->
               Aws.Query.Pair ("CacheNodeType", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "NodeGroupConfiguration.member"
                , NodeGroupConfigurationList.to_query v.node_group_configuration ))
         ; Aws.Util.option_map v.replicas_per_node_group (fun f ->
               Aws.Query.Pair ("ReplicasPerNodeGroup", Integer.to_query f))
         ; Aws.Util.option_map v.num_node_groups (fun f ->
               Aws.Query.Pair ("NumNodeGroups", Integer.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "PreferredCacheClusterAZs.member"
                , AvailabilityZonesList.to_query v.preferred_cache_cluster_a_zs ))
         ; Aws.Util.option_map v.num_cache_clusters (fun f ->
               Aws.Query.Pair ("NumCacheClusters", Integer.to_query f))
         ; Aws.Util.option_map v.multi_a_z_enabled (fun f ->
               Aws.Query.Pair ("MultiAZEnabled", Boolean.to_query f))
         ; Aws.Util.option_map v.automatic_failover_enabled (fun f ->
               Aws.Query.Pair ("AutomaticFailoverEnabled", Boolean.to_query f))
         ; Aws.Util.option_map v.primary_cluster_id (fun f ->
               Aws.Query.Pair ("PrimaryClusterId", String.to_query f))
         ; Aws.Util.option_map v.global_replication_group_id (fun f ->
               Aws.Query.Pair ("GlobalReplicationGroupId", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "ReplicationGroupDescription"
                , String.to_query v.replication_group_description ))
         ; Some
             (Aws.Query.Pair ("ReplicationGroupId", String.to_query v.replication_group_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("UserGroupIds", UserGroupIdListInput.to_json v.user_group_ids)
         ; Aws.Util.option_map v.kms_key_id (fun f -> "KmsKeyId", String.to_json f)
         ; Aws.Util.option_map v.at_rest_encryption_enabled (fun f ->
               "AtRestEncryptionEnabled", Boolean.to_json f)
         ; Aws.Util.option_map v.transit_encryption_enabled (fun f ->
               "TransitEncryptionEnabled", Boolean.to_json f)
         ; Aws.Util.option_map v.auth_token (fun f -> "AuthToken", String.to_json f)
         ; Aws.Util.option_map v.snapshot_window (fun f ->
               "SnapshotWindow", String.to_json f)
         ; Aws.Util.option_map v.snapshot_retention_limit (fun f ->
               "SnapshotRetentionLimit", Integer.to_json f)
         ; Aws.Util.option_map v.auto_minor_version_upgrade (fun f ->
               "AutoMinorVersionUpgrade", Boolean.to_json f)
         ; Aws.Util.option_map v.notification_topic_arn (fun f ->
               "NotificationTopicArn", String.to_json f)
         ; Aws.Util.option_map v.port (fun f -> "Port", Integer.to_json f)
         ; Aws.Util.option_map v.preferred_maintenance_window (fun f ->
               "PreferredMaintenanceWindow", String.to_json f)
         ; Aws.Util.option_map v.snapshot_name (fun f -> "SnapshotName", String.to_json f)
         ; Some ("SnapshotArns", SnapshotArnsList.to_json v.snapshot_arns)
         ; Some ("Tags", TagList.to_json v.tags)
         ; Some ("SecurityGroupIds", SecurityGroupIdsList.to_json v.security_group_ids)
         ; Some
             ( "CacheSecurityGroupNames"
             , CacheSecurityGroupNameList.to_json v.cache_security_group_names )
         ; Aws.Util.option_map v.cache_subnet_group_name (fun f ->
               "CacheSubnetGroupName", String.to_json f)
         ; Aws.Util.option_map v.cache_parameter_group_name (fun f ->
               "CacheParameterGroupName", String.to_json f)
         ; Aws.Util.option_map v.engine_version (fun f ->
               "EngineVersion", String.to_json f)
         ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
         ; Aws.Util.option_map v.cache_node_type (fun f ->
               "CacheNodeType", String.to_json f)
         ; Some
             ( "NodeGroupConfiguration"
             , NodeGroupConfigurationList.to_json v.node_group_configuration )
         ; Aws.Util.option_map v.replicas_per_node_group (fun f ->
               "ReplicasPerNodeGroup", Integer.to_json f)
         ; Aws.Util.option_map v.num_node_groups (fun f ->
               "NumNodeGroups", Integer.to_json f)
         ; Some
             ( "PreferredCacheClusterAZs"
             , AvailabilityZonesList.to_json v.preferred_cache_cluster_a_zs )
         ; Aws.Util.option_map v.num_cache_clusters (fun f ->
               "NumCacheClusters", Integer.to_json f)
         ; Aws.Util.option_map v.multi_a_z_enabled (fun f ->
               "MultiAZEnabled", Boolean.to_json f)
         ; Aws.Util.option_map v.automatic_failover_enabled (fun f ->
               "AutomaticFailoverEnabled", Boolean.to_json f)
         ; Aws.Util.option_map v.primary_cluster_id (fun f ->
               "PrimaryClusterId", String.to_json f)
         ; Aws.Util.option_map v.global_replication_group_id (fun f ->
               "GlobalReplicationGroupId", String.to_json f)
         ; Some
             ( "ReplicationGroupDescription"
             , String.to_json v.replication_group_description )
         ; Some ("ReplicationGroupId", String.to_json v.replication_group_id)
         ])

  let of_json j =
    { replication_group_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicationGroupId"))
    ; replication_group_description =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicationGroupDescription"))
    ; global_replication_group_id =
        Aws.Util.option_map (Aws.Json.lookup j "GlobalReplicationGroupId") String.of_json
    ; primary_cluster_id =
        Aws.Util.option_map (Aws.Json.lookup j "PrimaryClusterId") String.of_json
    ; automatic_failover_enabled =
        Aws.Util.option_map (Aws.Json.lookup j "AutomaticFailoverEnabled") Boolean.of_json
    ; multi_a_z_enabled =
        Aws.Util.option_map (Aws.Json.lookup j "MultiAZEnabled") Boolean.of_json
    ; num_cache_clusters =
        Aws.Util.option_map (Aws.Json.lookup j "NumCacheClusters") Integer.of_json
    ; preferred_cache_cluster_a_zs =
        AvailabilityZonesList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "PreferredCacheClusterAZs"))
    ; num_node_groups =
        Aws.Util.option_map (Aws.Json.lookup j "NumNodeGroups") Integer.of_json
    ; replicas_per_node_group =
        Aws.Util.option_map (Aws.Json.lookup j "ReplicasPerNodeGroup") Integer.of_json
    ; node_group_configuration =
        NodeGroupConfigurationList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "NodeGroupConfiguration"))
    ; cache_node_type =
        Aws.Util.option_map (Aws.Json.lookup j "CacheNodeType") String.of_json
    ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
    ; engine_version =
        Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
    ; cache_parameter_group_name =
        Aws.Util.option_map (Aws.Json.lookup j "CacheParameterGroupName") String.of_json
    ; cache_subnet_group_name =
        Aws.Util.option_map (Aws.Json.lookup j "CacheSubnetGroupName") String.of_json
    ; cache_security_group_names =
        CacheSecurityGroupNameList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheSecurityGroupNames"))
    ; security_group_ids =
        SecurityGroupIdsList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroupIds"))
    ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
    ; snapshot_arns =
        SnapshotArnsList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "SnapshotArns"))
    ; snapshot_name =
        Aws.Util.option_map (Aws.Json.lookup j "SnapshotName") String.of_json
    ; preferred_maintenance_window =
        Aws.Util.option_map
          (Aws.Json.lookup j "PreferredMaintenanceWindow")
          String.of_json
    ; port = Aws.Util.option_map (Aws.Json.lookup j "Port") Integer.of_json
    ; notification_topic_arn =
        Aws.Util.option_map (Aws.Json.lookup j "NotificationTopicArn") String.of_json
    ; auto_minor_version_upgrade =
        Aws.Util.option_map (Aws.Json.lookup j "AutoMinorVersionUpgrade") Boolean.of_json
    ; snapshot_retention_limit =
        Aws.Util.option_map (Aws.Json.lookup j "SnapshotRetentionLimit") Integer.of_json
    ; snapshot_window =
        Aws.Util.option_map (Aws.Json.lookup j "SnapshotWindow") String.of_json
    ; auth_token = Aws.Util.option_map (Aws.Json.lookup j "AuthToken") String.of_json
    ; transit_encryption_enabled =
        Aws.Util.option_map (Aws.Json.lookup j "TransitEncryptionEnabled") Boolean.of_json
    ; at_rest_encryption_enabled =
        Aws.Util.option_map (Aws.Json.lookup j "AtRestEncryptionEnabled") Boolean.of_json
    ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
    ; user_group_ids =
        UserGroupIdListInput.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "UserGroupIds"))
    }
end

module InvalidReplicationGroupStateFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module DecreaseNodeGroupsInGlobalReplicationGroupMessage = struct
  type t =
    { global_replication_group_id : String.t
    ; node_group_count : Integer.t
    ; global_node_groups_to_remove : GlobalNodeGroupIdList.t
    ; global_node_groups_to_retain : GlobalNodeGroupIdList.t
    ; apply_immediately : Boolean.t
    }

  let make
      ~global_replication_group_id
      ~node_group_count
      ?(global_node_groups_to_remove = [])
      ?(global_node_groups_to_retain = [])
      ~apply_immediately
      () =
    { global_replication_group_id
    ; node_group_count
    ; global_node_groups_to_remove
    ; global_node_groups_to_retain
    ; apply_immediately
    }

  let parse xml =
    Some
      { global_replication_group_id =
          Aws.Xml.required
            "GlobalReplicationGroupId"
            (Aws.Util.option_bind
               (Aws.Xml.member "GlobalReplicationGroupId" xml)
               String.parse)
      ; node_group_count =
          Aws.Xml.required
            "NodeGroupCount"
            (Aws.Util.option_bind (Aws.Xml.member "NodeGroupCount" xml) Integer.parse)
      ; global_node_groups_to_remove =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "GlobalNodeGroupsToRemove" xml)
               GlobalNodeGroupIdList.parse)
      ; global_node_groups_to_retain =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "GlobalNodeGroupsToRetain" xml)
               GlobalNodeGroupIdList.parse)
      ; apply_immediately =
          Aws.Xml.required
            "ApplyImmediately"
            (Aws.Util.option_bind (Aws.Xml.member "ApplyImmediately" xml) Boolean.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("ApplyImmediately", Boolean.to_query v.apply_immediately))
         ; Some
             (Aws.Query.Pair
                ( "GlobalNodeGroupsToRetain.member"
                , GlobalNodeGroupIdList.to_query v.global_node_groups_to_retain ))
         ; Some
             (Aws.Query.Pair
                ( "GlobalNodeGroupsToRemove.member"
                , GlobalNodeGroupIdList.to_query v.global_node_groups_to_remove ))
         ; Some (Aws.Query.Pair ("NodeGroupCount", Integer.to_query v.node_group_count))
         ; Some
             (Aws.Query.Pair
                ("GlobalReplicationGroupId", String.to_query v.global_replication_group_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("ApplyImmediately", Boolean.to_json v.apply_immediately)
         ; Some
             ( "GlobalNodeGroupsToRetain"
             , GlobalNodeGroupIdList.to_json v.global_node_groups_to_retain )
         ; Some
             ( "GlobalNodeGroupsToRemove"
             , GlobalNodeGroupIdList.to_json v.global_node_groups_to_remove )
         ; Some ("NodeGroupCount", Integer.to_json v.node_group_count)
         ; Some ("GlobalReplicationGroupId", String.to_json v.global_replication_group_id)
         ])

  let of_json j =
    { global_replication_group_id =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "GlobalReplicationGroupId"))
    ; node_group_count =
        Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "NodeGroupCount"))
    ; global_node_groups_to_remove =
        GlobalNodeGroupIdList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "GlobalNodeGroupsToRemove"))
    ; global_node_groups_to_retain =
        GlobalNodeGroupIdList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "GlobalNodeGroupsToRetain"))
    ; apply_immediately =
        Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ApplyImmediately"))
    }
end

module ReservedCacheNodeNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ModifyCacheSubnetGroupMessage = struct
  type t =
    { cache_subnet_group_name : String.t
    ; cache_subnet_group_description : String.t option
    ; subnet_ids : SubnetIdentifierList.t
    }

  let make ~cache_subnet_group_name ?cache_subnet_group_description ?(subnet_ids = []) ()
      =
    { cache_subnet_group_name; cache_subnet_group_description; subnet_ids }

  let parse xml =
    Some
      { cache_subnet_group_name =
          Aws.Xml.required
            "CacheSubnetGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "CacheSubnetGroupName" xml)
               String.parse)
      ; cache_subnet_group_description =
          Aws.Util.option_bind
            (Aws.Xml.member "CacheSubnetGroupDescription" xml)
            String.parse
      ; subnet_ids =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "SubnetIds" xml)
               SubnetIdentifierList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("SubnetIds.member", SubnetIdentifierList.to_query v.subnet_ids))
         ; Aws.Util.option_map v.cache_subnet_group_description (fun f ->
               Aws.Query.Pair ("CacheSubnetGroupDescription", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("CacheSubnetGroupName", String.to_query v.cache_subnet_group_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("SubnetIds", SubnetIdentifierList.to_json v.subnet_ids)
         ; Aws.Util.option_map v.cache_subnet_group_description (fun f ->
               "CacheSubnetGroupDescription", String.to_json f)
         ; Some ("CacheSubnetGroupName", String.to_json v.cache_subnet_group_name)
         ])

  let of_json j =
    { cache_subnet_group_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheSubnetGroupName"))
    ; cache_subnet_group_description =
        Aws.Util.option_map
          (Aws.Json.lookup j "CacheSubnetGroupDescription")
          String.of_json
    ; subnet_ids =
        SubnetIdentifierList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "SubnetIds"))
    }
end

module RevokeCacheSecurityGroupIngressResult = struct
  type t = { cache_security_group : CacheSecurityGroup.t option }

  let make ?cache_security_group () = { cache_security_group }

  let parse xml =
    Some
      { cache_security_group =
          Aws.Util.option_bind
            (Aws.Xml.member "CacheSecurityGroup" xml)
            CacheSecurityGroup.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cache_security_group (fun f ->
               Aws.Query.Pair ("CacheSecurityGroup", CacheSecurityGroup.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cache_security_group (fun f ->
               "CacheSecurityGroup", CacheSecurityGroup.to_json f)
         ])

  let of_json j =
    { cache_security_group =
        Aws.Util.option_map
          (Aws.Json.lookup j "CacheSecurityGroup")
          CacheSecurityGroup.of_json
    }
end

module CreateUserGroupMessage = struct
  type t =
    { user_group_id : String.t
    ; engine : String.t
    ; user_ids : UserIdListInput.t
    }

  let make ~user_group_id ~engine ?(user_ids = []) () =
    { user_group_id; engine; user_ids }

  let parse xml =
    Some
      { user_group_id =
          Aws.Xml.required
            "UserGroupId"
            (Aws.Util.option_bind (Aws.Xml.member "UserGroupId" xml) String.parse)
      ; engine =
          Aws.Xml.required
            "Engine"
            (Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse)
      ; user_ids =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "UserIds" xml) UserIdListInput.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("UserIds.member", UserIdListInput.to_query v.user_ids))
         ; Some (Aws.Query.Pair ("Engine", String.to_query v.engine))
         ; Some (Aws.Query.Pair ("UserGroupId", String.to_query v.user_group_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("UserIds", UserIdListInput.to_json v.user_ids)
         ; Some ("Engine", String.to_json v.engine)
         ; Some ("UserGroupId", String.to_json v.user_group_id)
         ])

  let of_json j =
    { user_group_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UserGroupId"))
    ; engine = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Engine"))
    ; user_ids =
        UserIdListInput.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UserIds"))
    }
end

module CacheSecurityGroupAlreadyExistsFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module CacheSubnetGroupNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module DescribeCacheSecurityGroupsMessage = struct
  type t =
    { cache_security_group_name : String.t option
    ; max_records : Integer.t option
    ; marker : String.t option
    }

  let make ?cache_security_group_name ?max_records ?marker () =
    { cache_security_group_name; max_records; marker }

  let parse xml =
    Some
      { cache_security_group_name =
          Aws.Util.option_bind (Aws.Xml.member "CacheSecurityGroupName" xml) String.parse
      ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ; Aws.Util.option_map v.max_records (fun f ->
               Aws.Query.Pair ("MaxRecords", Integer.to_query f))
         ; Aws.Util.option_map v.cache_security_group_name (fun f ->
               Aws.Query.Pair ("CacheSecurityGroupName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
         ; Aws.Util.option_map v.cache_security_group_name (fun f ->
               "CacheSecurityGroupName", String.to_json f)
         ])

  let of_json j =
    { cache_security_group_name =
        Aws.Util.option_map (Aws.Json.lookup j "CacheSecurityGroupName") String.of_json
    ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
    ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    }
end

module TestFailoverMessage = struct
  type t =
    { replication_group_id : String.t
    ; node_group_id : String.t
    }

  let make ~replication_group_id ~node_group_id () =
    { replication_group_id; node_group_id }

  let parse xml =
    Some
      { replication_group_id =
          Aws.Xml.required
            "ReplicationGroupId"
            (Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse)
      ; node_group_id =
          Aws.Xml.required
            "NodeGroupId"
            (Aws.Util.option_bind (Aws.Xml.member "NodeGroupId" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("NodeGroupId", String.to_query v.node_group_id))
         ; Some
             (Aws.Query.Pair ("ReplicationGroupId", String.to_query v.replication_group_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("NodeGroupId", String.to_json v.node_group_id)
         ; Some ("ReplicationGroupId", String.to_json v.replication_group_id)
         ])

  let of_json j =
    { replication_group_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicationGroupId"))
    ; node_group_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "NodeGroupId"))
    }
end

module UserQuotaExceededFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module AuthorizationAlreadyExistsFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module IncreaseReplicaCountResult = struct
  type t = { replication_group : ReplicationGroup.t option }

  let make ?replication_group () = { replication_group }

  let parse xml =
    Some
      { replication_group =
          Aws.Util.option_bind
            (Aws.Xml.member "ReplicationGroup" xml)
            ReplicationGroup.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.replication_group (fun f ->
               Aws.Query.Pair ("ReplicationGroup", ReplicationGroup.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.replication_group (fun f ->
               "ReplicationGroup", ReplicationGroup.to_json f)
         ])

  let of_json j =
    { replication_group =
        Aws.Util.option_map
          (Aws.Json.lookup j "ReplicationGroup")
          ReplicationGroup.of_json
    }
end

module PurchaseReservedCacheNodesOfferingMessage = struct
  type t =
    { reserved_cache_nodes_offering_id : String.t
    ; reserved_cache_node_id : String.t option
    ; cache_node_count : Integer.t option
    }

  let make ~reserved_cache_nodes_offering_id ?reserved_cache_node_id ?cache_node_count ()
      =
    { reserved_cache_nodes_offering_id; reserved_cache_node_id; cache_node_count }

  let parse xml =
    Some
      { reserved_cache_nodes_offering_id =
          Aws.Xml.required
            "ReservedCacheNodesOfferingId"
            (Aws.Util.option_bind
               (Aws.Xml.member "ReservedCacheNodesOfferingId" xml)
               String.parse)
      ; reserved_cache_node_id =
          Aws.Util.option_bind (Aws.Xml.member "ReservedCacheNodeId" xml) String.parse
      ; cache_node_count =
          Aws.Util.option_bind (Aws.Xml.member "CacheNodeCount" xml) Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cache_node_count (fun f ->
               Aws.Query.Pair ("CacheNodeCount", Integer.to_query f))
         ; Aws.Util.option_map v.reserved_cache_node_id (fun f ->
               Aws.Query.Pair ("ReservedCacheNodeId", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "ReservedCacheNodesOfferingId"
                , String.to_query v.reserved_cache_nodes_offering_id ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cache_node_count (fun f ->
               "CacheNodeCount", Integer.to_json f)
         ; Aws.Util.option_map v.reserved_cache_node_id (fun f ->
               "ReservedCacheNodeId", String.to_json f)
         ; Some
             ( "ReservedCacheNodesOfferingId"
             , String.to_json v.reserved_cache_nodes_offering_id )
         ])

  let of_json j =
    { reserved_cache_nodes_offering_id =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ReservedCacheNodesOfferingId"))
    ; reserved_cache_node_id =
        Aws.Util.option_map (Aws.Json.lookup j "ReservedCacheNodeId") String.of_json
    ; cache_node_count =
        Aws.Util.option_map (Aws.Json.lookup j "CacheNodeCount") Integer.of_json
    }
end

module TagListMessage = struct
  type t = { tag_list : TagList.t }

  let make ?(tag_list = []) () = { tag_list }

  let parse xml =
    Some
      { tag_list =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "TagList" xml) TagList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("TagList.member", TagList.to_query v.tag_list)) ])

  let to_json v =
    `Assoc (Aws.Util.list_filter_opt [ Some ("TagList", TagList.to_json v.tag_list) ])

  let of_json j =
    { tag_list = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TagList")) }
end

module NodeGroupsPerReplicationGroupQuotaExceededFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module UserGroupAlreadyExistsFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module NodeQuotaForCustomerExceededFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module DecreaseReplicaCountResult = struct
  type t = { replication_group : ReplicationGroup.t option }

  let make ?replication_group () = { replication_group }

  let parse xml =
    Some
      { replication_group =
          Aws.Util.option_bind
            (Aws.Xml.member "ReplicationGroup" xml)
            ReplicationGroup.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.replication_group (fun f ->
               Aws.Query.Pair ("ReplicationGroup", ReplicationGroup.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.replication_group (fun f ->
               "ReplicationGroup", ReplicationGroup.to_json f)
         ])

  let of_json j =
    { replication_group =
        Aws.Util.option_map
          (Aws.Json.lookup j "ReplicationGroup")
          ReplicationGroup.of_json
    }
end

module DeleteCacheParameterGroupMessage = struct
  type t = { cache_parameter_group_name : String.t }

  let make ~cache_parameter_group_name () = { cache_parameter_group_name }

  let parse xml =
    Some
      { cache_parameter_group_name =
          Aws.Xml.required
            "CacheParameterGroupName"
            (Aws.Util.option_bind
               (Aws.Xml.member "CacheParameterGroupName" xml)
               String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("CacheParameterGroupName", String.to_query v.cache_parameter_group_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("CacheParameterGroupName", String.to_json v.cache_parameter_group_name) ])

  let of_json j =
    { cache_parameter_group_name =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheParameterGroupName"))
    }
end

module CompleteMigrationMessage = struct
  type t =
    { replication_group_id : String.t
    ; force : Boolean.t option
    }

  let make ~replication_group_id ?force () = { replication_group_id; force }

  let parse xml =
    Some
      { replication_group_id =
          Aws.Xml.required
            "ReplicationGroupId"
            (Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse)
      ; force = Aws.Util.option_bind (Aws.Xml.member "Force" xml) Boolean.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.force (fun f ->
               Aws.Query.Pair ("Force", Boolean.to_query f))
         ; Some
             (Aws.Query.Pair ("ReplicationGroupId", String.to_query v.replication_group_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.force (fun f -> "Force", Boolean.to_json f)
         ; Some ("ReplicationGroupId", String.to_json v.replication_group_id)
         ])

  let of_json j =
    { replication_group_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicationGroupId"))
    ; force = Aws.Util.option_map (Aws.Json.lookup j "Force") Boolean.of_json
    }
end

module DuplicateUserNameFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module CacheSecurityGroupNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module CacheSubnetGroupMessage = struct
  type t =
    { marker : String.t option
    ; cache_subnet_groups : CacheSubnetGroups.t
    }

  let make ?marker ?(cache_subnet_groups = []) () = { marker; cache_subnet_groups }

  let parse xml =
    Some
      { marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      ; cache_subnet_groups =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "CacheSubnetGroups" xml)
               CacheSubnetGroups.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "CacheSubnetGroups.member"
                , CacheSubnetGroups.to_query v.cache_subnet_groups ))
         ; Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("CacheSubnetGroups", CacheSubnetGroups.to_json v.cache_subnet_groups)
         ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ])

  let of_json j =
    { marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    ; cache_subnet_groups =
        CacheSubnetGroups.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheSubnetGroups"))
    }
end

module ReplicationGroupAlreadyUnderMigrationFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module DefaultUserAssociatedToUserGroupFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module AuthorizationNotFoundFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module DescribeGlobalReplicationGroupsMessage = struct
  type t =
    { global_replication_group_id : String.t option
    ; max_records : Integer.t option
    ; marker : String.t option
    ; show_member_info : Boolean.t option
    }

  let make ?global_replication_group_id ?max_records ?marker ?show_member_info () =
    { global_replication_group_id; max_records; marker; show_member_info }

  let parse xml =
    Some
      { global_replication_group_id =
          Aws.Util.option_bind
            (Aws.Xml.member "GlobalReplicationGroupId" xml)
            String.parse
      ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      ; show_member_info =
          Aws.Util.option_bind (Aws.Xml.member "ShowMemberInfo" xml) Boolean.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.show_member_info (fun f ->
               Aws.Query.Pair ("ShowMemberInfo", Boolean.to_query f))
         ; Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ; Aws.Util.option_map v.max_records (fun f ->
               Aws.Query.Pair ("MaxRecords", Integer.to_query f))
         ; Aws.Util.option_map v.global_replication_group_id (fun f ->
               Aws.Query.Pair ("GlobalReplicationGroupId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.show_member_info (fun f ->
               "ShowMemberInfo", Boolean.to_json f)
         ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
         ; Aws.Util.option_map v.global_replication_group_id (fun f ->
               "GlobalReplicationGroupId", String.to_json f)
         ])

  let of_json j =
    { global_replication_group_id =
        Aws.Util.option_map (Aws.Json.lookup j "GlobalReplicationGroupId") String.of_json
    ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
    ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    ; show_member_info =
        Aws.Util.option_map (Aws.Json.lookup j "ShowMemberInfo") Boolean.of_json
    }
end

module DeleteUserMessage = struct
  type t = { user_id : String.t }

  let make ~user_id () = { user_id }

  let parse xml =
    Some
      { user_id =
          Aws.Xml.required
            "UserId"
            (Aws.Util.option_bind (Aws.Xml.member "UserId" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("UserId", String.to_query v.user_id)) ])

  let to_json v =
    `Assoc (Aws.Util.list_filter_opt [ Some ("UserId", String.to_json v.user_id) ])

  let of_json j =
    { user_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UserId")) }
end

module DeleteCacheClusterResult = struct
  type t = { cache_cluster : CacheCluster.t option }

  let make ?cache_cluster () = { cache_cluster }

  let parse xml =
    Some
      { cache_cluster =
          Aws.Util.option_bind (Aws.Xml.member "CacheCluster" xml) CacheCluster.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cache_cluster (fun f ->
               Aws.Query.Pair ("CacheCluster", CacheCluster.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cache_cluster (fun f ->
               "CacheCluster", CacheCluster.to_json f)
         ])

  let of_json j =
    { cache_cluster =
        Aws.Util.option_map (Aws.Json.lookup j "CacheCluster") CacheCluster.of_json
    }
end

module CacheParameterGroupAlreadyExistsFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ModifyCacheSubnetGroupResult = struct
  type t = { cache_subnet_group : CacheSubnetGroup.t option }

  let make ?cache_subnet_group () = { cache_subnet_group }

  let parse xml =
    Some
      { cache_subnet_group =
          Aws.Util.option_bind
            (Aws.Xml.member "CacheSubnetGroup" xml)
            CacheSubnetGroup.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cache_subnet_group (fun f ->
               Aws.Query.Pair ("CacheSubnetGroup", CacheSubnetGroup.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.cache_subnet_group (fun f ->
               "CacheSubnetGroup", CacheSubnetGroup.to_json f)
         ])

  let of_json j =
    { cache_subnet_group =
        Aws.Util.option_map
          (Aws.Json.lookup j "CacheSubnetGroup")
          CacheSubnetGroup.of_json
    }
end

module CreateUserMessage = struct
  type t =
    { user_id : String.t
    ; user_name : String.t
    ; engine : String.t
    ; passwords : PasswordListInput.t
    ; access_string : String.t
    ; no_password_required : Boolean.t option
    }

  let make
      ~user_id
      ~user_name
      ~engine
      ?(passwords = [])
      ~access_string
      ?no_password_required
      () =
    { user_id; user_name; engine; passwords; access_string; no_password_required }

  let parse xml =
    Some
      { user_id =
          Aws.Xml.required
            "UserId"
            (Aws.Util.option_bind (Aws.Xml.member "UserId" xml) String.parse)
      ; user_name =
          Aws.Xml.required
            "UserName"
            (Aws.Util.option_bind (Aws.Xml.member "UserName" xml) String.parse)
      ; engine =
          Aws.Xml.required
            "Engine"
            (Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse)
      ; passwords =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "Passwords" xml)
               PasswordListInput.parse)
      ; access_string =
          Aws.Xml.required
            "AccessString"
            (Aws.Util.option_bind (Aws.Xml.member "AccessString" xml) String.parse)
      ; no_password_required =
          Aws.Util.option_bind (Aws.Xml.member "NoPasswordRequired" xml) Boolean.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.no_password_required (fun f ->
               Aws.Query.Pair ("NoPasswordRequired", Boolean.to_query f))
         ; Some (Aws.Query.Pair ("AccessString", String.to_query v.access_string))
         ; Some
             (Aws.Query.Pair ("Passwords.member", PasswordListInput.to_query v.passwords))
         ; Some (Aws.Query.Pair ("Engine", String.to_query v.engine))
         ; Some (Aws.Query.Pair ("UserName", String.to_query v.user_name))
         ; Some (Aws.Query.Pair ("UserId", String.to_query v.user_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.no_password_required (fun f ->
               "NoPasswordRequired", Boolean.to_json f)
         ; Some ("AccessString", String.to_json v.access_string)
         ; Some ("Passwords", PasswordListInput.to_json v.passwords)
         ; Some ("Engine", String.to_json v.engine)
         ; Some ("UserName", String.to_json v.user_name)
         ; Some ("UserId", String.to_json v.user_id)
         ])

  let of_json j =
    { user_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UserId"))
    ; user_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UserName"))
    ; engine = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Engine"))
    ; passwords =
        PasswordListInput.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Passwords"))
    ; access_string =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AccessString"))
    ; no_password_required =
        Aws.Util.option_map (Aws.Json.lookup j "NoPasswordRequired") Boolean.of_json
    }
end

module CreateReplicationGroupResult = struct
  type t = { replication_group : ReplicationGroup.t option }

  let make ?replication_group () = { replication_group }

  let parse xml =
    Some
      { replication_group =
          Aws.Util.option_bind
            (Aws.Xml.member "ReplicationGroup" xml)
            ReplicationGroup.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.replication_group (fun f ->
               Aws.Query.Pair ("ReplicationGroup", ReplicationGroup.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.replication_group (fun f ->
               "ReplicationGroup", ReplicationGroup.to_json f)
         ])

  let of_json j =
    { replication_group =
        Aws.Util.option_map
          (Aws.Json.lookup j "ReplicationGroup")
          ReplicationGroup.of_json
    }
end

module DescribeCacheClustersMessage = struct
  type t =
    { cache_cluster_id : String.t option
    ; max_records : Integer.t option
    ; marker : String.t option
    ; show_cache_node_info : Boolean.t option
    ; show_cache_clusters_not_in_replication_groups : Boolean.t option
    }

  let make
      ?cache_cluster_id
      ?max_records
      ?marker
      ?show_cache_node_info
      ?show_cache_clusters_not_in_replication_groups
      () =
    { cache_cluster_id
    ; max_records
    ; marker
    ; show_cache_node_info
    ; show_cache_clusters_not_in_replication_groups
    }

  let parse xml =
    Some
      { cache_cluster_id =
          Aws.Util.option_bind (Aws.Xml.member "CacheClusterId" xml) String.parse
      ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
      ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      ; show_cache_node_info =
          Aws.Util.option_bind (Aws.Xml.member "ShowCacheNodeInfo" xml) Boolean.parse
      ; show_cache_clusters_not_in_replication_groups =
          Aws.Util.option_bind
            (Aws.Xml.member "ShowCacheClustersNotInReplicationGroups" xml)
            Boolean.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.show_cache_clusters_not_in_replication_groups (fun f ->
               Aws.Query.Pair
                 ("ShowCacheClustersNotInReplicationGroups", Boolean.to_query f))
         ; Aws.Util.option_map v.show_cache_node_info (fun f ->
               Aws.Query.Pair ("ShowCacheNodeInfo", Boolean.to_query f))
         ; Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ; Aws.Util.option_map v.max_records (fun f ->
               Aws.Query.Pair ("MaxRecords", Integer.to_query f))
         ; Aws.Util.option_map v.cache_cluster_id (fun f ->
               Aws.Query.Pair ("CacheClusterId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.show_cache_clusters_not_in_replication_groups (fun f ->
               "ShowCacheClustersNotInReplicationGroups", Boolean.to_json f)
         ; Aws.Util.option_map v.show_cache_node_info (fun f ->
               "ShowCacheNodeInfo", Boolean.to_json f)
         ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
         ; Aws.Util.option_map v.cache_cluster_id (fun f ->
               "CacheClusterId", String.to_json f)
         ])

  let of_json j =
    { cache_cluster_id =
        Aws.Util.option_map (Aws.Json.lookup j "CacheClusterId") String.of_json
    ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
    ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    ; show_cache_node_info =
        Aws.Util.option_map (Aws.Json.lookup j "ShowCacheNodeInfo") Boolean.of_json
    ; show_cache_clusters_not_in_replication_groups =
        Aws.Util.option_map
          (Aws.Json.lookup j "ShowCacheClustersNotInReplicationGroups")
          Boolean.of_json
    }
end

module CacheParameterGroupsMessage = struct
  type t =
    { marker : String.t option
    ; cache_parameter_groups : CacheParameterGroupList.t
    }

  let make ?marker ?(cache_parameter_groups = []) () = { marker; cache_parameter_groups }

  let parse xml =
    Some
      { marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
      ; cache_parameter_groups =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "CacheParameterGroups" xml)
               CacheParameterGroupList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "CacheParameterGroups.member"
                , CacheParameterGroupList.to_query v.cache_parameter_groups ))
         ; Aws.Util.option_map v.marker (fun f ->
               Aws.Query.Pair ("Marker", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "CacheParameterGroups"
             , CacheParameterGroupList.to_json v.cache_parameter_groups )
         ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
         ])

  let of_json j =
    { marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
    ; cache_parameter_groups =
        CacheParameterGroupList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheParameterGroups"))
    }
end

module ListAllowedNodeTypeModificationsMessage = struct
  type t =
    { cache_cluster_id : String.t option
    ; replication_group_id : String.t option
    }

  let make ?cache_cluster_id ?replication_group_id () =
    { cache_cluster_id; replication_group_id }

  let parse xml =
    Some
      { cache_cluster_id =
          Aws.Util.option_bind (Aws.Xml.member "CacheClusterId" xml) String.parse
      ; replication_group_id =
          Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.replication_group_id (fun f ->
               Aws.Query.Pair ("ReplicationGroupId", String.to_query f))
         ; Aws.Util.option_map v.cache_cluster_id (fun f ->
               Aws.Query.Pair ("CacheClusterId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.replication_group_id (fun f ->
               "ReplicationGroupId", String.to_json f)
         ; Aws.Util.option_map v.cache_cluster_id (fun f ->
               "CacheClusterId", String.to_json f)
         ])

  let of_json j =
    { cache_cluster_id =
        Aws.Util.option_map (Aws.Json.lookup j "CacheClusterId") String.of_json
    ; replication_group_id =
        Aws.Util.option_map (Aws.Json.lookup j "ReplicationGroupId") String.of_json
    }
end

module InvalidUserGroupStateFault = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end
