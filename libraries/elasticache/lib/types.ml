open Aws
open Aws.BaseTypes
open CalendarLib
type calendar = Calendar.t
module NodeUpdateInitiatedBy =
  struct
    type t =
      | System 
      | Customer 
    let str_to_t = [("customer", Customer); ("system", System)]
    let t_to_str = [(Customer, "customer"); (System, "system")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module NodeUpdateStatus =
  struct
    type t =
      | Not_applied 
      | Waiting_to_start 
      | In_progress 
      | Stopping 
      | Stopped 
      | Complete 
    let str_to_t =
      [("complete", Complete);
      ("stopped", Stopped);
      ("stopping", Stopping);
      ("in-progress", In_progress);
      ("waiting-to-start", Waiting_to_start);
      ("not-applied", Not_applied)]
    let t_to_str =
      [(Complete, "complete");
      (Stopped, "stopped");
      (Stopping, "stopping");
      (In_progress, "in-progress");
      (Waiting_to_start, "waiting-to-start");
      (Not_applied, "not-applied")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module Endpoint =
  struct
    type t = {
      address: String.t option ;
      port: Integer.t option }
    let make ?address  ?port  () = { address; port }
    let parse xml =
      Some
        {
          address =
            (Util.option_bind (Xml.member "Address" xml) String.parse);
          port = (Util.option_bind (Xml.member "Port" xml) Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.port
              (fun f -> Query.Pair ("Port", (Integer.to_query f)));
           Util.option_map v.address
             (fun f -> Query.Pair ("Address", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.port (fun f -> ("port", (Integer.to_json f)));
           Util.option_map v.address
             (fun f -> ("address", (String.to_json f)))])
    let of_json j =
      {
        address = (Util.option_map (Json.lookup j "address") String.of_json);
        port = (Util.option_map (Json.lookup j "port") Integer.of_json)
      }
  end
module AvailabilityZonesList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map String.parse (Xml.members "AvailabilityZone" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module OutpostArnsList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "OutpostArn" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module NodeGroupMemberUpdateStatus =
  struct
    type t =
      {
      cache_cluster_id: String.t option ;
      cache_node_id: String.t option ;
      node_update_status: NodeUpdateStatus.t option ;
      node_deletion_date: DateTime.t option ;
      node_update_start_date: DateTime.t option ;
      node_update_end_date: DateTime.t option ;
      node_update_initiated_by: NodeUpdateInitiatedBy.t option ;
      node_update_initiated_date: DateTime.t option ;
      node_update_status_modified_date: DateTime.t option }
    let make ?cache_cluster_id  ?cache_node_id  ?node_update_status 
      ?node_deletion_date  ?node_update_start_date  ?node_update_end_date 
      ?node_update_initiated_by  ?node_update_initiated_date 
      ?node_update_status_modified_date  () =
      {
        cache_cluster_id;
        cache_node_id;
        node_update_status;
        node_deletion_date;
        node_update_start_date;
        node_update_end_date;
        node_update_initiated_by;
        node_update_initiated_date;
        node_update_status_modified_date
      }
    let parse xml =
      Some
        {
          cache_cluster_id =
            (Util.option_bind (Xml.member "CacheClusterId" xml) String.parse);
          cache_node_id =
            (Util.option_bind (Xml.member "CacheNodeId" xml) String.parse);
          node_update_status =
            (Util.option_bind (Xml.member "NodeUpdateStatus" xml)
               NodeUpdateStatus.parse);
          node_deletion_date =
            (Util.option_bind (Xml.member "NodeDeletionDate" xml)
               DateTime.parse);
          node_update_start_date =
            (Util.option_bind (Xml.member "NodeUpdateStartDate" xml)
               DateTime.parse);
          node_update_end_date =
            (Util.option_bind (Xml.member "NodeUpdateEndDate" xml)
               DateTime.parse);
          node_update_initiated_by =
            (Util.option_bind (Xml.member "NodeUpdateInitiatedBy" xml)
               NodeUpdateInitiatedBy.parse);
          node_update_initiated_date =
            (Util.option_bind (Xml.member "NodeUpdateInitiatedDate" xml)
               DateTime.parse);
          node_update_status_modified_date =
            (Util.option_bind (Xml.member "NodeUpdateStatusModifiedDate" xml)
               DateTime.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.node_update_status_modified_date
              (fun f ->
                 Query.Pair
                   ("NodeUpdateStatusModifiedDate", (DateTime.to_query f)));
           Util.option_map v.node_update_initiated_date
             (fun f ->
                Query.Pair ("NodeUpdateInitiatedDate", (DateTime.to_query f)));
           Util.option_map v.node_update_initiated_by
             (fun f ->
                Query.Pair
                  ("NodeUpdateInitiatedBy",
                    (NodeUpdateInitiatedBy.to_query f)));
           Util.option_map v.node_update_end_date
             (fun f ->
                Query.Pair ("NodeUpdateEndDate", (DateTime.to_query f)));
           Util.option_map v.node_update_start_date
             (fun f ->
                Query.Pair ("NodeUpdateStartDate", (DateTime.to_query f)));
           Util.option_map v.node_deletion_date
             (fun f -> Query.Pair ("NodeDeletionDate", (DateTime.to_query f)));
           Util.option_map v.node_update_status
             (fun f ->
                Query.Pair
                  ("NodeUpdateStatus", (NodeUpdateStatus.to_query f)));
           Util.option_map v.cache_node_id
             (fun f -> Query.Pair ("CacheNodeId", (String.to_query f)));
           Util.option_map v.cache_cluster_id
             (fun f -> Query.Pair ("CacheClusterId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.node_update_status_modified_date
              (fun f ->
                 ("node_update_status_modified_date", (DateTime.to_json f)));
           Util.option_map v.node_update_initiated_date
             (fun f -> ("node_update_initiated_date", (DateTime.to_json f)));
           Util.option_map v.node_update_initiated_by
             (fun f ->
                ("node_update_initiated_by",
                  (NodeUpdateInitiatedBy.to_json f)));
           Util.option_map v.node_update_end_date
             (fun f -> ("node_update_end_date", (DateTime.to_json f)));
           Util.option_map v.node_update_start_date
             (fun f -> ("node_update_start_date", (DateTime.to_json f)));
           Util.option_map v.node_deletion_date
             (fun f -> ("node_deletion_date", (DateTime.to_json f)));
           Util.option_map v.node_update_status
             (fun f -> ("node_update_status", (NodeUpdateStatus.to_json f)));
           Util.option_map v.cache_node_id
             (fun f -> ("cache_node_id", (String.to_json f)));
           Util.option_map v.cache_cluster_id
             (fun f -> ("cache_cluster_id", (String.to_json f)))])
    let of_json j =
      {
        cache_cluster_id =
          (Util.option_map (Json.lookup j "cache_cluster_id") String.of_json);
        cache_node_id =
          (Util.option_map (Json.lookup j "cache_node_id") String.of_json);
        node_update_status =
          (Util.option_map (Json.lookup j "node_update_status")
             NodeUpdateStatus.of_json);
        node_deletion_date =
          (Util.option_map (Json.lookup j "node_deletion_date")
             DateTime.of_json);
        node_update_start_date =
          (Util.option_map (Json.lookup j "node_update_start_date")
             DateTime.of_json);
        node_update_end_date =
          (Util.option_map (Json.lookup j "node_update_end_date")
             DateTime.of_json);
        node_update_initiated_by =
          (Util.option_map (Json.lookup j "node_update_initiated_by")
             NodeUpdateInitiatedBy.of_json);
        node_update_initiated_date =
          (Util.option_map (Json.lookup j "node_update_initiated_date")
             DateTime.of_json);
        node_update_status_modified_date =
          (Util.option_map (Json.lookup j "node_update_status_modified_date")
             DateTime.of_json)
      }
  end
module NodeGroupMember =
  struct
    type t =
      {
      cache_cluster_id: String.t option ;
      cache_node_id: String.t option ;
      read_endpoint: Endpoint.t option ;
      preferred_availability_zone: String.t option ;
      preferred_outpost_arn: String.t option ;
      current_role: String.t option }
    let make ?cache_cluster_id  ?cache_node_id  ?read_endpoint 
      ?preferred_availability_zone  ?preferred_outpost_arn  ?current_role  ()
      =
      {
        cache_cluster_id;
        cache_node_id;
        read_endpoint;
        preferred_availability_zone;
        preferred_outpost_arn;
        current_role
      }
    let parse xml =
      Some
        {
          cache_cluster_id =
            (Util.option_bind (Xml.member "CacheClusterId" xml) String.parse);
          cache_node_id =
            (Util.option_bind (Xml.member "CacheNodeId" xml) String.parse);
          read_endpoint =
            (Util.option_bind (Xml.member "ReadEndpoint" xml) Endpoint.parse);
          preferred_availability_zone =
            (Util.option_bind (Xml.member "PreferredAvailabilityZone" xml)
               String.parse);
          preferred_outpost_arn =
            (Util.option_bind (Xml.member "PreferredOutpostArn" xml)
               String.parse);
          current_role =
            (Util.option_bind (Xml.member "CurrentRole" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.current_role
              (fun f -> Query.Pair ("CurrentRole", (String.to_query f)));
           Util.option_map v.preferred_outpost_arn
             (fun f ->
                Query.Pair ("PreferredOutpostArn", (String.to_query f)));
           Util.option_map v.preferred_availability_zone
             (fun f ->
                Query.Pair ("PreferredAvailabilityZone", (String.to_query f)));
           Util.option_map v.read_endpoint
             (fun f -> Query.Pair ("ReadEndpoint", (Endpoint.to_query f)));
           Util.option_map v.cache_node_id
             (fun f -> Query.Pair ("CacheNodeId", (String.to_query f)));
           Util.option_map v.cache_cluster_id
             (fun f -> Query.Pair ("CacheClusterId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.current_role
              (fun f -> ("current_role", (String.to_json f)));
           Util.option_map v.preferred_outpost_arn
             (fun f -> ("preferred_outpost_arn", (String.to_json f)));
           Util.option_map v.preferred_availability_zone
             (fun f -> ("preferred_availability_zone", (String.to_json f)));
           Util.option_map v.read_endpoint
             (fun f -> ("read_endpoint", (Endpoint.to_json f)));
           Util.option_map v.cache_node_id
             (fun f -> ("cache_node_id", (String.to_json f)));
           Util.option_map v.cache_cluster_id
             (fun f -> ("cache_cluster_id", (String.to_json f)))])
    let of_json j =
      {
        cache_cluster_id =
          (Util.option_map (Json.lookup j "cache_cluster_id") String.of_json);
        cache_node_id =
          (Util.option_map (Json.lookup j "cache_node_id") String.of_json);
        read_endpoint =
          (Util.option_map (Json.lookup j "read_endpoint") Endpoint.of_json);
        preferred_availability_zone =
          (Util.option_map (Json.lookup j "preferred_availability_zone")
             String.of_json);
        preferred_outpost_arn =
          (Util.option_map (Json.lookup j "preferred_outpost_arn")
             String.of_json);
        current_role =
          (Util.option_map (Json.lookup j "current_role") String.of_json)
      }
  end
module AvailabilityZone =
  struct
    type t = {
      name: String.t option }
    let make ?name  () = { name }
    let parse xml =
      Some { name = (Util.option_bind (Xml.member "Name" xml) String.parse) }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.name
              (fun f -> Query.Pair ("Name", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.name (fun f -> ("name", (String.to_json f)))])
    let of_json j =
      { name = (Util.option_map (Json.lookup j "name") String.of_json) }
  end
module SubnetOutpost =
  struct
    type t = {
      subnet_outpost_arn: String.t option }
    let make ?subnet_outpost_arn  () = { subnet_outpost_arn }
    let parse xml =
      Some
        {
          subnet_outpost_arn =
            (Util.option_bind (Xml.member "SubnetOutpostArn" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.subnet_outpost_arn
              (fun f -> Query.Pair ("SubnetOutpostArn", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.subnet_outpost_arn
              (fun f -> ("subnet_outpost_arn", (String.to_json f)))])
    let of_json j =
      {
        subnet_outpost_arn =
          (Util.option_map (Json.lookup j "subnet_outpost_arn")
             String.of_json)
      }
  end
module CacheNodeTypeSpecificValue =
  struct
    type t = {
      cache_node_type: String.t option ;
      value: String.t option }
    let make ?cache_node_type  ?value  () = { cache_node_type; value }
    let parse xml =
      Some
        {
          cache_node_type =
            (Util.option_bind (Xml.member "CacheNodeType" xml) String.parse);
          value = (Util.option_bind (Xml.member "Value" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.value
              (fun f -> Query.Pair ("Value", (String.to_query f)));
           Util.option_map v.cache_node_type
             (fun f -> Query.Pair ("CacheNodeType", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.value (fun f -> ("value", (String.to_json f)));
           Util.option_map v.cache_node_type
             (fun f -> ("cache_node_type", (String.to_json f)))])
    let of_json j =
      {
        cache_node_type =
          (Util.option_map (Json.lookup j "cache_node_type") String.of_json);
        value = (Util.option_map (Json.lookup j "value") String.of_json)
      }
  end
module NodeGroupConfiguration =
  struct
    type t =
      {
      node_group_id: String.t option ;
      slots: String.t option ;
      replica_count: Integer.t option ;
      primary_availability_zone: String.t option ;
      replica_availability_zones: AvailabilityZonesList.t ;
      primary_outpost_arn: String.t option ;
      replica_outpost_arns: OutpostArnsList.t }
    let make ?node_group_id  ?slots  ?replica_count 
      ?primary_availability_zone  ?(replica_availability_zones= []) 
      ?primary_outpost_arn  ?(replica_outpost_arns= [])  () =
      {
        node_group_id;
        slots;
        replica_count;
        primary_availability_zone;
        replica_availability_zones;
        primary_outpost_arn;
        replica_outpost_arns
      }
    let parse xml =
      Some
        {
          node_group_id =
            (Util.option_bind (Xml.member "NodeGroupId" xml) String.parse);
          slots = (Util.option_bind (Xml.member "Slots" xml) String.parse);
          replica_count =
            (Util.option_bind (Xml.member "ReplicaCount" xml) Integer.parse);
          primary_availability_zone =
            (Util.option_bind (Xml.member "PrimaryAvailabilityZone" xml)
               String.parse);
          replica_availability_zones =
            (Util.of_option []
               (Util.option_bind (Xml.member "ReplicaAvailabilityZones" xml)
                  AvailabilityZonesList.parse));
          primary_outpost_arn =
            (Util.option_bind (Xml.member "PrimaryOutpostArn" xml)
               String.parse);
          replica_outpost_arns =
            (Util.of_option []
               (Util.option_bind (Xml.member "ReplicaOutpostArns" xml)
                  OutpostArnsList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ReplicaOutpostArns.member",
                   (OutpostArnsList.to_query v.replica_outpost_arns)));
           Util.option_map v.primary_outpost_arn
             (fun f -> Query.Pair ("PrimaryOutpostArn", (String.to_query f)));
           Some
             (Query.Pair
                ("ReplicaAvailabilityZones.member",
                  (AvailabilityZonesList.to_query
                     v.replica_availability_zones)));
           Util.option_map v.primary_availability_zone
             (fun f ->
                Query.Pair ("PrimaryAvailabilityZone", (String.to_query f)));
           Util.option_map v.replica_count
             (fun f -> Query.Pair ("ReplicaCount", (Integer.to_query f)));
           Util.option_map v.slots
             (fun f -> Query.Pair ("Slots", (String.to_query f)));
           Util.option_map v.node_group_id
             (fun f -> Query.Pair ("NodeGroupId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("replica_outpost_arns",
                (OutpostArnsList.to_json v.replica_outpost_arns));
           Util.option_map v.primary_outpost_arn
             (fun f -> ("primary_outpost_arn", (String.to_json f)));
           Some
             ("replica_availability_zones",
               (AvailabilityZonesList.to_json v.replica_availability_zones));
           Util.option_map v.primary_availability_zone
             (fun f -> ("primary_availability_zone", (String.to_json f)));
           Util.option_map v.replica_count
             (fun f -> ("replica_count", (Integer.to_json f)));
           Util.option_map v.slots (fun f -> ("slots", (String.to_json f)));
           Util.option_map v.node_group_id
             (fun f -> ("node_group_id", (String.to_json f)))])
    let of_json j =
      {
        node_group_id =
          (Util.option_map (Json.lookup j "node_group_id") String.of_json);
        slots = (Util.option_map (Json.lookup j "slots") String.of_json);
        replica_count =
          (Util.option_map (Json.lookup j "replica_count") Integer.of_json);
        primary_availability_zone =
          (Util.option_map (Json.lookup j "primary_availability_zone")
             String.of_json);
        replica_availability_zones =
          (AvailabilityZonesList.of_json
             (Util.of_option_exn (Json.lookup j "replica_availability_zones")));
        primary_outpost_arn =
          (Util.option_map (Json.lookup j "primary_outpost_arn")
             String.of_json);
        replica_outpost_arns =
          (OutpostArnsList.of_json
             (Util.of_option_exn (Json.lookup j "replica_outpost_arns")))
      }
  end
module AutomaticFailoverStatus =
  struct
    type t =
      | Enabled 
      | Disabled 
      | Enabling 
      | Disabling 
    let str_to_t =
      [("disabling", Disabling);
      ("enabling", Enabling);
      ("disabled", Disabled);
      ("enabled", Enabled)]
    let t_to_str =
      [(Disabling, "disabling");
      (Enabling, "enabling");
      (Disabled, "disabled");
      (Enabled, "enabled")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module NodeGroupMemberUpdateStatusList =
  struct
    type t = NodeGroupMemberUpdateStatus.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map NodeGroupMemberUpdateStatus.parse
           (Xml.members "NodeGroupMemberUpdateStatus" xml))
    let to_query v =
      Query.to_query_list NodeGroupMemberUpdateStatus.to_query v
    let to_json v = `List (List.map NodeGroupMemberUpdateStatus.to_json v)
    let of_json j = Json.to_list NodeGroupMemberUpdateStatus.of_json j
  end
module NodeGroupMemberList =
  struct
    type t = NodeGroupMember.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map NodeGroupMember.parse (Xml.members "NodeGroupMember" xml))
    let to_query v = Query.to_query_list NodeGroupMember.to_query v
    let to_json v = `List (List.map NodeGroupMember.to_json v)
    let of_json j = Json.to_list NodeGroupMember.of_json j
  end
module SlotMigration =
  struct
    type t = {
      progress_percentage: Double.t option }
    let make ?progress_percentage  () = { progress_percentage }
    let parse xml =
      Some
        {
          progress_percentage =
            (Util.option_bind (Xml.member "ProgressPercentage" xml)
               Double.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.progress_percentage
              (fun f ->
                 Query.Pair ("ProgressPercentage", (Double.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.progress_percentage
              (fun f -> ("progress_percentage", (Double.to_json f)))])
    let of_json j =
      {
        progress_percentage =
          (Util.option_map (Json.lookup j "progress_percentage")
             Double.of_json)
      }
  end
module UserGroupIdList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module Subnet =
  struct
    type t =
      {
      subnet_identifier: String.t option ;
      subnet_availability_zone: AvailabilityZone.t option ;
      subnet_outpost: SubnetOutpost.t option }
    let make ?subnet_identifier  ?subnet_availability_zone  ?subnet_outpost 
      () = { subnet_identifier; subnet_availability_zone; subnet_outpost }
    let parse xml =
      Some
        {
          subnet_identifier =
            (Util.option_bind (Xml.member "SubnetIdentifier" xml)
               String.parse);
          subnet_availability_zone =
            (Util.option_bind (Xml.member "SubnetAvailabilityZone" xml)
               AvailabilityZone.parse);
          subnet_outpost =
            (Util.option_bind (Xml.member "SubnetOutpost" xml)
               SubnetOutpost.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.subnet_outpost
              (fun f ->
                 Query.Pair ("SubnetOutpost", (SubnetOutpost.to_query f)));
           Util.option_map v.subnet_availability_zone
             (fun f ->
                Query.Pair
                  ("SubnetAvailabilityZone", (AvailabilityZone.to_query f)));
           Util.option_map v.subnet_identifier
             (fun f -> Query.Pair ("SubnetIdentifier", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.subnet_outpost
              (fun f -> ("subnet_outpost", (SubnetOutpost.to_json f)));
           Util.option_map v.subnet_availability_zone
             (fun f ->
                ("subnet_availability_zone", (AvailabilityZone.to_json f)));
           Util.option_map v.subnet_identifier
             (fun f -> ("subnet_identifier", (String.to_json f)))])
    let of_json j =
      {
        subnet_identifier =
          (Util.option_map (Json.lookup j "subnet_identifier") String.of_json);
        subnet_availability_zone =
          (Util.option_map (Json.lookup j "subnet_availability_zone")
             AvailabilityZone.of_json);
        subnet_outpost =
          (Util.option_map (Json.lookup j "subnet_outpost")
             SubnetOutpost.of_json)
      }
  end
module CacheNodeTypeSpecificValueList =
  struct
    type t = CacheNodeTypeSpecificValue.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map CacheNodeTypeSpecificValue.parse
           (Xml.members "CacheNodeTypeSpecificValue" xml))
    let to_query v =
      Query.to_query_list CacheNodeTypeSpecificValue.to_query v
    let to_json v = `List (List.map CacheNodeTypeSpecificValue.to_json v)
    let of_json j = Json.to_list CacheNodeTypeSpecificValue.of_json j
  end
module ChangeType =
  struct
    type t =
      | Immediate 
      | Requires_reboot 
    let str_to_t =
      [("requires-reboot", Requires_reboot); ("immediate", Immediate)]
    let t_to_str =
      [(Requires_reboot, "requires-reboot"); (Immediate, "immediate")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module ReshardingConfiguration =
  struct
    type t =
      {
      node_group_id: String.t option ;
      preferred_availability_zones: AvailabilityZonesList.t }
    let make ?node_group_id  ?(preferred_availability_zones= [])  () =
      { node_group_id; preferred_availability_zones }
    let parse xml =
      Some
        {
          node_group_id =
            (Util.option_bind (Xml.member "NodeGroupId" xml) String.parse);
          preferred_availability_zones =
            (Util.of_option []
               (Util.option_bind
                  (Xml.member "PreferredAvailabilityZones" xml)
                  AvailabilityZonesList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("PreferredAvailabilityZones.member",
                   (AvailabilityZonesList.to_query
                      v.preferred_availability_zones)));
           Util.option_map v.node_group_id
             (fun f -> Query.Pair ("NodeGroupId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("preferred_availability_zones",
                (AvailabilityZonesList.to_json v.preferred_availability_zones));
           Util.option_map v.node_group_id
             (fun f -> ("node_group_id", (String.to_json f)))])
    let of_json j =
      {
        node_group_id =
          (Util.option_map (Json.lookup j "node_group_id") String.of_json);
        preferred_availability_zones =
          (AvailabilityZonesList.of_json
             (Util.of_option_exn
                (Json.lookup j "preferred_availability_zones")))
      }
  end
module EC2SecurityGroup =
  struct
    type t =
      {
      status: String.t option ;
      e_c2_security_group_name: String.t option ;
      e_c2_security_group_owner_id: String.t option }
    let make ?status  ?e_c2_security_group_name 
      ?e_c2_security_group_owner_id  () =
      { status; e_c2_security_group_name; e_c2_security_group_owner_id }
    let parse xml =
      Some
        {
          status = (Util.option_bind (Xml.member "Status" xml) String.parse);
          e_c2_security_group_name =
            (Util.option_bind (Xml.member "EC2SecurityGroupName" xml)
               String.parse);
          e_c2_security_group_owner_id =
            (Util.option_bind (Xml.member "EC2SecurityGroupOwnerId" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.e_c2_security_group_owner_id
              (fun f ->
                 Query.Pair ("EC2SecurityGroupOwnerId", (String.to_query f)));
           Util.option_map v.e_c2_security_group_name
             (fun f ->
                Query.Pair ("EC2SecurityGroupName", (String.to_query f)));
           Util.option_map v.status
             (fun f -> Query.Pair ("Status", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.e_c2_security_group_owner_id
              (fun f -> ("e_c2_security_group_owner_id", (String.to_json f)));
           Util.option_map v.e_c2_security_group_name
             (fun f -> ("e_c2_security_group_name", (String.to_json f)));
           Util.option_map v.status (fun f -> ("status", (String.to_json f)))])
    let of_json j =
      {
        status = (Util.option_map (Json.lookup j "status") String.of_json);
        e_c2_security_group_name =
          (Util.option_map (Json.lookup j "e_c2_security_group_name")
             String.of_json);
        e_c2_security_group_owner_id =
          (Util.option_map (Json.lookup j "e_c2_security_group_owner_id")
             String.of_json)
      }
  end
module RecurringCharge =
  struct
    type t =
      {
      recurring_charge_amount: Double.t option ;
      recurring_charge_frequency: String.t option }
    let make ?recurring_charge_amount  ?recurring_charge_frequency  () =
      { recurring_charge_amount; recurring_charge_frequency }
    let parse xml =
      Some
        {
          recurring_charge_amount =
            (Util.option_bind (Xml.member "RecurringChargeAmount" xml)
               Double.parse);
          recurring_charge_frequency =
            (Util.option_bind (Xml.member "RecurringChargeFrequency" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.recurring_charge_frequency
              (fun f ->
                 Query.Pair ("RecurringChargeFrequency", (String.to_query f)));
           Util.option_map v.recurring_charge_amount
             (fun f ->
                Query.Pair ("RecurringChargeAmount", (Double.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.recurring_charge_frequency
              (fun f -> ("recurring_charge_frequency", (String.to_json f)));
           Util.option_map v.recurring_charge_amount
             (fun f -> ("recurring_charge_amount", (Double.to_json f)))])
    let of_json j =
      {
        recurring_charge_amount =
          (Util.option_map (Json.lookup j "recurring_charge_amount")
             Double.of_json);
        recurring_charge_frequency =
          (Util.option_map (Json.lookup j "recurring_charge_frequency")
             String.of_json)
      }
  end
module NodeSnapshot =
  struct
    type t =
      {
      cache_cluster_id: String.t option ;
      node_group_id: String.t option ;
      cache_node_id: String.t option ;
      node_group_configuration: NodeGroupConfiguration.t option ;
      cache_size: String.t option ;
      cache_node_create_time: DateTime.t option ;
      snapshot_create_time: DateTime.t option }
    let make ?cache_cluster_id  ?node_group_id  ?cache_node_id 
      ?node_group_configuration  ?cache_size  ?cache_node_create_time 
      ?snapshot_create_time  () =
      {
        cache_cluster_id;
        node_group_id;
        cache_node_id;
        node_group_configuration;
        cache_size;
        cache_node_create_time;
        snapshot_create_time
      }
    let parse xml =
      Some
        {
          cache_cluster_id =
            (Util.option_bind (Xml.member "CacheClusterId" xml) String.parse);
          node_group_id =
            (Util.option_bind (Xml.member "NodeGroupId" xml) String.parse);
          cache_node_id =
            (Util.option_bind (Xml.member "CacheNodeId" xml) String.parse);
          node_group_configuration =
            (Util.option_bind (Xml.member "NodeGroupConfiguration" xml)
               NodeGroupConfiguration.parse);
          cache_size =
            (Util.option_bind (Xml.member "CacheSize" xml) String.parse);
          cache_node_create_time =
            (Util.option_bind (Xml.member "CacheNodeCreateTime" xml)
               DateTime.parse);
          snapshot_create_time =
            (Util.option_bind (Xml.member "SnapshotCreateTime" xml)
               DateTime.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.snapshot_create_time
              (fun f ->
                 Query.Pair ("SnapshotCreateTime", (DateTime.to_query f)));
           Util.option_map v.cache_node_create_time
             (fun f ->
                Query.Pair ("CacheNodeCreateTime", (DateTime.to_query f)));
           Util.option_map v.cache_size
             (fun f -> Query.Pair ("CacheSize", (String.to_query f)));
           Util.option_map v.node_group_configuration
             (fun f ->
                Query.Pair
                  ("NodeGroupConfiguration",
                    (NodeGroupConfiguration.to_query f)));
           Util.option_map v.cache_node_id
             (fun f -> Query.Pair ("CacheNodeId", (String.to_query f)));
           Util.option_map v.node_group_id
             (fun f -> Query.Pair ("NodeGroupId", (String.to_query f)));
           Util.option_map v.cache_cluster_id
             (fun f -> Query.Pair ("CacheClusterId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.snapshot_create_time
              (fun f -> ("snapshot_create_time", (DateTime.to_json f)));
           Util.option_map v.cache_node_create_time
             (fun f -> ("cache_node_create_time", (DateTime.to_json f)));
           Util.option_map v.cache_size
             (fun f -> ("cache_size", (String.to_json f)));
           Util.option_map v.node_group_configuration
             (fun f ->
                ("node_group_configuration",
                  (NodeGroupConfiguration.to_json f)));
           Util.option_map v.cache_node_id
             (fun f -> ("cache_node_id", (String.to_json f)));
           Util.option_map v.node_group_id
             (fun f -> ("node_group_id", (String.to_json f)));
           Util.option_map v.cache_cluster_id
             (fun f -> ("cache_cluster_id", (String.to_json f)))])
    let of_json j =
      {
        cache_cluster_id =
          (Util.option_map (Json.lookup j "cache_cluster_id") String.of_json);
        node_group_id =
          (Util.option_map (Json.lookup j "node_group_id") String.of_json);
        cache_node_id =
          (Util.option_map (Json.lookup j "cache_node_id") String.of_json);
        node_group_configuration =
          (Util.option_map (Json.lookup j "node_group_configuration")
             NodeGroupConfiguration.of_json);
        cache_size =
          (Util.option_map (Json.lookup j "cache_size") String.of_json);
        cache_node_create_time =
          (Util.option_map (Json.lookup j "cache_node_create_time")
             DateTime.of_json);
        snapshot_create_time =
          (Util.option_map (Json.lookup j "snapshot_create_time")
             DateTime.of_json)
      }
  end
module GlobalNodeGroup =
  struct
    type t = {
      global_node_group_id: String.t option ;
      slots: String.t option }
    let make ?global_node_group_id  ?slots  () =
      { global_node_group_id; slots }
    let parse xml =
      Some
        {
          global_node_group_id =
            (Util.option_bind (Xml.member "GlobalNodeGroupId" xml)
               String.parse);
          slots = (Util.option_bind (Xml.member "Slots" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.slots
              (fun f -> Query.Pair ("Slots", (String.to_query f)));
           Util.option_map v.global_node_group_id
             (fun f -> Query.Pair ("GlobalNodeGroupId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.slots (fun f -> ("slots", (String.to_json f)));
           Util.option_map v.global_node_group_id
             (fun f -> ("global_node_group_id", (String.to_json f)))])
    let of_json j =
      {
        global_node_group_id =
          (Util.option_map (Json.lookup j "global_node_group_id")
             String.of_json);
        slots = (Util.option_map (Json.lookup j "slots") String.of_json)
      }
  end
module GlobalReplicationGroupMember =
  struct
    type t =
      {
      replication_group_id: String.t option ;
      replication_group_region: String.t option ;
      role: String.t option ;
      automatic_failover: AutomaticFailoverStatus.t option ;
      status: String.t option }
    let make ?replication_group_id  ?replication_group_region  ?role 
      ?automatic_failover  ?status  () =
      {
        replication_group_id;
        replication_group_region;
        role;
        automatic_failover;
        status
      }
    let parse xml =
      Some
        {
          replication_group_id =
            (Util.option_bind (Xml.member "ReplicationGroupId" xml)
               String.parse);
          replication_group_region =
            (Util.option_bind (Xml.member "ReplicationGroupRegion" xml)
               String.parse);
          role = (Util.option_bind (Xml.member "Role" xml) String.parse);
          automatic_failover =
            (Util.option_bind (Xml.member "AutomaticFailover" xml)
               AutomaticFailoverStatus.parse);
          status = (Util.option_bind (Xml.member "Status" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.status
              (fun f -> Query.Pair ("Status", (String.to_query f)));
           Util.option_map v.automatic_failover
             (fun f ->
                Query.Pair
                  ("AutomaticFailover", (AutomaticFailoverStatus.to_query f)));
           Util.option_map v.role
             (fun f -> Query.Pair ("Role", (String.to_query f)));
           Util.option_map v.replication_group_region
             (fun f ->
                Query.Pair ("ReplicationGroupRegion", (String.to_query f)));
           Util.option_map v.replication_group_id
             (fun f -> Query.Pair ("ReplicationGroupId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.status
              (fun f -> ("status", (String.to_json f)));
           Util.option_map v.automatic_failover
             (fun f ->
                ("automatic_failover", (AutomaticFailoverStatus.to_json f)));
           Util.option_map v.role (fun f -> ("role", (String.to_json f)));
           Util.option_map v.replication_group_region
             (fun f -> ("replication_group_region", (String.to_json f)));
           Util.option_map v.replication_group_id
             (fun f -> ("replication_group_id", (String.to_json f)))])
    let of_json j =
      {
        replication_group_id =
          (Util.option_map (Json.lookup j "replication_group_id")
             String.of_json);
        replication_group_region =
          (Util.option_map (Json.lookup j "replication_group_region")
             String.of_json);
        role = (Util.option_map (Json.lookup j "role") String.of_json);
        automatic_failover =
          (Util.option_map (Json.lookup j "automatic_failover")
             AutomaticFailoverStatus.of_json);
        status = (Util.option_map (Json.lookup j "status") String.of_json)
      }
  end
module AuthenticationType =
  struct
    type t =
      | Password 
      | No_password 
    let str_to_t = [("no-password", No_password); ("password", Password)]
    let t_to_str = [(No_password, "no-password"); (Password, "password")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module CacheNode =
  struct
    type t =
      {
      cache_node_id: String.t option ;
      cache_node_status: String.t option ;
      cache_node_create_time: DateTime.t option ;
      endpoint: Endpoint.t option ;
      parameter_group_status: String.t option ;
      source_cache_node_id: String.t option ;
      customer_availability_zone: String.t option ;
      customer_outpost_arn: String.t option }
    let make ?cache_node_id  ?cache_node_status  ?cache_node_create_time 
      ?endpoint  ?parameter_group_status  ?source_cache_node_id 
      ?customer_availability_zone  ?customer_outpost_arn  () =
      {
        cache_node_id;
        cache_node_status;
        cache_node_create_time;
        endpoint;
        parameter_group_status;
        source_cache_node_id;
        customer_availability_zone;
        customer_outpost_arn
      }
    let parse xml =
      Some
        {
          cache_node_id =
            (Util.option_bind (Xml.member "CacheNodeId" xml) String.parse);
          cache_node_status =
            (Util.option_bind (Xml.member "CacheNodeStatus" xml) String.parse);
          cache_node_create_time =
            (Util.option_bind (Xml.member "CacheNodeCreateTime" xml)
               DateTime.parse);
          endpoint =
            (Util.option_bind (Xml.member "Endpoint" xml) Endpoint.parse);
          parameter_group_status =
            (Util.option_bind (Xml.member "ParameterGroupStatus" xml)
               String.parse);
          source_cache_node_id =
            (Util.option_bind (Xml.member "SourceCacheNodeId" xml)
               String.parse);
          customer_availability_zone =
            (Util.option_bind (Xml.member "CustomerAvailabilityZone" xml)
               String.parse);
          customer_outpost_arn =
            (Util.option_bind (Xml.member "CustomerOutpostArn" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.customer_outpost_arn
              (fun f ->
                 Query.Pair ("CustomerOutpostArn", (String.to_query f)));
           Util.option_map v.customer_availability_zone
             (fun f ->
                Query.Pair ("CustomerAvailabilityZone", (String.to_query f)));
           Util.option_map v.source_cache_node_id
             (fun f -> Query.Pair ("SourceCacheNodeId", (String.to_query f)));
           Util.option_map v.parameter_group_status
             (fun f ->
                Query.Pair ("ParameterGroupStatus", (String.to_query f)));
           Util.option_map v.endpoint
             (fun f -> Query.Pair ("Endpoint", (Endpoint.to_query f)));
           Util.option_map v.cache_node_create_time
             (fun f ->
                Query.Pair ("CacheNodeCreateTime", (DateTime.to_query f)));
           Util.option_map v.cache_node_status
             (fun f -> Query.Pair ("CacheNodeStatus", (String.to_query f)));
           Util.option_map v.cache_node_id
             (fun f -> Query.Pair ("CacheNodeId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.customer_outpost_arn
              (fun f -> ("customer_outpost_arn", (String.to_json f)));
           Util.option_map v.customer_availability_zone
             (fun f -> ("customer_availability_zone", (String.to_json f)));
           Util.option_map v.source_cache_node_id
             (fun f -> ("source_cache_node_id", (String.to_json f)));
           Util.option_map v.parameter_group_status
             (fun f -> ("parameter_group_status", (String.to_json f)));
           Util.option_map v.endpoint
             (fun f -> ("endpoint", (Endpoint.to_json f)));
           Util.option_map v.cache_node_create_time
             (fun f -> ("cache_node_create_time", (DateTime.to_json f)));
           Util.option_map v.cache_node_status
             (fun f -> ("cache_node_status", (String.to_json f)));
           Util.option_map v.cache_node_id
             (fun f -> ("cache_node_id", (String.to_json f)))])
    let of_json j =
      {
        cache_node_id =
          (Util.option_map (Json.lookup j "cache_node_id") String.of_json);
        cache_node_status =
          (Util.option_map (Json.lookup j "cache_node_status") String.of_json);
        cache_node_create_time =
          (Util.option_map (Json.lookup j "cache_node_create_time")
             DateTime.of_json);
        endpoint =
          (Util.option_map (Json.lookup j "endpoint") Endpoint.of_json);
        parameter_group_status =
          (Util.option_map (Json.lookup j "parameter_group_status")
             String.of_json);
        source_cache_node_id =
          (Util.option_map (Json.lookup j "source_cache_node_id")
             String.of_json);
        customer_availability_zone =
          (Util.option_map (Json.lookup j "customer_availability_zone")
             String.of_json);
        customer_outpost_arn =
          (Util.option_map (Json.lookup j "customer_outpost_arn")
             String.of_json)
      }
  end
module CacheNodeIdsList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "CacheNodeId" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module CacheSecurityGroupMembership =
  struct
    type t =
      {
      cache_security_group_name: String.t option ;
      status: String.t option }
    let make ?cache_security_group_name  ?status  () =
      { cache_security_group_name; status }
    let parse xml =
      Some
        {
          cache_security_group_name =
            (Util.option_bind (Xml.member "CacheSecurityGroupName" xml)
               String.parse);
          status = (Util.option_bind (Xml.member "Status" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.status
              (fun f -> Query.Pair ("Status", (String.to_query f)));
           Util.option_map v.cache_security_group_name
             (fun f ->
                Query.Pair ("CacheSecurityGroupName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.status
              (fun f -> ("status", (String.to_json f)));
           Util.option_map v.cache_security_group_name
             (fun f -> ("cache_security_group_name", (String.to_json f)))])
    let of_json j =
      {
        cache_security_group_name =
          (Util.option_map (Json.lookup j "cache_security_group_name")
             String.of_json);
        status = (Util.option_map (Json.lookup j "status") String.of_json)
      }
  end
module AuthTokenUpdateStatus =
  struct
    type t =
      | SETTING 
      | ROTATING 
    let str_to_t = [("ROTATING", ROTATING); ("SETTING", SETTING)]
    let t_to_str = [(ROTATING, "ROTATING"); (SETTING, "SETTING")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module SecurityGroupMembership =
  struct
    type t = {
      security_group_id: String.t option ;
      status: String.t option }
    let make ?security_group_id  ?status  () = { security_group_id; status }
    let parse xml =
      Some
        {
          security_group_id =
            (Util.option_bind (Xml.member "SecurityGroupId" xml) String.parse);
          status = (Util.option_bind (Xml.member "Status" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.status
              (fun f -> Query.Pair ("Status", (String.to_query f)));
           Util.option_map v.security_group_id
             (fun f -> Query.Pair ("SecurityGroupId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.status
              (fun f -> ("status", (String.to_json f)));
           Util.option_map v.security_group_id
             (fun f -> ("security_group_id", (String.to_json f)))])
    let of_json j =
      {
        security_group_id =
          (Util.option_map (Json.lookup j "security_group_id") String.of_json);
        status = (Util.option_map (Json.lookup j "status") String.of_json)
      }
  end
module UserIdList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module CacheNodeUpdateStatus =
  struct
    type t =
      {
      cache_node_id: String.t option ;
      node_update_status: NodeUpdateStatus.t option ;
      node_deletion_date: DateTime.t option ;
      node_update_start_date: DateTime.t option ;
      node_update_end_date: DateTime.t option ;
      node_update_initiated_by: NodeUpdateInitiatedBy.t option ;
      node_update_initiated_date: DateTime.t option ;
      node_update_status_modified_date: DateTime.t option }
    let make ?cache_node_id  ?node_update_status  ?node_deletion_date 
      ?node_update_start_date  ?node_update_end_date 
      ?node_update_initiated_by  ?node_update_initiated_date 
      ?node_update_status_modified_date  () =
      {
        cache_node_id;
        node_update_status;
        node_deletion_date;
        node_update_start_date;
        node_update_end_date;
        node_update_initiated_by;
        node_update_initiated_date;
        node_update_status_modified_date
      }
    let parse xml =
      Some
        {
          cache_node_id =
            (Util.option_bind (Xml.member "CacheNodeId" xml) String.parse);
          node_update_status =
            (Util.option_bind (Xml.member "NodeUpdateStatus" xml)
               NodeUpdateStatus.parse);
          node_deletion_date =
            (Util.option_bind (Xml.member "NodeDeletionDate" xml)
               DateTime.parse);
          node_update_start_date =
            (Util.option_bind (Xml.member "NodeUpdateStartDate" xml)
               DateTime.parse);
          node_update_end_date =
            (Util.option_bind (Xml.member "NodeUpdateEndDate" xml)
               DateTime.parse);
          node_update_initiated_by =
            (Util.option_bind (Xml.member "NodeUpdateInitiatedBy" xml)
               NodeUpdateInitiatedBy.parse);
          node_update_initiated_date =
            (Util.option_bind (Xml.member "NodeUpdateInitiatedDate" xml)
               DateTime.parse);
          node_update_status_modified_date =
            (Util.option_bind (Xml.member "NodeUpdateStatusModifiedDate" xml)
               DateTime.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.node_update_status_modified_date
              (fun f ->
                 Query.Pair
                   ("NodeUpdateStatusModifiedDate", (DateTime.to_query f)));
           Util.option_map v.node_update_initiated_date
             (fun f ->
                Query.Pair ("NodeUpdateInitiatedDate", (DateTime.to_query f)));
           Util.option_map v.node_update_initiated_by
             (fun f ->
                Query.Pair
                  ("NodeUpdateInitiatedBy",
                    (NodeUpdateInitiatedBy.to_query f)));
           Util.option_map v.node_update_end_date
             (fun f ->
                Query.Pair ("NodeUpdateEndDate", (DateTime.to_query f)));
           Util.option_map v.node_update_start_date
             (fun f ->
                Query.Pair ("NodeUpdateStartDate", (DateTime.to_query f)));
           Util.option_map v.node_deletion_date
             (fun f -> Query.Pair ("NodeDeletionDate", (DateTime.to_query f)));
           Util.option_map v.node_update_status
             (fun f ->
                Query.Pair
                  ("NodeUpdateStatus", (NodeUpdateStatus.to_query f)));
           Util.option_map v.cache_node_id
             (fun f -> Query.Pair ("CacheNodeId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.node_update_status_modified_date
              (fun f ->
                 ("node_update_status_modified_date", (DateTime.to_json f)));
           Util.option_map v.node_update_initiated_date
             (fun f -> ("node_update_initiated_date", (DateTime.to_json f)));
           Util.option_map v.node_update_initiated_by
             (fun f ->
                ("node_update_initiated_by",
                  (NodeUpdateInitiatedBy.to_json f)));
           Util.option_map v.node_update_end_date
             (fun f -> ("node_update_end_date", (DateTime.to_json f)));
           Util.option_map v.node_update_start_date
             (fun f -> ("node_update_start_date", (DateTime.to_json f)));
           Util.option_map v.node_deletion_date
             (fun f -> ("node_deletion_date", (DateTime.to_json f)));
           Util.option_map v.node_update_status
             (fun f -> ("node_update_status", (NodeUpdateStatus.to_json f)));
           Util.option_map v.cache_node_id
             (fun f -> ("cache_node_id", (String.to_json f)))])
    let of_json j =
      {
        cache_node_id =
          (Util.option_map (Json.lookup j "cache_node_id") String.of_json);
        node_update_status =
          (Util.option_map (Json.lookup j "node_update_status")
             NodeUpdateStatus.of_json);
        node_deletion_date =
          (Util.option_map (Json.lookup j "node_deletion_date")
             DateTime.of_json);
        node_update_start_date =
          (Util.option_map (Json.lookup j "node_update_start_date")
             DateTime.of_json);
        node_update_end_date =
          (Util.option_map (Json.lookup j "node_update_end_date")
             DateTime.of_json);
        node_update_initiated_by =
          (Util.option_map (Json.lookup j "node_update_initiated_by")
             NodeUpdateInitiatedBy.of_json);
        node_update_initiated_date =
          (Util.option_map (Json.lookup j "node_update_initiated_date")
             DateTime.of_json);
        node_update_status_modified_date =
          (Util.option_map (Json.lookup j "node_update_status_modified_date")
             DateTime.of_json)
      }
  end
module NodeGroupUpdateStatus =
  struct
    type t =
      {
      node_group_id: String.t option ;
      node_group_member_update_status: NodeGroupMemberUpdateStatusList.t }
    let make ?node_group_id  ?(node_group_member_update_status= [])  () =
      { node_group_id; node_group_member_update_status }
    let parse xml =
      Some
        {
          node_group_id =
            (Util.option_bind (Xml.member "NodeGroupId" xml) String.parse);
          node_group_member_update_status =
            (Util.of_option []
               (Util.option_bind
                  (Xml.member "NodeGroupMemberUpdateStatus" xml)
                  NodeGroupMemberUpdateStatusList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("NodeGroupMemberUpdateStatus.member",
                   (NodeGroupMemberUpdateStatusList.to_query
                      v.node_group_member_update_status)));
           Util.option_map v.node_group_id
             (fun f -> Query.Pair ("NodeGroupId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("node_group_member_update_status",
                (NodeGroupMemberUpdateStatusList.to_json
                   v.node_group_member_update_status));
           Util.option_map v.node_group_id
             (fun f -> ("node_group_id", (String.to_json f)))])
    let of_json j =
      {
        node_group_id =
          (Util.option_map (Json.lookup j "node_group_id") String.of_json);
        node_group_member_update_status =
          (NodeGroupMemberUpdateStatusList.of_json
             (Util.of_option_exn
                (Json.lookup j "node_group_member_update_status")))
      }
  end
module NodeGroup =
  struct
    type t =
      {
      node_group_id: String.t option ;
      status: String.t option ;
      primary_endpoint: Endpoint.t option ;
      reader_endpoint: Endpoint.t option ;
      slots: String.t option ;
      node_group_members: NodeGroupMemberList.t }
    let make ?node_group_id  ?status  ?primary_endpoint  ?reader_endpoint 
      ?slots  ?(node_group_members= [])  () =
      {
        node_group_id;
        status;
        primary_endpoint;
        reader_endpoint;
        slots;
        node_group_members
      }
    let parse xml =
      Some
        {
          node_group_id =
            (Util.option_bind (Xml.member "NodeGroupId" xml) String.parse);
          status = (Util.option_bind (Xml.member "Status" xml) String.parse);
          primary_endpoint =
            (Util.option_bind (Xml.member "PrimaryEndpoint" xml)
               Endpoint.parse);
          reader_endpoint =
            (Util.option_bind (Xml.member "ReaderEndpoint" xml)
               Endpoint.parse);
          slots = (Util.option_bind (Xml.member "Slots" xml) String.parse);
          node_group_members =
            (Util.of_option []
               (Util.option_bind (Xml.member "NodeGroupMembers" xml)
                  NodeGroupMemberList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("NodeGroupMembers.member",
                   (NodeGroupMemberList.to_query v.node_group_members)));
           Util.option_map v.slots
             (fun f -> Query.Pair ("Slots", (String.to_query f)));
           Util.option_map v.reader_endpoint
             (fun f -> Query.Pair ("ReaderEndpoint", (Endpoint.to_query f)));
           Util.option_map v.primary_endpoint
             (fun f -> Query.Pair ("PrimaryEndpoint", (Endpoint.to_query f)));
           Util.option_map v.status
             (fun f -> Query.Pair ("Status", (String.to_query f)));
           Util.option_map v.node_group_id
             (fun f -> Query.Pair ("NodeGroupId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("node_group_members",
                (NodeGroupMemberList.to_json v.node_group_members));
           Util.option_map v.slots (fun f -> ("slots", (String.to_json f)));
           Util.option_map v.reader_endpoint
             (fun f -> ("reader_endpoint", (Endpoint.to_json f)));
           Util.option_map v.primary_endpoint
             (fun f -> ("primary_endpoint", (Endpoint.to_json f)));
           Util.option_map v.status (fun f -> ("status", (String.to_json f)));
           Util.option_map v.node_group_id
             (fun f -> ("node_group_id", (String.to_json f)))])
    let of_json j =
      {
        node_group_id =
          (Util.option_map (Json.lookup j "node_group_id") String.of_json);
        status = (Util.option_map (Json.lookup j "status") String.of_json);
        primary_endpoint =
          (Util.option_map (Json.lookup j "primary_endpoint")
             Endpoint.of_json);
        reader_endpoint =
          (Util.option_map (Json.lookup j "reader_endpoint") Endpoint.of_json);
        slots = (Util.option_map (Json.lookup j "slots") String.of_json);
        node_group_members =
          (NodeGroupMemberList.of_json
             (Util.of_option_exn (Json.lookup j "node_group_members")))
      }
  end
module PendingAutomaticFailoverStatus =
  struct
    type t =
      | Enabled 
      | Disabled 
    let str_to_t = [("disabled", Disabled); ("enabled", Enabled)]
    let t_to_str = [(Disabled, "disabled"); (Enabled, "enabled")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module ReshardingStatus =
  struct
    type t = {
      slot_migration: SlotMigration.t option }
    let make ?slot_migration  () = { slot_migration }
    let parse xml =
      Some
        {
          slot_migration =
            (Util.option_bind (Xml.member "SlotMigration" xml)
               SlotMigration.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.slot_migration
              (fun f ->
                 Query.Pair ("SlotMigration", (SlotMigration.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.slot_migration
              (fun f -> ("slot_migration", (SlotMigration.to_json f)))])
    let of_json j =
      {
        slot_migration =
          (Util.option_map (Json.lookup j "slot_migration")
             SlotMigration.of_json)
      }
  end
module UserGroupsUpdateStatus =
  struct
    type t =
      {
      user_group_ids_to_add: UserGroupIdList.t ;
      user_group_ids_to_remove: UserGroupIdList.t }
    let make ?(user_group_ids_to_add= [])  ?(user_group_ids_to_remove= []) 
      () = { user_group_ids_to_add; user_group_ids_to_remove }
    let parse xml =
      Some
        {
          user_group_ids_to_add =
            (Util.of_option []
               (Util.option_bind (Xml.member "UserGroupIdsToAdd" xml)
                  UserGroupIdList.parse));
          user_group_ids_to_remove =
            (Util.of_option []
               (Util.option_bind (Xml.member "UserGroupIdsToRemove" xml)
                  UserGroupIdList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("UserGroupIdsToRemove.member",
                   (UserGroupIdList.to_query v.user_group_ids_to_remove)));
           Some
             (Query.Pair
                ("UserGroupIdsToAdd.member",
                  (UserGroupIdList.to_query v.user_group_ids_to_add)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("user_group_ids_to_remove",
                (UserGroupIdList.to_json v.user_group_ids_to_remove));
           Some
             ("user_group_ids_to_add",
               (UserGroupIdList.to_json v.user_group_ids_to_add))])
    let of_json j =
      {
        user_group_ids_to_add =
          (UserGroupIdList.of_json
             (Util.of_option_exn (Json.lookup j "user_group_ids_to_add")));
        user_group_ids_to_remove =
          (UserGroupIdList.of_json
             (Util.of_option_exn (Json.lookup j "user_group_ids_to_remove")))
      }
  end
module SubnetList =
  struct
    type t = Subnet.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Subnet.parse (Xml.members "Subnet" xml))
    let to_query v = Query.to_query_list Subnet.to_query v
    let to_json v = `List (List.map Subnet.to_json v)
    let of_json j = Json.to_list Subnet.of_json j
  end
module CacheNodeTypeSpecificParameter =
  struct
    type t =
      {
      parameter_name: String.t option ;
      description: String.t option ;
      source: String.t option ;
      data_type: String.t option ;
      allowed_values: String.t option ;
      is_modifiable: Boolean.t option ;
      minimum_engine_version: String.t option ;
      cache_node_type_specific_values: CacheNodeTypeSpecificValueList.t ;
      change_type: ChangeType.t option }
    let make ?parameter_name  ?description  ?source  ?data_type 
      ?allowed_values  ?is_modifiable  ?minimum_engine_version 
      ?(cache_node_type_specific_values= [])  ?change_type  () =
      {
        parameter_name;
        description;
        source;
        data_type;
        allowed_values;
        is_modifiable;
        minimum_engine_version;
        cache_node_type_specific_values;
        change_type
      }
    let parse xml =
      Some
        {
          parameter_name =
            (Util.option_bind (Xml.member "ParameterName" xml) String.parse);
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse);
          source = (Util.option_bind (Xml.member "Source" xml) String.parse);
          data_type =
            (Util.option_bind (Xml.member "DataType" xml) String.parse);
          allowed_values =
            (Util.option_bind (Xml.member "AllowedValues" xml) String.parse);
          is_modifiable =
            (Util.option_bind (Xml.member "IsModifiable" xml) Boolean.parse);
          minimum_engine_version =
            (Util.option_bind (Xml.member "MinimumEngineVersion" xml)
               String.parse);
          cache_node_type_specific_values =
            (Util.of_option []
               (Util.option_bind
                  (Xml.member "CacheNodeTypeSpecificValues" xml)
                  CacheNodeTypeSpecificValueList.parse));
          change_type =
            (Util.option_bind (Xml.member "ChangeType" xml) ChangeType.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.change_type
              (fun f -> Query.Pair ("ChangeType", (ChangeType.to_query f)));
           Some
             (Query.Pair
                ("CacheNodeTypeSpecificValues.member",
                  (CacheNodeTypeSpecificValueList.to_query
                     v.cache_node_type_specific_values)));
           Util.option_map v.minimum_engine_version
             (fun f ->
                Query.Pair ("MinimumEngineVersion", (String.to_query f)));
           Util.option_map v.is_modifiable
             (fun f -> Query.Pair ("IsModifiable", (Boolean.to_query f)));
           Util.option_map v.allowed_values
             (fun f -> Query.Pair ("AllowedValues", (String.to_query f)));
           Util.option_map v.data_type
             (fun f -> Query.Pair ("DataType", (String.to_query f)));
           Util.option_map v.source
             (fun f -> Query.Pair ("Source", (String.to_query f)));
           Util.option_map v.description
             (fun f -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.parameter_name
             (fun f -> Query.Pair ("ParameterName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.change_type
              (fun f -> ("change_type", (ChangeType.to_json f)));
           Some
             ("cache_node_type_specific_values",
               (CacheNodeTypeSpecificValueList.to_json
                  v.cache_node_type_specific_values));
           Util.option_map v.minimum_engine_version
             (fun f -> ("minimum_engine_version", (String.to_json f)));
           Util.option_map v.is_modifiable
             (fun f -> ("is_modifiable", (Boolean.to_json f)));
           Util.option_map v.allowed_values
             (fun f -> ("allowed_values", (String.to_json f)));
           Util.option_map v.data_type
             (fun f -> ("data_type", (String.to_json f)));
           Util.option_map v.source (fun f -> ("source", (String.to_json f)));
           Util.option_map v.description
             (fun f -> ("description", (String.to_json f)));
           Util.option_map v.parameter_name
             (fun f -> ("parameter_name", (String.to_json f)))])
    let of_json j =
      {
        parameter_name =
          (Util.option_map (Json.lookup j "parameter_name") String.of_json);
        description =
          (Util.option_map (Json.lookup j "description") String.of_json);
        source = (Util.option_map (Json.lookup j "source") String.of_json);
        data_type =
          (Util.option_map (Json.lookup j "data_type") String.of_json);
        allowed_values =
          (Util.option_map (Json.lookup j "allowed_values") String.of_json);
        is_modifiable =
          (Util.option_map (Json.lookup j "is_modifiable") Boolean.of_json);
        minimum_engine_version =
          (Util.option_map (Json.lookup j "minimum_engine_version")
             String.of_json);
        cache_node_type_specific_values =
          (CacheNodeTypeSpecificValueList.of_json
             (Util.of_option_exn
                (Json.lookup j "cache_node_type_specific_values")));
        change_type =
          (Util.option_map (Json.lookup j "change_type") ChangeType.of_json)
      }
  end
module Parameter =
  struct
    type t =
      {
      parameter_name: String.t option ;
      parameter_value: String.t option ;
      description: String.t option ;
      source: String.t option ;
      data_type: String.t option ;
      allowed_values: String.t option ;
      is_modifiable: Boolean.t option ;
      minimum_engine_version: String.t option ;
      change_type: ChangeType.t option }
    let make ?parameter_name  ?parameter_value  ?description  ?source 
      ?data_type  ?allowed_values  ?is_modifiable  ?minimum_engine_version 
      ?change_type  () =
      {
        parameter_name;
        parameter_value;
        description;
        source;
        data_type;
        allowed_values;
        is_modifiable;
        minimum_engine_version;
        change_type
      }
    let parse xml =
      Some
        {
          parameter_name =
            (Util.option_bind (Xml.member "ParameterName" xml) String.parse);
          parameter_value =
            (Util.option_bind (Xml.member "ParameterValue" xml) String.parse);
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse);
          source = (Util.option_bind (Xml.member "Source" xml) String.parse);
          data_type =
            (Util.option_bind (Xml.member "DataType" xml) String.parse);
          allowed_values =
            (Util.option_bind (Xml.member "AllowedValues" xml) String.parse);
          is_modifiable =
            (Util.option_bind (Xml.member "IsModifiable" xml) Boolean.parse);
          minimum_engine_version =
            (Util.option_bind (Xml.member "MinimumEngineVersion" xml)
               String.parse);
          change_type =
            (Util.option_bind (Xml.member "ChangeType" xml) ChangeType.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.change_type
              (fun f -> Query.Pair ("ChangeType", (ChangeType.to_query f)));
           Util.option_map v.minimum_engine_version
             (fun f ->
                Query.Pair ("MinimumEngineVersion", (String.to_query f)));
           Util.option_map v.is_modifiable
             (fun f -> Query.Pair ("IsModifiable", (Boolean.to_query f)));
           Util.option_map v.allowed_values
             (fun f -> Query.Pair ("AllowedValues", (String.to_query f)));
           Util.option_map v.data_type
             (fun f -> Query.Pair ("DataType", (String.to_query f)));
           Util.option_map v.source
             (fun f -> Query.Pair ("Source", (String.to_query f)));
           Util.option_map v.description
             (fun f -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.parameter_value
             (fun f -> Query.Pair ("ParameterValue", (String.to_query f)));
           Util.option_map v.parameter_name
             (fun f -> Query.Pair ("ParameterName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.change_type
              (fun f -> ("change_type", (ChangeType.to_json f)));
           Util.option_map v.minimum_engine_version
             (fun f -> ("minimum_engine_version", (String.to_json f)));
           Util.option_map v.is_modifiable
             (fun f -> ("is_modifiable", (Boolean.to_json f)));
           Util.option_map v.allowed_values
             (fun f -> ("allowed_values", (String.to_json f)));
           Util.option_map v.data_type
             (fun f -> ("data_type", (String.to_json f)));
           Util.option_map v.source (fun f -> ("source", (String.to_json f)));
           Util.option_map v.description
             (fun f -> ("description", (String.to_json f)));
           Util.option_map v.parameter_value
             (fun f -> ("parameter_value", (String.to_json f)));
           Util.option_map v.parameter_name
             (fun f -> ("parameter_name", (String.to_json f)))])
    let of_json j =
      {
        parameter_name =
          (Util.option_map (Json.lookup j "parameter_name") String.of_json);
        parameter_value =
          (Util.option_map (Json.lookup j "parameter_value") String.of_json);
        description =
          (Util.option_map (Json.lookup j "description") String.of_json);
        source = (Util.option_map (Json.lookup j "source") String.of_json);
        data_type =
          (Util.option_map (Json.lookup j "data_type") String.of_json);
        allowed_values =
          (Util.option_map (Json.lookup j "allowed_values") String.of_json);
        is_modifiable =
          (Util.option_map (Json.lookup j "is_modifiable") Boolean.of_json);
        minimum_engine_version =
          (Util.option_map (Json.lookup j "minimum_engine_version")
             String.of_json);
        change_type =
          (Util.option_map (Json.lookup j "change_type") ChangeType.of_json)
      }
  end
module ReshardingConfigurationList =
  struct
    type t = ReshardingConfiguration.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map ReshardingConfiguration.parse
           (Xml.members "ReshardingConfiguration" xml))
    let to_query v = Query.to_query_list ReshardingConfiguration.to_query v
    let to_json v = `List (List.map ReshardingConfiguration.to_json v)
    let of_json j = Json.to_list ReshardingConfiguration.of_json j
  end
module EC2SecurityGroupList =
  struct
    type t = EC2SecurityGroup.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map EC2SecurityGroup.parse (Xml.members "EC2SecurityGroup" xml))
    let to_query v = Query.to_query_list EC2SecurityGroup.to_query v
    let to_json v = `List (List.map EC2SecurityGroup.to_json v)
    let of_json j = Json.to_list EC2SecurityGroup.of_json j
  end
module FilterValueList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module RecurringChargeList =
  struct
    type t = RecurringCharge.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map RecurringCharge.parse (Xml.members "RecurringCharge" xml))
    let to_query v = Query.to_query_list RecurringCharge.to_query v
    let to_json v = `List (List.map RecurringCharge.to_json v)
    let of_json j = Json.to_list RecurringCharge.of_json j
  end
module NodeSnapshotList =
  struct
    type t = NodeSnapshot.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map NodeSnapshot.parse (Xml.members "NodeSnapshot" xml))
    let to_query v = Query.to_query_list NodeSnapshot.to_query v
    let to_json v = `List (List.map NodeSnapshot.to_json v)
    let of_json j = Json.to_list NodeSnapshot.of_json j
  end
module GlobalNodeGroupList =
  struct
    type t = GlobalNodeGroup.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map GlobalNodeGroup.parse (Xml.members "GlobalNodeGroup" xml))
    let to_query v = Query.to_query_list GlobalNodeGroup.to_query v
    let to_json v = `List (List.map GlobalNodeGroup.to_json v)
    let of_json j = Json.to_list GlobalNodeGroup.of_json j
  end
module GlobalReplicationGroupMemberList =
  struct
    type t = GlobalReplicationGroupMember.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map GlobalReplicationGroupMember.parse
           (Xml.members "GlobalReplicationGroupMember" xml))
    let to_query v =
      Query.to_query_list GlobalReplicationGroupMember.to_query v
    let to_json v = `List (List.map GlobalReplicationGroupMember.to_json v)
    let of_json j = Json.to_list GlobalReplicationGroupMember.of_json j
  end
module ServiceUpdateSeverity =
  struct
    type t =
      | Critical 
      | Important 
      | Medium 
      | Low 
    let str_to_t =
      [("low", Low);
      ("medium", Medium);
      ("important", Important);
      ("critical", Critical)]
    let t_to_str =
      [(Low, "low");
      (Medium, "medium");
      (Important, "important");
      (Critical, "critical")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module ServiceUpdateStatus =
  struct
    type t =
      | Available 
      | Cancelled 
      | Expired 
    let str_to_t =
      [("expired", Expired);
      ("cancelled", Cancelled);
      ("available", Available)]
    let t_to_str =
      [(Expired, "expired");
      (Cancelled, "cancelled");
      (Available, "available")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module ServiceUpdateType =
  struct
    type t =
      | Security_update 
    let str_to_t = [("security-update", Security_update)]
    let t_to_str = [(Security_update, "security-update")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module Authentication =
  struct
    type t =
      {
      type_: AuthenticationType.t option ;
      password_count: Integer.t option }
    let make ?type_  ?password_count  () = { type_; password_count }
    let parse xml =
      Some
        {
          type_ =
            (Util.option_bind (Xml.member "Type" xml)
               AuthenticationType.parse);
          password_count =
            (Util.option_bind (Xml.member "PasswordCount" xml) Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.password_count
              (fun f -> Query.Pair ("PasswordCount", (Integer.to_query f)));
           Util.option_map v.type_
             (fun f -> Query.Pair ("Type", (AuthenticationType.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.password_count
              (fun f -> ("password_count", (Integer.to_json f)));
           Util.option_map v.type_
             (fun f -> ("type_", (AuthenticationType.to_json f)))])
    let of_json j =
      {
        type_ =
          (Util.option_map (Json.lookup j "type_") AuthenticationType.of_json);
        password_count =
          (Util.option_map (Json.lookup j "password_count") Integer.of_json)
      }
  end
module PreferredAvailabilityZoneList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map String.parse (Xml.members "PreferredAvailabilityZone" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module PreferredOutpostArnList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map String.parse (Xml.members "PreferredOutpostArn" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module CacheNodeList =
  struct
    type t = CacheNode.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map CacheNode.parse (Xml.members "CacheNode" xml))
    let to_query v = Query.to_query_list CacheNode.to_query v
    let to_json v = `List (List.map CacheNode.to_json v)
    let of_json j = Json.to_list CacheNode.of_json j
  end
module CacheParameterGroupStatus =
  struct
    type t =
      {
      cache_parameter_group_name: String.t option ;
      parameter_apply_status: String.t option ;
      cache_node_ids_to_reboot: CacheNodeIdsList.t }
    let make ?cache_parameter_group_name  ?parameter_apply_status 
      ?(cache_node_ids_to_reboot= [])  () =
      {
        cache_parameter_group_name;
        parameter_apply_status;
        cache_node_ids_to_reboot
      }
    let parse xml =
      Some
        {
          cache_parameter_group_name =
            (Util.option_bind (Xml.member "CacheParameterGroupName" xml)
               String.parse);
          parameter_apply_status =
            (Util.option_bind (Xml.member "ParameterApplyStatus" xml)
               String.parse);
          cache_node_ids_to_reboot =
            (Util.of_option []
               (Util.option_bind (Xml.member "CacheNodeIdsToReboot" xml)
                  CacheNodeIdsList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("CacheNodeIdsToReboot.member",
                   (CacheNodeIdsList.to_query v.cache_node_ids_to_reboot)));
           Util.option_map v.parameter_apply_status
             (fun f ->
                Query.Pair ("ParameterApplyStatus", (String.to_query f)));
           Util.option_map v.cache_parameter_group_name
             (fun f ->
                Query.Pair ("CacheParameterGroupName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("cache_node_ids_to_reboot",
                (CacheNodeIdsList.to_json v.cache_node_ids_to_reboot));
           Util.option_map v.parameter_apply_status
             (fun f -> ("parameter_apply_status", (String.to_json f)));
           Util.option_map v.cache_parameter_group_name
             (fun f -> ("cache_parameter_group_name", (String.to_json f)))])
    let of_json j =
      {
        cache_parameter_group_name =
          (Util.option_map (Json.lookup j "cache_parameter_group_name")
             String.of_json);
        parameter_apply_status =
          (Util.option_map (Json.lookup j "parameter_apply_status")
             String.of_json);
        cache_node_ids_to_reboot =
          (CacheNodeIdsList.of_json
             (Util.of_option_exn (Json.lookup j "cache_node_ids_to_reboot")))
      }
  end
module CacheSecurityGroupMembershipList =
  struct
    type t = CacheSecurityGroupMembership.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map CacheSecurityGroupMembership.parse
           (Xml.members "CacheSecurityGroup" xml))
    let to_query v =
      Query.to_query_list CacheSecurityGroupMembership.to_query v
    let to_json v = `List (List.map CacheSecurityGroupMembership.to_json v)
    let of_json j = Json.to_list CacheSecurityGroupMembership.of_json j
  end
module NotificationConfiguration =
  struct
    type t = {
      topic_arn: String.t option ;
      topic_status: String.t option }
    let make ?topic_arn  ?topic_status  () = { topic_arn; topic_status }
    let parse xml =
      Some
        {
          topic_arn =
            (Util.option_bind (Xml.member "TopicArn" xml) String.parse);
          topic_status =
            (Util.option_bind (Xml.member "TopicStatus" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.topic_status
              (fun f -> Query.Pair ("TopicStatus", (String.to_query f)));
           Util.option_map v.topic_arn
             (fun f -> Query.Pair ("TopicArn", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.topic_status
              (fun f -> ("topic_status", (String.to_json f)));
           Util.option_map v.topic_arn
             (fun f -> ("topic_arn", (String.to_json f)))])
    let of_json j =
      {
        topic_arn =
          (Util.option_map (Json.lookup j "topic_arn") String.of_json);
        topic_status =
          (Util.option_map (Json.lookup j "topic_status") String.of_json)
      }
  end
module PendingModifiedValues =
  struct
    type t =
      {
      num_cache_nodes: Integer.t option ;
      cache_node_ids_to_remove: CacheNodeIdsList.t ;
      engine_version: String.t option ;
      cache_node_type: String.t option ;
      auth_token_status: AuthTokenUpdateStatus.t option }
    let make ?num_cache_nodes  ?(cache_node_ids_to_remove= []) 
      ?engine_version  ?cache_node_type  ?auth_token_status  () =
      {
        num_cache_nodes;
        cache_node_ids_to_remove;
        engine_version;
        cache_node_type;
        auth_token_status
      }
    let parse xml =
      Some
        {
          num_cache_nodes =
            (Util.option_bind (Xml.member "NumCacheNodes" xml) Integer.parse);
          cache_node_ids_to_remove =
            (Util.of_option []
               (Util.option_bind (Xml.member "CacheNodeIdsToRemove" xml)
                  CacheNodeIdsList.parse));
          engine_version =
            (Util.option_bind (Xml.member "EngineVersion" xml) String.parse);
          cache_node_type =
            (Util.option_bind (Xml.member "CacheNodeType" xml) String.parse);
          auth_token_status =
            (Util.option_bind (Xml.member "AuthTokenStatus" xml)
               AuthTokenUpdateStatus.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.auth_token_status
              (fun f ->
                 Query.Pair
                   ("AuthTokenStatus", (AuthTokenUpdateStatus.to_query f)));
           Util.option_map v.cache_node_type
             (fun f -> Query.Pair ("CacheNodeType", (String.to_query f)));
           Util.option_map v.engine_version
             (fun f -> Query.Pair ("EngineVersion", (String.to_query f)));
           Some
             (Query.Pair
                ("CacheNodeIdsToRemove.member",
                  (CacheNodeIdsList.to_query v.cache_node_ids_to_remove)));
           Util.option_map v.num_cache_nodes
             (fun f -> Query.Pair ("NumCacheNodes", (Integer.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.auth_token_status
              (fun f ->
                 ("auth_token_status", (AuthTokenUpdateStatus.to_json f)));
           Util.option_map v.cache_node_type
             (fun f -> ("cache_node_type", (String.to_json f)));
           Util.option_map v.engine_version
             (fun f -> ("engine_version", (String.to_json f)));
           Some
             ("cache_node_ids_to_remove",
               (CacheNodeIdsList.to_json v.cache_node_ids_to_remove));
           Util.option_map v.num_cache_nodes
             (fun f -> ("num_cache_nodes", (Integer.to_json f)))])
    let of_json j =
      {
        num_cache_nodes =
          (Util.option_map (Json.lookup j "num_cache_nodes") Integer.of_json);
        cache_node_ids_to_remove =
          (CacheNodeIdsList.of_json
             (Util.of_option_exn (Json.lookup j "cache_node_ids_to_remove")));
        engine_version =
          (Util.option_map (Json.lookup j "engine_version") String.of_json);
        cache_node_type =
          (Util.option_map (Json.lookup j "cache_node_type") String.of_json);
        auth_token_status =
          (Util.option_map (Json.lookup j "auth_token_status")
             AuthTokenUpdateStatus.of_json)
      }
  end
module SecurityGroupMembershipList =
  struct
    type t = SecurityGroupMembership.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map SecurityGroupMembership.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list SecurityGroupMembership.to_query v
    let to_json v = `List (List.map SecurityGroupMembership.to_json v)
    let of_json j = Json.to_list SecurityGroupMembership.of_json j
  end
module UGReplicationGroupIdList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module UserGroupPendingChanges =
  struct
    type t =
      {
      user_ids_to_remove: UserIdList.t ;
      user_ids_to_add: UserIdList.t }
    let make ?(user_ids_to_remove= [])  ?(user_ids_to_add= [])  () =
      { user_ids_to_remove; user_ids_to_add }
    let parse xml =
      Some
        {
          user_ids_to_remove =
            (Util.of_option []
               (Util.option_bind (Xml.member "UserIdsToRemove" xml)
                  UserIdList.parse));
          user_ids_to_add =
            (Util.of_option []
               (Util.option_bind (Xml.member "UserIdsToAdd" xml)
                  UserIdList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("UserIdsToAdd.member",
                   (UserIdList.to_query v.user_ids_to_add)));
           Some
             (Query.Pair
                ("UserIdsToRemove.member",
                  (UserIdList.to_query v.user_ids_to_remove)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("user_ids_to_add", (UserIdList.to_json v.user_ids_to_add));
           Some
             ("user_ids_to_remove",
               (UserIdList.to_json v.user_ids_to_remove))])
    let of_json j =
      {
        user_ids_to_remove =
          (UserIdList.of_json
             (Util.of_option_exn (Json.lookup j "user_ids_to_remove")));
        user_ids_to_add =
          (UserIdList.of_json
             (Util.of_option_exn (Json.lookup j "user_ids_to_add")))
      }
  end
module CacheNodeUpdateStatusList =
  struct
    type t = CacheNodeUpdateStatus.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map CacheNodeUpdateStatus.parse
           (Xml.members "CacheNodeUpdateStatus" xml))
    let to_query v = Query.to_query_list CacheNodeUpdateStatus.to_query v
    let to_json v = `List (List.map CacheNodeUpdateStatus.to_json v)
    let of_json j = Json.to_list CacheNodeUpdateStatus.of_json j
  end
module NodeGroupUpdateStatusList =
  struct
    type t = NodeGroupUpdateStatus.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map NodeGroupUpdateStatus.parse
           (Xml.members "NodeGroupUpdateStatus" xml))
    let to_query v = Query.to_query_list NodeGroupUpdateStatus.to_query v
    let to_json v = `List (List.map NodeGroupUpdateStatus.to_json v)
    let of_json j = Json.to_list NodeGroupUpdateStatus.of_json j
  end
module SlaMet =
  struct
    type t =
      | Yes 
      | No 
      | N_a 
    let str_to_t = [("n/a", N_a); ("no", No); ("yes", Yes)]
    let t_to_str = [(N_a, "n/a"); (No, "no"); (Yes, "yes")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module UpdateActionStatus =
  struct
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
      [("not-applicable", Not_applicable);
      ("scheduled", Scheduled);
      ("scheduling", Scheduling);
      ("complete", Complete);
      ("stopped", Stopped);
      ("stopping", Stopping);
      ("in-progress", In_progress);
      ("waiting-to-start", Waiting_to_start);
      ("not-applied", Not_applied)]
    let t_to_str =
      [(Not_applicable, "not-applicable");
      (Scheduled, "scheduled");
      (Scheduling, "scheduling");
      (Complete, "complete");
      (Stopped, "stopped");
      (Stopping, "stopping");
      (In_progress, "in-progress");
      (Waiting_to_start, "waiting-to-start");
      (Not_applied, "not-applied")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module SourceType =
  struct
    type t =
      | Cache_cluster 
      | Cache_parameter_group 
      | Cache_security_group 
      | Cache_subnet_group 
      | Replication_group 
      | User 
      | User_group 
    let str_to_t =
      [("user-group", User_group);
      ("user", User);
      ("replication-group", Replication_group);
      ("cache-subnet-group", Cache_subnet_group);
      ("cache-security-group", Cache_security_group);
      ("cache-parameter-group", Cache_parameter_group);
      ("cache-cluster", Cache_cluster)]
    let t_to_str =
      [(User_group, "user-group");
      (User, "user");
      (Replication_group, "replication-group");
      (Cache_subnet_group, "cache-subnet-group");
      (Cache_security_group, "cache-security-group");
      (Cache_parameter_group, "cache-parameter-group");
      (Cache_cluster, "cache-cluster")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module ClusterIdList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "ClusterId" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module GlobalReplicationGroupInfo =
  struct
    type t =
      {
      global_replication_group_id: String.t option ;
      global_replication_group_member_role: String.t option }
    let make ?global_replication_group_id 
      ?global_replication_group_member_role  () =
      { global_replication_group_id; global_replication_group_member_role }
    let parse xml =
      Some
        {
          global_replication_group_id =
            (Util.option_bind (Xml.member "GlobalReplicationGroupId" xml)
               String.parse);
          global_replication_group_member_role =
            (Util.option_bind
               (Xml.member "GlobalReplicationGroupMemberRole" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.global_replication_group_member_role
              (fun f ->
                 Query.Pair
                   ("GlobalReplicationGroupMemberRole", (String.to_query f)));
           Util.option_map v.global_replication_group_id
             (fun f ->
                Query.Pair ("GlobalReplicationGroupId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.global_replication_group_member_role
              (fun f ->
                 ("global_replication_group_member_role", (String.to_json f)));
           Util.option_map v.global_replication_group_id
             (fun f -> ("global_replication_group_id", (String.to_json f)))])
    let of_json j =
      {
        global_replication_group_id =
          (Util.option_map (Json.lookup j "global_replication_group_id")
             String.of_json);
        global_replication_group_member_role =
          (Util.option_map
             (Json.lookup j "global_replication_group_member_role")
             String.of_json)
      }
  end
module MultiAZStatus =
  struct
    type t =
      | Enabled 
      | Disabled 
    let str_to_t = [("disabled", Disabled); ("enabled", Enabled)]
    let t_to_str = [(Disabled, "disabled"); (Enabled, "enabled")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module NodeGroupList =
  struct
    type t = NodeGroup.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map NodeGroup.parse (Xml.members "NodeGroup" xml))
    let to_query v = Query.to_query_list NodeGroup.to_query v
    let to_json v = `List (List.map NodeGroup.to_json v)
    let of_json j = Json.to_list NodeGroup.of_json j
  end
module ReplicationGroupOutpostArnList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map String.parse (Xml.members "ReplicationGroupOutpostArn" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module ReplicationGroupPendingModifiedValues =
  struct
    type t =
      {
      primary_cluster_id: String.t option ;
      automatic_failover_status: PendingAutomaticFailoverStatus.t option ;
      resharding: ReshardingStatus.t option ;
      auth_token_status: AuthTokenUpdateStatus.t option ;
      user_groups: UserGroupsUpdateStatus.t option }
    let make ?primary_cluster_id  ?automatic_failover_status  ?resharding 
      ?auth_token_status  ?user_groups  () =
      {
        primary_cluster_id;
        automatic_failover_status;
        resharding;
        auth_token_status;
        user_groups
      }
    let parse xml =
      Some
        {
          primary_cluster_id =
            (Util.option_bind (Xml.member "PrimaryClusterId" xml)
               String.parse);
          automatic_failover_status =
            (Util.option_bind (Xml.member "AutomaticFailoverStatus" xml)
               PendingAutomaticFailoverStatus.parse);
          resharding =
            (Util.option_bind (Xml.member "Resharding" xml)
               ReshardingStatus.parse);
          auth_token_status =
            (Util.option_bind (Xml.member "AuthTokenStatus" xml)
               AuthTokenUpdateStatus.parse);
          user_groups =
            (Util.option_bind (Xml.member "UserGroups" xml)
               UserGroupsUpdateStatus.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.user_groups
              (fun f ->
                 Query.Pair
                   ("UserGroups", (UserGroupsUpdateStatus.to_query f)));
           Util.option_map v.auth_token_status
             (fun f ->
                Query.Pair
                  ("AuthTokenStatus", (AuthTokenUpdateStatus.to_query f)));
           Util.option_map v.resharding
             (fun f ->
                Query.Pair ("Resharding", (ReshardingStatus.to_query f)));
           Util.option_map v.automatic_failover_status
             (fun f ->
                Query.Pair
                  ("AutomaticFailoverStatus",
                    (PendingAutomaticFailoverStatus.to_query f)));
           Util.option_map v.primary_cluster_id
             (fun f -> Query.Pair ("PrimaryClusterId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.user_groups
              (fun f -> ("user_groups", (UserGroupsUpdateStatus.to_json f)));
           Util.option_map v.auth_token_status
             (fun f ->
                ("auth_token_status", (AuthTokenUpdateStatus.to_json f)));
           Util.option_map v.resharding
             (fun f -> ("resharding", (ReshardingStatus.to_json f)));
           Util.option_map v.automatic_failover_status
             (fun f ->
                ("automatic_failover_status",
                  (PendingAutomaticFailoverStatus.to_json f)));
           Util.option_map v.primary_cluster_id
             (fun f -> ("primary_cluster_id", (String.to_json f)))])
    let of_json j =
      {
        primary_cluster_id =
          (Util.option_map (Json.lookup j "primary_cluster_id")
             String.of_json);
        automatic_failover_status =
          (Util.option_map (Json.lookup j "automatic_failover_status")
             PendingAutomaticFailoverStatus.of_json);
        resharding =
          (Util.option_map (Json.lookup j "resharding")
             ReshardingStatus.of_json);
        auth_token_status =
          (Util.option_map (Json.lookup j "auth_token_status")
             AuthTokenUpdateStatus.of_json);
        user_groups =
          (Util.option_map (Json.lookup j "user_groups")
             UserGroupsUpdateStatus.of_json)
      }
  end
module CacheParameterGroup =
  struct
    type t =
      {
      cache_parameter_group_name: String.t option ;
      cache_parameter_group_family: String.t option ;
      description: String.t option ;
      is_global: Boolean.t option ;
      a_r_n: String.t option }
    let make ?cache_parameter_group_name  ?cache_parameter_group_family 
      ?description  ?is_global  ?a_r_n  () =
      {
        cache_parameter_group_name;
        cache_parameter_group_family;
        description;
        is_global;
        a_r_n
      }
    let parse xml =
      Some
        {
          cache_parameter_group_name =
            (Util.option_bind (Xml.member "CacheParameterGroupName" xml)
               String.parse);
          cache_parameter_group_family =
            (Util.option_bind (Xml.member "CacheParameterGroupFamily" xml)
               String.parse);
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse);
          is_global =
            (Util.option_bind (Xml.member "IsGlobal" xml) Boolean.parse);
          a_r_n = (Util.option_bind (Xml.member "ARN" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.a_r_n
              (fun f -> Query.Pair ("ARN", (String.to_query f)));
           Util.option_map v.is_global
             (fun f -> Query.Pair ("IsGlobal", (Boolean.to_query f)));
           Util.option_map v.description
             (fun f -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.cache_parameter_group_family
             (fun f ->
                Query.Pair ("CacheParameterGroupFamily", (String.to_query f)));
           Util.option_map v.cache_parameter_group_name
             (fun f ->
                Query.Pair ("CacheParameterGroupName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.a_r_n (fun f -> ("a_r_n", (String.to_json f)));
           Util.option_map v.is_global
             (fun f -> ("is_global", (Boolean.to_json f)));
           Util.option_map v.description
             (fun f -> ("description", (String.to_json f)));
           Util.option_map v.cache_parameter_group_family
             (fun f -> ("cache_parameter_group_family", (String.to_json f)));
           Util.option_map v.cache_parameter_group_name
             (fun f -> ("cache_parameter_group_name", (String.to_json f)))])
    let of_json j =
      {
        cache_parameter_group_name =
          (Util.option_map (Json.lookup j "cache_parameter_group_name")
             String.of_json);
        cache_parameter_group_family =
          (Util.option_map (Json.lookup j "cache_parameter_group_family")
             String.of_json);
        description =
          (Util.option_map (Json.lookup j "description") String.of_json);
        is_global =
          (Util.option_map (Json.lookup j "is_global") Boolean.of_json);
        a_r_n = (Util.option_map (Json.lookup j "a_r_n") String.of_json)
      }
  end
module CacheSubnetGroup =
  struct
    type t =
      {
      cache_subnet_group_name: String.t option ;
      cache_subnet_group_description: String.t option ;
      vpc_id: String.t option ;
      subnets: SubnetList.t ;
      a_r_n: String.t option }
    let make ?cache_subnet_group_name  ?cache_subnet_group_description 
      ?vpc_id  ?(subnets= [])  ?a_r_n  () =
      {
        cache_subnet_group_name;
        cache_subnet_group_description;
        vpc_id;
        subnets;
        a_r_n
      }
    let parse xml =
      Some
        {
          cache_subnet_group_name =
            (Util.option_bind (Xml.member "CacheSubnetGroupName" xml)
               String.parse);
          cache_subnet_group_description =
            (Util.option_bind (Xml.member "CacheSubnetGroupDescription" xml)
               String.parse);
          vpc_id = (Util.option_bind (Xml.member "VpcId" xml) String.parse);
          subnets =
            (Util.of_option []
               (Util.option_bind (Xml.member "Subnets" xml) SubnetList.parse));
          a_r_n = (Util.option_bind (Xml.member "ARN" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.a_r_n
              (fun f -> Query.Pair ("ARN", (String.to_query f)));
           Some
             (Query.Pair ("Subnets.member", (SubnetList.to_query v.subnets)));
           Util.option_map v.vpc_id
             (fun f -> Query.Pair ("VpcId", (String.to_query f)));
           Util.option_map v.cache_subnet_group_description
             (fun f ->
                Query.Pair
                  ("CacheSubnetGroupDescription", (String.to_query f)));
           Util.option_map v.cache_subnet_group_name
             (fun f ->
                Query.Pair ("CacheSubnetGroupName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.a_r_n (fun f -> ("a_r_n", (String.to_json f)));
           Some ("subnets", (SubnetList.to_json v.subnets));
           Util.option_map v.vpc_id (fun f -> ("vpc_id", (String.to_json f)));
           Util.option_map v.cache_subnet_group_description
             (fun f -> ("cache_subnet_group_description", (String.to_json f)));
           Util.option_map v.cache_subnet_group_name
             (fun f -> ("cache_subnet_group_name", (String.to_json f)))])
    let of_json j =
      {
        cache_subnet_group_name =
          (Util.option_map (Json.lookup j "cache_subnet_group_name")
             String.of_json);
        cache_subnet_group_description =
          (Util.option_map (Json.lookup j "cache_subnet_group_description")
             String.of_json);
        vpc_id = (Util.option_map (Json.lookup j "vpc_id") String.of_json);
        subnets =
          (SubnetList.of_json (Util.of_option_exn (Json.lookup j "subnets")));
        a_r_n = (Util.option_map (Json.lookup j "a_r_n") String.of_json)
      }
  end
module Tag =
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
module CacheNodeTypeSpecificParametersList =
  struct
    type t = CacheNodeTypeSpecificParameter.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map CacheNodeTypeSpecificParameter.parse
           (Xml.members "CacheNodeTypeSpecificParameter" xml))
    let to_query v =
      Query.to_query_list CacheNodeTypeSpecificParameter.to_query v
    let to_json v = `List (List.map CacheNodeTypeSpecificParameter.to_json v)
    let of_json j = Json.to_list CacheNodeTypeSpecificParameter.of_json j
  end
module ParametersList =
  struct
    type t = Parameter.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map Parameter.parse (Xml.members "Parameter" xml))
    let to_query v = Query.to_query_list Parameter.to_query v
    let to_json v = `List (List.map Parameter.to_json v)
    let of_json j = Json.to_list Parameter.of_json j
  end
module RegionalConfiguration =
  struct
    type t =
      {
      replication_group_id: String.t ;
      replication_group_region: String.t ;
      resharding_configuration: ReshardingConfigurationList.t }
    let make ~replication_group_id  ~replication_group_region 
      ~resharding_configuration  () =
      {
        replication_group_id;
        replication_group_region;
        resharding_configuration
      }
    let parse xml =
      Some
        {
          replication_group_id =
            (Xml.required "ReplicationGroupId"
               (Util.option_bind (Xml.member "ReplicationGroupId" xml)
                  String.parse));
          replication_group_region =
            (Xml.required "ReplicationGroupRegion"
               (Util.option_bind (Xml.member "ReplicationGroupRegion" xml)
                  String.parse));
          resharding_configuration =
            (Xml.required "ReshardingConfiguration"
               (Util.option_bind (Xml.member "ReshardingConfiguration" xml)
                  ReshardingConfigurationList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ReshardingConfiguration.member",
                   (ReshardingConfigurationList.to_query
                      v.resharding_configuration)));
           Some
             (Query.Pair
                ("ReplicationGroupRegion",
                  (String.to_query v.replication_group_region)));
           Some
             (Query.Pair
                ("ReplicationGroupId",
                  (String.to_query v.replication_group_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("resharding_configuration",
                (ReshardingConfigurationList.to_json
                   v.resharding_configuration));
           Some
             ("replication_group_region",
               (String.to_json v.replication_group_region));
           Some
             ("replication_group_id",
               (String.to_json v.replication_group_id))])
    let of_json j =
      {
        replication_group_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "replication_group_id")));
        replication_group_region =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "replication_group_region")));
        resharding_configuration =
          (ReshardingConfigurationList.of_json
             (Util.of_option_exn (Json.lookup j "resharding_configuration")))
      }
  end
module CacheSecurityGroup =
  struct
    type t =
      {
      owner_id: String.t option ;
      cache_security_group_name: String.t option ;
      description: String.t option ;
      e_c2_security_groups: EC2SecurityGroupList.t ;
      a_r_n: String.t option }
    let make ?owner_id  ?cache_security_group_name  ?description 
      ?(e_c2_security_groups= [])  ?a_r_n  () =
      {
        owner_id;
        cache_security_group_name;
        description;
        e_c2_security_groups;
        a_r_n
      }
    let parse xml =
      Some
        {
          owner_id =
            (Util.option_bind (Xml.member "OwnerId" xml) String.parse);
          cache_security_group_name =
            (Util.option_bind (Xml.member "CacheSecurityGroupName" xml)
               String.parse);
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse);
          e_c2_security_groups =
            (Util.of_option []
               (Util.option_bind (Xml.member "EC2SecurityGroups" xml)
                  EC2SecurityGroupList.parse));
          a_r_n = (Util.option_bind (Xml.member "ARN" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.a_r_n
              (fun f -> Query.Pair ("ARN", (String.to_query f)));
           Some
             (Query.Pair
                ("EC2SecurityGroups.member",
                  (EC2SecurityGroupList.to_query v.e_c2_security_groups)));
           Util.option_map v.description
             (fun f -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.cache_security_group_name
             (fun f ->
                Query.Pair ("CacheSecurityGroupName", (String.to_query f)));
           Util.option_map v.owner_id
             (fun f -> Query.Pair ("OwnerId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.a_r_n (fun f -> ("a_r_n", (String.to_json f)));
           Some
             ("e_c2_security_groups",
               (EC2SecurityGroupList.to_json v.e_c2_security_groups));
           Util.option_map v.description
             (fun f -> ("description", (String.to_json f)));
           Util.option_map v.cache_security_group_name
             (fun f -> ("cache_security_group_name", (String.to_json f)));
           Util.option_map v.owner_id
             (fun f -> ("owner_id", (String.to_json f)))])
    let of_json j =
      {
        owner_id =
          (Util.option_map (Json.lookup j "owner_id") String.of_json);
        cache_security_group_name =
          (Util.option_map (Json.lookup j "cache_security_group_name")
             String.of_json);
        description =
          (Util.option_map (Json.lookup j "description") String.of_json);
        e_c2_security_groups =
          (EC2SecurityGroupList.of_json
             (Util.of_option_exn (Json.lookup j "e_c2_security_groups")));
        a_r_n = (Util.option_map (Json.lookup j "a_r_n") String.of_json)
      }
  end
module Filter =
  struct
    type t = {
      name: String.t ;
      values: FilterValueList.t }
    let make ~name  ~values  () = { name; values }
    let parse xml =
      Some
        {
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse));
          values =
            (Xml.required "Values"
               (Util.option_bind (Xml.member "Values" xml)
                  FilterValueList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Values.member", (FilterValueList.to_query v.values)));
           Some (Query.Pair ("Name", (String.to_query v.name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("values", (FilterValueList.to_json v.values));
           Some ("name", (String.to_json v.name))])
    let of_json j =
      {
        name = (String.of_json (Util.of_option_exn (Json.lookup j "name")));
        values =
          (FilterValueList.of_json
             (Util.of_option_exn (Json.lookup j "values")))
      }
  end
module CustomerNodeEndpoint =
  struct
    type t = {
      address: String.t option ;
      port: Integer.t option }
    let make ?address  ?port  () = { address; port }
    let parse xml =
      Some
        {
          address =
            (Util.option_bind (Xml.member "Address" xml) String.parse);
          port = (Util.option_bind (Xml.member "Port" xml) Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.port
              (fun f -> Query.Pair ("Port", (Integer.to_query f)));
           Util.option_map v.address
             (fun f -> Query.Pair ("Address", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.port (fun f -> ("port", (Integer.to_json f)));
           Util.option_map v.address
             (fun f -> ("address", (String.to_json f)))])
    let of_json j =
      {
        address = (Util.option_map (Json.lookup j "address") String.of_json);
        port = (Util.option_map (Json.lookup j "port") Integer.of_json)
      }
  end
module ReservedCacheNodesOffering =
  struct
    type t =
      {
      reserved_cache_nodes_offering_id: String.t option ;
      cache_node_type: String.t option ;
      duration: Integer.t option ;
      fixed_price: Double.t option ;
      usage_price: Double.t option ;
      product_description: String.t option ;
      offering_type: String.t option ;
      recurring_charges: RecurringChargeList.t }
    let make ?reserved_cache_nodes_offering_id  ?cache_node_type  ?duration 
      ?fixed_price  ?usage_price  ?product_description  ?offering_type 
      ?(recurring_charges= [])  () =
      {
        reserved_cache_nodes_offering_id;
        cache_node_type;
        duration;
        fixed_price;
        usage_price;
        product_description;
        offering_type;
        recurring_charges
      }
    let parse xml =
      Some
        {
          reserved_cache_nodes_offering_id =
            (Util.option_bind (Xml.member "ReservedCacheNodesOfferingId" xml)
               String.parse);
          cache_node_type =
            (Util.option_bind (Xml.member "CacheNodeType" xml) String.parse);
          duration =
            (Util.option_bind (Xml.member "Duration" xml) Integer.parse);
          fixed_price =
            (Util.option_bind (Xml.member "FixedPrice" xml) Double.parse);
          usage_price =
            (Util.option_bind (Xml.member "UsagePrice" xml) Double.parse);
          product_description =
            (Util.option_bind (Xml.member "ProductDescription" xml)
               String.parse);
          offering_type =
            (Util.option_bind (Xml.member "OfferingType" xml) String.parse);
          recurring_charges =
            (Util.of_option []
               (Util.option_bind (Xml.member "RecurringCharges" xml)
                  RecurringChargeList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("RecurringCharges.member",
                   (RecurringChargeList.to_query v.recurring_charges)));
           Util.option_map v.offering_type
             (fun f -> Query.Pair ("OfferingType", (String.to_query f)));
           Util.option_map v.product_description
             (fun f -> Query.Pair ("ProductDescription", (String.to_query f)));
           Util.option_map v.usage_price
             (fun f -> Query.Pair ("UsagePrice", (Double.to_query f)));
           Util.option_map v.fixed_price
             (fun f -> Query.Pair ("FixedPrice", (Double.to_query f)));
           Util.option_map v.duration
             (fun f -> Query.Pair ("Duration", (Integer.to_query f)));
           Util.option_map v.cache_node_type
             (fun f -> Query.Pair ("CacheNodeType", (String.to_query f)));
           Util.option_map v.reserved_cache_nodes_offering_id
             (fun f ->
                Query.Pair
                  ("ReservedCacheNodesOfferingId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("recurring_charges",
                (RecurringChargeList.to_json v.recurring_charges));
           Util.option_map v.offering_type
             (fun f -> ("offering_type", (String.to_json f)));
           Util.option_map v.product_description
             (fun f -> ("product_description", (String.to_json f)));
           Util.option_map v.usage_price
             (fun f -> ("usage_price", (Double.to_json f)));
           Util.option_map v.fixed_price
             (fun f -> ("fixed_price", (Double.to_json f)));
           Util.option_map v.duration
             (fun f -> ("duration", (Integer.to_json f)));
           Util.option_map v.cache_node_type
             (fun f -> ("cache_node_type", (String.to_json f)));
           Util.option_map v.reserved_cache_nodes_offering_id
             (fun f ->
                ("reserved_cache_nodes_offering_id", (String.to_json f)))])
    let of_json j =
      {
        reserved_cache_nodes_offering_id =
          (Util.option_map (Json.lookup j "reserved_cache_nodes_offering_id")
             String.of_json);
        cache_node_type =
          (Util.option_map (Json.lookup j "cache_node_type") String.of_json);
        duration =
          (Util.option_map (Json.lookup j "duration") Integer.of_json);
        fixed_price =
          (Util.option_map (Json.lookup j "fixed_price") Double.of_json);
        usage_price =
          (Util.option_map (Json.lookup j "usage_price") Double.of_json);
        product_description =
          (Util.option_map (Json.lookup j "product_description")
             String.of_json);
        offering_type =
          (Util.option_map (Json.lookup j "offering_type") String.of_json);
        recurring_charges =
          (RecurringChargeList.of_json
             (Util.of_option_exn (Json.lookup j "recurring_charges")))
      }
  end
module Snapshot =
  struct
    type t =
      {
      snapshot_name: String.t option ;
      replication_group_id: String.t option ;
      replication_group_description: String.t option ;
      cache_cluster_id: String.t option ;
      snapshot_status: String.t option ;
      snapshot_source: String.t option ;
      cache_node_type: String.t option ;
      engine: String.t option ;
      engine_version: String.t option ;
      num_cache_nodes: Integer.t option ;
      preferred_availability_zone: String.t option ;
      preferred_outpost_arn: String.t option ;
      cache_cluster_create_time: DateTime.t option ;
      preferred_maintenance_window: String.t option ;
      topic_arn: String.t option ;
      port: Integer.t option ;
      cache_parameter_group_name: String.t option ;
      cache_subnet_group_name: String.t option ;
      vpc_id: String.t option ;
      auto_minor_version_upgrade: Boolean.t option ;
      snapshot_retention_limit: Integer.t option ;
      snapshot_window: String.t option ;
      num_node_groups: Integer.t option ;
      automatic_failover: AutomaticFailoverStatus.t option ;
      node_snapshots: NodeSnapshotList.t ;
      kms_key_id: String.t option ;
      a_r_n: String.t option }
    let make ?snapshot_name  ?replication_group_id 
      ?replication_group_description  ?cache_cluster_id  ?snapshot_status 
      ?snapshot_source  ?cache_node_type  ?engine  ?engine_version 
      ?num_cache_nodes  ?preferred_availability_zone  ?preferred_outpost_arn 
      ?cache_cluster_create_time  ?preferred_maintenance_window  ?topic_arn 
      ?port  ?cache_parameter_group_name  ?cache_subnet_group_name  ?vpc_id 
      ?auto_minor_version_upgrade  ?snapshot_retention_limit 
      ?snapshot_window  ?num_node_groups  ?automatic_failover 
      ?(node_snapshots= [])  ?kms_key_id  ?a_r_n  () =
      {
        snapshot_name;
        replication_group_id;
        replication_group_description;
        cache_cluster_id;
        snapshot_status;
        snapshot_source;
        cache_node_type;
        engine;
        engine_version;
        num_cache_nodes;
        preferred_availability_zone;
        preferred_outpost_arn;
        cache_cluster_create_time;
        preferred_maintenance_window;
        topic_arn;
        port;
        cache_parameter_group_name;
        cache_subnet_group_name;
        vpc_id;
        auto_minor_version_upgrade;
        snapshot_retention_limit;
        snapshot_window;
        num_node_groups;
        automatic_failover;
        node_snapshots;
        kms_key_id;
        a_r_n
      }
    let parse xml =
      Some
        {
          snapshot_name =
            (Util.option_bind (Xml.member "SnapshotName" xml) String.parse);
          replication_group_id =
            (Util.option_bind (Xml.member "ReplicationGroupId" xml)
               String.parse);
          replication_group_description =
            (Util.option_bind (Xml.member "ReplicationGroupDescription" xml)
               String.parse);
          cache_cluster_id =
            (Util.option_bind (Xml.member "CacheClusterId" xml) String.parse);
          snapshot_status =
            (Util.option_bind (Xml.member "SnapshotStatus" xml) String.parse);
          snapshot_source =
            (Util.option_bind (Xml.member "SnapshotSource" xml) String.parse);
          cache_node_type =
            (Util.option_bind (Xml.member "CacheNodeType" xml) String.parse);
          engine = (Util.option_bind (Xml.member "Engine" xml) String.parse);
          engine_version =
            (Util.option_bind (Xml.member "EngineVersion" xml) String.parse);
          num_cache_nodes =
            (Util.option_bind (Xml.member "NumCacheNodes" xml) Integer.parse);
          preferred_availability_zone =
            (Util.option_bind (Xml.member "PreferredAvailabilityZone" xml)
               String.parse);
          preferred_outpost_arn =
            (Util.option_bind (Xml.member "PreferredOutpostArn" xml)
               String.parse);
          cache_cluster_create_time =
            (Util.option_bind (Xml.member "CacheClusterCreateTime" xml)
               DateTime.parse);
          preferred_maintenance_window =
            (Util.option_bind (Xml.member "PreferredMaintenanceWindow" xml)
               String.parse);
          topic_arn =
            (Util.option_bind (Xml.member "TopicArn" xml) String.parse);
          port = (Util.option_bind (Xml.member "Port" xml) Integer.parse);
          cache_parameter_group_name =
            (Util.option_bind (Xml.member "CacheParameterGroupName" xml)
               String.parse);
          cache_subnet_group_name =
            (Util.option_bind (Xml.member "CacheSubnetGroupName" xml)
               String.parse);
          vpc_id = (Util.option_bind (Xml.member "VpcId" xml) String.parse);
          auto_minor_version_upgrade =
            (Util.option_bind (Xml.member "AutoMinorVersionUpgrade" xml)
               Boolean.parse);
          snapshot_retention_limit =
            (Util.option_bind (Xml.member "SnapshotRetentionLimit" xml)
               Integer.parse);
          snapshot_window =
            (Util.option_bind (Xml.member "SnapshotWindow" xml) String.parse);
          num_node_groups =
            (Util.option_bind (Xml.member "NumNodeGroups" xml) Integer.parse);
          automatic_failover =
            (Util.option_bind (Xml.member "AutomaticFailover" xml)
               AutomaticFailoverStatus.parse);
          node_snapshots =
            (Util.of_option []
               (Util.option_bind (Xml.member "NodeSnapshots" xml)
                  NodeSnapshotList.parse));
          kms_key_id =
            (Util.option_bind (Xml.member "KmsKeyId" xml) String.parse);
          a_r_n = (Util.option_bind (Xml.member "ARN" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.a_r_n
              (fun f -> Query.Pair ("ARN", (String.to_query f)));
           Util.option_map v.kms_key_id
             (fun f -> Query.Pair ("KmsKeyId", (String.to_query f)));
           Some
             (Query.Pair
                ("NodeSnapshots.member",
                  (NodeSnapshotList.to_query v.node_snapshots)));
           Util.option_map v.automatic_failover
             (fun f ->
                Query.Pair
                  ("AutomaticFailover", (AutomaticFailoverStatus.to_query f)));
           Util.option_map v.num_node_groups
             (fun f -> Query.Pair ("NumNodeGroups", (Integer.to_query f)));
           Util.option_map v.snapshot_window
             (fun f -> Query.Pair ("SnapshotWindow", (String.to_query f)));
           Util.option_map v.snapshot_retention_limit
             (fun f ->
                Query.Pair ("SnapshotRetentionLimit", (Integer.to_query f)));
           Util.option_map v.auto_minor_version_upgrade
             (fun f ->
                Query.Pair ("AutoMinorVersionUpgrade", (Boolean.to_query f)));
           Util.option_map v.vpc_id
             (fun f -> Query.Pair ("VpcId", (String.to_query f)));
           Util.option_map v.cache_subnet_group_name
             (fun f ->
                Query.Pair ("CacheSubnetGroupName", (String.to_query f)));
           Util.option_map v.cache_parameter_group_name
             (fun f ->
                Query.Pair ("CacheParameterGroupName", (String.to_query f)));
           Util.option_map v.port
             (fun f -> Query.Pair ("Port", (Integer.to_query f)));
           Util.option_map v.topic_arn
             (fun f -> Query.Pair ("TopicArn", (String.to_query f)));
           Util.option_map v.preferred_maintenance_window
             (fun f ->
                Query.Pair
                  ("PreferredMaintenanceWindow", (String.to_query f)));
           Util.option_map v.cache_cluster_create_time
             (fun f ->
                Query.Pair ("CacheClusterCreateTime", (DateTime.to_query f)));
           Util.option_map v.preferred_outpost_arn
             (fun f ->
                Query.Pair ("PreferredOutpostArn", (String.to_query f)));
           Util.option_map v.preferred_availability_zone
             (fun f ->
                Query.Pair ("PreferredAvailabilityZone", (String.to_query f)));
           Util.option_map v.num_cache_nodes
             (fun f -> Query.Pair ("NumCacheNodes", (Integer.to_query f)));
           Util.option_map v.engine_version
             (fun f -> Query.Pair ("EngineVersion", (String.to_query f)));
           Util.option_map v.engine
             (fun f -> Query.Pair ("Engine", (String.to_query f)));
           Util.option_map v.cache_node_type
             (fun f -> Query.Pair ("CacheNodeType", (String.to_query f)));
           Util.option_map v.snapshot_source
             (fun f -> Query.Pair ("SnapshotSource", (String.to_query f)));
           Util.option_map v.snapshot_status
             (fun f -> Query.Pair ("SnapshotStatus", (String.to_query f)));
           Util.option_map v.cache_cluster_id
             (fun f -> Query.Pair ("CacheClusterId", (String.to_query f)));
           Util.option_map v.replication_group_description
             (fun f ->
                Query.Pair
                  ("ReplicationGroupDescription", (String.to_query f)));
           Util.option_map v.replication_group_id
             (fun f -> Query.Pair ("ReplicationGroupId", (String.to_query f)));
           Util.option_map v.snapshot_name
             (fun f -> Query.Pair ("SnapshotName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.a_r_n (fun f -> ("a_r_n", (String.to_json f)));
           Util.option_map v.kms_key_id
             (fun f -> ("kms_key_id", (String.to_json f)));
           Some
             ("node_snapshots", (NodeSnapshotList.to_json v.node_snapshots));
           Util.option_map v.automatic_failover
             (fun f ->
                ("automatic_failover", (AutomaticFailoverStatus.to_json f)));
           Util.option_map v.num_node_groups
             (fun f -> ("num_node_groups", (Integer.to_json f)));
           Util.option_map v.snapshot_window
             (fun f -> ("snapshot_window", (String.to_json f)));
           Util.option_map v.snapshot_retention_limit
             (fun f -> ("snapshot_retention_limit", (Integer.to_json f)));
           Util.option_map v.auto_minor_version_upgrade
             (fun f -> ("auto_minor_version_upgrade", (Boolean.to_json f)));
           Util.option_map v.vpc_id (fun f -> ("vpc_id", (String.to_json f)));
           Util.option_map v.cache_subnet_group_name
             (fun f -> ("cache_subnet_group_name", (String.to_json f)));
           Util.option_map v.cache_parameter_group_name
             (fun f -> ("cache_parameter_group_name", (String.to_json f)));
           Util.option_map v.port (fun f -> ("port", (Integer.to_json f)));
           Util.option_map v.topic_arn
             (fun f -> ("topic_arn", (String.to_json f)));
           Util.option_map v.preferred_maintenance_window
             (fun f -> ("preferred_maintenance_window", (String.to_json f)));
           Util.option_map v.cache_cluster_create_time
             (fun f -> ("cache_cluster_create_time", (DateTime.to_json f)));
           Util.option_map v.preferred_outpost_arn
             (fun f -> ("preferred_outpost_arn", (String.to_json f)));
           Util.option_map v.preferred_availability_zone
             (fun f -> ("preferred_availability_zone", (String.to_json f)));
           Util.option_map v.num_cache_nodes
             (fun f -> ("num_cache_nodes", (Integer.to_json f)));
           Util.option_map v.engine_version
             (fun f -> ("engine_version", (String.to_json f)));
           Util.option_map v.engine (fun f -> ("engine", (String.to_json f)));
           Util.option_map v.cache_node_type
             (fun f -> ("cache_node_type", (String.to_json f)));
           Util.option_map v.snapshot_source
             (fun f -> ("snapshot_source", (String.to_json f)));
           Util.option_map v.snapshot_status
             (fun f -> ("snapshot_status", (String.to_json f)));
           Util.option_map v.cache_cluster_id
             (fun f -> ("cache_cluster_id", (String.to_json f)));
           Util.option_map v.replication_group_description
             (fun f -> ("replication_group_description", (String.to_json f)));
           Util.option_map v.replication_group_id
             (fun f -> ("replication_group_id", (String.to_json f)));
           Util.option_map v.snapshot_name
             (fun f -> ("snapshot_name", (String.to_json f)))])
    let of_json j =
      {
        snapshot_name =
          (Util.option_map (Json.lookup j "snapshot_name") String.of_json);
        replication_group_id =
          (Util.option_map (Json.lookup j "replication_group_id")
             String.of_json);
        replication_group_description =
          (Util.option_map (Json.lookup j "replication_group_description")
             String.of_json);
        cache_cluster_id =
          (Util.option_map (Json.lookup j "cache_cluster_id") String.of_json);
        snapshot_status =
          (Util.option_map (Json.lookup j "snapshot_status") String.of_json);
        snapshot_source =
          (Util.option_map (Json.lookup j "snapshot_source") String.of_json);
        cache_node_type =
          (Util.option_map (Json.lookup j "cache_node_type") String.of_json);
        engine = (Util.option_map (Json.lookup j "engine") String.of_json);
        engine_version =
          (Util.option_map (Json.lookup j "engine_version") String.of_json);
        num_cache_nodes =
          (Util.option_map (Json.lookup j "num_cache_nodes") Integer.of_json);
        preferred_availability_zone =
          (Util.option_map (Json.lookup j "preferred_availability_zone")
             String.of_json);
        preferred_outpost_arn =
          (Util.option_map (Json.lookup j "preferred_outpost_arn")
             String.of_json);
        cache_cluster_create_time =
          (Util.option_map (Json.lookup j "cache_cluster_create_time")
             DateTime.of_json);
        preferred_maintenance_window =
          (Util.option_map (Json.lookup j "preferred_maintenance_window")
             String.of_json);
        topic_arn =
          (Util.option_map (Json.lookup j "topic_arn") String.of_json);
        port = (Util.option_map (Json.lookup j "port") Integer.of_json);
        cache_parameter_group_name =
          (Util.option_map (Json.lookup j "cache_parameter_group_name")
             String.of_json);
        cache_subnet_group_name =
          (Util.option_map (Json.lookup j "cache_subnet_group_name")
             String.of_json);
        vpc_id = (Util.option_map (Json.lookup j "vpc_id") String.of_json);
        auto_minor_version_upgrade =
          (Util.option_map (Json.lookup j "auto_minor_version_upgrade")
             Boolean.of_json);
        snapshot_retention_limit =
          (Util.option_map (Json.lookup j "snapshot_retention_limit")
             Integer.of_json);
        snapshot_window =
          (Util.option_map (Json.lookup j "snapshot_window") String.of_json);
        num_node_groups =
          (Util.option_map (Json.lookup j "num_node_groups") Integer.of_json);
        automatic_failover =
          (Util.option_map (Json.lookup j "automatic_failover")
             AutomaticFailoverStatus.of_json);
        node_snapshots =
          (NodeSnapshotList.of_json
             (Util.of_option_exn (Json.lookup j "node_snapshots")));
        kms_key_id =
          (Util.option_map (Json.lookup j "kms_key_id") String.of_json);
        a_r_n = (Util.option_map (Json.lookup j "a_r_n") String.of_json)
      }
  end
module GlobalReplicationGroup =
  struct
    type t =
      {
      global_replication_group_id: String.t option ;
      global_replication_group_description: String.t option ;
      status: String.t option ;
      cache_node_type: String.t option ;
      engine: String.t option ;
      engine_version: String.t option ;
      members: GlobalReplicationGroupMemberList.t ;
      cluster_enabled: Boolean.t option ;
      global_node_groups: GlobalNodeGroupList.t ;
      auth_token_enabled: Boolean.t option ;
      transit_encryption_enabled: Boolean.t option ;
      at_rest_encryption_enabled: Boolean.t option ;
      a_r_n: String.t option }
    let make ?global_replication_group_id 
      ?global_replication_group_description  ?status  ?cache_node_type 
      ?engine  ?engine_version  ?(members= [])  ?cluster_enabled 
      ?(global_node_groups= [])  ?auth_token_enabled 
      ?transit_encryption_enabled  ?at_rest_encryption_enabled  ?a_r_n  () =
      {
        global_replication_group_id;
        global_replication_group_description;
        status;
        cache_node_type;
        engine;
        engine_version;
        members;
        cluster_enabled;
        global_node_groups;
        auth_token_enabled;
        transit_encryption_enabled;
        at_rest_encryption_enabled;
        a_r_n
      }
    let parse xml =
      Some
        {
          global_replication_group_id =
            (Util.option_bind (Xml.member "GlobalReplicationGroupId" xml)
               String.parse);
          global_replication_group_description =
            (Util.option_bind
               (Xml.member "GlobalReplicationGroupDescription" xml)
               String.parse);
          status = (Util.option_bind (Xml.member "Status" xml) String.parse);
          cache_node_type =
            (Util.option_bind (Xml.member "CacheNodeType" xml) String.parse);
          engine = (Util.option_bind (Xml.member "Engine" xml) String.parse);
          engine_version =
            (Util.option_bind (Xml.member "EngineVersion" xml) String.parse);
          members =
            (Util.of_option []
               (Util.option_bind (Xml.member "Members" xml)
                  GlobalReplicationGroupMemberList.parse));
          cluster_enabled =
            (Util.option_bind (Xml.member "ClusterEnabled" xml) Boolean.parse);
          global_node_groups =
            (Util.of_option []
               (Util.option_bind (Xml.member "GlobalNodeGroups" xml)
                  GlobalNodeGroupList.parse));
          auth_token_enabled =
            (Util.option_bind (Xml.member "AuthTokenEnabled" xml)
               Boolean.parse);
          transit_encryption_enabled =
            (Util.option_bind (Xml.member "TransitEncryptionEnabled" xml)
               Boolean.parse);
          at_rest_encryption_enabled =
            (Util.option_bind (Xml.member "AtRestEncryptionEnabled" xml)
               Boolean.parse);
          a_r_n = (Util.option_bind (Xml.member "ARN" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.a_r_n
              (fun f -> Query.Pair ("ARN", (String.to_query f)));
           Util.option_map v.at_rest_encryption_enabled
             (fun f ->
                Query.Pair ("AtRestEncryptionEnabled", (Boolean.to_query f)));
           Util.option_map v.transit_encryption_enabled
             (fun f ->
                Query.Pair ("TransitEncryptionEnabled", (Boolean.to_query f)));
           Util.option_map v.auth_token_enabled
             (fun f -> Query.Pair ("AuthTokenEnabled", (Boolean.to_query f)));
           Some
             (Query.Pair
                ("GlobalNodeGroups.member",
                  (GlobalNodeGroupList.to_query v.global_node_groups)));
           Util.option_map v.cluster_enabled
             (fun f -> Query.Pair ("ClusterEnabled", (Boolean.to_query f)));
           Some
             (Query.Pair
                ("Members.member",
                  (GlobalReplicationGroupMemberList.to_query v.members)));
           Util.option_map v.engine_version
             (fun f -> Query.Pair ("EngineVersion", (String.to_query f)));
           Util.option_map v.engine
             (fun f -> Query.Pair ("Engine", (String.to_query f)));
           Util.option_map v.cache_node_type
             (fun f -> Query.Pair ("CacheNodeType", (String.to_query f)));
           Util.option_map v.status
             (fun f -> Query.Pair ("Status", (String.to_query f)));
           Util.option_map v.global_replication_group_description
             (fun f ->
                Query.Pair
                  ("GlobalReplicationGroupDescription", (String.to_query f)));
           Util.option_map v.global_replication_group_id
             (fun f ->
                Query.Pair ("GlobalReplicationGroupId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.a_r_n (fun f -> ("a_r_n", (String.to_json f)));
           Util.option_map v.at_rest_encryption_enabled
             (fun f -> ("at_rest_encryption_enabled", (Boolean.to_json f)));
           Util.option_map v.transit_encryption_enabled
             (fun f -> ("transit_encryption_enabled", (Boolean.to_json f)));
           Util.option_map v.auth_token_enabled
             (fun f -> ("auth_token_enabled", (Boolean.to_json f)));
           Some
             ("global_node_groups",
               (GlobalNodeGroupList.to_json v.global_node_groups));
           Util.option_map v.cluster_enabled
             (fun f -> ("cluster_enabled", (Boolean.to_json f)));
           Some
             ("members",
               (GlobalReplicationGroupMemberList.to_json v.members));
           Util.option_map v.engine_version
             (fun f -> ("engine_version", (String.to_json f)));
           Util.option_map v.engine (fun f -> ("engine", (String.to_json f)));
           Util.option_map v.cache_node_type
             (fun f -> ("cache_node_type", (String.to_json f)));
           Util.option_map v.status (fun f -> ("status", (String.to_json f)));
           Util.option_map v.global_replication_group_description
             (fun f ->
                ("global_replication_group_description", (String.to_json f)));
           Util.option_map v.global_replication_group_id
             (fun f -> ("global_replication_group_id", (String.to_json f)))])
    let of_json j =
      {
        global_replication_group_id =
          (Util.option_map (Json.lookup j "global_replication_group_id")
             String.of_json);
        global_replication_group_description =
          (Util.option_map
             (Json.lookup j "global_replication_group_description")
             String.of_json);
        status = (Util.option_map (Json.lookup j "status") String.of_json);
        cache_node_type =
          (Util.option_map (Json.lookup j "cache_node_type") String.of_json);
        engine = (Util.option_map (Json.lookup j "engine") String.of_json);
        engine_version =
          (Util.option_map (Json.lookup j "engine_version") String.of_json);
        members =
          (GlobalReplicationGroupMemberList.of_json
             (Util.of_option_exn (Json.lookup j "members")));
        cluster_enabled =
          (Util.option_map (Json.lookup j "cluster_enabled") Boolean.of_json);
        global_node_groups =
          (GlobalNodeGroupList.of_json
             (Util.of_option_exn (Json.lookup j "global_node_groups")));
        auth_token_enabled =
          (Util.option_map (Json.lookup j "auth_token_enabled")
             Boolean.of_json);
        transit_encryption_enabled =
          (Util.option_map (Json.lookup j "transit_encryption_enabled")
             Boolean.of_json);
        at_rest_encryption_enabled =
          (Util.option_map (Json.lookup j "at_rest_encryption_enabled")
             Boolean.of_json);
        a_r_n = (Util.option_map (Json.lookup j "a_r_n") String.of_json)
      }
  end
module ServiceUpdate =
  struct
    type t =
      {
      service_update_name: String.t option ;
      service_update_release_date: DateTime.t option ;
      service_update_end_date: DateTime.t option ;
      service_update_severity: ServiceUpdateSeverity.t option ;
      service_update_recommended_apply_by_date: DateTime.t option ;
      service_update_status: ServiceUpdateStatus.t option ;
      service_update_description: String.t option ;
      service_update_type: ServiceUpdateType.t option ;
      engine: String.t option ;
      engine_version: String.t option ;
      auto_update_after_recommended_apply_by_date: Boolean.t option ;
      estimated_update_time: String.t option }
    let make ?service_update_name  ?service_update_release_date 
      ?service_update_end_date  ?service_update_severity 
      ?service_update_recommended_apply_by_date  ?service_update_status 
      ?service_update_description  ?service_update_type  ?engine 
      ?engine_version  ?auto_update_after_recommended_apply_by_date 
      ?estimated_update_time  () =
      {
        service_update_name;
        service_update_release_date;
        service_update_end_date;
        service_update_severity;
        service_update_recommended_apply_by_date;
        service_update_status;
        service_update_description;
        service_update_type;
        engine;
        engine_version;
        auto_update_after_recommended_apply_by_date;
        estimated_update_time
      }
    let parse xml =
      Some
        {
          service_update_name =
            (Util.option_bind (Xml.member "ServiceUpdateName" xml)
               String.parse);
          service_update_release_date =
            (Util.option_bind (Xml.member "ServiceUpdateReleaseDate" xml)
               DateTime.parse);
          service_update_end_date =
            (Util.option_bind (Xml.member "ServiceUpdateEndDate" xml)
               DateTime.parse);
          service_update_severity =
            (Util.option_bind (Xml.member "ServiceUpdateSeverity" xml)
               ServiceUpdateSeverity.parse);
          service_update_recommended_apply_by_date =
            (Util.option_bind
               (Xml.member "ServiceUpdateRecommendedApplyByDate" xml)
               DateTime.parse);
          service_update_status =
            (Util.option_bind (Xml.member "ServiceUpdateStatus" xml)
               ServiceUpdateStatus.parse);
          service_update_description =
            (Util.option_bind (Xml.member "ServiceUpdateDescription" xml)
               String.parse);
          service_update_type =
            (Util.option_bind (Xml.member "ServiceUpdateType" xml)
               ServiceUpdateType.parse);
          engine = (Util.option_bind (Xml.member "Engine" xml) String.parse);
          engine_version =
            (Util.option_bind (Xml.member "EngineVersion" xml) String.parse);
          auto_update_after_recommended_apply_by_date =
            (Util.option_bind
               (Xml.member "AutoUpdateAfterRecommendedApplyByDate" xml)
               Boolean.parse);
          estimated_update_time =
            (Util.option_bind (Xml.member "EstimatedUpdateTime" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.estimated_update_time
              (fun f ->
                 Query.Pair ("EstimatedUpdateTime", (String.to_query f)));
           Util.option_map v.auto_update_after_recommended_apply_by_date
             (fun f ->
                Query.Pair
                  ("AutoUpdateAfterRecommendedApplyByDate",
                    (Boolean.to_query f)));
           Util.option_map v.engine_version
             (fun f -> Query.Pair ("EngineVersion", (String.to_query f)));
           Util.option_map v.engine
             (fun f -> Query.Pair ("Engine", (String.to_query f)));
           Util.option_map v.service_update_type
             (fun f ->
                Query.Pair
                  ("ServiceUpdateType", (ServiceUpdateType.to_query f)));
           Util.option_map v.service_update_description
             (fun f ->
                Query.Pair ("ServiceUpdateDescription", (String.to_query f)));
           Util.option_map v.service_update_status
             (fun f ->
                Query.Pair
                  ("ServiceUpdateStatus", (ServiceUpdateStatus.to_query f)));
           Util.option_map v.service_update_recommended_apply_by_date
             (fun f ->
                Query.Pair
                  ("ServiceUpdateRecommendedApplyByDate",
                    (DateTime.to_query f)));
           Util.option_map v.service_update_severity
             (fun f ->
                Query.Pair
                  ("ServiceUpdateSeverity",
                    (ServiceUpdateSeverity.to_query f)));
           Util.option_map v.service_update_end_date
             (fun f ->
                Query.Pair ("ServiceUpdateEndDate", (DateTime.to_query f)));
           Util.option_map v.service_update_release_date
             (fun f ->
                Query.Pair
                  ("ServiceUpdateReleaseDate", (DateTime.to_query f)));
           Util.option_map v.service_update_name
             (fun f -> Query.Pair ("ServiceUpdateName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.estimated_update_time
              (fun f -> ("estimated_update_time", (String.to_json f)));
           Util.option_map v.auto_update_after_recommended_apply_by_date
             (fun f ->
                ("auto_update_after_recommended_apply_by_date",
                  (Boolean.to_json f)));
           Util.option_map v.engine_version
             (fun f -> ("engine_version", (String.to_json f)));
           Util.option_map v.engine (fun f -> ("engine", (String.to_json f)));
           Util.option_map v.service_update_type
             (fun f -> ("service_update_type", (ServiceUpdateType.to_json f)));
           Util.option_map v.service_update_description
             (fun f -> ("service_update_description", (String.to_json f)));
           Util.option_map v.service_update_status
             (fun f ->
                ("service_update_status", (ServiceUpdateStatus.to_json f)));
           Util.option_map v.service_update_recommended_apply_by_date
             (fun f ->
                ("service_update_recommended_apply_by_date",
                  (DateTime.to_json f)));
           Util.option_map v.service_update_severity
             (fun f ->
                ("service_update_severity",
                  (ServiceUpdateSeverity.to_json f)));
           Util.option_map v.service_update_end_date
             (fun f -> ("service_update_end_date", (DateTime.to_json f)));
           Util.option_map v.service_update_release_date
             (fun f -> ("service_update_release_date", (DateTime.to_json f)));
           Util.option_map v.service_update_name
             (fun f -> ("service_update_name", (String.to_json f)))])
    let of_json j =
      {
        service_update_name =
          (Util.option_map (Json.lookup j "service_update_name")
             String.of_json);
        service_update_release_date =
          (Util.option_map (Json.lookup j "service_update_release_date")
             DateTime.of_json);
        service_update_end_date =
          (Util.option_map (Json.lookup j "service_update_end_date")
             DateTime.of_json);
        service_update_severity =
          (Util.option_map (Json.lookup j "service_update_severity")
             ServiceUpdateSeverity.of_json);
        service_update_recommended_apply_by_date =
          (Util.option_map
             (Json.lookup j "service_update_recommended_apply_by_date")
             DateTime.of_json);
        service_update_status =
          (Util.option_map (Json.lookup j "service_update_status")
             ServiceUpdateStatus.of_json);
        service_update_description =
          (Util.option_map (Json.lookup j "service_update_description")
             String.of_json);
        service_update_type =
          (Util.option_map (Json.lookup j "service_update_type")
             ServiceUpdateType.of_json);
        engine = (Util.option_map (Json.lookup j "engine") String.of_json);
        engine_version =
          (Util.option_map (Json.lookup j "engine_version") String.of_json);
        auto_update_after_recommended_apply_by_date =
          (Util.option_map
             (Json.lookup j "auto_update_after_recommended_apply_by_date")
             Boolean.of_json);
        estimated_update_time =
          (Util.option_map (Json.lookup j "estimated_update_time")
             String.of_json)
      }
  end
module User =
  struct
    type t =
      {
      user_id: String.t option ;
      user_name: String.t option ;
      status: String.t option ;
      engine: String.t option ;
      access_string: String.t option ;
      user_group_ids: UserGroupIdList.t ;
      authentication: Authentication.t option ;
      a_r_n: String.t option }
    let make ?user_id  ?user_name  ?status  ?engine  ?access_string 
      ?(user_group_ids= [])  ?authentication  ?a_r_n  () =
      {
        user_id;
        user_name;
        status;
        engine;
        access_string;
        user_group_ids;
        authentication;
        a_r_n
      }
    let parse xml =
      Some
        {
          user_id = (Util.option_bind (Xml.member "UserId" xml) String.parse);
          user_name =
            (Util.option_bind (Xml.member "UserName" xml) String.parse);
          status = (Util.option_bind (Xml.member "Status" xml) String.parse);
          engine = (Util.option_bind (Xml.member "Engine" xml) String.parse);
          access_string =
            (Util.option_bind (Xml.member "AccessString" xml) String.parse);
          user_group_ids =
            (Util.of_option []
               (Util.option_bind (Xml.member "UserGroupIds" xml)
                  UserGroupIdList.parse));
          authentication =
            (Util.option_bind (Xml.member "Authentication" xml)
               Authentication.parse);
          a_r_n = (Util.option_bind (Xml.member "ARN" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.a_r_n
              (fun f -> Query.Pair ("ARN", (String.to_query f)));
           Util.option_map v.authentication
             (fun f ->
                Query.Pair ("Authentication", (Authentication.to_query f)));
           Some
             (Query.Pair
                ("UserGroupIds.member",
                  (UserGroupIdList.to_query v.user_group_ids)));
           Util.option_map v.access_string
             (fun f -> Query.Pair ("AccessString", (String.to_query f)));
           Util.option_map v.engine
             (fun f -> Query.Pair ("Engine", (String.to_query f)));
           Util.option_map v.status
             (fun f -> Query.Pair ("Status", (String.to_query f)));
           Util.option_map v.user_name
             (fun f -> Query.Pair ("UserName", (String.to_query f)));
           Util.option_map v.user_id
             (fun f -> Query.Pair ("UserId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.a_r_n (fun f -> ("a_r_n", (String.to_json f)));
           Util.option_map v.authentication
             (fun f -> ("authentication", (Authentication.to_json f)));
           Some
             ("user_group_ids", (UserGroupIdList.to_json v.user_group_ids));
           Util.option_map v.access_string
             (fun f -> ("access_string", (String.to_json f)));
           Util.option_map v.engine (fun f -> ("engine", (String.to_json f)));
           Util.option_map v.status (fun f -> ("status", (String.to_json f)));
           Util.option_map v.user_name
             (fun f -> ("user_name", (String.to_json f)));
           Util.option_map v.user_id
             (fun f -> ("user_id", (String.to_json f)))])
    let of_json j =
      {
        user_id = (Util.option_map (Json.lookup j "user_id") String.of_json);
        user_name =
          (Util.option_map (Json.lookup j "user_name") String.of_json);
        status = (Util.option_map (Json.lookup j "status") String.of_json);
        engine = (Util.option_map (Json.lookup j "engine") String.of_json);
        access_string =
          (Util.option_map (Json.lookup j "access_string") String.of_json);
        user_group_ids =
          (UserGroupIdList.of_json
             (Util.of_option_exn (Json.lookup j "user_group_ids")));
        authentication =
          (Util.option_map (Json.lookup j "authentication")
             Authentication.of_json);
        a_r_n = (Util.option_map (Json.lookup j "a_r_n") String.of_json)
      }
  end
module ConfigureShard =
  struct
    type t =
      {
      node_group_id: String.t ;
      new_replica_count: Integer.t ;
      preferred_availability_zones: PreferredAvailabilityZoneList.t ;
      preferred_outpost_arns: PreferredOutpostArnList.t }
    let make ~node_group_id  ~new_replica_count 
      ?(preferred_availability_zones= [])  ?(preferred_outpost_arns= [])  ()
      =
      {
        node_group_id;
        new_replica_count;
        preferred_availability_zones;
        preferred_outpost_arns
      }
    let parse xml =
      Some
        {
          node_group_id =
            (Xml.required "NodeGroupId"
               (Util.option_bind (Xml.member "NodeGroupId" xml) String.parse));
          new_replica_count =
            (Xml.required "NewReplicaCount"
               (Util.option_bind (Xml.member "NewReplicaCount" xml)
                  Integer.parse));
          preferred_availability_zones =
            (Util.of_option []
               (Util.option_bind
                  (Xml.member "PreferredAvailabilityZones" xml)
                  PreferredAvailabilityZoneList.parse));
          preferred_outpost_arns =
            (Util.of_option []
               (Util.option_bind (Xml.member "PreferredOutpostArns" xml)
                  PreferredOutpostArnList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("PreferredOutpostArns.member",
                   (PreferredOutpostArnList.to_query v.preferred_outpost_arns)));
           Some
             (Query.Pair
                ("PreferredAvailabilityZones.member",
                  (PreferredAvailabilityZoneList.to_query
                     v.preferred_availability_zones)));
           Some
             (Query.Pair
                ("NewReplicaCount", (Integer.to_query v.new_replica_count)));
           Some
             (Query.Pair ("NodeGroupId", (String.to_query v.node_group_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("preferred_outpost_arns",
                (PreferredOutpostArnList.to_json v.preferred_outpost_arns));
           Some
             ("preferred_availability_zones",
               (PreferredAvailabilityZoneList.to_json
                  v.preferred_availability_zones));
           Some ("new_replica_count", (Integer.to_json v.new_replica_count));
           Some ("node_group_id", (String.to_json v.node_group_id))])
    let of_json j =
      {
        node_group_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "node_group_id")));
        new_replica_count =
          (Integer.of_json
             (Util.of_option_exn (Json.lookup j "new_replica_count")));
        preferred_availability_zones =
          (PreferredAvailabilityZoneList.of_json
             (Util.of_option_exn
                (Json.lookup j "preferred_availability_zones")));
        preferred_outpost_arns =
          (PreferredOutpostArnList.of_json
             (Util.of_option_exn (Json.lookup j "preferred_outpost_arns")))
      }
  end
module ParameterNameValue =
  struct
    type t =
      {
      parameter_name: String.t option ;
      parameter_value: String.t option }
    let make ?parameter_name  ?parameter_value  () =
      { parameter_name; parameter_value }
    let parse xml =
      Some
        {
          parameter_name =
            (Util.option_bind (Xml.member "ParameterName" xml) String.parse);
          parameter_value =
            (Util.option_bind (Xml.member "ParameterValue" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.parameter_value
              (fun f -> Query.Pair ("ParameterValue", (String.to_query f)));
           Util.option_map v.parameter_name
             (fun f -> Query.Pair ("ParameterName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.parameter_value
              (fun f -> ("parameter_value", (String.to_json f)));
           Util.option_map v.parameter_name
             (fun f -> ("parameter_name", (String.to_json f)))])
    let of_json j =
      {
        parameter_name =
          (Util.option_map (Json.lookup j "parameter_name") String.of_json);
        parameter_value =
          (Util.option_map (Json.lookup j "parameter_value") String.of_json)
      }
  end
module CacheCluster =
  struct
    type t =
      {
      cache_cluster_id: String.t option ;
      configuration_endpoint: Endpoint.t option ;
      client_download_landing_page: String.t option ;
      cache_node_type: String.t option ;
      engine: String.t option ;
      engine_version: String.t option ;
      cache_cluster_status: String.t option ;
      num_cache_nodes: Integer.t option ;
      preferred_availability_zone: String.t option ;
      preferred_outpost_arn: String.t option ;
      cache_cluster_create_time: DateTime.t option ;
      preferred_maintenance_window: String.t option ;
      pending_modified_values: PendingModifiedValues.t option ;
      notification_configuration: NotificationConfiguration.t option ;
      cache_security_groups: CacheSecurityGroupMembershipList.t ;
      cache_parameter_group: CacheParameterGroupStatus.t option ;
      cache_subnet_group_name: String.t option ;
      cache_nodes: CacheNodeList.t ;
      auto_minor_version_upgrade: Boolean.t option ;
      security_groups: SecurityGroupMembershipList.t ;
      replication_group_id: String.t option ;
      snapshot_retention_limit: Integer.t option ;
      snapshot_window: String.t option ;
      auth_token_enabled: Boolean.t option ;
      auth_token_last_modified_date: DateTime.t option ;
      transit_encryption_enabled: Boolean.t option ;
      at_rest_encryption_enabled: Boolean.t option ;
      a_r_n: String.t option }
    let make ?cache_cluster_id  ?configuration_endpoint 
      ?client_download_landing_page  ?cache_node_type  ?engine 
      ?engine_version  ?cache_cluster_status  ?num_cache_nodes 
      ?preferred_availability_zone  ?preferred_outpost_arn 
      ?cache_cluster_create_time  ?preferred_maintenance_window 
      ?pending_modified_values  ?notification_configuration 
      ?(cache_security_groups= [])  ?cache_parameter_group 
      ?cache_subnet_group_name  ?(cache_nodes= []) 
      ?auto_minor_version_upgrade  ?(security_groups= []) 
      ?replication_group_id  ?snapshot_retention_limit  ?snapshot_window 
      ?auth_token_enabled  ?auth_token_last_modified_date 
      ?transit_encryption_enabled  ?at_rest_encryption_enabled  ?a_r_n  () =
      {
        cache_cluster_id;
        configuration_endpoint;
        client_download_landing_page;
        cache_node_type;
        engine;
        engine_version;
        cache_cluster_status;
        num_cache_nodes;
        preferred_availability_zone;
        preferred_outpost_arn;
        cache_cluster_create_time;
        preferred_maintenance_window;
        pending_modified_values;
        notification_configuration;
        cache_security_groups;
        cache_parameter_group;
        cache_subnet_group_name;
        cache_nodes;
        auto_minor_version_upgrade;
        security_groups;
        replication_group_id;
        snapshot_retention_limit;
        snapshot_window;
        auth_token_enabled;
        auth_token_last_modified_date;
        transit_encryption_enabled;
        at_rest_encryption_enabled;
        a_r_n
      }
    let parse xml =
      Some
        {
          cache_cluster_id =
            (Util.option_bind (Xml.member "CacheClusterId" xml) String.parse);
          configuration_endpoint =
            (Util.option_bind (Xml.member "ConfigurationEndpoint" xml)
               Endpoint.parse);
          client_download_landing_page =
            (Util.option_bind (Xml.member "ClientDownloadLandingPage" xml)
               String.parse);
          cache_node_type =
            (Util.option_bind (Xml.member "CacheNodeType" xml) String.parse);
          engine = (Util.option_bind (Xml.member "Engine" xml) String.parse);
          engine_version =
            (Util.option_bind (Xml.member "EngineVersion" xml) String.parse);
          cache_cluster_status =
            (Util.option_bind (Xml.member "CacheClusterStatus" xml)
               String.parse);
          num_cache_nodes =
            (Util.option_bind (Xml.member "NumCacheNodes" xml) Integer.parse);
          preferred_availability_zone =
            (Util.option_bind (Xml.member "PreferredAvailabilityZone" xml)
               String.parse);
          preferred_outpost_arn =
            (Util.option_bind (Xml.member "PreferredOutpostArn" xml)
               String.parse);
          cache_cluster_create_time =
            (Util.option_bind (Xml.member "CacheClusterCreateTime" xml)
               DateTime.parse);
          preferred_maintenance_window =
            (Util.option_bind (Xml.member "PreferredMaintenanceWindow" xml)
               String.parse);
          pending_modified_values =
            (Util.option_bind (Xml.member "PendingModifiedValues" xml)
               PendingModifiedValues.parse);
          notification_configuration =
            (Util.option_bind (Xml.member "NotificationConfiguration" xml)
               NotificationConfiguration.parse);
          cache_security_groups =
            (Util.of_option []
               (Util.option_bind (Xml.member "CacheSecurityGroups" xml)
                  CacheSecurityGroupMembershipList.parse));
          cache_parameter_group =
            (Util.option_bind (Xml.member "CacheParameterGroup" xml)
               CacheParameterGroupStatus.parse);
          cache_subnet_group_name =
            (Util.option_bind (Xml.member "CacheSubnetGroupName" xml)
               String.parse);
          cache_nodes =
            (Util.of_option []
               (Util.option_bind (Xml.member "CacheNodes" xml)
                  CacheNodeList.parse));
          auto_minor_version_upgrade =
            (Util.option_bind (Xml.member "AutoMinorVersionUpgrade" xml)
               Boolean.parse);
          security_groups =
            (Util.of_option []
               (Util.option_bind (Xml.member "SecurityGroups" xml)
                  SecurityGroupMembershipList.parse));
          replication_group_id =
            (Util.option_bind (Xml.member "ReplicationGroupId" xml)
               String.parse);
          snapshot_retention_limit =
            (Util.option_bind (Xml.member "SnapshotRetentionLimit" xml)
               Integer.parse);
          snapshot_window =
            (Util.option_bind (Xml.member "SnapshotWindow" xml) String.parse);
          auth_token_enabled =
            (Util.option_bind (Xml.member "AuthTokenEnabled" xml)
               Boolean.parse);
          auth_token_last_modified_date =
            (Util.option_bind (Xml.member "AuthTokenLastModifiedDate" xml)
               DateTime.parse);
          transit_encryption_enabled =
            (Util.option_bind (Xml.member "TransitEncryptionEnabled" xml)
               Boolean.parse);
          at_rest_encryption_enabled =
            (Util.option_bind (Xml.member "AtRestEncryptionEnabled" xml)
               Boolean.parse);
          a_r_n = (Util.option_bind (Xml.member "ARN" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.a_r_n
              (fun f -> Query.Pair ("ARN", (String.to_query f)));
           Util.option_map v.at_rest_encryption_enabled
             (fun f ->
                Query.Pair ("AtRestEncryptionEnabled", (Boolean.to_query f)));
           Util.option_map v.transit_encryption_enabled
             (fun f ->
                Query.Pair ("TransitEncryptionEnabled", (Boolean.to_query f)));
           Util.option_map v.auth_token_last_modified_date
             (fun f ->
                Query.Pair
                  ("AuthTokenLastModifiedDate", (DateTime.to_query f)));
           Util.option_map v.auth_token_enabled
             (fun f -> Query.Pair ("AuthTokenEnabled", (Boolean.to_query f)));
           Util.option_map v.snapshot_window
             (fun f -> Query.Pair ("SnapshotWindow", (String.to_query f)));
           Util.option_map v.snapshot_retention_limit
             (fun f ->
                Query.Pair ("SnapshotRetentionLimit", (Integer.to_query f)));
           Util.option_map v.replication_group_id
             (fun f -> Query.Pair ("ReplicationGroupId", (String.to_query f)));
           Some
             (Query.Pair
                ("SecurityGroups.member",
                  (SecurityGroupMembershipList.to_query v.security_groups)));
           Util.option_map v.auto_minor_version_upgrade
             (fun f ->
                Query.Pair ("AutoMinorVersionUpgrade", (Boolean.to_query f)));
           Some
             (Query.Pair
                ("CacheNodes.member", (CacheNodeList.to_query v.cache_nodes)));
           Util.option_map v.cache_subnet_group_name
             (fun f ->
                Query.Pair ("CacheSubnetGroupName", (String.to_query f)));
           Util.option_map v.cache_parameter_group
             (fun f ->
                Query.Pair
                  ("CacheParameterGroup",
                    (CacheParameterGroupStatus.to_query f)));
           Some
             (Query.Pair
                ("CacheSecurityGroups.member",
                  (CacheSecurityGroupMembershipList.to_query
                     v.cache_security_groups)));
           Util.option_map v.notification_configuration
             (fun f ->
                Query.Pair
                  ("NotificationConfiguration",
                    (NotificationConfiguration.to_query f)));
           Util.option_map v.pending_modified_values
             (fun f ->
                Query.Pair
                  ("PendingModifiedValues",
                    (PendingModifiedValues.to_query f)));
           Util.option_map v.preferred_maintenance_window
             (fun f ->
                Query.Pair
                  ("PreferredMaintenanceWindow", (String.to_query f)));
           Util.option_map v.cache_cluster_create_time
             (fun f ->
                Query.Pair ("CacheClusterCreateTime", (DateTime.to_query f)));
           Util.option_map v.preferred_outpost_arn
             (fun f ->
                Query.Pair ("PreferredOutpostArn", (String.to_query f)));
           Util.option_map v.preferred_availability_zone
             (fun f ->
                Query.Pair ("PreferredAvailabilityZone", (String.to_query f)));
           Util.option_map v.num_cache_nodes
             (fun f -> Query.Pair ("NumCacheNodes", (Integer.to_query f)));
           Util.option_map v.cache_cluster_status
             (fun f -> Query.Pair ("CacheClusterStatus", (String.to_query f)));
           Util.option_map v.engine_version
             (fun f -> Query.Pair ("EngineVersion", (String.to_query f)));
           Util.option_map v.engine
             (fun f -> Query.Pair ("Engine", (String.to_query f)));
           Util.option_map v.cache_node_type
             (fun f -> Query.Pair ("CacheNodeType", (String.to_query f)));
           Util.option_map v.client_download_landing_page
             (fun f ->
                Query.Pair ("ClientDownloadLandingPage", (String.to_query f)));
           Util.option_map v.configuration_endpoint
             (fun f ->
                Query.Pair ("ConfigurationEndpoint", (Endpoint.to_query f)));
           Util.option_map v.cache_cluster_id
             (fun f -> Query.Pair ("CacheClusterId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.a_r_n (fun f -> ("a_r_n", (String.to_json f)));
           Util.option_map v.at_rest_encryption_enabled
             (fun f -> ("at_rest_encryption_enabled", (Boolean.to_json f)));
           Util.option_map v.transit_encryption_enabled
             (fun f -> ("transit_encryption_enabled", (Boolean.to_json f)));
           Util.option_map v.auth_token_last_modified_date
             (fun f ->
                ("auth_token_last_modified_date", (DateTime.to_json f)));
           Util.option_map v.auth_token_enabled
             (fun f -> ("auth_token_enabled", (Boolean.to_json f)));
           Util.option_map v.snapshot_window
             (fun f -> ("snapshot_window", (String.to_json f)));
           Util.option_map v.snapshot_retention_limit
             (fun f -> ("snapshot_retention_limit", (Integer.to_json f)));
           Util.option_map v.replication_group_id
             (fun f -> ("replication_group_id", (String.to_json f)));
           Some
             ("security_groups",
               (SecurityGroupMembershipList.to_json v.security_groups));
           Util.option_map v.auto_minor_version_upgrade
             (fun f -> ("auto_minor_version_upgrade", (Boolean.to_json f)));
           Some ("cache_nodes", (CacheNodeList.to_json v.cache_nodes));
           Util.option_map v.cache_subnet_group_name
             (fun f -> ("cache_subnet_group_name", (String.to_json f)));
           Util.option_map v.cache_parameter_group
             (fun f ->
                ("cache_parameter_group",
                  (CacheParameterGroupStatus.to_json f)));
           Some
             ("cache_security_groups",
               (CacheSecurityGroupMembershipList.to_json
                  v.cache_security_groups));
           Util.option_map v.notification_configuration
             (fun f ->
                ("notification_configuration",
                  (NotificationConfiguration.to_json f)));
           Util.option_map v.pending_modified_values
             (fun f ->
                ("pending_modified_values",
                  (PendingModifiedValues.to_json f)));
           Util.option_map v.preferred_maintenance_window
             (fun f -> ("preferred_maintenance_window", (String.to_json f)));
           Util.option_map v.cache_cluster_create_time
             (fun f -> ("cache_cluster_create_time", (DateTime.to_json f)));
           Util.option_map v.preferred_outpost_arn
             (fun f -> ("preferred_outpost_arn", (String.to_json f)));
           Util.option_map v.preferred_availability_zone
             (fun f -> ("preferred_availability_zone", (String.to_json f)));
           Util.option_map v.num_cache_nodes
             (fun f -> ("num_cache_nodes", (Integer.to_json f)));
           Util.option_map v.cache_cluster_status
             (fun f -> ("cache_cluster_status", (String.to_json f)));
           Util.option_map v.engine_version
             (fun f -> ("engine_version", (String.to_json f)));
           Util.option_map v.engine (fun f -> ("engine", (String.to_json f)));
           Util.option_map v.cache_node_type
             (fun f -> ("cache_node_type", (String.to_json f)));
           Util.option_map v.client_download_landing_page
             (fun f -> ("client_download_landing_page", (String.to_json f)));
           Util.option_map v.configuration_endpoint
             (fun f -> ("configuration_endpoint", (Endpoint.to_json f)));
           Util.option_map v.cache_cluster_id
             (fun f -> ("cache_cluster_id", (String.to_json f)))])
    let of_json j =
      {
        cache_cluster_id =
          (Util.option_map (Json.lookup j "cache_cluster_id") String.of_json);
        configuration_endpoint =
          (Util.option_map (Json.lookup j "configuration_endpoint")
             Endpoint.of_json);
        client_download_landing_page =
          (Util.option_map (Json.lookup j "client_download_landing_page")
             String.of_json);
        cache_node_type =
          (Util.option_map (Json.lookup j "cache_node_type") String.of_json);
        engine = (Util.option_map (Json.lookup j "engine") String.of_json);
        engine_version =
          (Util.option_map (Json.lookup j "engine_version") String.of_json);
        cache_cluster_status =
          (Util.option_map (Json.lookup j "cache_cluster_status")
             String.of_json);
        num_cache_nodes =
          (Util.option_map (Json.lookup j "num_cache_nodes") Integer.of_json);
        preferred_availability_zone =
          (Util.option_map (Json.lookup j "preferred_availability_zone")
             String.of_json);
        preferred_outpost_arn =
          (Util.option_map (Json.lookup j "preferred_outpost_arn")
             String.of_json);
        cache_cluster_create_time =
          (Util.option_map (Json.lookup j "cache_cluster_create_time")
             DateTime.of_json);
        preferred_maintenance_window =
          (Util.option_map (Json.lookup j "preferred_maintenance_window")
             String.of_json);
        pending_modified_values =
          (Util.option_map (Json.lookup j "pending_modified_values")
             PendingModifiedValues.of_json);
        notification_configuration =
          (Util.option_map (Json.lookup j "notification_configuration")
             NotificationConfiguration.of_json);
        cache_security_groups =
          (CacheSecurityGroupMembershipList.of_json
             (Util.of_option_exn (Json.lookup j "cache_security_groups")));
        cache_parameter_group =
          (Util.option_map (Json.lookup j "cache_parameter_group")
             CacheParameterGroupStatus.of_json);
        cache_subnet_group_name =
          (Util.option_map (Json.lookup j "cache_subnet_group_name")
             String.of_json);
        cache_nodes =
          (CacheNodeList.of_json
             (Util.of_option_exn (Json.lookup j "cache_nodes")));
        auto_minor_version_upgrade =
          (Util.option_map (Json.lookup j "auto_minor_version_upgrade")
             Boolean.of_json);
        security_groups =
          (SecurityGroupMembershipList.of_json
             (Util.of_option_exn (Json.lookup j "security_groups")));
        replication_group_id =
          (Util.option_map (Json.lookup j "replication_group_id")
             String.of_json);
        snapshot_retention_limit =
          (Util.option_map (Json.lookup j "snapshot_retention_limit")
             Integer.of_json);
        snapshot_window =
          (Util.option_map (Json.lookup j "snapshot_window") String.of_json);
        auth_token_enabled =
          (Util.option_map (Json.lookup j "auth_token_enabled")
             Boolean.of_json);
        auth_token_last_modified_date =
          (Util.option_map (Json.lookup j "auth_token_last_modified_date")
             DateTime.of_json);
        transit_encryption_enabled =
          (Util.option_map (Json.lookup j "transit_encryption_enabled")
             Boolean.of_json);
        at_rest_encryption_enabled =
          (Util.option_map (Json.lookup j "at_rest_encryption_enabled")
             Boolean.of_json);
        a_r_n = (Util.option_map (Json.lookup j "a_r_n") String.of_json)
      }
  end
module UserGroup =
  struct
    type t =
      {
      user_group_id: String.t option ;
      status: String.t option ;
      engine: String.t option ;
      user_ids: UserIdList.t ;
      pending_changes: UserGroupPendingChanges.t option ;
      replication_groups: UGReplicationGroupIdList.t ;
      a_r_n: String.t option }
    let make ?user_group_id  ?status  ?engine  ?(user_ids= []) 
      ?pending_changes  ?(replication_groups= [])  ?a_r_n  () =
      {
        user_group_id;
        status;
        engine;
        user_ids;
        pending_changes;
        replication_groups;
        a_r_n
      }
    let parse xml =
      Some
        {
          user_group_id =
            (Util.option_bind (Xml.member "UserGroupId" xml) String.parse);
          status = (Util.option_bind (Xml.member "Status" xml) String.parse);
          engine = (Util.option_bind (Xml.member "Engine" xml) String.parse);
          user_ids =
            (Util.of_option []
               (Util.option_bind (Xml.member "UserIds" xml) UserIdList.parse));
          pending_changes =
            (Util.option_bind (Xml.member "PendingChanges" xml)
               UserGroupPendingChanges.parse);
          replication_groups =
            (Util.of_option []
               (Util.option_bind (Xml.member "ReplicationGroups" xml)
                  UGReplicationGroupIdList.parse));
          a_r_n = (Util.option_bind (Xml.member "ARN" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.a_r_n
              (fun f -> Query.Pair ("ARN", (String.to_query f)));
           Some
             (Query.Pair
                ("ReplicationGroups.member",
                  (UGReplicationGroupIdList.to_query v.replication_groups)));
           Util.option_map v.pending_changes
             (fun f ->
                Query.Pair
                  ("PendingChanges", (UserGroupPendingChanges.to_query f)));
           Some
             (Query.Pair ("UserIds.member", (UserIdList.to_query v.user_ids)));
           Util.option_map v.engine
             (fun f -> Query.Pair ("Engine", (String.to_query f)));
           Util.option_map v.status
             (fun f -> Query.Pair ("Status", (String.to_query f)));
           Util.option_map v.user_group_id
             (fun f -> Query.Pair ("UserGroupId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.a_r_n (fun f -> ("a_r_n", (String.to_json f)));
           Some
             ("replication_groups",
               (UGReplicationGroupIdList.to_json v.replication_groups));
           Util.option_map v.pending_changes
             (fun f ->
                ("pending_changes", (UserGroupPendingChanges.to_json f)));
           Some ("user_ids", (UserIdList.to_json v.user_ids));
           Util.option_map v.engine (fun f -> ("engine", (String.to_json f)));
           Util.option_map v.status (fun f -> ("status", (String.to_json f)));
           Util.option_map v.user_group_id
             (fun f -> ("user_group_id", (String.to_json f)))])
    let of_json j =
      {
        user_group_id =
          (Util.option_map (Json.lookup j "user_group_id") String.of_json);
        status = (Util.option_map (Json.lookup j "status") String.of_json);
        engine = (Util.option_map (Json.lookup j "engine") String.of_json);
        user_ids =
          (UserIdList.of_json (Util.of_option_exn (Json.lookup j "user_ids")));
        pending_changes =
          (Util.option_map (Json.lookup j "pending_changes")
             UserGroupPendingChanges.of_json);
        replication_groups =
          (UGReplicationGroupIdList.of_json
             (Util.of_option_exn (Json.lookup j "replication_groups")));
        a_r_n = (Util.option_map (Json.lookup j "a_r_n") String.of_json)
      }
  end
module UpdateAction =
  struct
    type t =
      {
      replication_group_id: String.t option ;
      cache_cluster_id: String.t option ;
      service_update_name: String.t option ;
      service_update_release_date: DateTime.t option ;
      service_update_severity: ServiceUpdateSeverity.t option ;
      service_update_status: ServiceUpdateStatus.t option ;
      service_update_recommended_apply_by_date: DateTime.t option ;
      service_update_type: ServiceUpdateType.t option ;
      update_action_available_date: DateTime.t option ;
      update_action_status: UpdateActionStatus.t option ;
      nodes_updated: String.t option ;
      update_action_status_modified_date: DateTime.t option ;
      sla_met: SlaMet.t option ;
      node_group_update_status: NodeGroupUpdateStatusList.t ;
      cache_node_update_status: CacheNodeUpdateStatusList.t ;
      estimated_update_time: String.t option ;
      engine: String.t option }
    let make ?replication_group_id  ?cache_cluster_id  ?service_update_name 
      ?service_update_release_date  ?service_update_severity 
      ?service_update_status  ?service_update_recommended_apply_by_date 
      ?service_update_type  ?update_action_available_date 
      ?update_action_status  ?nodes_updated 
      ?update_action_status_modified_date  ?sla_met 
      ?(node_group_update_status= [])  ?(cache_node_update_status= []) 
      ?estimated_update_time  ?engine  () =
      {
        replication_group_id;
        cache_cluster_id;
        service_update_name;
        service_update_release_date;
        service_update_severity;
        service_update_status;
        service_update_recommended_apply_by_date;
        service_update_type;
        update_action_available_date;
        update_action_status;
        nodes_updated;
        update_action_status_modified_date;
        sla_met;
        node_group_update_status;
        cache_node_update_status;
        estimated_update_time;
        engine
      }
    let parse xml =
      Some
        {
          replication_group_id =
            (Util.option_bind (Xml.member "ReplicationGroupId" xml)
               String.parse);
          cache_cluster_id =
            (Util.option_bind (Xml.member "CacheClusterId" xml) String.parse);
          service_update_name =
            (Util.option_bind (Xml.member "ServiceUpdateName" xml)
               String.parse);
          service_update_release_date =
            (Util.option_bind (Xml.member "ServiceUpdateReleaseDate" xml)
               DateTime.parse);
          service_update_severity =
            (Util.option_bind (Xml.member "ServiceUpdateSeverity" xml)
               ServiceUpdateSeverity.parse);
          service_update_status =
            (Util.option_bind (Xml.member "ServiceUpdateStatus" xml)
               ServiceUpdateStatus.parse);
          service_update_recommended_apply_by_date =
            (Util.option_bind
               (Xml.member "ServiceUpdateRecommendedApplyByDate" xml)
               DateTime.parse);
          service_update_type =
            (Util.option_bind (Xml.member "ServiceUpdateType" xml)
               ServiceUpdateType.parse);
          update_action_available_date =
            (Util.option_bind (Xml.member "UpdateActionAvailableDate" xml)
               DateTime.parse);
          update_action_status =
            (Util.option_bind (Xml.member "UpdateActionStatus" xml)
               UpdateActionStatus.parse);
          nodes_updated =
            (Util.option_bind (Xml.member "NodesUpdated" xml) String.parse);
          update_action_status_modified_date =
            (Util.option_bind
               (Xml.member "UpdateActionStatusModifiedDate" xml)
               DateTime.parse);
          sla_met = (Util.option_bind (Xml.member "SlaMet" xml) SlaMet.parse);
          node_group_update_status =
            (Util.of_option []
               (Util.option_bind (Xml.member "NodeGroupUpdateStatus" xml)
                  NodeGroupUpdateStatusList.parse));
          cache_node_update_status =
            (Util.of_option []
               (Util.option_bind (Xml.member "CacheNodeUpdateStatus" xml)
                  CacheNodeUpdateStatusList.parse));
          estimated_update_time =
            (Util.option_bind (Xml.member "EstimatedUpdateTime" xml)
               String.parse);
          engine = (Util.option_bind (Xml.member "Engine" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.engine
              (fun f -> Query.Pair ("Engine", (String.to_query f)));
           Util.option_map v.estimated_update_time
             (fun f ->
                Query.Pair ("EstimatedUpdateTime", (String.to_query f)));
           Some
             (Query.Pair
                ("CacheNodeUpdateStatus.member",
                  (CacheNodeUpdateStatusList.to_query
                     v.cache_node_update_status)));
           Some
             (Query.Pair
                ("NodeGroupUpdateStatus.member",
                  (NodeGroupUpdateStatusList.to_query
                     v.node_group_update_status)));
           Util.option_map v.sla_met
             (fun f -> Query.Pair ("SlaMet", (SlaMet.to_query f)));
           Util.option_map v.update_action_status_modified_date
             (fun f ->
                Query.Pair
                  ("UpdateActionStatusModifiedDate", (DateTime.to_query f)));
           Util.option_map v.nodes_updated
             (fun f -> Query.Pair ("NodesUpdated", (String.to_query f)));
           Util.option_map v.update_action_status
             (fun f ->
                Query.Pair
                  ("UpdateActionStatus", (UpdateActionStatus.to_query f)));
           Util.option_map v.update_action_available_date
             (fun f ->
                Query.Pair
                  ("UpdateActionAvailableDate", (DateTime.to_query f)));
           Util.option_map v.service_update_type
             (fun f ->
                Query.Pair
                  ("ServiceUpdateType", (ServiceUpdateType.to_query f)));
           Util.option_map v.service_update_recommended_apply_by_date
             (fun f ->
                Query.Pair
                  ("ServiceUpdateRecommendedApplyByDate",
                    (DateTime.to_query f)));
           Util.option_map v.service_update_status
             (fun f ->
                Query.Pair
                  ("ServiceUpdateStatus", (ServiceUpdateStatus.to_query f)));
           Util.option_map v.service_update_severity
             (fun f ->
                Query.Pair
                  ("ServiceUpdateSeverity",
                    (ServiceUpdateSeverity.to_query f)));
           Util.option_map v.service_update_release_date
             (fun f ->
                Query.Pair
                  ("ServiceUpdateReleaseDate", (DateTime.to_query f)));
           Util.option_map v.service_update_name
             (fun f -> Query.Pair ("ServiceUpdateName", (String.to_query f)));
           Util.option_map v.cache_cluster_id
             (fun f -> Query.Pair ("CacheClusterId", (String.to_query f)));
           Util.option_map v.replication_group_id
             (fun f -> Query.Pair ("ReplicationGroupId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.engine
              (fun f -> ("engine", (String.to_json f)));
           Util.option_map v.estimated_update_time
             (fun f -> ("estimated_update_time", (String.to_json f)));
           Some
             ("cache_node_update_status",
               (CacheNodeUpdateStatusList.to_json v.cache_node_update_status));
           Some
             ("node_group_update_status",
               (NodeGroupUpdateStatusList.to_json v.node_group_update_status));
           Util.option_map v.sla_met
             (fun f -> ("sla_met", (SlaMet.to_json f)));
           Util.option_map v.update_action_status_modified_date
             (fun f ->
                ("update_action_status_modified_date", (DateTime.to_json f)));
           Util.option_map v.nodes_updated
             (fun f -> ("nodes_updated", (String.to_json f)));
           Util.option_map v.update_action_status
             (fun f ->
                ("update_action_status", (UpdateActionStatus.to_json f)));
           Util.option_map v.update_action_available_date
             (fun f -> ("update_action_available_date", (DateTime.to_json f)));
           Util.option_map v.service_update_type
             (fun f -> ("service_update_type", (ServiceUpdateType.to_json f)));
           Util.option_map v.service_update_recommended_apply_by_date
             (fun f ->
                ("service_update_recommended_apply_by_date",
                  (DateTime.to_json f)));
           Util.option_map v.service_update_status
             (fun f ->
                ("service_update_status", (ServiceUpdateStatus.to_json f)));
           Util.option_map v.service_update_severity
             (fun f ->
                ("service_update_severity",
                  (ServiceUpdateSeverity.to_json f)));
           Util.option_map v.service_update_release_date
             (fun f -> ("service_update_release_date", (DateTime.to_json f)));
           Util.option_map v.service_update_name
             (fun f -> ("service_update_name", (String.to_json f)));
           Util.option_map v.cache_cluster_id
             (fun f -> ("cache_cluster_id", (String.to_json f)));
           Util.option_map v.replication_group_id
             (fun f -> ("replication_group_id", (String.to_json f)))])
    let of_json j =
      {
        replication_group_id =
          (Util.option_map (Json.lookup j "replication_group_id")
             String.of_json);
        cache_cluster_id =
          (Util.option_map (Json.lookup j "cache_cluster_id") String.of_json);
        service_update_name =
          (Util.option_map (Json.lookup j "service_update_name")
             String.of_json);
        service_update_release_date =
          (Util.option_map (Json.lookup j "service_update_release_date")
             DateTime.of_json);
        service_update_severity =
          (Util.option_map (Json.lookup j "service_update_severity")
             ServiceUpdateSeverity.of_json);
        service_update_status =
          (Util.option_map (Json.lookup j "service_update_status")
             ServiceUpdateStatus.of_json);
        service_update_recommended_apply_by_date =
          (Util.option_map
             (Json.lookup j "service_update_recommended_apply_by_date")
             DateTime.of_json);
        service_update_type =
          (Util.option_map (Json.lookup j "service_update_type")
             ServiceUpdateType.of_json);
        update_action_available_date =
          (Util.option_map (Json.lookup j "update_action_available_date")
             DateTime.of_json);
        update_action_status =
          (Util.option_map (Json.lookup j "update_action_status")
             UpdateActionStatus.of_json);
        nodes_updated =
          (Util.option_map (Json.lookup j "nodes_updated") String.of_json);
        update_action_status_modified_date =
          (Util.option_map
             (Json.lookup j "update_action_status_modified_date")
             DateTime.of_json);
        sla_met = (Util.option_map (Json.lookup j "sla_met") SlaMet.of_json);
        node_group_update_status =
          (NodeGroupUpdateStatusList.of_json
             (Util.of_option_exn (Json.lookup j "node_group_update_status")));
        cache_node_update_status =
          (CacheNodeUpdateStatusList.of_json
             (Util.of_option_exn (Json.lookup j "cache_node_update_status")));
        estimated_update_time =
          (Util.option_map (Json.lookup j "estimated_update_time")
             String.of_json);
        engine = (Util.option_map (Json.lookup j "engine") String.of_json)
      }
  end
module Event =
  struct
    type t =
      {
      source_identifier: String.t option ;
      source_type: SourceType.t option ;
      message: String.t option ;
      date: DateTime.t option }
    let make ?source_identifier  ?source_type  ?message  ?date  () =
      { source_identifier; source_type; message; date }
    let parse xml =
      Some
        {
          source_identifier =
            (Util.option_bind (Xml.member "SourceIdentifier" xml)
               String.parse);
          source_type =
            (Util.option_bind (Xml.member "SourceType" xml) SourceType.parse);
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse);
          date = (Util.option_bind (Xml.member "Date" xml) DateTime.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.date
              (fun f -> Query.Pair ("Date", (DateTime.to_query f)));
           Util.option_map v.message
             (fun f -> Query.Pair ("Message", (String.to_query f)));
           Util.option_map v.source_type
             (fun f -> Query.Pair ("SourceType", (SourceType.to_query f)));
           Util.option_map v.source_identifier
             (fun f -> Query.Pair ("SourceIdentifier", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.date (fun f -> ("date", (DateTime.to_json f)));
           Util.option_map v.message
             (fun f -> ("message", (String.to_json f)));
           Util.option_map v.source_type
             (fun f -> ("source_type", (SourceType.to_json f)));
           Util.option_map v.source_identifier
             (fun f -> ("source_identifier", (String.to_json f)))])
    let of_json j =
      {
        source_identifier =
          (Util.option_map (Json.lookup j "source_identifier") String.of_json);
        source_type =
          (Util.option_map (Json.lookup j "source_type") SourceType.of_json);
        message = (Util.option_map (Json.lookup j "message") String.of_json);
        date = (Util.option_map (Json.lookup j "date") DateTime.of_json)
      }
  end
module ProcessedUpdateAction =
  struct
    type t =
      {
      replication_group_id: String.t option ;
      cache_cluster_id: String.t option ;
      service_update_name: String.t option ;
      update_action_status: UpdateActionStatus.t option }
    let make ?replication_group_id  ?cache_cluster_id  ?service_update_name 
      ?update_action_status  () =
      {
        replication_group_id;
        cache_cluster_id;
        service_update_name;
        update_action_status
      }
    let parse xml =
      Some
        {
          replication_group_id =
            (Util.option_bind (Xml.member "ReplicationGroupId" xml)
               String.parse);
          cache_cluster_id =
            (Util.option_bind (Xml.member "CacheClusterId" xml) String.parse);
          service_update_name =
            (Util.option_bind (Xml.member "ServiceUpdateName" xml)
               String.parse);
          update_action_status =
            (Util.option_bind (Xml.member "UpdateActionStatus" xml)
               UpdateActionStatus.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.update_action_status
              (fun f ->
                 Query.Pair
                   ("UpdateActionStatus", (UpdateActionStatus.to_query f)));
           Util.option_map v.service_update_name
             (fun f -> Query.Pair ("ServiceUpdateName", (String.to_query f)));
           Util.option_map v.cache_cluster_id
             (fun f -> Query.Pair ("CacheClusterId", (String.to_query f)));
           Util.option_map v.replication_group_id
             (fun f -> Query.Pair ("ReplicationGroupId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.update_action_status
              (fun f ->
                 ("update_action_status", (UpdateActionStatus.to_json f)));
           Util.option_map v.service_update_name
             (fun f -> ("service_update_name", (String.to_json f)));
           Util.option_map v.cache_cluster_id
             (fun f -> ("cache_cluster_id", (String.to_json f)));
           Util.option_map v.replication_group_id
             (fun f -> ("replication_group_id", (String.to_json f)))])
    let of_json j =
      {
        replication_group_id =
          (Util.option_map (Json.lookup j "replication_group_id")
             String.of_json);
        cache_cluster_id =
          (Util.option_map (Json.lookup j "cache_cluster_id") String.of_json);
        service_update_name =
          (Util.option_map (Json.lookup j "service_update_name")
             String.of_json);
        update_action_status =
          (Util.option_map (Json.lookup j "update_action_status")
             UpdateActionStatus.of_json)
      }
  end
module UnprocessedUpdateAction =
  struct
    type t =
      {
      replication_group_id: String.t option ;
      cache_cluster_id: String.t option ;
      service_update_name: String.t option ;
      error_type: String.t option ;
      error_message: String.t option }
    let make ?replication_group_id  ?cache_cluster_id  ?service_update_name 
      ?error_type  ?error_message  () =
      {
        replication_group_id;
        cache_cluster_id;
        service_update_name;
        error_type;
        error_message
      }
    let parse xml =
      Some
        {
          replication_group_id =
            (Util.option_bind (Xml.member "ReplicationGroupId" xml)
               String.parse);
          cache_cluster_id =
            (Util.option_bind (Xml.member "CacheClusterId" xml) String.parse);
          service_update_name =
            (Util.option_bind (Xml.member "ServiceUpdateName" xml)
               String.parse);
          error_type =
            (Util.option_bind (Xml.member "ErrorType" xml) String.parse);
          error_message =
            (Util.option_bind (Xml.member "ErrorMessage" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.error_message
              (fun f -> Query.Pair ("ErrorMessage", (String.to_query f)));
           Util.option_map v.error_type
             (fun f -> Query.Pair ("ErrorType", (String.to_query f)));
           Util.option_map v.service_update_name
             (fun f -> Query.Pair ("ServiceUpdateName", (String.to_query f)));
           Util.option_map v.cache_cluster_id
             (fun f -> Query.Pair ("CacheClusterId", (String.to_query f)));
           Util.option_map v.replication_group_id
             (fun f -> Query.Pair ("ReplicationGroupId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.error_message
              (fun f -> ("error_message", (String.to_json f)));
           Util.option_map v.error_type
             (fun f -> ("error_type", (String.to_json f)));
           Util.option_map v.service_update_name
             (fun f -> ("service_update_name", (String.to_json f)));
           Util.option_map v.cache_cluster_id
             (fun f -> ("cache_cluster_id", (String.to_json f)));
           Util.option_map v.replication_group_id
             (fun f -> ("replication_group_id", (String.to_json f)))])
    let of_json j =
      {
        replication_group_id =
          (Util.option_map (Json.lookup j "replication_group_id")
             String.of_json);
        cache_cluster_id =
          (Util.option_map (Json.lookup j "cache_cluster_id") String.of_json);
        service_update_name =
          (Util.option_map (Json.lookup j "service_update_name")
             String.of_json);
        error_type =
          (Util.option_map (Json.lookup j "error_type") String.of_json);
        error_message =
          (Util.option_map (Json.lookup j "error_message") String.of_json)
      }
  end
module ReplicationGroup =
  struct
    type t =
      {
      replication_group_id: String.t option ;
      description: String.t option ;
      global_replication_group_info: GlobalReplicationGroupInfo.t option ;
      status: String.t option ;
      pending_modified_values: ReplicationGroupPendingModifiedValues.t option ;
      member_clusters: ClusterIdList.t ;
      node_groups: NodeGroupList.t ;
      snapshotting_cluster_id: String.t option ;
      automatic_failover: AutomaticFailoverStatus.t option ;
      multi_a_z: MultiAZStatus.t option ;
      configuration_endpoint: Endpoint.t option ;
      snapshot_retention_limit: Integer.t option ;
      snapshot_window: String.t option ;
      cluster_enabled: Boolean.t option ;
      cache_node_type: String.t option ;
      auth_token_enabled: Boolean.t option ;
      auth_token_last_modified_date: DateTime.t option ;
      transit_encryption_enabled: Boolean.t option ;
      at_rest_encryption_enabled: Boolean.t option ;
      member_clusters_outpost_arns: ReplicationGroupOutpostArnList.t ;
      kms_key_id: String.t option ;
      a_r_n: String.t option ;
      user_group_ids: UserGroupIdList.t }
    let make ?replication_group_id  ?description 
      ?global_replication_group_info  ?status  ?pending_modified_values 
      ?(member_clusters= [])  ?(node_groups= [])  ?snapshotting_cluster_id 
      ?automatic_failover  ?multi_a_z  ?configuration_endpoint 
      ?snapshot_retention_limit  ?snapshot_window  ?cluster_enabled 
      ?cache_node_type  ?auth_token_enabled  ?auth_token_last_modified_date 
      ?transit_encryption_enabled  ?at_rest_encryption_enabled 
      ?(member_clusters_outpost_arns= [])  ?kms_key_id  ?a_r_n 
      ?(user_group_ids= [])  () =
      {
        replication_group_id;
        description;
        global_replication_group_info;
        status;
        pending_modified_values;
        member_clusters;
        node_groups;
        snapshotting_cluster_id;
        automatic_failover;
        multi_a_z;
        configuration_endpoint;
        snapshot_retention_limit;
        snapshot_window;
        cluster_enabled;
        cache_node_type;
        auth_token_enabled;
        auth_token_last_modified_date;
        transit_encryption_enabled;
        at_rest_encryption_enabled;
        member_clusters_outpost_arns;
        kms_key_id;
        a_r_n;
        user_group_ids
      }
    let parse xml =
      Some
        {
          replication_group_id =
            (Util.option_bind (Xml.member "ReplicationGroupId" xml)
               String.parse);
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse);
          global_replication_group_info =
            (Util.option_bind (Xml.member "GlobalReplicationGroupInfo" xml)
               GlobalReplicationGroupInfo.parse);
          status = (Util.option_bind (Xml.member "Status" xml) String.parse);
          pending_modified_values =
            (Util.option_bind (Xml.member "PendingModifiedValues" xml)
               ReplicationGroupPendingModifiedValues.parse);
          member_clusters =
            (Util.of_option []
               (Util.option_bind (Xml.member "MemberClusters" xml)
                  ClusterIdList.parse));
          node_groups =
            (Util.of_option []
               (Util.option_bind (Xml.member "NodeGroups" xml)
                  NodeGroupList.parse));
          snapshotting_cluster_id =
            (Util.option_bind (Xml.member "SnapshottingClusterId" xml)
               String.parse);
          automatic_failover =
            (Util.option_bind (Xml.member "AutomaticFailover" xml)
               AutomaticFailoverStatus.parse);
          multi_a_z =
            (Util.option_bind (Xml.member "MultiAZ" xml) MultiAZStatus.parse);
          configuration_endpoint =
            (Util.option_bind (Xml.member "ConfigurationEndpoint" xml)
               Endpoint.parse);
          snapshot_retention_limit =
            (Util.option_bind (Xml.member "SnapshotRetentionLimit" xml)
               Integer.parse);
          snapshot_window =
            (Util.option_bind (Xml.member "SnapshotWindow" xml) String.parse);
          cluster_enabled =
            (Util.option_bind (Xml.member "ClusterEnabled" xml) Boolean.parse);
          cache_node_type =
            (Util.option_bind (Xml.member "CacheNodeType" xml) String.parse);
          auth_token_enabled =
            (Util.option_bind (Xml.member "AuthTokenEnabled" xml)
               Boolean.parse);
          auth_token_last_modified_date =
            (Util.option_bind (Xml.member "AuthTokenLastModifiedDate" xml)
               DateTime.parse);
          transit_encryption_enabled =
            (Util.option_bind (Xml.member "TransitEncryptionEnabled" xml)
               Boolean.parse);
          at_rest_encryption_enabled =
            (Util.option_bind (Xml.member "AtRestEncryptionEnabled" xml)
               Boolean.parse);
          member_clusters_outpost_arns =
            (Util.of_option []
               (Util.option_bind (Xml.member "MemberClustersOutpostArns" xml)
                  ReplicationGroupOutpostArnList.parse));
          kms_key_id =
            (Util.option_bind (Xml.member "KmsKeyId" xml) String.parse);
          a_r_n = (Util.option_bind (Xml.member "ARN" xml) String.parse);
          user_group_ids =
            (Util.of_option []
               (Util.option_bind (Xml.member "UserGroupIds" xml)
                  UserGroupIdList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("UserGroupIds.member",
                   (UserGroupIdList.to_query v.user_group_ids)));
           Util.option_map v.a_r_n
             (fun f -> Query.Pair ("ARN", (String.to_query f)));
           Util.option_map v.kms_key_id
             (fun f -> Query.Pair ("KmsKeyId", (String.to_query f)));
           Some
             (Query.Pair
                ("MemberClustersOutpostArns.member",
                  (ReplicationGroupOutpostArnList.to_query
                     v.member_clusters_outpost_arns)));
           Util.option_map v.at_rest_encryption_enabled
             (fun f ->
                Query.Pair ("AtRestEncryptionEnabled", (Boolean.to_query f)));
           Util.option_map v.transit_encryption_enabled
             (fun f ->
                Query.Pair ("TransitEncryptionEnabled", (Boolean.to_query f)));
           Util.option_map v.auth_token_last_modified_date
             (fun f ->
                Query.Pair
                  ("AuthTokenLastModifiedDate", (DateTime.to_query f)));
           Util.option_map v.auth_token_enabled
             (fun f -> Query.Pair ("AuthTokenEnabled", (Boolean.to_query f)));
           Util.option_map v.cache_node_type
             (fun f -> Query.Pair ("CacheNodeType", (String.to_query f)));
           Util.option_map v.cluster_enabled
             (fun f -> Query.Pair ("ClusterEnabled", (Boolean.to_query f)));
           Util.option_map v.snapshot_window
             (fun f -> Query.Pair ("SnapshotWindow", (String.to_query f)));
           Util.option_map v.snapshot_retention_limit
             (fun f ->
                Query.Pair ("SnapshotRetentionLimit", (Integer.to_query f)));
           Util.option_map v.configuration_endpoint
             (fun f ->
                Query.Pair ("ConfigurationEndpoint", (Endpoint.to_query f)));
           Util.option_map v.multi_a_z
             (fun f -> Query.Pair ("MultiAZ", (MultiAZStatus.to_query f)));
           Util.option_map v.automatic_failover
             (fun f ->
                Query.Pair
                  ("AutomaticFailover", (AutomaticFailoverStatus.to_query f)));
           Util.option_map v.snapshotting_cluster_id
             (fun f ->
                Query.Pair ("SnapshottingClusterId", (String.to_query f)));
           Some
             (Query.Pair
                ("NodeGroups.member", (NodeGroupList.to_query v.node_groups)));
           Some
             (Query.Pair
                ("MemberClusters.member",
                  (ClusterIdList.to_query v.member_clusters)));
           Util.option_map v.pending_modified_values
             (fun f ->
                Query.Pair
                  ("PendingModifiedValues",
                    (ReplicationGroupPendingModifiedValues.to_query f)));
           Util.option_map v.status
             (fun f -> Query.Pair ("Status", (String.to_query f)));
           Util.option_map v.global_replication_group_info
             (fun f ->
                Query.Pair
                  ("GlobalReplicationGroupInfo",
                    (GlobalReplicationGroupInfo.to_query f)));
           Util.option_map v.description
             (fun f -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.replication_group_id
             (fun f -> Query.Pair ("ReplicationGroupId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("user_group_ids", (UserGroupIdList.to_json v.user_group_ids));
           Util.option_map v.a_r_n (fun f -> ("a_r_n", (String.to_json f)));
           Util.option_map v.kms_key_id
             (fun f -> ("kms_key_id", (String.to_json f)));
           Some
             ("member_clusters_outpost_arns",
               (ReplicationGroupOutpostArnList.to_json
                  v.member_clusters_outpost_arns));
           Util.option_map v.at_rest_encryption_enabled
             (fun f -> ("at_rest_encryption_enabled", (Boolean.to_json f)));
           Util.option_map v.transit_encryption_enabled
             (fun f -> ("transit_encryption_enabled", (Boolean.to_json f)));
           Util.option_map v.auth_token_last_modified_date
             (fun f ->
                ("auth_token_last_modified_date", (DateTime.to_json f)));
           Util.option_map v.auth_token_enabled
             (fun f -> ("auth_token_enabled", (Boolean.to_json f)));
           Util.option_map v.cache_node_type
             (fun f -> ("cache_node_type", (String.to_json f)));
           Util.option_map v.cluster_enabled
             (fun f -> ("cluster_enabled", (Boolean.to_json f)));
           Util.option_map v.snapshot_window
             (fun f -> ("snapshot_window", (String.to_json f)));
           Util.option_map v.snapshot_retention_limit
             (fun f -> ("snapshot_retention_limit", (Integer.to_json f)));
           Util.option_map v.configuration_endpoint
             (fun f -> ("configuration_endpoint", (Endpoint.to_json f)));
           Util.option_map v.multi_a_z
             (fun f -> ("multi_a_z", (MultiAZStatus.to_json f)));
           Util.option_map v.automatic_failover
             (fun f ->
                ("automatic_failover", (AutomaticFailoverStatus.to_json f)));
           Util.option_map v.snapshotting_cluster_id
             (fun f -> ("snapshotting_cluster_id", (String.to_json f)));
           Some ("node_groups", (NodeGroupList.to_json v.node_groups));
           Some
             ("member_clusters", (ClusterIdList.to_json v.member_clusters));
           Util.option_map v.pending_modified_values
             (fun f ->
                ("pending_modified_values",
                  (ReplicationGroupPendingModifiedValues.to_json f)));
           Util.option_map v.status (fun f -> ("status", (String.to_json f)));
           Util.option_map v.global_replication_group_info
             (fun f ->
                ("global_replication_group_info",
                  (GlobalReplicationGroupInfo.to_json f)));
           Util.option_map v.description
             (fun f -> ("description", (String.to_json f)));
           Util.option_map v.replication_group_id
             (fun f -> ("replication_group_id", (String.to_json f)))])
    let of_json j =
      {
        replication_group_id =
          (Util.option_map (Json.lookup j "replication_group_id")
             String.of_json);
        description =
          (Util.option_map (Json.lookup j "description") String.of_json);
        global_replication_group_info =
          (Util.option_map (Json.lookup j "global_replication_group_info")
             GlobalReplicationGroupInfo.of_json);
        status = (Util.option_map (Json.lookup j "status") String.of_json);
        pending_modified_values =
          (Util.option_map (Json.lookup j "pending_modified_values")
             ReplicationGroupPendingModifiedValues.of_json);
        member_clusters =
          (ClusterIdList.of_json
             (Util.of_option_exn (Json.lookup j "member_clusters")));
        node_groups =
          (NodeGroupList.of_json
             (Util.of_option_exn (Json.lookup j "node_groups")));
        snapshotting_cluster_id =
          (Util.option_map (Json.lookup j "snapshotting_cluster_id")
             String.of_json);
        automatic_failover =
          (Util.option_map (Json.lookup j "automatic_failover")
             AutomaticFailoverStatus.of_json);
        multi_a_z =
          (Util.option_map (Json.lookup j "multi_a_z") MultiAZStatus.of_json);
        configuration_endpoint =
          (Util.option_map (Json.lookup j "configuration_endpoint")
             Endpoint.of_json);
        snapshot_retention_limit =
          (Util.option_map (Json.lookup j "snapshot_retention_limit")
             Integer.of_json);
        snapshot_window =
          (Util.option_map (Json.lookup j "snapshot_window") String.of_json);
        cluster_enabled =
          (Util.option_map (Json.lookup j "cluster_enabled") Boolean.of_json);
        cache_node_type =
          (Util.option_map (Json.lookup j "cache_node_type") String.of_json);
        auth_token_enabled =
          (Util.option_map (Json.lookup j "auth_token_enabled")
             Boolean.of_json);
        auth_token_last_modified_date =
          (Util.option_map (Json.lookup j "auth_token_last_modified_date")
             DateTime.of_json);
        transit_encryption_enabled =
          (Util.option_map (Json.lookup j "transit_encryption_enabled")
             Boolean.of_json);
        at_rest_encryption_enabled =
          (Util.option_map (Json.lookup j "at_rest_encryption_enabled")
             Boolean.of_json);
        member_clusters_outpost_arns =
          (ReplicationGroupOutpostArnList.of_json
             (Util.of_option_exn
                (Json.lookup j "member_clusters_outpost_arns")));
        kms_key_id =
          (Util.option_map (Json.lookup j "kms_key_id") String.of_json);
        a_r_n = (Util.option_map (Json.lookup j "a_r_n") String.of_json);
        user_group_ids =
          (UserGroupIdList.of_json
             (Util.of_option_exn (Json.lookup j "user_group_ids")))
      }
  end
module CacheEngineVersion =
  struct
    type t =
      {
      engine: String.t option ;
      engine_version: String.t option ;
      cache_parameter_group_family: String.t option ;
      cache_engine_description: String.t option ;
      cache_engine_version_description: String.t option }
    let make ?engine  ?engine_version  ?cache_parameter_group_family 
      ?cache_engine_description  ?cache_engine_version_description  () =
      {
        engine;
        engine_version;
        cache_parameter_group_family;
        cache_engine_description;
        cache_engine_version_description
      }
    let parse xml =
      Some
        {
          engine = (Util.option_bind (Xml.member "Engine" xml) String.parse);
          engine_version =
            (Util.option_bind (Xml.member "EngineVersion" xml) String.parse);
          cache_parameter_group_family =
            (Util.option_bind (Xml.member "CacheParameterGroupFamily" xml)
               String.parse);
          cache_engine_description =
            (Util.option_bind (Xml.member "CacheEngineDescription" xml)
               String.parse);
          cache_engine_version_description =
            (Util.option_bind
               (Xml.member "CacheEngineVersionDescription" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.cache_engine_version_description
              (fun f ->
                 Query.Pair
                   ("CacheEngineVersionDescription", (String.to_query f)));
           Util.option_map v.cache_engine_description
             (fun f ->
                Query.Pair ("CacheEngineDescription", (String.to_query f)));
           Util.option_map v.cache_parameter_group_family
             (fun f ->
                Query.Pair ("CacheParameterGroupFamily", (String.to_query f)));
           Util.option_map v.engine_version
             (fun f -> Query.Pair ("EngineVersion", (String.to_query f)));
           Util.option_map v.engine
             (fun f -> Query.Pair ("Engine", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cache_engine_version_description
              (fun f ->
                 ("cache_engine_version_description", (String.to_json f)));
           Util.option_map v.cache_engine_description
             (fun f -> ("cache_engine_description", (String.to_json f)));
           Util.option_map v.cache_parameter_group_family
             (fun f -> ("cache_parameter_group_family", (String.to_json f)));
           Util.option_map v.engine_version
             (fun f -> ("engine_version", (String.to_json f)));
           Util.option_map v.engine (fun f -> ("engine", (String.to_json f)))])
    let of_json j =
      {
        engine = (Util.option_map (Json.lookup j "engine") String.of_json);
        engine_version =
          (Util.option_map (Json.lookup j "engine_version") String.of_json);
        cache_parameter_group_family =
          (Util.option_map (Json.lookup j "cache_parameter_group_family")
             String.of_json);
        cache_engine_description =
          (Util.option_map (Json.lookup j "cache_engine_description")
             String.of_json);
        cache_engine_version_description =
          (Util.option_map (Json.lookup j "cache_engine_version_description")
             String.of_json)
      }
  end
module ReservedCacheNode =
  struct
    type t =
      {
      reserved_cache_node_id: String.t option ;
      reserved_cache_nodes_offering_id: String.t option ;
      cache_node_type: String.t option ;
      start_time: DateTime.t option ;
      duration: Integer.t option ;
      fixed_price: Double.t option ;
      usage_price: Double.t option ;
      cache_node_count: Integer.t option ;
      product_description: String.t option ;
      offering_type: String.t option ;
      state: String.t option ;
      recurring_charges: RecurringChargeList.t ;
      reservation_a_r_n: String.t option }
    let make ?reserved_cache_node_id  ?reserved_cache_nodes_offering_id 
      ?cache_node_type  ?start_time  ?duration  ?fixed_price  ?usage_price 
      ?cache_node_count  ?product_description  ?offering_type  ?state 
      ?(recurring_charges= [])  ?reservation_a_r_n  () =
      {
        reserved_cache_node_id;
        reserved_cache_nodes_offering_id;
        cache_node_type;
        start_time;
        duration;
        fixed_price;
        usage_price;
        cache_node_count;
        product_description;
        offering_type;
        state;
        recurring_charges;
        reservation_a_r_n
      }
    let parse xml =
      Some
        {
          reserved_cache_node_id =
            (Util.option_bind (Xml.member "ReservedCacheNodeId" xml)
               String.parse);
          reserved_cache_nodes_offering_id =
            (Util.option_bind (Xml.member "ReservedCacheNodesOfferingId" xml)
               String.parse);
          cache_node_type =
            (Util.option_bind (Xml.member "CacheNodeType" xml) String.parse);
          start_time =
            (Util.option_bind (Xml.member "StartTime" xml) DateTime.parse);
          duration =
            (Util.option_bind (Xml.member "Duration" xml) Integer.parse);
          fixed_price =
            (Util.option_bind (Xml.member "FixedPrice" xml) Double.parse);
          usage_price =
            (Util.option_bind (Xml.member "UsagePrice" xml) Double.parse);
          cache_node_count =
            (Util.option_bind (Xml.member "CacheNodeCount" xml) Integer.parse);
          product_description =
            (Util.option_bind (Xml.member "ProductDescription" xml)
               String.parse);
          offering_type =
            (Util.option_bind (Xml.member "OfferingType" xml) String.parse);
          state = (Util.option_bind (Xml.member "State" xml) String.parse);
          recurring_charges =
            (Util.of_option []
               (Util.option_bind (Xml.member "RecurringCharges" xml)
                  RecurringChargeList.parse));
          reservation_a_r_n =
            (Util.option_bind (Xml.member "ReservationARN" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.reservation_a_r_n
              (fun f -> Query.Pair ("ReservationARN", (String.to_query f)));
           Some
             (Query.Pair
                ("RecurringCharges.member",
                  (RecurringChargeList.to_query v.recurring_charges)));
           Util.option_map v.state
             (fun f -> Query.Pair ("State", (String.to_query f)));
           Util.option_map v.offering_type
             (fun f -> Query.Pair ("OfferingType", (String.to_query f)));
           Util.option_map v.product_description
             (fun f -> Query.Pair ("ProductDescription", (String.to_query f)));
           Util.option_map v.cache_node_count
             (fun f -> Query.Pair ("CacheNodeCount", (Integer.to_query f)));
           Util.option_map v.usage_price
             (fun f -> Query.Pair ("UsagePrice", (Double.to_query f)));
           Util.option_map v.fixed_price
             (fun f -> Query.Pair ("FixedPrice", (Double.to_query f)));
           Util.option_map v.duration
             (fun f -> Query.Pair ("Duration", (Integer.to_query f)));
           Util.option_map v.start_time
             (fun f -> Query.Pair ("StartTime", (DateTime.to_query f)));
           Util.option_map v.cache_node_type
             (fun f -> Query.Pair ("CacheNodeType", (String.to_query f)));
           Util.option_map v.reserved_cache_nodes_offering_id
             (fun f ->
                Query.Pair
                  ("ReservedCacheNodesOfferingId", (String.to_query f)));
           Util.option_map v.reserved_cache_node_id
             (fun f ->
                Query.Pair ("ReservedCacheNodeId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.reservation_a_r_n
              (fun f -> ("reservation_a_r_n", (String.to_json f)));
           Some
             ("recurring_charges",
               (RecurringChargeList.to_json v.recurring_charges));
           Util.option_map v.state (fun f -> ("state", (String.to_json f)));
           Util.option_map v.offering_type
             (fun f -> ("offering_type", (String.to_json f)));
           Util.option_map v.product_description
             (fun f -> ("product_description", (String.to_json f)));
           Util.option_map v.cache_node_count
             (fun f -> ("cache_node_count", (Integer.to_json f)));
           Util.option_map v.usage_price
             (fun f -> ("usage_price", (Double.to_json f)));
           Util.option_map v.fixed_price
             (fun f -> ("fixed_price", (Double.to_json f)));
           Util.option_map v.duration
             (fun f -> ("duration", (Integer.to_json f)));
           Util.option_map v.start_time
             (fun f -> ("start_time", (DateTime.to_json f)));
           Util.option_map v.cache_node_type
             (fun f -> ("cache_node_type", (String.to_json f)));
           Util.option_map v.reserved_cache_nodes_offering_id
             (fun f ->
                ("reserved_cache_nodes_offering_id", (String.to_json f)));
           Util.option_map v.reserved_cache_node_id
             (fun f -> ("reserved_cache_node_id", (String.to_json f)))])
    let of_json j =
      {
        reserved_cache_node_id =
          (Util.option_map (Json.lookup j "reserved_cache_node_id")
             String.of_json);
        reserved_cache_nodes_offering_id =
          (Util.option_map (Json.lookup j "reserved_cache_nodes_offering_id")
             String.of_json);
        cache_node_type =
          (Util.option_map (Json.lookup j "cache_node_type") String.of_json);
        start_time =
          (Util.option_map (Json.lookup j "start_time") DateTime.of_json);
        duration =
          (Util.option_map (Json.lookup j "duration") Integer.of_json);
        fixed_price =
          (Util.option_map (Json.lookup j "fixed_price") Double.of_json);
        usage_price =
          (Util.option_map (Json.lookup j "usage_price") Double.of_json);
        cache_node_count =
          (Util.option_map (Json.lookup j "cache_node_count") Integer.of_json);
        product_description =
          (Util.option_map (Json.lookup j "product_description")
             String.of_json);
        offering_type =
          (Util.option_map (Json.lookup j "offering_type") String.of_json);
        state = (Util.option_map (Json.lookup j "state") String.of_json);
        recurring_charges =
          (RecurringChargeList.of_json
             (Util.of_option_exn (Json.lookup j "recurring_charges")));
        reservation_a_r_n =
          (Util.option_map (Json.lookup j "reservation_a_r_n") String.of_json)
      }
  end
module CacheParameterGroupList =
  struct
    type t = CacheParameterGroup.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map CacheParameterGroup.parse
           (Xml.members "CacheParameterGroup" xml))
    let to_query v = Query.to_query_list CacheParameterGroup.to_query v
    let to_json v = `List (List.map CacheParameterGroup.to_json v)
    let of_json j = Json.to_list CacheParameterGroup.of_json j
  end
module PasswordListInput =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module CacheSubnetGroups =
  struct
    type t = CacheSubnetGroup.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map CacheSubnetGroup.parse (Xml.members "CacheSubnetGroup" xml))
    let to_query v = Query.to_query_list CacheSubnetGroup.to_query v
    let to_json v = `List (List.map CacheSubnetGroup.to_json v)
    let of_json j = Json.to_list CacheSubnetGroup.of_json j
  end
module TagList =
  struct
    type t = Tag.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Tag.parse (Xml.members "Tag" xml))
    let to_query v = Query.to_query_list Tag.to_query v
    let to_json v = `List (List.map Tag.to_json v)
    let of_json j = Json.to_list Tag.of_json j
  end
module UserIdListInput =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module SubnetIdentifierList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map String.parse (Xml.members "SubnetIdentifier" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module GlobalNodeGroupIdList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map String.parse (Xml.members "GlobalNodeGroupId" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module CacheSecurityGroupNameList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map String.parse (Xml.members "CacheSecurityGroupName" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module NodeGroupConfigurationList =
  struct
    type t = NodeGroupConfiguration.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map NodeGroupConfiguration.parse
           (Xml.members "NodeGroupConfiguration" xml))
    let to_query v = Query.to_query_list NodeGroupConfiguration.to_query v
    let to_json v = `List (List.map NodeGroupConfiguration.to_json v)
    let of_json j = Json.to_list NodeGroupConfiguration.of_json j
  end
module SecurityGroupIdsList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map String.parse (Xml.members "SecurityGroupId" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module SnapshotArnsList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "SnapshotArn" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module UserGroupIdListInput =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module EngineDefaults =
  struct
    type t =
      {
      cache_parameter_group_family: String.t option ;
      marker: String.t option ;
      parameters: ParametersList.t ;
      cache_node_type_specific_parameters:
        CacheNodeTypeSpecificParametersList.t }
    let make ?cache_parameter_group_family  ?marker  ?(parameters= []) 
      ?(cache_node_type_specific_parameters= [])  () =
      {
        cache_parameter_group_family;
        marker;
        parameters;
        cache_node_type_specific_parameters
      }
    let parse xml =
      Some
        {
          cache_parameter_group_family =
            (Util.option_bind (Xml.member "CacheParameterGroupFamily" xml)
               String.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          parameters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Parameters" xml)
                  ParametersList.parse));
          cache_node_type_specific_parameters =
            (Util.of_option []
               (Util.option_bind
                  (Xml.member "CacheNodeTypeSpecificParameters" xml)
                  CacheNodeTypeSpecificParametersList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("CacheNodeTypeSpecificParameters.member",
                   (CacheNodeTypeSpecificParametersList.to_query
                      v.cache_node_type_specific_parameters)));
           Some
             (Query.Pair
                ("Parameters.member", (ParametersList.to_query v.parameters)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.cache_parameter_group_family
             (fun f ->
                Query.Pair ("CacheParameterGroupFamily", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("cache_node_type_specific_parameters",
                (CacheNodeTypeSpecificParametersList.to_json
                   v.cache_node_type_specific_parameters));
           Some ("parameters", (ParametersList.to_json v.parameters));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.cache_parameter_group_family
             (fun f -> ("cache_parameter_group_family", (String.to_json f)))])
    let of_json j =
      {
        cache_parameter_group_family =
          (Util.option_map (Json.lookup j "cache_parameter_group_family")
             String.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        parameters =
          (ParametersList.of_json
             (Util.of_option_exn (Json.lookup j "parameters")));
        cache_node_type_specific_parameters =
          (CacheNodeTypeSpecificParametersList.of_json
             (Util.of_option_exn
                (Json.lookup j "cache_node_type_specific_parameters")))
      }
  end
module CacheClusterIdList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module ReplicationGroupIdList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module RegionalConfigurationList =
  struct
    type t = RegionalConfiguration.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map RegionalConfiguration.parse
           (Xml.members "RegionalConfiguration" xml))
    let to_query v = Query.to_query_list RegionalConfiguration.to_query v
    let to_json v = `List (List.map RegionalConfiguration.to_json v)
    let of_json j = Json.to_list RegionalConfiguration.of_json j
  end
module NodeTypeList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module AuthTokenUpdateStrategyType =
  struct
    type t =
      | SET 
      | ROTATE 
      | DELETE 
    let str_to_t = [("DELETE", DELETE); ("ROTATE", ROTATE); ("SET", SET)]
    let t_to_str = [(DELETE, "DELETE"); (ROTATE, "ROTATE"); (SET, "SET")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module CacheSecurityGroups =
  struct
    type t = CacheSecurityGroup.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map CacheSecurityGroup.parse
           (Xml.members "CacheSecurityGroup" xml))
    let to_query v = Query.to_query_list CacheSecurityGroup.to_query v
    let to_json v = `List (List.map CacheSecurityGroup.to_json v)
    let of_json j = Json.to_list CacheSecurityGroup.of_json j
  end
module FilterList =
  struct
    type t = Filter.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Filter.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Filter.to_query v
    let to_json v = `List (List.map Filter.to_json v)
    let of_json j = Json.to_list Filter.of_json j
  end
module AZMode =
  struct
    type t =
      | Single_az 
      | Cross_az 
    let str_to_t = [("cross-az", Cross_az); ("single-az", Single_az)]
    let t_to_str = [(Cross_az, "cross-az"); (Single_az, "single-az")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module CustomerNodeEndpointList =
  struct
    type t = CustomerNodeEndpoint.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map CustomerNodeEndpoint.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list CustomerNodeEndpoint.to_query v
    let to_json v = `List (List.map CustomerNodeEndpoint.to_json v)
    let of_json j = Json.to_list CustomerNodeEndpoint.of_json j
  end
module ReservedCacheNodesOfferingList =
  struct
    type t = ReservedCacheNodesOffering.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map ReservedCacheNodesOffering.parse
           (Xml.members "ReservedCacheNodesOffering" xml))
    let to_query v =
      Query.to_query_list ReservedCacheNodesOffering.to_query v
    let to_json v = `List (List.map ReservedCacheNodesOffering.to_json v)
    let of_json j = Json.to_list ReservedCacheNodesOffering.of_json j
  end
module SnapshotList =
  struct
    type t = Snapshot.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Snapshot.parse (Xml.members "Snapshot" xml))
    let to_query v = Query.to_query_list Snapshot.to_query v
    let to_json v = `List (List.map Snapshot.to_json v)
    let of_json j = Json.to_list Snapshot.of_json j
  end
module ServiceUpdateStatusList =
  struct
    type t = ServiceUpdateStatus.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map ServiceUpdateStatus.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list ServiceUpdateStatus.to_query v
    let to_json v = `List (List.map ServiceUpdateStatus.to_json v)
    let of_json j = Json.to_list ServiceUpdateStatus.of_json j
  end
module GlobalReplicationGroupList =
  struct
    type t = GlobalReplicationGroup.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map GlobalReplicationGroup.parse
           (Xml.members "GlobalReplicationGroup" xml))
    let to_query v = Query.to_query_list GlobalReplicationGroup.to_query v
    let to_json v = `List (List.map GlobalReplicationGroup.to_json v)
    let of_json j = Json.to_list GlobalReplicationGroup.of_json j
  end
module ServiceUpdateList =
  struct
    type t = ServiceUpdate.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map ServiceUpdate.parse (Xml.members "ServiceUpdate" xml))
    let to_query v = Query.to_query_list ServiceUpdate.to_query v
    let to_json v = `List (List.map ServiceUpdate.to_json v)
    let of_json j = Json.to_list ServiceUpdate.of_json j
  end
module UserList =
  struct
    type t = User.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map User.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list User.to_query v
    let to_json v = `List (List.map User.to_json v)
    let of_json j = Json.to_list User.of_json j
  end
module RemoveReplicasList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module ReplicaConfigurationList =
  struct
    type t = ConfigureShard.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map ConfigureShard.parse (Xml.members "ConfigureShard" xml))
    let to_query v = Query.to_query_list ConfigureShard.to_query v
    let to_json v = `List (List.map ConfigureShard.to_json v)
    let of_json j = Json.to_list ConfigureShard.of_json j
  end
module ParameterNameValueList =
  struct
    type t = ParameterNameValue.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map ParameterNameValue.parse
           (Xml.members "ParameterNameValue" xml))
    let to_query v = Query.to_query_list ParameterNameValue.to_query v
    let to_json v = `List (List.map ParameterNameValue.to_json v)
    let of_json j = Json.to_list ParameterNameValue.of_json j
  end
module CacheClusterList =
  struct
    type t = CacheCluster.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map CacheCluster.parse (Xml.members "CacheCluster" xml))
    let to_query v = Query.to_query_list CacheCluster.to_query v
    let to_json v = `List (List.map CacheCluster.to_json v)
    let of_json j = Json.to_list CacheCluster.of_json j
  end
module UserGroupList =
  struct
    type t = UserGroup.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map UserGroup.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list UserGroup.to_query v
    let to_json v = `List (List.map UserGroup.to_json v)
    let of_json j = Json.to_list UserGroup.of_json j
  end
module NodeGroupsToRemoveList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map String.parse (Xml.members "NodeGroupToRemove" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module NodeGroupsToRetainList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map String.parse (Xml.members "NodeGroupToRetain" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module TimeRangeFilter =
  struct
    type t = {
      start_time: DateTime.t option ;
      end_time: DateTime.t option }
    let make ?start_time  ?end_time  () = { start_time; end_time }
    let parse xml =
      Some
        {
          start_time =
            (Util.option_bind (Xml.member "StartTime" xml) DateTime.parse);
          end_time =
            (Util.option_bind (Xml.member "EndTime" xml) DateTime.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.end_time
              (fun f -> Query.Pair ("EndTime", (DateTime.to_query f)));
           Util.option_map v.start_time
             (fun f -> Query.Pair ("StartTime", (DateTime.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.end_time
              (fun f -> ("end_time", (DateTime.to_json f)));
           Util.option_map v.start_time
             (fun f -> ("start_time", (DateTime.to_json f)))])
    let of_json j =
      {
        start_time =
          (Util.option_map (Json.lookup j "start_time") DateTime.of_json);
        end_time =
          (Util.option_map (Json.lookup j "end_time") DateTime.of_json)
      }
  end
module UpdateActionStatusList =
  struct
    type t = UpdateActionStatus.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map UpdateActionStatus.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list UpdateActionStatus.to_query v
    let to_json v = `List (List.map UpdateActionStatus.to_json v)
    let of_json j = Json.to_list UpdateActionStatus.of_json j
  end
module UpdateActionList =
  struct
    type t = UpdateAction.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map UpdateAction.parse (Xml.members "UpdateAction" xml))
    let to_query v = Query.to_query_list UpdateAction.to_query v
    let to_json v = `List (List.map UpdateAction.to_json v)
    let of_json j = Json.to_list UpdateAction.of_json j
  end
module EventList =
  struct
    type t = Event.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Event.parse (Xml.members "Event" xml))
    let to_query v = Query.to_query_list Event.to_query v
    let to_json v = `List (List.map Event.to_json v)
    let of_json j = Json.to_list Event.of_json j
  end
module OutpostMode =
  struct
    type t =
      | Single_outpost 
      | Cross_outpost 
    let str_to_t =
      [("cross-outpost", Cross_outpost); ("single-outpost", Single_outpost)]
    let t_to_str =
      [(Cross_outpost, "cross-outpost"); (Single_outpost, "single-outpost")]
    let to_string e = Util.of_option_exn (Util.list_find t_to_str e)
    let of_string s = Util.of_option_exn (Util.list_find str_to_t s)
    let make v () = v
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s -> Util.list_find str_to_t s)
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v)))
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v))
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j))
  end
module ProcessedUpdateActionList =
  struct
    type t = ProcessedUpdateAction.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map ProcessedUpdateAction.parse
           (Xml.members "ProcessedUpdateAction" xml))
    let to_query v = Query.to_query_list ProcessedUpdateAction.to_query v
    let to_json v = `List (List.map ProcessedUpdateAction.to_json v)
    let of_json j = Json.to_list ProcessedUpdateAction.of_json j
  end
module UnprocessedUpdateActionList =
  struct
    type t = UnprocessedUpdateAction.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map UnprocessedUpdateAction.parse
           (Xml.members "UnprocessedUpdateAction" xml))
    let to_query v = Query.to_query_list UnprocessedUpdateAction.to_query v
    let to_json v = `List (List.map UnprocessedUpdateAction.to_json v)
    let of_json j = Json.to_list UnprocessedUpdateAction.of_json j
  end
module KeyList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module ReplicationGroupList =
  struct
    type t = ReplicationGroup.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map ReplicationGroup.parse (Xml.members "ReplicationGroup" xml))
    let to_query v = Query.to_query_list ReplicationGroup.to_query v
    let to_json v = `List (List.map ReplicationGroup.to_json v)
    let of_json j = Json.to_list ReplicationGroup.of_json j
  end
module CacheEngineVersionList =
  struct
    type t = CacheEngineVersion.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map CacheEngineVersion.parse
           (Xml.members "CacheEngineVersion" xml))
    let to_query v = Query.to_query_list CacheEngineVersion.to_query v
    let to_json v = `List (List.map CacheEngineVersion.to_json v)
    let of_json j = Json.to_list CacheEngineVersion.of_json j
  end
module ReservedCacheNodeList =
  struct
    type t = ReservedCacheNode.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map ReservedCacheNode.parse
           (Xml.members "ReservedCacheNode" xml))
    let to_query v = Query.to_query_list ReservedCacheNode.to_query v
    let to_json v = `List (List.map ReservedCacheNode.to_json v)
    let of_json j = Json.to_list ReservedCacheNode.of_json j
  end
module InvalidUserGroupStateFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ListAllowedNodeTypeModificationsMessage =
  struct
    type t =
      {
      cache_cluster_id: String.t option ;
      replication_group_id: String.t option }
    let make ?cache_cluster_id  ?replication_group_id  () =
      { cache_cluster_id; replication_group_id }
    let parse xml =
      Some
        {
          cache_cluster_id =
            (Util.option_bind (Xml.member "CacheClusterId" xml) String.parse);
          replication_group_id =
            (Util.option_bind (Xml.member "ReplicationGroupId" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.replication_group_id
              (fun f ->
                 Query.Pair ("ReplicationGroupId", (String.to_query f)));
           Util.option_map v.cache_cluster_id
             (fun f -> Query.Pair ("CacheClusterId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.replication_group_id
              (fun f -> ("replication_group_id", (String.to_json f)));
           Util.option_map v.cache_cluster_id
             (fun f -> ("cache_cluster_id", (String.to_json f)))])
    let of_json j =
      {
        cache_cluster_id =
          (Util.option_map (Json.lookup j "cache_cluster_id") String.of_json);
        replication_group_id =
          (Util.option_map (Json.lookup j "replication_group_id")
             String.of_json)
      }
  end
module CacheParameterGroupsMessage =
  struct
    type t =
      {
      marker: String.t option ;
      cache_parameter_groups: CacheParameterGroupList.t }
    let make ?marker  ?(cache_parameter_groups= [])  () =
      { marker; cache_parameter_groups }
    let parse xml =
      Some
        {
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          cache_parameter_groups =
            (Util.of_option []
               (Util.option_bind (Xml.member "CacheParameterGroups" xml)
                  CacheParameterGroupList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("CacheParameterGroups.member",
                   (CacheParameterGroupList.to_query v.cache_parameter_groups)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("cache_parameter_groups",
                (CacheParameterGroupList.to_json v.cache_parameter_groups));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)))])
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        cache_parameter_groups =
          (CacheParameterGroupList.of_json
             (Util.of_option_exn (Json.lookup j "cache_parameter_groups")))
      }
  end
module DescribeCacheClustersMessage =
  struct
    type t =
      {
      cache_cluster_id: String.t option ;
      max_records: Integer.t option ;
      marker: String.t option ;
      show_cache_node_info: Boolean.t option ;
      show_cache_clusters_not_in_replication_groups: Boolean.t option }
    let make ?cache_cluster_id  ?max_records  ?marker  ?show_cache_node_info 
      ?show_cache_clusters_not_in_replication_groups  () =
      {
        cache_cluster_id;
        max_records;
        marker;
        show_cache_node_info;
        show_cache_clusters_not_in_replication_groups
      }
    let parse xml =
      Some
        {
          cache_cluster_id =
            (Util.option_bind (Xml.member "CacheClusterId" xml) String.parse);
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          show_cache_node_info =
            (Util.option_bind (Xml.member "ShowCacheNodeInfo" xml)
               Boolean.parse);
          show_cache_clusters_not_in_replication_groups =
            (Util.option_bind
               (Xml.member "ShowCacheClustersNotInReplicationGroups" xml)
               Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.show_cache_clusters_not_in_replication_groups
              (fun f ->
                 Query.Pair
                   ("ShowCacheClustersNotInReplicationGroups",
                     (Boolean.to_query f)));
           Util.option_map v.show_cache_node_info
             (fun f -> Query.Pair ("ShowCacheNodeInfo", (Boolean.to_query f)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Util.option_map v.cache_cluster_id
             (fun f -> Query.Pair ("CacheClusterId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.show_cache_clusters_not_in_replication_groups
              (fun f ->
                 ("show_cache_clusters_not_in_replication_groups",
                   (Boolean.to_json f)));
           Util.option_map v.show_cache_node_info
             (fun f -> ("show_cache_node_info", (Boolean.to_json f)));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Util.option_map v.cache_cluster_id
             (fun f -> ("cache_cluster_id", (String.to_json f)))])
    let of_json j =
      {
        cache_cluster_id =
          (Util.option_map (Json.lookup j "cache_cluster_id") String.of_json);
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        show_cache_node_info =
          (Util.option_map (Json.lookup j "show_cache_node_info")
             Boolean.of_json);
        show_cache_clusters_not_in_replication_groups =
          (Util.option_map
             (Json.lookup j "show_cache_clusters_not_in_replication_groups")
             Boolean.of_json)
      }
  end
module CreateReplicationGroupResult =
  struct
    type t = {
      replication_group: ReplicationGroup.t option }
    let make ?replication_group  () = { replication_group }
    let parse xml =
      Some
        {
          replication_group =
            (Util.option_bind (Xml.member "ReplicationGroup" xml)
               ReplicationGroup.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.replication_group
              (fun f ->
                 Query.Pair
                   ("ReplicationGroup", (ReplicationGroup.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.replication_group
              (fun f -> ("replication_group", (ReplicationGroup.to_json f)))])
    let of_json j =
      {
        replication_group =
          (Util.option_map (Json.lookup j "replication_group")
             ReplicationGroup.of_json)
      }
  end
module CreateUserMessage =
  struct
    type t =
      {
      user_id: String.t ;
      user_name: String.t ;
      engine: String.t ;
      passwords: PasswordListInput.t ;
      access_string: String.t ;
      no_password_required: Boolean.t option }
    let make ~user_id  ~user_name  ~engine  ?(passwords= [])  ~access_string 
      ?no_password_required  () =
      {
        user_id;
        user_name;
        engine;
        passwords;
        access_string;
        no_password_required
      }
    let parse xml =
      Some
        {
          user_id =
            (Xml.required "UserId"
               (Util.option_bind (Xml.member "UserId" xml) String.parse));
          user_name =
            (Xml.required "UserName"
               (Util.option_bind (Xml.member "UserName" xml) String.parse));
          engine =
            (Xml.required "Engine"
               (Util.option_bind (Xml.member "Engine" xml) String.parse));
          passwords =
            (Util.of_option []
               (Util.option_bind (Xml.member "Passwords" xml)
                  PasswordListInput.parse));
          access_string =
            (Xml.required "AccessString"
               (Util.option_bind (Xml.member "AccessString" xml) String.parse));
          no_password_required =
            (Util.option_bind (Xml.member "NoPasswordRequired" xml)
               Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.no_password_required
              (fun f ->
                 Query.Pair ("NoPasswordRequired", (Boolean.to_query f)));
           Some
             (Query.Pair ("AccessString", (String.to_query v.access_string)));
           Some
             (Query.Pair
                ("Passwords.member",
                  (PasswordListInput.to_query v.passwords)));
           Some (Query.Pair ("Engine", (String.to_query v.engine)));
           Some (Query.Pair ("UserName", (String.to_query v.user_name)));
           Some (Query.Pair ("UserId", (String.to_query v.user_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.no_password_required
              (fun f -> ("no_password_required", (Boolean.to_json f)));
           Some ("access_string", (String.to_json v.access_string));
           Some ("passwords", (PasswordListInput.to_json v.passwords));
           Some ("engine", (String.to_json v.engine));
           Some ("user_name", (String.to_json v.user_name));
           Some ("user_id", (String.to_json v.user_id))])
    let of_json j =
      {
        user_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "user_id")));
        user_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "user_name")));
        engine =
          (String.of_json (Util.of_option_exn (Json.lookup j "engine")));
        passwords =
          (PasswordListInput.of_json
             (Util.of_option_exn (Json.lookup j "passwords")));
        access_string =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "access_string")));
        no_password_required =
          (Util.option_map (Json.lookup j "no_password_required")
             Boolean.of_json)
      }
  end
module ModifyCacheSubnetGroupResult =
  struct
    type t = {
      cache_subnet_group: CacheSubnetGroup.t option }
    let make ?cache_subnet_group  () = { cache_subnet_group }
    let parse xml =
      Some
        {
          cache_subnet_group =
            (Util.option_bind (Xml.member "CacheSubnetGroup" xml)
               CacheSubnetGroup.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.cache_subnet_group
              (fun f ->
                 Query.Pair
                   ("CacheSubnetGroup", (CacheSubnetGroup.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cache_subnet_group
              (fun f -> ("cache_subnet_group", (CacheSubnetGroup.to_json f)))])
    let of_json j =
      {
        cache_subnet_group =
          (Util.option_map (Json.lookup j "cache_subnet_group")
             CacheSubnetGroup.of_json)
      }
  end
module CacheParameterGroupAlreadyExistsFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DeleteCacheClusterResult =
  struct
    type t = {
      cache_cluster: CacheCluster.t option }
    let make ?cache_cluster  () = { cache_cluster }
    let parse xml =
      Some
        {
          cache_cluster =
            (Util.option_bind (Xml.member "CacheCluster" xml)
               CacheCluster.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.cache_cluster
              (fun f ->
                 Query.Pair ("CacheCluster", (CacheCluster.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cache_cluster
              (fun f -> ("cache_cluster", (CacheCluster.to_json f)))])
    let of_json j =
      {
        cache_cluster =
          (Util.option_map (Json.lookup j "cache_cluster")
             CacheCluster.of_json)
      }
  end
module DeleteUserMessage =
  struct
    type t = {
      user_id: String.t }
    let make ~user_id  () = { user_id }
    let parse xml =
      Some
        {
          user_id =
            (Xml.required "UserId"
               (Util.option_bind (Xml.member "UserId" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("UserId", (String.to_query v.user_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt [Some ("user_id", (String.to_json v.user_id))])
    let of_json j =
      {
        user_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "user_id")))
      }
  end
module DescribeGlobalReplicationGroupsMessage =
  struct
    type t =
      {
      global_replication_group_id: String.t option ;
      max_records: Integer.t option ;
      marker: String.t option ;
      show_member_info: Boolean.t option }
    let make ?global_replication_group_id  ?max_records  ?marker 
      ?show_member_info  () =
      { global_replication_group_id; max_records; marker; show_member_info }
    let parse xml =
      Some
        {
          global_replication_group_id =
            (Util.option_bind (Xml.member "GlobalReplicationGroupId" xml)
               String.parse);
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          show_member_info =
            (Util.option_bind (Xml.member "ShowMemberInfo" xml) Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.show_member_info
              (fun f -> Query.Pair ("ShowMemberInfo", (Boolean.to_query f)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Util.option_map v.global_replication_group_id
             (fun f ->
                Query.Pair ("GlobalReplicationGroupId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.show_member_info
              (fun f -> ("show_member_info", (Boolean.to_json f)));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Util.option_map v.global_replication_group_id
             (fun f -> ("global_replication_group_id", (String.to_json f)))])
    let of_json j =
      {
        global_replication_group_id =
          (Util.option_map (Json.lookup j "global_replication_group_id")
             String.of_json);
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        show_member_info =
          (Util.option_map (Json.lookup j "show_member_info") Boolean.of_json)
      }
  end
module AuthorizationNotFoundFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DefaultUserAssociatedToUserGroupFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ReplicationGroupAlreadyUnderMigrationFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CacheSubnetGroupMessage =
  struct
    type t =
      {
      marker: String.t option ;
      cache_subnet_groups: CacheSubnetGroups.t }
    let make ?marker  ?(cache_subnet_groups= [])  () =
      { marker; cache_subnet_groups }
    let parse xml =
      Some
        {
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          cache_subnet_groups =
            (Util.of_option []
               (Util.option_bind (Xml.member "CacheSubnetGroups" xml)
                  CacheSubnetGroups.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("CacheSubnetGroups.member",
                   (CacheSubnetGroups.to_query v.cache_subnet_groups)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("cache_subnet_groups",
                (CacheSubnetGroups.to_json v.cache_subnet_groups));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)))])
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        cache_subnet_groups =
          (CacheSubnetGroups.of_json
             (Util.of_option_exn (Json.lookup j "cache_subnet_groups")))
      }
  end
module CacheSecurityGroupNotFoundFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DuplicateUserNameFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CompleteMigrationMessage =
  struct
    type t = {
      replication_group_id: String.t ;
      force: Boolean.t option }
    let make ~replication_group_id  ?force  () =
      { replication_group_id; force }
    let parse xml =
      Some
        {
          replication_group_id =
            (Xml.required "ReplicationGroupId"
               (Util.option_bind (Xml.member "ReplicationGroupId" xml)
                  String.parse));
          force = (Util.option_bind (Xml.member "Force" xml) Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.force
              (fun f -> Query.Pair ("Force", (Boolean.to_query f)));
           Some
             (Query.Pair
                ("ReplicationGroupId",
                  (String.to_query v.replication_group_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.force (fun f -> ("force", (Boolean.to_json f)));
           Some
             ("replication_group_id",
               (String.to_json v.replication_group_id))])
    let of_json j =
      {
        replication_group_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "replication_group_id")));
        force = (Util.option_map (Json.lookup j "force") Boolean.of_json)
      }
  end
module DeleteCacheParameterGroupMessage =
  struct
    type t = {
      cache_parameter_group_name: String.t }
    let make ~cache_parameter_group_name  () = { cache_parameter_group_name }
    let parse xml =
      Some
        {
          cache_parameter_group_name =
            (Xml.required "CacheParameterGroupName"
               (Util.option_bind (Xml.member "CacheParameterGroupName" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("CacheParameterGroupName",
                   (String.to_query v.cache_parameter_group_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("cache_parameter_group_name",
                (String.to_json v.cache_parameter_group_name))])
    let of_json j =
      {
        cache_parameter_group_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "cache_parameter_group_name")))
      }
  end
module DecreaseReplicaCountResult =
  struct
    type t = {
      replication_group: ReplicationGroup.t option }
    let make ?replication_group  () = { replication_group }
    let parse xml =
      Some
        {
          replication_group =
            (Util.option_bind (Xml.member "ReplicationGroup" xml)
               ReplicationGroup.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.replication_group
              (fun f ->
                 Query.Pair
                   ("ReplicationGroup", (ReplicationGroup.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.replication_group
              (fun f -> ("replication_group", (ReplicationGroup.to_json f)))])
    let of_json j =
      {
        replication_group =
          (Util.option_map (Json.lookup j "replication_group")
             ReplicationGroup.of_json)
      }
  end
module NodeQuotaForCustomerExceededFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module UserGroupAlreadyExistsFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module NodeGroupsPerReplicationGroupQuotaExceededFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module TagListMessage =
  struct
    type t = {
      tag_list: TagList.t }
    let make ?(tag_list= [])  () = { tag_list }
    let parse xml =
      Some
        {
          tag_list =
            (Util.of_option []
               (Util.option_bind (Xml.member "TagList" xml) TagList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("TagList.member", (TagList.to_query v.tag_list)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tag_list", (TagList.to_json v.tag_list))])
    let of_json j =
      {
        tag_list =
          (TagList.of_json (Util.of_option_exn (Json.lookup j "tag_list")))
      }
  end
module PurchaseReservedCacheNodesOfferingMessage =
  struct
    type t =
      {
      reserved_cache_nodes_offering_id: String.t ;
      reserved_cache_node_id: String.t option ;
      cache_node_count: Integer.t option }
    let make ~reserved_cache_nodes_offering_id  ?reserved_cache_node_id 
      ?cache_node_count  () =
      {
        reserved_cache_nodes_offering_id;
        reserved_cache_node_id;
        cache_node_count
      }
    let parse xml =
      Some
        {
          reserved_cache_nodes_offering_id =
            (Xml.required "ReservedCacheNodesOfferingId"
               (Util.option_bind
                  (Xml.member "ReservedCacheNodesOfferingId" xml)
                  String.parse));
          reserved_cache_node_id =
            (Util.option_bind (Xml.member "ReservedCacheNodeId" xml)
               String.parse);
          cache_node_count =
            (Util.option_bind (Xml.member "CacheNodeCount" xml) Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.cache_node_count
              (fun f -> Query.Pair ("CacheNodeCount", (Integer.to_query f)));
           Util.option_map v.reserved_cache_node_id
             (fun f ->
                Query.Pair ("ReservedCacheNodeId", (String.to_query f)));
           Some
             (Query.Pair
                ("ReservedCacheNodesOfferingId",
                  (String.to_query v.reserved_cache_nodes_offering_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cache_node_count
              (fun f -> ("cache_node_count", (Integer.to_json f)));
           Util.option_map v.reserved_cache_node_id
             (fun f -> ("reserved_cache_node_id", (String.to_json f)));
           Some
             ("reserved_cache_nodes_offering_id",
               (String.to_json v.reserved_cache_nodes_offering_id))])
    let of_json j =
      {
        reserved_cache_nodes_offering_id =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "reserved_cache_nodes_offering_id")));
        reserved_cache_node_id =
          (Util.option_map (Json.lookup j "reserved_cache_node_id")
             String.of_json);
        cache_node_count =
          (Util.option_map (Json.lookup j "cache_node_count") Integer.of_json)
      }
  end
module IncreaseReplicaCountResult =
  struct
    type t = {
      replication_group: ReplicationGroup.t option }
    let make ?replication_group  () = { replication_group }
    let parse xml =
      Some
        {
          replication_group =
            (Util.option_bind (Xml.member "ReplicationGroup" xml)
               ReplicationGroup.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.replication_group
              (fun f ->
                 Query.Pair
                   ("ReplicationGroup", (ReplicationGroup.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.replication_group
              (fun f -> ("replication_group", (ReplicationGroup.to_json f)))])
    let of_json j =
      {
        replication_group =
          (Util.option_map (Json.lookup j "replication_group")
             ReplicationGroup.of_json)
      }
  end
module AuthorizationAlreadyExistsFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module UserQuotaExceededFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module TestFailoverMessage =
  struct
    type t = {
      replication_group_id: String.t ;
      node_group_id: String.t }
    let make ~replication_group_id  ~node_group_id  () =
      { replication_group_id; node_group_id }
    let parse xml =
      Some
        {
          replication_group_id =
            (Xml.required "ReplicationGroupId"
               (Util.option_bind (Xml.member "ReplicationGroupId" xml)
                  String.parse));
          node_group_id =
            (Xml.required "NodeGroupId"
               (Util.option_bind (Xml.member "NodeGroupId" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("NodeGroupId", (String.to_query v.node_group_id)));
           Some
             (Query.Pair
                ("ReplicationGroupId",
                  (String.to_query v.replication_group_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("node_group_id", (String.to_json v.node_group_id));
           Some
             ("replication_group_id",
               (String.to_json v.replication_group_id))])
    let of_json j =
      {
        replication_group_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "replication_group_id")));
        node_group_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "node_group_id")))
      }
  end
module DescribeCacheSecurityGroupsMessage =
  struct
    type t =
      {
      cache_security_group_name: String.t option ;
      max_records: Integer.t option ;
      marker: String.t option }
    let make ?cache_security_group_name  ?max_records  ?marker  () =
      { cache_security_group_name; max_records; marker }
    let parse xml =
      Some
        {
          cache_security_group_name =
            (Util.option_bind (Xml.member "CacheSecurityGroupName" xml)
               String.parse);
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Util.option_map v.cache_security_group_name
             (fun f ->
                Query.Pair ("CacheSecurityGroupName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Util.option_map v.cache_security_group_name
             (fun f -> ("cache_security_group_name", (String.to_json f)))])
    let of_json j =
      {
        cache_security_group_name =
          (Util.option_map (Json.lookup j "cache_security_group_name")
             String.of_json);
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module CacheSubnetGroupNotFoundFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CacheSecurityGroupAlreadyExistsFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CreateUserGroupMessage =
  struct
    type t =
      {
      user_group_id: String.t ;
      engine: String.t ;
      user_ids: UserIdListInput.t }
    let make ~user_group_id  ~engine  ?(user_ids= [])  () =
      { user_group_id; engine; user_ids }
    let parse xml =
      Some
        {
          user_group_id =
            (Xml.required "UserGroupId"
               (Util.option_bind (Xml.member "UserGroupId" xml) String.parse));
          engine =
            (Xml.required "Engine"
               (Util.option_bind (Xml.member "Engine" xml) String.parse));
          user_ids =
            (Util.of_option []
               (Util.option_bind (Xml.member "UserIds" xml)
                  UserIdListInput.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("UserIds.member", (UserIdListInput.to_query v.user_ids)));
           Some (Query.Pair ("Engine", (String.to_query v.engine)));
           Some
             (Query.Pair ("UserGroupId", (String.to_query v.user_group_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("user_ids", (UserIdListInput.to_json v.user_ids));
           Some ("engine", (String.to_json v.engine));
           Some ("user_group_id", (String.to_json v.user_group_id))])
    let of_json j =
      {
        user_group_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "user_group_id")));
        engine =
          (String.of_json (Util.of_option_exn (Json.lookup j "engine")));
        user_ids =
          (UserIdListInput.of_json
             (Util.of_option_exn (Json.lookup j "user_ids")))
      }
  end
module RevokeCacheSecurityGroupIngressResult =
  struct
    type t = {
      cache_security_group: CacheSecurityGroup.t option }
    let make ?cache_security_group  () = { cache_security_group }
    let parse xml =
      Some
        {
          cache_security_group =
            (Util.option_bind (Xml.member "CacheSecurityGroup" xml)
               CacheSecurityGroup.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.cache_security_group
              (fun f ->
                 Query.Pair
                   ("CacheSecurityGroup", (CacheSecurityGroup.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cache_security_group
              (fun f ->
                 ("cache_security_group", (CacheSecurityGroup.to_json f)))])
    let of_json j =
      {
        cache_security_group =
          (Util.option_map (Json.lookup j "cache_security_group")
             CacheSecurityGroup.of_json)
      }
  end
module ModifyCacheSubnetGroupMessage =
  struct
    type t =
      {
      cache_subnet_group_name: String.t ;
      cache_subnet_group_description: String.t option ;
      subnet_ids: SubnetIdentifierList.t }
    let make ~cache_subnet_group_name  ?cache_subnet_group_description 
      ?(subnet_ids= [])  () =
      { cache_subnet_group_name; cache_subnet_group_description; subnet_ids }
    let parse xml =
      Some
        {
          cache_subnet_group_name =
            (Xml.required "CacheSubnetGroupName"
               (Util.option_bind (Xml.member "CacheSubnetGroupName" xml)
                  String.parse));
          cache_subnet_group_description =
            (Util.option_bind (Xml.member "CacheSubnetGroupDescription" xml)
               String.parse);
          subnet_ids =
            (Util.of_option []
               (Util.option_bind (Xml.member "SubnetIds" xml)
                  SubnetIdentifierList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("SubnetIds.member",
                   (SubnetIdentifierList.to_query v.subnet_ids)));
           Util.option_map v.cache_subnet_group_description
             (fun f ->
                Query.Pair
                  ("CacheSubnetGroupDescription", (String.to_query f)));
           Some
             (Query.Pair
                ("CacheSubnetGroupName",
                  (String.to_query v.cache_subnet_group_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("subnet_ids", (SubnetIdentifierList.to_json v.subnet_ids));
           Util.option_map v.cache_subnet_group_description
             (fun f -> ("cache_subnet_group_description", (String.to_json f)));
           Some
             ("cache_subnet_group_name",
               (String.to_json v.cache_subnet_group_name))])
    let of_json j =
      {
        cache_subnet_group_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "cache_subnet_group_name")));
        cache_subnet_group_description =
          (Util.option_map (Json.lookup j "cache_subnet_group_description")
             String.of_json);
        subnet_ids =
          (SubnetIdentifierList.of_json
             (Util.of_option_exn (Json.lookup j "subnet_ids")))
      }
  end
module ReservedCacheNodeNotFoundFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DecreaseNodeGroupsInGlobalReplicationGroupMessage =
  struct
    type t =
      {
      global_replication_group_id: String.t ;
      node_group_count: Integer.t ;
      global_node_groups_to_remove: GlobalNodeGroupIdList.t ;
      global_node_groups_to_retain: GlobalNodeGroupIdList.t ;
      apply_immediately: Boolean.t }
    let make ~global_replication_group_id  ~node_group_count 
      ?(global_node_groups_to_remove= [])  ?(global_node_groups_to_retain=
      [])  ~apply_immediately  () =
      {
        global_replication_group_id;
        node_group_count;
        global_node_groups_to_remove;
        global_node_groups_to_retain;
        apply_immediately
      }
    let parse xml =
      Some
        {
          global_replication_group_id =
            (Xml.required "GlobalReplicationGroupId"
               (Util.option_bind (Xml.member "GlobalReplicationGroupId" xml)
                  String.parse));
          node_group_count =
            (Xml.required "NodeGroupCount"
               (Util.option_bind (Xml.member "NodeGroupCount" xml)
                  Integer.parse));
          global_node_groups_to_remove =
            (Util.of_option []
               (Util.option_bind (Xml.member "GlobalNodeGroupsToRemove" xml)
                  GlobalNodeGroupIdList.parse));
          global_node_groups_to_retain =
            (Util.of_option []
               (Util.option_bind (Xml.member "GlobalNodeGroupsToRetain" xml)
                  GlobalNodeGroupIdList.parse));
          apply_immediately =
            (Xml.required "ApplyImmediately"
               (Util.option_bind (Xml.member "ApplyImmediately" xml)
                  Boolean.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ApplyImmediately", (Boolean.to_query v.apply_immediately)));
           Some
             (Query.Pair
                ("GlobalNodeGroupsToRetain.member",
                  (GlobalNodeGroupIdList.to_query
                     v.global_node_groups_to_retain)));
           Some
             (Query.Pair
                ("GlobalNodeGroupsToRemove.member",
                  (GlobalNodeGroupIdList.to_query
                     v.global_node_groups_to_remove)));
           Some
             (Query.Pair
                ("NodeGroupCount", (Integer.to_query v.node_group_count)));
           Some
             (Query.Pair
                ("GlobalReplicationGroupId",
                  (String.to_query v.global_replication_group_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("apply_immediately", (Boolean.to_json v.apply_immediately));
           Some
             ("global_node_groups_to_retain",
               (GlobalNodeGroupIdList.to_json v.global_node_groups_to_retain));
           Some
             ("global_node_groups_to_remove",
               (GlobalNodeGroupIdList.to_json v.global_node_groups_to_remove));
           Some ("node_group_count", (Integer.to_json v.node_group_count));
           Some
             ("global_replication_group_id",
               (String.to_json v.global_replication_group_id))])
    let of_json j =
      {
        global_replication_group_id =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "global_replication_group_id")));
        node_group_count =
          (Integer.of_json
             (Util.of_option_exn (Json.lookup j "node_group_count")));
        global_node_groups_to_remove =
          (GlobalNodeGroupIdList.of_json
             (Util.of_option_exn
                (Json.lookup j "global_node_groups_to_remove")));
        global_node_groups_to_retain =
          (GlobalNodeGroupIdList.of_json
             (Util.of_option_exn
                (Json.lookup j "global_node_groups_to_retain")));
        apply_immediately =
          (Boolean.of_json
             (Util.of_option_exn (Json.lookup j "apply_immediately")))
      }
  end
module InvalidReplicationGroupStateFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CreateReplicationGroupMessage =
  struct
    type t =
      {
      replication_group_id: String.t ;
      replication_group_description: String.t ;
      global_replication_group_id: String.t option ;
      primary_cluster_id: String.t option ;
      automatic_failover_enabled: Boolean.t option ;
      multi_a_z_enabled: Boolean.t option ;
      num_cache_clusters: Integer.t option ;
      preferred_cache_cluster_a_zs: AvailabilityZonesList.t ;
      num_node_groups: Integer.t option ;
      replicas_per_node_group: Integer.t option ;
      node_group_configuration: NodeGroupConfigurationList.t ;
      cache_node_type: String.t option ;
      engine: String.t option ;
      engine_version: String.t option ;
      cache_parameter_group_name: String.t option ;
      cache_subnet_group_name: String.t option ;
      cache_security_group_names: CacheSecurityGroupNameList.t ;
      security_group_ids: SecurityGroupIdsList.t ;
      tags: TagList.t ;
      snapshot_arns: SnapshotArnsList.t ;
      snapshot_name: String.t option ;
      preferred_maintenance_window: String.t option ;
      port: Integer.t option ;
      notification_topic_arn: String.t option ;
      auto_minor_version_upgrade: Boolean.t option ;
      snapshot_retention_limit: Integer.t option ;
      snapshot_window: String.t option ;
      auth_token: String.t option ;
      transit_encryption_enabled: Boolean.t option ;
      at_rest_encryption_enabled: Boolean.t option ;
      kms_key_id: String.t option ;
      user_group_ids: UserGroupIdListInput.t }
    let make ~replication_group_id  ~replication_group_description 
      ?global_replication_group_id  ?primary_cluster_id 
      ?automatic_failover_enabled  ?multi_a_z_enabled  ?num_cache_clusters 
      ?(preferred_cache_cluster_a_zs= [])  ?num_node_groups 
      ?replicas_per_node_group  ?(node_group_configuration= []) 
      ?cache_node_type  ?engine  ?engine_version  ?cache_parameter_group_name
       ?cache_subnet_group_name  ?(cache_security_group_names= []) 
      ?(security_group_ids= [])  ?(tags= [])  ?(snapshot_arns= []) 
      ?snapshot_name  ?preferred_maintenance_window  ?port 
      ?notification_topic_arn  ?auto_minor_version_upgrade 
      ?snapshot_retention_limit  ?snapshot_window  ?auth_token 
      ?transit_encryption_enabled  ?at_rest_encryption_enabled  ?kms_key_id 
      ?(user_group_ids= [])  () =
      {
        replication_group_id;
        replication_group_description;
        global_replication_group_id;
        primary_cluster_id;
        automatic_failover_enabled;
        multi_a_z_enabled;
        num_cache_clusters;
        preferred_cache_cluster_a_zs;
        num_node_groups;
        replicas_per_node_group;
        node_group_configuration;
        cache_node_type;
        engine;
        engine_version;
        cache_parameter_group_name;
        cache_subnet_group_name;
        cache_security_group_names;
        security_group_ids;
        tags;
        snapshot_arns;
        snapshot_name;
        preferred_maintenance_window;
        port;
        notification_topic_arn;
        auto_minor_version_upgrade;
        snapshot_retention_limit;
        snapshot_window;
        auth_token;
        transit_encryption_enabled;
        at_rest_encryption_enabled;
        kms_key_id;
        user_group_ids
      }
    let parse xml =
      Some
        {
          replication_group_id =
            (Xml.required "ReplicationGroupId"
               (Util.option_bind (Xml.member "ReplicationGroupId" xml)
                  String.parse));
          replication_group_description =
            (Xml.required "ReplicationGroupDescription"
               (Util.option_bind
                  (Xml.member "ReplicationGroupDescription" xml) String.parse));
          global_replication_group_id =
            (Util.option_bind (Xml.member "GlobalReplicationGroupId" xml)
               String.parse);
          primary_cluster_id =
            (Util.option_bind (Xml.member "PrimaryClusterId" xml)
               String.parse);
          automatic_failover_enabled =
            (Util.option_bind (Xml.member "AutomaticFailoverEnabled" xml)
               Boolean.parse);
          multi_a_z_enabled =
            (Util.option_bind (Xml.member "MultiAZEnabled" xml) Boolean.parse);
          num_cache_clusters =
            (Util.option_bind (Xml.member "NumCacheClusters" xml)
               Integer.parse);
          preferred_cache_cluster_a_zs =
            (Util.of_option []
               (Util.option_bind (Xml.member "PreferredCacheClusterAZs" xml)
                  AvailabilityZonesList.parse));
          num_node_groups =
            (Util.option_bind (Xml.member "NumNodeGroups" xml) Integer.parse);
          replicas_per_node_group =
            (Util.option_bind (Xml.member "ReplicasPerNodeGroup" xml)
               Integer.parse);
          node_group_configuration =
            (Util.of_option []
               (Util.option_bind (Xml.member "NodeGroupConfiguration" xml)
                  NodeGroupConfigurationList.parse));
          cache_node_type =
            (Util.option_bind (Xml.member "CacheNodeType" xml) String.parse);
          engine = (Util.option_bind (Xml.member "Engine" xml) String.parse);
          engine_version =
            (Util.option_bind (Xml.member "EngineVersion" xml) String.parse);
          cache_parameter_group_name =
            (Util.option_bind (Xml.member "CacheParameterGroupName" xml)
               String.parse);
          cache_subnet_group_name =
            (Util.option_bind (Xml.member "CacheSubnetGroupName" xml)
               String.parse);
          cache_security_group_names =
            (Util.of_option []
               (Util.option_bind (Xml.member "CacheSecurityGroupNames" xml)
                  CacheSecurityGroupNameList.parse));
          security_group_ids =
            (Util.of_option []
               (Util.option_bind (Xml.member "SecurityGroupIds" xml)
                  SecurityGroupIdsList.parse));
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) TagList.parse));
          snapshot_arns =
            (Util.of_option []
               (Util.option_bind (Xml.member "SnapshotArns" xml)
                  SnapshotArnsList.parse));
          snapshot_name =
            (Util.option_bind (Xml.member "SnapshotName" xml) String.parse);
          preferred_maintenance_window =
            (Util.option_bind (Xml.member "PreferredMaintenanceWindow" xml)
               String.parse);
          port = (Util.option_bind (Xml.member "Port" xml) Integer.parse);
          notification_topic_arn =
            (Util.option_bind (Xml.member "NotificationTopicArn" xml)
               String.parse);
          auto_minor_version_upgrade =
            (Util.option_bind (Xml.member "AutoMinorVersionUpgrade" xml)
               Boolean.parse);
          snapshot_retention_limit =
            (Util.option_bind (Xml.member "SnapshotRetentionLimit" xml)
               Integer.parse);
          snapshot_window =
            (Util.option_bind (Xml.member "SnapshotWindow" xml) String.parse);
          auth_token =
            (Util.option_bind (Xml.member "AuthToken" xml) String.parse);
          transit_encryption_enabled =
            (Util.option_bind (Xml.member "TransitEncryptionEnabled" xml)
               Boolean.parse);
          at_rest_encryption_enabled =
            (Util.option_bind (Xml.member "AtRestEncryptionEnabled" xml)
               Boolean.parse);
          kms_key_id =
            (Util.option_bind (Xml.member "KmsKeyId" xml) String.parse);
          user_group_ids =
            (Util.of_option []
               (Util.option_bind (Xml.member "UserGroupIds" xml)
                  UserGroupIdListInput.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("UserGroupIds.member",
                   (UserGroupIdListInput.to_query v.user_group_ids)));
           Util.option_map v.kms_key_id
             (fun f -> Query.Pair ("KmsKeyId", (String.to_query f)));
           Util.option_map v.at_rest_encryption_enabled
             (fun f ->
                Query.Pair ("AtRestEncryptionEnabled", (Boolean.to_query f)));
           Util.option_map v.transit_encryption_enabled
             (fun f ->
                Query.Pair ("TransitEncryptionEnabled", (Boolean.to_query f)));
           Util.option_map v.auth_token
             (fun f -> Query.Pair ("AuthToken", (String.to_query f)));
           Util.option_map v.snapshot_window
             (fun f -> Query.Pair ("SnapshotWindow", (String.to_query f)));
           Util.option_map v.snapshot_retention_limit
             (fun f ->
                Query.Pair ("SnapshotRetentionLimit", (Integer.to_query f)));
           Util.option_map v.auto_minor_version_upgrade
             (fun f ->
                Query.Pair ("AutoMinorVersionUpgrade", (Boolean.to_query f)));
           Util.option_map v.notification_topic_arn
             (fun f ->
                Query.Pair ("NotificationTopicArn", (String.to_query f)));
           Util.option_map v.port
             (fun f -> Query.Pair ("Port", (Integer.to_query f)));
           Util.option_map v.preferred_maintenance_window
             (fun f ->
                Query.Pair
                  ("PreferredMaintenanceWindow", (String.to_query f)));
           Util.option_map v.snapshot_name
             (fun f -> Query.Pair ("SnapshotName", (String.to_query f)));
           Some
             (Query.Pair
                ("SnapshotArns.member",
                  (SnapshotArnsList.to_query v.snapshot_arns)));
           Some (Query.Pair ("Tags.member", (TagList.to_query v.tags)));
           Some
             (Query.Pair
                ("SecurityGroupIds.member",
                  (SecurityGroupIdsList.to_query v.security_group_ids)));
           Some
             (Query.Pair
                ("CacheSecurityGroupNames.member",
                  (CacheSecurityGroupNameList.to_query
                     v.cache_security_group_names)));
           Util.option_map v.cache_subnet_group_name
             (fun f ->
                Query.Pair ("CacheSubnetGroupName", (String.to_query f)));
           Util.option_map v.cache_parameter_group_name
             (fun f ->
                Query.Pair ("CacheParameterGroupName", (String.to_query f)));
           Util.option_map v.engine_version
             (fun f -> Query.Pair ("EngineVersion", (String.to_query f)));
           Util.option_map v.engine
             (fun f -> Query.Pair ("Engine", (String.to_query f)));
           Util.option_map v.cache_node_type
             (fun f -> Query.Pair ("CacheNodeType", (String.to_query f)));
           Some
             (Query.Pair
                ("NodeGroupConfiguration.member",
                  (NodeGroupConfigurationList.to_query
                     v.node_group_configuration)));
           Util.option_map v.replicas_per_node_group
             (fun f ->
                Query.Pair ("ReplicasPerNodeGroup", (Integer.to_query f)));
           Util.option_map v.num_node_groups
             (fun f -> Query.Pair ("NumNodeGroups", (Integer.to_query f)));
           Some
             (Query.Pair
                ("PreferredCacheClusterAZs.member",
                  (AvailabilityZonesList.to_query
                     v.preferred_cache_cluster_a_zs)));
           Util.option_map v.num_cache_clusters
             (fun f -> Query.Pair ("NumCacheClusters", (Integer.to_query f)));
           Util.option_map v.multi_a_z_enabled
             (fun f -> Query.Pair ("MultiAZEnabled", (Boolean.to_query f)));
           Util.option_map v.automatic_failover_enabled
             (fun f ->
                Query.Pair ("AutomaticFailoverEnabled", (Boolean.to_query f)));
           Util.option_map v.primary_cluster_id
             (fun f -> Query.Pair ("PrimaryClusterId", (String.to_query f)));
           Util.option_map v.global_replication_group_id
             (fun f ->
                Query.Pair ("GlobalReplicationGroupId", (String.to_query f)));
           Some
             (Query.Pair
                ("ReplicationGroupDescription",
                  (String.to_query v.replication_group_description)));
           Some
             (Query.Pair
                ("ReplicationGroupId",
                  (String.to_query v.replication_group_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("user_group_ids",
                (UserGroupIdListInput.to_json v.user_group_ids));
           Util.option_map v.kms_key_id
             (fun f -> ("kms_key_id", (String.to_json f)));
           Util.option_map v.at_rest_encryption_enabled
             (fun f -> ("at_rest_encryption_enabled", (Boolean.to_json f)));
           Util.option_map v.transit_encryption_enabled
             (fun f -> ("transit_encryption_enabled", (Boolean.to_json f)));
           Util.option_map v.auth_token
             (fun f -> ("auth_token", (String.to_json f)));
           Util.option_map v.snapshot_window
             (fun f -> ("snapshot_window", (String.to_json f)));
           Util.option_map v.snapshot_retention_limit
             (fun f -> ("snapshot_retention_limit", (Integer.to_json f)));
           Util.option_map v.auto_minor_version_upgrade
             (fun f -> ("auto_minor_version_upgrade", (Boolean.to_json f)));
           Util.option_map v.notification_topic_arn
             (fun f -> ("notification_topic_arn", (String.to_json f)));
           Util.option_map v.port (fun f -> ("port", (Integer.to_json f)));
           Util.option_map v.preferred_maintenance_window
             (fun f -> ("preferred_maintenance_window", (String.to_json f)));
           Util.option_map v.snapshot_name
             (fun f -> ("snapshot_name", (String.to_json f)));
           Some ("snapshot_arns", (SnapshotArnsList.to_json v.snapshot_arns));
           Some ("tags", (TagList.to_json v.tags));
           Some
             ("security_group_ids",
               (SecurityGroupIdsList.to_json v.security_group_ids));
           Some
             ("cache_security_group_names",
               (CacheSecurityGroupNameList.to_json
                  v.cache_security_group_names));
           Util.option_map v.cache_subnet_group_name
             (fun f -> ("cache_subnet_group_name", (String.to_json f)));
           Util.option_map v.cache_parameter_group_name
             (fun f -> ("cache_parameter_group_name", (String.to_json f)));
           Util.option_map v.engine_version
             (fun f -> ("engine_version", (String.to_json f)));
           Util.option_map v.engine (fun f -> ("engine", (String.to_json f)));
           Util.option_map v.cache_node_type
             (fun f -> ("cache_node_type", (String.to_json f)));
           Some
             ("node_group_configuration",
               (NodeGroupConfigurationList.to_json v.node_group_configuration));
           Util.option_map v.replicas_per_node_group
             (fun f -> ("replicas_per_node_group", (Integer.to_json f)));
           Util.option_map v.num_node_groups
             (fun f -> ("num_node_groups", (Integer.to_json f)));
           Some
             ("preferred_cache_cluster_a_zs",
               (AvailabilityZonesList.to_json v.preferred_cache_cluster_a_zs));
           Util.option_map v.num_cache_clusters
             (fun f -> ("num_cache_clusters", (Integer.to_json f)));
           Util.option_map v.multi_a_z_enabled
             (fun f -> ("multi_a_z_enabled", (Boolean.to_json f)));
           Util.option_map v.automatic_failover_enabled
             (fun f -> ("automatic_failover_enabled", (Boolean.to_json f)));
           Util.option_map v.primary_cluster_id
             (fun f -> ("primary_cluster_id", (String.to_json f)));
           Util.option_map v.global_replication_group_id
             (fun f -> ("global_replication_group_id", (String.to_json f)));
           Some
             ("replication_group_description",
               (String.to_json v.replication_group_description));
           Some
             ("replication_group_id",
               (String.to_json v.replication_group_id))])
    let of_json j =
      {
        replication_group_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "replication_group_id")));
        replication_group_description =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "replication_group_description")));
        global_replication_group_id =
          (Util.option_map (Json.lookup j "global_replication_group_id")
             String.of_json);
        primary_cluster_id =
          (Util.option_map (Json.lookup j "primary_cluster_id")
             String.of_json);
        automatic_failover_enabled =
          (Util.option_map (Json.lookup j "automatic_failover_enabled")
             Boolean.of_json);
        multi_a_z_enabled =
          (Util.option_map (Json.lookup j "multi_a_z_enabled")
             Boolean.of_json);
        num_cache_clusters =
          (Util.option_map (Json.lookup j "num_cache_clusters")
             Integer.of_json);
        preferred_cache_cluster_a_zs =
          (AvailabilityZonesList.of_json
             (Util.of_option_exn
                (Json.lookup j "preferred_cache_cluster_a_zs")));
        num_node_groups =
          (Util.option_map (Json.lookup j "num_node_groups") Integer.of_json);
        replicas_per_node_group =
          (Util.option_map (Json.lookup j "replicas_per_node_group")
             Integer.of_json);
        node_group_configuration =
          (NodeGroupConfigurationList.of_json
             (Util.of_option_exn (Json.lookup j "node_group_configuration")));
        cache_node_type =
          (Util.option_map (Json.lookup j "cache_node_type") String.of_json);
        engine = (Util.option_map (Json.lookup j "engine") String.of_json);
        engine_version =
          (Util.option_map (Json.lookup j "engine_version") String.of_json);
        cache_parameter_group_name =
          (Util.option_map (Json.lookup j "cache_parameter_group_name")
             String.of_json);
        cache_subnet_group_name =
          (Util.option_map (Json.lookup j "cache_subnet_group_name")
             String.of_json);
        cache_security_group_names =
          (CacheSecurityGroupNameList.of_json
             (Util.of_option_exn (Json.lookup j "cache_security_group_names")));
        security_group_ids =
          (SecurityGroupIdsList.of_json
             (Util.of_option_exn (Json.lookup j "security_group_ids")));
        tags = (TagList.of_json (Util.of_option_exn (Json.lookup j "tags")));
        snapshot_arns =
          (SnapshotArnsList.of_json
             (Util.of_option_exn (Json.lookup j "snapshot_arns")));
        snapshot_name =
          (Util.option_map (Json.lookup j "snapshot_name") String.of_json);
        preferred_maintenance_window =
          (Util.option_map (Json.lookup j "preferred_maintenance_window")
             String.of_json);
        port = (Util.option_map (Json.lookup j "port") Integer.of_json);
        notification_topic_arn =
          (Util.option_map (Json.lookup j "notification_topic_arn")
             String.of_json);
        auto_minor_version_upgrade =
          (Util.option_map (Json.lookup j "auto_minor_version_upgrade")
             Boolean.of_json);
        snapshot_retention_limit =
          (Util.option_map (Json.lookup j "snapshot_retention_limit")
             Integer.of_json);
        snapshot_window =
          (Util.option_map (Json.lookup j "snapshot_window") String.of_json);
        auth_token =
          (Util.option_map (Json.lookup j "auth_token") String.of_json);
        transit_encryption_enabled =
          (Util.option_map (Json.lookup j "transit_encryption_enabled")
             Boolean.of_json);
        at_rest_encryption_enabled =
          (Util.option_map (Json.lookup j "at_rest_encryption_enabled")
             Boolean.of_json);
        kms_key_id =
          (Util.option_map (Json.lookup j "kms_key_id") String.of_json);
        user_group_ids =
          (UserGroupIdListInput.of_json
             (Util.of_option_exn (Json.lookup j "user_group_ids")))
      }
  end
module ReplicationGroupNotFoundFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CacheParameterGroupQuotaExceededFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CreateCacheSubnetGroupMessage =
  struct
    type t =
      {
      cache_subnet_group_name: String.t ;
      cache_subnet_group_description: String.t ;
      subnet_ids: SubnetIdentifierList.t }
    let make ~cache_subnet_group_name  ~cache_subnet_group_description 
      ~subnet_ids  () =
      { cache_subnet_group_name; cache_subnet_group_description; subnet_ids }
    let parse xml =
      Some
        {
          cache_subnet_group_name =
            (Xml.required "CacheSubnetGroupName"
               (Util.option_bind (Xml.member "CacheSubnetGroupName" xml)
                  String.parse));
          cache_subnet_group_description =
            (Xml.required "CacheSubnetGroupDescription"
               (Util.option_bind
                  (Xml.member "CacheSubnetGroupDescription" xml) String.parse));
          subnet_ids =
            (Xml.required "SubnetIds"
               (Util.option_bind (Xml.member "SubnetIds" xml)
                  SubnetIdentifierList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("SubnetIds.member",
                   (SubnetIdentifierList.to_query v.subnet_ids)));
           Some
             (Query.Pair
                ("CacheSubnetGroupDescription",
                  (String.to_query v.cache_subnet_group_description)));
           Some
             (Query.Pair
                ("CacheSubnetGroupName",
                  (String.to_query v.cache_subnet_group_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("subnet_ids", (SubnetIdentifierList.to_json v.subnet_ids));
           Some
             ("cache_subnet_group_description",
               (String.to_json v.cache_subnet_group_description));
           Some
             ("cache_subnet_group_name",
               (String.to_json v.cache_subnet_group_name))])
    let of_json j =
      {
        cache_subnet_group_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "cache_subnet_group_name")));
        cache_subnet_group_description =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "cache_subnet_group_description")));
        subnet_ids =
          (SubnetIdentifierList.of_json
             (Util.of_option_exn (Json.lookup j "subnet_ids")))
      }
  end
module DescribeEngineDefaultParametersResult =
  struct
    type t = {
      engine_defaults: EngineDefaults.t }
    let make ~engine_defaults  () = { engine_defaults }
    let parse xml =
      Some
        {
          engine_defaults =
            (Xml.required "EngineDefaults"
               (Util.option_bind (Xml.member "EngineDefaults" xml)
                  EngineDefaults.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("EngineDefaults",
                   (EngineDefaults.to_query v.engine_defaults)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("engine_defaults", (EngineDefaults.to_json v.engine_defaults))])
    let of_json j =
      {
        engine_defaults =
          (EngineDefaults.of_json
             (Util.of_option_exn (Json.lookup j "engine_defaults")))
      }
  end
module InvalidParameterValueException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module TagQuotaPerResourceExceeded =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DeleteSnapshotResult =
  struct
    type t = {
      snapshot: Snapshot.t option }
    let make ?snapshot  () = { snapshot }
    let parse xml =
      Some
        {
          snapshot =
            (Util.option_bind (Xml.member "Snapshot" xml) Snapshot.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.snapshot
              (fun f -> Query.Pair ("Snapshot", (Snapshot.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.snapshot
              (fun f -> ("snapshot", (Snapshot.to_json f)))])
    let of_json j =
      {
        snapshot =
          (Util.option_map (Json.lookup j "snapshot") Snapshot.of_json)
      }
  end
module InvalidKMSKeyFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module InsufficientCacheClusterCapacityFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module InvalidSubnet =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module UserAlreadyExistsFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CreateCacheSecurityGroupMessage =
  struct
    type t = {
      cache_security_group_name: String.t ;
      description: String.t }
    let make ~cache_security_group_name  ~description  () =
      { cache_security_group_name; description }
    let parse xml =
      Some
        {
          cache_security_group_name =
            (Xml.required "CacheSecurityGroupName"
               (Util.option_bind (Xml.member "CacheSecurityGroupName" xml)
                  String.parse));
          description =
            (Xml.required "Description"
               (Util.option_bind (Xml.member "Description" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("Description", (String.to_query v.description)));
           Some
             (Query.Pair
                ("CacheSecurityGroupName",
                  (String.to_query v.cache_security_group_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("description", (String.to_json v.description));
           Some
             ("cache_security_group_name",
               (String.to_json v.cache_security_group_name))])
    let of_json j =
      {
        cache_security_group_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "cache_security_group_name")));
        description =
          (String.of_json (Util.of_option_exn (Json.lookup j "description")))
      }
  end
module AddTagsToResourceMessage =
  struct
    type t = {
      resource_name: String.t ;
      tags: TagList.t }
    let make ~resource_name  ~tags  () = { resource_name; tags }
    let parse xml =
      Some
        {
          resource_name =
            (Xml.required "ResourceName"
               (Util.option_bind (Xml.member "ResourceName" xml) String.parse));
          tags =
            (Xml.required "Tags"
               (Util.option_bind (Xml.member "Tags" xml) TagList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Tags.member", (TagList.to_query v.tags)));
           Some
             (Query.Pair ("ResourceName", (String.to_query v.resource_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tags", (TagList.to_json v.tags));
           Some ("resource_name", (String.to_json v.resource_name))])
    let of_json j =
      {
        resource_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "resource_name")));
        tags = (TagList.of_json (Util.of_option_exn (Json.lookup j "tags")))
      }
  end
module CacheSubnetQuotaExceededFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module BatchStopUpdateActionMessage =
  struct
    type t =
      {
      replication_group_ids: ReplicationGroupIdList.t ;
      cache_cluster_ids: CacheClusterIdList.t ;
      service_update_name: String.t }
    let make ?(replication_group_ids= [])  ?(cache_cluster_ids= []) 
      ~service_update_name  () =
      { replication_group_ids; cache_cluster_ids; service_update_name }
    let parse xml =
      Some
        {
          replication_group_ids =
            (Util.of_option []
               (Util.option_bind (Xml.member "ReplicationGroupIds" xml)
                  ReplicationGroupIdList.parse));
          cache_cluster_ids =
            (Util.of_option []
               (Util.option_bind (Xml.member "CacheClusterIds" xml)
                  CacheClusterIdList.parse));
          service_update_name =
            (Xml.required "ServiceUpdateName"
               (Util.option_bind (Xml.member "ServiceUpdateName" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ServiceUpdateName",
                   (String.to_query v.service_update_name)));
           Some
             (Query.Pair
                ("CacheClusterIds.member",
                  (CacheClusterIdList.to_query v.cache_cluster_ids)));
           Some
             (Query.Pair
                ("ReplicationGroupIds.member",
                  (ReplicationGroupIdList.to_query v.replication_group_ids)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("service_update_name", (String.to_json v.service_update_name));
           Some
             ("cache_cluster_ids",
               (CacheClusterIdList.to_json v.cache_cluster_ids));
           Some
             ("replication_group_ids",
               (ReplicationGroupIdList.to_json v.replication_group_ids))])
    let of_json j =
      {
        replication_group_ids =
          (ReplicationGroupIdList.of_json
             (Util.of_option_exn (Json.lookup j "replication_group_ids")));
        cache_cluster_ids =
          (CacheClusterIdList.of_json
             (Util.of_option_exn (Json.lookup j "cache_cluster_ids")));
        service_update_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "service_update_name")))
      }
  end
module SubnetInUse =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module InvalidVPCNetworkStateFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module AuthorizeCacheSecurityGroupIngressResult =
  struct
    type t = {
      cache_security_group: CacheSecurityGroup.t option }
    let make ?cache_security_group  () = { cache_security_group }
    let parse xml =
      Some
        {
          cache_security_group =
            (Util.option_bind (Xml.member "CacheSecurityGroup" xml)
               CacheSecurityGroup.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.cache_security_group
              (fun f ->
                 Query.Pair
                   ("CacheSecurityGroup", (CacheSecurityGroup.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cache_security_group
              (fun f ->
                 ("cache_security_group", (CacheSecurityGroup.to_json f)))])
    let of_json j =
      {
        cache_security_group =
          (Util.option_map (Json.lookup j "cache_security_group")
             CacheSecurityGroup.of_json)
      }
  end
module InvalidParameterCombinationException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module ModifyUserMessage =
  struct
    type t =
      {
      user_id: String.t ;
      access_string: String.t option ;
      append_access_string: String.t option ;
      passwords: PasswordListInput.t ;
      no_password_required: Boolean.t option }
    let make ~user_id  ?access_string  ?append_access_string  ?(passwords=
      [])  ?no_password_required  () =
      {
        user_id;
        access_string;
        append_access_string;
        passwords;
        no_password_required
      }
    let parse xml =
      Some
        {
          user_id =
            (Xml.required "UserId"
               (Util.option_bind (Xml.member "UserId" xml) String.parse));
          access_string =
            (Util.option_bind (Xml.member "AccessString" xml) String.parse);
          append_access_string =
            (Util.option_bind (Xml.member "AppendAccessString" xml)
               String.parse);
          passwords =
            (Util.of_option []
               (Util.option_bind (Xml.member "Passwords" xml)
                  PasswordListInput.parse));
          no_password_required =
            (Util.option_bind (Xml.member "NoPasswordRequired" xml)
               Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.no_password_required
              (fun f ->
                 Query.Pair ("NoPasswordRequired", (Boolean.to_query f)));
           Some
             (Query.Pair
                ("Passwords.member",
                  (PasswordListInput.to_query v.passwords)));
           Util.option_map v.append_access_string
             (fun f -> Query.Pair ("AppendAccessString", (String.to_query f)));
           Util.option_map v.access_string
             (fun f -> Query.Pair ("AccessString", (String.to_query f)));
           Some (Query.Pair ("UserId", (String.to_query v.user_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.no_password_required
              (fun f -> ("no_password_required", (Boolean.to_json f)));
           Some ("passwords", (PasswordListInput.to_json v.passwords));
           Util.option_map v.append_access_string
             (fun f -> ("append_access_string", (String.to_json f)));
           Util.option_map v.access_string
             (fun f -> ("access_string", (String.to_json f)));
           Some ("user_id", (String.to_json v.user_id))])
    let of_json j =
      {
        user_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "user_id")));
        access_string =
          (Util.option_map (Json.lookup j "access_string") String.of_json);
        append_access_string =
          (Util.option_map (Json.lookup j "append_access_string")
             String.of_json);
        passwords =
          (PasswordListInput.of_json
             (Util.of_option_exn (Json.lookup j "passwords")));
        no_password_required =
          (Util.option_map (Json.lookup j "no_password_required")
             Boolean.of_json)
      }
  end
module CacheClusterNotFoundFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DescribeUserGroupsMessage =
  struct
    type t =
      {
      user_group_id: String.t option ;
      max_records: Integer.t option ;
      marker: String.t option }
    let make ?user_group_id  ?max_records  ?marker  () =
      { user_group_id; max_records; marker }
    let parse xml =
      Some
        {
          user_group_id =
            (Util.option_bind (Xml.member "UserGroupId" xml) String.parse);
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Util.option_map v.user_group_id
             (fun f -> Query.Pair ("UserGroupId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Util.option_map v.user_group_id
             (fun f -> ("user_group_id", (String.to_json f)))])
    let of_json j =
      {
        user_group_id =
          (Util.option_map (Json.lookup j "user_group_id") String.of_json);
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module CacheParameterGroupNotFoundFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module InvalidARNFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module InvalidCacheParameterGroupStateFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DescribeSnapshotsMessage =
  struct
    type t =
      {
      replication_group_id: String.t option ;
      cache_cluster_id: String.t option ;
      snapshot_name: String.t option ;
      snapshot_source: String.t option ;
      marker: String.t option ;
      max_records: Integer.t option ;
      show_node_group_config: Boolean.t option }
    let make ?replication_group_id  ?cache_cluster_id  ?snapshot_name 
      ?snapshot_source  ?marker  ?max_records  ?show_node_group_config  () =
      {
        replication_group_id;
        cache_cluster_id;
        snapshot_name;
        snapshot_source;
        marker;
        max_records;
        show_node_group_config
      }
    let parse xml =
      Some
        {
          replication_group_id =
            (Util.option_bind (Xml.member "ReplicationGroupId" xml)
               String.parse);
          cache_cluster_id =
            (Util.option_bind (Xml.member "CacheClusterId" xml) String.parse);
          snapshot_name =
            (Util.option_bind (Xml.member "SnapshotName" xml) String.parse);
          snapshot_source =
            (Util.option_bind (Xml.member "SnapshotSource" xml) String.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          show_node_group_config =
            (Util.option_bind (Xml.member "ShowNodeGroupConfig" xml)
               Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.show_node_group_config
              (fun f ->
                 Query.Pair ("ShowNodeGroupConfig", (Boolean.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.snapshot_source
             (fun f -> Query.Pair ("SnapshotSource", (String.to_query f)));
           Util.option_map v.snapshot_name
             (fun f -> Query.Pair ("SnapshotName", (String.to_query f)));
           Util.option_map v.cache_cluster_id
             (fun f -> Query.Pair ("CacheClusterId", (String.to_query f)));
           Util.option_map v.replication_group_id
             (fun f -> Query.Pair ("ReplicationGroupId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.show_node_group_config
              (fun f -> ("show_node_group_config", (Boolean.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.snapshot_source
             (fun f -> ("snapshot_source", (String.to_json f)));
           Util.option_map v.snapshot_name
             (fun f -> ("snapshot_name", (String.to_json f)));
           Util.option_map v.cache_cluster_id
             (fun f -> ("cache_cluster_id", (String.to_json f)));
           Util.option_map v.replication_group_id
             (fun f -> ("replication_group_id", (String.to_json f)))])
    let of_json j =
      {
        replication_group_id =
          (Util.option_map (Json.lookup j "replication_group_id")
             String.of_json);
        cache_cluster_id =
          (Util.option_map (Json.lookup j "cache_cluster_id") String.of_json);
        snapshot_name =
          (Util.option_map (Json.lookup j "snapshot_name") String.of_json);
        snapshot_source =
          (Util.option_map (Json.lookup j "snapshot_source") String.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        show_node_group_config =
          (Util.option_map (Json.lookup j "show_node_group_config")
             Boolean.of_json)
      }
  end
module ModifyGlobalReplicationGroupMessage =
  struct
    type t =
      {
      global_replication_group_id: String.t ;
      apply_immediately: Boolean.t ;
      cache_node_type: String.t option ;
      engine_version: String.t option ;
      global_replication_group_description: String.t option ;
      automatic_failover_enabled: Boolean.t option }
    let make ~global_replication_group_id  ~apply_immediately 
      ?cache_node_type  ?engine_version 
      ?global_replication_group_description  ?automatic_failover_enabled  ()
      =
      {
        global_replication_group_id;
        apply_immediately;
        cache_node_type;
        engine_version;
        global_replication_group_description;
        automatic_failover_enabled
      }
    let parse xml =
      Some
        {
          global_replication_group_id =
            (Xml.required "GlobalReplicationGroupId"
               (Util.option_bind (Xml.member "GlobalReplicationGroupId" xml)
                  String.parse));
          apply_immediately =
            (Xml.required "ApplyImmediately"
               (Util.option_bind (Xml.member "ApplyImmediately" xml)
                  Boolean.parse));
          cache_node_type =
            (Util.option_bind (Xml.member "CacheNodeType" xml) String.parse);
          engine_version =
            (Util.option_bind (Xml.member "EngineVersion" xml) String.parse);
          global_replication_group_description =
            (Util.option_bind
               (Xml.member "GlobalReplicationGroupDescription" xml)
               String.parse);
          automatic_failover_enabled =
            (Util.option_bind (Xml.member "AutomaticFailoverEnabled" xml)
               Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.automatic_failover_enabled
              (fun f ->
                 Query.Pair
                   ("AutomaticFailoverEnabled", (Boolean.to_query f)));
           Util.option_map v.global_replication_group_description
             (fun f ->
                Query.Pair
                  ("GlobalReplicationGroupDescription", (String.to_query f)));
           Util.option_map v.engine_version
             (fun f -> Query.Pair ("EngineVersion", (String.to_query f)));
           Util.option_map v.cache_node_type
             (fun f -> Query.Pair ("CacheNodeType", (String.to_query f)));
           Some
             (Query.Pair
                ("ApplyImmediately", (Boolean.to_query v.apply_immediately)));
           Some
             (Query.Pair
                ("GlobalReplicationGroupId",
                  (String.to_query v.global_replication_group_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.automatic_failover_enabled
              (fun f -> ("automatic_failover_enabled", (Boolean.to_json f)));
           Util.option_map v.global_replication_group_description
             (fun f ->
                ("global_replication_group_description", (String.to_json f)));
           Util.option_map v.engine_version
             (fun f -> ("engine_version", (String.to_json f)));
           Util.option_map v.cache_node_type
             (fun f -> ("cache_node_type", (String.to_json f)));
           Some ("apply_immediately", (Boolean.to_json v.apply_immediately));
           Some
             ("global_replication_group_id",
               (String.to_json v.global_replication_group_id))])
    let of_json j =
      {
        global_replication_group_id =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "global_replication_group_id")));
        apply_immediately =
          (Boolean.of_json
             (Util.of_option_exn (Json.lookup j "apply_immediately")));
        cache_node_type =
          (Util.option_map (Json.lookup j "cache_node_type") String.of_json);
        engine_version =
          (Util.option_map (Json.lookup j "engine_version") String.of_json);
        global_replication_group_description =
          (Util.option_map
             (Json.lookup j "global_replication_group_description")
             String.of_json);
        automatic_failover_enabled =
          (Util.option_map (Json.lookup j "automatic_failover_enabled")
             Boolean.of_json)
      }
  end
module InvalidGlobalReplicationGroupStateFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module IncreaseNodeGroupsInGlobalReplicationGroupMessage =
  struct
    type t =
      {
      global_replication_group_id: String.t ;
      node_group_count: Integer.t ;
      regional_configurations: RegionalConfigurationList.t ;
      apply_immediately: Boolean.t }
    let make ~global_replication_group_id  ~node_group_count 
      ?(regional_configurations= [])  ~apply_immediately  () =
      {
        global_replication_group_id;
        node_group_count;
        regional_configurations;
        apply_immediately
      }
    let parse xml =
      Some
        {
          global_replication_group_id =
            (Xml.required "GlobalReplicationGroupId"
               (Util.option_bind (Xml.member "GlobalReplicationGroupId" xml)
                  String.parse));
          node_group_count =
            (Xml.required "NodeGroupCount"
               (Util.option_bind (Xml.member "NodeGroupCount" xml)
                  Integer.parse));
          regional_configurations =
            (Util.of_option []
               (Util.option_bind (Xml.member "RegionalConfigurations" xml)
                  RegionalConfigurationList.parse));
          apply_immediately =
            (Xml.required "ApplyImmediately"
               (Util.option_bind (Xml.member "ApplyImmediately" xml)
                  Boolean.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ApplyImmediately", (Boolean.to_query v.apply_immediately)));
           Some
             (Query.Pair
                ("RegionalConfigurations.member",
                  (RegionalConfigurationList.to_query
                     v.regional_configurations)));
           Some
             (Query.Pair
                ("NodeGroupCount", (Integer.to_query v.node_group_count)));
           Some
             (Query.Pair
                ("GlobalReplicationGroupId",
                  (String.to_query v.global_replication_group_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("apply_immediately", (Boolean.to_json v.apply_immediately));
           Some
             ("regional_configurations",
               (RegionalConfigurationList.to_json v.regional_configurations));
           Some ("node_group_count", (Integer.to_json v.node_group_count));
           Some
             ("global_replication_group_id",
               (String.to_json v.global_replication_group_id))])
    let of_json j =
      {
        global_replication_group_id =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "global_replication_group_id")));
        node_group_count =
          (Integer.of_json
             (Util.of_option_exn (Json.lookup j "node_group_count")));
        regional_configurations =
          (RegionalConfigurationList.of_json
             (Util.of_option_exn (Json.lookup j "regional_configurations")));
        apply_immediately =
          (Boolean.of_json
             (Util.of_option_exn (Json.lookup j "apply_immediately")))
      }
  end
module AllowedNodeTypeModificationsMessage =
  struct
    type t =
      {
      scale_up_modifications: NodeTypeList.t ;
      scale_down_modifications: NodeTypeList.t }
    let make ?(scale_up_modifications= [])  ?(scale_down_modifications= []) 
      () = { scale_up_modifications; scale_down_modifications }
    let parse xml =
      Some
        {
          scale_up_modifications =
            (Util.of_option []
               (Util.option_bind (Xml.member "ScaleUpModifications" xml)
                  NodeTypeList.parse));
          scale_down_modifications =
            (Util.of_option []
               (Util.option_bind (Xml.member "ScaleDownModifications" xml)
                  NodeTypeList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ScaleDownModifications.member",
                   (NodeTypeList.to_query v.scale_down_modifications)));
           Some
             (Query.Pair
                ("ScaleUpModifications.member",
                  (NodeTypeList.to_query v.scale_up_modifications)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("scale_down_modifications",
                (NodeTypeList.to_json v.scale_down_modifications));
           Some
             ("scale_up_modifications",
               (NodeTypeList.to_json v.scale_up_modifications))])
    let of_json j =
      {
        scale_up_modifications =
          (NodeTypeList.of_json
             (Util.of_option_exn (Json.lookup j "scale_up_modifications")));
        scale_down_modifications =
          (NodeTypeList.of_json
             (Util.of_option_exn (Json.lookup j "scale_down_modifications")))
      }
  end
module ReplicationGroupAlreadyExistsFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module SubnetNotAllowedFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module NodeQuotaForClusterExceededFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ModifyCacheClusterResult =
  struct
    type t = {
      cache_cluster: CacheCluster.t option }
    let make ?cache_cluster  () = { cache_cluster }
    let parse xml =
      Some
        {
          cache_cluster =
            (Util.option_bind (Xml.member "CacheCluster" xml)
               CacheCluster.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.cache_cluster
              (fun f ->
                 Query.Pair ("CacheCluster", (CacheCluster.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cache_cluster
              (fun f -> ("cache_cluster", (CacheCluster.to_json f)))])
    let of_json j =
      {
        cache_cluster =
          (Util.option_map (Json.lookup j "cache_cluster")
             CacheCluster.of_json)
      }
  end
module CacheSubnetGroupInUse =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CacheParameterGroupDetails =
  struct
    type t =
      {
      marker: String.t option ;
      parameters: ParametersList.t ;
      cache_node_type_specific_parameters:
        CacheNodeTypeSpecificParametersList.t }
    let make ?marker  ?(parameters= []) 
      ?(cache_node_type_specific_parameters= [])  () =
      { marker; parameters; cache_node_type_specific_parameters }
    let parse xml =
      Some
        {
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          parameters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Parameters" xml)
                  ParametersList.parse));
          cache_node_type_specific_parameters =
            (Util.of_option []
               (Util.option_bind
                  (Xml.member "CacheNodeTypeSpecificParameters" xml)
                  CacheNodeTypeSpecificParametersList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("CacheNodeTypeSpecificParameters.member",
                   (CacheNodeTypeSpecificParametersList.to_query
                      v.cache_node_type_specific_parameters)));
           Some
             (Query.Pair
                ("Parameters.member", (ParametersList.to_query v.parameters)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("cache_node_type_specific_parameters",
                (CacheNodeTypeSpecificParametersList.to_json
                   v.cache_node_type_specific_parameters));
           Some ("parameters", (ParametersList.to_json v.parameters));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)))])
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        parameters =
          (ParametersList.of_json
             (Util.of_option_exn (Json.lookup j "parameters")));
        cache_node_type_specific_parameters =
          (CacheNodeTypeSpecificParametersList.of_json
             (Util.of_option_exn
                (Json.lookup j "cache_node_type_specific_parameters")))
      }
  end
module ModifyReplicationGroupMessage =
  struct
    type t =
      {
      replication_group_id: String.t ;
      replication_group_description: String.t option ;
      primary_cluster_id: String.t option ;
      snapshotting_cluster_id: String.t option ;
      automatic_failover_enabled: Boolean.t option ;
      multi_a_z_enabled: Boolean.t option ;
      node_group_id: String.t option ;
      cache_security_group_names: CacheSecurityGroupNameList.t ;
      security_group_ids: SecurityGroupIdsList.t ;
      preferred_maintenance_window: String.t option ;
      notification_topic_arn: String.t option ;
      cache_parameter_group_name: String.t option ;
      notification_topic_status: String.t option ;
      apply_immediately: Boolean.t option ;
      engine_version: String.t option ;
      auto_minor_version_upgrade: Boolean.t option ;
      snapshot_retention_limit: Integer.t option ;
      snapshot_window: String.t option ;
      cache_node_type: String.t option ;
      auth_token: String.t option ;
      auth_token_update_strategy: AuthTokenUpdateStrategyType.t option ;
      user_group_ids_to_add: UserGroupIdList.t ;
      user_group_ids_to_remove: UserGroupIdList.t ;
      remove_user_groups: Boolean.t option }
    let make ~replication_group_id  ?replication_group_description 
      ?primary_cluster_id  ?snapshotting_cluster_id 
      ?automatic_failover_enabled  ?multi_a_z_enabled  ?node_group_id 
      ?(cache_security_group_names= [])  ?(security_group_ids= []) 
      ?preferred_maintenance_window  ?notification_topic_arn 
      ?cache_parameter_group_name  ?notification_topic_status 
      ?apply_immediately  ?engine_version  ?auto_minor_version_upgrade 
      ?snapshot_retention_limit  ?snapshot_window  ?cache_node_type 
      ?auth_token  ?auth_token_update_strategy  ?(user_group_ids_to_add= []) 
      ?(user_group_ids_to_remove= [])  ?remove_user_groups  () =
      {
        replication_group_id;
        replication_group_description;
        primary_cluster_id;
        snapshotting_cluster_id;
        automatic_failover_enabled;
        multi_a_z_enabled;
        node_group_id;
        cache_security_group_names;
        security_group_ids;
        preferred_maintenance_window;
        notification_topic_arn;
        cache_parameter_group_name;
        notification_topic_status;
        apply_immediately;
        engine_version;
        auto_minor_version_upgrade;
        snapshot_retention_limit;
        snapshot_window;
        cache_node_type;
        auth_token;
        auth_token_update_strategy;
        user_group_ids_to_add;
        user_group_ids_to_remove;
        remove_user_groups
      }
    let parse xml =
      Some
        {
          replication_group_id =
            (Xml.required "ReplicationGroupId"
               (Util.option_bind (Xml.member "ReplicationGroupId" xml)
                  String.parse));
          replication_group_description =
            (Util.option_bind (Xml.member "ReplicationGroupDescription" xml)
               String.parse);
          primary_cluster_id =
            (Util.option_bind (Xml.member "PrimaryClusterId" xml)
               String.parse);
          snapshotting_cluster_id =
            (Util.option_bind (Xml.member "SnapshottingClusterId" xml)
               String.parse);
          automatic_failover_enabled =
            (Util.option_bind (Xml.member "AutomaticFailoverEnabled" xml)
               Boolean.parse);
          multi_a_z_enabled =
            (Util.option_bind (Xml.member "MultiAZEnabled" xml) Boolean.parse);
          node_group_id =
            (Util.option_bind (Xml.member "NodeGroupId" xml) String.parse);
          cache_security_group_names =
            (Util.of_option []
               (Util.option_bind (Xml.member "CacheSecurityGroupNames" xml)
                  CacheSecurityGroupNameList.parse));
          security_group_ids =
            (Util.of_option []
               (Util.option_bind (Xml.member "SecurityGroupIds" xml)
                  SecurityGroupIdsList.parse));
          preferred_maintenance_window =
            (Util.option_bind (Xml.member "PreferredMaintenanceWindow" xml)
               String.parse);
          notification_topic_arn =
            (Util.option_bind (Xml.member "NotificationTopicArn" xml)
               String.parse);
          cache_parameter_group_name =
            (Util.option_bind (Xml.member "CacheParameterGroupName" xml)
               String.parse);
          notification_topic_status =
            (Util.option_bind (Xml.member "NotificationTopicStatus" xml)
               String.parse);
          apply_immediately =
            (Util.option_bind (Xml.member "ApplyImmediately" xml)
               Boolean.parse);
          engine_version =
            (Util.option_bind (Xml.member "EngineVersion" xml) String.parse);
          auto_minor_version_upgrade =
            (Util.option_bind (Xml.member "AutoMinorVersionUpgrade" xml)
               Boolean.parse);
          snapshot_retention_limit =
            (Util.option_bind (Xml.member "SnapshotRetentionLimit" xml)
               Integer.parse);
          snapshot_window =
            (Util.option_bind (Xml.member "SnapshotWindow" xml) String.parse);
          cache_node_type =
            (Util.option_bind (Xml.member "CacheNodeType" xml) String.parse);
          auth_token =
            (Util.option_bind (Xml.member "AuthToken" xml) String.parse);
          auth_token_update_strategy =
            (Util.option_bind (Xml.member "AuthTokenUpdateStrategy" xml)
               AuthTokenUpdateStrategyType.parse);
          user_group_ids_to_add =
            (Util.of_option []
               (Util.option_bind (Xml.member "UserGroupIdsToAdd" xml)
                  UserGroupIdList.parse));
          user_group_ids_to_remove =
            (Util.of_option []
               (Util.option_bind (Xml.member "UserGroupIdsToRemove" xml)
                  UserGroupIdList.parse));
          remove_user_groups =
            (Util.option_bind (Xml.member "RemoveUserGroups" xml)
               Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.remove_user_groups
              (fun f -> Query.Pair ("RemoveUserGroups", (Boolean.to_query f)));
           Some
             (Query.Pair
                ("UserGroupIdsToRemove.member",
                  (UserGroupIdList.to_query v.user_group_ids_to_remove)));
           Some
             (Query.Pair
                ("UserGroupIdsToAdd.member",
                  (UserGroupIdList.to_query v.user_group_ids_to_add)));
           Util.option_map v.auth_token_update_strategy
             (fun f ->
                Query.Pair
                  ("AuthTokenUpdateStrategy",
                    (AuthTokenUpdateStrategyType.to_query f)));
           Util.option_map v.auth_token
             (fun f -> Query.Pair ("AuthToken", (String.to_query f)));
           Util.option_map v.cache_node_type
             (fun f -> Query.Pair ("CacheNodeType", (String.to_query f)));
           Util.option_map v.snapshot_window
             (fun f -> Query.Pair ("SnapshotWindow", (String.to_query f)));
           Util.option_map v.snapshot_retention_limit
             (fun f ->
                Query.Pair ("SnapshotRetentionLimit", (Integer.to_query f)));
           Util.option_map v.auto_minor_version_upgrade
             (fun f ->
                Query.Pair ("AutoMinorVersionUpgrade", (Boolean.to_query f)));
           Util.option_map v.engine_version
             (fun f -> Query.Pair ("EngineVersion", (String.to_query f)));
           Util.option_map v.apply_immediately
             (fun f -> Query.Pair ("ApplyImmediately", (Boolean.to_query f)));
           Util.option_map v.notification_topic_status
             (fun f ->
                Query.Pair ("NotificationTopicStatus", (String.to_query f)));
           Util.option_map v.cache_parameter_group_name
             (fun f ->
                Query.Pair ("CacheParameterGroupName", (String.to_query f)));
           Util.option_map v.notification_topic_arn
             (fun f ->
                Query.Pair ("NotificationTopicArn", (String.to_query f)));
           Util.option_map v.preferred_maintenance_window
             (fun f ->
                Query.Pair
                  ("PreferredMaintenanceWindow", (String.to_query f)));
           Some
             (Query.Pair
                ("SecurityGroupIds.member",
                  (SecurityGroupIdsList.to_query v.security_group_ids)));
           Some
             (Query.Pair
                ("CacheSecurityGroupNames.member",
                  (CacheSecurityGroupNameList.to_query
                     v.cache_security_group_names)));
           Util.option_map v.node_group_id
             (fun f -> Query.Pair ("NodeGroupId", (String.to_query f)));
           Util.option_map v.multi_a_z_enabled
             (fun f -> Query.Pair ("MultiAZEnabled", (Boolean.to_query f)));
           Util.option_map v.automatic_failover_enabled
             (fun f ->
                Query.Pair ("AutomaticFailoverEnabled", (Boolean.to_query f)));
           Util.option_map v.snapshotting_cluster_id
             (fun f ->
                Query.Pair ("SnapshottingClusterId", (String.to_query f)));
           Util.option_map v.primary_cluster_id
             (fun f -> Query.Pair ("PrimaryClusterId", (String.to_query f)));
           Util.option_map v.replication_group_description
             (fun f ->
                Query.Pair
                  ("ReplicationGroupDescription", (String.to_query f)));
           Some
             (Query.Pair
                ("ReplicationGroupId",
                  (String.to_query v.replication_group_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.remove_user_groups
              (fun f -> ("remove_user_groups", (Boolean.to_json f)));
           Some
             ("user_group_ids_to_remove",
               (UserGroupIdList.to_json v.user_group_ids_to_remove));
           Some
             ("user_group_ids_to_add",
               (UserGroupIdList.to_json v.user_group_ids_to_add));
           Util.option_map v.auth_token_update_strategy
             (fun f ->
                ("auth_token_update_strategy",
                  (AuthTokenUpdateStrategyType.to_json f)));
           Util.option_map v.auth_token
             (fun f -> ("auth_token", (String.to_json f)));
           Util.option_map v.cache_node_type
             (fun f -> ("cache_node_type", (String.to_json f)));
           Util.option_map v.snapshot_window
             (fun f -> ("snapshot_window", (String.to_json f)));
           Util.option_map v.snapshot_retention_limit
             (fun f -> ("snapshot_retention_limit", (Integer.to_json f)));
           Util.option_map v.auto_minor_version_upgrade
             (fun f -> ("auto_minor_version_upgrade", (Boolean.to_json f)));
           Util.option_map v.engine_version
             (fun f -> ("engine_version", (String.to_json f)));
           Util.option_map v.apply_immediately
             (fun f -> ("apply_immediately", (Boolean.to_json f)));
           Util.option_map v.notification_topic_status
             (fun f -> ("notification_topic_status", (String.to_json f)));
           Util.option_map v.cache_parameter_group_name
             (fun f -> ("cache_parameter_group_name", (String.to_json f)));
           Util.option_map v.notification_topic_arn
             (fun f -> ("notification_topic_arn", (String.to_json f)));
           Util.option_map v.preferred_maintenance_window
             (fun f -> ("preferred_maintenance_window", (String.to_json f)));
           Some
             ("security_group_ids",
               (SecurityGroupIdsList.to_json v.security_group_ids));
           Some
             ("cache_security_group_names",
               (CacheSecurityGroupNameList.to_json
                  v.cache_security_group_names));
           Util.option_map v.node_group_id
             (fun f -> ("node_group_id", (String.to_json f)));
           Util.option_map v.multi_a_z_enabled
             (fun f -> ("multi_a_z_enabled", (Boolean.to_json f)));
           Util.option_map v.automatic_failover_enabled
             (fun f -> ("automatic_failover_enabled", (Boolean.to_json f)));
           Util.option_map v.snapshotting_cluster_id
             (fun f -> ("snapshotting_cluster_id", (String.to_json f)));
           Util.option_map v.primary_cluster_id
             (fun f -> ("primary_cluster_id", (String.to_json f)));
           Util.option_map v.replication_group_description
             (fun f -> ("replication_group_description", (String.to_json f)));
           Some
             ("replication_group_id",
               (String.to_json v.replication_group_id))])
    let of_json j =
      {
        replication_group_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "replication_group_id")));
        replication_group_description =
          (Util.option_map (Json.lookup j "replication_group_description")
             String.of_json);
        primary_cluster_id =
          (Util.option_map (Json.lookup j "primary_cluster_id")
             String.of_json);
        snapshotting_cluster_id =
          (Util.option_map (Json.lookup j "snapshotting_cluster_id")
             String.of_json);
        automatic_failover_enabled =
          (Util.option_map (Json.lookup j "automatic_failover_enabled")
             Boolean.of_json);
        multi_a_z_enabled =
          (Util.option_map (Json.lookup j "multi_a_z_enabled")
             Boolean.of_json);
        node_group_id =
          (Util.option_map (Json.lookup j "node_group_id") String.of_json);
        cache_security_group_names =
          (CacheSecurityGroupNameList.of_json
             (Util.of_option_exn (Json.lookup j "cache_security_group_names")));
        security_group_ids =
          (SecurityGroupIdsList.of_json
             (Util.of_option_exn (Json.lookup j "security_group_ids")));
        preferred_maintenance_window =
          (Util.option_map (Json.lookup j "preferred_maintenance_window")
             String.of_json);
        notification_topic_arn =
          (Util.option_map (Json.lookup j "notification_topic_arn")
             String.of_json);
        cache_parameter_group_name =
          (Util.option_map (Json.lookup j "cache_parameter_group_name")
             String.of_json);
        notification_topic_status =
          (Util.option_map (Json.lookup j "notification_topic_status")
             String.of_json);
        apply_immediately =
          (Util.option_map (Json.lookup j "apply_immediately")
             Boolean.of_json);
        engine_version =
          (Util.option_map (Json.lookup j "engine_version") String.of_json);
        auto_minor_version_upgrade =
          (Util.option_map (Json.lookup j "auto_minor_version_upgrade")
             Boolean.of_json);
        snapshot_retention_limit =
          (Util.option_map (Json.lookup j "snapshot_retention_limit")
             Integer.of_json);
        snapshot_window =
          (Util.option_map (Json.lookup j "snapshot_window") String.of_json);
        cache_node_type =
          (Util.option_map (Json.lookup j "cache_node_type") String.of_json);
        auth_token =
          (Util.option_map (Json.lookup j "auth_token") String.of_json);
        auth_token_update_strategy =
          (Util.option_map (Json.lookup j "auth_token_update_strategy")
             AuthTokenUpdateStrategyType.of_json);
        user_group_ids_to_add =
          (UserGroupIdList.of_json
             (Util.of_option_exn (Json.lookup j "user_group_ids_to_add")));
        user_group_ids_to_remove =
          (UserGroupIdList.of_json
             (Util.of_option_exn (Json.lookup j "user_group_ids_to_remove")));
        remove_user_groups =
          (Util.option_map (Json.lookup j "remove_user_groups")
             Boolean.of_json)
      }
  end
module RebalanceSlotsInGlobalReplicationGroupResult =
  struct
    type t = {
      global_replication_group: GlobalReplicationGroup.t option }
    let make ?global_replication_group  () = { global_replication_group }
    let parse xml =
      Some
        {
          global_replication_group =
            (Util.option_bind (Xml.member "GlobalReplicationGroup" xml)
               GlobalReplicationGroup.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.global_replication_group
              (fun f ->
                 Query.Pair
                   ("GlobalReplicationGroup",
                     (GlobalReplicationGroup.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.global_replication_group
              (fun f ->
                 ("global_replication_group",
                   (GlobalReplicationGroup.to_json f)))])
    let of_json j =
      {
        global_replication_group =
          (Util.option_map (Json.lookup j "global_replication_group")
             GlobalReplicationGroup.of_json)
      }
  end
module UserGroupQuotaExceededFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DeleteGlobalReplicationGroupMessage =
  struct
    type t =
      {
      global_replication_group_id: String.t ;
      retain_primary_replication_group: Boolean.t }
    let make ~global_replication_group_id  ~retain_primary_replication_group 
      () = { global_replication_group_id; retain_primary_replication_group }
    let parse xml =
      Some
        {
          global_replication_group_id =
            (Xml.required "GlobalReplicationGroupId"
               (Util.option_bind (Xml.member "GlobalReplicationGroupId" xml)
                  String.parse));
          retain_primary_replication_group =
            (Xml.required "RetainPrimaryReplicationGroup"
               (Util.option_bind
                  (Xml.member "RetainPrimaryReplicationGroup" xml)
                  Boolean.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("RetainPrimaryReplicationGroup",
                   (Boolean.to_query v.retain_primary_replication_group)));
           Some
             (Query.Pair
                ("GlobalReplicationGroupId",
                  (String.to_query v.global_replication_group_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("retain_primary_replication_group",
                (Boolean.to_json v.retain_primary_replication_group));
           Some
             ("global_replication_group_id",
               (String.to_json v.global_replication_group_id))])
    let of_json j =
      {
        global_replication_group_id =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "global_replication_group_id")));
        retain_primary_replication_group =
          (Boolean.of_json
             (Util.of_option_exn
                (Json.lookup j "retain_primary_replication_group")))
      }
  end
module TestFailoverResult =
  struct
    type t = {
      replication_group: ReplicationGroup.t option }
    let make ?replication_group  () = { replication_group }
    let parse xml =
      Some
        {
          replication_group =
            (Util.option_bind (Xml.member "ReplicationGroup" xml)
               ReplicationGroup.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.replication_group
              (fun f ->
                 Query.Pair
                   ("ReplicationGroup", (ReplicationGroup.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.replication_group
              (fun f -> ("replication_group", (ReplicationGroup.to_json f)))])
    let of_json j =
      {
        replication_group =
          (Util.option_map (Json.lookup j "replication_group")
             ReplicationGroup.of_json)
      }
  end
module InvalidCacheSecurityGroupStateFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module GlobalReplicationGroupNotFoundFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ModifyReplicationGroupResult =
  struct
    type t = {
      replication_group: ReplicationGroup.t option }
    let make ?replication_group  () = { replication_group }
    let parse xml =
      Some
        {
          replication_group =
            (Util.option_bind (Xml.member "ReplicationGroup" xml)
               ReplicationGroup.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.replication_group
              (fun f ->
                 Query.Pair
                   ("ReplicationGroup", (ReplicationGroup.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.replication_group
              (fun f -> ("replication_group", (ReplicationGroup.to_json f)))])
    let of_json j =
      {
        replication_group =
          (Util.option_map (Json.lookup j "replication_group")
             ReplicationGroup.of_json)
      }
  end
module CacheSecurityGroupMessage =
  struct
    type t =
      {
      marker: String.t option ;
      cache_security_groups: CacheSecurityGroups.t }
    let make ?marker  ?(cache_security_groups= [])  () =
      { marker; cache_security_groups }
    let parse xml =
      Some
        {
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          cache_security_groups =
            (Util.of_option []
               (Util.option_bind (Xml.member "CacheSecurityGroups" xml)
                  CacheSecurityGroups.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("CacheSecurityGroups.member",
                   (CacheSecurityGroups.to_query v.cache_security_groups)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("cache_security_groups",
                (CacheSecurityGroups.to_json v.cache_security_groups));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)))])
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        cache_security_groups =
          (CacheSecurityGroups.of_json
             (Util.of_option_exn (Json.lookup j "cache_security_groups")))
      }
  end
module DescribeUsersMessage =
  struct
    type t =
      {
      engine: String.t option ;
      user_id: String.t option ;
      filters: FilterList.t ;
      max_records: Integer.t option ;
      marker: String.t option }
    let make ?engine  ?user_id  ?(filters= [])  ?max_records  ?marker  () =
      { engine; user_id; filters; max_records; marker }
    let parse xml =
      Some
        {
          engine = (Util.option_bind (Xml.member "Engine" xml) String.parse);
          user_id = (Util.option_bind (Xml.member "UserId" xml) String.parse);
          filters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Filters" xml) FilterList.parse));
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Some
             (Query.Pair ("Filters.member", (FilterList.to_query v.filters)));
           Util.option_map v.user_id
             (fun f -> Query.Pair ("UserId", (String.to_query f)));
           Util.option_map v.engine
             (fun f -> Query.Pair ("Engine", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Some ("filters", (FilterList.to_json v.filters));
           Util.option_map v.user_id
             (fun f -> ("user_id", (String.to_json f)));
           Util.option_map v.engine (fun f -> ("engine", (String.to_json f)))])
    let of_json j =
      {
        engine = (Util.option_map (Json.lookup j "engine") String.of_json);
        user_id = (Util.option_map (Json.lookup j "user_id") String.of_json);
        filters =
          (FilterList.of_json (Util.of_option_exn (Json.lookup j "filters")));
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module DeleteCacheSubnetGroupMessage =
  struct
    type t = {
      cache_subnet_group_name: String.t }
    let make ~cache_subnet_group_name  () = { cache_subnet_group_name }
    let parse xml =
      Some
        {
          cache_subnet_group_name =
            (Xml.required "CacheSubnetGroupName"
               (Util.option_bind (Xml.member "CacheSubnetGroupName" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("CacheSubnetGroupName",
                   (String.to_query v.cache_subnet_group_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("cache_subnet_group_name",
                (String.to_json v.cache_subnet_group_name))])
    let of_json j =
      {
        cache_subnet_group_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "cache_subnet_group_name")))
      }
  end
module RebootCacheClusterResult =
  struct
    type t = {
      cache_cluster: CacheCluster.t option }
    let make ?cache_cluster  () = { cache_cluster }
    let parse xml =
      Some
        {
          cache_cluster =
            (Util.option_bind (Xml.member "CacheCluster" xml)
               CacheCluster.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.cache_cluster
              (fun f ->
                 Query.Pair ("CacheCluster", (CacheCluster.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cache_cluster
              (fun f -> ("cache_cluster", (CacheCluster.to_json f)))])
    let of_json j =
      {
        cache_cluster =
          (Util.option_map (Json.lookup j "cache_cluster")
             CacheCluster.of_json)
      }
  end
module DeleteSnapshotMessage =
  struct
    type t = {
      snapshot_name: String.t }
    let make ~snapshot_name  () = { snapshot_name }
    let parse xml =
      Some
        {
          snapshot_name =
            (Xml.required "SnapshotName"
               (Util.option_bind (Xml.member "SnapshotName" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("SnapshotName", (String.to_query v.snapshot_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("snapshot_name", (String.to_json v.snapshot_name))])
    let of_json j =
      {
        snapshot_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "snapshot_name")))
      }
  end
module CreateCacheSubnetGroupResult =
  struct
    type t = {
      cache_subnet_group: CacheSubnetGroup.t option }
    let make ?cache_subnet_group  () = { cache_subnet_group }
    let parse xml =
      Some
        {
          cache_subnet_group =
            (Util.option_bind (Xml.member "CacheSubnetGroup" xml)
               CacheSubnetGroup.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.cache_subnet_group
              (fun f ->
                 Query.Pair
                   ("CacheSubnetGroup", (CacheSubnetGroup.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cache_subnet_group
              (fun f -> ("cache_subnet_group", (CacheSubnetGroup.to_json f)))])
    let of_json j =
      {
        cache_subnet_group =
          (Util.option_map (Json.lookup j "cache_subnet_group")
             CacheSubnetGroup.of_json)
      }
  end
module ModifyCacheClusterMessage =
  struct
    type t =
      {
      cache_cluster_id: String.t ;
      num_cache_nodes: Integer.t option ;
      cache_node_ids_to_remove: CacheNodeIdsList.t ;
      a_z_mode: AZMode.t option ;
      new_availability_zones: PreferredAvailabilityZoneList.t ;
      cache_security_group_names: CacheSecurityGroupNameList.t ;
      security_group_ids: SecurityGroupIdsList.t ;
      preferred_maintenance_window: String.t option ;
      notification_topic_arn: String.t option ;
      cache_parameter_group_name: String.t option ;
      notification_topic_status: String.t option ;
      apply_immediately: Boolean.t option ;
      engine_version: String.t option ;
      auto_minor_version_upgrade: Boolean.t option ;
      snapshot_retention_limit: Integer.t option ;
      snapshot_window: String.t option ;
      cache_node_type: String.t option ;
      auth_token: String.t option ;
      auth_token_update_strategy: AuthTokenUpdateStrategyType.t option }
    let make ~cache_cluster_id  ?num_cache_nodes  ?(cache_node_ids_to_remove=
      [])  ?a_z_mode  ?(new_availability_zones= []) 
      ?(cache_security_group_names= [])  ?(security_group_ids= []) 
      ?preferred_maintenance_window  ?notification_topic_arn 
      ?cache_parameter_group_name  ?notification_topic_status 
      ?apply_immediately  ?engine_version  ?auto_minor_version_upgrade 
      ?snapshot_retention_limit  ?snapshot_window  ?cache_node_type 
      ?auth_token  ?auth_token_update_strategy  () =
      {
        cache_cluster_id;
        num_cache_nodes;
        cache_node_ids_to_remove;
        a_z_mode;
        new_availability_zones;
        cache_security_group_names;
        security_group_ids;
        preferred_maintenance_window;
        notification_topic_arn;
        cache_parameter_group_name;
        notification_topic_status;
        apply_immediately;
        engine_version;
        auto_minor_version_upgrade;
        snapshot_retention_limit;
        snapshot_window;
        cache_node_type;
        auth_token;
        auth_token_update_strategy
      }
    let parse xml =
      Some
        {
          cache_cluster_id =
            (Xml.required "CacheClusterId"
               (Util.option_bind (Xml.member "CacheClusterId" xml)
                  String.parse));
          num_cache_nodes =
            (Util.option_bind (Xml.member "NumCacheNodes" xml) Integer.parse);
          cache_node_ids_to_remove =
            (Util.of_option []
               (Util.option_bind (Xml.member "CacheNodeIdsToRemove" xml)
                  CacheNodeIdsList.parse));
          a_z_mode =
            (Util.option_bind (Xml.member "AZMode" xml) AZMode.parse);
          new_availability_zones =
            (Util.of_option []
               (Util.option_bind (Xml.member "NewAvailabilityZones" xml)
                  PreferredAvailabilityZoneList.parse));
          cache_security_group_names =
            (Util.of_option []
               (Util.option_bind (Xml.member "CacheSecurityGroupNames" xml)
                  CacheSecurityGroupNameList.parse));
          security_group_ids =
            (Util.of_option []
               (Util.option_bind (Xml.member "SecurityGroupIds" xml)
                  SecurityGroupIdsList.parse));
          preferred_maintenance_window =
            (Util.option_bind (Xml.member "PreferredMaintenanceWindow" xml)
               String.parse);
          notification_topic_arn =
            (Util.option_bind (Xml.member "NotificationTopicArn" xml)
               String.parse);
          cache_parameter_group_name =
            (Util.option_bind (Xml.member "CacheParameterGroupName" xml)
               String.parse);
          notification_topic_status =
            (Util.option_bind (Xml.member "NotificationTopicStatus" xml)
               String.parse);
          apply_immediately =
            (Util.option_bind (Xml.member "ApplyImmediately" xml)
               Boolean.parse);
          engine_version =
            (Util.option_bind (Xml.member "EngineVersion" xml) String.parse);
          auto_minor_version_upgrade =
            (Util.option_bind (Xml.member "AutoMinorVersionUpgrade" xml)
               Boolean.parse);
          snapshot_retention_limit =
            (Util.option_bind (Xml.member "SnapshotRetentionLimit" xml)
               Integer.parse);
          snapshot_window =
            (Util.option_bind (Xml.member "SnapshotWindow" xml) String.parse);
          cache_node_type =
            (Util.option_bind (Xml.member "CacheNodeType" xml) String.parse);
          auth_token =
            (Util.option_bind (Xml.member "AuthToken" xml) String.parse);
          auth_token_update_strategy =
            (Util.option_bind (Xml.member "AuthTokenUpdateStrategy" xml)
               AuthTokenUpdateStrategyType.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.auth_token_update_strategy
              (fun f ->
                 Query.Pair
                   ("AuthTokenUpdateStrategy",
                     (AuthTokenUpdateStrategyType.to_query f)));
           Util.option_map v.auth_token
             (fun f -> Query.Pair ("AuthToken", (String.to_query f)));
           Util.option_map v.cache_node_type
             (fun f -> Query.Pair ("CacheNodeType", (String.to_query f)));
           Util.option_map v.snapshot_window
             (fun f -> Query.Pair ("SnapshotWindow", (String.to_query f)));
           Util.option_map v.snapshot_retention_limit
             (fun f ->
                Query.Pair ("SnapshotRetentionLimit", (Integer.to_query f)));
           Util.option_map v.auto_minor_version_upgrade
             (fun f ->
                Query.Pair ("AutoMinorVersionUpgrade", (Boolean.to_query f)));
           Util.option_map v.engine_version
             (fun f -> Query.Pair ("EngineVersion", (String.to_query f)));
           Util.option_map v.apply_immediately
             (fun f -> Query.Pair ("ApplyImmediately", (Boolean.to_query f)));
           Util.option_map v.notification_topic_status
             (fun f ->
                Query.Pair ("NotificationTopicStatus", (String.to_query f)));
           Util.option_map v.cache_parameter_group_name
             (fun f ->
                Query.Pair ("CacheParameterGroupName", (String.to_query f)));
           Util.option_map v.notification_topic_arn
             (fun f ->
                Query.Pair ("NotificationTopicArn", (String.to_query f)));
           Util.option_map v.preferred_maintenance_window
             (fun f ->
                Query.Pair
                  ("PreferredMaintenanceWindow", (String.to_query f)));
           Some
             (Query.Pair
                ("SecurityGroupIds.member",
                  (SecurityGroupIdsList.to_query v.security_group_ids)));
           Some
             (Query.Pair
                ("CacheSecurityGroupNames.member",
                  (CacheSecurityGroupNameList.to_query
                     v.cache_security_group_names)));
           Some
             (Query.Pair
                ("NewAvailabilityZones.member",
                  (PreferredAvailabilityZoneList.to_query
                     v.new_availability_zones)));
           Util.option_map v.a_z_mode
             (fun f -> Query.Pair ("AZMode", (AZMode.to_query f)));
           Some
             (Query.Pair
                ("CacheNodeIdsToRemove.member",
                  (CacheNodeIdsList.to_query v.cache_node_ids_to_remove)));
           Util.option_map v.num_cache_nodes
             (fun f -> Query.Pair ("NumCacheNodes", (Integer.to_query f)));
           Some
             (Query.Pair
                ("CacheClusterId", (String.to_query v.cache_cluster_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.auth_token_update_strategy
              (fun f ->
                 ("auth_token_update_strategy",
                   (AuthTokenUpdateStrategyType.to_json f)));
           Util.option_map v.auth_token
             (fun f -> ("auth_token", (String.to_json f)));
           Util.option_map v.cache_node_type
             (fun f -> ("cache_node_type", (String.to_json f)));
           Util.option_map v.snapshot_window
             (fun f -> ("snapshot_window", (String.to_json f)));
           Util.option_map v.snapshot_retention_limit
             (fun f -> ("snapshot_retention_limit", (Integer.to_json f)));
           Util.option_map v.auto_minor_version_upgrade
             (fun f -> ("auto_minor_version_upgrade", (Boolean.to_json f)));
           Util.option_map v.engine_version
             (fun f -> ("engine_version", (String.to_json f)));
           Util.option_map v.apply_immediately
             (fun f -> ("apply_immediately", (Boolean.to_json f)));
           Util.option_map v.notification_topic_status
             (fun f -> ("notification_topic_status", (String.to_json f)));
           Util.option_map v.cache_parameter_group_name
             (fun f -> ("cache_parameter_group_name", (String.to_json f)));
           Util.option_map v.notification_topic_arn
             (fun f -> ("notification_topic_arn", (String.to_json f)));
           Util.option_map v.preferred_maintenance_window
             (fun f -> ("preferred_maintenance_window", (String.to_json f)));
           Some
             ("security_group_ids",
               (SecurityGroupIdsList.to_json v.security_group_ids));
           Some
             ("cache_security_group_names",
               (CacheSecurityGroupNameList.to_json
                  v.cache_security_group_names));
           Some
             ("new_availability_zones",
               (PreferredAvailabilityZoneList.to_json
                  v.new_availability_zones));
           Util.option_map v.a_z_mode
             (fun f -> ("a_z_mode", (AZMode.to_json f)));
           Some
             ("cache_node_ids_to_remove",
               (CacheNodeIdsList.to_json v.cache_node_ids_to_remove));
           Util.option_map v.num_cache_nodes
             (fun f -> ("num_cache_nodes", (Integer.to_json f)));
           Some ("cache_cluster_id", (String.to_json v.cache_cluster_id))])
    let of_json j =
      {
        cache_cluster_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "cache_cluster_id")));
        num_cache_nodes =
          (Util.option_map (Json.lookup j "num_cache_nodes") Integer.of_json);
        cache_node_ids_to_remove =
          (CacheNodeIdsList.of_json
             (Util.of_option_exn (Json.lookup j "cache_node_ids_to_remove")));
        a_z_mode =
          (Util.option_map (Json.lookup j "a_z_mode") AZMode.of_json);
        new_availability_zones =
          (PreferredAvailabilityZoneList.of_json
             (Util.of_option_exn (Json.lookup j "new_availability_zones")));
        cache_security_group_names =
          (CacheSecurityGroupNameList.of_json
             (Util.of_option_exn (Json.lookup j "cache_security_group_names")));
        security_group_ids =
          (SecurityGroupIdsList.of_json
             (Util.of_option_exn (Json.lookup j "security_group_ids")));
        preferred_maintenance_window =
          (Util.option_map (Json.lookup j "preferred_maintenance_window")
             String.of_json);
        notification_topic_arn =
          (Util.option_map (Json.lookup j "notification_topic_arn")
             String.of_json);
        cache_parameter_group_name =
          (Util.option_map (Json.lookup j "cache_parameter_group_name")
             String.of_json);
        notification_topic_status =
          (Util.option_map (Json.lookup j "notification_topic_status")
             String.of_json);
        apply_immediately =
          (Util.option_map (Json.lookup j "apply_immediately")
             Boolean.of_json);
        engine_version =
          (Util.option_map (Json.lookup j "engine_version") String.of_json);
        auto_minor_version_upgrade =
          (Util.option_map (Json.lookup j "auto_minor_version_upgrade")
             Boolean.of_json);
        snapshot_retention_limit =
          (Util.option_map (Json.lookup j "snapshot_retention_limit")
             Integer.of_json);
        snapshot_window =
          (Util.option_map (Json.lookup j "snapshot_window") String.of_json);
        cache_node_type =
          (Util.option_map (Json.lookup j "cache_node_type") String.of_json);
        auth_token =
          (Util.option_map (Json.lookup j "auth_token") String.of_json);
        auth_token_update_strategy =
          (Util.option_map (Json.lookup j "auth_token_update_strategy")
             AuthTokenUpdateStrategyType.of_json)
      }
  end
module CacheParameterGroupNameMessage =
  struct
    type t = {
      cache_parameter_group_name: String.t option }
    let make ?cache_parameter_group_name  () = { cache_parameter_group_name }
    let parse xml =
      Some
        {
          cache_parameter_group_name =
            (Util.option_bind (Xml.member "CacheParameterGroupName" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.cache_parameter_group_name
              (fun f ->
                 Query.Pair ("CacheParameterGroupName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cache_parameter_group_name
              (fun f -> ("cache_parameter_group_name", (String.to_json f)))])
    let of_json j =
      {
        cache_parameter_group_name =
          (Util.option_map (Json.lookup j "cache_parameter_group_name")
             String.of_json)
      }
  end
module DescribeCacheParameterGroupsMessage =
  struct
    type t =
      {
      cache_parameter_group_name: String.t option ;
      max_records: Integer.t option ;
      marker: String.t option }
    let make ?cache_parameter_group_name  ?max_records  ?marker  () =
      { cache_parameter_group_name; max_records; marker }
    let parse xml =
      Some
        {
          cache_parameter_group_name =
            (Util.option_bind (Xml.member "CacheParameterGroupName" xml)
               String.parse);
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Util.option_map v.cache_parameter_group_name
             (fun f ->
                Query.Pair ("CacheParameterGroupName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Util.option_map v.cache_parameter_group_name
             (fun f -> ("cache_parameter_group_name", (String.to_json f)))])
    let of_json j =
      {
        cache_parameter_group_name =
          (Util.option_map (Json.lookup j "cache_parameter_group_name")
             String.of_json);
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module StartMigrationMessage =
  struct
    type t =
      {
      replication_group_id: String.t ;
      customer_node_endpoint_list: CustomerNodeEndpointList.t }
    let make ~replication_group_id  ~customer_node_endpoint_list  () =
      { replication_group_id; customer_node_endpoint_list }
    let parse xml =
      Some
        {
          replication_group_id =
            (Xml.required "ReplicationGroupId"
               (Util.option_bind (Xml.member "ReplicationGroupId" xml)
                  String.parse));
          customer_node_endpoint_list =
            (Xml.required "CustomerNodeEndpointList"
               (Util.option_bind (Xml.member "CustomerNodeEndpointList" xml)
                  CustomerNodeEndpointList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("CustomerNodeEndpointList.member",
                   (CustomerNodeEndpointList.to_query
                      v.customer_node_endpoint_list)));
           Some
             (Query.Pair
                ("ReplicationGroupId",
                  (String.to_query v.replication_group_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("customer_node_endpoint_list",
                (CustomerNodeEndpointList.to_json
                   v.customer_node_endpoint_list));
           Some
             ("replication_group_id",
               (String.to_json v.replication_group_id))])
    let of_json j =
      {
        replication_group_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "replication_group_id")));
        customer_node_endpoint_list =
          (CustomerNodeEndpointList.of_json
             (Util.of_option_exn
                (Json.lookup j "customer_node_endpoint_list")))
      }
  end
module CreateCacheSecurityGroupResult =
  struct
    type t = {
      cache_security_group: CacheSecurityGroup.t option }
    let make ?cache_security_group  () = { cache_security_group }
    let parse xml =
      Some
        {
          cache_security_group =
            (Util.option_bind (Xml.member "CacheSecurityGroup" xml)
               CacheSecurityGroup.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.cache_security_group
              (fun f ->
                 Query.Pair
                   ("CacheSecurityGroup", (CacheSecurityGroup.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cache_security_group
              (fun f ->
                 ("cache_security_group", (CacheSecurityGroup.to_json f)))])
    let of_json j =
      {
        cache_security_group =
          (Util.option_map (Json.lookup j "cache_security_group")
             CacheSecurityGroup.of_json)
      }
  end
module CreateGlobalReplicationGroupMessage =
  struct
    type t =
      {
      global_replication_group_id_suffix: String.t ;
      global_replication_group_description: String.t option ;
      primary_replication_group_id: String.t }
    let make ~global_replication_group_id_suffix 
      ?global_replication_group_description  ~primary_replication_group_id 
      () =
      {
        global_replication_group_id_suffix;
        global_replication_group_description;
        primary_replication_group_id
      }
    let parse xml =
      Some
        {
          global_replication_group_id_suffix =
            (Xml.required "GlobalReplicationGroupIdSuffix"
               (Util.option_bind
                  (Xml.member "GlobalReplicationGroupIdSuffix" xml)
                  String.parse));
          global_replication_group_description =
            (Util.option_bind
               (Xml.member "GlobalReplicationGroupDescription" xml)
               String.parse);
          primary_replication_group_id =
            (Xml.required "PrimaryReplicationGroupId"
               (Util.option_bind (Xml.member "PrimaryReplicationGroupId" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("PrimaryReplicationGroupId",
                   (String.to_query v.primary_replication_group_id)));
           Util.option_map v.global_replication_group_description
             (fun f ->
                Query.Pair
                  ("GlobalReplicationGroupDescription", (String.to_query f)));
           Some
             (Query.Pair
                ("GlobalReplicationGroupIdSuffix",
                  (String.to_query v.global_replication_group_id_suffix)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("primary_replication_group_id",
                (String.to_json v.primary_replication_group_id));
           Util.option_map v.global_replication_group_description
             (fun f ->
                ("global_replication_group_description", (String.to_json f)));
           Some
             ("global_replication_group_id_suffix",
               (String.to_json v.global_replication_group_id_suffix))])
    let of_json j =
      {
        global_replication_group_id_suffix =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "global_replication_group_id_suffix")));
        global_replication_group_description =
          (Util.option_map
             (Json.lookup j "global_replication_group_description")
             String.of_json);
        primary_replication_group_id =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "primary_replication_group_id")))
      }
  end
module ReservedCacheNodesOfferingMessage =
  struct
    type t =
      {
      marker: String.t option ;
      reserved_cache_nodes_offerings: ReservedCacheNodesOfferingList.t }
    let make ?marker  ?(reserved_cache_nodes_offerings= [])  () =
      { marker; reserved_cache_nodes_offerings }
    let parse xml =
      Some
        {
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          reserved_cache_nodes_offerings =
            (Util.of_option []
               (Util.option_bind
                  (Xml.member "ReservedCacheNodesOfferings" xml)
                  ReservedCacheNodesOfferingList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ReservedCacheNodesOfferings.member",
                   (ReservedCacheNodesOfferingList.to_query
                      v.reserved_cache_nodes_offerings)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("reserved_cache_nodes_offerings",
                (ReservedCacheNodesOfferingList.to_json
                   v.reserved_cache_nodes_offerings));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)))])
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        reserved_cache_nodes_offerings =
          (ReservedCacheNodesOfferingList.of_json
             (Util.of_option_exn
                (Json.lookup j "reserved_cache_nodes_offerings")))
      }
  end
module DeleteCacheClusterMessage =
  struct
    type t =
      {
      cache_cluster_id: String.t ;
      final_snapshot_identifier: String.t option }
    let make ~cache_cluster_id  ?final_snapshot_identifier  () =
      { cache_cluster_id; final_snapshot_identifier }
    let parse xml =
      Some
        {
          cache_cluster_id =
            (Xml.required "CacheClusterId"
               (Util.option_bind (Xml.member "CacheClusterId" xml)
                  String.parse));
          final_snapshot_identifier =
            (Util.option_bind (Xml.member "FinalSnapshotIdentifier" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.final_snapshot_identifier
              (fun f ->
                 Query.Pair ("FinalSnapshotIdentifier", (String.to_query f)));
           Some
             (Query.Pair
                ("CacheClusterId", (String.to_query v.cache_cluster_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.final_snapshot_identifier
              (fun f -> ("final_snapshot_identifier", (String.to_json f)));
           Some ("cache_cluster_id", (String.to_json v.cache_cluster_id))])
    let of_json j =
      {
        cache_cluster_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "cache_cluster_id")));
        final_snapshot_identifier =
          (Util.option_map (Json.lookup j "final_snapshot_identifier")
             String.of_json)
      }
  end
module DeleteReplicationGroupResult =
  struct
    type t = {
      replication_group: ReplicationGroup.t option }
    let make ?replication_group  () = { replication_group }
    let parse xml =
      Some
        {
          replication_group =
            (Util.option_bind (Xml.member "ReplicationGroup" xml)
               ReplicationGroup.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.replication_group
              (fun f ->
                 Query.Pair
                   ("ReplicationGroup", (ReplicationGroup.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.replication_group
              (fun f -> ("replication_group", (ReplicationGroup.to_json f)))])
    let of_json j =
      {
        replication_group =
          (Util.option_map (Json.lookup j "replication_group")
             ReplicationGroup.of_json)
      }
  end
module ServiceUpdateNotFoundFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DescribeSnapshotsListMessage =
  struct
    type t = {
      marker: String.t option ;
      snapshots: SnapshotList.t }
    let make ?marker  ?(snapshots= [])  () = { marker; snapshots }
    let parse xml =
      Some
        {
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          snapshots =
            (Util.of_option []
               (Util.option_bind (Xml.member "Snapshots" xml)
                  SnapshotList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Snapshots.member", (SnapshotList.to_query v.snapshots)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("snapshots", (SnapshotList.to_json v.snapshots));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)))])
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        snapshots =
          (SnapshotList.of_json
             (Util.of_option_exn (Json.lookup j "snapshots")))
      }
  end
module DescribeServiceUpdatesMessage =
  struct
    type t =
      {
      service_update_name: String.t option ;
      service_update_status: ServiceUpdateStatusList.t ;
      max_records: Integer.t option ;
      marker: String.t option }
    let make ?service_update_name  ?(service_update_status= [])  ?max_records
       ?marker  () =
      { service_update_name; service_update_status; max_records; marker }
    let parse xml =
      Some
        {
          service_update_name =
            (Util.option_bind (Xml.member "ServiceUpdateName" xml)
               String.parse);
          service_update_status =
            (Util.of_option []
               (Util.option_bind (Xml.member "ServiceUpdateStatus" xml)
                  ServiceUpdateStatusList.parse));
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Some
             (Query.Pair
                ("ServiceUpdateStatus.member",
                  (ServiceUpdateStatusList.to_query v.service_update_status)));
           Util.option_map v.service_update_name
             (fun f -> Query.Pair ("ServiceUpdateName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Some
             ("service_update_status",
               (ServiceUpdateStatusList.to_json v.service_update_status));
           Util.option_map v.service_update_name
             (fun f -> ("service_update_name", (String.to_json f)))])
    let of_json j =
      {
        service_update_name =
          (Util.option_map (Json.lookup j "service_update_name")
             String.of_json);
        service_update_status =
          (ServiceUpdateStatusList.of_json
             (Util.of_option_exn (Json.lookup j "service_update_status")));
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module FailoverGlobalReplicationGroupMessage =
  struct
    type t =
      {
      global_replication_group_id: String.t ;
      primary_region: String.t ;
      primary_replication_group_id: String.t }
    let make ~global_replication_group_id  ~primary_region 
      ~primary_replication_group_id  () =
      {
        global_replication_group_id;
        primary_region;
        primary_replication_group_id
      }
    let parse xml =
      Some
        {
          global_replication_group_id =
            (Xml.required "GlobalReplicationGroupId"
               (Util.option_bind (Xml.member "GlobalReplicationGroupId" xml)
                  String.parse));
          primary_region =
            (Xml.required "PrimaryRegion"
               (Util.option_bind (Xml.member "PrimaryRegion" xml)
                  String.parse));
          primary_replication_group_id =
            (Xml.required "PrimaryReplicationGroupId"
               (Util.option_bind (Xml.member "PrimaryReplicationGroupId" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("PrimaryReplicationGroupId",
                   (String.to_query v.primary_replication_group_id)));
           Some
             (Query.Pair
                ("PrimaryRegion", (String.to_query v.primary_region)));
           Some
             (Query.Pair
                ("GlobalReplicationGroupId",
                  (String.to_query v.global_replication_group_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("primary_replication_group_id",
                (String.to_json v.primary_replication_group_id));
           Some ("primary_region", (String.to_json v.primary_region));
           Some
             ("global_replication_group_id",
               (String.to_json v.global_replication_group_id))])
    let of_json j =
      {
        global_replication_group_id =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "global_replication_group_id")));
        primary_region =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "primary_region")));
        primary_replication_group_id =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "primary_replication_group_id")))
      }
  end
module SnapshotNotFoundFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DescribeReplicationGroupsMessage =
  struct
    type t =
      {
      replication_group_id: String.t option ;
      max_records: Integer.t option ;
      marker: String.t option }
    let make ?replication_group_id  ?max_records  ?marker  () =
      { replication_group_id; max_records; marker }
    let parse xml =
      Some
        {
          replication_group_id =
            (Util.option_bind (Xml.member "ReplicationGroupId" xml)
               String.parse);
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Util.option_map v.replication_group_id
             (fun f -> Query.Pair ("ReplicationGroupId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Util.option_map v.replication_group_id
             (fun f -> ("replication_group_id", (String.to_json f)))])
    let of_json j =
      {
        replication_group_id =
          (Util.option_map (Json.lookup j "replication_group_id")
             String.of_json);
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module ReservedCacheNodesOfferingNotFoundFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DescribeGlobalReplicationGroupsResult =
  struct
    type t =
      {
      marker: String.t option ;
      global_replication_groups: GlobalReplicationGroupList.t }
    let make ?marker  ?(global_replication_groups= [])  () =
      { marker; global_replication_groups }
    let parse xml =
      Some
        {
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          global_replication_groups =
            (Util.of_option []
               (Util.option_bind (Xml.member "GlobalReplicationGroups" xml)
                  GlobalReplicationGroupList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("GlobalReplicationGroups.member",
                   (GlobalReplicationGroupList.to_query
                      v.global_replication_groups)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("global_replication_groups",
                (GlobalReplicationGroupList.to_json
                   v.global_replication_groups));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)))])
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        global_replication_groups =
          (GlobalReplicationGroupList.of_json
             (Util.of_option_exn (Json.lookup j "global_replication_groups")))
      }
  end
module DecreaseNodeGroupsInGlobalReplicationGroupResult =
  struct
    type t = {
      global_replication_group: GlobalReplicationGroup.t option }
    let make ?global_replication_group  () = { global_replication_group }
    let parse xml =
      Some
        {
          global_replication_group =
            (Util.option_bind (Xml.member "GlobalReplicationGroup" xml)
               GlobalReplicationGroup.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.global_replication_group
              (fun f ->
                 Query.Pair
                   ("GlobalReplicationGroup",
                     (GlobalReplicationGroup.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.global_replication_group
              (fun f ->
                 ("global_replication_group",
                   (GlobalReplicationGroup.to_json f)))])
    let of_json j =
      {
        global_replication_group =
          (Util.option_map (Json.lookup j "global_replication_group")
             GlobalReplicationGroup.of_json)
      }
  end
module ServiceUpdatesMessage =
  struct
    type t = {
      marker: String.t option ;
      service_updates: ServiceUpdateList.t }
    let make ?marker  ?(service_updates= [])  () =
      { marker; service_updates }
    let parse xml =
      Some
        {
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          service_updates =
            (Util.of_option []
               (Util.option_bind (Xml.member "ServiceUpdates" xml)
                  ServiceUpdateList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ServiceUpdates.member",
                   (ServiceUpdateList.to_query v.service_updates)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("service_updates",
                (ServiceUpdateList.to_json v.service_updates));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)))])
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        service_updates =
          (ServiceUpdateList.of_json
             (Util.of_option_exn (Json.lookup j "service_updates")))
      }
  end
module ReservedCacheNodeAlreadyExistsFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module NodeGroupNotFoundFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module RebalanceSlotsInGlobalReplicationGroupMessage =
  struct
    type t =
      {
      global_replication_group_id: String.t ;
      apply_immediately: Boolean.t }
    let make ~global_replication_group_id  ~apply_immediately  () =
      { global_replication_group_id; apply_immediately }
    let parse xml =
      Some
        {
          global_replication_group_id =
            (Xml.required "GlobalReplicationGroupId"
               (Util.option_bind (Xml.member "GlobalReplicationGroupId" xml)
                  String.parse));
          apply_immediately =
            (Xml.required "ApplyImmediately"
               (Util.option_bind (Xml.member "ApplyImmediately" xml)
                  Boolean.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ApplyImmediately", (Boolean.to_query v.apply_immediately)));
           Some
             (Query.Pair
                ("GlobalReplicationGroupId",
                  (String.to_query v.global_replication_group_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("apply_immediately", (Boolean.to_json v.apply_immediately));
           Some
             ("global_replication_group_id",
               (String.to_json v.global_replication_group_id))])
    let of_json j =
      {
        global_replication_group_id =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "global_replication_group_id")));
        apply_immediately =
          (Boolean.of_json
             (Util.of_option_exn (Json.lookup j "apply_immediately")))
      }
  end
module AuthorizeCacheSecurityGroupIngressMessage =
  struct
    type t =
      {
      cache_security_group_name: String.t ;
      e_c2_security_group_name: String.t ;
      e_c2_security_group_owner_id: String.t }
    let make ~cache_security_group_name  ~e_c2_security_group_name 
      ~e_c2_security_group_owner_id  () =
      {
        cache_security_group_name;
        e_c2_security_group_name;
        e_c2_security_group_owner_id
      }
    let parse xml =
      Some
        {
          cache_security_group_name =
            (Xml.required "CacheSecurityGroupName"
               (Util.option_bind (Xml.member "CacheSecurityGroupName" xml)
                  String.parse));
          e_c2_security_group_name =
            (Xml.required "EC2SecurityGroupName"
               (Util.option_bind (Xml.member "EC2SecurityGroupName" xml)
                  String.parse));
          e_c2_security_group_owner_id =
            (Xml.required "EC2SecurityGroupOwnerId"
               (Util.option_bind (Xml.member "EC2SecurityGroupOwnerId" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("EC2SecurityGroupOwnerId",
                   (String.to_query v.e_c2_security_group_owner_id)));
           Some
             (Query.Pair
                ("EC2SecurityGroupName",
                  (String.to_query v.e_c2_security_group_name)));
           Some
             (Query.Pair
                ("CacheSecurityGroupName",
                  (String.to_query v.cache_security_group_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("e_c2_security_group_owner_id",
                (String.to_json v.e_c2_security_group_owner_id));
           Some
             ("e_c2_security_group_name",
               (String.to_json v.e_c2_security_group_name));
           Some
             ("cache_security_group_name",
               (String.to_json v.cache_security_group_name))])
    let of_json j =
      {
        cache_security_group_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "cache_security_group_name")));
        e_c2_security_group_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "e_c2_security_group_name")));
        e_c2_security_group_owner_id =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "e_c2_security_group_owner_id")))
      }
  end
module CompleteMigrationResponse =
  struct
    type t = {
      replication_group: ReplicationGroup.t option }
    let make ?replication_group  () = { replication_group }
    let parse xml =
      Some
        {
          replication_group =
            (Util.option_bind (Xml.member "ReplicationGroup" xml)
               ReplicationGroup.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.replication_group
              (fun f ->
                 Query.Pair
                   ("ReplicationGroup", (ReplicationGroup.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.replication_group
              (fun f -> ("replication_group", (ReplicationGroup.to_json f)))])
    let of_json j =
      {
        replication_group =
          (Util.option_map (Json.lookup j "replication_group")
             ReplicationGroup.of_json)
      }
  end
module DescribeReservedCacheNodesOfferingsMessage =
  struct
    type t =
      {
      reserved_cache_nodes_offering_id: String.t option ;
      cache_node_type: String.t option ;
      duration: String.t option ;
      product_description: String.t option ;
      offering_type: String.t option ;
      max_records: Integer.t option ;
      marker: String.t option }
    let make ?reserved_cache_nodes_offering_id  ?cache_node_type  ?duration 
      ?product_description  ?offering_type  ?max_records  ?marker  () =
      {
        reserved_cache_nodes_offering_id;
        cache_node_type;
        duration;
        product_description;
        offering_type;
        max_records;
        marker
      }
    let parse xml =
      Some
        {
          reserved_cache_nodes_offering_id =
            (Util.option_bind (Xml.member "ReservedCacheNodesOfferingId" xml)
               String.parse);
          cache_node_type =
            (Util.option_bind (Xml.member "CacheNodeType" xml) String.parse);
          duration =
            (Util.option_bind (Xml.member "Duration" xml) String.parse);
          product_description =
            (Util.option_bind (Xml.member "ProductDescription" xml)
               String.parse);
          offering_type =
            (Util.option_bind (Xml.member "OfferingType" xml) String.parse);
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Util.option_map v.offering_type
             (fun f -> Query.Pair ("OfferingType", (String.to_query f)));
           Util.option_map v.product_description
             (fun f -> Query.Pair ("ProductDescription", (String.to_query f)));
           Util.option_map v.duration
             (fun f -> Query.Pair ("Duration", (String.to_query f)));
           Util.option_map v.cache_node_type
             (fun f -> Query.Pair ("CacheNodeType", (String.to_query f)));
           Util.option_map v.reserved_cache_nodes_offering_id
             (fun f ->
                Query.Pair
                  ("ReservedCacheNodesOfferingId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Util.option_map v.offering_type
             (fun f -> ("offering_type", (String.to_json f)));
           Util.option_map v.product_description
             (fun f -> ("product_description", (String.to_json f)));
           Util.option_map v.duration
             (fun f -> ("duration", (String.to_json f)));
           Util.option_map v.cache_node_type
             (fun f -> ("cache_node_type", (String.to_json f)));
           Util.option_map v.reserved_cache_nodes_offering_id
             (fun f ->
                ("reserved_cache_nodes_offering_id", (String.to_json f)))])
    let of_json j =
      {
        reserved_cache_nodes_offering_id =
          (Util.option_map (Json.lookup j "reserved_cache_nodes_offering_id")
             String.of_json);
        cache_node_type =
          (Util.option_map (Json.lookup j "cache_node_type") String.of_json);
        duration =
          (Util.option_map (Json.lookup j "duration") String.of_json);
        product_description =
          (Util.option_map (Json.lookup j "product_description")
             String.of_json);
        offering_type =
          (Util.option_map (Json.lookup j "offering_type") String.of_json);
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module CacheSecurityGroupQuotaExceededFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DescribeUsersResult =
  struct
    type t = {
      users: UserList.t ;
      marker: String.t option }
    let make ?(users= [])  ?marker  () = { users; marker }
    let parse xml =
      Some
        {
          users =
            (Util.of_option []
               (Util.option_bind (Xml.member "Users" xml) UserList.parse));
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Some (Query.Pair ("Users.member", (UserList.to_query v.users)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Some ("users", (UserList.to_json v.users))])
    let of_json j =
      {
        users =
          (UserList.of_json (Util.of_option_exn (Json.lookup j "users")));
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module DeleteReplicationGroupMessage =
  struct
    type t =
      {
      replication_group_id: String.t ;
      retain_primary_cluster: Boolean.t option ;
      final_snapshot_identifier: String.t option }
    let make ~replication_group_id  ?retain_primary_cluster 
      ?final_snapshot_identifier  () =
      {
        replication_group_id;
        retain_primary_cluster;
        final_snapshot_identifier
      }
    let parse xml =
      Some
        {
          replication_group_id =
            (Xml.required "ReplicationGroupId"
               (Util.option_bind (Xml.member "ReplicationGroupId" xml)
                  String.parse));
          retain_primary_cluster =
            (Util.option_bind (Xml.member "RetainPrimaryCluster" xml)
               Boolean.parse);
          final_snapshot_identifier =
            (Util.option_bind (Xml.member "FinalSnapshotIdentifier" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.final_snapshot_identifier
              (fun f ->
                 Query.Pair ("FinalSnapshotIdentifier", (String.to_query f)));
           Util.option_map v.retain_primary_cluster
             (fun f ->
                Query.Pair ("RetainPrimaryCluster", (Boolean.to_query f)));
           Some
             (Query.Pair
                ("ReplicationGroupId",
                  (String.to_query v.replication_group_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.final_snapshot_identifier
              (fun f -> ("final_snapshot_identifier", (String.to_json f)));
           Util.option_map v.retain_primary_cluster
             (fun f -> ("retain_primary_cluster", (Boolean.to_json f)));
           Some
             ("replication_group_id",
               (String.to_json v.replication_group_id))])
    let of_json j =
      {
        replication_group_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "replication_group_id")));
        retain_primary_cluster =
          (Util.option_map (Json.lookup j "retain_primary_cluster")
             Boolean.of_json);
        final_snapshot_identifier =
          (Util.option_map (Json.lookup j "final_snapshot_identifier")
             String.of_json)
      }
  end
module ClusterQuotaForCustomerExceededFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module TagNotFoundFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CopySnapshotResult =
  struct
    type t = {
      snapshot: Snapshot.t option }
    let make ?snapshot  () = { snapshot }
    let parse xml =
      Some
        {
          snapshot =
            (Util.option_bind (Xml.member "Snapshot" xml) Snapshot.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.snapshot
              (fun f -> Query.Pair ("Snapshot", (Snapshot.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.snapshot
              (fun f -> ("snapshot", (Snapshot.to_json f)))])
    let of_json j =
      {
        snapshot =
          (Util.option_map (Json.lookup j "snapshot") Snapshot.of_json)
      }
  end
module DeleteUserGroupMessage =
  struct
    type t = {
      user_group_id: String.t }
    let make ~user_group_id  () = { user_group_id }
    let parse xml =
      Some
        {
          user_group_id =
            (Xml.required "UserGroupId"
               (Util.option_bind (Xml.member "UserGroupId" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("UserGroupId", (String.to_query v.user_group_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("user_group_id", (String.to_json v.user_group_id))])
    let of_json j =
      {
        user_group_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "user_group_id")))
      }
  end
module FailoverGlobalReplicationGroupResult =
  struct
    type t = {
      global_replication_group: GlobalReplicationGroup.t option }
    let make ?global_replication_group  () = { global_replication_group }
    let parse xml =
      Some
        {
          global_replication_group =
            (Util.option_bind (Xml.member "GlobalReplicationGroup" xml)
               GlobalReplicationGroup.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.global_replication_group
              (fun f ->
                 Query.Pair
                   ("GlobalReplicationGroup",
                     (GlobalReplicationGroup.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.global_replication_group
              (fun f ->
                 ("global_replication_group",
                   (GlobalReplicationGroup.to_json f)))])
    let of_json j =
      {
        global_replication_group =
          (Util.option_map (Json.lookup j "global_replication_group")
             GlobalReplicationGroup.of_json)
      }
  end
module TestFailoverNotAvailableFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DecreaseReplicaCountMessage =
  struct
    type t =
      {
      replication_group_id: String.t ;
      new_replica_count: Integer.t option ;
      replica_configuration: ReplicaConfigurationList.t ;
      replicas_to_remove: RemoveReplicasList.t ;
      apply_immediately: Boolean.t }
    let make ~replication_group_id  ?new_replica_count 
      ?(replica_configuration= [])  ?(replicas_to_remove= []) 
      ~apply_immediately  () =
      {
        replication_group_id;
        new_replica_count;
        replica_configuration;
        replicas_to_remove;
        apply_immediately
      }
    let parse xml =
      Some
        {
          replication_group_id =
            (Xml.required "ReplicationGroupId"
               (Util.option_bind (Xml.member "ReplicationGroupId" xml)
                  String.parse));
          new_replica_count =
            (Util.option_bind (Xml.member "NewReplicaCount" xml)
               Integer.parse);
          replica_configuration =
            (Util.of_option []
               (Util.option_bind (Xml.member "ReplicaConfiguration" xml)
                  ReplicaConfigurationList.parse));
          replicas_to_remove =
            (Util.of_option []
               (Util.option_bind (Xml.member "ReplicasToRemove" xml)
                  RemoveReplicasList.parse));
          apply_immediately =
            (Xml.required "ApplyImmediately"
               (Util.option_bind (Xml.member "ApplyImmediately" xml)
                  Boolean.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ApplyImmediately", (Boolean.to_query v.apply_immediately)));
           Some
             (Query.Pair
                ("ReplicasToRemove.member",
                  (RemoveReplicasList.to_query v.replicas_to_remove)));
           Some
             (Query.Pair
                ("ReplicaConfiguration.member",
                  (ReplicaConfigurationList.to_query v.replica_configuration)));
           Util.option_map v.new_replica_count
             (fun f -> Query.Pair ("NewReplicaCount", (Integer.to_query f)));
           Some
             (Query.Pair
                ("ReplicationGroupId",
                  (String.to_query v.replication_group_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("apply_immediately", (Boolean.to_json v.apply_immediately));
           Some
             ("replicas_to_remove",
               (RemoveReplicasList.to_json v.replicas_to_remove));
           Some
             ("replica_configuration",
               (ReplicaConfigurationList.to_json v.replica_configuration));
           Util.option_map v.new_replica_count
             (fun f -> ("new_replica_count", (Integer.to_json f)));
           Some
             ("replication_group_id",
               (String.to_json v.replication_group_id))])
    let of_json j =
      {
        replication_group_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "replication_group_id")));
        new_replica_count =
          (Util.option_map (Json.lookup j "new_replica_count")
             Integer.of_json);
        replica_configuration =
          (ReplicaConfigurationList.of_json
             (Util.of_option_exn (Json.lookup j "replica_configuration")));
        replicas_to_remove =
          (RemoveReplicasList.of_json
             (Util.of_option_exn (Json.lookup j "replicas_to_remove")));
        apply_immediately =
          (Boolean.of_json
             (Util.of_option_exn (Json.lookup j "apply_immediately")))
      }
  end
module ResetCacheParameterGroupMessage =
  struct
    type t =
      {
      cache_parameter_group_name: String.t ;
      reset_all_parameters: Boolean.t option ;
      parameter_name_values: ParameterNameValueList.t }
    let make ~cache_parameter_group_name  ?reset_all_parameters 
      ?(parameter_name_values= [])  () =
      {
        cache_parameter_group_name;
        reset_all_parameters;
        parameter_name_values
      }
    let parse xml =
      Some
        {
          cache_parameter_group_name =
            (Xml.required "CacheParameterGroupName"
               (Util.option_bind (Xml.member "CacheParameterGroupName" xml)
                  String.parse));
          reset_all_parameters =
            (Util.option_bind (Xml.member "ResetAllParameters" xml)
               Boolean.parse);
          parameter_name_values =
            (Util.of_option []
               (Util.option_bind (Xml.member "ParameterNameValues" xml)
                  ParameterNameValueList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ParameterNameValues.member",
                   (ParameterNameValueList.to_query v.parameter_name_values)));
           Util.option_map v.reset_all_parameters
             (fun f ->
                Query.Pair ("ResetAllParameters", (Boolean.to_query f)));
           Some
             (Query.Pair
                ("CacheParameterGroupName",
                  (String.to_query v.cache_parameter_group_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("parameter_name_values",
                (ParameterNameValueList.to_json v.parameter_name_values));
           Util.option_map v.reset_all_parameters
             (fun f -> ("reset_all_parameters", (Boolean.to_json f)));
           Some
             ("cache_parameter_group_name",
               (String.to_json v.cache_parameter_group_name))])
    let of_json j =
      {
        cache_parameter_group_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "cache_parameter_group_name")));
        reset_all_parameters =
          (Util.option_map (Json.lookup j "reset_all_parameters")
             Boolean.of_json);
        parameter_name_values =
          (ParameterNameValueList.of_json
             (Util.of_option_exn (Json.lookup j "parameter_name_values")))
      }
  end
module CreateGlobalReplicationGroupResult =
  struct
    type t = {
      global_replication_group: GlobalReplicationGroup.t option }
    let make ?global_replication_group  () = { global_replication_group }
    let parse xml =
      Some
        {
          global_replication_group =
            (Util.option_bind (Xml.member "GlobalReplicationGroup" xml)
               GlobalReplicationGroup.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.global_replication_group
              (fun f ->
                 Query.Pair
                   ("GlobalReplicationGroup",
                     (GlobalReplicationGroup.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.global_replication_group
              (fun f ->
                 ("global_replication_group",
                   (GlobalReplicationGroup.to_json f)))])
    let of_json j =
      {
        global_replication_group =
          (Util.option_map (Json.lookup j "global_replication_group")
             GlobalReplicationGroup.of_json)
      }
  end
module DescribeEventsMessage =
  struct
    type t =
      {
      source_identifier: String.t option ;
      source_type: SourceType.t option ;
      start_time: DateTime.t option ;
      end_time: DateTime.t option ;
      duration: Integer.t option ;
      max_records: Integer.t option ;
      marker: String.t option }
    let make ?source_identifier  ?source_type  ?start_time  ?end_time 
      ?duration  ?max_records  ?marker  () =
      {
        source_identifier;
        source_type;
        start_time;
        end_time;
        duration;
        max_records;
        marker
      }
    let parse xml =
      Some
        {
          source_identifier =
            (Util.option_bind (Xml.member "SourceIdentifier" xml)
               String.parse);
          source_type =
            (Util.option_bind (Xml.member "SourceType" xml) SourceType.parse);
          start_time =
            (Util.option_bind (Xml.member "StartTime" xml) DateTime.parse);
          end_time =
            (Util.option_bind (Xml.member "EndTime" xml) DateTime.parse);
          duration =
            (Util.option_bind (Xml.member "Duration" xml) Integer.parse);
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Util.option_map v.duration
             (fun f -> Query.Pair ("Duration", (Integer.to_query f)));
           Util.option_map v.end_time
             (fun f -> Query.Pair ("EndTime", (DateTime.to_query f)));
           Util.option_map v.start_time
             (fun f -> Query.Pair ("StartTime", (DateTime.to_query f)));
           Util.option_map v.source_type
             (fun f -> Query.Pair ("SourceType", (SourceType.to_query f)));
           Util.option_map v.source_identifier
             (fun f -> Query.Pair ("SourceIdentifier", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Util.option_map v.duration
             (fun f -> ("duration", (Integer.to_json f)));
           Util.option_map v.end_time
             (fun f -> ("end_time", (DateTime.to_json f)));
           Util.option_map v.start_time
             (fun f -> ("start_time", (DateTime.to_json f)));
           Util.option_map v.source_type
             (fun f -> ("source_type", (SourceType.to_json f)));
           Util.option_map v.source_identifier
             (fun f -> ("source_identifier", (String.to_json f)))])
    let of_json j =
      {
        source_identifier =
          (Util.option_map (Json.lookup j "source_identifier") String.of_json);
        source_type =
          (Util.option_map (Json.lookup j "source_type") SourceType.of_json);
        start_time =
          (Util.option_map (Json.lookup j "start_time") DateTime.of_json);
        end_time =
          (Util.option_map (Json.lookup j "end_time") DateTime.of_json);
        duration =
          (Util.option_map (Json.lookup j "duration") Integer.of_json);
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module RevokeCacheSecurityGroupIngressMessage =
  struct
    type t =
      {
      cache_security_group_name: String.t ;
      e_c2_security_group_name: String.t ;
      e_c2_security_group_owner_id: String.t }
    let make ~cache_security_group_name  ~e_c2_security_group_name 
      ~e_c2_security_group_owner_id  () =
      {
        cache_security_group_name;
        e_c2_security_group_name;
        e_c2_security_group_owner_id
      }
    let parse xml =
      Some
        {
          cache_security_group_name =
            (Xml.required "CacheSecurityGroupName"
               (Util.option_bind (Xml.member "CacheSecurityGroupName" xml)
                  String.parse));
          e_c2_security_group_name =
            (Xml.required "EC2SecurityGroupName"
               (Util.option_bind (Xml.member "EC2SecurityGroupName" xml)
                  String.parse));
          e_c2_security_group_owner_id =
            (Xml.required "EC2SecurityGroupOwnerId"
               (Util.option_bind (Xml.member "EC2SecurityGroupOwnerId" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("EC2SecurityGroupOwnerId",
                   (String.to_query v.e_c2_security_group_owner_id)));
           Some
             (Query.Pair
                ("EC2SecurityGroupName",
                  (String.to_query v.e_c2_security_group_name)));
           Some
             (Query.Pair
                ("CacheSecurityGroupName",
                  (String.to_query v.cache_security_group_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("e_c2_security_group_owner_id",
                (String.to_json v.e_c2_security_group_owner_id));
           Some
             ("e_c2_security_group_name",
               (String.to_json v.e_c2_security_group_name));
           Some
             ("cache_security_group_name",
               (String.to_json v.cache_security_group_name))])
    let of_json j =
      {
        cache_security_group_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "cache_security_group_name")));
        e_c2_security_group_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "e_c2_security_group_name")));
        e_c2_security_group_owner_id =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "e_c2_security_group_owner_id")))
      }
  end
module ServiceLinkedRoleNotFoundFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module SnapshotAlreadyExistsFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module InvalidCacheClusterStateFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CacheClusterMessage =
  struct
    type t = {
      marker: String.t option ;
      cache_clusters: CacheClusterList.t }
    let make ?marker  ?(cache_clusters= [])  () = { marker; cache_clusters }
    let parse xml =
      Some
        {
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          cache_clusters =
            (Util.of_option []
               (Util.option_bind (Xml.member "CacheClusters" xml)
                  CacheClusterList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("CacheClusters.member",
                   (CacheClusterList.to_query v.cache_clusters)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("cache_clusters", (CacheClusterList.to_json v.cache_clusters));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)))])
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        cache_clusters =
          (CacheClusterList.of_json
             (Util.of_option_exn (Json.lookup j "cache_clusters")))
      }
  end
module CacheSubnetGroupAlreadyExistsFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module RebootCacheClusterMessage =
  struct
    type t =
      {
      cache_cluster_id: String.t ;
      cache_node_ids_to_reboot: CacheNodeIdsList.t }
    let make ~cache_cluster_id  ~cache_node_ids_to_reboot  () =
      { cache_cluster_id; cache_node_ids_to_reboot }
    let parse xml =
      Some
        {
          cache_cluster_id =
            (Xml.required "CacheClusterId"
               (Util.option_bind (Xml.member "CacheClusterId" xml)
                  String.parse));
          cache_node_ids_to_reboot =
            (Xml.required "CacheNodeIdsToReboot"
               (Util.option_bind (Xml.member "CacheNodeIdsToReboot" xml)
                  CacheNodeIdsList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("CacheNodeIdsToReboot.member",
                   (CacheNodeIdsList.to_query v.cache_node_ids_to_reboot)));
           Some
             (Query.Pair
                ("CacheClusterId", (String.to_query v.cache_cluster_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("cache_node_ids_to_reboot",
                (CacheNodeIdsList.to_json v.cache_node_ids_to_reboot));
           Some ("cache_cluster_id", (String.to_json v.cache_cluster_id))])
    let of_json j =
      {
        cache_cluster_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "cache_cluster_id")));
        cache_node_ids_to_reboot =
          (CacheNodeIdsList.of_json
             (Util.of_option_exn (Json.lookup j "cache_node_ids_to_reboot")))
      }
  end
module ReservedCacheNodeQuotaExceededFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DescribeUserGroupsResult =
  struct
    type t = {
      user_groups: UserGroupList.t ;
      marker: String.t option }
    let make ?(user_groups= [])  ?marker  () = { user_groups; marker }
    let parse xml =
      Some
        {
          user_groups =
            (Util.of_option []
               (Util.option_bind (Xml.member "UserGroups" xml)
                  UserGroupList.parse));
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Some
             (Query.Pair
                ("UserGroups.member", (UserGroupList.to_query v.user_groups)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Some ("user_groups", (UserGroupList.to_json v.user_groups))])
    let of_json j =
      {
        user_groups =
          (UserGroupList.of_json
             (Util.of_option_exn (Json.lookup j "user_groups")));
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module UserNotFoundFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module SnapshotQuotaExceededFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ModifyReplicationGroupShardConfigurationMessage =
  struct
    type t =
      {
      replication_group_id: String.t ;
      node_group_count: Integer.t ;
      apply_immediately: Boolean.t ;
      resharding_configuration: ReshardingConfigurationList.t ;
      node_groups_to_remove: NodeGroupsToRemoveList.t ;
      node_groups_to_retain: NodeGroupsToRetainList.t }
    let make ~replication_group_id  ~node_group_count  ~apply_immediately 
      ?(resharding_configuration= [])  ?(node_groups_to_remove= []) 
      ?(node_groups_to_retain= [])  () =
      {
        replication_group_id;
        node_group_count;
        apply_immediately;
        resharding_configuration;
        node_groups_to_remove;
        node_groups_to_retain
      }
    let parse xml =
      Some
        {
          replication_group_id =
            (Xml.required "ReplicationGroupId"
               (Util.option_bind (Xml.member "ReplicationGroupId" xml)
                  String.parse));
          node_group_count =
            (Xml.required "NodeGroupCount"
               (Util.option_bind (Xml.member "NodeGroupCount" xml)
                  Integer.parse));
          apply_immediately =
            (Xml.required "ApplyImmediately"
               (Util.option_bind (Xml.member "ApplyImmediately" xml)
                  Boolean.parse));
          resharding_configuration =
            (Util.of_option []
               (Util.option_bind (Xml.member "ReshardingConfiguration" xml)
                  ReshardingConfigurationList.parse));
          node_groups_to_remove =
            (Util.of_option []
               (Util.option_bind (Xml.member "NodeGroupsToRemove" xml)
                  NodeGroupsToRemoveList.parse));
          node_groups_to_retain =
            (Util.of_option []
               (Util.option_bind (Xml.member "NodeGroupsToRetain" xml)
                  NodeGroupsToRetainList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("NodeGroupsToRetain.member",
                   (NodeGroupsToRetainList.to_query v.node_groups_to_retain)));
           Some
             (Query.Pair
                ("NodeGroupsToRemove.member",
                  (NodeGroupsToRemoveList.to_query v.node_groups_to_remove)));
           Some
             (Query.Pair
                ("ReshardingConfiguration.member",
                  (ReshardingConfigurationList.to_query
                     v.resharding_configuration)));
           Some
             (Query.Pair
                ("ApplyImmediately", (Boolean.to_query v.apply_immediately)));
           Some
             (Query.Pair
                ("NodeGroupCount", (Integer.to_query v.node_group_count)));
           Some
             (Query.Pair
                ("ReplicationGroupId",
                  (String.to_query v.replication_group_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("node_groups_to_retain",
                (NodeGroupsToRetainList.to_json v.node_groups_to_retain));
           Some
             ("node_groups_to_remove",
               (NodeGroupsToRemoveList.to_json v.node_groups_to_remove));
           Some
             ("resharding_configuration",
               (ReshardingConfigurationList.to_json
                  v.resharding_configuration));
           Some ("apply_immediately", (Boolean.to_json v.apply_immediately));
           Some ("node_group_count", (Integer.to_json v.node_group_count));
           Some
             ("replication_group_id",
               (String.to_json v.replication_group_id))])
    let of_json j =
      {
        replication_group_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "replication_group_id")));
        node_group_count =
          (Integer.of_json
             (Util.of_option_exn (Json.lookup j "node_group_count")));
        apply_immediately =
          (Boolean.of_json
             (Util.of_option_exn (Json.lookup j "apply_immediately")));
        resharding_configuration =
          (ReshardingConfigurationList.of_json
             (Util.of_option_exn (Json.lookup j "resharding_configuration")));
        node_groups_to_remove =
          (NodeGroupsToRemoveList.of_json
             (Util.of_option_exn (Json.lookup j "node_groups_to_remove")));
        node_groups_to_retain =
          (NodeGroupsToRetainList.of_json
             (Util.of_option_exn (Json.lookup j "node_groups_to_retain")))
      }
  end
module BatchApplyUpdateActionMessage =
  struct
    type t =
      {
      replication_group_ids: ReplicationGroupIdList.t ;
      cache_cluster_ids: CacheClusterIdList.t ;
      service_update_name: String.t }
    let make ?(replication_group_ids= [])  ?(cache_cluster_ids= []) 
      ~service_update_name  () =
      { replication_group_ids; cache_cluster_ids; service_update_name }
    let parse xml =
      Some
        {
          replication_group_ids =
            (Util.of_option []
               (Util.option_bind (Xml.member "ReplicationGroupIds" xml)
                  ReplicationGroupIdList.parse));
          cache_cluster_ids =
            (Util.of_option []
               (Util.option_bind (Xml.member "CacheClusterIds" xml)
                  CacheClusterIdList.parse));
          service_update_name =
            (Xml.required "ServiceUpdateName"
               (Util.option_bind (Xml.member "ServiceUpdateName" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ServiceUpdateName",
                   (String.to_query v.service_update_name)));
           Some
             (Query.Pair
                ("CacheClusterIds.member",
                  (CacheClusterIdList.to_query v.cache_cluster_ids)));
           Some
             (Query.Pair
                ("ReplicationGroupIds.member",
                  (ReplicationGroupIdList.to_query v.replication_group_ids)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("service_update_name", (String.to_json v.service_update_name));
           Some
             ("cache_cluster_ids",
               (CacheClusterIdList.to_json v.cache_cluster_ids));
           Some
             ("replication_group_ids",
               (ReplicationGroupIdList.to_json v.replication_group_ids))])
    let of_json j =
      {
        replication_group_ids =
          (ReplicationGroupIdList.of_json
             (Util.of_option_exn (Json.lookup j "replication_group_ids")));
        cache_cluster_ids =
          (CacheClusterIdList.of_json
             (Util.of_option_exn (Json.lookup j "cache_cluster_ids")));
        service_update_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "service_update_name")))
      }
  end
module CreateCacheParameterGroupResult =
  struct
    type t = {
      cache_parameter_group: CacheParameterGroup.t option }
    let make ?cache_parameter_group  () = { cache_parameter_group }
    let parse xml =
      Some
        {
          cache_parameter_group =
            (Util.option_bind (Xml.member "CacheParameterGroup" xml)
               CacheParameterGroup.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.cache_parameter_group
              (fun f ->
                 Query.Pair
                   ("CacheParameterGroup", (CacheParameterGroup.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cache_parameter_group
              (fun f ->
                 ("cache_parameter_group", (CacheParameterGroup.to_json f)))])
    let of_json j =
      {
        cache_parameter_group =
          (Util.option_map (Json.lookup j "cache_parameter_group")
             CacheParameterGroup.of_json)
      }
  end
module IncreaseNodeGroupsInGlobalReplicationGroupResult =
  struct
    type t = {
      global_replication_group: GlobalReplicationGroup.t option }
    let make ?global_replication_group  () = { global_replication_group }
    let parse xml =
      Some
        {
          global_replication_group =
            (Util.option_bind (Xml.member "GlobalReplicationGroup" xml)
               GlobalReplicationGroup.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.global_replication_group
              (fun f ->
                 Query.Pair
                   ("GlobalReplicationGroup",
                     (GlobalReplicationGroup.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.global_replication_group
              (fun f ->
                 ("global_replication_group",
                   (GlobalReplicationGroup.to_json f)))])
    let of_json j =
      {
        global_replication_group =
          (Util.option_map (Json.lookup j "global_replication_group")
             GlobalReplicationGroup.of_json)
      }
  end
module ReplicationGroupNotUnderMigrationFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module APICallRateForCustomerExceededFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ModifyGlobalReplicationGroupResult =
  struct
    type t = {
      global_replication_group: GlobalReplicationGroup.t option }
    let make ?global_replication_group  () = { global_replication_group }
    let parse xml =
      Some
        {
          global_replication_group =
            (Util.option_bind (Xml.member "GlobalReplicationGroup" xml)
               GlobalReplicationGroup.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.global_replication_group
              (fun f ->
                 Query.Pair
                   ("GlobalReplicationGroup",
                     (GlobalReplicationGroup.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.global_replication_group
              (fun f ->
                 ("global_replication_group",
                   (GlobalReplicationGroup.to_json f)))])
    let of_json j =
      {
        global_replication_group =
          (Util.option_map (Json.lookup j "global_replication_group")
             GlobalReplicationGroup.of_json)
      }
  end
module DescribeReservedCacheNodesMessage =
  struct
    type t =
      {
      reserved_cache_node_id: String.t option ;
      reserved_cache_nodes_offering_id: String.t option ;
      cache_node_type: String.t option ;
      duration: String.t option ;
      product_description: String.t option ;
      offering_type: String.t option ;
      max_records: Integer.t option ;
      marker: String.t option }
    let make ?reserved_cache_node_id  ?reserved_cache_nodes_offering_id 
      ?cache_node_type  ?duration  ?product_description  ?offering_type 
      ?max_records  ?marker  () =
      {
        reserved_cache_node_id;
        reserved_cache_nodes_offering_id;
        cache_node_type;
        duration;
        product_description;
        offering_type;
        max_records;
        marker
      }
    let parse xml =
      Some
        {
          reserved_cache_node_id =
            (Util.option_bind (Xml.member "ReservedCacheNodeId" xml)
               String.parse);
          reserved_cache_nodes_offering_id =
            (Util.option_bind (Xml.member "ReservedCacheNodesOfferingId" xml)
               String.parse);
          cache_node_type =
            (Util.option_bind (Xml.member "CacheNodeType" xml) String.parse);
          duration =
            (Util.option_bind (Xml.member "Duration" xml) String.parse);
          product_description =
            (Util.option_bind (Xml.member "ProductDescription" xml)
               String.parse);
          offering_type =
            (Util.option_bind (Xml.member "OfferingType" xml) String.parse);
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Util.option_map v.offering_type
             (fun f -> Query.Pair ("OfferingType", (String.to_query f)));
           Util.option_map v.product_description
             (fun f -> Query.Pair ("ProductDescription", (String.to_query f)));
           Util.option_map v.duration
             (fun f -> Query.Pair ("Duration", (String.to_query f)));
           Util.option_map v.cache_node_type
             (fun f -> Query.Pair ("CacheNodeType", (String.to_query f)));
           Util.option_map v.reserved_cache_nodes_offering_id
             (fun f ->
                Query.Pair
                  ("ReservedCacheNodesOfferingId", (String.to_query f)));
           Util.option_map v.reserved_cache_node_id
             (fun f ->
                Query.Pair ("ReservedCacheNodeId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Util.option_map v.offering_type
             (fun f -> ("offering_type", (String.to_json f)));
           Util.option_map v.product_description
             (fun f -> ("product_description", (String.to_json f)));
           Util.option_map v.duration
             (fun f -> ("duration", (String.to_json f)));
           Util.option_map v.cache_node_type
             (fun f -> ("cache_node_type", (String.to_json f)));
           Util.option_map v.reserved_cache_nodes_offering_id
             (fun f ->
                ("reserved_cache_nodes_offering_id", (String.to_json f)));
           Util.option_map v.reserved_cache_node_id
             (fun f -> ("reserved_cache_node_id", (String.to_json f)))])
    let of_json j =
      {
        reserved_cache_node_id =
          (Util.option_map (Json.lookup j "reserved_cache_node_id")
             String.of_json);
        reserved_cache_nodes_offering_id =
          (Util.option_map (Json.lookup j "reserved_cache_nodes_offering_id")
             String.of_json);
        cache_node_type =
          (Util.option_map (Json.lookup j "cache_node_type") String.of_json);
        duration =
          (Util.option_map (Json.lookup j "duration") String.of_json);
        product_description =
          (Util.option_map (Json.lookup j "product_description")
             String.of_json);
        offering_type =
          (Util.option_map (Json.lookup j "offering_type") String.of_json);
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module InvalidUserStateFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module UserGroupNotFoundFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DefaultUserRequired =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CreateCacheClusterResult =
  struct
    type t = {
      cache_cluster: CacheCluster.t option }
    let make ?cache_cluster  () = { cache_cluster }
    let parse xml =
      Some
        {
          cache_cluster =
            (Util.option_bind (Xml.member "CacheCluster" xml)
               CacheCluster.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.cache_cluster
              (fun f ->
                 Query.Pair ("CacheCluster", (CacheCluster.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cache_cluster
              (fun f -> ("cache_cluster", (CacheCluster.to_json f)))])
    let of_json j =
      {
        cache_cluster =
          (Util.option_map (Json.lookup j "cache_cluster")
             CacheCluster.of_json)
      }
  end
module SnapshotFeatureNotSupportedFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CreateSnapshotMessage =
  struct
    type t =
      {
      replication_group_id: String.t option ;
      cache_cluster_id: String.t option ;
      snapshot_name: String.t ;
      kms_key_id: String.t option }
    let make ?replication_group_id  ?cache_cluster_id  ~snapshot_name 
      ?kms_key_id  () =
      { replication_group_id; cache_cluster_id; snapshot_name; kms_key_id }
    let parse xml =
      Some
        {
          replication_group_id =
            (Util.option_bind (Xml.member "ReplicationGroupId" xml)
               String.parse);
          cache_cluster_id =
            (Util.option_bind (Xml.member "CacheClusterId" xml) String.parse);
          snapshot_name =
            (Xml.required "SnapshotName"
               (Util.option_bind (Xml.member "SnapshotName" xml) String.parse));
          kms_key_id =
            (Util.option_bind (Xml.member "KmsKeyId" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.kms_key_id
              (fun f -> Query.Pair ("KmsKeyId", (String.to_query f)));
           Some
             (Query.Pair ("SnapshotName", (String.to_query v.snapshot_name)));
           Util.option_map v.cache_cluster_id
             (fun f -> Query.Pair ("CacheClusterId", (String.to_query f)));
           Util.option_map v.replication_group_id
             (fun f -> Query.Pair ("ReplicationGroupId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.kms_key_id
              (fun f -> ("kms_key_id", (String.to_json f)));
           Some ("snapshot_name", (String.to_json v.snapshot_name));
           Util.option_map v.cache_cluster_id
             (fun f -> ("cache_cluster_id", (String.to_json f)));
           Util.option_map v.replication_group_id
             (fun f -> ("replication_group_id", (String.to_json f)))])
    let of_json j =
      {
        replication_group_id =
          (Util.option_map (Json.lookup j "replication_group_id")
             String.of_json);
        cache_cluster_id =
          (Util.option_map (Json.lookup j "cache_cluster_id") String.of_json);
        snapshot_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "snapshot_name")));
        kms_key_id =
          (Util.option_map (Json.lookup j "kms_key_id") String.of_json)
      }
  end
module ModifyReplicationGroupShardConfigurationResult =
  struct
    type t = {
      replication_group: ReplicationGroup.t option }
    let make ?replication_group  () = { replication_group }
    let parse xml =
      Some
        {
          replication_group =
            (Util.option_bind (Xml.member "ReplicationGroup" xml)
               ReplicationGroup.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.replication_group
              (fun f ->
                 Query.Pair
                   ("ReplicationGroup", (ReplicationGroup.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.replication_group
              (fun f -> ("replication_group", (ReplicationGroup.to_json f)))])
    let of_json j =
      {
        replication_group =
          (Util.option_map (Json.lookup j "replication_group")
             ReplicationGroup.of_json)
      }
  end
module DescribeUpdateActionsMessage =
  struct
    type t =
      {
      service_update_name: String.t option ;
      replication_group_ids: ReplicationGroupIdList.t ;
      cache_cluster_ids: CacheClusterIdList.t ;
      engine: String.t option ;
      service_update_status: ServiceUpdateStatusList.t ;
      service_update_time_range: TimeRangeFilter.t option ;
      update_action_status: UpdateActionStatusList.t ;
      show_node_level_update_status: Boolean.t option ;
      max_records: Integer.t option ;
      marker: String.t option }
    let make ?service_update_name  ?(replication_group_ids= []) 
      ?(cache_cluster_ids= [])  ?engine  ?(service_update_status= []) 
      ?service_update_time_range  ?(update_action_status= []) 
      ?show_node_level_update_status  ?max_records  ?marker  () =
      {
        service_update_name;
        replication_group_ids;
        cache_cluster_ids;
        engine;
        service_update_status;
        service_update_time_range;
        update_action_status;
        show_node_level_update_status;
        max_records;
        marker
      }
    let parse xml =
      Some
        {
          service_update_name =
            (Util.option_bind (Xml.member "ServiceUpdateName" xml)
               String.parse);
          replication_group_ids =
            (Util.of_option []
               (Util.option_bind (Xml.member "ReplicationGroupIds" xml)
                  ReplicationGroupIdList.parse));
          cache_cluster_ids =
            (Util.of_option []
               (Util.option_bind (Xml.member "CacheClusterIds" xml)
                  CacheClusterIdList.parse));
          engine = (Util.option_bind (Xml.member "Engine" xml) String.parse);
          service_update_status =
            (Util.of_option []
               (Util.option_bind (Xml.member "ServiceUpdateStatus" xml)
                  ServiceUpdateStatusList.parse));
          service_update_time_range =
            (Util.option_bind (Xml.member "ServiceUpdateTimeRange" xml)
               TimeRangeFilter.parse);
          update_action_status =
            (Util.of_option []
               (Util.option_bind (Xml.member "UpdateActionStatus" xml)
                  UpdateActionStatusList.parse));
          show_node_level_update_status =
            (Util.option_bind (Xml.member "ShowNodeLevelUpdateStatus" xml)
               Boolean.parse);
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Util.option_map v.show_node_level_update_status
             (fun f ->
                Query.Pair
                  ("ShowNodeLevelUpdateStatus", (Boolean.to_query f)));
           Some
             (Query.Pair
                ("UpdateActionStatus.member",
                  (UpdateActionStatusList.to_query v.update_action_status)));
           Util.option_map v.service_update_time_range
             (fun f ->
                Query.Pair
                  ("ServiceUpdateTimeRange", (TimeRangeFilter.to_query f)));
           Some
             (Query.Pair
                ("ServiceUpdateStatus.member",
                  (ServiceUpdateStatusList.to_query v.service_update_status)));
           Util.option_map v.engine
             (fun f -> Query.Pair ("Engine", (String.to_query f)));
           Some
             (Query.Pair
                ("CacheClusterIds.member",
                  (CacheClusterIdList.to_query v.cache_cluster_ids)));
           Some
             (Query.Pair
                ("ReplicationGroupIds.member",
                  (ReplicationGroupIdList.to_query v.replication_group_ids)));
           Util.option_map v.service_update_name
             (fun f -> Query.Pair ("ServiceUpdateName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Util.option_map v.show_node_level_update_status
             (fun f -> ("show_node_level_update_status", (Boolean.to_json f)));
           Some
             ("update_action_status",
               (UpdateActionStatusList.to_json v.update_action_status));
           Util.option_map v.service_update_time_range
             (fun f ->
                ("service_update_time_range", (TimeRangeFilter.to_json f)));
           Some
             ("service_update_status",
               (ServiceUpdateStatusList.to_json v.service_update_status));
           Util.option_map v.engine (fun f -> ("engine", (String.to_json f)));
           Some
             ("cache_cluster_ids",
               (CacheClusterIdList.to_json v.cache_cluster_ids));
           Some
             ("replication_group_ids",
               (ReplicationGroupIdList.to_json v.replication_group_ids));
           Util.option_map v.service_update_name
             (fun f -> ("service_update_name", (String.to_json f)))])
    let of_json j =
      {
        service_update_name =
          (Util.option_map (Json.lookup j "service_update_name")
             String.of_json);
        replication_group_ids =
          (ReplicationGroupIdList.of_json
             (Util.of_option_exn (Json.lookup j "replication_group_ids")));
        cache_cluster_ids =
          (CacheClusterIdList.of_json
             (Util.of_option_exn (Json.lookup j "cache_cluster_ids")));
        engine = (Util.option_map (Json.lookup j "engine") String.of_json);
        service_update_status =
          (ServiceUpdateStatusList.of_json
             (Util.of_option_exn (Json.lookup j "service_update_status")));
        service_update_time_range =
          (Util.option_map (Json.lookup j "service_update_time_range")
             TimeRangeFilter.of_json);
        update_action_status =
          (UpdateActionStatusList.of_json
             (Util.of_option_exn (Json.lookup j "update_action_status")));
        show_node_level_update_status =
          (Util.option_map (Json.lookup j "show_node_level_update_status")
             Boolean.of_json);
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module DescribeCacheSubnetGroupsMessage =
  struct
    type t =
      {
      cache_subnet_group_name: String.t option ;
      max_records: Integer.t option ;
      marker: String.t option }
    let make ?cache_subnet_group_name  ?max_records  ?marker  () =
      { cache_subnet_group_name; max_records; marker }
    let parse xml =
      Some
        {
          cache_subnet_group_name =
            (Util.option_bind (Xml.member "CacheSubnetGroupName" xml)
               String.parse);
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Util.option_map v.cache_subnet_group_name
             (fun f ->
                Query.Pair ("CacheSubnetGroupName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Util.option_map v.cache_subnet_group_name
             (fun f -> ("cache_subnet_group_name", (String.to_json f)))])
    let of_json j =
      {
        cache_subnet_group_name =
          (Util.option_map (Json.lookup j "cache_subnet_group_name")
             String.of_json);
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module DisassociateGlobalReplicationGroupResult =
  struct
    type t = {
      global_replication_group: GlobalReplicationGroup.t option }
    let make ?global_replication_group  () = { global_replication_group }
    let parse xml =
      Some
        {
          global_replication_group =
            (Util.option_bind (Xml.member "GlobalReplicationGroup" xml)
               GlobalReplicationGroup.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.global_replication_group
              (fun f ->
                 Query.Pair
                   ("GlobalReplicationGroup",
                     (GlobalReplicationGroup.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.global_replication_group
              (fun f ->
                 ("global_replication_group",
                   (GlobalReplicationGroup.to_json f)))])
    let of_json j =
      {
        global_replication_group =
          (Util.option_map (Json.lookup j "global_replication_group")
             GlobalReplicationGroup.of_json)
      }
  end
module UpdateActionsMessage =
  struct
    type t = {
      marker: String.t option ;
      update_actions: UpdateActionList.t }
    let make ?marker  ?(update_actions= [])  () = { marker; update_actions }
    let parse xml =
      Some
        {
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          update_actions =
            (Util.of_option []
               (Util.option_bind (Xml.member "UpdateActions" xml)
                  UpdateActionList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("UpdateActions.member",
                   (UpdateActionList.to_query v.update_actions)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("update_actions", (UpdateActionList.to_json v.update_actions));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)))])
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        update_actions =
          (UpdateActionList.of_json
             (Util.of_option_exn (Json.lookup j "update_actions")))
      }
  end
module DeleteCacheSecurityGroupMessage =
  struct
    type t = {
      cache_security_group_name: String.t }
    let make ~cache_security_group_name  () = { cache_security_group_name }
    let parse xml =
      Some
        {
          cache_security_group_name =
            (Xml.required "CacheSecurityGroupName"
               (Util.option_bind (Xml.member "CacheSecurityGroupName" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("CacheSecurityGroupName",
                   (String.to_query v.cache_security_group_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("cache_security_group_name",
                (String.to_json v.cache_security_group_name))])
    let of_json j =
      {
        cache_security_group_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "cache_security_group_name")))
      }
  end
module EventsMessage =
  struct
    type t = {
      marker: String.t option ;
      events: EventList.t }
    let make ?marker  ?(events= [])  () = { marker; events }
    let parse xml =
      Some
        {
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          events =
            (Util.of_option []
               (Util.option_bind (Xml.member "Events" xml) EventList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("Events.member", (EventList.to_query v.events)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("events", (EventList.to_json v.events));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)))])
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        events =
          (EventList.of_json (Util.of_option_exn (Json.lookup j "events")))
      }
  end
module CopySnapshotMessage =
  struct
    type t =
      {
      source_snapshot_name: String.t ;
      target_snapshot_name: String.t ;
      target_bucket: String.t option ;
      kms_key_id: String.t option }
    let make ~source_snapshot_name  ~target_snapshot_name  ?target_bucket 
      ?kms_key_id  () =
      { source_snapshot_name; target_snapshot_name; target_bucket; kms_key_id
      }
    let parse xml =
      Some
        {
          source_snapshot_name =
            (Xml.required "SourceSnapshotName"
               (Util.option_bind (Xml.member "SourceSnapshotName" xml)
                  String.parse));
          target_snapshot_name =
            (Xml.required "TargetSnapshotName"
               (Util.option_bind (Xml.member "TargetSnapshotName" xml)
                  String.parse));
          target_bucket =
            (Util.option_bind (Xml.member "TargetBucket" xml) String.parse);
          kms_key_id =
            (Util.option_bind (Xml.member "KmsKeyId" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.kms_key_id
              (fun f -> Query.Pair ("KmsKeyId", (String.to_query f)));
           Util.option_map v.target_bucket
             (fun f -> Query.Pair ("TargetBucket", (String.to_query f)));
           Some
             (Query.Pair
                ("TargetSnapshotName",
                  (String.to_query v.target_snapshot_name)));
           Some
             (Query.Pair
                ("SourceSnapshotName",
                  (String.to_query v.source_snapshot_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.kms_key_id
              (fun f -> ("kms_key_id", (String.to_json f)));
           Util.option_map v.target_bucket
             (fun f -> ("target_bucket", (String.to_json f)));
           Some
             ("target_snapshot_name",
               (String.to_json v.target_snapshot_name));
           Some
             ("source_snapshot_name",
               (String.to_json v.source_snapshot_name))])
    let of_json j =
      {
        source_snapshot_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "source_snapshot_name")));
        target_snapshot_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "target_snapshot_name")));
        target_bucket =
          (Util.option_map (Json.lookup j "target_bucket") String.of_json);
        kms_key_id =
          (Util.option_map (Json.lookup j "kms_key_id") String.of_json)
      }
  end
module CreateSnapshotResult =
  struct
    type t = {
      snapshot: Snapshot.t option }
    let make ?snapshot  () = { snapshot }
    let parse xml =
      Some
        {
          snapshot =
            (Util.option_bind (Xml.member "Snapshot" xml) Snapshot.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.snapshot
              (fun f -> Query.Pair ("Snapshot", (Snapshot.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.snapshot
              (fun f -> ("snapshot", (Snapshot.to_json f)))])
    let of_json j =
      {
        snapshot =
          (Util.option_map (Json.lookup j "snapshot") Snapshot.of_json)
      }
  end
module CreateCacheClusterMessage =
  struct
    type t =
      {
      cache_cluster_id: String.t ;
      replication_group_id: String.t option ;
      a_z_mode: AZMode.t option ;
      preferred_availability_zone: String.t option ;
      preferred_availability_zones: PreferredAvailabilityZoneList.t ;
      num_cache_nodes: Integer.t option ;
      cache_node_type: String.t option ;
      engine: String.t option ;
      engine_version: String.t option ;
      cache_parameter_group_name: String.t option ;
      cache_subnet_group_name: String.t option ;
      cache_security_group_names: CacheSecurityGroupNameList.t ;
      security_group_ids: SecurityGroupIdsList.t ;
      tags: TagList.t ;
      snapshot_arns: SnapshotArnsList.t ;
      snapshot_name: String.t option ;
      preferred_maintenance_window: String.t option ;
      port: Integer.t option ;
      notification_topic_arn: String.t option ;
      auto_minor_version_upgrade: Boolean.t option ;
      snapshot_retention_limit: Integer.t option ;
      snapshot_window: String.t option ;
      auth_token: String.t option ;
      outpost_mode: OutpostMode.t option ;
      preferred_outpost_arn: String.t option ;
      preferred_outpost_arns: PreferredOutpostArnList.t }
    let make ~cache_cluster_id  ?replication_group_id  ?a_z_mode 
      ?preferred_availability_zone  ?(preferred_availability_zones= []) 
      ?num_cache_nodes  ?cache_node_type  ?engine  ?engine_version 
      ?cache_parameter_group_name  ?cache_subnet_group_name 
      ?(cache_security_group_names= [])  ?(security_group_ids= [])  ?(tags=
      [])  ?(snapshot_arns= [])  ?snapshot_name 
      ?preferred_maintenance_window  ?port  ?notification_topic_arn 
      ?auto_minor_version_upgrade  ?snapshot_retention_limit 
      ?snapshot_window  ?auth_token  ?outpost_mode  ?preferred_outpost_arn 
      ?(preferred_outpost_arns= [])  () =
      {
        cache_cluster_id;
        replication_group_id;
        a_z_mode;
        preferred_availability_zone;
        preferred_availability_zones;
        num_cache_nodes;
        cache_node_type;
        engine;
        engine_version;
        cache_parameter_group_name;
        cache_subnet_group_name;
        cache_security_group_names;
        security_group_ids;
        tags;
        snapshot_arns;
        snapshot_name;
        preferred_maintenance_window;
        port;
        notification_topic_arn;
        auto_minor_version_upgrade;
        snapshot_retention_limit;
        snapshot_window;
        auth_token;
        outpost_mode;
        preferred_outpost_arn;
        preferred_outpost_arns
      }
    let parse xml =
      Some
        {
          cache_cluster_id =
            (Xml.required "CacheClusterId"
               (Util.option_bind (Xml.member "CacheClusterId" xml)
                  String.parse));
          replication_group_id =
            (Util.option_bind (Xml.member "ReplicationGroupId" xml)
               String.parse);
          a_z_mode =
            (Util.option_bind (Xml.member "AZMode" xml) AZMode.parse);
          preferred_availability_zone =
            (Util.option_bind (Xml.member "PreferredAvailabilityZone" xml)
               String.parse);
          preferred_availability_zones =
            (Util.of_option []
               (Util.option_bind
                  (Xml.member "PreferredAvailabilityZones" xml)
                  PreferredAvailabilityZoneList.parse));
          num_cache_nodes =
            (Util.option_bind (Xml.member "NumCacheNodes" xml) Integer.parse);
          cache_node_type =
            (Util.option_bind (Xml.member "CacheNodeType" xml) String.parse);
          engine = (Util.option_bind (Xml.member "Engine" xml) String.parse);
          engine_version =
            (Util.option_bind (Xml.member "EngineVersion" xml) String.parse);
          cache_parameter_group_name =
            (Util.option_bind (Xml.member "CacheParameterGroupName" xml)
               String.parse);
          cache_subnet_group_name =
            (Util.option_bind (Xml.member "CacheSubnetGroupName" xml)
               String.parse);
          cache_security_group_names =
            (Util.of_option []
               (Util.option_bind (Xml.member "CacheSecurityGroupNames" xml)
                  CacheSecurityGroupNameList.parse));
          security_group_ids =
            (Util.of_option []
               (Util.option_bind (Xml.member "SecurityGroupIds" xml)
                  SecurityGroupIdsList.parse));
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) TagList.parse));
          snapshot_arns =
            (Util.of_option []
               (Util.option_bind (Xml.member "SnapshotArns" xml)
                  SnapshotArnsList.parse));
          snapshot_name =
            (Util.option_bind (Xml.member "SnapshotName" xml) String.parse);
          preferred_maintenance_window =
            (Util.option_bind (Xml.member "PreferredMaintenanceWindow" xml)
               String.parse);
          port = (Util.option_bind (Xml.member "Port" xml) Integer.parse);
          notification_topic_arn =
            (Util.option_bind (Xml.member "NotificationTopicArn" xml)
               String.parse);
          auto_minor_version_upgrade =
            (Util.option_bind (Xml.member "AutoMinorVersionUpgrade" xml)
               Boolean.parse);
          snapshot_retention_limit =
            (Util.option_bind (Xml.member "SnapshotRetentionLimit" xml)
               Integer.parse);
          snapshot_window =
            (Util.option_bind (Xml.member "SnapshotWindow" xml) String.parse);
          auth_token =
            (Util.option_bind (Xml.member "AuthToken" xml) String.parse);
          outpost_mode =
            (Util.option_bind (Xml.member "OutpostMode" xml)
               OutpostMode.parse);
          preferred_outpost_arn =
            (Util.option_bind (Xml.member "PreferredOutpostArn" xml)
               String.parse);
          preferred_outpost_arns =
            (Util.of_option []
               (Util.option_bind (Xml.member "PreferredOutpostArns" xml)
                  PreferredOutpostArnList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("PreferredOutpostArns.member",
                   (PreferredOutpostArnList.to_query v.preferred_outpost_arns)));
           Util.option_map v.preferred_outpost_arn
             (fun f ->
                Query.Pair ("PreferredOutpostArn", (String.to_query f)));
           Util.option_map v.outpost_mode
             (fun f -> Query.Pair ("OutpostMode", (OutpostMode.to_query f)));
           Util.option_map v.auth_token
             (fun f -> Query.Pair ("AuthToken", (String.to_query f)));
           Util.option_map v.snapshot_window
             (fun f -> Query.Pair ("SnapshotWindow", (String.to_query f)));
           Util.option_map v.snapshot_retention_limit
             (fun f ->
                Query.Pair ("SnapshotRetentionLimit", (Integer.to_query f)));
           Util.option_map v.auto_minor_version_upgrade
             (fun f ->
                Query.Pair ("AutoMinorVersionUpgrade", (Boolean.to_query f)));
           Util.option_map v.notification_topic_arn
             (fun f ->
                Query.Pair ("NotificationTopicArn", (String.to_query f)));
           Util.option_map v.port
             (fun f -> Query.Pair ("Port", (Integer.to_query f)));
           Util.option_map v.preferred_maintenance_window
             (fun f ->
                Query.Pair
                  ("PreferredMaintenanceWindow", (String.to_query f)));
           Util.option_map v.snapshot_name
             (fun f -> Query.Pair ("SnapshotName", (String.to_query f)));
           Some
             (Query.Pair
                ("SnapshotArns.member",
                  (SnapshotArnsList.to_query v.snapshot_arns)));
           Some (Query.Pair ("Tags.member", (TagList.to_query v.tags)));
           Some
             (Query.Pair
                ("SecurityGroupIds.member",
                  (SecurityGroupIdsList.to_query v.security_group_ids)));
           Some
             (Query.Pair
                ("CacheSecurityGroupNames.member",
                  (CacheSecurityGroupNameList.to_query
                     v.cache_security_group_names)));
           Util.option_map v.cache_subnet_group_name
             (fun f ->
                Query.Pair ("CacheSubnetGroupName", (String.to_query f)));
           Util.option_map v.cache_parameter_group_name
             (fun f ->
                Query.Pair ("CacheParameterGroupName", (String.to_query f)));
           Util.option_map v.engine_version
             (fun f -> Query.Pair ("EngineVersion", (String.to_query f)));
           Util.option_map v.engine
             (fun f -> Query.Pair ("Engine", (String.to_query f)));
           Util.option_map v.cache_node_type
             (fun f -> Query.Pair ("CacheNodeType", (String.to_query f)));
           Util.option_map v.num_cache_nodes
             (fun f -> Query.Pair ("NumCacheNodes", (Integer.to_query f)));
           Some
             (Query.Pair
                ("PreferredAvailabilityZones.member",
                  (PreferredAvailabilityZoneList.to_query
                     v.preferred_availability_zones)));
           Util.option_map v.preferred_availability_zone
             (fun f ->
                Query.Pair ("PreferredAvailabilityZone", (String.to_query f)));
           Util.option_map v.a_z_mode
             (fun f -> Query.Pair ("AZMode", (AZMode.to_query f)));
           Util.option_map v.replication_group_id
             (fun f -> Query.Pair ("ReplicationGroupId", (String.to_query f)));
           Some
             (Query.Pair
                ("CacheClusterId", (String.to_query v.cache_cluster_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("preferred_outpost_arns",
                (PreferredOutpostArnList.to_json v.preferred_outpost_arns));
           Util.option_map v.preferred_outpost_arn
             (fun f -> ("preferred_outpost_arn", (String.to_json f)));
           Util.option_map v.outpost_mode
             (fun f -> ("outpost_mode", (OutpostMode.to_json f)));
           Util.option_map v.auth_token
             (fun f -> ("auth_token", (String.to_json f)));
           Util.option_map v.snapshot_window
             (fun f -> ("snapshot_window", (String.to_json f)));
           Util.option_map v.snapshot_retention_limit
             (fun f -> ("snapshot_retention_limit", (Integer.to_json f)));
           Util.option_map v.auto_minor_version_upgrade
             (fun f -> ("auto_minor_version_upgrade", (Boolean.to_json f)));
           Util.option_map v.notification_topic_arn
             (fun f -> ("notification_topic_arn", (String.to_json f)));
           Util.option_map v.port (fun f -> ("port", (Integer.to_json f)));
           Util.option_map v.preferred_maintenance_window
             (fun f -> ("preferred_maintenance_window", (String.to_json f)));
           Util.option_map v.snapshot_name
             (fun f -> ("snapshot_name", (String.to_json f)));
           Some ("snapshot_arns", (SnapshotArnsList.to_json v.snapshot_arns));
           Some ("tags", (TagList.to_json v.tags));
           Some
             ("security_group_ids",
               (SecurityGroupIdsList.to_json v.security_group_ids));
           Some
             ("cache_security_group_names",
               (CacheSecurityGroupNameList.to_json
                  v.cache_security_group_names));
           Util.option_map v.cache_subnet_group_name
             (fun f -> ("cache_subnet_group_name", (String.to_json f)));
           Util.option_map v.cache_parameter_group_name
             (fun f -> ("cache_parameter_group_name", (String.to_json f)));
           Util.option_map v.engine_version
             (fun f -> ("engine_version", (String.to_json f)));
           Util.option_map v.engine (fun f -> ("engine", (String.to_json f)));
           Util.option_map v.cache_node_type
             (fun f -> ("cache_node_type", (String.to_json f)));
           Util.option_map v.num_cache_nodes
             (fun f -> ("num_cache_nodes", (Integer.to_json f)));
           Some
             ("preferred_availability_zones",
               (PreferredAvailabilityZoneList.to_json
                  v.preferred_availability_zones));
           Util.option_map v.preferred_availability_zone
             (fun f -> ("preferred_availability_zone", (String.to_json f)));
           Util.option_map v.a_z_mode
             (fun f -> ("a_z_mode", (AZMode.to_json f)));
           Util.option_map v.replication_group_id
             (fun f -> ("replication_group_id", (String.to_json f)));
           Some ("cache_cluster_id", (String.to_json v.cache_cluster_id))])
    let of_json j =
      {
        cache_cluster_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "cache_cluster_id")));
        replication_group_id =
          (Util.option_map (Json.lookup j "replication_group_id")
             String.of_json);
        a_z_mode =
          (Util.option_map (Json.lookup j "a_z_mode") AZMode.of_json);
        preferred_availability_zone =
          (Util.option_map (Json.lookup j "preferred_availability_zone")
             String.of_json);
        preferred_availability_zones =
          (PreferredAvailabilityZoneList.of_json
             (Util.of_option_exn
                (Json.lookup j "preferred_availability_zones")));
        num_cache_nodes =
          (Util.option_map (Json.lookup j "num_cache_nodes") Integer.of_json);
        cache_node_type =
          (Util.option_map (Json.lookup j "cache_node_type") String.of_json);
        engine = (Util.option_map (Json.lookup j "engine") String.of_json);
        engine_version =
          (Util.option_map (Json.lookup j "engine_version") String.of_json);
        cache_parameter_group_name =
          (Util.option_map (Json.lookup j "cache_parameter_group_name")
             String.of_json);
        cache_subnet_group_name =
          (Util.option_map (Json.lookup j "cache_subnet_group_name")
             String.of_json);
        cache_security_group_names =
          (CacheSecurityGroupNameList.of_json
             (Util.of_option_exn (Json.lookup j "cache_security_group_names")));
        security_group_ids =
          (SecurityGroupIdsList.of_json
             (Util.of_option_exn (Json.lookup j "security_group_ids")));
        tags = (TagList.of_json (Util.of_option_exn (Json.lookup j "tags")));
        snapshot_arns =
          (SnapshotArnsList.of_json
             (Util.of_option_exn (Json.lookup j "snapshot_arns")));
        snapshot_name =
          (Util.option_map (Json.lookup j "snapshot_name") String.of_json);
        preferred_maintenance_window =
          (Util.option_map (Json.lookup j "preferred_maintenance_window")
             String.of_json);
        port = (Util.option_map (Json.lookup j "port") Integer.of_json);
        notification_topic_arn =
          (Util.option_map (Json.lookup j "notification_topic_arn")
             String.of_json);
        auto_minor_version_upgrade =
          (Util.option_map (Json.lookup j "auto_minor_version_upgrade")
             Boolean.of_json);
        snapshot_retention_limit =
          (Util.option_map (Json.lookup j "snapshot_retention_limit")
             Integer.of_json);
        snapshot_window =
          (Util.option_map (Json.lookup j "snapshot_window") String.of_json);
        auth_token =
          (Util.option_map (Json.lookup j "auth_token") String.of_json);
        outpost_mode =
          (Util.option_map (Json.lookup j "outpost_mode") OutpostMode.of_json);
        preferred_outpost_arn =
          (Util.option_map (Json.lookup j "preferred_outpost_arn")
             String.of_json);
        preferred_outpost_arns =
          (PreferredOutpostArnList.of_json
             (Util.of_option_exn (Json.lookup j "preferred_outpost_arns")))
      }
  end
module DescribeCacheEngineVersionsMessage =
  struct
    type t =
      {
      engine: String.t option ;
      engine_version: String.t option ;
      cache_parameter_group_family: String.t option ;
      max_records: Integer.t option ;
      marker: String.t option ;
      default_only: Boolean.t option }
    let make ?engine  ?engine_version  ?cache_parameter_group_family 
      ?max_records  ?marker  ?default_only  () =
      {
        engine;
        engine_version;
        cache_parameter_group_family;
        max_records;
        marker;
        default_only
      }
    let parse xml =
      Some
        {
          engine = (Util.option_bind (Xml.member "Engine" xml) String.parse);
          engine_version =
            (Util.option_bind (Xml.member "EngineVersion" xml) String.parse);
          cache_parameter_group_family =
            (Util.option_bind (Xml.member "CacheParameterGroupFamily" xml)
               String.parse);
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          default_only =
            (Util.option_bind (Xml.member "DefaultOnly" xml) Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.default_only
              (fun f -> Query.Pair ("DefaultOnly", (Boolean.to_query f)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Util.option_map v.cache_parameter_group_family
             (fun f ->
                Query.Pair ("CacheParameterGroupFamily", (String.to_query f)));
           Util.option_map v.engine_version
             (fun f -> Query.Pair ("EngineVersion", (String.to_query f)));
           Util.option_map v.engine
             (fun f -> Query.Pair ("Engine", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.default_only
              (fun f -> ("default_only", (Boolean.to_json f)));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Util.option_map v.cache_parameter_group_family
             (fun f -> ("cache_parameter_group_family", (String.to_json f)));
           Util.option_map v.engine_version
             (fun f -> ("engine_version", (String.to_json f)));
           Util.option_map v.engine (fun f -> ("engine", (String.to_json f)))])
    let of_json j =
      {
        engine = (Util.option_map (Json.lookup j "engine") String.of_json);
        engine_version =
          (Util.option_map (Json.lookup j "engine_version") String.of_json);
        cache_parameter_group_family =
          (Util.option_map (Json.lookup j "cache_parameter_group_family")
             String.of_json);
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        default_only =
          (Util.option_map (Json.lookup j "default_only") Boolean.of_json)
      }
  end
module UpdateActionResultsMessage =
  struct
    type t =
      {
      processed_update_actions: ProcessedUpdateActionList.t ;
      unprocessed_update_actions: UnprocessedUpdateActionList.t }
    let make ?(processed_update_actions= [])  ?(unprocessed_update_actions=
      [])  () = { processed_update_actions; unprocessed_update_actions }
    let parse xml =
      Some
        {
          processed_update_actions =
            (Util.of_option []
               (Util.option_bind (Xml.member "ProcessedUpdateActions" xml)
                  ProcessedUpdateActionList.parse));
          unprocessed_update_actions =
            (Util.of_option []
               (Util.option_bind (Xml.member "UnprocessedUpdateActions" xml)
                  UnprocessedUpdateActionList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("UnprocessedUpdateActions.member",
                   (UnprocessedUpdateActionList.to_query
                      v.unprocessed_update_actions)));
           Some
             (Query.Pair
                ("ProcessedUpdateActions.member",
                  (ProcessedUpdateActionList.to_query
                     v.processed_update_actions)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("unprocessed_update_actions",
                (UnprocessedUpdateActionList.to_json
                   v.unprocessed_update_actions));
           Some
             ("processed_update_actions",
               (ProcessedUpdateActionList.to_json v.processed_update_actions))])
    let of_json j =
      {
        processed_update_actions =
          (ProcessedUpdateActionList.of_json
             (Util.of_option_exn (Json.lookup j "processed_update_actions")));
        unprocessed_update_actions =
          (UnprocessedUpdateActionList.of_json
             (Util.of_option_exn (Json.lookup j "unprocessed_update_actions")))
      }
  end
module RemoveTagsFromResourceMessage =
  struct
    type t = {
      resource_name: String.t ;
      tag_keys: KeyList.t }
    let make ~resource_name  ~tag_keys  () = { resource_name; tag_keys }
    let parse xml =
      Some
        {
          resource_name =
            (Xml.required "ResourceName"
               (Util.option_bind (Xml.member "ResourceName" xml) String.parse));
          tag_keys =
            (Xml.required "TagKeys"
               (Util.option_bind (Xml.member "TagKeys" xml) KeyList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("TagKeys.member", (KeyList.to_query v.tag_keys)));
           Some
             (Query.Pair ("ResourceName", (String.to_query v.resource_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tag_keys", (KeyList.to_json v.tag_keys));
           Some ("resource_name", (String.to_json v.resource_name))])
    let of_json j =
      {
        resource_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "resource_name")));
        tag_keys =
          (KeyList.of_json (Util.of_option_exn (Json.lookup j "tag_keys")))
      }
  end
module ReplicationGroupMessage =
  struct
    type t =
      {
      marker: String.t option ;
      replication_groups: ReplicationGroupList.t }
    let make ?marker  ?(replication_groups= [])  () =
      { marker; replication_groups }
    let parse xml =
      Some
        {
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          replication_groups =
            (Util.of_option []
               (Util.option_bind (Xml.member "ReplicationGroups" xml)
                  ReplicationGroupList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ReplicationGroups.member",
                   (ReplicationGroupList.to_query v.replication_groups)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("replication_groups",
                (ReplicationGroupList.to_json v.replication_groups));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)))])
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        replication_groups =
          (ReplicationGroupList.of_json
             (Util.of_option_exn (Json.lookup j "replication_groups")))
      }
  end
module DescribeCacheParametersMessage =
  struct
    type t =
      {
      cache_parameter_group_name: String.t ;
      source: String.t option ;
      max_records: Integer.t option ;
      marker: String.t option }
    let make ~cache_parameter_group_name  ?source  ?max_records  ?marker  ()
      = { cache_parameter_group_name; source; max_records; marker }
    let parse xml =
      Some
        {
          cache_parameter_group_name =
            (Xml.required "CacheParameterGroupName"
               (Util.option_bind (Xml.member "CacheParameterGroupName" xml)
                  String.parse));
          source = (Util.option_bind (Xml.member "Source" xml) String.parse);
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Util.option_map v.source
             (fun f -> Query.Pair ("Source", (String.to_query f)));
           Some
             (Query.Pair
                ("CacheParameterGroupName",
                  (String.to_query v.cache_parameter_group_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Util.option_map v.source (fun f -> ("source", (String.to_json f)));
           Some
             ("cache_parameter_group_name",
               (String.to_json v.cache_parameter_group_name))])
    let of_json j =
      {
        cache_parameter_group_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "cache_parameter_group_name")));
        source = (Util.option_map (Json.lookup j "source") String.of_json);
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module CacheSubnetGroupQuotaExceededFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CacheClusterAlreadyExistsFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CreateCacheParameterGroupMessage =
  struct
    type t =
      {
      cache_parameter_group_name: String.t ;
      cache_parameter_group_family: String.t ;
      description: String.t }
    let make ~cache_parameter_group_name  ~cache_parameter_group_family 
      ~description  () =
      { cache_parameter_group_name; cache_parameter_group_family; description
      }
    let parse xml =
      Some
        {
          cache_parameter_group_name =
            (Xml.required "CacheParameterGroupName"
               (Util.option_bind (Xml.member "CacheParameterGroupName" xml)
                  String.parse));
          cache_parameter_group_family =
            (Xml.required "CacheParameterGroupFamily"
               (Util.option_bind (Xml.member "CacheParameterGroupFamily" xml)
                  String.parse));
          description =
            (Xml.required "Description"
               (Util.option_bind (Xml.member "Description" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("Description", (String.to_query v.description)));
           Some
             (Query.Pair
                ("CacheParameterGroupFamily",
                  (String.to_query v.cache_parameter_group_family)));
           Some
             (Query.Pair
                ("CacheParameterGroupName",
                  (String.to_query v.cache_parameter_group_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("description", (String.to_json v.description));
           Some
             ("cache_parameter_group_family",
               (String.to_json v.cache_parameter_group_family));
           Some
             ("cache_parameter_group_name",
               (String.to_json v.cache_parameter_group_name))])
    let of_json j =
      {
        cache_parameter_group_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "cache_parameter_group_name")));
        cache_parameter_group_family =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "cache_parameter_group_family")));
        description =
          (String.of_json (Util.of_option_exn (Json.lookup j "description")))
      }
  end
module CacheEngineVersionMessage =
  struct
    type t =
      {
      marker: String.t option ;
      cache_engine_versions: CacheEngineVersionList.t }
    let make ?marker  ?(cache_engine_versions= [])  () =
      { marker; cache_engine_versions }
    let parse xml =
      Some
        {
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          cache_engine_versions =
            (Util.of_option []
               (Util.option_bind (Xml.member "CacheEngineVersions" xml)
                  CacheEngineVersionList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("CacheEngineVersions.member",
                   (CacheEngineVersionList.to_query v.cache_engine_versions)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("cache_engine_versions",
                (CacheEngineVersionList.to_json v.cache_engine_versions));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)))])
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        cache_engine_versions =
          (CacheEngineVersionList.of_json
             (Util.of_option_exn (Json.lookup j "cache_engine_versions")))
      }
  end
module PurchaseReservedCacheNodesOfferingResult =
  struct
    type t = {
      reserved_cache_node: ReservedCacheNode.t option }
    let make ?reserved_cache_node  () = { reserved_cache_node }
    let parse xml =
      Some
        {
          reserved_cache_node =
            (Util.option_bind (Xml.member "ReservedCacheNode" xml)
               ReservedCacheNode.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.reserved_cache_node
              (fun f ->
                 Query.Pair
                   ("ReservedCacheNode", (ReservedCacheNode.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.reserved_cache_node
              (fun f ->
                 ("reserved_cache_node", (ReservedCacheNode.to_json f)))])
    let of_json j =
      {
        reserved_cache_node =
          (Util.option_map (Json.lookup j "reserved_cache_node")
             ReservedCacheNode.of_json)
      }
  end
module InvalidSnapshotStateFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ListTagsForResourceMessage =
  struct
    type t = {
      resource_name: String.t }
    let make ~resource_name  () = { resource_name }
    let parse xml =
      Some
        {
          resource_name =
            (Xml.required "ResourceName"
               (Util.option_bind (Xml.member "ResourceName" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("ResourceName", (String.to_query v.resource_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("resource_name", (String.to_json v.resource_name))])
    let of_json j =
      {
        resource_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "resource_name")))
      }
  end
module ModifyUserGroupMessage =
  struct
    type t =
      {
      user_group_id: String.t ;
      user_ids_to_add: UserIdListInput.t ;
      user_ids_to_remove: UserIdListInput.t }
    let make ~user_group_id  ?(user_ids_to_add= [])  ?(user_ids_to_remove=
      [])  () = { user_group_id; user_ids_to_add; user_ids_to_remove }
    let parse xml =
      Some
        {
          user_group_id =
            (Xml.required "UserGroupId"
               (Util.option_bind (Xml.member "UserGroupId" xml) String.parse));
          user_ids_to_add =
            (Util.of_option []
               (Util.option_bind (Xml.member "UserIdsToAdd" xml)
                  UserIdListInput.parse));
          user_ids_to_remove =
            (Util.of_option []
               (Util.option_bind (Xml.member "UserIdsToRemove" xml)
                  UserIdListInput.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("UserIdsToRemove.member",
                   (UserIdListInput.to_query v.user_ids_to_remove)));
           Some
             (Query.Pair
                ("UserIdsToAdd.member",
                  (UserIdListInput.to_query v.user_ids_to_add)));
           Some
             (Query.Pair ("UserGroupId", (String.to_query v.user_group_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("user_ids_to_remove",
                (UserIdListInput.to_json v.user_ids_to_remove));
           Some
             ("user_ids_to_add", (UserIdListInput.to_json v.user_ids_to_add));
           Some ("user_group_id", (String.to_json v.user_group_id))])
    let of_json j =
      {
        user_group_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "user_group_id")));
        user_ids_to_add =
          (UserIdListInput.of_json
             (Util.of_option_exn (Json.lookup j "user_ids_to_add")));
        user_ids_to_remove =
          (UserIdListInput.of_json
             (Util.of_option_exn (Json.lookup j "user_ids_to_remove")))
      }
  end
module DescribeEngineDefaultParametersMessage =
  struct
    type t =
      {
      cache_parameter_group_family: String.t ;
      max_records: Integer.t option ;
      marker: String.t option }
    let make ~cache_parameter_group_family  ?max_records  ?marker  () =
      { cache_parameter_group_family; max_records; marker }
    let parse xml =
      Some
        {
          cache_parameter_group_family =
            (Xml.required "CacheParameterGroupFamily"
               (Util.option_bind (Xml.member "CacheParameterGroupFamily" xml)
                  String.parse));
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Some
             (Query.Pair
                ("CacheParameterGroupFamily",
                  (String.to_query v.cache_parameter_group_family)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Some
             ("cache_parameter_group_family",
               (String.to_json v.cache_parameter_group_family))])
    let of_json j =
      {
        cache_parameter_group_family =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "cache_parameter_group_family")));
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module StartMigrationResponse =
  struct
    type t = {
      replication_group: ReplicationGroup.t option }
    let make ?replication_group  () = { replication_group }
    let parse xml =
      Some
        {
          replication_group =
            (Util.option_bind (Xml.member "ReplicationGroup" xml)
               ReplicationGroup.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.replication_group
              (fun f ->
                 Query.Pair
                   ("ReplicationGroup", (ReplicationGroup.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.replication_group
              (fun f -> ("replication_group", (ReplicationGroup.to_json f)))])
    let of_json j =
      {
        replication_group =
          (Util.option_map (Json.lookup j "replication_group")
             ReplicationGroup.of_json)
      }
  end
module DeleteGlobalReplicationGroupResult =
  struct
    type t = {
      global_replication_group: GlobalReplicationGroup.t option }
    let make ?global_replication_group  () = { global_replication_group }
    let parse xml =
      Some
        {
          global_replication_group =
            (Util.option_bind (Xml.member "GlobalReplicationGroup" xml)
               GlobalReplicationGroup.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.global_replication_group
              (fun f ->
                 Query.Pair
                   ("GlobalReplicationGroup",
                     (GlobalReplicationGroup.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.global_replication_group
              (fun f ->
                 ("global_replication_group",
                   (GlobalReplicationGroup.to_json f)))])
    let of_json j =
      {
        global_replication_group =
          (Util.option_map (Json.lookup j "global_replication_group")
             GlobalReplicationGroup.of_json)
      }
  end
module IncreaseReplicaCountMessage =
  struct
    type t =
      {
      replication_group_id: String.t ;
      new_replica_count: Integer.t option ;
      replica_configuration: ReplicaConfigurationList.t ;
      apply_immediately: Boolean.t }
    let make ~replication_group_id  ?new_replica_count 
      ?(replica_configuration= [])  ~apply_immediately  () =
      {
        replication_group_id;
        new_replica_count;
        replica_configuration;
        apply_immediately
      }
    let parse xml =
      Some
        {
          replication_group_id =
            (Xml.required "ReplicationGroupId"
               (Util.option_bind (Xml.member "ReplicationGroupId" xml)
                  String.parse));
          new_replica_count =
            (Util.option_bind (Xml.member "NewReplicaCount" xml)
               Integer.parse);
          replica_configuration =
            (Util.of_option []
               (Util.option_bind (Xml.member "ReplicaConfiguration" xml)
                  ReplicaConfigurationList.parse));
          apply_immediately =
            (Xml.required "ApplyImmediately"
               (Util.option_bind (Xml.member "ApplyImmediately" xml)
                  Boolean.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ApplyImmediately", (Boolean.to_query v.apply_immediately)));
           Some
             (Query.Pair
                ("ReplicaConfiguration.member",
                  (ReplicaConfigurationList.to_query v.replica_configuration)));
           Util.option_map v.new_replica_count
             (fun f -> Query.Pair ("NewReplicaCount", (Integer.to_query f)));
           Some
             (Query.Pair
                ("ReplicationGroupId",
                  (String.to_query v.replication_group_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("apply_immediately", (Boolean.to_json v.apply_immediately));
           Some
             ("replica_configuration",
               (ReplicaConfigurationList.to_json v.replica_configuration));
           Util.option_map v.new_replica_count
             (fun f -> ("new_replica_count", (Integer.to_json f)));
           Some
             ("replication_group_id",
               (String.to_json v.replication_group_id))])
    let of_json j =
      {
        replication_group_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "replication_group_id")));
        new_replica_count =
          (Util.option_map (Json.lookup j "new_replica_count")
             Integer.of_json);
        replica_configuration =
          (ReplicaConfigurationList.of_json
             (Util.of_option_exn (Json.lookup j "replica_configuration")));
        apply_immediately =
          (Boolean.of_json
             (Util.of_option_exn (Json.lookup j "apply_immediately")))
      }
  end
module ModifyCacheParameterGroupMessage =
  struct
    type t =
      {
      cache_parameter_group_name: String.t ;
      parameter_name_values: ParameterNameValueList.t }
    let make ~cache_parameter_group_name  ~parameter_name_values  () =
      { cache_parameter_group_name; parameter_name_values }
    let parse xml =
      Some
        {
          cache_parameter_group_name =
            (Xml.required "CacheParameterGroupName"
               (Util.option_bind (Xml.member "CacheParameterGroupName" xml)
                  String.parse));
          parameter_name_values =
            (Xml.required "ParameterNameValues"
               (Util.option_bind (Xml.member "ParameterNameValues" xml)
                  ParameterNameValueList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ParameterNameValues.member",
                   (ParameterNameValueList.to_query v.parameter_name_values)));
           Some
             (Query.Pair
                ("CacheParameterGroupName",
                  (String.to_query v.cache_parameter_group_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("parameter_name_values",
                (ParameterNameValueList.to_json v.parameter_name_values));
           Some
             ("cache_parameter_group_name",
               (String.to_json v.cache_parameter_group_name))])
    let of_json j =
      {
        cache_parameter_group_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "cache_parameter_group_name")));
        parameter_name_values =
          (ParameterNameValueList.of_json
             (Util.of_option_exn (Json.lookup j "parameter_name_values")))
      }
  end
module GlobalReplicationGroupAlreadyExistsFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ReservedCacheNodeMessage =
  struct
    type t =
      {
      marker: String.t option ;
      reserved_cache_nodes: ReservedCacheNodeList.t }
    let make ?marker  ?(reserved_cache_nodes= [])  () =
      { marker; reserved_cache_nodes }
    let parse xml =
      Some
        {
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          reserved_cache_nodes =
            (Util.of_option []
               (Util.option_bind (Xml.member "ReservedCacheNodes" xml)
                  ReservedCacheNodeList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ReservedCacheNodes.member",
                   (ReservedCacheNodeList.to_query v.reserved_cache_nodes)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("reserved_cache_nodes",
                (ReservedCacheNodeList.to_json v.reserved_cache_nodes));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)))])
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        reserved_cache_nodes =
          (ReservedCacheNodeList.of_json
             (Util.of_option_exn (Json.lookup j "reserved_cache_nodes")))
      }
  end
module NoOperationFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DisassociateGlobalReplicationGroupMessage =
  struct
    type t =
      {
      global_replication_group_id: String.t ;
      replication_group_id: String.t ;
      replication_group_region: String.t }
    let make ~global_replication_group_id  ~replication_group_id 
      ~replication_group_region  () =
      {
        global_replication_group_id;
        replication_group_id;
        replication_group_region
      }
    let parse xml =
      Some
        {
          global_replication_group_id =
            (Xml.required "GlobalReplicationGroupId"
               (Util.option_bind (Xml.member "GlobalReplicationGroupId" xml)
                  String.parse));
          replication_group_id =
            (Xml.required "ReplicationGroupId"
               (Util.option_bind (Xml.member "ReplicationGroupId" xml)
                  String.parse));
          replication_group_region =
            (Xml.required "ReplicationGroupRegion"
               (Util.option_bind (Xml.member "ReplicationGroupRegion" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ReplicationGroupRegion",
                   (String.to_query v.replication_group_region)));
           Some
             (Query.Pair
                ("ReplicationGroupId",
                  (String.to_query v.replication_group_id)));
           Some
             (Query.Pair
                ("GlobalReplicationGroupId",
                  (String.to_query v.global_replication_group_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("replication_group_region",
                (String.to_json v.replication_group_region));
           Some
             ("replication_group_id",
               (String.to_json v.replication_group_id));
           Some
             ("global_replication_group_id",
               (String.to_json v.global_replication_group_id))])
    let of_json j =
      {
        global_replication_group_id =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "global_replication_group_id")));
        replication_group_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "replication_group_id")));
        replication_group_region =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "replication_group_region")))
      }
  end