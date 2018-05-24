open Aws
open Aws.BaseTypes
open CalendarLib
type calendar = Calendar.t
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
              (fun f  -> Query.Pair ("Port", (Integer.to_query f)));
           Util.option_map v.address
             (fun f  -> Query.Pair ("Address", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.port (fun f  -> ("port", (Integer.to_json f)));
           Util.option_map v.address
             (fun f  -> ("address", (String.to_json f)))])
      
    let of_json j =
      {
        address = (Util.option_map (Json.lookup j "address") String.of_json);
        port = (Util.option_map (Json.lookup j "port") Integer.of_json)
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
      current_role: String.t option }
    let make ?cache_cluster_id  ?cache_node_id  ?read_endpoint 
      ?preferred_availability_zone  ?current_role  () =
      {
        cache_cluster_id;
        cache_node_id;
        read_endpoint;
        preferred_availability_zone;
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
          current_role =
            (Util.option_bind (Xml.member "CurrentRole" xml) String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.current_role
              (fun f  -> Query.Pair ("CurrentRole", (String.to_query f)));
           Util.option_map v.preferred_availability_zone
             (fun f  ->
                Query.Pair ("PreferredAvailabilityZone", (String.to_query f)));
           Util.option_map v.read_endpoint
             (fun f  -> Query.Pair ("ReadEndpoint", (Endpoint.to_query f)));
           Util.option_map v.cache_node_id
             (fun f  -> Query.Pair ("CacheNodeId", (String.to_query f)));
           Util.option_map v.cache_cluster_id
             (fun f  -> Query.Pair ("CacheClusterId", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.current_role
              (fun f  -> ("current_role", (String.to_json f)));
           Util.option_map v.preferred_availability_zone
             (fun f  -> ("preferred_availability_zone", (String.to_json f)));
           Util.option_map v.read_endpoint
             (fun f  -> ("read_endpoint", (Endpoint.to_json f)));
           Util.option_map v.cache_node_id
             (fun f  -> ("cache_node_id", (String.to_json f)));
           Util.option_map v.cache_cluster_id
             (fun f  -> ("cache_cluster_id", (String.to_json f)))])
      
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
              (fun f  -> Query.Pair ("Name", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.name (fun f  -> ("name", (String.to_json f)))])
      
    let of_json j =
      { name = (Util.option_map (Json.lookup j "name") String.of_json) } 
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
              (fun f  -> Query.Pair ("Value", (String.to_query f)));
           Util.option_map v.cache_node_type
             (fun f  -> Query.Pair ("CacheNodeType", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.value (fun f  -> ("value", (String.to_json f)));
           Util.option_map v.cache_node_type
             (fun f  -> ("cache_node_type", (String.to_json f)))])
      
    let of_json j =
      {
        cache_node_type =
          (Util.option_map (Json.lookup j "cache_node_type") String.of_json);
        value = (Util.option_map (Json.lookup j "value") String.of_json)
      } 
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
              (fun f  ->
                 Query.Pair ("RecurringChargeFrequency", (String.to_query f)));
           Util.option_map v.recurring_charge_amount
             (fun f  ->
                Query.Pair ("RecurringChargeAmount", (Double.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.recurring_charge_frequency
              (fun f  -> ("recurring_charge_frequency", (String.to_json f)));
           Util.option_map v.recurring_charge_amount
             (fun f  -> ("recurring_charge_amount", (Double.to_json f)))])
      
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
      cache_node_id: String.t option ;
      cache_size: String.t option ;
      cache_node_create_time: DateTime.t option ;
      snapshot_create_time: DateTime.t option }
    let make ?cache_node_id  ?cache_size  ?cache_node_create_time 
      ?snapshot_create_time  () =
      {
        cache_node_id;
        cache_size;
        cache_node_create_time;
        snapshot_create_time
      } 
    let parse xml =
      Some
        {
          cache_node_id =
            (Util.option_bind (Xml.member "CacheNodeId" xml) String.parse);
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
              (fun f  ->
                 Query.Pair ("SnapshotCreateTime", (DateTime.to_query f)));
           Util.option_map v.cache_node_create_time
             (fun f  ->
                Query.Pair ("CacheNodeCreateTime", (DateTime.to_query f)));
           Util.option_map v.cache_size
             (fun f  -> Query.Pair ("CacheSize", (String.to_query f)));
           Util.option_map v.cache_node_id
             (fun f  -> Query.Pair ("CacheNodeId", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.snapshot_create_time
              (fun f  -> ("snapshot_create_time", (DateTime.to_json f)));
           Util.option_map v.cache_node_create_time
             (fun f  -> ("cache_node_create_time", (DateTime.to_json f)));
           Util.option_map v.cache_size
             (fun f  -> ("cache_size", (String.to_json f)));
           Util.option_map v.cache_node_id
             (fun f  -> ("cache_node_id", (String.to_json f)))])
      
    let of_json j =
      {
        cache_node_id =
          (Util.option_map (Json.lookup j "cache_node_id") String.of_json);
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
module Subnet =
  struct
    type t =
      {
      subnet_identifier: String.t option ;
      subnet_availability_zone: AvailabilityZone.t option }
    let make ?subnet_identifier  ?subnet_availability_zone  () =
      { subnet_identifier; subnet_availability_zone } 
    let parse xml =
      Some
        {
          subnet_identifier =
            (Util.option_bind (Xml.member "SubnetIdentifier" xml)
               String.parse);
          subnet_availability_zone =
            (Util.option_bind (Xml.member "SubnetAvailabilityZone" xml)
               AvailabilityZone.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.subnet_availability_zone
              (fun f  ->
                 Query.Pair
                   ("SubnetAvailabilityZone", (AvailabilityZone.to_query f)));
           Util.option_map v.subnet_identifier
             (fun f  -> Query.Pair ("SubnetIdentifier", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.subnet_availability_zone
              (fun f  ->
                 ("subnet_availability_zone", (AvailabilityZone.to_json f)));
           Util.option_map v.subnet_identifier
             (fun f  -> ("subnet_identifier", (String.to_json f)))])
      
    let of_json j =
      {
        subnet_identifier =
          (Util.option_map (Json.lookup j "subnet_identifier") String.of_json);
        subnet_availability_zone =
          (Util.option_map (Json.lookup j "subnet_availability_zone")
             AvailabilityZone.of_json)
      } 
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
      customer_availability_zone: String.t option }
    let make ?cache_node_id  ?cache_node_status  ?cache_node_create_time 
      ?endpoint  ?parameter_group_status  ?source_cache_node_id 
      ?customer_availability_zone  () =
      {
        cache_node_id;
        cache_node_status;
        cache_node_create_time;
        endpoint;
        parameter_group_status;
        source_cache_node_id;
        customer_availability_zone
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
               String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.customer_availability_zone
              (fun f  ->
                 Query.Pair ("CustomerAvailabilityZone", (String.to_query f)));
           Util.option_map v.source_cache_node_id
             (fun f  -> Query.Pair ("SourceCacheNodeId", (String.to_query f)));
           Util.option_map v.parameter_group_status
             (fun f  ->
                Query.Pair ("ParameterGroupStatus", (String.to_query f)));
           Util.option_map v.endpoint
             (fun f  -> Query.Pair ("Endpoint", (Endpoint.to_query f)));
           Util.option_map v.cache_node_create_time
             (fun f  ->
                Query.Pair ("CacheNodeCreateTime", (DateTime.to_query f)));
           Util.option_map v.cache_node_status
             (fun f  -> Query.Pair ("CacheNodeStatus", (String.to_query f)));
           Util.option_map v.cache_node_id
             (fun f  -> Query.Pair ("CacheNodeId", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.customer_availability_zone
              (fun f  -> ("customer_availability_zone", (String.to_json f)));
           Util.option_map v.source_cache_node_id
             (fun f  -> ("source_cache_node_id", (String.to_json f)));
           Util.option_map v.parameter_group_status
             (fun f  -> ("parameter_group_status", (String.to_json f)));
           Util.option_map v.endpoint
             (fun f  -> ("endpoint", (Endpoint.to_json f)));
           Util.option_map v.cache_node_create_time
             (fun f  -> ("cache_node_create_time", (DateTime.to_json f)));
           Util.option_map v.cache_node_status
             (fun f  -> ("cache_node_status", (String.to_json f)));
           Util.option_map v.cache_node_id
             (fun f  -> ("cache_node_id", (String.to_json f)))])
      
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
              (fun f  -> Query.Pair ("Status", (String.to_query f)));
           Util.option_map v.cache_security_group_name
             (fun f  ->
                Query.Pair ("CacheSecurityGroupName", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.status
              (fun f  -> ("status", (String.to_json f)));
           Util.option_map v.cache_security_group_name
             (fun f  -> ("cache_security_group_name", (String.to_json f)))])
      
    let of_json j =
      {
        cache_security_group_name =
          (Util.option_map (Json.lookup j "cache_security_group_name")
             String.of_json);
        status = (Util.option_map (Json.lookup j "status") String.of_json)
      } 
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
              (fun f  -> Query.Pair ("Status", (String.to_query f)));
           Util.option_map v.security_group_id
             (fun f  -> Query.Pair ("SecurityGroupId", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.status
              (fun f  -> ("status", (String.to_json f)));
           Util.option_map v.security_group_id
             (fun f  -> ("security_group_id", (String.to_json f)))])
      
    let of_json j =
      {
        security_group_id =
          (Util.option_map (Json.lookup j "security_group_id") String.of_json);
        status = (Util.option_map (Json.lookup j "status") String.of_json)
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
module NodeGroup =
  struct
    type t =
      {
      node_group_id: String.t option ;
      status: String.t option ;
      primary_endpoint: Endpoint.t option ;
      node_group_members: NodeGroupMemberList.t }
    let make ?node_group_id  ?status  ?primary_endpoint 
      ?(node_group_members= [])  () =
      { node_group_id; status; primary_endpoint; node_group_members } 
    let parse xml =
      Some
        {
          node_group_id =
            (Util.option_bind (Xml.member "NodeGroupId" xml) String.parse);
          status = (Util.option_bind (Xml.member "Status" xml) String.parse);
          primary_endpoint =
            (Util.option_bind (Xml.member "PrimaryEndpoint" xml)
               Endpoint.parse);
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
           Util.option_map v.primary_endpoint
             (fun f  -> Query.Pair ("PrimaryEndpoint", (Endpoint.to_query f)));
           Util.option_map v.status
             (fun f  -> Query.Pair ("Status", (String.to_query f)));
           Util.option_map v.node_group_id
             (fun f  -> Query.Pair ("NodeGroupId", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("node_group_members",
                (NodeGroupMemberList.to_json v.node_group_members));
           Util.option_map v.primary_endpoint
             (fun f  -> ("primary_endpoint", (Endpoint.to_json f)));
           Util.option_map v.status
             (fun f  -> ("status", (String.to_json f)));
           Util.option_map v.node_group_id
             (fun f  -> ("node_group_id", (String.to_json f)))])
      
    let of_json j =
      {
        node_group_id =
          (Util.option_map (Json.lookup j "node_group_id") String.of_json);
        status = (Util.option_map (Json.lookup j "status") String.of_json);
        primary_endpoint =
          (Util.option_map (Json.lookup j "primary_endpoint")
             Endpoint.of_json);
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
    let make v () = v 
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s  -> Util.list_find str_to_t s)
      
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v))) 
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v)) 
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j)) 
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
              (fun f  ->
                 Query.Pair ("EC2SecurityGroupOwnerId", (String.to_query f)));
           Util.option_map v.e_c2_security_group_name
             (fun f  ->
                Query.Pair ("EC2SecurityGroupName", (String.to_query f)));
           Util.option_map v.status
             (fun f  -> Query.Pair ("Status", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.e_c2_security_group_owner_id
              (fun f  -> ("e_c2_security_group_owner_id", (String.to_json f)));
           Util.option_map v.e_c2_security_group_name
             (fun f  -> ("e_c2_security_group_name", (String.to_json f)));
           Util.option_map v.status
             (fun f  -> ("status", (String.to_json f)))])
      
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
             (fun f  ->
                Query.Pair ("ParameterApplyStatus", (String.to_query f)));
           Util.option_map v.cache_parameter_group_name
             (fun f  ->
                Query.Pair ("CacheParameterGroupName", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("cache_node_ids_to_reboot",
                (CacheNodeIdsList.to_json v.cache_node_ids_to_reboot));
           Util.option_map v.parameter_apply_status
             (fun f  -> ("parameter_apply_status", (String.to_json f)));
           Util.option_map v.cache_parameter_group_name
             (fun f  -> ("cache_parameter_group_name", (String.to_json f)))])
      
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
              (fun f  -> Query.Pair ("TopicStatus", (String.to_query f)));
           Util.option_map v.topic_arn
             (fun f  -> Query.Pair ("TopicArn", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.topic_status
              (fun f  -> ("topic_status", (String.to_json f)));
           Util.option_map v.topic_arn
             (fun f  -> ("topic_arn", (String.to_json f)))])
      
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
      engine_version: String.t option }
    let make ?num_cache_nodes  ?(cache_node_ids_to_remove= []) 
      ?engine_version  () =
      { num_cache_nodes; cache_node_ids_to_remove; engine_version } 
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
            (Util.option_bind (Xml.member "EngineVersion" xml) String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.engine_version
              (fun f  -> Query.Pair ("EngineVersion", (String.to_query f)));
           Some
             (Query.Pair
                ("CacheNodeIdsToRemove.member",
                  (CacheNodeIdsList.to_query v.cache_node_ids_to_remove)));
           Util.option_map v.num_cache_nodes
             (fun f  -> Query.Pair ("NumCacheNodes", (Integer.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.engine_version
              (fun f  -> ("engine_version", (String.to_json f)));
           Some
             ("cache_node_ids_to_remove",
               (CacheNodeIdsList.to_json v.cache_node_ids_to_remove));
           Util.option_map v.num_cache_nodes
             (fun f  -> ("num_cache_nodes", (Integer.to_json f)))])
      
    let of_json j =
      {
        num_cache_nodes =
          (Util.option_map (Json.lookup j "num_cache_nodes") Integer.of_json);
        cache_node_ids_to_remove =
          (CacheNodeIdsList.of_json
             (Util.of_option_exn (Json.lookup j "cache_node_ids_to_remove")));
        engine_version =
          (Util.option_map (Json.lookup j "engine_version") String.of_json)
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
      cache_node_type_specific_values: CacheNodeTypeSpecificValueList.t }
    let make ?parameter_name  ?description  ?source  ?data_type 
      ?allowed_values  ?is_modifiable  ?minimum_engine_version 
      ?(cache_node_type_specific_values= [])  () =
      {
        parameter_name;
        description;
        source;
        data_type;
        allowed_values;
        is_modifiable;
        minimum_engine_version;
        cache_node_type_specific_values
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
                  CacheNodeTypeSpecificValueList.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("CacheNodeTypeSpecificValues.member",
                   (CacheNodeTypeSpecificValueList.to_query
                      v.cache_node_type_specific_values)));
           Util.option_map v.minimum_engine_version
             (fun f  ->
                Query.Pair ("MinimumEngineVersion", (String.to_query f)));
           Util.option_map v.is_modifiable
             (fun f  -> Query.Pair ("IsModifiable", (Boolean.to_query f)));
           Util.option_map v.allowed_values
             (fun f  -> Query.Pair ("AllowedValues", (String.to_query f)));
           Util.option_map v.data_type
             (fun f  -> Query.Pair ("DataType", (String.to_query f)));
           Util.option_map v.source
             (fun f  -> Query.Pair ("Source", (String.to_query f)));
           Util.option_map v.description
             (fun f  -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.parameter_name
             (fun f  -> Query.Pair ("ParameterName", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("cache_node_type_specific_values",
                (CacheNodeTypeSpecificValueList.to_json
                   v.cache_node_type_specific_values));
           Util.option_map v.minimum_engine_version
             (fun f  -> ("minimum_engine_version", (String.to_json f)));
           Util.option_map v.is_modifiable
             (fun f  -> ("is_modifiable", (Boolean.to_json f)));
           Util.option_map v.allowed_values
             (fun f  -> ("allowed_values", (String.to_json f)));
           Util.option_map v.data_type
             (fun f  -> ("data_type", (String.to_json f)));
           Util.option_map v.source
             (fun f  -> ("source", (String.to_json f)));
           Util.option_map v.description
             (fun f  -> ("description", (String.to_json f)));
           Util.option_map v.parameter_name
             (fun f  -> ("parameter_name", (String.to_json f)))])
      
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
                (Json.lookup j "cache_node_type_specific_values")))
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
      minimum_engine_version: String.t option }
    let make ?parameter_name  ?parameter_value  ?description  ?source 
      ?data_type  ?allowed_values  ?is_modifiable  ?minimum_engine_version 
      () =
      {
        parameter_name;
        parameter_value;
        description;
        source;
        data_type;
        allowed_values;
        is_modifiable;
        minimum_engine_version
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
               String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.minimum_engine_version
              (fun f  ->
                 Query.Pair ("MinimumEngineVersion", (String.to_query f)));
           Util.option_map v.is_modifiable
             (fun f  -> Query.Pair ("IsModifiable", (Boolean.to_query f)));
           Util.option_map v.allowed_values
             (fun f  -> Query.Pair ("AllowedValues", (String.to_query f)));
           Util.option_map v.data_type
             (fun f  -> Query.Pair ("DataType", (String.to_query f)));
           Util.option_map v.source
             (fun f  -> Query.Pair ("Source", (String.to_query f)));
           Util.option_map v.description
             (fun f  -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.parameter_value
             (fun f  -> Query.Pair ("ParameterValue", (String.to_query f)));
           Util.option_map v.parameter_name
             (fun f  -> Query.Pair ("ParameterName", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.minimum_engine_version
              (fun f  -> ("minimum_engine_version", (String.to_json f)));
           Util.option_map v.is_modifiable
             (fun f  -> ("is_modifiable", (Boolean.to_json f)));
           Util.option_map v.allowed_values
             (fun f  -> ("allowed_values", (String.to_json f)));
           Util.option_map v.data_type
             (fun f  -> ("data_type", (String.to_json f)));
           Util.option_map v.source
             (fun f  -> ("source", (String.to_json f)));
           Util.option_map v.description
             (fun f  -> ("description", (String.to_json f)));
           Util.option_map v.parameter_value
             (fun f  -> ("parameter_value", (String.to_json f)));
           Util.option_map v.parameter_name
             (fun f  -> ("parameter_name", (String.to_json f)))])
      
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
             String.of_json)
      } 
  end
module SourceType =
  struct
    type t =
      | Cache_cluster 
      | Cache_parameter_group 
      | Cache_security_group 
      | Cache_subnet_group 
    let str_to_t =
      [("cache-subnet-group", Cache_subnet_group);
      ("cache-security-group", Cache_security_group);
      ("cache-parameter-group", Cache_parameter_group);
      ("cache-cluster", Cache_cluster)] 
    let t_to_str =
      [(Cache_subnet_group, "cache-subnet-group");
      (Cache_security_group, "cache-security-group");
      (Cache_parameter_group, "cache-parameter-group");
      (Cache_cluster, "cache-cluster")] 
    let make v () = v 
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s  -> Util.list_find str_to_t s)
      
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v))) 
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v)) 
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j)) 
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
    let make v () = v 
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s  -> Util.list_find str_to_t s)
      
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
module ReplicationGroupPendingModifiedValues =
  struct
    type t =
      {
      primary_cluster_id: String.t option ;
      automatic_failover_status: PendingAutomaticFailoverStatus.t option }
    let make ?primary_cluster_id  ?automatic_failover_status  () =
      { primary_cluster_id; automatic_failover_status } 
    let parse xml =
      Some
        {
          primary_cluster_id =
            (Util.option_bind (Xml.member "PrimaryClusterId" xml)
               String.parse);
          automatic_failover_status =
            (Util.option_bind (Xml.member "AutomaticFailoverStatus" xml)
               PendingAutomaticFailoverStatus.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.automatic_failover_status
              (fun f  ->
                 Query.Pair
                   ("AutomaticFailoverStatus",
                     (PendingAutomaticFailoverStatus.to_query f)));
           Util.option_map v.primary_cluster_id
             (fun f  -> Query.Pair ("PrimaryClusterId", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.automatic_failover_status
              (fun f  ->
                 ("automatic_failover_status",
                   (PendingAutomaticFailoverStatus.to_json f)));
           Util.option_map v.primary_cluster_id
             (fun f  -> ("primary_cluster_id", (String.to_json f)))])
      
    let of_json j =
      {
        primary_cluster_id =
          (Util.option_map (Json.lookup j "primary_cluster_id")
             String.of_json);
        automatic_failover_status =
          (Util.option_map (Json.lookup j "automatic_failover_status")
             PendingAutomaticFailoverStatus.of_json)
      } 
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
             (fun f  -> Query.Pair ("OfferingType", (String.to_query f)));
           Util.option_map v.product_description
             (fun f  ->
                Query.Pair ("ProductDescription", (String.to_query f)));
           Util.option_map v.usage_price
             (fun f  -> Query.Pair ("UsagePrice", (Double.to_query f)));
           Util.option_map v.fixed_price
             (fun f  -> Query.Pair ("FixedPrice", (Double.to_query f)));
           Util.option_map v.duration
             (fun f  -> Query.Pair ("Duration", (Integer.to_query f)));
           Util.option_map v.cache_node_type
             (fun f  -> Query.Pair ("CacheNodeType", (String.to_query f)));
           Util.option_map v.reserved_cache_nodes_offering_id
             (fun f  ->
                Query.Pair
                  ("ReservedCacheNodesOfferingId", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("recurring_charges",
                (RecurringChargeList.to_json v.recurring_charges));
           Util.option_map v.offering_type
             (fun f  -> ("offering_type", (String.to_json f)));
           Util.option_map v.product_description
             (fun f  -> ("product_description", (String.to_json f)));
           Util.option_map v.usage_price
             (fun f  -> ("usage_price", (Double.to_json f)));
           Util.option_map v.fixed_price
             (fun f  -> ("fixed_price", (Double.to_json f)));
           Util.option_map v.duration
             (fun f  -> ("duration", (Integer.to_json f)));
           Util.option_map v.cache_node_type
             (fun f  -> ("cache_node_type", (String.to_json f)));
           Util.option_map v.reserved_cache_nodes_offering_id
             (fun f  ->
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
module CacheParameterGroup =
  struct
    type t =
      {
      cache_parameter_group_name: String.t option ;
      cache_parameter_group_family: String.t option ;
      description: String.t option }
    let make ?cache_parameter_group_name  ?cache_parameter_group_family 
      ?description  () =
      { cache_parameter_group_name; cache_parameter_group_family; description
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
            (Util.option_bind (Xml.member "Description" xml) String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.description
              (fun f  -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.cache_parameter_group_family
             (fun f  ->
                Query.Pair ("CacheParameterGroupFamily", (String.to_query f)));
           Util.option_map v.cache_parameter_group_name
             (fun f  ->
                Query.Pair ("CacheParameterGroupName", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.description
              (fun f  -> ("description", (String.to_json f)));
           Util.option_map v.cache_parameter_group_family
             (fun f  -> ("cache_parameter_group_family", (String.to_json f)));
           Util.option_map v.cache_parameter_group_name
             (fun f  -> ("cache_parameter_group_name", (String.to_json f)))])
      
    let of_json j =
      {
        cache_parameter_group_name =
          (Util.option_map (Json.lookup j "cache_parameter_group_name")
             String.of_json);
        cache_parameter_group_family =
          (Util.option_map (Json.lookup j "cache_parameter_group_family")
             String.of_json);
        description =
          (Util.option_map (Json.lookup j "description") String.of_json)
      } 
  end
module Snapshot =
  struct
    type t =
      {
      snapshot_name: String.t option ;
      cache_cluster_id: String.t option ;
      snapshot_status: String.t option ;
      snapshot_source: String.t option ;
      cache_node_type: String.t option ;
      engine: String.t option ;
      engine_version: String.t option ;
      num_cache_nodes: Integer.t option ;
      preferred_availability_zone: String.t option ;
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
      node_snapshots: NodeSnapshotList.t }
    let make ?snapshot_name  ?cache_cluster_id  ?snapshot_status 
      ?snapshot_source  ?cache_node_type  ?engine  ?engine_version 
      ?num_cache_nodes  ?preferred_availability_zone 
      ?cache_cluster_create_time  ?preferred_maintenance_window  ?topic_arn 
      ?port  ?cache_parameter_group_name  ?cache_subnet_group_name  ?vpc_id 
      ?auto_minor_version_upgrade  ?snapshot_retention_limit 
      ?snapshot_window  ?(node_snapshots= [])  () =
      {
        snapshot_name;
        cache_cluster_id;
        snapshot_status;
        snapshot_source;
        cache_node_type;
        engine;
        engine_version;
        num_cache_nodes;
        preferred_availability_zone;
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
        node_snapshots
      } 
    let parse xml =
      Some
        {
          snapshot_name =
            (Util.option_bind (Xml.member "SnapshotName" xml) String.parse);
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
          node_snapshots =
            (Util.of_option []
               (Util.option_bind (Xml.member "NodeSnapshots" xml)
                  NodeSnapshotList.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("NodeSnapshots.member",
                   (NodeSnapshotList.to_query v.node_snapshots)));
           Util.option_map v.snapshot_window
             (fun f  -> Query.Pair ("SnapshotWindow", (String.to_query f)));
           Util.option_map v.snapshot_retention_limit
             (fun f  ->
                Query.Pair ("SnapshotRetentionLimit", (Integer.to_query f)));
           Util.option_map v.auto_minor_version_upgrade
             (fun f  ->
                Query.Pair ("AutoMinorVersionUpgrade", (Boolean.to_query f)));
           Util.option_map v.vpc_id
             (fun f  -> Query.Pair ("VpcId", (String.to_query f)));
           Util.option_map v.cache_subnet_group_name
             (fun f  ->
                Query.Pair ("CacheSubnetGroupName", (String.to_query f)));
           Util.option_map v.cache_parameter_group_name
             (fun f  ->
                Query.Pair ("CacheParameterGroupName", (String.to_query f)));
           Util.option_map v.port
             (fun f  -> Query.Pair ("Port", (Integer.to_query f)));
           Util.option_map v.topic_arn
             (fun f  -> Query.Pair ("TopicArn", (String.to_query f)));
           Util.option_map v.preferred_maintenance_window
             (fun f  ->
                Query.Pair
                  ("PreferredMaintenanceWindow", (String.to_query f)));
           Util.option_map v.cache_cluster_create_time
             (fun f  ->
                Query.Pair ("CacheClusterCreateTime", (DateTime.to_query f)));
           Util.option_map v.preferred_availability_zone
             (fun f  ->
                Query.Pair ("PreferredAvailabilityZone", (String.to_query f)));
           Util.option_map v.num_cache_nodes
             (fun f  -> Query.Pair ("NumCacheNodes", (Integer.to_query f)));
           Util.option_map v.engine_version
             (fun f  -> Query.Pair ("EngineVersion", (String.to_query f)));
           Util.option_map v.engine
             (fun f  -> Query.Pair ("Engine", (String.to_query f)));
           Util.option_map v.cache_node_type
             (fun f  -> Query.Pair ("CacheNodeType", (String.to_query f)));
           Util.option_map v.snapshot_source
             (fun f  -> Query.Pair ("SnapshotSource", (String.to_query f)));
           Util.option_map v.snapshot_status
             (fun f  -> Query.Pair ("SnapshotStatus", (String.to_query f)));
           Util.option_map v.cache_cluster_id
             (fun f  -> Query.Pair ("CacheClusterId", (String.to_query f)));
           Util.option_map v.snapshot_name
             (fun f  -> Query.Pair ("SnapshotName", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("node_snapshots", (NodeSnapshotList.to_json v.node_snapshots));
           Util.option_map v.snapshot_window
             (fun f  -> ("snapshot_window", (String.to_json f)));
           Util.option_map v.snapshot_retention_limit
             (fun f  -> ("snapshot_retention_limit", (Integer.to_json f)));
           Util.option_map v.auto_minor_version_upgrade
             (fun f  -> ("auto_minor_version_upgrade", (Boolean.to_json f)));
           Util.option_map v.vpc_id
             (fun f  -> ("vpc_id", (String.to_json f)));
           Util.option_map v.cache_subnet_group_name
             (fun f  -> ("cache_subnet_group_name", (String.to_json f)));
           Util.option_map v.cache_parameter_group_name
             (fun f  -> ("cache_parameter_group_name", (String.to_json f)));
           Util.option_map v.port (fun f  -> ("port", (Integer.to_json f)));
           Util.option_map v.topic_arn
             (fun f  -> ("topic_arn", (String.to_json f)));
           Util.option_map v.preferred_maintenance_window
             (fun f  -> ("preferred_maintenance_window", (String.to_json f)));
           Util.option_map v.cache_cluster_create_time
             (fun f  -> ("cache_cluster_create_time", (DateTime.to_json f)));
           Util.option_map v.preferred_availability_zone
             (fun f  -> ("preferred_availability_zone", (String.to_json f)));
           Util.option_map v.num_cache_nodes
             (fun f  -> ("num_cache_nodes", (Integer.to_json f)));
           Util.option_map v.engine_version
             (fun f  -> ("engine_version", (String.to_json f)));
           Util.option_map v.engine
             (fun f  -> ("engine", (String.to_json f)));
           Util.option_map v.cache_node_type
             (fun f  -> ("cache_node_type", (String.to_json f)));
           Util.option_map v.snapshot_source
             (fun f  -> ("snapshot_source", (String.to_json f)));
           Util.option_map v.snapshot_status
             (fun f  -> ("snapshot_status", (String.to_json f)));
           Util.option_map v.cache_cluster_id
             (fun f  -> ("cache_cluster_id", (String.to_json f)));
           Util.option_map v.snapshot_name
             (fun f  -> ("snapshot_name", (String.to_json f)))])
      
    let of_json j =
      {
        snapshot_name =
          (Util.option_map (Json.lookup j "snapshot_name") String.of_json);
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
        node_snapshots =
          (NodeSnapshotList.of_json
             (Util.of_option_exn (Json.lookup j "node_snapshots")))
      } 
  end
module CacheSubnetGroup =
  struct
    type t =
      {
      cache_subnet_group_name: String.t option ;
      cache_subnet_group_description: String.t option ;
      vpc_id: String.t option ;
      subnets: SubnetList.t }
    let make ?cache_subnet_group_name  ?cache_subnet_group_description 
      ?vpc_id  ?(subnets= [])  () =
      {
        cache_subnet_group_name;
        cache_subnet_group_description;
        vpc_id;
        subnets
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
               (Util.option_bind (Xml.member "Subnets" xml) SubnetList.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("Subnets.member", (SubnetList.to_query v.subnets)));
           Util.option_map v.vpc_id
             (fun f  -> Query.Pair ("VpcId", (String.to_query f)));
           Util.option_map v.cache_subnet_group_description
             (fun f  ->
                Query.Pair
                  ("CacheSubnetGroupDescription", (String.to_query f)));
           Util.option_map v.cache_subnet_group_name
             (fun f  ->
                Query.Pair ("CacheSubnetGroupName", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("subnets", (SubnetList.to_json v.subnets));
           Util.option_map v.vpc_id
             (fun f  -> ("vpc_id", (String.to_json f)));
           Util.option_map v.cache_subnet_group_description
             (fun f  ->
                ("cache_subnet_group_description", (String.to_json f)));
           Util.option_map v.cache_subnet_group_name
             (fun f  -> ("cache_subnet_group_name", (String.to_json f)))])
      
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
          (SubnetList.of_json (Util.of_option_exn (Json.lookup j "subnets")))
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
              (fun f  -> Query.Pair ("Value", (String.to_query f)));
           Util.option_map v.key
             (fun f  -> Query.Pair ("Key", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.value (fun f  -> ("value", (String.to_json f)));
           Util.option_map v.key (fun f  -> ("key", (String.to_json f)))])
      
    let of_json j =
      {
        key = (Util.option_map (Json.lookup j "key") String.of_json);
        value = (Util.option_map (Json.lookup j "value") String.of_json)
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
              (fun f  -> Query.Pair ("ParameterValue", (String.to_query f)));
           Util.option_map v.parameter_name
             (fun f  -> Query.Pair ("ParameterName", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.parameter_value
              (fun f  -> ("parameter_value", (String.to_json f)));
           Util.option_map v.parameter_name
             (fun f  -> ("parameter_name", (String.to_json f)))])
      
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
      snapshot_window: String.t option }
    let make ?cache_cluster_id  ?configuration_endpoint 
      ?client_download_landing_page  ?cache_node_type  ?engine 
      ?engine_version  ?cache_cluster_status  ?num_cache_nodes 
      ?preferred_availability_zone  ?cache_cluster_create_time 
      ?preferred_maintenance_window  ?pending_modified_values 
      ?notification_configuration  ?(cache_security_groups= []) 
      ?cache_parameter_group  ?cache_subnet_group_name  ?(cache_nodes= []) 
      ?auto_minor_version_upgrade  ?(security_groups= []) 
      ?replication_group_id  ?snapshot_retention_limit  ?snapshot_window  ()
      =
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
        snapshot_window
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
            (Util.option_bind (Xml.member "SnapshotWindow" xml) String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.snapshot_window
              (fun f  -> Query.Pair ("SnapshotWindow", (String.to_query f)));
           Util.option_map v.snapshot_retention_limit
             (fun f  ->
                Query.Pair ("SnapshotRetentionLimit", (Integer.to_query f)));
           Util.option_map v.replication_group_id
             (fun f  ->
                Query.Pair ("ReplicationGroupId", (String.to_query f)));
           Some
             (Query.Pair
                ("SecurityGroups.member",
                  (SecurityGroupMembershipList.to_query v.security_groups)));
           Util.option_map v.auto_minor_version_upgrade
             (fun f  ->
                Query.Pair ("AutoMinorVersionUpgrade", (Boolean.to_query f)));
           Some
             (Query.Pair
                ("CacheNodes.member", (CacheNodeList.to_query v.cache_nodes)));
           Util.option_map v.cache_subnet_group_name
             (fun f  ->
                Query.Pair ("CacheSubnetGroupName", (String.to_query f)));
           Util.option_map v.cache_parameter_group
             (fun f  ->
                Query.Pair
                  ("CacheParameterGroup",
                    (CacheParameterGroupStatus.to_query f)));
           Some
             (Query.Pair
                ("CacheSecurityGroups.member",
                  (CacheSecurityGroupMembershipList.to_query
                     v.cache_security_groups)));
           Util.option_map v.notification_configuration
             (fun f  ->
                Query.Pair
                  ("NotificationConfiguration",
                    (NotificationConfiguration.to_query f)));
           Util.option_map v.pending_modified_values
             (fun f  ->
                Query.Pair
                  ("PendingModifiedValues",
                    (PendingModifiedValues.to_query f)));
           Util.option_map v.preferred_maintenance_window
             (fun f  ->
                Query.Pair
                  ("PreferredMaintenanceWindow", (String.to_query f)));
           Util.option_map v.cache_cluster_create_time
             (fun f  ->
                Query.Pair ("CacheClusterCreateTime", (DateTime.to_query f)));
           Util.option_map v.preferred_availability_zone
             (fun f  ->
                Query.Pair ("PreferredAvailabilityZone", (String.to_query f)));
           Util.option_map v.num_cache_nodes
             (fun f  -> Query.Pair ("NumCacheNodes", (Integer.to_query f)));
           Util.option_map v.cache_cluster_status
             (fun f  ->
                Query.Pair ("CacheClusterStatus", (String.to_query f)));
           Util.option_map v.engine_version
             (fun f  -> Query.Pair ("EngineVersion", (String.to_query f)));
           Util.option_map v.engine
             (fun f  -> Query.Pair ("Engine", (String.to_query f)));
           Util.option_map v.cache_node_type
             (fun f  -> Query.Pair ("CacheNodeType", (String.to_query f)));
           Util.option_map v.client_download_landing_page
             (fun f  ->
                Query.Pair ("ClientDownloadLandingPage", (String.to_query f)));
           Util.option_map v.configuration_endpoint
             (fun f  ->
                Query.Pair ("ConfigurationEndpoint", (Endpoint.to_query f)));
           Util.option_map v.cache_cluster_id
             (fun f  -> Query.Pair ("CacheClusterId", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.snapshot_window
              (fun f  -> ("snapshot_window", (String.to_json f)));
           Util.option_map v.snapshot_retention_limit
             (fun f  -> ("snapshot_retention_limit", (Integer.to_json f)));
           Util.option_map v.replication_group_id
             (fun f  -> ("replication_group_id", (String.to_json f)));
           Some
             ("security_groups",
               (SecurityGroupMembershipList.to_json v.security_groups));
           Util.option_map v.auto_minor_version_upgrade
             (fun f  -> ("auto_minor_version_upgrade", (Boolean.to_json f)));
           Some ("cache_nodes", (CacheNodeList.to_json v.cache_nodes));
           Util.option_map v.cache_subnet_group_name
             (fun f  -> ("cache_subnet_group_name", (String.to_json f)));
           Util.option_map v.cache_parameter_group
             (fun f  ->
                ("cache_parameter_group",
                  (CacheParameterGroupStatus.to_json f)));
           Some
             ("cache_security_groups",
               (CacheSecurityGroupMembershipList.to_json
                  v.cache_security_groups));
           Util.option_map v.notification_configuration
             (fun f  ->
                ("notification_configuration",
                  (NotificationConfiguration.to_json f)));
           Util.option_map v.pending_modified_values
             (fun f  ->
                ("pending_modified_values",
                  (PendingModifiedValues.to_json f)));
           Util.option_map v.preferred_maintenance_window
             (fun f  -> ("preferred_maintenance_window", (String.to_json f)));
           Util.option_map v.cache_cluster_create_time
             (fun f  -> ("cache_cluster_create_time", (DateTime.to_json f)));
           Util.option_map v.preferred_availability_zone
             (fun f  -> ("preferred_availability_zone", (String.to_json f)));
           Util.option_map v.num_cache_nodes
             (fun f  -> ("num_cache_nodes", (Integer.to_json f)));
           Util.option_map v.cache_cluster_status
             (fun f  -> ("cache_cluster_status", (String.to_json f)));
           Util.option_map v.engine_version
             (fun f  -> ("engine_version", (String.to_json f)));
           Util.option_map v.engine
             (fun f  -> ("engine", (String.to_json f)));
           Util.option_map v.cache_node_type
             (fun f  -> ("cache_node_type", (String.to_json f)));
           Util.option_map v.client_download_landing_page
             (fun f  -> ("client_download_landing_page", (String.to_json f)));
           Util.option_map v.configuration_endpoint
             (fun f  -> ("configuration_endpoint", (Endpoint.to_json f)));
           Util.option_map v.cache_cluster_id
             (fun f  -> ("cache_cluster_id", (String.to_json f)))])
      
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
          (Util.option_map (Json.lookup j "snapshot_window") String.of_json)
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
              (fun f  -> Query.Pair ("Date", (DateTime.to_query f)));
           Util.option_map v.message
             (fun f  -> Query.Pair ("Message", (String.to_query f)));
           Util.option_map v.source_type
             (fun f  -> Query.Pair ("SourceType", (SourceType.to_query f)));
           Util.option_map v.source_identifier
             (fun f  -> Query.Pair ("SourceIdentifier", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.date (fun f  -> ("date", (DateTime.to_json f)));
           Util.option_map v.message
             (fun f  -> ("message", (String.to_json f)));
           Util.option_map v.source_type
             (fun f  -> ("source_type", (SourceType.to_json f)));
           Util.option_map v.source_identifier
             (fun f  -> ("source_identifier", (String.to_json f)))])
      
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
module ReplicationGroup =
  struct
    type t =
      {
      replication_group_id: String.t option ;
      description: String.t option ;
      status: String.t option ;
      pending_modified_values: ReplicationGroupPendingModifiedValues.t option ;
      member_clusters: ClusterIdList.t ;
      node_groups: NodeGroupList.t ;
      snapshotting_cluster_id: String.t option ;
      automatic_failover: AutomaticFailoverStatus.t option }
    let make ?replication_group_id  ?description  ?status 
      ?pending_modified_values  ?(member_clusters= [])  ?(node_groups= []) 
      ?snapshotting_cluster_id  ?automatic_failover  () =
      {
        replication_group_id;
        description;
        status;
        pending_modified_values;
        member_clusters;
        node_groups;
        snapshotting_cluster_id;
        automatic_failover
      } 
    let parse xml =
      Some
        {
          replication_group_id =
            (Util.option_bind (Xml.member "ReplicationGroupId" xml)
               String.parse);
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse);
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
               AutomaticFailoverStatus.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.automatic_failover
              (fun f  ->
                 Query.Pair
                   ("AutomaticFailover",
                     (AutomaticFailoverStatus.to_query f)));
           Util.option_map v.snapshotting_cluster_id
             (fun f  ->
                Query.Pair ("SnapshottingClusterId", (String.to_query f)));
           Some
             (Query.Pair
                ("NodeGroups.member", (NodeGroupList.to_query v.node_groups)));
           Some
             (Query.Pair
                ("MemberClusters.member",
                  (ClusterIdList.to_query v.member_clusters)));
           Util.option_map v.pending_modified_values
             (fun f  ->
                Query.Pair
                  ("PendingModifiedValues",
                    (ReplicationGroupPendingModifiedValues.to_query f)));
           Util.option_map v.status
             (fun f  -> Query.Pair ("Status", (String.to_query f)));
           Util.option_map v.description
             (fun f  -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.replication_group_id
             (fun f  ->
                Query.Pair ("ReplicationGroupId", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.automatic_failover
              (fun f  ->
                 ("automatic_failover", (AutomaticFailoverStatus.to_json f)));
           Util.option_map v.snapshotting_cluster_id
             (fun f  -> ("snapshotting_cluster_id", (String.to_json f)));
           Some ("node_groups", (NodeGroupList.to_json v.node_groups));
           Some
             ("member_clusters", (ClusterIdList.to_json v.member_clusters));
           Util.option_map v.pending_modified_values
             (fun f  ->
                ("pending_modified_values",
                  (ReplicationGroupPendingModifiedValues.to_json f)));
           Util.option_map v.status
             (fun f  -> ("status", (String.to_json f)));
           Util.option_map v.description
             (fun f  -> ("description", (String.to_json f)));
           Util.option_map v.replication_group_id
             (fun f  -> ("replication_group_id", (String.to_json f)))])
      
    let of_json j =
      {
        replication_group_id =
          (Util.option_map (Json.lookup j "replication_group_id")
             String.of_json);
        description =
          (Util.option_map (Json.lookup j "description") String.of_json);
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
             AutomaticFailoverStatus.of_json)
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
              (fun f  ->
                 Query.Pair
                   ("CacheEngineVersionDescription", (String.to_query f)));
           Util.option_map v.cache_engine_description
             (fun f  ->
                Query.Pair ("CacheEngineDescription", (String.to_query f)));
           Util.option_map v.cache_parameter_group_family
             (fun f  ->
                Query.Pair ("CacheParameterGroupFamily", (String.to_query f)));
           Util.option_map v.engine_version
             (fun f  -> Query.Pair ("EngineVersion", (String.to_query f)));
           Util.option_map v.engine
             (fun f  -> Query.Pair ("Engine", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cache_engine_version_description
              (fun f  ->
                 ("cache_engine_version_description", (String.to_json f)));
           Util.option_map v.cache_engine_description
             (fun f  -> ("cache_engine_description", (String.to_json f)));
           Util.option_map v.cache_parameter_group_family
             (fun f  -> ("cache_parameter_group_family", (String.to_json f)));
           Util.option_map v.engine_version
             (fun f  -> ("engine_version", (String.to_json f)));
           Util.option_map v.engine
             (fun f  -> ("engine", (String.to_json f)))])
      
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
module CacheSecurityGroup =
  struct
    type t =
      {
      owner_id: String.t option ;
      cache_security_group_name: String.t option ;
      description: String.t option ;
      e_c2_security_groups: EC2SecurityGroupList.t }
    let make ?owner_id  ?cache_security_group_name  ?description 
      ?(e_c2_security_groups= [])  () =
      {
        owner_id;
        cache_security_group_name;
        description;
        e_c2_security_groups
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
                  EC2SecurityGroupList.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("EC2SecurityGroups.member",
                   (EC2SecurityGroupList.to_query v.e_c2_security_groups)));
           Util.option_map v.description
             (fun f  -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.cache_security_group_name
             (fun f  ->
                Query.Pair ("CacheSecurityGroupName", (String.to_query f)));
           Util.option_map v.owner_id
             (fun f  -> Query.Pair ("OwnerId", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("e_c2_security_groups",
                (EC2SecurityGroupList.to_json v.e_c2_security_groups));
           Util.option_map v.description
             (fun f  -> ("description", (String.to_json f)));
           Util.option_map v.cache_security_group_name
             (fun f  -> ("cache_security_group_name", (String.to_json f)));
           Util.option_map v.owner_id
             (fun f  -> ("owner_id", (String.to_json f)))])
      
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
             (Util.of_option_exn (Json.lookup j "e_c2_security_groups")))
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
      recurring_charges: RecurringChargeList.t }
    let make ?reserved_cache_node_id  ?reserved_cache_nodes_offering_id 
      ?cache_node_type  ?start_time  ?duration  ?fixed_price  ?usage_price 
      ?cache_node_count  ?product_description  ?offering_type  ?state 
      ?(recurring_charges= [])  () =
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
        recurring_charges
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
                  RecurringChargeList.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("RecurringCharges.member",
                   (RecurringChargeList.to_query v.recurring_charges)));
           Util.option_map v.state
             (fun f  -> Query.Pair ("State", (String.to_query f)));
           Util.option_map v.offering_type
             (fun f  -> Query.Pair ("OfferingType", (String.to_query f)));
           Util.option_map v.product_description
             (fun f  ->
                Query.Pair ("ProductDescription", (String.to_query f)));
           Util.option_map v.cache_node_count
             (fun f  -> Query.Pair ("CacheNodeCount", (Integer.to_query f)));
           Util.option_map v.usage_price
             (fun f  -> Query.Pair ("UsagePrice", (Double.to_query f)));
           Util.option_map v.fixed_price
             (fun f  -> Query.Pair ("FixedPrice", (Double.to_query f)));
           Util.option_map v.duration
             (fun f  -> Query.Pair ("Duration", (Integer.to_query f)));
           Util.option_map v.start_time
             (fun f  -> Query.Pair ("StartTime", (DateTime.to_query f)));
           Util.option_map v.cache_node_type
             (fun f  -> Query.Pair ("CacheNodeType", (String.to_query f)));
           Util.option_map v.reserved_cache_nodes_offering_id
             (fun f  ->
                Query.Pair
                  ("ReservedCacheNodesOfferingId", (String.to_query f)));
           Util.option_map v.reserved_cache_node_id
             (fun f  ->
                Query.Pair ("ReservedCacheNodeId", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("recurring_charges",
                (RecurringChargeList.to_json v.recurring_charges));
           Util.option_map v.state (fun f  -> ("state", (String.to_json f)));
           Util.option_map v.offering_type
             (fun f  -> ("offering_type", (String.to_json f)));
           Util.option_map v.product_description
             (fun f  -> ("product_description", (String.to_json f)));
           Util.option_map v.cache_node_count
             (fun f  -> ("cache_node_count", (Integer.to_json f)));
           Util.option_map v.usage_price
             (fun f  -> ("usage_price", (Double.to_json f)));
           Util.option_map v.fixed_price
             (fun f  -> ("fixed_price", (Double.to_json f)));
           Util.option_map v.duration
             (fun f  -> ("duration", (Integer.to_json f)));
           Util.option_map v.start_time
             (fun f  -> ("start_time", (DateTime.to_json f)));
           Util.option_map v.cache_node_type
             (fun f  -> ("cache_node_type", (String.to_json f)));
           Util.option_map v.reserved_cache_nodes_offering_id
             (fun f  ->
                ("reserved_cache_nodes_offering_id", (String.to_json f)));
           Util.option_map v.reserved_cache_node_id
             (fun f  -> ("reserved_cache_node_id", (String.to_json f)))])
      
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
             (Util.of_option_exn (Json.lookup j "recurring_charges")))
      } 
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
             (fun f  -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.cache_parameter_group_family
             (fun f  ->
                Query.Pair ("CacheParameterGroupFamily", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("cache_node_type_specific_parameters",
                (CacheNodeTypeSpecificParametersList.to_json
                   v.cache_node_type_specific_parameters));
           Some ("parameters", (ParametersList.to_json v.parameters));
           Util.option_map v.marker
             (fun f  -> ("marker", (String.to_json f)));
           Util.option_map v.cache_parameter_group_family
             (fun f  -> ("cache_parameter_group_family", (String.to_json f)))])
      
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
module AZMode =
  struct
    type t =
      | Single_az 
      | Cross_az 
    let str_to_t = [("cross-az", Cross_az); ("single-az", Single_az)] 
    let t_to_str = [(Cross_az, "cross-az"); (Single_az, "single-az")] 
    let make v () = v 
    let parse xml =
      Util.option_bind (String.parse xml)
        (fun s  -> Util.list_find str_to_t s)
      
    let to_query v =
      Query.Value (Some (Util.of_option_exn (Util.list_find t_to_str v))) 
    let to_json v =
      String.to_json (Util.of_option_exn (Util.list_find t_to_str v)) 
    let of_json j =
      Util.of_option_exn (Util.list_find str_to_t (String.of_json j)) 
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
              (fun f  ->
                 Query.Pair
                   ("CacheSecurityGroup", (CacheSecurityGroup.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cache_security_group
              (fun f  ->
                 ("cache_security_group", (CacheSecurityGroup.to_json f)))])
      
    let of_json j =
      {
        cache_security_group =
          (Util.option_map (Json.lookup j "cache_security_group")
             CacheSecurityGroup.of_json)
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
             (fun f  -> Query.Pair ("Marker", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("reserved_cache_nodes_offerings",
                (ReservedCacheNodesOfferingList.to_json
                   v.reserved_cache_nodes_offerings));
           Util.option_map v.marker
             (fun f  -> ("marker", (String.to_json f)))])
      
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        reserved_cache_nodes_offerings =
          (ReservedCacheNodesOfferingList.of_json
             (Util.of_option_exn
                (Json.lookup j "reserved_cache_nodes_offerings")))
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
             (fun f  -> Query.Pair ("Marker", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("cache_parameter_groups",
                (CacheParameterGroupList.to_json v.cache_parameter_groups));
           Util.option_map v.marker
             (fun f  -> ("marker", (String.to_json f)))])
      
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
      show_cache_node_info: Boolean.t option }
    let make ?cache_cluster_id  ?max_records  ?marker  ?show_cache_node_info 
      () = { cache_cluster_id; max_records; marker; show_cache_node_info } 
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
               Boolean.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.show_cache_node_info
              (fun f  ->
                 Query.Pair ("ShowCacheNodeInfo", (Boolean.to_query f)));
           Util.option_map v.marker
             (fun f  -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f  -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Util.option_map v.cache_cluster_id
             (fun f  -> Query.Pair ("CacheClusterId", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.show_cache_node_info
              (fun f  -> ("show_cache_node_info", (Boolean.to_json f)));
           Util.option_map v.marker
             (fun f  -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f  -> ("max_records", (Integer.to_json f)));
           Util.option_map v.cache_cluster_id
             (fun f  -> ("cache_cluster_id", (String.to_json f)))])
      
    let of_json j =
      {
        cache_cluster_id =
          (Util.option_map (Json.lookup j "cache_cluster_id") String.of_json);
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        show_cache_node_info =
          (Util.option_map (Json.lookup j "show_cache_node_info")
             Boolean.of_json)
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
              (fun f  ->
                 Query.Pair ("FinalSnapshotIdentifier", (String.to_query f)));
           Some
             (Query.Pair
                ("CacheClusterId", (String.to_query v.cache_cluster_id)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.final_snapshot_identifier
              (fun f  -> ("final_snapshot_identifier", (String.to_json f)));
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
              (fun f  ->
                 Query.Pair
                   ("ReplicationGroup", (ReplicationGroup.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.replication_group
              (fun f  -> ("replication_group", (ReplicationGroup.to_json f)))])
      
    let of_json j =
      {
        replication_group =
          (Util.option_map (Json.lookup j "replication_group")
             ReplicationGroup.of_json)
      } 
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
             (fun f  -> Query.Pair ("Marker", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("snapshots", (SnapshotList.to_json v.snapshots));
           Util.option_map v.marker
             (fun f  -> ("marker", (String.to_json f)))])
      
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        snapshots =
          (SnapshotList.of_json
             (Util.of_option_exn (Json.lookup j "snapshots")))
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
              (fun f  ->
                 Query.Pair
                   ("ReplicationGroup", (ReplicationGroup.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.replication_group
              (fun f  -> ("replication_group", (ReplicationGroup.to_json f)))])
      
    let of_json j =
      {
        replication_group =
          (Util.option_map (Json.lookup j "replication_group")
             ReplicationGroup.of_json)
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
              (fun f  ->
                 Query.Pair
                   ("CacheSubnetGroup", (CacheSubnetGroup.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cache_subnet_group
              (fun f  -> ("cache_subnet_group", (CacheSubnetGroup.to_json f)))])
      
    let of_json j =
      {
        cache_subnet_group =
          (Util.option_map (Json.lookup j "cache_subnet_group")
             CacheSubnetGroup.of_json)
      } 
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
              (fun f  -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f  -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Util.option_map v.replication_group_id
             (fun f  ->
                Query.Pair ("ReplicationGroupId", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f  -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f  -> ("max_records", (Integer.to_json f)));
           Util.option_map v.replication_group_id
             (fun f  -> ("replication_group_id", (String.to_json f)))])
      
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
              (fun f  ->
                 Query.Pair ("CacheCluster", (CacheCluster.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cache_cluster
              (fun f  -> ("cache_cluster", (CacheCluster.to_json f)))])
      
    let of_json j =
      {
        cache_cluster =
          (Util.option_map (Json.lookup j "cache_cluster")
             CacheCluster.of_json)
      } 
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
             (fun f  -> Query.Pair ("Marker", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("cache_subnet_groups",
                (CacheSubnetGroups.to_json v.cache_subnet_groups));
           Util.option_map v.marker
             (fun f  -> ("marker", (String.to_json f)))])
      
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        cache_subnet_groups =
          (CacheSubnetGroups.of_json
             (Util.of_option_exn (Json.lookup j "cache_subnet_groups")))
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
              (fun f  -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f  -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Util.option_map v.offering_type
             (fun f  -> Query.Pair ("OfferingType", (String.to_query f)));
           Util.option_map v.product_description
             (fun f  ->
                Query.Pair ("ProductDescription", (String.to_query f)));
           Util.option_map v.duration
             (fun f  -> Query.Pair ("Duration", (String.to_query f)));
           Util.option_map v.cache_node_type
             (fun f  -> Query.Pair ("CacheNodeType", (String.to_query f)));
           Util.option_map v.reserved_cache_nodes_offering_id
             (fun f  ->
                Query.Pair
                  ("ReservedCacheNodesOfferingId", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f  -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f  -> ("max_records", (Integer.to_json f)));
           Util.option_map v.offering_type
             (fun f  -> ("offering_type", (String.to_json f)));
           Util.option_map v.product_description
             (fun f  -> ("product_description", (String.to_json f)));
           Util.option_map v.duration
             (fun f  -> ("duration", (String.to_json f)));
           Util.option_map v.cache_node_type
             (fun f  -> ("cache_node_type", (String.to_json f)));
           Util.option_map v.reserved_cache_nodes_offering_id
             (fun f  ->
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
              (fun f  -> Query.Pair ("CacheNodeCount", (Integer.to_query f)));
           Util.option_map v.reserved_cache_node_id
             (fun f  ->
                Query.Pair ("ReservedCacheNodeId", (String.to_query f)));
           Some
             (Query.Pair
                ("ReservedCacheNodesOfferingId",
                  (String.to_query v.reserved_cache_nodes_offering_id)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cache_node_count
              (fun f  -> ("cache_node_count", (Integer.to_json f)));
           Util.option_map v.reserved_cache_node_id
             (fun f  -> ("reserved_cache_node_id", (String.to_json f)));
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
              (fun f  ->
                 Query.Pair ("FinalSnapshotIdentifier", (String.to_query f)));
           Util.option_map v.retain_primary_cluster
             (fun f  ->
                Query.Pair ("RetainPrimaryCluster", (Boolean.to_query f)));
           Some
             (Query.Pair
                ("ReplicationGroupId",
                  (String.to_query v.replication_group_id)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.final_snapshot_identifier
              (fun f  -> ("final_snapshot_identifier", (String.to_json f)));
           Util.option_map v.retain_primary_cluster
             (fun f  -> ("retain_primary_cluster", (Boolean.to_json f)));
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
              (fun f  -> Query.Pair ("Snapshot", (Snapshot.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.snapshot
              (fun f  -> ("snapshot", (Snapshot.to_json f)))])
      
    let of_json j =
      {
        snapshot =
          (Util.option_map (Json.lookup j "snapshot") Snapshot.of_json)
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
              (fun f  -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f  -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Util.option_map v.cache_security_group_name
             (fun f  ->
                Query.Pair ("CacheSecurityGroupName", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f  -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f  -> ("max_records", (Integer.to_json f)));
           Util.option_map v.cache_security_group_name
             (fun f  -> ("cache_security_group_name", (String.to_json f)))])
      
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
module ResetCacheParameterGroupMessage =
  struct
    type t =
      {
      cache_parameter_group_name: String.t ;
      reset_all_parameters: Boolean.t option ;
      parameter_name_values: ParameterNameValueList.t }
    let make ~cache_parameter_group_name  ?reset_all_parameters 
      ~parameter_name_values  () =
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
           Util.option_map v.reset_all_parameters
             (fun f  ->
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
             (fun f  -> ("reset_all_parameters", (Boolean.to_json f)));
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
              (fun f  ->
                 Query.Pair
                   ("CacheSecurityGroup", (CacheSecurityGroup.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cache_security_group
              (fun f  ->
                 ("cache_security_group", (CacheSecurityGroup.to_json f)))])
      
    let of_json j =
      {
        cache_security_group =
          (Util.option_map (Json.lookup j "cache_security_group")
             CacheSecurityGroup.of_json)
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
              (fun f  -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f  -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Util.option_map v.duration
             (fun f  -> Query.Pair ("Duration", (Integer.to_query f)));
           Util.option_map v.end_time
             (fun f  -> Query.Pair ("EndTime", (DateTime.to_query f)));
           Util.option_map v.start_time
             (fun f  -> Query.Pair ("StartTime", (DateTime.to_query f)));
           Util.option_map v.source_type
             (fun f  -> Query.Pair ("SourceType", (SourceType.to_query f)));
           Util.option_map v.source_identifier
             (fun f  -> Query.Pair ("SourceIdentifier", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f  -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f  -> ("max_records", (Integer.to_json f)));
           Util.option_map v.duration
             (fun f  -> ("duration", (Integer.to_json f)));
           Util.option_map v.end_time
             (fun f  -> ("end_time", (DateTime.to_json f)));
           Util.option_map v.start_time
             (fun f  -> ("start_time", (DateTime.to_json f)));
           Util.option_map v.source_type
             (fun f  -> ("source_type", (SourceType.to_json f)));
           Util.option_map v.source_identifier
             (fun f  -> ("source_identifier", (String.to_json f)))])
      
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
             (fun f  ->
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
             (fun f  ->
                ("cache_subnet_group_description", (String.to_json f)));
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
module CreateReplicationGroupMessage =
  struct
    type t =
      {
      replication_group_id: String.t ;
      replication_group_description: String.t ;
      primary_cluster_id: String.t option ;
      automatic_failover_enabled: Boolean.t option ;
      num_cache_clusters: Integer.t option ;
      preferred_cache_cluster_a_zs: AvailabilityZonesList.t ;
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
      snapshot_window: String.t option }
    let make ~replication_group_id  ~replication_group_description 
      ?primary_cluster_id  ?automatic_failover_enabled  ?num_cache_clusters 
      ?(preferred_cache_cluster_a_zs= [])  ?cache_node_type  ?engine 
      ?engine_version  ?cache_parameter_group_name  ?cache_subnet_group_name 
      ?(cache_security_group_names= [])  ?(security_group_ids= [])  ?(tags=
      [])  ?(snapshot_arns= [])  ?snapshot_name 
      ?preferred_maintenance_window  ?port  ?notification_topic_arn 
      ?auto_minor_version_upgrade  ?snapshot_retention_limit 
      ?snapshot_window  () =
      {
        replication_group_id;
        replication_group_description;
        primary_cluster_id;
        automatic_failover_enabled;
        num_cache_clusters;
        preferred_cache_cluster_a_zs;
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
        snapshot_window
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
          primary_cluster_id =
            (Util.option_bind (Xml.member "PrimaryClusterId" xml)
               String.parse);
          automatic_failover_enabled =
            (Util.option_bind (Xml.member "AutomaticFailoverEnabled" xml)
               Boolean.parse);
          num_cache_clusters =
            (Util.option_bind (Xml.member "NumCacheClusters" xml)
               Integer.parse);
          preferred_cache_cluster_a_zs =
            (Util.of_option []
               (Util.option_bind (Xml.member "PreferredCacheClusterAZs" xml)
                  AvailabilityZonesList.parse));
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
            (Util.option_bind (Xml.member "SnapshotWindow" xml) String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.snapshot_window
              (fun f  -> Query.Pair ("SnapshotWindow", (String.to_query f)));
           Util.option_map v.snapshot_retention_limit
             (fun f  ->
                Query.Pair ("SnapshotRetentionLimit", (Integer.to_query f)));
           Util.option_map v.auto_minor_version_upgrade
             (fun f  ->
                Query.Pair ("AutoMinorVersionUpgrade", (Boolean.to_query f)));
           Util.option_map v.notification_topic_arn
             (fun f  ->
                Query.Pair ("NotificationTopicArn", (String.to_query f)));
           Util.option_map v.port
             (fun f  -> Query.Pair ("Port", (Integer.to_query f)));
           Util.option_map v.preferred_maintenance_window
             (fun f  ->
                Query.Pair
                  ("PreferredMaintenanceWindow", (String.to_query f)));
           Util.option_map v.snapshot_name
             (fun f  -> Query.Pair ("SnapshotName", (String.to_query f)));
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
             (fun f  ->
                Query.Pair ("CacheSubnetGroupName", (String.to_query f)));
           Util.option_map v.cache_parameter_group_name
             (fun f  ->
                Query.Pair ("CacheParameterGroupName", (String.to_query f)));
           Util.option_map v.engine_version
             (fun f  -> Query.Pair ("EngineVersion", (String.to_query f)));
           Util.option_map v.engine
             (fun f  -> Query.Pair ("Engine", (String.to_query f)));
           Util.option_map v.cache_node_type
             (fun f  -> Query.Pair ("CacheNodeType", (String.to_query f)));
           Some
             (Query.Pair
                ("PreferredCacheClusterAZs.member",
                  (AvailabilityZonesList.to_query
                     v.preferred_cache_cluster_a_zs)));
           Util.option_map v.num_cache_clusters
             (fun f  -> Query.Pair ("NumCacheClusters", (Integer.to_query f)));
           Util.option_map v.automatic_failover_enabled
             (fun f  ->
                Query.Pair ("AutomaticFailoverEnabled", (Boolean.to_query f)));
           Util.option_map v.primary_cluster_id
             (fun f  -> Query.Pair ("PrimaryClusterId", (String.to_query f)));
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
           [Util.option_map v.snapshot_window
              (fun f  -> ("snapshot_window", (String.to_json f)));
           Util.option_map v.snapshot_retention_limit
             (fun f  -> ("snapshot_retention_limit", (Integer.to_json f)));
           Util.option_map v.auto_minor_version_upgrade
             (fun f  -> ("auto_minor_version_upgrade", (Boolean.to_json f)));
           Util.option_map v.notification_topic_arn
             (fun f  -> ("notification_topic_arn", (String.to_json f)));
           Util.option_map v.port (fun f  -> ("port", (Integer.to_json f)));
           Util.option_map v.preferred_maintenance_window
             (fun f  -> ("preferred_maintenance_window", (String.to_json f)));
           Util.option_map v.snapshot_name
             (fun f  -> ("snapshot_name", (String.to_json f)));
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
             (fun f  -> ("cache_subnet_group_name", (String.to_json f)));
           Util.option_map v.cache_parameter_group_name
             (fun f  -> ("cache_parameter_group_name", (String.to_json f)));
           Util.option_map v.engine_version
             (fun f  -> ("engine_version", (String.to_json f)));
           Util.option_map v.engine
             (fun f  -> ("engine", (String.to_json f)));
           Util.option_map v.cache_node_type
             (fun f  -> ("cache_node_type", (String.to_json f)));
           Some
             ("preferred_cache_cluster_a_zs",
               (AvailabilityZonesList.to_json v.preferred_cache_cluster_a_zs));
           Util.option_map v.num_cache_clusters
             (fun f  -> ("num_cache_clusters", (Integer.to_json f)));
           Util.option_map v.automatic_failover_enabled
             (fun f  -> ("automatic_failover_enabled", (Boolean.to_json f)));
           Util.option_map v.primary_cluster_id
             (fun f  -> ("primary_cluster_id", (String.to_json f)));
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
        primary_cluster_id =
          (Util.option_map (Json.lookup j "primary_cluster_id")
             String.of_json);
        automatic_failover_enabled =
          (Util.option_map (Json.lookup j "automatic_failover_enabled")
             Boolean.of_json);
        num_cache_clusters =
          (Util.option_map (Json.lookup j "num_cache_clusters")
             Integer.of_json);
        preferred_cache_cluster_a_zs =
          (AvailabilityZonesList.of_json
             (Util.of_option_exn
                (Json.lookup j "preferred_cache_cluster_a_zs")));
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
          (Util.option_map (Json.lookup j "snapshot_window") String.of_json)
      } 
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
             (fun f  -> Query.Pair ("Marker", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("cache_clusters", (CacheClusterList.to_json v.cache_clusters));
           Util.option_map v.marker
             (fun f  -> ("marker", (String.to_json f)))])
      
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        cache_clusters =
          (CacheClusterList.of_json
             (Util.of_option_exn (Json.lookup j "cache_clusters")))
      } 
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
              (fun f  -> Query.Pair ("message", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f  -> ("message", (String.to_json f)))])
      
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      } 
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
              (fun f  -> Query.Pair ("Snapshot", (Snapshot.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.snapshot
              (fun f  -> ("snapshot", (Snapshot.to_json f)))])
      
    let of_json j =
      {
        snapshot =
          (Util.option_map (Json.lookup j "snapshot") Snapshot.of_json)
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
              (fun f  ->
                 Query.Pair
                   ("CacheParameterGroup", (CacheParameterGroup.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cache_parameter_group
              (fun f  ->
                 ("cache_parameter_group", (CacheParameterGroup.to_json f)))])
      
    let of_json j =
      {
        cache_parameter_group =
          (Util.option_map (Json.lookup j "cache_parameter_group")
             CacheParameterGroup.of_json)
      } 
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
              (fun f  -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f  -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Util.option_map v.offering_type
             (fun f  -> Query.Pair ("OfferingType", (String.to_query f)));
           Util.option_map v.product_description
             (fun f  ->
                Query.Pair ("ProductDescription", (String.to_query f)));
           Util.option_map v.duration
             (fun f  -> Query.Pair ("Duration", (String.to_query f)));
           Util.option_map v.cache_node_type
             (fun f  -> Query.Pair ("CacheNodeType", (String.to_query f)));
           Util.option_map v.reserved_cache_nodes_offering_id
             (fun f  ->
                Query.Pair
                  ("ReservedCacheNodesOfferingId", (String.to_query f)));
           Util.option_map v.reserved_cache_node_id
             (fun f  ->
                Query.Pair ("ReservedCacheNodeId", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f  -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f  -> ("max_records", (Integer.to_json f)));
           Util.option_map v.offering_type
             (fun f  -> ("offering_type", (String.to_json f)));
           Util.option_map v.product_description
             (fun f  -> ("product_description", (String.to_json f)));
           Util.option_map v.duration
             (fun f  -> ("duration", (String.to_json f)));
           Util.option_map v.cache_node_type
             (fun f  -> ("cache_node_type", (String.to_json f)));
           Util.option_map v.reserved_cache_nodes_offering_id
             (fun f  ->
                ("reserved_cache_nodes_offering_id", (String.to_json f)));
           Util.option_map v.reserved_cache_node_id
             (fun f  -> ("reserved_cache_node_id", (String.to_json f)))])
      
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
              (fun f  ->
                 Query.Pair ("CacheCluster", (CacheCluster.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cache_cluster
              (fun f  -> ("cache_cluster", (CacheCluster.to_json f)))])
      
    let of_json j =
      {
        cache_cluster =
          (Util.option_map (Json.lookup j "cache_cluster")
             CacheCluster.of_json)
      } 
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
              (fun f  ->
                 Query.Pair
                   ("CacheSecurityGroup", (CacheSecurityGroup.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cache_security_group
              (fun f  ->
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
              (fun f  -> Query.Pair ("message", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f  -> ("message", (String.to_json f)))])
      
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      } 
  end
module CreateSnapshotMessage =
  struct
    type t = {
      cache_cluster_id: String.t ;
      snapshot_name: String.t }
    let make ~cache_cluster_id  ~snapshot_name  () =
      { cache_cluster_id; snapshot_name } 
    let parse xml =
      Some
        {
          cache_cluster_id =
            (Xml.required "CacheClusterId"
               (Util.option_bind (Xml.member "CacheClusterId" xml)
                  String.parse));
          snapshot_name =
            (Xml.required "SnapshotName"
               (Util.option_bind (Xml.member "SnapshotName" xml) String.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("SnapshotName", (String.to_query v.snapshot_name)));
           Some
             (Query.Pair
                ("CacheClusterId", (String.to_query v.cache_cluster_id)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("snapshot_name", (String.to_json v.snapshot_name));
           Some ("cache_cluster_id", (String.to_json v.cache_cluster_id))])
      
    let of_json j =
      {
        cache_cluster_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "cache_cluster_id")));
        snapshot_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "snapshot_name")))
      } 
  end
module DescribeSnapshotsMessage =
  struct
    type t =
      {
      cache_cluster_id: String.t option ;
      snapshot_name: String.t option ;
      snapshot_source: String.t option ;
      marker: String.t option ;
      max_records: Integer.t option }
    let make ?cache_cluster_id  ?snapshot_name  ?snapshot_source  ?marker 
      ?max_records  () =
      { cache_cluster_id; snapshot_name; snapshot_source; marker; max_records
      } 
    let parse xml =
      Some
        {
          cache_cluster_id =
            (Util.option_bind (Xml.member "CacheClusterId" xml) String.parse);
          snapshot_name =
            (Util.option_bind (Xml.member "SnapshotName" xml) String.parse);
          snapshot_source =
            (Util.option_bind (Xml.member "SnapshotSource" xml) String.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.max_records
              (fun f  -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Util.option_map v.marker
             (fun f  -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.snapshot_source
             (fun f  -> Query.Pair ("SnapshotSource", (String.to_query f)));
           Util.option_map v.snapshot_name
             (fun f  -> Query.Pair ("SnapshotName", (String.to_query f)));
           Util.option_map v.cache_cluster_id
             (fun f  -> Query.Pair ("CacheClusterId", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.max_records
              (fun f  -> ("max_records", (Integer.to_json f)));
           Util.option_map v.marker
             (fun f  -> ("marker", (String.to_json f)));
           Util.option_map v.snapshot_source
             (fun f  -> ("snapshot_source", (String.to_json f)));
           Util.option_map v.snapshot_name
             (fun f  -> ("snapshot_name", (String.to_json f)));
           Util.option_map v.cache_cluster_id
             (fun f  -> ("cache_cluster_id", (String.to_json f)))])
      
    let of_json j =
      {
        cache_cluster_id =
          (Util.option_map (Json.lookup j "cache_cluster_id") String.of_json);
        snapshot_name =
          (Util.option_map (Json.lookup j "snapshot_name") String.of_json);
        snapshot_source =
          (Util.option_map (Json.lookup j "snapshot_source") String.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json)
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
              (fun f  -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f  -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Util.option_map v.cache_subnet_group_name
             (fun f  ->
                Query.Pair ("CacheSubnetGroupName", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f  -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f  -> ("max_records", (Integer.to_json f)));
           Util.option_map v.cache_subnet_group_name
             (fun f  -> ("cache_subnet_group_name", (String.to_json f)))])
      
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
             (fun f  -> Query.Pair ("Marker", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("events", (EventList.to_json v.events));
           Util.option_map v.marker
             (fun f  -> ("marker", (String.to_json f)))])
      
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
      target_snapshot_name: String.t }
    let make ~source_snapshot_name  ~target_snapshot_name  () =
      { source_snapshot_name; target_snapshot_name } 
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
                  String.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
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
           [Some
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
             (Util.of_option_exn (Json.lookup j "target_snapshot_name")))
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
              (fun f  -> Query.Pair ("Snapshot", (Snapshot.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.snapshot
              (fun f  -> ("snapshot", (Snapshot.to_json f)))])
      
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
      snapshot_window: String.t option }
    let make ~cache_cluster_id  ?replication_group_id  ?a_z_mode 
      ?preferred_availability_zone  ?(preferred_availability_zones= []) 
      ?num_cache_nodes  ?cache_node_type  ?engine  ?engine_version 
      ?cache_parameter_group_name  ?cache_subnet_group_name 
      ?(cache_security_group_names= [])  ?(security_group_ids= [])  ?(tags=
      [])  ?(snapshot_arns= [])  ?snapshot_name 
      ?preferred_maintenance_window  ?port  ?notification_topic_arn 
      ?auto_minor_version_upgrade  ?snapshot_retention_limit 
      ?snapshot_window  () =
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
        snapshot_window
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
            (Util.option_bind (Xml.member "SnapshotWindow" xml) String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.snapshot_window
              (fun f  -> Query.Pair ("SnapshotWindow", (String.to_query f)));
           Util.option_map v.snapshot_retention_limit
             (fun f  ->
                Query.Pair ("SnapshotRetentionLimit", (Integer.to_query f)));
           Util.option_map v.auto_minor_version_upgrade
             (fun f  ->
                Query.Pair ("AutoMinorVersionUpgrade", (Boolean.to_query f)));
           Util.option_map v.notification_topic_arn
             (fun f  ->
                Query.Pair ("NotificationTopicArn", (String.to_query f)));
           Util.option_map v.port
             (fun f  -> Query.Pair ("Port", (Integer.to_query f)));
           Util.option_map v.preferred_maintenance_window
             (fun f  ->
                Query.Pair
                  ("PreferredMaintenanceWindow", (String.to_query f)));
           Util.option_map v.snapshot_name
             (fun f  -> Query.Pair ("SnapshotName", (String.to_query f)));
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
             (fun f  ->
                Query.Pair ("CacheSubnetGroupName", (String.to_query f)));
           Util.option_map v.cache_parameter_group_name
             (fun f  ->
                Query.Pair ("CacheParameterGroupName", (String.to_query f)));
           Util.option_map v.engine_version
             (fun f  -> Query.Pair ("EngineVersion", (String.to_query f)));
           Util.option_map v.engine
             (fun f  -> Query.Pair ("Engine", (String.to_query f)));
           Util.option_map v.cache_node_type
             (fun f  -> Query.Pair ("CacheNodeType", (String.to_query f)));
           Util.option_map v.num_cache_nodes
             (fun f  -> Query.Pair ("NumCacheNodes", (Integer.to_query f)));
           Some
             (Query.Pair
                ("PreferredAvailabilityZones.member",
                  (PreferredAvailabilityZoneList.to_query
                     v.preferred_availability_zones)));
           Util.option_map v.preferred_availability_zone
             (fun f  ->
                Query.Pair ("PreferredAvailabilityZone", (String.to_query f)));
           Util.option_map v.a_z_mode
             (fun f  -> Query.Pair ("AZMode", (AZMode.to_query f)));
           Util.option_map v.replication_group_id
             (fun f  ->
                Query.Pair ("ReplicationGroupId", (String.to_query f)));
           Some
             (Query.Pair
                ("CacheClusterId", (String.to_query v.cache_cluster_id)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.snapshot_window
              (fun f  -> ("snapshot_window", (String.to_json f)));
           Util.option_map v.snapshot_retention_limit
             (fun f  -> ("snapshot_retention_limit", (Integer.to_json f)));
           Util.option_map v.auto_minor_version_upgrade
             (fun f  -> ("auto_minor_version_upgrade", (Boolean.to_json f)));
           Util.option_map v.notification_topic_arn
             (fun f  -> ("notification_topic_arn", (String.to_json f)));
           Util.option_map v.port (fun f  -> ("port", (Integer.to_json f)));
           Util.option_map v.preferred_maintenance_window
             (fun f  -> ("preferred_maintenance_window", (String.to_json f)));
           Util.option_map v.snapshot_name
             (fun f  -> ("snapshot_name", (String.to_json f)));
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
             (fun f  -> ("cache_subnet_group_name", (String.to_json f)));
           Util.option_map v.cache_parameter_group_name
             (fun f  -> ("cache_parameter_group_name", (String.to_json f)));
           Util.option_map v.engine_version
             (fun f  -> ("engine_version", (String.to_json f)));
           Util.option_map v.engine
             (fun f  -> ("engine", (String.to_json f)));
           Util.option_map v.cache_node_type
             (fun f  -> ("cache_node_type", (String.to_json f)));
           Util.option_map v.num_cache_nodes
             (fun f  -> ("num_cache_nodes", (Integer.to_json f)));
           Some
             ("preferred_availability_zones",
               (PreferredAvailabilityZoneList.to_json
                  v.preferred_availability_zones));
           Util.option_map v.preferred_availability_zone
             (fun f  -> ("preferred_availability_zone", (String.to_json f)));
           Util.option_map v.a_z_mode
             (fun f  -> ("a_z_mode", (AZMode.to_json f)));
           Util.option_map v.replication_group_id
             (fun f  -> ("replication_group_id", (String.to_json f)));
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
          (Util.option_map (Json.lookup j "snapshot_window") String.of_json)
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
              (fun f  -> Query.Pair ("DefaultOnly", (Boolean.to_query f)));
           Util.option_map v.marker
             (fun f  -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f  -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Util.option_map v.cache_parameter_group_family
             (fun f  ->
                Query.Pair ("CacheParameterGroupFamily", (String.to_query f)));
           Util.option_map v.engine_version
             (fun f  -> Query.Pair ("EngineVersion", (String.to_query f)));
           Util.option_map v.engine
             (fun f  -> Query.Pair ("Engine", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.default_only
              (fun f  -> ("default_only", (Boolean.to_json f)));
           Util.option_map v.marker
             (fun f  -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f  -> ("max_records", (Integer.to_json f)));
           Util.option_map v.cache_parameter_group_family
             (fun f  -> ("cache_parameter_group_family", (String.to_json f)));
           Util.option_map v.engine_version
             (fun f  -> ("engine_version", (String.to_json f)));
           Util.option_map v.engine
             (fun f  -> ("engine", (String.to_json f)))])
      
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
              (fun f  ->
                 Query.Pair ("CacheCluster", (CacheCluster.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cache_cluster
              (fun f  -> ("cache_cluster", (CacheCluster.to_json f)))])
      
    let of_json j =
      {
        cache_cluster =
          (Util.option_map (Json.lookup j "cache_cluster")
             CacheCluster.of_json)
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
             (fun f  -> Query.Pair ("Marker", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("replication_groups",
                (ReplicationGroupList.to_json v.replication_groups));
           Util.option_map v.marker
             (fun f  -> ("marker", (String.to_json f)))])
      
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
              (fun f  -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f  -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Util.option_map v.source
             (fun f  -> Query.Pair ("Source", (String.to_query f)));
           Some
             (Query.Pair
                ("CacheParameterGroupName",
                  (String.to_query v.cache_parameter_group_name)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f  -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f  -> ("max_records", (Integer.to_json f)));
           Util.option_map v.source
             (fun f  -> ("source", (String.to_json f)));
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
             (fun f  -> Query.Pair ("Marker", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("cache_node_type_specific_parameters",
                (CacheNodeTypeSpecificParametersList.to_json
                   v.cache_node_type_specific_parameters));
           Some ("parameters", (ParametersList.to_json v.parameters));
           Util.option_map v.marker
             (fun f  -> ("marker", (String.to_json f)))])
      
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
      snapshot_window: String.t option }
    let make ~replication_group_id  ?replication_group_description 
      ?primary_cluster_id  ?snapshotting_cluster_id 
      ?automatic_failover_enabled  ?(cache_security_group_names= []) 
      ?(security_group_ids= [])  ?preferred_maintenance_window 
      ?notification_topic_arn  ?cache_parameter_group_name 
      ?notification_topic_status  ?apply_immediately  ?engine_version 
      ?auto_minor_version_upgrade  ?snapshot_retention_limit 
      ?snapshot_window  () =
      {
        replication_group_id;
        replication_group_description;
        primary_cluster_id;
        snapshotting_cluster_id;
        automatic_failover_enabled;
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
        snapshot_window
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
            (Util.option_bind (Xml.member "SnapshotWindow" xml) String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.snapshot_window
              (fun f  -> Query.Pair ("SnapshotWindow", (String.to_query f)));
           Util.option_map v.snapshot_retention_limit
             (fun f  ->
                Query.Pair ("SnapshotRetentionLimit", (Integer.to_query f)));
           Util.option_map v.auto_minor_version_upgrade
             (fun f  ->
                Query.Pair ("AutoMinorVersionUpgrade", (Boolean.to_query f)));
           Util.option_map v.engine_version
             (fun f  -> Query.Pair ("EngineVersion", (String.to_query f)));
           Util.option_map v.apply_immediately
             (fun f  -> Query.Pair ("ApplyImmediately", (Boolean.to_query f)));
           Util.option_map v.notification_topic_status
             (fun f  ->
                Query.Pair ("NotificationTopicStatus", (String.to_query f)));
           Util.option_map v.cache_parameter_group_name
             (fun f  ->
                Query.Pair ("CacheParameterGroupName", (String.to_query f)));
           Util.option_map v.notification_topic_arn
             (fun f  ->
                Query.Pair ("NotificationTopicArn", (String.to_query f)));
           Util.option_map v.preferred_maintenance_window
             (fun f  ->
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
           Util.option_map v.automatic_failover_enabled
             (fun f  ->
                Query.Pair ("AutomaticFailoverEnabled", (Boolean.to_query f)));
           Util.option_map v.snapshotting_cluster_id
             (fun f  ->
                Query.Pair ("SnapshottingClusterId", (String.to_query f)));
           Util.option_map v.primary_cluster_id
             (fun f  -> Query.Pair ("PrimaryClusterId", (String.to_query f)));
           Util.option_map v.replication_group_description
             (fun f  ->
                Query.Pair
                  ("ReplicationGroupDescription", (String.to_query f)));
           Some
             (Query.Pair
                ("ReplicationGroupId",
                  (String.to_query v.replication_group_id)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.snapshot_window
              (fun f  -> ("snapshot_window", (String.to_json f)));
           Util.option_map v.snapshot_retention_limit
             (fun f  -> ("snapshot_retention_limit", (Integer.to_json f)));
           Util.option_map v.auto_minor_version_upgrade
             (fun f  -> ("auto_minor_version_upgrade", (Boolean.to_json f)));
           Util.option_map v.engine_version
             (fun f  -> ("engine_version", (String.to_json f)));
           Util.option_map v.apply_immediately
             (fun f  -> ("apply_immediately", (Boolean.to_json f)));
           Util.option_map v.notification_topic_status
             (fun f  -> ("notification_topic_status", (String.to_json f)));
           Util.option_map v.cache_parameter_group_name
             (fun f  -> ("cache_parameter_group_name", (String.to_json f)));
           Util.option_map v.notification_topic_arn
             (fun f  -> ("notification_topic_arn", (String.to_json f)));
           Util.option_map v.preferred_maintenance_window
             (fun f  -> ("preferred_maintenance_window", (String.to_json f)));
           Some
             ("security_group_ids",
               (SecurityGroupIdsList.to_json v.security_group_ids));
           Some
             ("cache_security_group_names",
               (CacheSecurityGroupNameList.to_json
                  v.cache_security_group_names));
           Util.option_map v.automatic_failover_enabled
             (fun f  -> ("automatic_failover_enabled", (Boolean.to_json f)));
           Util.option_map v.snapshotting_cluster_id
             (fun f  -> ("snapshotting_cluster_id", (String.to_json f)));
           Util.option_map v.primary_cluster_id
             (fun f  -> ("primary_cluster_id", (String.to_json f)));
           Util.option_map v.replication_group_description
             (fun f  -> ("replication_group_description", (String.to_json f)));
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
          (Util.option_map (Json.lookup j "snapshot_window") String.of_json)
      } 
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
             (fun f  -> Query.Pair ("Marker", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("cache_engine_versions",
                (CacheEngineVersionList.to_json v.cache_engine_versions));
           Util.option_map v.marker
             (fun f  -> ("marker", (String.to_json f)))])
      
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
              (fun f  ->
                 Query.Pair
                   ("ReservedCacheNode", (ReservedCacheNode.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.reserved_cache_node
              (fun f  ->
                 ("reserved_cache_node", (ReservedCacheNode.to_json f)))])
      
    let of_json j =
      {
        reserved_cache_node =
          (Util.option_map (Json.lookup j "reserved_cache_node")
             ReservedCacheNode.of_json)
      } 
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
              (fun f  ->
                 Query.Pair
                   ("ReplicationGroup", (ReplicationGroup.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.replication_group
              (fun f  -> ("replication_group", (ReplicationGroup.to_json f)))])
      
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
             (fun f  -> Query.Pair ("Marker", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("cache_security_groups",
                (CacheSecurityGroups.to_json v.cache_security_groups));
           Util.option_map v.marker
             (fun f  -> ("marker", (String.to_json f)))])
      
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        cache_security_groups =
          (CacheSecurityGroups.of_json
             (Util.of_option_exn (Json.lookup j "cache_security_groups")))
      } 
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
              (fun f  -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f  -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Some
             (Query.Pair
                ("CacheParameterGroupFamily",
                  (String.to_query v.cache_parameter_group_family)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f  -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f  -> ("max_records", (Integer.to_json f)));
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
              (fun f  ->
                 Query.Pair ("CacheCluster", (CacheCluster.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cache_cluster
              (fun f  -> ("cache_cluster", (CacheCluster.to_json f)))])
      
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
              (fun f  ->
                 Query.Pair
                   ("CacheSubnetGroup", (CacheSubnetGroup.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cache_subnet_group
              (fun f  -> ("cache_subnet_group", (CacheSubnetGroup.to_json f)))])
      
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
      snapshot_window: String.t option }
    let make ~cache_cluster_id  ?num_cache_nodes  ?(cache_node_ids_to_remove=
      [])  ?a_z_mode  ?(new_availability_zones= []) 
      ?(cache_security_group_names= [])  ?(security_group_ids= []) 
      ?preferred_maintenance_window  ?notification_topic_arn 
      ?cache_parameter_group_name  ?notification_topic_status 
      ?apply_immediately  ?engine_version  ?auto_minor_version_upgrade 
      ?snapshot_retention_limit  ?snapshot_window  () =
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
        snapshot_window
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
            (Util.option_bind (Xml.member "SnapshotWindow" xml) String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.snapshot_window
              (fun f  -> Query.Pair ("SnapshotWindow", (String.to_query f)));
           Util.option_map v.snapshot_retention_limit
             (fun f  ->
                Query.Pair ("SnapshotRetentionLimit", (Integer.to_query f)));
           Util.option_map v.auto_minor_version_upgrade
             (fun f  ->
                Query.Pair ("AutoMinorVersionUpgrade", (Boolean.to_query f)));
           Util.option_map v.engine_version
             (fun f  -> Query.Pair ("EngineVersion", (String.to_query f)));
           Util.option_map v.apply_immediately
             (fun f  -> Query.Pair ("ApplyImmediately", (Boolean.to_query f)));
           Util.option_map v.notification_topic_status
             (fun f  ->
                Query.Pair ("NotificationTopicStatus", (String.to_query f)));
           Util.option_map v.cache_parameter_group_name
             (fun f  ->
                Query.Pair ("CacheParameterGroupName", (String.to_query f)));
           Util.option_map v.notification_topic_arn
             (fun f  ->
                Query.Pair ("NotificationTopicArn", (String.to_query f)));
           Util.option_map v.preferred_maintenance_window
             (fun f  ->
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
             (fun f  -> Query.Pair ("AZMode", (AZMode.to_query f)));
           Some
             (Query.Pair
                ("CacheNodeIdsToRemove.member",
                  (CacheNodeIdsList.to_query v.cache_node_ids_to_remove)));
           Util.option_map v.num_cache_nodes
             (fun f  -> Query.Pair ("NumCacheNodes", (Integer.to_query f)));
           Some
             (Query.Pair
                ("CacheClusterId", (String.to_query v.cache_cluster_id)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.snapshot_window
              (fun f  -> ("snapshot_window", (String.to_json f)));
           Util.option_map v.snapshot_retention_limit
             (fun f  -> ("snapshot_retention_limit", (Integer.to_json f)));
           Util.option_map v.auto_minor_version_upgrade
             (fun f  -> ("auto_minor_version_upgrade", (Boolean.to_json f)));
           Util.option_map v.engine_version
             (fun f  -> ("engine_version", (String.to_json f)));
           Util.option_map v.apply_immediately
             (fun f  -> ("apply_immediately", (Boolean.to_json f)));
           Util.option_map v.notification_topic_status
             (fun f  -> ("notification_topic_status", (String.to_json f)));
           Util.option_map v.cache_parameter_group_name
             (fun f  -> ("cache_parameter_group_name", (String.to_json f)));
           Util.option_map v.notification_topic_arn
             (fun f  -> ("notification_topic_arn", (String.to_json f)));
           Util.option_map v.preferred_maintenance_window
             (fun f  -> ("preferred_maintenance_window", (String.to_json f)));
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
             (fun f  -> ("a_z_mode", (AZMode.to_json f)));
           Some
             ("cache_node_ids_to_remove",
               (CacheNodeIdsList.to_json v.cache_node_ids_to_remove));
           Util.option_map v.num_cache_nodes
             (fun f  -> ("num_cache_nodes", (Integer.to_json f)));
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
          (Util.option_map (Json.lookup j "snapshot_window") String.of_json)
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
              (fun f  ->
                 Query.Pair ("CacheParameterGroupName", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.cache_parameter_group_name
              (fun f  -> ("cache_parameter_group_name", (String.to_json f)))])
      
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
              (fun f  -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f  -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Util.option_map v.cache_parameter_group_name
             (fun f  ->
                Query.Pair ("CacheParameterGroupName", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f  -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f  -> ("max_records", (Integer.to_json f)));
           Util.option_map v.cache_parameter_group_name
             (fun f  -> ("cache_parameter_group_name", (String.to_json f)))])
      
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
             (fun f  -> Query.Pair ("Marker", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("reserved_cache_nodes",
                (ReservedCacheNodeList.to_json v.reserved_cache_nodes));
           Util.option_map v.marker
             (fun f  -> ("marker", (String.to_json f)))])
      
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        reserved_cache_nodes =
          (ReservedCacheNodeList.of_json
             (Util.of_option_exn (Json.lookup j "reserved_cache_nodes")))
      } 
  end