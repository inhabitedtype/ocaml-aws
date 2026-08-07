open Aws.BaseTypes

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
