open Aws.BaseTypes

type t =
  { reserved_d_b_instances_offering_id : String.t
  ; reserved_d_b_instance_id : String.t option
  ; d_b_instance_count : Integer.t option
  ; tags : TagList.t
  }

let make
    ~reserved_d_b_instances_offering_id
    ?reserved_d_b_instance_id
    ?d_b_instance_count
    ?(tags = [])
    () =
  { reserved_d_b_instances_offering_id
  ; reserved_d_b_instance_id
  ; d_b_instance_count
  ; tags
  }

let parse xml =
  Some
    { reserved_d_b_instances_offering_id =
        Aws.Xml.required
          "ReservedDBInstancesOfferingId"
          (Aws.Util.option_bind
             (Aws.Xml.member "ReservedDBInstancesOfferingId" xml)
             String.parse)
    ; reserved_d_b_instance_id =
        Aws.Util.option_bind (Aws.Xml.member "ReservedDBInstanceId" xml) String.parse
    ; d_b_instance_count =
        Aws.Util.option_bind (Aws.Xml.member "DBInstanceCount" xml) Integer.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Aws.Util.option_map v.d_b_instance_count (fun f ->
             Aws.Query.Pair ("DBInstanceCount", Integer.to_query f))
       ; Aws.Util.option_map v.reserved_d_b_instance_id (fun f ->
             Aws.Query.Pair ("ReservedDBInstanceId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ReservedDBInstancesOfferingId"
              , String.to_query v.reserved_d_b_instances_offering_id ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Tags", TagList.to_json v.tags)
       ; Aws.Util.option_map v.d_b_instance_count (fun f ->
             "DBInstanceCount", Integer.to_json f)
       ; Aws.Util.option_map v.reserved_d_b_instance_id (fun f ->
             "ReservedDBInstanceId", String.to_json f)
       ; Some
           ( "ReservedDBInstancesOfferingId"
           , String.to_json v.reserved_d_b_instances_offering_id )
       ])

let of_json j =
  { reserved_d_b_instances_offering_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ReservedDBInstancesOfferingId"))
  ; reserved_d_b_instance_id =
      Aws.Util.option_map (Aws.Json.lookup j "ReservedDBInstanceId") String.of_json
  ; d_b_instance_count =
      Aws.Util.option_map (Aws.Json.lookup j "DBInstanceCount") Integer.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  }
