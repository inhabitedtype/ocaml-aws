open Aws.BaseTypes

type t =
  { reserved_d_b_instance_id : String.t option
  ; reserved_d_b_instances_offering_id : String.t option
  ; d_b_instance_class : String.t option
  ; duration : String.t option
  ; product_description : String.t option
  ; offering_type : String.t option
  ; multi_a_z : Boolean.t option
  ; lease_id : String.t option
  ; filters : FilterList.t
  ; max_records : Integer.t option
  ; marker : String.t option
  }

let make
    ?reserved_d_b_instance_id
    ?reserved_d_b_instances_offering_id
    ?d_b_instance_class
    ?duration
    ?product_description
    ?offering_type
    ?multi_a_z
    ?lease_id
    ?(filters = [])
    ?max_records
    ?marker
    () =
  { reserved_d_b_instance_id
  ; reserved_d_b_instances_offering_id
  ; d_b_instance_class
  ; duration
  ; product_description
  ; offering_type
  ; multi_a_z
  ; lease_id
  ; filters
  ; max_records
  ; marker
  }

let parse xml =
  Some
    { reserved_d_b_instance_id =
        Aws.Util.option_bind (Aws.Xml.member "ReservedDBInstanceId" xml) String.parse
    ; reserved_d_b_instances_offering_id =
        Aws.Util.option_bind
          (Aws.Xml.member "ReservedDBInstancesOfferingId" xml)
          String.parse
    ; d_b_instance_class =
        Aws.Util.option_bind (Aws.Xml.member "DBInstanceClass" xml) String.parse
    ; duration = Aws.Util.option_bind (Aws.Xml.member "Duration" xml) String.parse
    ; product_description =
        Aws.Util.option_bind (Aws.Xml.member "ProductDescription" xml) String.parse
    ; offering_type =
        Aws.Util.option_bind (Aws.Xml.member "OfferingType" xml) String.parse
    ; multi_a_z = Aws.Util.option_bind (Aws.Xml.member "MultiAZ" xml) Boolean.parse
    ; lease_id = Aws.Util.option_bind (Aws.Xml.member "LeaseId" xml) String.parse
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
       ; Aws.Util.option_map v.lease_id (fun f ->
             Aws.Query.Pair ("LeaseId", String.to_query f))
       ; Aws.Util.option_map v.multi_a_z (fun f ->
             Aws.Query.Pair ("MultiAZ", Boolean.to_query f))
       ; Aws.Util.option_map v.offering_type (fun f ->
             Aws.Query.Pair ("OfferingType", String.to_query f))
       ; Aws.Util.option_map v.product_description (fun f ->
             Aws.Query.Pair ("ProductDescription", String.to_query f))
       ; Aws.Util.option_map v.duration (fun f ->
             Aws.Query.Pair ("Duration", String.to_query f))
       ; Aws.Util.option_map v.d_b_instance_class (fun f ->
             Aws.Query.Pair ("DBInstanceClass", String.to_query f))
       ; Aws.Util.option_map v.reserved_d_b_instances_offering_id (fun f ->
             Aws.Query.Pair ("ReservedDBInstancesOfferingId", String.to_query f))
       ; Aws.Util.option_map v.reserved_d_b_instance_id (fun f ->
             Aws.Query.Pair ("ReservedDBInstanceId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
       ; Some ("Filters", FilterList.to_json v.filters)
       ; Aws.Util.option_map v.lease_id (fun f -> "LeaseId", String.to_json f)
       ; Aws.Util.option_map v.multi_a_z (fun f -> "MultiAZ", Boolean.to_json f)
       ; Aws.Util.option_map v.offering_type (fun f -> "OfferingType", String.to_json f)
       ; Aws.Util.option_map v.product_description (fun f ->
             "ProductDescription", String.to_json f)
       ; Aws.Util.option_map v.duration (fun f -> "Duration", String.to_json f)
       ; Aws.Util.option_map v.d_b_instance_class (fun f ->
             "DBInstanceClass", String.to_json f)
       ; Aws.Util.option_map v.reserved_d_b_instances_offering_id (fun f ->
             "ReservedDBInstancesOfferingId", String.to_json f)
       ; Aws.Util.option_map v.reserved_d_b_instance_id (fun f ->
             "ReservedDBInstanceId", String.to_json f)
       ])

let of_json j =
  { reserved_d_b_instance_id =
      Aws.Util.option_map (Aws.Json.lookup j "ReservedDBInstanceId") String.of_json
  ; reserved_d_b_instances_offering_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "ReservedDBInstancesOfferingId")
        String.of_json
  ; d_b_instance_class =
      Aws.Util.option_map (Aws.Json.lookup j "DBInstanceClass") String.of_json
  ; duration = Aws.Util.option_map (Aws.Json.lookup j "Duration") String.of_json
  ; product_description =
      Aws.Util.option_map (Aws.Json.lookup j "ProductDescription") String.of_json
  ; offering_type = Aws.Util.option_map (Aws.Json.lookup j "OfferingType") String.of_json
  ; multi_a_z = Aws.Util.option_map (Aws.Json.lookup j "MultiAZ") Boolean.of_json
  ; lease_id = Aws.Util.option_map (Aws.Json.lookup j "LeaseId") String.of_json
  ; filters = FilterList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Filters"))
  ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  }
