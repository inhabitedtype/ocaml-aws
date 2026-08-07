open Aws.BaseTypes

type t =
  { network_card_index : Integer.t option
  ; ena_srd_specification : EnaSrdSpecification.t option
  ; ena_queue_count : Integer.t option
  ; dry_run : Boolean.t option
  ; network_interface_id : String.t
  ; instance_id : String.t
  ; device_index : Integer.t
  }

let make
    ?network_card_index
    ?ena_srd_specification
    ?ena_queue_count
    ?dry_run
    ~network_interface_id
    ~instance_id
    ~device_index
    () =
  { network_card_index
  ; ena_srd_specification
  ; ena_queue_count
  ; dry_run
  ; network_interface_id
  ; instance_id
  ; device_index
  }

let parse xml =
  Some
    { network_card_index =
        Aws.Util.option_bind (Aws.Xml.member "NetworkCardIndex" xml) Integer.parse
    ; ena_srd_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "EnaSrdSpecification" xml)
          EnaSrdSpecification.parse
    ; ena_queue_count =
        Aws.Util.option_bind (Aws.Xml.member "EnaQueueCount" xml) Integer.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; network_interface_id =
        Aws.Xml.required
          "networkInterfaceId"
          (Aws.Util.option_bind (Aws.Xml.member "networkInterfaceId" xml) String.parse)
    ; instance_id =
        Aws.Xml.required
          "instanceId"
          (Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse)
    ; device_index =
        Aws.Xml.required
          "deviceIndex"
          (Aws.Util.option_bind (Aws.Xml.member "deviceIndex" xml) Integer.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("DeviceIndex", Integer.to_query v.device_index))
       ; Some (Aws.Query.Pair ("InstanceId", String.to_query v.instance_id))
       ; Some
           (Aws.Query.Pair ("NetworkInterfaceId", String.to_query v.network_interface_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.ena_queue_count (fun f ->
             Aws.Query.Pair ("EnaQueueCount", Integer.to_query f))
       ; Aws.Util.option_map v.ena_srd_specification (fun f ->
             Aws.Query.Pair ("EnaSrdSpecification", EnaSrdSpecification.to_query f))
       ; Aws.Util.option_map v.network_card_index (fun f ->
             Aws.Query.Pair ("NetworkCardIndex", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("deviceIndex", Integer.to_json v.device_index)
       ; Some ("instanceId", String.to_json v.instance_id)
       ; Some ("networkInterfaceId", String.to_json v.network_interface_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.ena_queue_count (fun f ->
             "EnaQueueCount", Integer.to_json f)
       ; Aws.Util.option_map v.ena_srd_specification (fun f ->
             "EnaSrdSpecification", EnaSrdSpecification.to_json f)
       ; Aws.Util.option_map v.network_card_index (fun f ->
             "NetworkCardIndex", Integer.to_json f)
       ])

let of_json j =
  { network_card_index =
      Aws.Util.option_map (Aws.Json.lookup j "NetworkCardIndex") Integer.of_json
  ; ena_srd_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "EnaSrdSpecification")
        EnaSrdSpecification.of_json
  ; ena_queue_count =
      Aws.Util.option_map (Aws.Json.lookup j "EnaQueueCount") Integer.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; network_interface_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "networkInterfaceId"))
  ; instance_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "instanceId"))
  ; device_index =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "deviceIndex"))
  }
