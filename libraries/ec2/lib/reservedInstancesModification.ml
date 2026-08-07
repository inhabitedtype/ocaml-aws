open Aws.BaseTypes

type t =
  { client_token : String.t option
  ; create_date : DateTime.t option
  ; effective_date : DateTime.t option
  ; modification_results : ReservedInstancesModificationResultList.t
  ; reserved_instances_ids : ReservedIntancesIds.t
  ; reserved_instances_modification_id : String.t option
  ; status : String.t option
  ; status_message : String.t option
  ; update_date : DateTime.t option
  }

let make
    ?client_token
    ?create_date
    ?effective_date
    ?(modification_results = [])
    ?(reserved_instances_ids = [])
    ?reserved_instances_modification_id
    ?status
    ?status_message
    ?update_date
    () =
  { client_token
  ; create_date
  ; effective_date
  ; modification_results
  ; reserved_instances_ids
  ; reserved_instances_modification_id
  ; status
  ; status_message
  ; update_date
  }

let parse xml =
  Some
    { client_token = Aws.Util.option_bind (Aws.Xml.member "clientToken" xml) String.parse
    ; create_date = Aws.Util.option_bind (Aws.Xml.member "createDate" xml) DateTime.parse
    ; effective_date =
        Aws.Util.option_bind (Aws.Xml.member "effectiveDate" xml) DateTime.parse
    ; modification_results =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "modificationResultSet" xml)
             ReservedInstancesModificationResultList.parse)
    ; reserved_instances_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "reservedInstancesSet" xml)
             ReservedIntancesIds.parse)
    ; reserved_instances_modification_id =
        Aws.Util.option_bind
          (Aws.Xml.member "reservedInstancesModificationId" xml)
          String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "status" xml) String.parse
    ; status_message =
        Aws.Util.option_bind (Aws.Xml.member "statusMessage" xml) String.parse
    ; update_date = Aws.Util.option_bind (Aws.Xml.member "updateDate" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.update_date (fun f ->
             Aws.Query.Pair ("UpdateDate", DateTime.to_query f))
       ; Aws.Util.option_map v.status_message (fun f ->
             Aws.Query.Pair ("StatusMessage", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.reserved_instances_modification_id (fun f ->
             Aws.Query.Pair ("ReservedInstancesModificationId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ReservedInstancesSet"
              , ReservedIntancesIds.to_query v.reserved_instances_ids ))
       ; Some
           (Aws.Query.Pair
              ( "ModificationResultSet"
              , ReservedInstancesModificationResultList.to_query v.modification_results ))
       ; Aws.Util.option_map v.effective_date (fun f ->
             Aws.Query.Pair ("EffectiveDate", DateTime.to_query f))
       ; Aws.Util.option_map v.create_date (fun f ->
             Aws.Query.Pair ("CreateDate", DateTime.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.update_date (fun f -> "updateDate", DateTime.to_json f)
       ; Aws.Util.option_map v.status_message (fun f -> "statusMessage", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "status", String.to_json f)
       ; Aws.Util.option_map v.reserved_instances_modification_id (fun f ->
             "reservedInstancesModificationId", String.to_json f)
       ; Some
           ("reservedInstancesSet", ReservedIntancesIds.to_json v.reserved_instances_ids)
       ; Some
           ( "modificationResultSet"
           , ReservedInstancesModificationResultList.to_json v.modification_results )
       ; Aws.Util.option_map v.effective_date (fun f ->
             "effectiveDate", DateTime.to_json f)
       ; Aws.Util.option_map v.create_date (fun f -> "createDate", DateTime.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "clientToken", String.to_json f)
       ])

let of_json j =
  { client_token = Aws.Util.option_map (Aws.Json.lookup j "clientToken") String.of_json
  ; create_date = Aws.Util.option_map (Aws.Json.lookup j "createDate") DateTime.of_json
  ; effective_date =
      Aws.Util.option_map (Aws.Json.lookup j "effectiveDate") DateTime.of_json
  ; modification_results =
      ReservedInstancesModificationResultList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "modificationResultSet"))
  ; reserved_instances_ids =
      ReservedIntancesIds.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "reservedInstancesSet"))
  ; reserved_instances_modification_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "reservedInstancesModificationId")
        String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "status") String.of_json
  ; status_message =
      Aws.Util.option_map (Aws.Json.lookup j "statusMessage") String.of_json
  ; update_date = Aws.Util.option_map (Aws.Json.lookup j "updateDate") DateTime.of_json
  }
