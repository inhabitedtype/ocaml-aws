open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; input_storage_location : StorageLocation.t
  ; logs_storage_location : StorageLocation.t option
  ; description : String.t option
  ; name : String.t option
  ; client_token : String.t option
  ; tag_specifications : TagSpecificationList.t
  }

let make
    ?dry_run
    ~input_storage_location
    ?logs_storage_location
    ?description
    ?name
    ?client_token
    ?(tag_specifications = [])
    () =
  { dry_run
  ; input_storage_location
  ; logs_storage_location
  ; description
  ; name
  ; client_token
  ; tag_specifications
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; input_storage_location =
        Aws.Xml.required
          "InputStorageLocation"
          (Aws.Util.option_bind
             (Aws.Xml.member "InputStorageLocation" xml)
             StorageLocation.parse)
    ; logs_storage_location =
        Aws.Util.option_bind
          (Aws.Xml.member "LogsStorageLocation" xml)
          StorageLocation.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.logs_storage_location (fun f ->
             Aws.Query.Pair ("LogsStorageLocation", StorageLocation.to_query f))
       ; Some
           (Aws.Query.Pair
              ("InputStorageLocation", StorageLocation.to_query v.input_storage_location))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.logs_storage_location (fun f ->
             "LogsStorageLocation", StorageLocation.to_json f)
       ; Some ("InputStorageLocation", StorageLocation.to_json v.input_storage_location)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; input_storage_location =
      StorageLocation.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "InputStorageLocation"))
  ; logs_storage_location =
      Aws.Util.option_map
        (Aws.Json.lookup j "LogsStorageLocation")
        StorageLocation.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  }
