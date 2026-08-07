open Aws.BaseTypes

type t =
  { attribute : ResourceAttribute.t option
  ; name : String.t option
  ; requires_recreation : RequiresRecreation.t option
  ; path : String.t option
  ; before_value : String.t option
  ; after_value : String.t option
  ; before_value_from : BeforeValueFrom.t option
  ; after_value_from : AfterValueFrom.t option
  ; drift : LiveResourceDrift.t option
  ; attribute_change_type : AttributeChangeType.t option
  }

let make
    ?attribute
    ?name
    ?requires_recreation
    ?path
    ?before_value
    ?after_value
    ?before_value_from
    ?after_value_from
    ?drift
    ?attribute_change_type
    () =
  { attribute
  ; name
  ; requires_recreation
  ; path
  ; before_value
  ; after_value
  ; before_value_from
  ; after_value_from
  ; drift
  ; attribute_change_type
  }

let parse xml =
  Some
    { attribute =
        Aws.Util.option_bind (Aws.Xml.member "Attribute" xml) ResourceAttribute.parse
    ; name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
    ; requires_recreation =
        Aws.Util.option_bind
          (Aws.Xml.member "RequiresRecreation" xml)
          RequiresRecreation.parse
    ; path = Aws.Util.option_bind (Aws.Xml.member "Path" xml) String.parse
    ; before_value = Aws.Util.option_bind (Aws.Xml.member "BeforeValue" xml) String.parse
    ; after_value = Aws.Util.option_bind (Aws.Xml.member "AfterValue" xml) String.parse
    ; before_value_from =
        Aws.Util.option_bind (Aws.Xml.member "BeforeValueFrom" xml) BeforeValueFrom.parse
    ; after_value_from =
        Aws.Util.option_bind (Aws.Xml.member "AfterValueFrom" xml) AfterValueFrom.parse
    ; drift = Aws.Util.option_bind (Aws.Xml.member "Drift" xml) LiveResourceDrift.parse
    ; attribute_change_type =
        Aws.Util.option_bind
          (Aws.Xml.member "AttributeChangeType" xml)
          AttributeChangeType.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.attribute_change_type (fun f ->
             Aws.Query.Pair ("AttributeChangeType", AttributeChangeType.to_query f))
       ; Aws.Util.option_map v.drift (fun f ->
             Aws.Query.Pair ("Drift", LiveResourceDrift.to_query f))
       ; Aws.Util.option_map v.after_value_from (fun f ->
             Aws.Query.Pair ("AfterValueFrom", AfterValueFrom.to_query f))
       ; Aws.Util.option_map v.before_value_from (fun f ->
             Aws.Query.Pair ("BeforeValueFrom", BeforeValueFrom.to_query f))
       ; Aws.Util.option_map v.after_value (fun f ->
             Aws.Query.Pair ("AfterValue", String.to_query f))
       ; Aws.Util.option_map v.before_value (fun f ->
             Aws.Query.Pair ("BeforeValue", String.to_query f))
       ; Aws.Util.option_map v.path (fun f -> Aws.Query.Pair ("Path", String.to_query f))
       ; Aws.Util.option_map v.requires_recreation (fun f ->
             Aws.Query.Pair ("RequiresRecreation", RequiresRecreation.to_query f))
       ; Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ; Aws.Util.option_map v.attribute (fun f ->
             Aws.Query.Pair ("Attribute", ResourceAttribute.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.attribute_change_type (fun f ->
             "AttributeChangeType", AttributeChangeType.to_json f)
       ; Aws.Util.option_map v.drift (fun f -> "Drift", LiveResourceDrift.to_json f)
       ; Aws.Util.option_map v.after_value_from (fun f ->
             "AfterValueFrom", AfterValueFrom.to_json f)
       ; Aws.Util.option_map v.before_value_from (fun f ->
             "BeforeValueFrom", BeforeValueFrom.to_json f)
       ; Aws.Util.option_map v.after_value (fun f -> "AfterValue", String.to_json f)
       ; Aws.Util.option_map v.before_value (fun f -> "BeforeValue", String.to_json f)
       ; Aws.Util.option_map v.path (fun f -> "Path", String.to_json f)
       ; Aws.Util.option_map v.requires_recreation (fun f ->
             "RequiresRecreation", RequiresRecreation.to_json f)
       ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
       ; Aws.Util.option_map v.attribute (fun f ->
             "Attribute", ResourceAttribute.to_json f)
       ])

let of_json j =
  { attribute =
      Aws.Util.option_map (Aws.Json.lookup j "Attribute") ResourceAttribute.of_json
  ; name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
  ; requires_recreation =
      Aws.Util.option_map
        (Aws.Json.lookup j "RequiresRecreation")
        RequiresRecreation.of_json
  ; path = Aws.Util.option_map (Aws.Json.lookup j "Path") String.of_json
  ; before_value = Aws.Util.option_map (Aws.Json.lookup j "BeforeValue") String.of_json
  ; after_value = Aws.Util.option_map (Aws.Json.lookup j "AfterValue") String.of_json
  ; before_value_from =
      Aws.Util.option_map (Aws.Json.lookup j "BeforeValueFrom") BeforeValueFrom.of_json
  ; after_value_from =
      Aws.Util.option_map (Aws.Json.lookup j "AfterValueFrom") AfterValueFrom.of_json
  ; drift = Aws.Util.option_map (Aws.Json.lookup j "Drift") LiveResourceDrift.of_json
  ; attribute_change_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "AttributeChangeType")
        AttributeChangeType.of_json
  }
