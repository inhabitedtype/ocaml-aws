open Aws
open Aws.BaseTypes
open CalendarLib
type calendar = Calendar.t
module RequiresRecreation =
  struct
    type t =
      | Never 
      | Conditionally 
      | Always 
    let str_to_t =
      [("Always", Always);
      ("Conditionally", Conditionally);
      ("Never", Never)]
    let t_to_str =
      [(Always, "Always");
      (Conditionally, "Conditionally");
      (Never, "Never")]
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
module ResourceAttribute =
  struct
    type t =
      | Properties 
      | Metadata 
      | CreationPolicy 
      | UpdatePolicy 
      | DeletionPolicy 
      | Tags 
    let str_to_t =
      [("Tags", Tags);
      ("DeletionPolicy", DeletionPolicy);
      ("UpdatePolicy", UpdatePolicy);
      ("CreationPolicy", CreationPolicy);
      ("Metadata", Metadata);
      ("Properties", Properties)]
    let t_to_str =
      [(Tags, "Tags");
      (DeletionPolicy, "DeletionPolicy");
      (UpdatePolicy, "UpdatePolicy");
      (CreationPolicy, "CreationPolicy");
      (Metadata, "Metadata");
      (Properties, "Properties")]
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
module ChangeSource =
  struct
    type t =
      | ResourceReference 
      | ParameterReference 
      | ResourceAttribute 
      | DirectModification 
      | Automatic 
    let str_to_t =
      [("Automatic", Automatic);
      ("DirectModification", DirectModification);
      ("ResourceAttribute", ResourceAttribute);
      ("ParameterReference", ParameterReference);
      ("ResourceReference", ResourceReference)]
    let t_to_str =
      [(Automatic, "Automatic");
      (DirectModification, "DirectModification");
      (ResourceAttribute, "ResourceAttribute");
      (ParameterReference, "ParameterReference");
      (ResourceReference, "ResourceReference")]
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
module EvaluationType =
  struct
    type t =
      | Static 
      | Dynamic 
    let str_to_t = [("Dynamic", Dynamic); ("Static", Static)]
    let t_to_str = [(Dynamic, "Dynamic"); (Static, "Static")]
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
module ResourceTargetDefinition =
  struct
    type t =
      {
      attribute: ResourceAttribute.t option ;
      name: String.t option ;
      requires_recreation: RequiresRecreation.t option }
    let make ?attribute  ?name  ?requires_recreation  () =
      { attribute; name; requires_recreation }
    let parse xml =
      Some
        {
          attribute =
            (Util.option_bind (Xml.member "Attribute" xml)
               ResourceAttribute.parse);
          name = (Util.option_bind (Xml.member "Name" xml) String.parse);
          requires_recreation =
            (Util.option_bind (Xml.member "RequiresRecreation" xml)
               RequiresRecreation.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.requires_recreation
              (fun f ->
                 Query.Pair
                   ("RequiresRecreation", (RequiresRecreation.to_query f)));
           Util.option_map v.name
             (fun f -> Query.Pair ("Name", (String.to_query f)));
           Util.option_map v.attribute
             (fun f ->
                Query.Pair ("Attribute", (ResourceAttribute.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.requires_recreation
              (fun f ->
                 ("requires_recreation", (RequiresRecreation.to_json f)));
           Util.option_map v.name (fun f -> ("name", (String.to_json f)));
           Util.option_map v.attribute
             (fun f -> ("attribute", (ResourceAttribute.to_json f)))])
    let of_json j =
      {
        attribute =
          (Util.option_map (Json.lookup j "attribute")
             ResourceAttribute.of_json);
        name = (Util.option_map (Json.lookup j "name") String.of_json);
        requires_recreation =
          (Util.option_map (Json.lookup j "requires_recreation")
             RequiresRecreation.of_json)
      }
  end
module DifferenceType =
  struct
    type t =
      | ADD 
      | REMOVE 
      | NOT_EQUAL 
    let str_to_t =
      [("NOT_EQUAL", NOT_EQUAL); ("REMOVE", REMOVE); ("ADD", ADD)]
    let t_to_str =
      [(NOT_EQUAL, "NOT_EQUAL"); (REMOVE, "REMOVE"); (ADD, "ADD")]
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
module ResourceChangeDetail =
  struct
    type t =
      {
      target: ResourceTargetDefinition.t option ;
      evaluation: EvaluationType.t option ;
      change_source: ChangeSource.t option ;
      causing_entity: String.t option }
    let make ?target  ?evaluation  ?change_source  ?causing_entity  () =
      { target; evaluation; change_source; causing_entity }
    let parse xml =
      Some
        {
          target =
            (Util.option_bind (Xml.member "Target" xml)
               ResourceTargetDefinition.parse);
          evaluation =
            (Util.option_bind (Xml.member "Evaluation" xml)
               EvaluationType.parse);
          change_source =
            (Util.option_bind (Xml.member "ChangeSource" xml)
               ChangeSource.parse);
          causing_entity =
            (Util.option_bind (Xml.member "CausingEntity" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.causing_entity
              (fun f -> Query.Pair ("CausingEntity", (String.to_query f)));
           Util.option_map v.change_source
             (fun f -> Query.Pair ("ChangeSource", (ChangeSource.to_query f)));
           Util.option_map v.evaluation
             (fun f -> Query.Pair ("Evaluation", (EvaluationType.to_query f)));
           Util.option_map v.target
             (fun f ->
                Query.Pair ("Target", (ResourceTargetDefinition.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.causing_entity
              (fun f -> ("causing_entity", (String.to_json f)));
           Util.option_map v.change_source
             (fun f -> ("change_source", (ChangeSource.to_json f)));
           Util.option_map v.evaluation
             (fun f -> ("evaluation", (EvaluationType.to_json f)));
           Util.option_map v.target
             (fun f -> ("target", (ResourceTargetDefinition.to_json f)))])
    let of_json j =
      {
        target =
          (Util.option_map (Json.lookup j "target")
             ResourceTargetDefinition.of_json);
        evaluation =
          (Util.option_map (Json.lookup j "evaluation")
             EvaluationType.of_json);
        change_source =
          (Util.option_map (Json.lookup j "change_source")
             ChangeSource.of_json);
        causing_entity =
          (Util.option_map (Json.lookup j "causing_entity") String.of_json)
      }
  end
module RollbackTrigger =
  struct
    type t = {
      arn: String.t ;
      type_: String.t }
    let make ~arn  ~type_  () = { arn; type_ }
    let parse xml =
      Some
        {
          arn =
            (Xml.required "Arn"
               (Util.option_bind (Xml.member "Arn" xml) String.parse));
          type_ =
            (Xml.required "Type"
               (Util.option_bind (Xml.member "Type" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Type", (String.to_query v.type_)));
           Some (Query.Pair ("Arn", (String.to_query v.arn)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("type_", (String.to_json v.type_));
           Some ("arn", (String.to_json v.arn))])
    let of_json j =
      {
        arn = (String.of_json (Util.of_option_exn (Json.lookup j "arn")));
        type_ = (String.of_json (Util.of_option_exn (Json.lookup j "type_")))
      }
  end
module AccountGateStatus =
  struct
    type t =
      | SUCCEEDED 
      | FAILED 
      | SKIPPED 
    let str_to_t =
      [("SKIPPED", SKIPPED); ("FAILED", FAILED); ("SUCCEEDED", SUCCEEDED)]
    let t_to_str =
      [(SKIPPED, "SKIPPED"); (FAILED, "FAILED"); (SUCCEEDED, "SUCCEEDED")]
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
module StackDriftStatus =
  struct
    type t =
      | DRIFTED 
      | IN_SYNC 
      | UNKNOWN 
      | NOT_CHECKED 
    let str_to_t =
      [("NOT_CHECKED", NOT_CHECKED);
      ("UNKNOWN", UNKNOWN);
      ("IN_SYNC", IN_SYNC);
      ("DRIFTED", DRIFTED)]
    let t_to_str =
      [(NOT_CHECKED, "NOT_CHECKED");
      (UNKNOWN, "UNKNOWN");
      (IN_SYNC, "IN_SYNC");
      (DRIFTED, "DRIFTED")]
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
module PhysicalResourceIdContextKeyValuePair =
  struct
    type t = {
      key: String.t ;
      value: String.t }
    let make ~key  ~value  () = { key; value }
    let parse xml =
      Some
        {
          key =
            (Xml.required "Key"
               (Util.option_bind (Xml.member "Key" xml) String.parse));
          value =
            (Xml.required "Value"
               (Util.option_bind (Xml.member "Value" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Value", (String.to_query v.value)));
           Some (Query.Pair ("Key", (String.to_query v.key)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("value", (String.to_json v.value));
           Some ("key", (String.to_json v.key))])
    let of_json j =
      {
        key = (String.of_json (Util.of_option_exn (Json.lookup j "key")));
        value = (String.of_json (Util.of_option_exn (Json.lookup j "value")))
      }
  end
module PropertyDifference =
  struct
    type t =
      {
      property_path: String.t ;
      expected_value: String.t ;
      actual_value: String.t ;
      difference_type: DifferenceType.t }
    let make ~property_path  ~expected_value  ~actual_value  ~difference_type
       () = { property_path; expected_value; actual_value; difference_type }
    let parse xml =
      Some
        {
          property_path =
            (Xml.required "PropertyPath"
               (Util.option_bind (Xml.member "PropertyPath" xml) String.parse));
          expected_value =
            (Xml.required "ExpectedValue"
               (Util.option_bind (Xml.member "ExpectedValue" xml)
                  String.parse));
          actual_value =
            (Xml.required "ActualValue"
               (Util.option_bind (Xml.member "ActualValue" xml) String.parse));
          difference_type =
            (Xml.required "DifferenceType"
               (Util.option_bind (Xml.member "DifferenceType" xml)
                  DifferenceType.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("DifferenceType",
                   (DifferenceType.to_query v.difference_type)));
           Some
             (Query.Pair ("ActualValue", (String.to_query v.actual_value)));
           Some
             (Query.Pair
                ("ExpectedValue", (String.to_query v.expected_value)));
           Some
             (Query.Pair ("PropertyPath", (String.to_query v.property_path)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("difference_type", (DifferenceType.to_json v.difference_type));
           Some ("actual_value", (String.to_json v.actual_value));
           Some ("expected_value", (String.to_json v.expected_value));
           Some ("property_path", (String.to_json v.property_path))])
    let of_json j =
      {
        property_path =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "property_path")));
        expected_value =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "expected_value")));
        actual_value =
          (String.of_json (Util.of_option_exn (Json.lookup j "actual_value")));
        difference_type =
          (DifferenceType.of_json
             (Util.of_option_exn (Json.lookup j "difference_type")))
      }
  end
module ChangeAction =
  struct
    type t =
      | Add 
      | Modify 
      | Remove 
      | Import 
    let str_to_t =
      [("Import", Import);
      ("Remove", Remove);
      ("Modify", Modify);
      ("Add", Add)]
    let t_to_str =
      [(Import, "Import");
      (Remove, "Remove");
      (Modify, "Modify");
      (Add, "Add")]
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
module Replacement =
  struct
    type t =
      | True 
      | False 
      | Conditional 
    let str_to_t =
      [("Conditional", Conditional); ("False", False); ("True", True)]
    let t_to_str =
      [(Conditional, "Conditional"); (False, "False"); (True, "True")]
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
module ResourceChangeDetails =
  struct
    type t = ResourceChangeDetail.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map ResourceChangeDetail.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list ResourceChangeDetail.to_query v
    let to_json v = `List (List.map ResourceChangeDetail.to_json v)
    let of_json j = Json.to_list ResourceChangeDetail.of_json j
  end
module Scope =
  struct
    type t = ResourceAttribute.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map ResourceAttribute.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list ResourceAttribute.to_query v
    let to_json v = `List (List.map ResourceAttribute.to_json v)
    let of_json j = Json.to_list ResourceAttribute.of_json j
  end
module StackResourceDriftStatus =
  struct
    type t =
      | IN_SYNC 
      | MODIFIED 
      | DELETED 
      | NOT_CHECKED 
    let str_to_t =
      [("NOT_CHECKED", NOT_CHECKED);
      ("DELETED", DELETED);
      ("MODIFIED", MODIFIED);
      ("IN_SYNC", IN_SYNC)]
    let t_to_str =
      [(NOT_CHECKED, "NOT_CHECKED");
      (DELETED, "DELETED");
      (MODIFIED, "MODIFIED");
      (IN_SYNC, "IN_SYNC")]
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
module AllowedValues =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module StackInstanceDetailedStatus =
  struct
    type t =
      | PENDING 
      | RUNNING 
      | SUCCEEDED 
      | FAILED 
      | CANCELLED 
      | INOPERABLE 
    let str_to_t =
      [("INOPERABLE", INOPERABLE);
      ("CANCELLED", CANCELLED);
      ("FAILED", FAILED);
      ("SUCCEEDED", SUCCEEDED);
      ("RUNNING", RUNNING);
      ("PENDING", PENDING)]
    let t_to_str =
      [(INOPERABLE, "INOPERABLE");
      (CANCELLED, "CANCELLED");
      (FAILED, "FAILED");
      (SUCCEEDED, "SUCCEEDED");
      (RUNNING, "RUNNING");
      (PENDING, "PENDING")]
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
module Capability =
  struct
    type t =
      | CAPABILITY_IAM 
      | CAPABILITY_NAMED_IAM 
      | CAPABILITY_AUTO_EXPAND 
    let str_to_t =
      [("CAPABILITY_AUTO_EXPAND", CAPABILITY_AUTO_EXPAND);
      ("CAPABILITY_NAMED_IAM", CAPABILITY_NAMED_IAM);
      ("CAPABILITY_IAM", CAPABILITY_IAM)]
    let t_to_str =
      [(CAPABILITY_AUTO_EXPAND, "CAPABILITY_AUTO_EXPAND");
      (CAPABILITY_NAMED_IAM, "CAPABILITY_NAMED_IAM");
      (CAPABILITY_IAM, "CAPABILITY_IAM")]
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
module Output =
  struct
    type t =
      {
      output_key: String.t option ;
      output_value: String.t option ;
      description: String.t option ;
      export_name: String.t option }
    let make ?output_key  ?output_value  ?description  ?export_name  () =
      { output_key; output_value; description; export_name }
    let parse xml =
      Some
        {
          output_key =
            (Util.option_bind (Xml.member "OutputKey" xml) String.parse);
          output_value =
            (Util.option_bind (Xml.member "OutputValue" xml) String.parse);
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse);
          export_name =
            (Util.option_bind (Xml.member "ExportName" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.export_name
              (fun f -> Query.Pair ("ExportName", (String.to_query f)));
           Util.option_map v.description
             (fun f -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.output_value
             (fun f -> Query.Pair ("OutputValue", (String.to_query f)));
           Util.option_map v.output_key
             (fun f -> Query.Pair ("OutputKey", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.export_name
              (fun f -> ("export_name", (String.to_json f)));
           Util.option_map v.description
             (fun f -> ("description", (String.to_json f)));
           Util.option_map v.output_value
             (fun f -> ("output_value", (String.to_json f)));
           Util.option_map v.output_key
             (fun f -> ("output_key", (String.to_json f)))])
    let of_json j =
      {
        output_key =
          (Util.option_map (Json.lookup j "output_key") String.of_json);
        output_value =
          (Util.option_map (Json.lookup j "output_value") String.of_json);
        description =
          (Util.option_map (Json.lookup j "description") String.of_json);
        export_name =
          (Util.option_map (Json.lookup j "export_name") String.of_json)
      }
  end
module Parameter =
  struct
    type t =
      {
      parameter_key: String.t option ;
      parameter_value: String.t option ;
      use_previous_value: Boolean.t option ;
      resolved_value: String.t option }
    let make ?parameter_key  ?parameter_value  ?use_previous_value 
      ?resolved_value  () =
      { parameter_key; parameter_value; use_previous_value; resolved_value }
    let parse xml =
      Some
        {
          parameter_key =
            (Util.option_bind (Xml.member "ParameterKey" xml) String.parse);
          parameter_value =
            (Util.option_bind (Xml.member "ParameterValue" xml) String.parse);
          use_previous_value =
            (Util.option_bind (Xml.member "UsePreviousValue" xml)
               Boolean.parse);
          resolved_value =
            (Util.option_bind (Xml.member "ResolvedValue" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.resolved_value
              (fun f -> Query.Pair ("ResolvedValue", (String.to_query f)));
           Util.option_map v.use_previous_value
             (fun f -> Query.Pair ("UsePreviousValue", (Boolean.to_query f)));
           Util.option_map v.parameter_value
             (fun f -> Query.Pair ("ParameterValue", (String.to_query f)));
           Util.option_map v.parameter_key
             (fun f -> Query.Pair ("ParameterKey", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.resolved_value
              (fun f -> ("resolved_value", (String.to_json f)));
           Util.option_map v.use_previous_value
             (fun f -> ("use_previous_value", (Boolean.to_json f)));
           Util.option_map v.parameter_value
             (fun f -> ("parameter_value", (String.to_json f)));
           Util.option_map v.parameter_key
             (fun f -> ("parameter_key", (String.to_json f)))])
    let of_json j =
      {
        parameter_key =
          (Util.option_map (Json.lookup j "parameter_key") String.of_json);
        parameter_value =
          (Util.option_map (Json.lookup j "parameter_value") String.of_json);
        use_previous_value =
          (Util.option_map (Json.lookup j "use_previous_value")
             Boolean.of_json);
        resolved_value =
          (Util.option_map (Json.lookup j "resolved_value") String.of_json)
      }
  end
module RollbackTriggers =
  struct
    type t = RollbackTrigger.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map RollbackTrigger.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list RollbackTrigger.to_query v
    let to_json v = `List (List.map RollbackTrigger.to_json v)
    let of_json j = Json.to_list RollbackTrigger.of_json j
  end
module Tag =
  struct
    type t = {
      key: String.t ;
      value: String.t }
    let make ~key  ~value  () = { key; value }
    let parse xml =
      Some
        {
          key =
            (Xml.required "Key"
               (Util.option_bind (Xml.member "Key" xml) String.parse));
          value =
            (Xml.required "Value"
               (Util.option_bind (Xml.member "Value" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Value", (String.to_query v.value)));
           Some (Query.Pair ("Key", (String.to_query v.key)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("value", (String.to_json v.value));
           Some ("key", (String.to_json v.key))])
    let of_json j =
      {
        key = (String.of_json (Util.of_option_exn (Json.lookup j "key")));
        value = (String.of_json (Util.of_option_exn (Json.lookup j "value")))
      }
  end
module AccountGateResult =
  struct
    type t =
      {
      status: AccountGateStatus.t option ;
      status_reason: String.t option }
    let make ?status  ?status_reason  () = { status; status_reason }
    let parse xml =
      Some
        {
          status =
            (Util.option_bind (Xml.member "Status" xml)
               AccountGateStatus.parse);
          status_reason =
            (Util.option_bind (Xml.member "StatusReason" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.status_reason
              (fun f -> Query.Pair ("StatusReason", (String.to_query f)));
           Util.option_map v.status
             (fun f -> Query.Pair ("Status", (AccountGateStatus.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.status_reason
              (fun f -> ("status_reason", (String.to_json f)));
           Util.option_map v.status
             (fun f -> ("status", (AccountGateStatus.to_json f)))])
    let of_json j =
      {
        status =
          (Util.option_map (Json.lookup j "status") AccountGateStatus.of_json);
        status_reason =
          (Util.option_map (Json.lookup j "status_reason") String.of_json)
      }
  end
module StackSetOperationResultStatus =
  struct
    type t =
      | PENDING 
      | RUNNING 
      | SUCCEEDED 
      | FAILED 
      | CANCELLED 
    let str_to_t =
      [("CANCELLED", CANCELLED);
      ("FAILED", FAILED);
      ("SUCCEEDED", SUCCEEDED);
      ("RUNNING", RUNNING);
      ("PENDING", PENDING)]
    let t_to_str =
      [(CANCELLED, "CANCELLED");
      (FAILED, "FAILED");
      (SUCCEEDED, "SUCCEEDED");
      (RUNNING, "RUNNING");
      (PENDING, "PENDING")]
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
module StackDriftInformationSummary =
  struct
    type t =
      {
      stack_drift_status: StackDriftStatus.t ;
      last_check_timestamp: DateTime.t option }
    let make ~stack_drift_status  ?last_check_timestamp  () =
      { stack_drift_status; last_check_timestamp }
    let parse xml =
      Some
        {
          stack_drift_status =
            (Xml.required "StackDriftStatus"
               (Util.option_bind (Xml.member "StackDriftStatus" xml)
                  StackDriftStatus.parse));
          last_check_timestamp =
            (Util.option_bind (Xml.member "LastCheckTimestamp" xml)
               DateTime.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.last_check_timestamp
              (fun f ->
                 Query.Pair ("LastCheckTimestamp", (DateTime.to_query f)));
           Some
             (Query.Pair
                ("StackDriftStatus",
                  (StackDriftStatus.to_query v.stack_drift_status)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.last_check_timestamp
              (fun f -> ("last_check_timestamp", (DateTime.to_json f)));
           Some
             ("stack_drift_status",
               (StackDriftStatus.to_json v.stack_drift_status))])
    let of_json j =
      {
        stack_drift_status =
          (StackDriftStatus.of_json
             (Util.of_option_exn (Json.lookup j "stack_drift_status")));
        last_check_timestamp =
          (Util.option_map (Json.lookup j "last_check_timestamp")
             DateTime.of_json)
      }
  end
module StackStatus =
  struct
    type t =
      | CREATE_IN_PROGRESS 
      | CREATE_FAILED 
      | CREATE_COMPLETE 
      | ROLLBACK_IN_PROGRESS 
      | ROLLBACK_FAILED 
      | ROLLBACK_COMPLETE 
      | DELETE_IN_PROGRESS 
      | DELETE_FAILED 
      | DELETE_COMPLETE 
      | UPDATE_IN_PROGRESS 
      | UPDATE_COMPLETE_CLEANUP_IN_PROGRESS 
      | UPDATE_COMPLETE 
      | UPDATE_ROLLBACK_IN_PROGRESS 
      | UPDATE_ROLLBACK_FAILED 
      | UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS 
      | UPDATE_ROLLBACK_COMPLETE 
      | REVIEW_IN_PROGRESS 
      | IMPORT_IN_PROGRESS 
      | IMPORT_COMPLETE 
      | IMPORT_ROLLBACK_IN_PROGRESS 
      | IMPORT_ROLLBACK_FAILED 
      | IMPORT_ROLLBACK_COMPLETE 
    let str_to_t =
      [("IMPORT_ROLLBACK_COMPLETE", IMPORT_ROLLBACK_COMPLETE);
      ("IMPORT_ROLLBACK_FAILED", IMPORT_ROLLBACK_FAILED);
      ("IMPORT_ROLLBACK_IN_PROGRESS", IMPORT_ROLLBACK_IN_PROGRESS);
      ("IMPORT_COMPLETE", IMPORT_COMPLETE);
      ("IMPORT_IN_PROGRESS", IMPORT_IN_PROGRESS);
      ("REVIEW_IN_PROGRESS", REVIEW_IN_PROGRESS);
      ("UPDATE_ROLLBACK_COMPLETE", UPDATE_ROLLBACK_COMPLETE);
      ("UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS",
        UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS);
      ("UPDATE_ROLLBACK_FAILED", UPDATE_ROLLBACK_FAILED);
      ("UPDATE_ROLLBACK_IN_PROGRESS", UPDATE_ROLLBACK_IN_PROGRESS);
      ("UPDATE_COMPLETE", UPDATE_COMPLETE);
      ("UPDATE_COMPLETE_CLEANUP_IN_PROGRESS",
        UPDATE_COMPLETE_CLEANUP_IN_PROGRESS);
      ("UPDATE_IN_PROGRESS", UPDATE_IN_PROGRESS);
      ("DELETE_COMPLETE", DELETE_COMPLETE);
      ("DELETE_FAILED", DELETE_FAILED);
      ("DELETE_IN_PROGRESS", DELETE_IN_PROGRESS);
      ("ROLLBACK_COMPLETE", ROLLBACK_COMPLETE);
      ("ROLLBACK_FAILED", ROLLBACK_FAILED);
      ("ROLLBACK_IN_PROGRESS", ROLLBACK_IN_PROGRESS);
      ("CREATE_COMPLETE", CREATE_COMPLETE);
      ("CREATE_FAILED", CREATE_FAILED);
      ("CREATE_IN_PROGRESS", CREATE_IN_PROGRESS)]
    let t_to_str =
      [(IMPORT_ROLLBACK_COMPLETE, "IMPORT_ROLLBACK_COMPLETE");
      (IMPORT_ROLLBACK_FAILED, "IMPORT_ROLLBACK_FAILED");
      (IMPORT_ROLLBACK_IN_PROGRESS, "IMPORT_ROLLBACK_IN_PROGRESS");
      (IMPORT_COMPLETE, "IMPORT_COMPLETE");
      (IMPORT_IN_PROGRESS, "IMPORT_IN_PROGRESS");
      (REVIEW_IN_PROGRESS, "REVIEW_IN_PROGRESS");
      (UPDATE_ROLLBACK_COMPLETE, "UPDATE_ROLLBACK_COMPLETE");
      (UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS,
        "UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS");
      (UPDATE_ROLLBACK_FAILED, "UPDATE_ROLLBACK_FAILED");
      (UPDATE_ROLLBACK_IN_PROGRESS, "UPDATE_ROLLBACK_IN_PROGRESS");
      (UPDATE_COMPLETE, "UPDATE_COMPLETE");
      (UPDATE_COMPLETE_CLEANUP_IN_PROGRESS,
        "UPDATE_COMPLETE_CLEANUP_IN_PROGRESS");
      (UPDATE_IN_PROGRESS, "UPDATE_IN_PROGRESS");
      (DELETE_COMPLETE, "DELETE_COMPLETE");
      (DELETE_FAILED, "DELETE_FAILED");
      (DELETE_IN_PROGRESS, "DELETE_IN_PROGRESS");
      (ROLLBACK_COMPLETE, "ROLLBACK_COMPLETE");
      (ROLLBACK_FAILED, "ROLLBACK_FAILED");
      (ROLLBACK_IN_PROGRESS, "ROLLBACK_IN_PROGRESS");
      (CREATE_COMPLETE, "CREATE_COMPLETE");
      (CREATE_FAILED, "CREATE_FAILED");
      (CREATE_IN_PROGRESS, "CREATE_IN_PROGRESS")]
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
module PhysicalResourceIdContext =
  struct
    type t = PhysicalResourceIdContextKeyValuePair.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map PhysicalResourceIdContextKeyValuePair.parse
           (Xml.members "member" xml))
    let to_query v =
      Query.to_query_list PhysicalResourceIdContextKeyValuePair.to_query v
    let to_json v =
      `List (List.map PhysicalResourceIdContextKeyValuePair.to_json v)
    let of_json j =
      Json.to_list PhysicalResourceIdContextKeyValuePair.of_json j
  end
module PropertyDifferences =
  struct
    type t = PropertyDifference.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map PropertyDifference.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list PropertyDifference.to_query v
    let to_json v = `List (List.map PropertyDifference.to_json v)
    let of_json j = Json.to_list PropertyDifference.of_json j
  end
module ChangeType =
  struct
    type t =
      | Resource 
    let str_to_t = [("Resource", Resource)]
    let t_to_str = [(Resource, "Resource")]
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
module ResourceChange =
  struct
    type t =
      {
      action: ChangeAction.t option ;
      logical_resource_id: String.t option ;
      physical_resource_id: String.t option ;
      resource_type: String.t option ;
      replacement: Replacement.t option ;
      scope: Scope.t ;
      details: ResourceChangeDetails.t }
    let make ?action  ?logical_resource_id  ?physical_resource_id 
      ?resource_type  ?replacement  ?(scope= [])  ?(details= [])  () =
      {
        action;
        logical_resource_id;
        physical_resource_id;
        resource_type;
        replacement;
        scope;
        details
      }
    let parse xml =
      Some
        {
          action =
            (Util.option_bind (Xml.member "Action" xml) ChangeAction.parse);
          logical_resource_id =
            (Util.option_bind (Xml.member "LogicalResourceId" xml)
               String.parse);
          physical_resource_id =
            (Util.option_bind (Xml.member "PhysicalResourceId" xml)
               String.parse);
          resource_type =
            (Util.option_bind (Xml.member "ResourceType" xml) String.parse);
          replacement =
            (Util.option_bind (Xml.member "Replacement" xml)
               Replacement.parse);
          scope =
            (Util.of_option []
               (Util.option_bind (Xml.member "Scope" xml) Scope.parse));
          details =
            (Util.of_option []
               (Util.option_bind (Xml.member "Details" xml)
                  ResourceChangeDetails.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Details.member",
                   (ResourceChangeDetails.to_query v.details)));
           Some (Query.Pair ("Scope.member", (Scope.to_query v.scope)));
           Util.option_map v.replacement
             (fun f -> Query.Pair ("Replacement", (Replacement.to_query f)));
           Util.option_map v.resource_type
             (fun f -> Query.Pair ("ResourceType", (String.to_query f)));
           Util.option_map v.physical_resource_id
             (fun f -> Query.Pair ("PhysicalResourceId", (String.to_query f)));
           Util.option_map v.logical_resource_id
             (fun f -> Query.Pair ("LogicalResourceId", (String.to_query f)));
           Util.option_map v.action
             (fun f -> Query.Pair ("Action", (ChangeAction.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("details", (ResourceChangeDetails.to_json v.details));
           Some ("scope", (Scope.to_json v.scope));
           Util.option_map v.replacement
             (fun f -> ("replacement", (Replacement.to_json f)));
           Util.option_map v.resource_type
             (fun f -> ("resource_type", (String.to_json f)));
           Util.option_map v.physical_resource_id
             (fun f -> ("physical_resource_id", (String.to_json f)));
           Util.option_map v.logical_resource_id
             (fun f -> ("logical_resource_id", (String.to_json f)));
           Util.option_map v.action
             (fun f -> ("action", (ChangeAction.to_json f)))])
    let of_json j =
      {
        action =
          (Util.option_map (Json.lookup j "action") ChangeAction.of_json);
        logical_resource_id =
          (Util.option_map (Json.lookup j "logical_resource_id")
             String.of_json);
        physical_resource_id =
          (Util.option_map (Json.lookup j "physical_resource_id")
             String.of_json);
        resource_type =
          (Util.option_map (Json.lookup j "resource_type") String.of_json);
        replacement =
          (Util.option_map (Json.lookup j "replacement") Replacement.of_json);
        scope = (Scope.of_json (Util.of_option_exn (Json.lookup j "scope")));
        details =
          (ResourceChangeDetails.of_json
             (Util.of_option_exn (Json.lookup j "details")))
      }
  end
module ChangeSetStatus =
  struct
    type t =
      | CREATE_PENDING 
      | CREATE_IN_PROGRESS 
      | CREATE_COMPLETE 
      | DELETE_COMPLETE 
      | FAILED 
    let str_to_t =
      [("FAILED", FAILED);
      ("DELETE_COMPLETE", DELETE_COMPLETE);
      ("CREATE_COMPLETE", CREATE_COMPLETE);
      ("CREATE_IN_PROGRESS", CREATE_IN_PROGRESS);
      ("CREATE_PENDING", CREATE_PENDING)]
    let t_to_str =
      [(FAILED, "FAILED");
      (DELETE_COMPLETE, "DELETE_COMPLETE");
      (CREATE_COMPLETE, "CREATE_COMPLETE");
      (CREATE_IN_PROGRESS, "CREATE_IN_PROGRESS");
      (CREATE_PENDING, "CREATE_PENDING")]
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
module ExecutionStatus =
  struct
    type t =
      | UNAVAILABLE 
      | AVAILABLE 
      | EXECUTE_IN_PROGRESS 
      | EXECUTE_COMPLETE 
      | EXECUTE_FAILED 
      | OBSOLETE 
    let str_to_t =
      [("OBSOLETE", OBSOLETE);
      ("EXECUTE_FAILED", EXECUTE_FAILED);
      ("EXECUTE_COMPLETE", EXECUTE_COMPLETE);
      ("EXECUTE_IN_PROGRESS", EXECUTE_IN_PROGRESS);
      ("AVAILABLE", AVAILABLE);
      ("UNAVAILABLE", UNAVAILABLE)]
    let t_to_str =
      [(OBSOLETE, "OBSOLETE");
      (EXECUTE_FAILED, "EXECUTE_FAILED");
      (EXECUTE_COMPLETE, "EXECUTE_COMPLETE");
      (EXECUTE_IN_PROGRESS, "EXECUTE_IN_PROGRESS");
      (AVAILABLE, "AVAILABLE");
      (UNAVAILABLE, "UNAVAILABLE")]
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
module ResourceIdentifierProperties =
  struct
    type t = (String.t, String.t) Hashtbl.t
    let make elems () = elems
    let parse xml = None
    let to_query v =
      Query.to_query_hashtbl String.to_string String.to_query v
    let to_json v =
      `Assoc
        (Hashtbl.fold
           (fun k ->
              fun v ->
                fun acc -> ((String.to_string k), (String.to_json v)) :: acc)
           v [])
    let of_json j = Json.to_hashtbl String.of_string String.of_json j
  end
module AutoDeployment =
  struct
    type t =
      {
      enabled: Boolean.t option ;
      retain_stacks_on_account_removal: Boolean.t option }
    let make ?enabled  ?retain_stacks_on_account_removal  () =
      { enabled; retain_stacks_on_account_removal }
    let parse xml =
      Some
        {
          enabled =
            (Util.option_bind (Xml.member "Enabled" xml) Boolean.parse);
          retain_stacks_on_account_removal =
            (Util.option_bind (Xml.member "RetainStacksOnAccountRemoval" xml)
               Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.retain_stacks_on_account_removal
              (fun f ->
                 Query.Pair
                   ("RetainStacksOnAccountRemoval", (Boolean.to_query f)));
           Util.option_map v.enabled
             (fun f -> Query.Pair ("Enabled", (Boolean.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.retain_stacks_on_account_removal
              (fun f ->
                 ("retain_stacks_on_account_removal", (Boolean.to_json f)));
           Util.option_map v.enabled
             (fun f -> ("enabled", (Boolean.to_json f)))])
    let of_json j =
      {
        enabled = (Util.option_map (Json.lookup j "enabled") Boolean.of_json);
        retain_stacks_on_account_removal =
          (Util.option_map (Json.lookup j "retain_stacks_on_account_removal")
             Boolean.of_json)
      }
  end
module PermissionModels =
  struct
    type t =
      | SERVICE_MANAGED 
      | SELF_MANAGED 
    let str_to_t =
      [("SELF_MANAGED", SELF_MANAGED); ("SERVICE_MANAGED", SERVICE_MANAGED)]
    let t_to_str =
      [(SELF_MANAGED, "SELF_MANAGED"); (SERVICE_MANAGED, "SERVICE_MANAGED")]
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
module StackSetStatus =
  struct
    type t =
      | ACTIVE 
      | DELETED 
    let str_to_t = [("DELETED", DELETED); ("ACTIVE", ACTIVE)]
    let t_to_str = [(DELETED, "DELETED"); (ACTIVE, "ACTIVE")]
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
module RegistryType =
  struct
    type t =
      | RESOURCE 
    let str_to_t = [("RESOURCE", RESOURCE)]
    let t_to_str = [(RESOURCE, "RESOURCE")]
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
module AccountList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module OrganizationalUnitIdList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module StackSetDriftDetectionStatus =
  struct
    type t =
      | COMPLETED 
      | FAILED 
      | PARTIAL_SUCCESS 
      | IN_PROGRESS 
      | STOPPED 
    let str_to_t =
      [("STOPPED", STOPPED);
      ("IN_PROGRESS", IN_PROGRESS);
      ("PARTIAL_SUCCESS", PARTIAL_SUCCESS);
      ("FAILED", FAILED);
      ("COMPLETED", COMPLETED)]
    let t_to_str =
      [(STOPPED, "STOPPED");
      (IN_PROGRESS, "IN_PROGRESS");
      (PARTIAL_SUCCESS, "PARTIAL_SUCCESS");
      (FAILED, "FAILED");
      (COMPLETED, "COMPLETED")]
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
module StackSetDriftStatus =
  struct
    type t =
      | DRIFTED 
      | IN_SYNC 
      | NOT_CHECKED 
    let str_to_t =
      [("NOT_CHECKED", NOT_CHECKED);
      ("IN_SYNC", IN_SYNC);
      ("DRIFTED", DRIFTED)]
    let t_to_str =
      [(NOT_CHECKED, "NOT_CHECKED");
      (IN_SYNC, "IN_SYNC");
      (DRIFTED, "DRIFTED")]
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
module RegionList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module ResourceStatus =
  struct
    type t =
      | CREATE_IN_PROGRESS 
      | CREATE_FAILED 
      | CREATE_COMPLETE 
      | DELETE_IN_PROGRESS 
      | DELETE_FAILED 
      | DELETE_COMPLETE 
      | DELETE_SKIPPED 
      | UPDATE_IN_PROGRESS 
      | UPDATE_FAILED 
      | UPDATE_COMPLETE 
      | IMPORT_FAILED 
      | IMPORT_COMPLETE 
      | IMPORT_IN_PROGRESS 
      | IMPORT_ROLLBACK_IN_PROGRESS 
      | IMPORT_ROLLBACK_FAILED 
      | IMPORT_ROLLBACK_COMPLETE 
    let str_to_t =
      [("IMPORT_ROLLBACK_COMPLETE", IMPORT_ROLLBACK_COMPLETE);
      ("IMPORT_ROLLBACK_FAILED", IMPORT_ROLLBACK_FAILED);
      ("IMPORT_ROLLBACK_IN_PROGRESS", IMPORT_ROLLBACK_IN_PROGRESS);
      ("IMPORT_IN_PROGRESS", IMPORT_IN_PROGRESS);
      ("IMPORT_COMPLETE", IMPORT_COMPLETE);
      ("IMPORT_FAILED", IMPORT_FAILED);
      ("UPDATE_COMPLETE", UPDATE_COMPLETE);
      ("UPDATE_FAILED", UPDATE_FAILED);
      ("UPDATE_IN_PROGRESS", UPDATE_IN_PROGRESS);
      ("DELETE_SKIPPED", DELETE_SKIPPED);
      ("DELETE_COMPLETE", DELETE_COMPLETE);
      ("DELETE_FAILED", DELETE_FAILED);
      ("DELETE_IN_PROGRESS", DELETE_IN_PROGRESS);
      ("CREATE_COMPLETE", CREATE_COMPLETE);
      ("CREATE_FAILED", CREATE_FAILED);
      ("CREATE_IN_PROGRESS", CREATE_IN_PROGRESS)]
    let t_to_str =
      [(IMPORT_ROLLBACK_COMPLETE, "IMPORT_ROLLBACK_COMPLETE");
      (IMPORT_ROLLBACK_FAILED, "IMPORT_ROLLBACK_FAILED");
      (IMPORT_ROLLBACK_IN_PROGRESS, "IMPORT_ROLLBACK_IN_PROGRESS");
      (IMPORT_IN_PROGRESS, "IMPORT_IN_PROGRESS");
      (IMPORT_COMPLETE, "IMPORT_COMPLETE");
      (IMPORT_FAILED, "IMPORT_FAILED");
      (UPDATE_COMPLETE, "UPDATE_COMPLETE");
      (UPDATE_FAILED, "UPDATE_FAILED");
      (UPDATE_IN_PROGRESS, "UPDATE_IN_PROGRESS");
      (DELETE_SKIPPED, "DELETE_SKIPPED");
      (DELETE_COMPLETE, "DELETE_COMPLETE");
      (DELETE_FAILED, "DELETE_FAILED");
      (DELETE_IN_PROGRESS, "DELETE_IN_PROGRESS");
      (CREATE_COMPLETE, "CREATE_COMPLETE");
      (CREATE_FAILED, "CREATE_FAILED");
      (CREATE_IN_PROGRESS, "CREATE_IN_PROGRESS")]
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
module StackSetOperationAction =
  struct
    type t =
      | CREATE 
      | UPDATE 
      | DELETE 
      | DETECT_DRIFT 
    let str_to_t =
      [("DETECT_DRIFT", DETECT_DRIFT);
      ("DELETE", DELETE);
      ("UPDATE", UPDATE);
      ("CREATE", CREATE)]
    let t_to_str =
      [(DETECT_DRIFT, "DETECT_DRIFT");
      (DELETE, "DELETE");
      (UPDATE, "UPDATE");
      (CREATE, "CREATE")]
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
module StackSetOperationStatus =
  struct
    type t =
      | RUNNING 
      | SUCCEEDED 
      | FAILED 
      | STOPPING 
      | STOPPED 
      | QUEUED 
    let str_to_t =
      [("QUEUED", QUEUED);
      ("STOPPED", STOPPED);
      ("STOPPING", STOPPING);
      ("FAILED", FAILED);
      ("SUCCEEDED", SUCCEEDED);
      ("RUNNING", RUNNING)]
    let t_to_str =
      [(QUEUED, "QUEUED");
      (STOPPED, "STOPPED");
      (STOPPING, "STOPPING");
      (FAILED, "FAILED");
      (SUCCEEDED, "SUCCEEDED");
      (RUNNING, "RUNNING")]
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
module StackResourceDriftInformationSummary =
  struct
    type t =
      {
      stack_resource_drift_status: StackResourceDriftStatus.t ;
      last_check_timestamp: DateTime.t option }
    let make ~stack_resource_drift_status  ?last_check_timestamp  () =
      { stack_resource_drift_status; last_check_timestamp }
    let parse xml =
      Some
        {
          stack_resource_drift_status =
            (Xml.required "StackResourceDriftStatus"
               (Util.option_bind (Xml.member "StackResourceDriftStatus" xml)
                  StackResourceDriftStatus.parse));
          last_check_timestamp =
            (Util.option_bind (Xml.member "LastCheckTimestamp" xml)
               DateTime.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.last_check_timestamp
              (fun f ->
                 Query.Pair ("LastCheckTimestamp", (DateTime.to_query f)));
           Some
             (Query.Pair
                ("StackResourceDriftStatus",
                  (StackResourceDriftStatus.to_query
                     v.stack_resource_drift_status)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.last_check_timestamp
              (fun f -> ("last_check_timestamp", (DateTime.to_json f)));
           Some
             ("stack_resource_drift_status",
               (StackResourceDriftStatus.to_json
                  v.stack_resource_drift_status))])
    let of_json j =
      {
        stack_resource_drift_status =
          (StackResourceDriftStatus.of_json
             (Util.of_option_exn
                (Json.lookup j "stack_resource_drift_status")));
        last_check_timestamp =
          (Util.option_map (Json.lookup j "last_check_timestamp")
             DateTime.of_json)
      }
  end
module StackInstanceFilterName =
  struct
    type t =
      | DETAILED_STATUS 
    let str_to_t = [("DETAILED_STATUS", DETAILED_STATUS)]
    let t_to_str = [(DETAILED_STATUS, "DETAILED_STATUS")]
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
module ParameterConstraints =
  struct
    type t = {
      allowed_values: AllowedValues.t }
    let make ?(allowed_values= [])  () = { allowed_values }
    let parse xml =
      Some
        {
          allowed_values =
            (Util.of_option []
               (Util.option_bind (Xml.member "AllowedValues" xml)
                  AllowedValues.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("AllowedValues.member",
                   (AllowedValues.to_query v.allowed_values)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("allowed_values", (AllowedValues.to_json v.allowed_values))])
    let of_json j =
      {
        allowed_values =
          (AllowedValues.of_json
             (Util.of_option_exn (Json.lookup j "allowed_values")))
      }
  end
module LogicalResourceIds =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module ResourceIdentifiers =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module StackInstanceComprehensiveStatus =
  struct
    type t = {
      detailed_status: StackInstanceDetailedStatus.t option }
    let make ?detailed_status  () = { detailed_status }
    let parse xml =
      Some
        {
          detailed_status =
            (Util.option_bind (Xml.member "DetailedStatus" xml)
               StackInstanceDetailedStatus.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.detailed_status
              (fun f ->
                 Query.Pair
                   ("DetailedStatus",
                     (StackInstanceDetailedStatus.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.detailed_status
              (fun f ->
                 ("detailed_status", (StackInstanceDetailedStatus.to_json f)))])
    let of_json j =
      {
        detailed_status =
          (Util.option_map (Json.lookup j "detailed_status")
             StackInstanceDetailedStatus.of_json)
      }
  end
module StackInstanceStatus =
  struct
    type t =
      | CURRENT 
      | OUTDATED 
      | INOPERABLE 
    let str_to_t =
      [("INOPERABLE", INOPERABLE);
      ("OUTDATED", OUTDATED);
      ("CURRENT", CURRENT)]
    let t_to_str =
      [(INOPERABLE, "INOPERABLE");
      (OUTDATED, "OUTDATED");
      (CURRENT, "CURRENT")]
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
module Capabilities =
  struct
    type t = Capability.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Capability.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Capability.to_query v
    let to_json v = `List (List.map Capability.to_json v)
    let of_json j = Json.to_list Capability.of_json j
  end
module NotificationARNs =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module Outputs =
  struct
    type t = Output.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Output.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Output.to_query v
    let to_json v = `List (List.map Output.to_json v)
    let of_json j = Json.to_list Output.of_json j
  end
module Parameters =
  struct
    type t = Parameter.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Parameter.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Parameter.to_query v
    let to_json v = `List (List.map Parameter.to_json v)
    let of_json j = Json.to_list Parameter.of_json j
  end
module RollbackConfiguration =
  struct
    type t =
      {
      rollback_triggers: RollbackTriggers.t ;
      monitoring_time_in_minutes: Integer.t option }
    let make ?(rollback_triggers= [])  ?monitoring_time_in_minutes  () =
      { rollback_triggers; monitoring_time_in_minutes }
    let parse xml =
      Some
        {
          rollback_triggers =
            (Util.of_option []
               (Util.option_bind (Xml.member "RollbackTriggers" xml)
                  RollbackTriggers.parse));
          monitoring_time_in_minutes =
            (Util.option_bind (Xml.member "MonitoringTimeInMinutes" xml)
               Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.monitoring_time_in_minutes
              (fun f ->
                 Query.Pair ("MonitoringTimeInMinutes", (Integer.to_query f)));
           Some
             (Query.Pair
                ("RollbackTriggers.member",
                  (RollbackTriggers.to_query v.rollback_triggers)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.monitoring_time_in_minutes
              (fun f -> ("monitoring_time_in_minutes", (Integer.to_json f)));
           Some
             ("rollback_triggers",
               (RollbackTriggers.to_json v.rollback_triggers))])
    let of_json j =
      {
        rollback_triggers =
          (RollbackTriggers.of_json
             (Util.of_option_exn (Json.lookup j "rollback_triggers")));
        monitoring_time_in_minutes =
          (Util.option_map (Json.lookup j "monitoring_time_in_minutes")
             Integer.of_json)
      }
  end
module StackDriftInformation =
  struct
    type t =
      {
      stack_drift_status: StackDriftStatus.t ;
      last_check_timestamp: DateTime.t option }
    let make ~stack_drift_status  ?last_check_timestamp  () =
      { stack_drift_status; last_check_timestamp }
    let parse xml =
      Some
        {
          stack_drift_status =
            (Xml.required "StackDriftStatus"
               (Util.option_bind (Xml.member "StackDriftStatus" xml)
                  StackDriftStatus.parse));
          last_check_timestamp =
            (Util.option_bind (Xml.member "LastCheckTimestamp" xml)
               DateTime.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.last_check_timestamp
              (fun f ->
                 Query.Pair ("LastCheckTimestamp", (DateTime.to_query f)));
           Some
             (Query.Pair
                ("StackDriftStatus",
                  (StackDriftStatus.to_query v.stack_drift_status)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.last_check_timestamp
              (fun f -> ("last_check_timestamp", (DateTime.to_json f)));
           Some
             ("stack_drift_status",
               (StackDriftStatus.to_json v.stack_drift_status))])
    let of_json j =
      {
        stack_drift_status =
          (StackDriftStatus.of_json
             (Util.of_option_exn (Json.lookup j "stack_drift_status")));
        last_check_timestamp =
          (Util.option_map (Json.lookup j "last_check_timestamp")
             DateTime.of_json)
      }
  end
module Tags =
  struct
    type t = Tag.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Tag.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Tag.to_query v
    let to_json v = `List (List.map Tag.to_json v)
    let of_json j = Json.to_list Tag.of_json j
  end
module StackResourceDriftInformation =
  struct
    type t =
      {
      stack_resource_drift_status: StackResourceDriftStatus.t ;
      last_check_timestamp: DateTime.t option }
    let make ~stack_resource_drift_status  ?last_check_timestamp  () =
      { stack_resource_drift_status; last_check_timestamp }
    let parse xml =
      Some
        {
          stack_resource_drift_status =
            (Xml.required "StackResourceDriftStatus"
               (Util.option_bind (Xml.member "StackResourceDriftStatus" xml)
                  StackResourceDriftStatus.parse));
          last_check_timestamp =
            (Util.option_bind (Xml.member "LastCheckTimestamp" xml)
               DateTime.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.last_check_timestamp
              (fun f ->
                 Query.Pair ("LastCheckTimestamp", (DateTime.to_query f)));
           Some
             (Query.Pair
                ("StackResourceDriftStatus",
                  (StackResourceDriftStatus.to_query
                     v.stack_resource_drift_status)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.last_check_timestamp
              (fun f -> ("last_check_timestamp", (DateTime.to_json f)));
           Some
             ("stack_resource_drift_status",
               (StackResourceDriftStatus.to_json
                  v.stack_resource_drift_status))])
    let of_json j =
      {
        stack_resource_drift_status =
          (StackResourceDriftStatus.of_json
             (Util.of_option_exn
                (Json.lookup j "stack_resource_drift_status")));
        last_check_timestamp =
          (Util.option_map (Json.lookup j "last_check_timestamp")
             DateTime.of_json)
      }
  end
module StackSetOperationResultSummary =
  struct
    type t =
      {
      account: String.t option ;
      region: String.t option ;
      status: StackSetOperationResultStatus.t option ;
      status_reason: String.t option ;
      account_gate_result: AccountGateResult.t option ;
      organizational_unit_id: String.t option }
    let make ?account  ?region  ?status  ?status_reason  ?account_gate_result
       ?organizational_unit_id  () =
      {
        account;
        region;
        status;
        status_reason;
        account_gate_result;
        organizational_unit_id
      }
    let parse xml =
      Some
        {
          account =
            (Util.option_bind (Xml.member "Account" xml) String.parse);
          region = (Util.option_bind (Xml.member "Region" xml) String.parse);
          status =
            (Util.option_bind (Xml.member "Status" xml)
               StackSetOperationResultStatus.parse);
          status_reason =
            (Util.option_bind (Xml.member "StatusReason" xml) String.parse);
          account_gate_result =
            (Util.option_bind (Xml.member "AccountGateResult" xml)
               AccountGateResult.parse);
          organizational_unit_id =
            (Util.option_bind (Xml.member "OrganizationalUnitId" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.organizational_unit_id
              (fun f ->
                 Query.Pair ("OrganizationalUnitId", (String.to_query f)));
           Util.option_map v.account_gate_result
             (fun f ->
                Query.Pair
                  ("AccountGateResult", (AccountGateResult.to_query f)));
           Util.option_map v.status_reason
             (fun f -> Query.Pair ("StatusReason", (String.to_query f)));
           Util.option_map v.status
             (fun f ->
                Query.Pair
                  ("Status", (StackSetOperationResultStatus.to_query f)));
           Util.option_map v.region
             (fun f -> Query.Pair ("Region", (String.to_query f)));
           Util.option_map v.account
             (fun f -> Query.Pair ("Account", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.organizational_unit_id
              (fun f -> ("organizational_unit_id", (String.to_json f)));
           Util.option_map v.account_gate_result
             (fun f -> ("account_gate_result", (AccountGateResult.to_json f)));
           Util.option_map v.status_reason
             (fun f -> ("status_reason", (String.to_json f)));
           Util.option_map v.status
             (fun f -> ("status", (StackSetOperationResultStatus.to_json f)));
           Util.option_map v.region (fun f -> ("region", (String.to_json f)));
           Util.option_map v.account
             (fun f -> ("account", (String.to_json f)))])
    let of_json j =
      {
        account = (Util.option_map (Json.lookup j "account") String.of_json);
        region = (Util.option_map (Json.lookup j "region") String.of_json);
        status =
          (Util.option_map (Json.lookup j "status")
             StackSetOperationResultStatus.of_json);
        status_reason =
          (Util.option_map (Json.lookup j "status_reason") String.of_json);
        account_gate_result =
          (Util.option_map (Json.lookup j "account_gate_result")
             AccountGateResult.of_json);
        organizational_unit_id =
          (Util.option_map (Json.lookup j "organizational_unit_id")
             String.of_json)
      }
  end
module AccountLimit =
  struct
    type t = {
      name: String.t option ;
      value: Integer.t option }
    let make ?name  ?value  () = { name; value }
    let parse xml =
      Some
        {
          name = (Util.option_bind (Xml.member "Name" xml) String.parse);
          value = (Util.option_bind (Xml.member "Value" xml) Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.value
              (fun f -> Query.Pair ("Value", (Integer.to_query f)));
           Util.option_map v.name
             (fun f -> Query.Pair ("Name", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.value (fun f -> ("value", (Integer.to_json f)));
           Util.option_map v.name (fun f -> ("name", (String.to_json f)))])
    let of_json j =
      {
        name = (Util.option_map (Json.lookup j "name") String.of_json);
        value = (Util.option_map (Json.lookup j "value") Integer.of_json)
      }
  end
module StackSummary =
  struct
    type t =
      {
      stack_id: String.t option ;
      stack_name: String.t ;
      template_description: String.t option ;
      creation_time: DateTime.t ;
      last_updated_time: DateTime.t option ;
      deletion_time: DateTime.t option ;
      stack_status: StackStatus.t ;
      stack_status_reason: String.t option ;
      parent_id: String.t option ;
      root_id: String.t option ;
      drift_information: StackDriftInformationSummary.t option }
    let make ?stack_id  ~stack_name  ?template_description  ~creation_time 
      ?last_updated_time  ?deletion_time  ~stack_status  ?stack_status_reason
       ?parent_id  ?root_id  ?drift_information  () =
      {
        stack_id;
        stack_name;
        template_description;
        creation_time;
        last_updated_time;
        deletion_time;
        stack_status;
        stack_status_reason;
        parent_id;
        root_id;
        drift_information
      }
    let parse xml =
      Some
        {
          stack_id =
            (Util.option_bind (Xml.member "StackId" xml) String.parse);
          stack_name =
            (Xml.required "StackName"
               (Util.option_bind (Xml.member "StackName" xml) String.parse));
          template_description =
            (Util.option_bind (Xml.member "TemplateDescription" xml)
               String.parse);
          creation_time =
            (Xml.required "CreationTime"
               (Util.option_bind (Xml.member "CreationTime" xml)
                  DateTime.parse));
          last_updated_time =
            (Util.option_bind (Xml.member "LastUpdatedTime" xml)
               DateTime.parse);
          deletion_time =
            (Util.option_bind (Xml.member "DeletionTime" xml) DateTime.parse);
          stack_status =
            (Xml.required "StackStatus"
               (Util.option_bind (Xml.member "StackStatus" xml)
                  StackStatus.parse));
          stack_status_reason =
            (Util.option_bind (Xml.member "StackStatusReason" xml)
               String.parse);
          parent_id =
            (Util.option_bind (Xml.member "ParentId" xml) String.parse);
          root_id = (Util.option_bind (Xml.member "RootId" xml) String.parse);
          drift_information =
            (Util.option_bind (Xml.member "DriftInformation" xml)
               StackDriftInformationSummary.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.drift_information
              (fun f ->
                 Query.Pair
                   ("DriftInformation",
                     (StackDriftInformationSummary.to_query f)));
           Util.option_map v.root_id
             (fun f -> Query.Pair ("RootId", (String.to_query f)));
           Util.option_map v.parent_id
             (fun f -> Query.Pair ("ParentId", (String.to_query f)));
           Util.option_map v.stack_status_reason
             (fun f -> Query.Pair ("StackStatusReason", (String.to_query f)));
           Some
             (Query.Pair
                ("StackStatus", (StackStatus.to_query v.stack_status)));
           Util.option_map v.deletion_time
             (fun f -> Query.Pair ("DeletionTime", (DateTime.to_query f)));
           Util.option_map v.last_updated_time
             (fun f -> Query.Pair ("LastUpdatedTime", (DateTime.to_query f)));
           Some
             (Query.Pair
                ("CreationTime", (DateTime.to_query v.creation_time)));
           Util.option_map v.template_description
             (fun f ->
                Query.Pair ("TemplateDescription", (String.to_query f)));
           Some (Query.Pair ("StackName", (String.to_query v.stack_name)));
           Util.option_map v.stack_id
             (fun f -> Query.Pair ("StackId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.drift_information
              (fun f ->
                 ("drift_information",
                   (StackDriftInformationSummary.to_json f)));
           Util.option_map v.root_id
             (fun f -> ("root_id", (String.to_json f)));
           Util.option_map v.parent_id
             (fun f -> ("parent_id", (String.to_json f)));
           Util.option_map v.stack_status_reason
             (fun f -> ("stack_status_reason", (String.to_json f)));
           Some ("stack_status", (StackStatus.to_json v.stack_status));
           Util.option_map v.deletion_time
             (fun f -> ("deletion_time", (DateTime.to_json f)));
           Util.option_map v.last_updated_time
             (fun f -> ("last_updated_time", (DateTime.to_json f)));
           Some ("creation_time", (DateTime.to_json v.creation_time));
           Util.option_map v.template_description
             (fun f -> ("template_description", (String.to_json f)));
           Some ("stack_name", (String.to_json v.stack_name));
           Util.option_map v.stack_id
             (fun f -> ("stack_id", (String.to_json f)))])
    let of_json j =
      {
        stack_id =
          (Util.option_map (Json.lookup j "stack_id") String.of_json);
        stack_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "stack_name")));
        template_description =
          (Util.option_map (Json.lookup j "template_description")
             String.of_json);
        creation_time =
          (DateTime.of_json
             (Util.of_option_exn (Json.lookup j "creation_time")));
        last_updated_time =
          (Util.option_map (Json.lookup j "last_updated_time")
             DateTime.of_json);
        deletion_time =
          (Util.option_map (Json.lookup j "deletion_time") DateTime.of_json);
        stack_status =
          (StackStatus.of_json
             (Util.of_option_exn (Json.lookup j "stack_status")));
        stack_status_reason =
          (Util.option_map (Json.lookup j "stack_status_reason")
             String.of_json);
        parent_id =
          (Util.option_map (Json.lookup j "parent_id") String.of_json);
        root_id = (Util.option_map (Json.lookup j "root_id") String.of_json);
        drift_information =
          (Util.option_map (Json.lookup j "drift_information")
             StackDriftInformationSummary.of_json)
      }
  end
module StackResourceDrift =
  struct
    type t =
      {
      stack_id: String.t ;
      logical_resource_id: String.t ;
      physical_resource_id: String.t option ;
      physical_resource_id_context: PhysicalResourceIdContext.t ;
      resource_type: String.t ;
      expected_properties: String.t option ;
      actual_properties: String.t option ;
      property_differences: PropertyDifferences.t ;
      stack_resource_drift_status: StackResourceDriftStatus.t ;
      timestamp: DateTime.t }
    let make ~stack_id  ~logical_resource_id  ?physical_resource_id 
      ?(physical_resource_id_context= [])  ~resource_type 
      ?expected_properties  ?actual_properties  ?(property_differences= []) 
      ~stack_resource_drift_status  ~timestamp  () =
      {
        stack_id;
        logical_resource_id;
        physical_resource_id;
        physical_resource_id_context;
        resource_type;
        expected_properties;
        actual_properties;
        property_differences;
        stack_resource_drift_status;
        timestamp
      }
    let parse xml =
      Some
        {
          stack_id =
            (Xml.required "StackId"
               (Util.option_bind (Xml.member "StackId" xml) String.parse));
          logical_resource_id =
            (Xml.required "LogicalResourceId"
               (Util.option_bind (Xml.member "LogicalResourceId" xml)
                  String.parse));
          physical_resource_id =
            (Util.option_bind (Xml.member "PhysicalResourceId" xml)
               String.parse);
          physical_resource_id_context =
            (Util.of_option []
               (Util.option_bind (Xml.member "PhysicalResourceIdContext" xml)
                  PhysicalResourceIdContext.parse));
          resource_type =
            (Xml.required "ResourceType"
               (Util.option_bind (Xml.member "ResourceType" xml) String.parse));
          expected_properties =
            (Util.option_bind (Xml.member "ExpectedProperties" xml)
               String.parse);
          actual_properties =
            (Util.option_bind (Xml.member "ActualProperties" xml)
               String.parse);
          property_differences =
            (Util.of_option []
               (Util.option_bind (Xml.member "PropertyDifferences" xml)
                  PropertyDifferences.parse));
          stack_resource_drift_status =
            (Xml.required "StackResourceDriftStatus"
               (Util.option_bind (Xml.member "StackResourceDriftStatus" xml)
                  StackResourceDriftStatus.parse));
          timestamp =
            (Xml.required "Timestamp"
               (Util.option_bind (Xml.member "Timestamp" xml) DateTime.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Timestamp", (DateTime.to_query v.timestamp)));
           Some
             (Query.Pair
                ("StackResourceDriftStatus",
                  (StackResourceDriftStatus.to_query
                     v.stack_resource_drift_status)));
           Some
             (Query.Pair
                ("PropertyDifferences.member",
                  (PropertyDifferences.to_query v.property_differences)));
           Util.option_map v.actual_properties
             (fun f -> Query.Pair ("ActualProperties", (String.to_query f)));
           Util.option_map v.expected_properties
             (fun f -> Query.Pair ("ExpectedProperties", (String.to_query f)));
           Some
             (Query.Pair ("ResourceType", (String.to_query v.resource_type)));
           Some
             (Query.Pair
                ("PhysicalResourceIdContext.member",
                  (PhysicalResourceIdContext.to_query
                     v.physical_resource_id_context)));
           Util.option_map v.physical_resource_id
             (fun f -> Query.Pair ("PhysicalResourceId", (String.to_query f)));
           Some
             (Query.Pair
                ("LogicalResourceId",
                  (String.to_query v.logical_resource_id)));
           Some (Query.Pair ("StackId", (String.to_query v.stack_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("timestamp", (DateTime.to_json v.timestamp));
           Some
             ("stack_resource_drift_status",
               (StackResourceDriftStatus.to_json
                  v.stack_resource_drift_status));
           Some
             ("property_differences",
               (PropertyDifferences.to_json v.property_differences));
           Util.option_map v.actual_properties
             (fun f -> ("actual_properties", (String.to_json f)));
           Util.option_map v.expected_properties
             (fun f -> ("expected_properties", (String.to_json f)));
           Some ("resource_type", (String.to_json v.resource_type));
           Some
             ("physical_resource_id_context",
               (PhysicalResourceIdContext.to_json
                  v.physical_resource_id_context));
           Util.option_map v.physical_resource_id
             (fun f -> ("physical_resource_id", (String.to_json f)));
           Some
             ("logical_resource_id", (String.to_json v.logical_resource_id));
           Some ("stack_id", (String.to_json v.stack_id))])
    let of_json j =
      {
        stack_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "stack_id")));
        logical_resource_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "logical_resource_id")));
        physical_resource_id =
          (Util.option_map (Json.lookup j "physical_resource_id")
             String.of_json);
        physical_resource_id_context =
          (PhysicalResourceIdContext.of_json
             (Util.of_option_exn
                (Json.lookup j "physical_resource_id_context")));
        resource_type =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "resource_type")));
        expected_properties =
          (Util.option_map (Json.lookup j "expected_properties")
             String.of_json);
        actual_properties =
          (Util.option_map (Json.lookup j "actual_properties") String.of_json);
        property_differences =
          (PropertyDifferences.of_json
             (Util.of_option_exn (Json.lookup j "property_differences")));
        stack_resource_drift_status =
          (StackResourceDriftStatus.of_json
             (Util.of_option_exn
                (Json.lookup j "stack_resource_drift_status")));
        timestamp =
          (DateTime.of_json (Util.of_option_exn (Json.lookup j "timestamp")))
      }
  end
module Change =
  struct
    type t =
      {
      type_: ChangeType.t option ;
      resource_change: ResourceChange.t option }
    let make ?type_  ?resource_change  () = { type_; resource_change }
    let parse xml =
      Some
        {
          type_ = (Util.option_bind (Xml.member "Type" xml) ChangeType.parse);
          resource_change =
            (Util.option_bind (Xml.member "ResourceChange" xml)
               ResourceChange.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.resource_change
              (fun f ->
                 Query.Pair ("ResourceChange", (ResourceChange.to_query f)));
           Util.option_map v.type_
             (fun f -> Query.Pair ("Type", (ChangeType.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.resource_change
              (fun f -> ("resource_change", (ResourceChange.to_json f)));
           Util.option_map v.type_
             (fun f -> ("type_", (ChangeType.to_json f)))])
    let of_json j =
      {
        type_ = (Util.option_map (Json.lookup j "type_") ChangeType.of_json);
        resource_change =
          (Util.option_map (Json.lookup j "resource_change")
             ResourceChange.of_json)
      }
  end
module ChangeSetSummary =
  struct
    type t =
      {
      stack_id: String.t option ;
      stack_name: String.t option ;
      change_set_id: String.t option ;
      change_set_name: String.t option ;
      execution_status: ExecutionStatus.t option ;
      status: ChangeSetStatus.t option ;
      status_reason: String.t option ;
      creation_time: DateTime.t option ;
      description: String.t option }
    let make ?stack_id  ?stack_name  ?change_set_id  ?change_set_name 
      ?execution_status  ?status  ?status_reason  ?creation_time 
      ?description  () =
      {
        stack_id;
        stack_name;
        change_set_id;
        change_set_name;
        execution_status;
        status;
        status_reason;
        creation_time;
        description
      }
    let parse xml =
      Some
        {
          stack_id =
            (Util.option_bind (Xml.member "StackId" xml) String.parse);
          stack_name =
            (Util.option_bind (Xml.member "StackName" xml) String.parse);
          change_set_id =
            (Util.option_bind (Xml.member "ChangeSetId" xml) String.parse);
          change_set_name =
            (Util.option_bind (Xml.member "ChangeSetName" xml) String.parse);
          execution_status =
            (Util.option_bind (Xml.member "ExecutionStatus" xml)
               ExecutionStatus.parse);
          status =
            (Util.option_bind (Xml.member "Status" xml) ChangeSetStatus.parse);
          status_reason =
            (Util.option_bind (Xml.member "StatusReason" xml) String.parse);
          creation_time =
            (Util.option_bind (Xml.member "CreationTime" xml) DateTime.parse);
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.description
              (fun f -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.creation_time
             (fun f -> Query.Pair ("CreationTime", (DateTime.to_query f)));
           Util.option_map v.status_reason
             (fun f -> Query.Pair ("StatusReason", (String.to_query f)));
           Util.option_map v.status
             (fun f -> Query.Pair ("Status", (ChangeSetStatus.to_query f)));
           Util.option_map v.execution_status
             (fun f ->
                Query.Pair ("ExecutionStatus", (ExecutionStatus.to_query f)));
           Util.option_map v.change_set_name
             (fun f -> Query.Pair ("ChangeSetName", (String.to_query f)));
           Util.option_map v.change_set_id
             (fun f -> Query.Pair ("ChangeSetId", (String.to_query f)));
           Util.option_map v.stack_name
             (fun f -> Query.Pair ("StackName", (String.to_query f)));
           Util.option_map v.stack_id
             (fun f -> Query.Pair ("StackId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.description
              (fun f -> ("description", (String.to_json f)));
           Util.option_map v.creation_time
             (fun f -> ("creation_time", (DateTime.to_json f)));
           Util.option_map v.status_reason
             (fun f -> ("status_reason", (String.to_json f)));
           Util.option_map v.status
             (fun f -> ("status", (ChangeSetStatus.to_json f)));
           Util.option_map v.execution_status
             (fun f -> ("execution_status", (ExecutionStatus.to_json f)));
           Util.option_map v.change_set_name
             (fun f -> ("change_set_name", (String.to_json f)));
           Util.option_map v.change_set_id
             (fun f -> ("change_set_id", (String.to_json f)));
           Util.option_map v.stack_name
             (fun f -> ("stack_name", (String.to_json f)));
           Util.option_map v.stack_id
             (fun f -> ("stack_id", (String.to_json f)))])
    let of_json j =
      {
        stack_id =
          (Util.option_map (Json.lookup j "stack_id") String.of_json);
        stack_name =
          (Util.option_map (Json.lookup j "stack_name") String.of_json);
        change_set_id =
          (Util.option_map (Json.lookup j "change_set_id") String.of_json);
        change_set_name =
          (Util.option_map (Json.lookup j "change_set_name") String.of_json);
        execution_status =
          (Util.option_map (Json.lookup j "execution_status")
             ExecutionStatus.of_json);
        status =
          (Util.option_map (Json.lookup j "status") ChangeSetStatus.of_json);
        status_reason =
          (Util.option_map (Json.lookup j "status_reason") String.of_json);
        creation_time =
          (Util.option_map (Json.lookup j "creation_time") DateTime.of_json);
        description =
          (Util.option_map (Json.lookup j "description") String.of_json)
      }
  end
module Export =
  struct
    type t =
      {
      exporting_stack_id: String.t option ;
      name: String.t option ;
      value: String.t option }
    let make ?exporting_stack_id  ?name  ?value  () =
      { exporting_stack_id; name; value }
    let parse xml =
      Some
        {
          exporting_stack_id =
            (Util.option_bind (Xml.member "ExportingStackId" xml)
               String.parse);
          name = (Util.option_bind (Xml.member "Name" xml) String.parse);
          value = (Util.option_bind (Xml.member "Value" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.value
              (fun f -> Query.Pair ("Value", (String.to_query f)));
           Util.option_map v.name
             (fun f -> Query.Pair ("Name", (String.to_query f)));
           Util.option_map v.exporting_stack_id
             (fun f -> Query.Pair ("ExportingStackId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.value (fun f -> ("value", (String.to_json f)));
           Util.option_map v.name (fun f -> ("name", (String.to_json f)));
           Util.option_map v.exporting_stack_id
             (fun f -> ("exporting_stack_id", (String.to_json f)))])
    let of_json j =
      {
        exporting_stack_id =
          (Util.option_map (Json.lookup j "exporting_stack_id")
             String.of_json);
        name = (Util.option_map (Json.lookup j "name") String.of_json);
        value = (Util.option_map (Json.lookup j "value") String.of_json)
      }
  end
module ResourceToImport =
  struct
    type t =
      {
      resource_type: String.t ;
      logical_resource_id: String.t ;
      resource_identifier: ResourceIdentifierProperties.t }
    let make ~resource_type  ~logical_resource_id  ~resource_identifier  () =
      { resource_type; logical_resource_id; resource_identifier }
    let parse xml =
      Some
        {
          resource_type =
            (Xml.required "ResourceType"
               (Util.option_bind (Xml.member "ResourceType" xml) String.parse));
          logical_resource_id =
            (Xml.required "LogicalResourceId"
               (Util.option_bind (Xml.member "LogicalResourceId" xml)
                  String.parse));
          resource_identifier =
            (Xml.required "ResourceIdentifier"
               (Util.option_bind (Xml.member "ResourceIdentifier" xml)
                  ResourceIdentifierProperties.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ResourceIdentifier",
                   (ResourceIdentifierProperties.to_query
                      v.resource_identifier)));
           Some
             (Query.Pair
                ("LogicalResourceId",
                  (String.to_query v.logical_resource_id)));
           Some
             (Query.Pair ("ResourceType", (String.to_query v.resource_type)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("resource_identifier",
                (ResourceIdentifierProperties.to_json v.resource_identifier));
           Some
             ("logical_resource_id", (String.to_json v.logical_resource_id));
           Some ("resource_type", (String.to_json v.resource_type))])
    let of_json j =
      {
        resource_type =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "resource_type")));
        logical_resource_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "logical_resource_id")));
        resource_identifier =
          (ResourceIdentifierProperties.of_json
             (Util.of_option_exn (Json.lookup j "resource_identifier")))
      }
  end
module TemplateParameter =
  struct
    type t =
      {
      parameter_key: String.t option ;
      default_value: String.t option ;
      no_echo: Boolean.t option ;
      description: String.t option }
    let make ?parameter_key  ?default_value  ?no_echo  ?description  () =
      { parameter_key; default_value; no_echo; description }
    let parse xml =
      Some
        {
          parameter_key =
            (Util.option_bind (Xml.member "ParameterKey" xml) String.parse);
          default_value =
            (Util.option_bind (Xml.member "DefaultValue" xml) String.parse);
          no_echo =
            (Util.option_bind (Xml.member "NoEcho" xml) Boolean.parse);
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.description
              (fun f -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.no_echo
             (fun f -> Query.Pair ("NoEcho", (Boolean.to_query f)));
           Util.option_map v.default_value
             (fun f -> Query.Pair ("DefaultValue", (String.to_query f)));
           Util.option_map v.parameter_key
             (fun f -> Query.Pair ("ParameterKey", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.description
              (fun f -> ("description", (String.to_json f)));
           Util.option_map v.no_echo
             (fun f -> ("no_echo", (Boolean.to_json f)));
           Util.option_map v.default_value
             (fun f -> ("default_value", (String.to_json f)));
           Util.option_map v.parameter_key
             (fun f -> ("parameter_key", (String.to_json f)))])
    let of_json j =
      {
        parameter_key =
          (Util.option_map (Json.lookup j "parameter_key") String.of_json);
        default_value =
          (Util.option_map (Json.lookup j "default_value") String.of_json);
        no_echo = (Util.option_map (Json.lookup j "no_echo") Boolean.of_json);
        description =
          (Util.option_map (Json.lookup j "description") String.of_json)
      }
  end
module StackSetSummary =
  struct
    type t =
      {
      stack_set_name: String.t option ;
      stack_set_id: String.t option ;
      description: String.t option ;
      status: StackSetStatus.t option ;
      auto_deployment: AutoDeployment.t option ;
      permission_model: PermissionModels.t option ;
      drift_status: StackDriftStatus.t option ;
      last_drift_check_timestamp: DateTime.t option }
    let make ?stack_set_name  ?stack_set_id  ?description  ?status 
      ?auto_deployment  ?permission_model  ?drift_status 
      ?last_drift_check_timestamp  () =
      {
        stack_set_name;
        stack_set_id;
        description;
        status;
        auto_deployment;
        permission_model;
        drift_status;
        last_drift_check_timestamp
      }
    let parse xml =
      Some
        {
          stack_set_name =
            (Util.option_bind (Xml.member "StackSetName" xml) String.parse);
          stack_set_id =
            (Util.option_bind (Xml.member "StackSetId" xml) String.parse);
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse);
          status =
            (Util.option_bind (Xml.member "Status" xml) StackSetStatus.parse);
          auto_deployment =
            (Util.option_bind (Xml.member "AutoDeployment" xml)
               AutoDeployment.parse);
          permission_model =
            (Util.option_bind (Xml.member "PermissionModel" xml)
               PermissionModels.parse);
          drift_status =
            (Util.option_bind (Xml.member "DriftStatus" xml)
               StackDriftStatus.parse);
          last_drift_check_timestamp =
            (Util.option_bind (Xml.member "LastDriftCheckTimestamp" xml)
               DateTime.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.last_drift_check_timestamp
              (fun f ->
                 Query.Pair
                   ("LastDriftCheckTimestamp", (DateTime.to_query f)));
           Util.option_map v.drift_status
             (fun f ->
                Query.Pair ("DriftStatus", (StackDriftStatus.to_query f)));
           Util.option_map v.permission_model
             (fun f ->
                Query.Pair ("PermissionModel", (PermissionModels.to_query f)));
           Util.option_map v.auto_deployment
             (fun f ->
                Query.Pair ("AutoDeployment", (AutoDeployment.to_query f)));
           Util.option_map v.status
             (fun f -> Query.Pair ("Status", (StackSetStatus.to_query f)));
           Util.option_map v.description
             (fun f -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.stack_set_id
             (fun f -> Query.Pair ("StackSetId", (String.to_query f)));
           Util.option_map v.stack_set_name
             (fun f -> Query.Pair ("StackSetName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.last_drift_check_timestamp
              (fun f -> ("last_drift_check_timestamp", (DateTime.to_json f)));
           Util.option_map v.drift_status
             (fun f -> ("drift_status", (StackDriftStatus.to_json f)));
           Util.option_map v.permission_model
             (fun f -> ("permission_model", (PermissionModels.to_json f)));
           Util.option_map v.auto_deployment
             (fun f -> ("auto_deployment", (AutoDeployment.to_json f)));
           Util.option_map v.status
             (fun f -> ("status", (StackSetStatus.to_json f)));
           Util.option_map v.description
             (fun f -> ("description", (String.to_json f)));
           Util.option_map v.stack_set_id
             (fun f -> ("stack_set_id", (String.to_json f)));
           Util.option_map v.stack_set_name
             (fun f -> ("stack_set_name", (String.to_json f)))])
    let of_json j =
      {
        stack_set_name =
          (Util.option_map (Json.lookup j "stack_set_name") String.of_json);
        stack_set_id =
          (Util.option_map (Json.lookup j "stack_set_id") String.of_json);
        description =
          (Util.option_map (Json.lookup j "description") String.of_json);
        status =
          (Util.option_map (Json.lookup j "status") StackSetStatus.of_json);
        auto_deployment =
          (Util.option_map (Json.lookup j "auto_deployment")
             AutoDeployment.of_json);
        permission_model =
          (Util.option_map (Json.lookup j "permission_model")
             PermissionModels.of_json);
        drift_status =
          (Util.option_map (Json.lookup j "drift_status")
             StackDriftStatus.of_json);
        last_drift_check_timestamp =
          (Util.option_map (Json.lookup j "last_drift_check_timestamp")
             DateTime.of_json)
      }
  end
module TypeSummary =
  struct
    type t =
      {
      type_: RegistryType.t option ;
      type_name: String.t option ;
      default_version_id: String.t option ;
      type_arn: String.t option ;
      last_updated: DateTime.t option ;
      description: String.t option }
    let make ?type_  ?type_name  ?default_version_id  ?type_arn 
      ?last_updated  ?description  () =
      {
        type_;
        type_name;
        default_version_id;
        type_arn;
        last_updated;
        description
      }
    let parse xml =
      Some
        {
          type_ =
            (Util.option_bind (Xml.member "Type" xml) RegistryType.parse);
          type_name =
            (Util.option_bind (Xml.member "TypeName" xml) String.parse);
          default_version_id =
            (Util.option_bind (Xml.member "DefaultVersionId" xml)
               String.parse);
          type_arn =
            (Util.option_bind (Xml.member "TypeArn" xml) String.parse);
          last_updated =
            (Util.option_bind (Xml.member "LastUpdated" xml) DateTime.parse);
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.description
              (fun f -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.last_updated
             (fun f -> Query.Pair ("LastUpdated", (DateTime.to_query f)));
           Util.option_map v.type_arn
             (fun f -> Query.Pair ("TypeArn", (String.to_query f)));
           Util.option_map v.default_version_id
             (fun f -> Query.Pair ("DefaultVersionId", (String.to_query f)));
           Util.option_map v.type_name
             (fun f -> Query.Pair ("TypeName", (String.to_query f)));
           Util.option_map v.type_
             (fun f -> Query.Pair ("Type", (RegistryType.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.description
              (fun f -> ("description", (String.to_json f)));
           Util.option_map v.last_updated
             (fun f -> ("last_updated", (DateTime.to_json f)));
           Util.option_map v.type_arn
             (fun f -> ("type_arn", (String.to_json f)));
           Util.option_map v.default_version_id
             (fun f -> ("default_version_id", (String.to_json f)));
           Util.option_map v.type_name
             (fun f -> ("type_name", (String.to_json f)));
           Util.option_map v.type_
             (fun f -> ("type_", (RegistryType.to_json f)))])
    let of_json j =
      {
        type_ =
          (Util.option_map (Json.lookup j "type_") RegistryType.of_json);
        type_name =
          (Util.option_map (Json.lookup j "type_name") String.of_json);
        default_version_id =
          (Util.option_map (Json.lookup j "default_version_id")
             String.of_json);
        type_arn =
          (Util.option_map (Json.lookup j "type_arn") String.of_json);
        last_updated =
          (Util.option_map (Json.lookup j "last_updated") DateTime.of_json);
        description =
          (Util.option_map (Json.lookup j "description") String.of_json)
      }
  end
module DeploymentTargets =
  struct
    type t =
      {
      accounts: AccountList.t ;
      organizational_unit_ids: OrganizationalUnitIdList.t }
    let make ?(accounts= [])  ?(organizational_unit_ids= [])  () =
      { accounts; organizational_unit_ids }
    let parse xml =
      Some
        {
          accounts =
            (Util.of_option []
               (Util.option_bind (Xml.member "Accounts" xml)
                  AccountList.parse));
          organizational_unit_ids =
            (Util.of_option []
               (Util.option_bind (Xml.member "OrganizationalUnitIds" xml)
                  OrganizationalUnitIdList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("OrganizationalUnitIds.member",
                   (OrganizationalUnitIdList.to_query
                      v.organizational_unit_ids)));
           Some
             (Query.Pair
                ("Accounts.member", (AccountList.to_query v.accounts)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("organizational_unit_ids",
                (OrganizationalUnitIdList.to_json v.organizational_unit_ids));
           Some ("accounts", (AccountList.to_json v.accounts))])
    let of_json j =
      {
        accounts =
          (AccountList.of_json
             (Util.of_option_exn (Json.lookup j "accounts")));
        organizational_unit_ids =
          (OrganizationalUnitIdList.of_json
             (Util.of_option_exn (Json.lookup j "organizational_unit_ids")))
      }
  end
module StackSetDriftDetectionDetails =
  struct
    type t =
      {
      drift_status: StackSetDriftStatus.t option ;
      drift_detection_status: StackSetDriftDetectionStatus.t option ;
      last_drift_check_timestamp: DateTime.t option ;
      total_stack_instances_count: Integer.t option ;
      drifted_stack_instances_count: Integer.t option ;
      in_sync_stack_instances_count: Integer.t option ;
      in_progress_stack_instances_count: Integer.t option ;
      failed_stack_instances_count: Integer.t option }
    let make ?drift_status  ?drift_detection_status 
      ?last_drift_check_timestamp  ?total_stack_instances_count 
      ?drifted_stack_instances_count  ?in_sync_stack_instances_count 
      ?in_progress_stack_instances_count  ?failed_stack_instances_count  () =
      {
        drift_status;
        drift_detection_status;
        last_drift_check_timestamp;
        total_stack_instances_count;
        drifted_stack_instances_count;
        in_sync_stack_instances_count;
        in_progress_stack_instances_count;
        failed_stack_instances_count
      }
    let parse xml =
      Some
        {
          drift_status =
            (Util.option_bind (Xml.member "DriftStatus" xml)
               StackSetDriftStatus.parse);
          drift_detection_status =
            (Util.option_bind (Xml.member "DriftDetectionStatus" xml)
               StackSetDriftDetectionStatus.parse);
          last_drift_check_timestamp =
            (Util.option_bind (Xml.member "LastDriftCheckTimestamp" xml)
               DateTime.parse);
          total_stack_instances_count =
            (Util.option_bind (Xml.member "TotalStackInstancesCount" xml)
               Integer.parse);
          drifted_stack_instances_count =
            (Util.option_bind (Xml.member "DriftedStackInstancesCount" xml)
               Integer.parse);
          in_sync_stack_instances_count =
            (Util.option_bind (Xml.member "InSyncStackInstancesCount" xml)
               Integer.parse);
          in_progress_stack_instances_count =
            (Util.option_bind
               (Xml.member "InProgressStackInstancesCount" xml) Integer.parse);
          failed_stack_instances_count =
            (Util.option_bind (Xml.member "FailedStackInstancesCount" xml)
               Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.failed_stack_instances_count
              (fun f ->
                 Query.Pair
                   ("FailedStackInstancesCount", (Integer.to_query f)));
           Util.option_map v.in_progress_stack_instances_count
             (fun f ->
                Query.Pair
                  ("InProgressStackInstancesCount", (Integer.to_query f)));
           Util.option_map v.in_sync_stack_instances_count
             (fun f ->
                Query.Pair
                  ("InSyncStackInstancesCount", (Integer.to_query f)));
           Util.option_map v.drifted_stack_instances_count
             (fun f ->
                Query.Pair
                  ("DriftedStackInstancesCount", (Integer.to_query f)));
           Util.option_map v.total_stack_instances_count
             (fun f ->
                Query.Pair ("TotalStackInstancesCount", (Integer.to_query f)));
           Util.option_map v.last_drift_check_timestamp
             (fun f ->
                Query.Pair ("LastDriftCheckTimestamp", (DateTime.to_query f)));
           Util.option_map v.drift_detection_status
             (fun f ->
                Query.Pair
                  ("DriftDetectionStatus",
                    (StackSetDriftDetectionStatus.to_query f)));
           Util.option_map v.drift_status
             (fun f ->
                Query.Pair ("DriftStatus", (StackSetDriftStatus.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.failed_stack_instances_count
              (fun f -> ("failed_stack_instances_count", (Integer.to_json f)));
           Util.option_map v.in_progress_stack_instances_count
             (fun f ->
                ("in_progress_stack_instances_count", (Integer.to_json f)));
           Util.option_map v.in_sync_stack_instances_count
             (fun f -> ("in_sync_stack_instances_count", (Integer.to_json f)));
           Util.option_map v.drifted_stack_instances_count
             (fun f -> ("drifted_stack_instances_count", (Integer.to_json f)));
           Util.option_map v.total_stack_instances_count
             (fun f -> ("total_stack_instances_count", (Integer.to_json f)));
           Util.option_map v.last_drift_check_timestamp
             (fun f -> ("last_drift_check_timestamp", (DateTime.to_json f)));
           Util.option_map v.drift_detection_status
             (fun f ->
                ("drift_detection_status",
                  (StackSetDriftDetectionStatus.to_json f)));
           Util.option_map v.drift_status
             (fun f -> ("drift_status", (StackSetDriftStatus.to_json f)))])
    let of_json j =
      {
        drift_status =
          (Util.option_map (Json.lookup j "drift_status")
             StackSetDriftStatus.of_json);
        drift_detection_status =
          (Util.option_map (Json.lookup j "drift_detection_status")
             StackSetDriftDetectionStatus.of_json);
        last_drift_check_timestamp =
          (Util.option_map (Json.lookup j "last_drift_check_timestamp")
             DateTime.of_json);
        total_stack_instances_count =
          (Util.option_map (Json.lookup j "total_stack_instances_count")
             Integer.of_json);
        drifted_stack_instances_count =
          (Util.option_map (Json.lookup j "drifted_stack_instances_count")
             Integer.of_json);
        in_sync_stack_instances_count =
          (Util.option_map (Json.lookup j "in_sync_stack_instances_count")
             Integer.of_json);
        in_progress_stack_instances_count =
          (Util.option_map
             (Json.lookup j "in_progress_stack_instances_count")
             Integer.of_json);
        failed_stack_instances_count =
          (Util.option_map (Json.lookup j "failed_stack_instances_count")
             Integer.of_json)
      }
  end
module StackSetOperationPreferences =
  struct
    type t =
      {
      region_order: RegionList.t ;
      failure_tolerance_count: Integer.t option ;
      failure_tolerance_percentage: Integer.t option ;
      max_concurrent_count: Integer.t option ;
      max_concurrent_percentage: Integer.t option }
    let make ?(region_order= [])  ?failure_tolerance_count 
      ?failure_tolerance_percentage  ?max_concurrent_count 
      ?max_concurrent_percentage  () =
      {
        region_order;
        failure_tolerance_count;
        failure_tolerance_percentage;
        max_concurrent_count;
        max_concurrent_percentage
      }
    let parse xml =
      Some
        {
          region_order =
            (Util.of_option []
               (Util.option_bind (Xml.member "RegionOrder" xml)
                  RegionList.parse));
          failure_tolerance_count =
            (Util.option_bind (Xml.member "FailureToleranceCount" xml)
               Integer.parse);
          failure_tolerance_percentage =
            (Util.option_bind (Xml.member "FailureTolerancePercentage" xml)
               Integer.parse);
          max_concurrent_count =
            (Util.option_bind (Xml.member "MaxConcurrentCount" xml)
               Integer.parse);
          max_concurrent_percentage =
            (Util.option_bind (Xml.member "MaxConcurrentPercentage" xml)
               Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.max_concurrent_percentage
              (fun f ->
                 Query.Pair ("MaxConcurrentPercentage", (Integer.to_query f)));
           Util.option_map v.max_concurrent_count
             (fun f ->
                Query.Pair ("MaxConcurrentCount", (Integer.to_query f)));
           Util.option_map v.failure_tolerance_percentage
             (fun f ->
                Query.Pair
                  ("FailureTolerancePercentage", (Integer.to_query f)));
           Util.option_map v.failure_tolerance_count
             (fun f ->
                Query.Pair ("FailureToleranceCount", (Integer.to_query f)));
           Some
             (Query.Pair
                ("RegionOrder.member", (RegionList.to_query v.region_order)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.max_concurrent_percentage
              (fun f -> ("max_concurrent_percentage", (Integer.to_json f)));
           Util.option_map v.max_concurrent_count
             (fun f -> ("max_concurrent_count", (Integer.to_json f)));
           Util.option_map v.failure_tolerance_percentage
             (fun f -> ("failure_tolerance_percentage", (Integer.to_json f)));
           Util.option_map v.failure_tolerance_count
             (fun f -> ("failure_tolerance_count", (Integer.to_json f)));
           Some ("region_order", (RegionList.to_json v.region_order))])
    let of_json j =
      {
        region_order =
          (RegionList.of_json
             (Util.of_option_exn (Json.lookup j "region_order")));
        failure_tolerance_count =
          (Util.option_map (Json.lookup j "failure_tolerance_count")
             Integer.of_json);
        failure_tolerance_percentage =
          (Util.option_map (Json.lookup j "failure_tolerance_percentage")
             Integer.of_json);
        max_concurrent_count =
          (Util.option_map (Json.lookup j "max_concurrent_count")
             Integer.of_json);
        max_concurrent_percentage =
          (Util.option_map (Json.lookup j "max_concurrent_percentage")
             Integer.of_json)
      }
  end
module StackEvent =
  struct
    type t =
      {
      stack_id: String.t ;
      event_id: String.t ;
      stack_name: String.t ;
      logical_resource_id: String.t option ;
      physical_resource_id: String.t option ;
      resource_type: String.t option ;
      timestamp: DateTime.t ;
      resource_status: ResourceStatus.t option ;
      resource_status_reason: String.t option ;
      resource_properties: String.t option ;
      client_request_token: String.t option }
    let make ~stack_id  ~event_id  ~stack_name  ?logical_resource_id 
      ?physical_resource_id  ?resource_type  ~timestamp  ?resource_status 
      ?resource_status_reason  ?resource_properties  ?client_request_token 
      () =
      {
        stack_id;
        event_id;
        stack_name;
        logical_resource_id;
        physical_resource_id;
        resource_type;
        timestamp;
        resource_status;
        resource_status_reason;
        resource_properties;
        client_request_token
      }
    let parse xml =
      Some
        {
          stack_id =
            (Xml.required "StackId"
               (Util.option_bind (Xml.member "StackId" xml) String.parse));
          event_id =
            (Xml.required "EventId"
               (Util.option_bind (Xml.member "EventId" xml) String.parse));
          stack_name =
            (Xml.required "StackName"
               (Util.option_bind (Xml.member "StackName" xml) String.parse));
          logical_resource_id =
            (Util.option_bind (Xml.member "LogicalResourceId" xml)
               String.parse);
          physical_resource_id =
            (Util.option_bind (Xml.member "PhysicalResourceId" xml)
               String.parse);
          resource_type =
            (Util.option_bind (Xml.member "ResourceType" xml) String.parse);
          timestamp =
            (Xml.required "Timestamp"
               (Util.option_bind (Xml.member "Timestamp" xml) DateTime.parse));
          resource_status =
            (Util.option_bind (Xml.member "ResourceStatus" xml)
               ResourceStatus.parse);
          resource_status_reason =
            (Util.option_bind (Xml.member "ResourceStatusReason" xml)
               String.parse);
          resource_properties =
            (Util.option_bind (Xml.member "ResourceProperties" xml)
               String.parse);
          client_request_token =
            (Util.option_bind (Xml.member "ClientRequestToken" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.client_request_token
              (fun f ->
                 Query.Pair ("ClientRequestToken", (String.to_query f)));
           Util.option_map v.resource_properties
             (fun f -> Query.Pair ("ResourceProperties", (String.to_query f)));
           Util.option_map v.resource_status_reason
             (fun f ->
                Query.Pair ("ResourceStatusReason", (String.to_query f)));
           Util.option_map v.resource_status
             (fun f ->
                Query.Pair ("ResourceStatus", (ResourceStatus.to_query f)));
           Some (Query.Pair ("Timestamp", (DateTime.to_query v.timestamp)));
           Util.option_map v.resource_type
             (fun f -> Query.Pair ("ResourceType", (String.to_query f)));
           Util.option_map v.physical_resource_id
             (fun f -> Query.Pair ("PhysicalResourceId", (String.to_query f)));
           Util.option_map v.logical_resource_id
             (fun f -> Query.Pair ("LogicalResourceId", (String.to_query f)));
           Some (Query.Pair ("StackName", (String.to_query v.stack_name)));
           Some (Query.Pair ("EventId", (String.to_query v.event_id)));
           Some (Query.Pair ("StackId", (String.to_query v.stack_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.client_request_token
              (fun f -> ("client_request_token", (String.to_json f)));
           Util.option_map v.resource_properties
             (fun f -> ("resource_properties", (String.to_json f)));
           Util.option_map v.resource_status_reason
             (fun f -> ("resource_status_reason", (String.to_json f)));
           Util.option_map v.resource_status
             (fun f -> ("resource_status", (ResourceStatus.to_json f)));
           Some ("timestamp", (DateTime.to_json v.timestamp));
           Util.option_map v.resource_type
             (fun f -> ("resource_type", (String.to_json f)));
           Util.option_map v.physical_resource_id
             (fun f -> ("physical_resource_id", (String.to_json f)));
           Util.option_map v.logical_resource_id
             (fun f -> ("logical_resource_id", (String.to_json f)));
           Some ("stack_name", (String.to_json v.stack_name));
           Some ("event_id", (String.to_json v.event_id));
           Some ("stack_id", (String.to_json v.stack_id))])
    let of_json j =
      {
        stack_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "stack_id")));
        event_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "event_id")));
        stack_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "stack_name")));
        logical_resource_id =
          (Util.option_map (Json.lookup j "logical_resource_id")
             String.of_json);
        physical_resource_id =
          (Util.option_map (Json.lookup j "physical_resource_id")
             String.of_json);
        resource_type =
          (Util.option_map (Json.lookup j "resource_type") String.of_json);
        timestamp =
          (DateTime.of_json (Util.of_option_exn (Json.lookup j "timestamp")));
        resource_status =
          (Util.option_map (Json.lookup j "resource_status")
             ResourceStatus.of_json);
        resource_status_reason =
          (Util.option_map (Json.lookup j "resource_status_reason")
             String.of_json);
        resource_properties =
          (Util.option_map (Json.lookup j "resource_properties")
             String.of_json);
        client_request_token =
          (Util.option_map (Json.lookup j "client_request_token")
             String.of_json)
      }
  end
module TypeVersionSummary =
  struct
    type t =
      {
      type_: RegistryType.t option ;
      type_name: String.t option ;
      version_id: String.t option ;
      is_default_version: Boolean.t option ;
      arn: String.t option ;
      time_created: DateTime.t option ;
      description: String.t option }
    let make ?type_  ?type_name  ?version_id  ?is_default_version  ?arn 
      ?time_created  ?description  () =
      {
        type_;
        type_name;
        version_id;
        is_default_version;
        arn;
        time_created;
        description
      }
    let parse xml =
      Some
        {
          type_ =
            (Util.option_bind (Xml.member "Type" xml) RegistryType.parse);
          type_name =
            (Util.option_bind (Xml.member "TypeName" xml) String.parse);
          version_id =
            (Util.option_bind (Xml.member "VersionId" xml) String.parse);
          is_default_version =
            (Util.option_bind (Xml.member "IsDefaultVersion" xml)
               Boolean.parse);
          arn = (Util.option_bind (Xml.member "Arn" xml) String.parse);
          time_created =
            (Util.option_bind (Xml.member "TimeCreated" xml) DateTime.parse);
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.description
              (fun f -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.time_created
             (fun f -> Query.Pair ("TimeCreated", (DateTime.to_query f)));
           Util.option_map v.arn
             (fun f -> Query.Pair ("Arn", (String.to_query f)));
           Util.option_map v.is_default_version
             (fun f -> Query.Pair ("IsDefaultVersion", (Boolean.to_query f)));
           Util.option_map v.version_id
             (fun f -> Query.Pair ("VersionId", (String.to_query f)));
           Util.option_map v.type_name
             (fun f -> Query.Pair ("TypeName", (String.to_query f)));
           Util.option_map v.type_
             (fun f -> Query.Pair ("Type", (RegistryType.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.description
              (fun f -> ("description", (String.to_json f)));
           Util.option_map v.time_created
             (fun f -> ("time_created", (DateTime.to_json f)));
           Util.option_map v.arn (fun f -> ("arn", (String.to_json f)));
           Util.option_map v.is_default_version
             (fun f -> ("is_default_version", (Boolean.to_json f)));
           Util.option_map v.version_id
             (fun f -> ("version_id", (String.to_json f)));
           Util.option_map v.type_name
             (fun f -> ("type_name", (String.to_json f)));
           Util.option_map v.type_
             (fun f -> ("type_", (RegistryType.to_json f)))])
    let of_json j =
      {
        type_ =
          (Util.option_map (Json.lookup j "type_") RegistryType.of_json);
        type_name =
          (Util.option_map (Json.lookup j "type_name") String.of_json);
        version_id =
          (Util.option_map (Json.lookup j "version_id") String.of_json);
        is_default_version =
          (Util.option_map (Json.lookup j "is_default_version")
             Boolean.of_json);
        arn = (Util.option_map (Json.lookup j "arn") String.of_json);
        time_created =
          (Util.option_map (Json.lookup j "time_created") DateTime.of_json);
        description =
          (Util.option_map (Json.lookup j "description") String.of_json)
      }
  end
module StackSetOperationSummary =
  struct
    type t =
      {
      operation_id: String.t option ;
      action: StackSetOperationAction.t option ;
      status: StackSetOperationStatus.t option ;
      creation_timestamp: DateTime.t option ;
      end_timestamp: DateTime.t option }
    let make ?operation_id  ?action  ?status  ?creation_timestamp 
      ?end_timestamp  () =
      { operation_id; action; status; creation_timestamp; end_timestamp }
    let parse xml =
      Some
        {
          operation_id =
            (Util.option_bind (Xml.member "OperationId" xml) String.parse);
          action =
            (Util.option_bind (Xml.member "Action" xml)
               StackSetOperationAction.parse);
          status =
            (Util.option_bind (Xml.member "Status" xml)
               StackSetOperationStatus.parse);
          creation_timestamp =
            (Util.option_bind (Xml.member "CreationTimestamp" xml)
               DateTime.parse);
          end_timestamp =
            (Util.option_bind (Xml.member "EndTimestamp" xml) DateTime.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.end_timestamp
              (fun f -> Query.Pair ("EndTimestamp", (DateTime.to_query f)));
           Util.option_map v.creation_timestamp
             (fun f ->
                Query.Pair ("CreationTimestamp", (DateTime.to_query f)));
           Util.option_map v.status
             (fun f ->
                Query.Pair ("Status", (StackSetOperationStatus.to_query f)));
           Util.option_map v.action
             (fun f ->
                Query.Pair ("Action", (StackSetOperationAction.to_query f)));
           Util.option_map v.operation_id
             (fun f -> Query.Pair ("OperationId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.end_timestamp
              (fun f -> ("end_timestamp", (DateTime.to_json f)));
           Util.option_map v.creation_timestamp
             (fun f -> ("creation_timestamp", (DateTime.to_json f)));
           Util.option_map v.status
             (fun f -> ("status", (StackSetOperationStatus.to_json f)));
           Util.option_map v.action
             (fun f -> ("action", (StackSetOperationAction.to_json f)));
           Util.option_map v.operation_id
             (fun f -> ("operation_id", (String.to_json f)))])
    let of_json j =
      {
        operation_id =
          (Util.option_map (Json.lookup j "operation_id") String.of_json);
        action =
          (Util.option_map (Json.lookup j "action")
             StackSetOperationAction.of_json);
        status =
          (Util.option_map (Json.lookup j "status")
             StackSetOperationStatus.of_json);
        creation_timestamp =
          (Util.option_map (Json.lookup j "creation_timestamp")
             DateTime.of_json);
        end_timestamp =
          (Util.option_map (Json.lookup j "end_timestamp") DateTime.of_json)
      }
  end
module StackResourceSummary =
  struct
    type t =
      {
      logical_resource_id: String.t ;
      physical_resource_id: String.t option ;
      resource_type: String.t ;
      last_updated_timestamp: DateTime.t ;
      resource_status: ResourceStatus.t ;
      resource_status_reason: String.t option ;
      drift_information: StackResourceDriftInformationSummary.t option }
    let make ~logical_resource_id  ?physical_resource_id  ~resource_type 
      ~last_updated_timestamp  ~resource_status  ?resource_status_reason 
      ?drift_information  () =
      {
        logical_resource_id;
        physical_resource_id;
        resource_type;
        last_updated_timestamp;
        resource_status;
        resource_status_reason;
        drift_information
      }
    let parse xml =
      Some
        {
          logical_resource_id =
            (Xml.required "LogicalResourceId"
               (Util.option_bind (Xml.member "LogicalResourceId" xml)
                  String.parse));
          physical_resource_id =
            (Util.option_bind (Xml.member "PhysicalResourceId" xml)
               String.parse);
          resource_type =
            (Xml.required "ResourceType"
               (Util.option_bind (Xml.member "ResourceType" xml) String.parse));
          last_updated_timestamp =
            (Xml.required "LastUpdatedTimestamp"
               (Util.option_bind (Xml.member "LastUpdatedTimestamp" xml)
                  DateTime.parse));
          resource_status =
            (Xml.required "ResourceStatus"
               (Util.option_bind (Xml.member "ResourceStatus" xml)
                  ResourceStatus.parse));
          resource_status_reason =
            (Util.option_bind (Xml.member "ResourceStatusReason" xml)
               String.parse);
          drift_information =
            (Util.option_bind (Xml.member "DriftInformation" xml)
               StackResourceDriftInformationSummary.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.drift_information
              (fun f ->
                 Query.Pair
                   ("DriftInformation",
                     (StackResourceDriftInformationSummary.to_query f)));
           Util.option_map v.resource_status_reason
             (fun f ->
                Query.Pair ("ResourceStatusReason", (String.to_query f)));
           Some
             (Query.Pair
                ("ResourceStatus",
                  (ResourceStatus.to_query v.resource_status)));
           Some
             (Query.Pair
                ("LastUpdatedTimestamp",
                  (DateTime.to_query v.last_updated_timestamp)));
           Some
             (Query.Pair ("ResourceType", (String.to_query v.resource_type)));
           Util.option_map v.physical_resource_id
             (fun f -> Query.Pair ("PhysicalResourceId", (String.to_query f)));
           Some
             (Query.Pair
                ("LogicalResourceId",
                  (String.to_query v.logical_resource_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.drift_information
              (fun f ->
                 ("drift_information",
                   (StackResourceDriftInformationSummary.to_json f)));
           Util.option_map v.resource_status_reason
             (fun f -> ("resource_status_reason", (String.to_json f)));
           Some
             ("resource_status", (ResourceStatus.to_json v.resource_status));
           Some
             ("last_updated_timestamp",
               (DateTime.to_json v.last_updated_timestamp));
           Some ("resource_type", (String.to_json v.resource_type));
           Util.option_map v.physical_resource_id
             (fun f -> ("physical_resource_id", (String.to_json f)));
           Some
             ("logical_resource_id", (String.to_json v.logical_resource_id))])
    let of_json j =
      {
        logical_resource_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "logical_resource_id")));
        physical_resource_id =
          (Util.option_map (Json.lookup j "physical_resource_id")
             String.of_json);
        resource_type =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "resource_type")));
        last_updated_timestamp =
          (DateTime.of_json
             (Util.of_option_exn (Json.lookup j "last_updated_timestamp")));
        resource_status =
          (ResourceStatus.of_json
             (Util.of_option_exn (Json.lookup j "resource_status")));
        resource_status_reason =
          (Util.option_map (Json.lookup j "resource_status_reason")
             String.of_json);
        drift_information =
          (Util.option_map (Json.lookup j "drift_information")
             StackResourceDriftInformationSummary.of_json)
      }
  end
module StackInstanceFilter =
  struct
    type t =
      {
      name: StackInstanceFilterName.t option ;
      values: String.t option }
    let make ?name  ?values  () = { name; values }
    let parse xml =
      Some
        {
          name =
            (Util.option_bind (Xml.member "Name" xml)
               StackInstanceFilterName.parse);
          values = (Util.option_bind (Xml.member "Values" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.values
              (fun f -> Query.Pair ("Values", (String.to_query f)));
           Util.option_map v.name
             (fun f ->
                Query.Pair ("Name", (StackInstanceFilterName.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.values
              (fun f -> ("values", (String.to_json f)));
           Util.option_map v.name
             (fun f -> ("name", (StackInstanceFilterName.to_json f)))])
    let of_json j =
      {
        name =
          (Util.option_map (Json.lookup j "name")
             StackInstanceFilterName.of_json);
        values = (Util.option_map (Json.lookup j "values") String.of_json)
      }
  end
module ParameterDeclaration =
  struct
    type t =
      {
      parameter_key: String.t option ;
      default_value: String.t option ;
      parameter_type: String.t option ;
      no_echo: Boolean.t option ;
      description: String.t option ;
      parameter_constraints: ParameterConstraints.t option }
    let make ?parameter_key  ?default_value  ?parameter_type  ?no_echo 
      ?description  ?parameter_constraints  () =
      {
        parameter_key;
        default_value;
        parameter_type;
        no_echo;
        description;
        parameter_constraints
      }
    let parse xml =
      Some
        {
          parameter_key =
            (Util.option_bind (Xml.member "ParameterKey" xml) String.parse);
          default_value =
            (Util.option_bind (Xml.member "DefaultValue" xml) String.parse);
          parameter_type =
            (Util.option_bind (Xml.member "ParameterType" xml) String.parse);
          no_echo =
            (Util.option_bind (Xml.member "NoEcho" xml) Boolean.parse);
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse);
          parameter_constraints =
            (Util.option_bind (Xml.member "ParameterConstraints" xml)
               ParameterConstraints.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.parameter_constraints
              (fun f ->
                 Query.Pair
                   ("ParameterConstraints",
                     (ParameterConstraints.to_query f)));
           Util.option_map v.description
             (fun f -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.no_echo
             (fun f -> Query.Pair ("NoEcho", (Boolean.to_query f)));
           Util.option_map v.parameter_type
             (fun f -> Query.Pair ("ParameterType", (String.to_query f)));
           Util.option_map v.default_value
             (fun f -> Query.Pair ("DefaultValue", (String.to_query f)));
           Util.option_map v.parameter_key
             (fun f -> Query.Pair ("ParameterKey", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.parameter_constraints
              (fun f ->
                 ("parameter_constraints", (ParameterConstraints.to_json f)));
           Util.option_map v.description
             (fun f -> ("description", (String.to_json f)));
           Util.option_map v.no_echo
             (fun f -> ("no_echo", (Boolean.to_json f)));
           Util.option_map v.parameter_type
             (fun f -> ("parameter_type", (String.to_json f)));
           Util.option_map v.default_value
             (fun f -> ("default_value", (String.to_json f)));
           Util.option_map v.parameter_key
             (fun f -> ("parameter_key", (String.to_json f)))])
    let of_json j =
      {
        parameter_key =
          (Util.option_map (Json.lookup j "parameter_key") String.of_json);
        default_value =
          (Util.option_map (Json.lookup j "default_value") String.of_json);
        parameter_type =
          (Util.option_map (Json.lookup j "parameter_type") String.of_json);
        no_echo = (Util.option_map (Json.lookup j "no_echo") Boolean.of_json);
        description =
          (Util.option_map (Json.lookup j "description") String.of_json);
        parameter_constraints =
          (Util.option_map (Json.lookup j "parameter_constraints")
             ParameterConstraints.of_json)
      }
  end
module ResourceIdentifierSummary =
  struct
    type t =
      {
      resource_type: String.t option ;
      logical_resource_ids: LogicalResourceIds.t ;
      resource_identifiers: ResourceIdentifiers.t }
    let make ?resource_type  ?(logical_resource_ids= []) 
      ?(resource_identifiers= [])  () =
      { resource_type; logical_resource_ids; resource_identifiers }
    let parse xml =
      Some
        {
          resource_type =
            (Util.option_bind (Xml.member "ResourceType" xml) String.parse);
          logical_resource_ids =
            (Util.of_option []
               (Util.option_bind (Xml.member "LogicalResourceIds" xml)
                  LogicalResourceIds.parse));
          resource_identifiers =
            (Util.of_option []
               (Util.option_bind (Xml.member "ResourceIdentifiers" xml)
                  ResourceIdentifiers.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ResourceIdentifiers.member",
                   (ResourceIdentifiers.to_query v.resource_identifiers)));
           Some
             (Query.Pair
                ("LogicalResourceIds.member",
                  (LogicalResourceIds.to_query v.logical_resource_ids)));
           Util.option_map v.resource_type
             (fun f -> Query.Pair ("ResourceType", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("resource_identifiers",
                (ResourceIdentifiers.to_json v.resource_identifiers));
           Some
             ("logical_resource_ids",
               (LogicalResourceIds.to_json v.logical_resource_ids));
           Util.option_map v.resource_type
             (fun f -> ("resource_type", (String.to_json f)))])
    let of_json j =
      {
        resource_type =
          (Util.option_map (Json.lookup j "resource_type") String.of_json);
        logical_resource_ids =
          (LogicalResourceIds.of_json
             (Util.of_option_exn (Json.lookup j "logical_resource_ids")));
        resource_identifiers =
          (ResourceIdentifiers.of_json
             (Util.of_option_exn (Json.lookup j "resource_identifiers")))
      }
  end
module StackInstanceSummary =
  struct
    type t =
      {
      stack_set_id: String.t option ;
      region: String.t option ;
      account: String.t option ;
      stack_id: String.t option ;
      status: StackInstanceStatus.t option ;
      status_reason: String.t option ;
      stack_instance_status: StackInstanceComprehensiveStatus.t option ;
      organizational_unit_id: String.t option ;
      drift_status: StackDriftStatus.t option ;
      last_drift_check_timestamp: DateTime.t option }
    let make ?stack_set_id  ?region  ?account  ?stack_id  ?status 
      ?status_reason  ?stack_instance_status  ?organizational_unit_id 
      ?drift_status  ?last_drift_check_timestamp  () =
      {
        stack_set_id;
        region;
        account;
        stack_id;
        status;
        status_reason;
        stack_instance_status;
        organizational_unit_id;
        drift_status;
        last_drift_check_timestamp
      }
    let parse xml =
      Some
        {
          stack_set_id =
            (Util.option_bind (Xml.member "StackSetId" xml) String.parse);
          region = (Util.option_bind (Xml.member "Region" xml) String.parse);
          account =
            (Util.option_bind (Xml.member "Account" xml) String.parse);
          stack_id =
            (Util.option_bind (Xml.member "StackId" xml) String.parse);
          status =
            (Util.option_bind (Xml.member "Status" xml)
               StackInstanceStatus.parse);
          status_reason =
            (Util.option_bind (Xml.member "StatusReason" xml) String.parse);
          stack_instance_status =
            (Util.option_bind (Xml.member "StackInstanceStatus" xml)
               StackInstanceComprehensiveStatus.parse);
          organizational_unit_id =
            (Util.option_bind (Xml.member "OrganizationalUnitId" xml)
               String.parse);
          drift_status =
            (Util.option_bind (Xml.member "DriftStatus" xml)
               StackDriftStatus.parse);
          last_drift_check_timestamp =
            (Util.option_bind (Xml.member "LastDriftCheckTimestamp" xml)
               DateTime.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.last_drift_check_timestamp
              (fun f ->
                 Query.Pair
                   ("LastDriftCheckTimestamp", (DateTime.to_query f)));
           Util.option_map v.drift_status
             (fun f ->
                Query.Pair ("DriftStatus", (StackDriftStatus.to_query f)));
           Util.option_map v.organizational_unit_id
             (fun f ->
                Query.Pair ("OrganizationalUnitId", (String.to_query f)));
           Util.option_map v.stack_instance_status
             (fun f ->
                Query.Pair
                  ("StackInstanceStatus",
                    (StackInstanceComprehensiveStatus.to_query f)));
           Util.option_map v.status_reason
             (fun f -> Query.Pair ("StatusReason", (String.to_query f)));
           Util.option_map v.status
             (fun f ->
                Query.Pair ("Status", (StackInstanceStatus.to_query f)));
           Util.option_map v.stack_id
             (fun f -> Query.Pair ("StackId", (String.to_query f)));
           Util.option_map v.account
             (fun f -> Query.Pair ("Account", (String.to_query f)));
           Util.option_map v.region
             (fun f -> Query.Pair ("Region", (String.to_query f)));
           Util.option_map v.stack_set_id
             (fun f -> Query.Pair ("StackSetId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.last_drift_check_timestamp
              (fun f -> ("last_drift_check_timestamp", (DateTime.to_json f)));
           Util.option_map v.drift_status
             (fun f -> ("drift_status", (StackDriftStatus.to_json f)));
           Util.option_map v.organizational_unit_id
             (fun f -> ("organizational_unit_id", (String.to_json f)));
           Util.option_map v.stack_instance_status
             (fun f ->
                ("stack_instance_status",
                  (StackInstanceComprehensiveStatus.to_json f)));
           Util.option_map v.status_reason
             (fun f -> ("status_reason", (String.to_json f)));
           Util.option_map v.status
             (fun f -> ("status", (StackInstanceStatus.to_json f)));
           Util.option_map v.stack_id
             (fun f -> ("stack_id", (String.to_json f)));
           Util.option_map v.account
             (fun f -> ("account", (String.to_json f)));
           Util.option_map v.region (fun f -> ("region", (String.to_json f)));
           Util.option_map v.stack_set_id
             (fun f -> ("stack_set_id", (String.to_json f)))])
    let of_json j =
      {
        stack_set_id =
          (Util.option_map (Json.lookup j "stack_set_id") String.of_json);
        region = (Util.option_map (Json.lookup j "region") String.of_json);
        account = (Util.option_map (Json.lookup j "account") String.of_json);
        stack_id =
          (Util.option_map (Json.lookup j "stack_id") String.of_json);
        status =
          (Util.option_map (Json.lookup j "status")
             StackInstanceStatus.of_json);
        status_reason =
          (Util.option_map (Json.lookup j "status_reason") String.of_json);
        stack_instance_status =
          (Util.option_map (Json.lookup j "stack_instance_status")
             StackInstanceComprehensiveStatus.of_json);
        organizational_unit_id =
          (Util.option_map (Json.lookup j "organizational_unit_id")
             String.of_json);
        drift_status =
          (Util.option_map (Json.lookup j "drift_status")
             StackDriftStatus.of_json);
        last_drift_check_timestamp =
          (Util.option_map (Json.lookup j "last_drift_check_timestamp")
             DateTime.of_json)
      }
  end
module Stack =
  struct
    type t =
      {
      stack_id: String.t option ;
      stack_name: String.t ;
      change_set_id: String.t option ;
      description: String.t option ;
      parameters: Parameters.t ;
      creation_time: DateTime.t ;
      deletion_time: DateTime.t option ;
      last_updated_time: DateTime.t option ;
      rollback_configuration: RollbackConfiguration.t option ;
      stack_status: StackStatus.t ;
      stack_status_reason: String.t option ;
      disable_rollback: Boolean.t option ;
      notification_a_r_ns: NotificationARNs.t ;
      timeout_in_minutes: Integer.t option ;
      capabilities: Capabilities.t ;
      outputs: Outputs.t ;
      role_a_r_n: String.t option ;
      tags: Tags.t ;
      enable_termination_protection: Boolean.t option ;
      parent_id: String.t option ;
      root_id: String.t option ;
      drift_information: StackDriftInformation.t option }
    let make ?stack_id  ~stack_name  ?change_set_id  ?description 
      ?(parameters= [])  ~creation_time  ?deletion_time  ?last_updated_time 
      ?rollback_configuration  ~stack_status  ?stack_status_reason 
      ?disable_rollback  ?(notification_a_r_ns= [])  ?timeout_in_minutes 
      ?(capabilities= [])  ?(outputs= [])  ?role_a_r_n  ?(tags= []) 
      ?enable_termination_protection  ?parent_id  ?root_id 
      ?drift_information  () =
      {
        stack_id;
        stack_name;
        change_set_id;
        description;
        parameters;
        creation_time;
        deletion_time;
        last_updated_time;
        rollback_configuration;
        stack_status;
        stack_status_reason;
        disable_rollback;
        notification_a_r_ns;
        timeout_in_minutes;
        capabilities;
        outputs;
        role_a_r_n;
        tags;
        enable_termination_protection;
        parent_id;
        root_id;
        drift_information
      }
    let parse xml =
      Some
        {
          stack_id =
            (Util.option_bind (Xml.member "StackId" xml) String.parse);
          stack_name =
            (Xml.required "StackName"
               (Util.option_bind (Xml.member "StackName" xml) String.parse));
          change_set_id =
            (Util.option_bind (Xml.member "ChangeSetId" xml) String.parse);
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse);
          parameters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Parameters" xml)
                  Parameters.parse));
          creation_time =
            (Xml.required "CreationTime"
               (Util.option_bind (Xml.member "CreationTime" xml)
                  DateTime.parse));
          deletion_time =
            (Util.option_bind (Xml.member "DeletionTime" xml) DateTime.parse);
          last_updated_time =
            (Util.option_bind (Xml.member "LastUpdatedTime" xml)
               DateTime.parse);
          rollback_configuration =
            (Util.option_bind (Xml.member "RollbackConfiguration" xml)
               RollbackConfiguration.parse);
          stack_status =
            (Xml.required "StackStatus"
               (Util.option_bind (Xml.member "StackStatus" xml)
                  StackStatus.parse));
          stack_status_reason =
            (Util.option_bind (Xml.member "StackStatusReason" xml)
               String.parse);
          disable_rollback =
            (Util.option_bind (Xml.member "DisableRollback" xml)
               Boolean.parse);
          notification_a_r_ns =
            (Util.of_option []
               (Util.option_bind (Xml.member "NotificationARNs" xml)
                  NotificationARNs.parse));
          timeout_in_minutes =
            (Util.option_bind (Xml.member "TimeoutInMinutes" xml)
               Integer.parse);
          capabilities =
            (Util.of_option []
               (Util.option_bind (Xml.member "Capabilities" xml)
                  Capabilities.parse));
          outputs =
            (Util.of_option []
               (Util.option_bind (Xml.member "Outputs" xml) Outputs.parse));
          role_a_r_n =
            (Util.option_bind (Xml.member "RoleARN" xml) String.parse);
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) Tags.parse));
          enable_termination_protection =
            (Util.option_bind (Xml.member "EnableTerminationProtection" xml)
               Boolean.parse);
          parent_id =
            (Util.option_bind (Xml.member "ParentId" xml) String.parse);
          root_id = (Util.option_bind (Xml.member "RootId" xml) String.parse);
          drift_information =
            (Util.option_bind (Xml.member "DriftInformation" xml)
               StackDriftInformation.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.drift_information
              (fun f ->
                 Query.Pair
                   ("DriftInformation", (StackDriftInformation.to_query f)));
           Util.option_map v.root_id
             (fun f -> Query.Pair ("RootId", (String.to_query f)));
           Util.option_map v.parent_id
             (fun f -> Query.Pair ("ParentId", (String.to_query f)));
           Util.option_map v.enable_termination_protection
             (fun f ->
                Query.Pair
                  ("EnableTerminationProtection", (Boolean.to_query f)));
           Some (Query.Pair ("Tags.member", (Tags.to_query v.tags)));
           Util.option_map v.role_a_r_n
             (fun f -> Query.Pair ("RoleARN", (String.to_query f)));
           Some (Query.Pair ("Outputs.member", (Outputs.to_query v.outputs)));
           Some
             (Query.Pair
                ("Capabilities.member",
                  (Capabilities.to_query v.capabilities)));
           Util.option_map v.timeout_in_minutes
             (fun f -> Query.Pair ("TimeoutInMinutes", (Integer.to_query f)));
           Some
             (Query.Pair
                ("NotificationARNs.member",
                  (NotificationARNs.to_query v.notification_a_r_ns)));
           Util.option_map v.disable_rollback
             (fun f -> Query.Pair ("DisableRollback", (Boolean.to_query f)));
           Util.option_map v.stack_status_reason
             (fun f -> Query.Pair ("StackStatusReason", (String.to_query f)));
           Some
             (Query.Pair
                ("StackStatus", (StackStatus.to_query v.stack_status)));
           Util.option_map v.rollback_configuration
             (fun f ->
                Query.Pair
                  ("RollbackConfiguration",
                    (RollbackConfiguration.to_query f)));
           Util.option_map v.last_updated_time
             (fun f -> Query.Pair ("LastUpdatedTime", (DateTime.to_query f)));
           Util.option_map v.deletion_time
             (fun f -> Query.Pair ("DeletionTime", (DateTime.to_query f)));
           Some
             (Query.Pair
                ("CreationTime", (DateTime.to_query v.creation_time)));
           Some
             (Query.Pair
                ("Parameters.member", (Parameters.to_query v.parameters)));
           Util.option_map v.description
             (fun f -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.change_set_id
             (fun f -> Query.Pair ("ChangeSetId", (String.to_query f)));
           Some (Query.Pair ("StackName", (String.to_query v.stack_name)));
           Util.option_map v.stack_id
             (fun f -> Query.Pair ("StackId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.drift_information
              (fun f ->
                 ("drift_information", (StackDriftInformation.to_json f)));
           Util.option_map v.root_id
             (fun f -> ("root_id", (String.to_json f)));
           Util.option_map v.parent_id
             (fun f -> ("parent_id", (String.to_json f)));
           Util.option_map v.enable_termination_protection
             (fun f -> ("enable_termination_protection", (Boolean.to_json f)));
           Some ("tags", (Tags.to_json v.tags));
           Util.option_map v.role_a_r_n
             (fun f -> ("role_a_r_n", (String.to_json f)));
           Some ("outputs", (Outputs.to_json v.outputs));
           Some ("capabilities", (Capabilities.to_json v.capabilities));
           Util.option_map v.timeout_in_minutes
             (fun f -> ("timeout_in_minutes", (Integer.to_json f)));
           Some
             ("notification_a_r_ns",
               (NotificationARNs.to_json v.notification_a_r_ns));
           Util.option_map v.disable_rollback
             (fun f -> ("disable_rollback", (Boolean.to_json f)));
           Util.option_map v.stack_status_reason
             (fun f -> ("stack_status_reason", (String.to_json f)));
           Some ("stack_status", (StackStatus.to_json v.stack_status));
           Util.option_map v.rollback_configuration
             (fun f ->
                ("rollback_configuration", (RollbackConfiguration.to_json f)));
           Util.option_map v.last_updated_time
             (fun f -> ("last_updated_time", (DateTime.to_json f)));
           Util.option_map v.deletion_time
             (fun f -> ("deletion_time", (DateTime.to_json f)));
           Some ("creation_time", (DateTime.to_json v.creation_time));
           Some ("parameters", (Parameters.to_json v.parameters));
           Util.option_map v.description
             (fun f -> ("description", (String.to_json f)));
           Util.option_map v.change_set_id
             (fun f -> ("change_set_id", (String.to_json f)));
           Some ("stack_name", (String.to_json v.stack_name));
           Util.option_map v.stack_id
             (fun f -> ("stack_id", (String.to_json f)))])
    let of_json j =
      {
        stack_id =
          (Util.option_map (Json.lookup j "stack_id") String.of_json);
        stack_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "stack_name")));
        change_set_id =
          (Util.option_map (Json.lookup j "change_set_id") String.of_json);
        description =
          (Util.option_map (Json.lookup j "description") String.of_json);
        parameters =
          (Parameters.of_json
             (Util.of_option_exn (Json.lookup j "parameters")));
        creation_time =
          (DateTime.of_json
             (Util.of_option_exn (Json.lookup j "creation_time")));
        deletion_time =
          (Util.option_map (Json.lookup j "deletion_time") DateTime.of_json);
        last_updated_time =
          (Util.option_map (Json.lookup j "last_updated_time")
             DateTime.of_json);
        rollback_configuration =
          (Util.option_map (Json.lookup j "rollback_configuration")
             RollbackConfiguration.of_json);
        stack_status =
          (StackStatus.of_json
             (Util.of_option_exn (Json.lookup j "stack_status")));
        stack_status_reason =
          (Util.option_map (Json.lookup j "stack_status_reason")
             String.of_json);
        disable_rollback =
          (Util.option_map (Json.lookup j "disable_rollback") Boolean.of_json);
        notification_a_r_ns =
          (NotificationARNs.of_json
             (Util.of_option_exn (Json.lookup j "notification_a_r_ns")));
        timeout_in_minutes =
          (Util.option_map (Json.lookup j "timeout_in_minutes")
             Integer.of_json);
        capabilities =
          (Capabilities.of_json
             (Util.of_option_exn (Json.lookup j "capabilities")));
        outputs =
          (Outputs.of_json (Util.of_option_exn (Json.lookup j "outputs")));
        role_a_r_n =
          (Util.option_map (Json.lookup j "role_a_r_n") String.of_json);
        tags = (Tags.of_json (Util.of_option_exn (Json.lookup j "tags")));
        enable_termination_protection =
          (Util.option_map (Json.lookup j "enable_termination_protection")
             Boolean.of_json);
        parent_id =
          (Util.option_map (Json.lookup j "parent_id") String.of_json);
        root_id = (Util.option_map (Json.lookup j "root_id") String.of_json);
        drift_information =
          (Util.option_map (Json.lookup j "drift_information")
             StackDriftInformation.of_json)
      }
  end
module StackResource =
  struct
    type t =
      {
      stack_name: String.t option ;
      stack_id: String.t option ;
      logical_resource_id: String.t ;
      physical_resource_id: String.t option ;
      resource_type: String.t ;
      timestamp: DateTime.t ;
      resource_status: ResourceStatus.t ;
      resource_status_reason: String.t option ;
      description: String.t option ;
      drift_information: StackResourceDriftInformation.t option }
    let make ?stack_name  ?stack_id  ~logical_resource_id 
      ?physical_resource_id  ~resource_type  ~timestamp  ~resource_status 
      ?resource_status_reason  ?description  ?drift_information  () =
      {
        stack_name;
        stack_id;
        logical_resource_id;
        physical_resource_id;
        resource_type;
        timestamp;
        resource_status;
        resource_status_reason;
        description;
        drift_information
      }
    let parse xml =
      Some
        {
          stack_name =
            (Util.option_bind (Xml.member "StackName" xml) String.parse);
          stack_id =
            (Util.option_bind (Xml.member "StackId" xml) String.parse);
          logical_resource_id =
            (Xml.required "LogicalResourceId"
               (Util.option_bind (Xml.member "LogicalResourceId" xml)
                  String.parse));
          physical_resource_id =
            (Util.option_bind (Xml.member "PhysicalResourceId" xml)
               String.parse);
          resource_type =
            (Xml.required "ResourceType"
               (Util.option_bind (Xml.member "ResourceType" xml) String.parse));
          timestamp =
            (Xml.required "Timestamp"
               (Util.option_bind (Xml.member "Timestamp" xml) DateTime.parse));
          resource_status =
            (Xml.required "ResourceStatus"
               (Util.option_bind (Xml.member "ResourceStatus" xml)
                  ResourceStatus.parse));
          resource_status_reason =
            (Util.option_bind (Xml.member "ResourceStatusReason" xml)
               String.parse);
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse);
          drift_information =
            (Util.option_bind (Xml.member "DriftInformation" xml)
               StackResourceDriftInformation.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.drift_information
              (fun f ->
                 Query.Pair
                   ("DriftInformation",
                     (StackResourceDriftInformation.to_query f)));
           Util.option_map v.description
             (fun f -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.resource_status_reason
             (fun f ->
                Query.Pair ("ResourceStatusReason", (String.to_query f)));
           Some
             (Query.Pair
                ("ResourceStatus",
                  (ResourceStatus.to_query v.resource_status)));
           Some (Query.Pair ("Timestamp", (DateTime.to_query v.timestamp)));
           Some
             (Query.Pair ("ResourceType", (String.to_query v.resource_type)));
           Util.option_map v.physical_resource_id
             (fun f -> Query.Pair ("PhysicalResourceId", (String.to_query f)));
           Some
             (Query.Pair
                ("LogicalResourceId",
                  (String.to_query v.logical_resource_id)));
           Util.option_map v.stack_id
             (fun f -> Query.Pair ("StackId", (String.to_query f)));
           Util.option_map v.stack_name
             (fun f -> Query.Pair ("StackName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.drift_information
              (fun f ->
                 ("drift_information",
                   (StackResourceDriftInformation.to_json f)));
           Util.option_map v.description
             (fun f -> ("description", (String.to_json f)));
           Util.option_map v.resource_status_reason
             (fun f -> ("resource_status_reason", (String.to_json f)));
           Some
             ("resource_status", (ResourceStatus.to_json v.resource_status));
           Some ("timestamp", (DateTime.to_json v.timestamp));
           Some ("resource_type", (String.to_json v.resource_type));
           Util.option_map v.physical_resource_id
             (fun f -> ("physical_resource_id", (String.to_json f)));
           Some
             ("logical_resource_id", (String.to_json v.logical_resource_id));
           Util.option_map v.stack_id
             (fun f -> ("stack_id", (String.to_json f)));
           Util.option_map v.stack_name
             (fun f -> ("stack_name", (String.to_json f)))])
    let of_json j =
      {
        stack_name =
          (Util.option_map (Json.lookup j "stack_name") String.of_json);
        stack_id =
          (Util.option_map (Json.lookup j "stack_id") String.of_json);
        logical_resource_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "logical_resource_id")));
        physical_resource_id =
          (Util.option_map (Json.lookup j "physical_resource_id")
             String.of_json);
        resource_type =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "resource_type")));
        timestamp =
          (DateTime.of_json (Util.of_option_exn (Json.lookup j "timestamp")));
        resource_status =
          (ResourceStatus.of_json
             (Util.of_option_exn (Json.lookup j "resource_status")));
        resource_status_reason =
          (Util.option_map (Json.lookup j "resource_status_reason")
             String.of_json);
        description =
          (Util.option_map (Json.lookup j "description") String.of_json);
        drift_information =
          (Util.option_map (Json.lookup j "drift_information")
             StackResourceDriftInformation.of_json)
      }
  end
module TemplateStage =
  struct
    type t =
      | Original 
      | Processed 
    let str_to_t = [("Processed", Processed); ("Original", Original)]
    let t_to_str = [(Processed, "Processed"); (Original, "Original")]
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
module StackResourceDriftStatusFilters =
  struct
    type t = StackResourceDriftStatus.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map StackResourceDriftStatus.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list StackResourceDriftStatus.to_query v
    let to_json v = `List (List.map StackResourceDriftStatus.to_json v)
    let of_json j = Json.to_list StackResourceDriftStatus.of_json j
  end
module StackSetOperationResultSummaries =
  struct
    type t = StackSetOperationResultSummary.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map StackSetOperationResultSummary.parse
           (Xml.members "member" xml))
    let to_query v =
      Query.to_query_list StackSetOperationResultSummary.to_query v
    let to_json v = `List (List.map StackSetOperationResultSummary.to_json v)
    let of_json j = Json.to_list StackSetOperationResultSummary.of_json j
  end
module AccountLimitList =
  struct
    type t = AccountLimit.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map AccountLimit.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list AccountLimit.to_query v
    let to_json v = `List (List.map AccountLimit.to_json v)
    let of_json j = Json.to_list AccountLimit.of_json j
  end
module StackSummaries =
  struct
    type t = StackSummary.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map StackSummary.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list StackSummary.to_query v
    let to_json v = `List (List.map StackSummary.to_json v)
    let of_json j = Json.to_list StackSummary.of_json j
  end
module StackResourceDrifts =
  struct
    type t = StackResourceDrift.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map StackResourceDrift.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list StackResourceDrift.to_query v
    let to_json v = `List (List.map StackResourceDrift.to_json v)
    let of_json j = Json.to_list StackResourceDrift.of_json j
  end
module StackStatusFilter =
  struct
    type t = StackStatus.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map StackStatus.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list StackStatus.to_query v
    let to_json v = `List (List.map StackStatus.to_json v)
    let of_json j = Json.to_list StackStatus.of_json j
  end
module Changes =
  struct
    type t = Change.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Change.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Change.to_query v
    let to_json v = `List (List.map Change.to_json v)
    let of_json j = Json.to_list Change.of_json j
  end
module ChangeSetSummaries =
  struct
    type t = ChangeSetSummary.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map ChangeSetSummary.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list ChangeSetSummary.to_query v
    let to_json v = `List (List.map ChangeSetSummary.to_json v)
    let of_json j = Json.to_list ChangeSetSummary.of_json j
  end
module Exports =
  struct
    type t = Export.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Export.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Export.to_query v
    let to_json v = `List (List.map Export.to_json v)
    let of_json j = Json.to_list Export.of_json j
  end
module RetainResources =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module ChangeSetType =
  struct
    type t =
      | CREATE 
      | UPDATE 
      | IMPORT 
    let str_to_t =
      [("IMPORT", IMPORT); ("UPDATE", UPDATE); ("CREATE", CREATE)]
    let t_to_str =
      [(IMPORT, "IMPORT"); (UPDATE, "UPDATE"); (CREATE, "CREATE")]
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
module ResourceTypes =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module ResourcesToImport =
  struct
    type t = ResourceToImport.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map ResourceToImport.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list ResourceToImport.to_query v
    let to_json v = `List (List.map ResourceToImport.to_json v)
    let of_json j = Json.to_list ResourceToImport.of_json j
  end
module ResourcesToSkip =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module TemplateParameters =
  struct
    type t = TemplateParameter.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map TemplateParameter.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list TemplateParameter.to_query v
    let to_json v = `List (List.map TemplateParameter.to_json v)
    let of_json j = Json.to_list TemplateParameter.of_json j
  end
module TransformsList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module StackInstance =
  struct
    type t =
      {
      stack_set_id: String.t option ;
      region: String.t option ;
      account: String.t option ;
      stack_id: String.t option ;
      parameter_overrides: Parameters.t ;
      status: StackInstanceStatus.t option ;
      stack_instance_status: StackInstanceComprehensiveStatus.t option ;
      status_reason: String.t option ;
      organizational_unit_id: String.t option ;
      drift_status: StackDriftStatus.t option ;
      last_drift_check_timestamp: DateTime.t option }
    let make ?stack_set_id  ?region  ?account  ?stack_id 
      ?(parameter_overrides= [])  ?status  ?stack_instance_status 
      ?status_reason  ?organizational_unit_id  ?drift_status 
      ?last_drift_check_timestamp  () =
      {
        stack_set_id;
        region;
        account;
        stack_id;
        parameter_overrides;
        status;
        stack_instance_status;
        status_reason;
        organizational_unit_id;
        drift_status;
        last_drift_check_timestamp
      }
    let parse xml =
      Some
        {
          stack_set_id =
            (Util.option_bind (Xml.member "StackSetId" xml) String.parse);
          region = (Util.option_bind (Xml.member "Region" xml) String.parse);
          account =
            (Util.option_bind (Xml.member "Account" xml) String.parse);
          stack_id =
            (Util.option_bind (Xml.member "StackId" xml) String.parse);
          parameter_overrides =
            (Util.of_option []
               (Util.option_bind (Xml.member "ParameterOverrides" xml)
                  Parameters.parse));
          status =
            (Util.option_bind (Xml.member "Status" xml)
               StackInstanceStatus.parse);
          stack_instance_status =
            (Util.option_bind (Xml.member "StackInstanceStatus" xml)
               StackInstanceComprehensiveStatus.parse);
          status_reason =
            (Util.option_bind (Xml.member "StatusReason" xml) String.parse);
          organizational_unit_id =
            (Util.option_bind (Xml.member "OrganizationalUnitId" xml)
               String.parse);
          drift_status =
            (Util.option_bind (Xml.member "DriftStatus" xml)
               StackDriftStatus.parse);
          last_drift_check_timestamp =
            (Util.option_bind (Xml.member "LastDriftCheckTimestamp" xml)
               DateTime.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.last_drift_check_timestamp
              (fun f ->
                 Query.Pair
                   ("LastDriftCheckTimestamp", (DateTime.to_query f)));
           Util.option_map v.drift_status
             (fun f ->
                Query.Pair ("DriftStatus", (StackDriftStatus.to_query f)));
           Util.option_map v.organizational_unit_id
             (fun f ->
                Query.Pair ("OrganizationalUnitId", (String.to_query f)));
           Util.option_map v.status_reason
             (fun f -> Query.Pair ("StatusReason", (String.to_query f)));
           Util.option_map v.stack_instance_status
             (fun f ->
                Query.Pair
                  ("StackInstanceStatus",
                    (StackInstanceComprehensiveStatus.to_query f)));
           Util.option_map v.status
             (fun f ->
                Query.Pair ("Status", (StackInstanceStatus.to_query f)));
           Some
             (Query.Pair
                ("ParameterOverrides.member",
                  (Parameters.to_query v.parameter_overrides)));
           Util.option_map v.stack_id
             (fun f -> Query.Pair ("StackId", (String.to_query f)));
           Util.option_map v.account
             (fun f -> Query.Pair ("Account", (String.to_query f)));
           Util.option_map v.region
             (fun f -> Query.Pair ("Region", (String.to_query f)));
           Util.option_map v.stack_set_id
             (fun f -> Query.Pair ("StackSetId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.last_drift_check_timestamp
              (fun f -> ("last_drift_check_timestamp", (DateTime.to_json f)));
           Util.option_map v.drift_status
             (fun f -> ("drift_status", (StackDriftStatus.to_json f)));
           Util.option_map v.organizational_unit_id
             (fun f -> ("organizational_unit_id", (String.to_json f)));
           Util.option_map v.status_reason
             (fun f -> ("status_reason", (String.to_json f)));
           Util.option_map v.stack_instance_status
             (fun f ->
                ("stack_instance_status",
                  (StackInstanceComprehensiveStatus.to_json f)));
           Util.option_map v.status
             (fun f -> ("status", (StackInstanceStatus.to_json f)));
           Some
             ("parameter_overrides",
               (Parameters.to_json v.parameter_overrides));
           Util.option_map v.stack_id
             (fun f -> ("stack_id", (String.to_json f)));
           Util.option_map v.account
             (fun f -> ("account", (String.to_json f)));
           Util.option_map v.region (fun f -> ("region", (String.to_json f)));
           Util.option_map v.stack_set_id
             (fun f -> ("stack_set_id", (String.to_json f)))])
    let of_json j =
      {
        stack_set_id =
          (Util.option_map (Json.lookup j "stack_set_id") String.of_json);
        region = (Util.option_map (Json.lookup j "region") String.of_json);
        account = (Util.option_map (Json.lookup j "account") String.of_json);
        stack_id =
          (Util.option_map (Json.lookup j "stack_id") String.of_json);
        parameter_overrides =
          (Parameters.of_json
             (Util.of_option_exn (Json.lookup j "parameter_overrides")));
        status =
          (Util.option_map (Json.lookup j "status")
             StackInstanceStatus.of_json);
        stack_instance_status =
          (Util.option_map (Json.lookup j "stack_instance_status")
             StackInstanceComprehensiveStatus.of_json);
        status_reason =
          (Util.option_map (Json.lookup j "status_reason") String.of_json);
        organizational_unit_id =
          (Util.option_map (Json.lookup j "organizational_unit_id")
             String.of_json);
        drift_status =
          (Util.option_map (Json.lookup j "drift_status")
             StackDriftStatus.of_json);
        last_drift_check_timestamp =
          (Util.option_map (Json.lookup j "last_drift_check_timestamp")
             DateTime.of_json)
      }
  end
module StackSetSummaries =
  struct
    type t = StackSetSummary.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map StackSetSummary.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list StackSetSummary.to_query v
    let to_json v = `List (List.map StackSetSummary.to_json v)
    let of_json j = Json.to_list StackSetSummary.of_json j
  end
module TypeSummaries =
  struct
    type t = TypeSummary.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map TypeSummary.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list TypeSummary.to_query v
    let to_json v = `List (List.map TypeSummary.to_json v)
    let of_json j = Json.to_list TypeSummary.of_json j
  end
module StackResourceDetail =
  struct
    type t =
      {
      stack_name: String.t option ;
      stack_id: String.t option ;
      logical_resource_id: String.t ;
      physical_resource_id: String.t option ;
      resource_type: String.t ;
      last_updated_timestamp: DateTime.t ;
      resource_status: ResourceStatus.t ;
      resource_status_reason: String.t option ;
      description: String.t option ;
      metadata: String.t option ;
      drift_information: StackResourceDriftInformation.t option }
    let make ?stack_name  ?stack_id  ~logical_resource_id 
      ?physical_resource_id  ~resource_type  ~last_updated_timestamp 
      ~resource_status  ?resource_status_reason  ?description  ?metadata 
      ?drift_information  () =
      {
        stack_name;
        stack_id;
        logical_resource_id;
        physical_resource_id;
        resource_type;
        last_updated_timestamp;
        resource_status;
        resource_status_reason;
        description;
        metadata;
        drift_information
      }
    let parse xml =
      Some
        {
          stack_name =
            (Util.option_bind (Xml.member "StackName" xml) String.parse);
          stack_id =
            (Util.option_bind (Xml.member "StackId" xml) String.parse);
          logical_resource_id =
            (Xml.required "LogicalResourceId"
               (Util.option_bind (Xml.member "LogicalResourceId" xml)
                  String.parse));
          physical_resource_id =
            (Util.option_bind (Xml.member "PhysicalResourceId" xml)
               String.parse);
          resource_type =
            (Xml.required "ResourceType"
               (Util.option_bind (Xml.member "ResourceType" xml) String.parse));
          last_updated_timestamp =
            (Xml.required "LastUpdatedTimestamp"
               (Util.option_bind (Xml.member "LastUpdatedTimestamp" xml)
                  DateTime.parse));
          resource_status =
            (Xml.required "ResourceStatus"
               (Util.option_bind (Xml.member "ResourceStatus" xml)
                  ResourceStatus.parse));
          resource_status_reason =
            (Util.option_bind (Xml.member "ResourceStatusReason" xml)
               String.parse);
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse);
          metadata =
            (Util.option_bind (Xml.member "Metadata" xml) String.parse);
          drift_information =
            (Util.option_bind (Xml.member "DriftInformation" xml)
               StackResourceDriftInformation.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.drift_information
              (fun f ->
                 Query.Pair
                   ("DriftInformation",
                     (StackResourceDriftInformation.to_query f)));
           Util.option_map v.metadata
             (fun f -> Query.Pair ("Metadata", (String.to_query f)));
           Util.option_map v.description
             (fun f -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.resource_status_reason
             (fun f ->
                Query.Pair ("ResourceStatusReason", (String.to_query f)));
           Some
             (Query.Pair
                ("ResourceStatus",
                  (ResourceStatus.to_query v.resource_status)));
           Some
             (Query.Pair
                ("LastUpdatedTimestamp",
                  (DateTime.to_query v.last_updated_timestamp)));
           Some
             (Query.Pair ("ResourceType", (String.to_query v.resource_type)));
           Util.option_map v.physical_resource_id
             (fun f -> Query.Pair ("PhysicalResourceId", (String.to_query f)));
           Some
             (Query.Pair
                ("LogicalResourceId",
                  (String.to_query v.logical_resource_id)));
           Util.option_map v.stack_id
             (fun f -> Query.Pair ("StackId", (String.to_query f)));
           Util.option_map v.stack_name
             (fun f -> Query.Pair ("StackName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.drift_information
              (fun f ->
                 ("drift_information",
                   (StackResourceDriftInformation.to_json f)));
           Util.option_map v.metadata
             (fun f -> ("metadata", (String.to_json f)));
           Util.option_map v.description
             (fun f -> ("description", (String.to_json f)));
           Util.option_map v.resource_status_reason
             (fun f -> ("resource_status_reason", (String.to_json f)));
           Some
             ("resource_status", (ResourceStatus.to_json v.resource_status));
           Some
             ("last_updated_timestamp",
               (DateTime.to_json v.last_updated_timestamp));
           Some ("resource_type", (String.to_json v.resource_type));
           Util.option_map v.physical_resource_id
             (fun f -> ("physical_resource_id", (String.to_json f)));
           Some
             ("logical_resource_id", (String.to_json v.logical_resource_id));
           Util.option_map v.stack_id
             (fun f -> ("stack_id", (String.to_json f)));
           Util.option_map v.stack_name
             (fun f -> ("stack_name", (String.to_json f)))])
    let of_json j =
      {
        stack_name =
          (Util.option_map (Json.lookup j "stack_name") String.of_json);
        stack_id =
          (Util.option_map (Json.lookup j "stack_id") String.of_json);
        logical_resource_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "logical_resource_id")));
        physical_resource_id =
          (Util.option_map (Json.lookup j "physical_resource_id")
             String.of_json);
        resource_type =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "resource_type")));
        last_updated_timestamp =
          (DateTime.of_json
             (Util.of_option_exn (Json.lookup j "last_updated_timestamp")));
        resource_status =
          (ResourceStatus.of_json
             (Util.of_option_exn (Json.lookup j "resource_status")));
        resource_status_reason =
          (Util.option_map (Json.lookup j "resource_status_reason")
             String.of_json);
        description =
          (Util.option_map (Json.lookup j "description") String.of_json);
        metadata =
          (Util.option_map (Json.lookup j "metadata") String.of_json);
        drift_information =
          (Util.option_map (Json.lookup j "drift_information")
             StackResourceDriftInformation.of_json)
      }
  end
module StackDriftDetectionStatus =
  struct
    type t =
      | DETECTION_IN_PROGRESS 
      | DETECTION_FAILED 
      | DETECTION_COMPLETE 
    let str_to_t =
      [("DETECTION_COMPLETE", DETECTION_COMPLETE);
      ("DETECTION_FAILED", DETECTION_FAILED);
      ("DETECTION_IN_PROGRESS", DETECTION_IN_PROGRESS)]
    let t_to_str =
      [(DETECTION_COMPLETE, "DETECTION_COMPLETE");
      (DETECTION_FAILED, "DETECTION_FAILED");
      (DETECTION_IN_PROGRESS, "DETECTION_IN_PROGRESS")]
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
module RegistrationStatus =
  struct
    type t =
      | COMPLETE 
      | IN_PROGRESS 
      | FAILED 
    let str_to_t =
      [("FAILED", FAILED);
      ("IN_PROGRESS", IN_PROGRESS);
      ("COMPLETE", COMPLETE)]
    let t_to_str =
      [(FAILED, "FAILED");
      (IN_PROGRESS, "IN_PROGRESS");
      (COMPLETE, "COMPLETE")]
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
module StackSetOperation =
  struct
    type t =
      {
      operation_id: String.t option ;
      stack_set_id: String.t option ;
      action: StackSetOperationAction.t option ;
      status: StackSetOperationStatus.t option ;
      operation_preferences: StackSetOperationPreferences.t option ;
      retain_stacks: Boolean.t option ;
      administration_role_a_r_n: String.t option ;
      execution_role_name: String.t option ;
      creation_timestamp: DateTime.t option ;
      end_timestamp: DateTime.t option ;
      deployment_targets: DeploymentTargets.t option ;
      stack_set_drift_detection_details:
        StackSetDriftDetectionDetails.t option }
    let make ?operation_id  ?stack_set_id  ?action  ?status 
      ?operation_preferences  ?retain_stacks  ?administration_role_a_r_n 
      ?execution_role_name  ?creation_timestamp  ?end_timestamp 
      ?deployment_targets  ?stack_set_drift_detection_details  () =
      {
        operation_id;
        stack_set_id;
        action;
        status;
        operation_preferences;
        retain_stacks;
        administration_role_a_r_n;
        execution_role_name;
        creation_timestamp;
        end_timestamp;
        deployment_targets;
        stack_set_drift_detection_details
      }
    let parse xml =
      Some
        {
          operation_id =
            (Util.option_bind (Xml.member "OperationId" xml) String.parse);
          stack_set_id =
            (Util.option_bind (Xml.member "StackSetId" xml) String.parse);
          action =
            (Util.option_bind (Xml.member "Action" xml)
               StackSetOperationAction.parse);
          status =
            (Util.option_bind (Xml.member "Status" xml)
               StackSetOperationStatus.parse);
          operation_preferences =
            (Util.option_bind (Xml.member "OperationPreferences" xml)
               StackSetOperationPreferences.parse);
          retain_stacks =
            (Util.option_bind (Xml.member "RetainStacks" xml) Boolean.parse);
          administration_role_a_r_n =
            (Util.option_bind (Xml.member "AdministrationRoleARN" xml)
               String.parse);
          execution_role_name =
            (Util.option_bind (Xml.member "ExecutionRoleName" xml)
               String.parse);
          creation_timestamp =
            (Util.option_bind (Xml.member "CreationTimestamp" xml)
               DateTime.parse);
          end_timestamp =
            (Util.option_bind (Xml.member "EndTimestamp" xml) DateTime.parse);
          deployment_targets =
            (Util.option_bind (Xml.member "DeploymentTargets" xml)
               DeploymentTargets.parse);
          stack_set_drift_detection_details =
            (Util.option_bind
               (Xml.member "StackSetDriftDetectionDetails" xml)
               StackSetDriftDetectionDetails.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.stack_set_drift_detection_details
              (fun f ->
                 Query.Pair
                   ("StackSetDriftDetectionDetails",
                     (StackSetDriftDetectionDetails.to_query f)));
           Util.option_map v.deployment_targets
             (fun f ->
                Query.Pair
                  ("DeploymentTargets", (DeploymentTargets.to_query f)));
           Util.option_map v.end_timestamp
             (fun f -> Query.Pair ("EndTimestamp", (DateTime.to_query f)));
           Util.option_map v.creation_timestamp
             (fun f ->
                Query.Pair ("CreationTimestamp", (DateTime.to_query f)));
           Util.option_map v.execution_role_name
             (fun f -> Query.Pair ("ExecutionRoleName", (String.to_query f)));
           Util.option_map v.administration_role_a_r_n
             (fun f ->
                Query.Pair ("AdministrationRoleARN", (String.to_query f)));
           Util.option_map v.retain_stacks
             (fun f -> Query.Pair ("RetainStacks", (Boolean.to_query f)));
           Util.option_map v.operation_preferences
             (fun f ->
                Query.Pair
                  ("OperationPreferences",
                    (StackSetOperationPreferences.to_query f)));
           Util.option_map v.status
             (fun f ->
                Query.Pair ("Status", (StackSetOperationStatus.to_query f)));
           Util.option_map v.action
             (fun f ->
                Query.Pair ("Action", (StackSetOperationAction.to_query f)));
           Util.option_map v.stack_set_id
             (fun f -> Query.Pair ("StackSetId", (String.to_query f)));
           Util.option_map v.operation_id
             (fun f -> Query.Pair ("OperationId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.stack_set_drift_detection_details
              (fun f ->
                 ("stack_set_drift_detection_details",
                   (StackSetDriftDetectionDetails.to_json f)));
           Util.option_map v.deployment_targets
             (fun f -> ("deployment_targets", (DeploymentTargets.to_json f)));
           Util.option_map v.end_timestamp
             (fun f -> ("end_timestamp", (DateTime.to_json f)));
           Util.option_map v.creation_timestamp
             (fun f -> ("creation_timestamp", (DateTime.to_json f)));
           Util.option_map v.execution_role_name
             (fun f -> ("execution_role_name", (String.to_json f)));
           Util.option_map v.administration_role_a_r_n
             (fun f -> ("administration_role_a_r_n", (String.to_json f)));
           Util.option_map v.retain_stacks
             (fun f -> ("retain_stacks", (Boolean.to_json f)));
           Util.option_map v.operation_preferences
             (fun f ->
                ("operation_preferences",
                  (StackSetOperationPreferences.to_json f)));
           Util.option_map v.status
             (fun f -> ("status", (StackSetOperationStatus.to_json f)));
           Util.option_map v.action
             (fun f -> ("action", (StackSetOperationAction.to_json f)));
           Util.option_map v.stack_set_id
             (fun f -> ("stack_set_id", (String.to_json f)));
           Util.option_map v.operation_id
             (fun f -> ("operation_id", (String.to_json f)))])
    let of_json j =
      {
        operation_id =
          (Util.option_map (Json.lookup j "operation_id") String.of_json);
        stack_set_id =
          (Util.option_map (Json.lookup j "stack_set_id") String.of_json);
        action =
          (Util.option_map (Json.lookup j "action")
             StackSetOperationAction.of_json);
        status =
          (Util.option_map (Json.lookup j "status")
             StackSetOperationStatus.of_json);
        operation_preferences =
          (Util.option_map (Json.lookup j "operation_preferences")
             StackSetOperationPreferences.of_json);
        retain_stacks =
          (Util.option_map (Json.lookup j "retain_stacks") Boolean.of_json);
        administration_role_a_r_n =
          (Util.option_map (Json.lookup j "administration_role_a_r_n")
             String.of_json);
        execution_role_name =
          (Util.option_map (Json.lookup j "execution_role_name")
             String.of_json);
        creation_timestamp =
          (Util.option_map (Json.lookup j "creation_timestamp")
             DateTime.of_json);
        end_timestamp =
          (Util.option_map (Json.lookup j "end_timestamp") DateTime.of_json);
        deployment_targets =
          (Util.option_map (Json.lookup j "deployment_targets")
             DeploymentTargets.of_json);
        stack_set_drift_detection_details =
          (Util.option_map
             (Json.lookup j "stack_set_drift_detection_details")
             StackSetDriftDetectionDetails.of_json)
      }
  end
module StackEvents =
  struct
    type t = StackEvent.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map StackEvent.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list StackEvent.to_query v
    let to_json v = `List (List.map StackEvent.to_json v)
    let of_json j = Json.to_list StackEvent.of_json j
  end
module StackSet =
  struct
    type t =
      {
      stack_set_name: String.t option ;
      stack_set_id: String.t option ;
      description: String.t option ;
      status: StackSetStatus.t option ;
      template_body: String.t option ;
      parameters: Parameters.t ;
      capabilities: Capabilities.t ;
      tags: Tags.t ;
      stack_set_a_r_n: String.t option ;
      administration_role_a_r_n: String.t option ;
      execution_role_name: String.t option ;
      stack_set_drift_detection_details:
        StackSetDriftDetectionDetails.t option ;
      auto_deployment: AutoDeployment.t option ;
      permission_model: PermissionModels.t option ;
      organizational_unit_ids: OrganizationalUnitIdList.t }
    let make ?stack_set_name  ?stack_set_id  ?description  ?status 
      ?template_body  ?(parameters= [])  ?(capabilities= [])  ?(tags= []) 
      ?stack_set_a_r_n  ?administration_role_a_r_n  ?execution_role_name 
      ?stack_set_drift_detection_details  ?auto_deployment  ?permission_model
       ?(organizational_unit_ids= [])  () =
      {
        stack_set_name;
        stack_set_id;
        description;
        status;
        template_body;
        parameters;
        capabilities;
        tags;
        stack_set_a_r_n;
        administration_role_a_r_n;
        execution_role_name;
        stack_set_drift_detection_details;
        auto_deployment;
        permission_model;
        organizational_unit_ids
      }
    let parse xml =
      Some
        {
          stack_set_name =
            (Util.option_bind (Xml.member "StackSetName" xml) String.parse);
          stack_set_id =
            (Util.option_bind (Xml.member "StackSetId" xml) String.parse);
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse);
          status =
            (Util.option_bind (Xml.member "Status" xml) StackSetStatus.parse);
          template_body =
            (Util.option_bind (Xml.member "TemplateBody" xml) String.parse);
          parameters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Parameters" xml)
                  Parameters.parse));
          capabilities =
            (Util.of_option []
               (Util.option_bind (Xml.member "Capabilities" xml)
                  Capabilities.parse));
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) Tags.parse));
          stack_set_a_r_n =
            (Util.option_bind (Xml.member "StackSetARN" xml) String.parse);
          administration_role_a_r_n =
            (Util.option_bind (Xml.member "AdministrationRoleARN" xml)
               String.parse);
          execution_role_name =
            (Util.option_bind (Xml.member "ExecutionRoleName" xml)
               String.parse);
          stack_set_drift_detection_details =
            (Util.option_bind
               (Xml.member "StackSetDriftDetectionDetails" xml)
               StackSetDriftDetectionDetails.parse);
          auto_deployment =
            (Util.option_bind (Xml.member "AutoDeployment" xml)
               AutoDeployment.parse);
          permission_model =
            (Util.option_bind (Xml.member "PermissionModel" xml)
               PermissionModels.parse);
          organizational_unit_ids =
            (Util.of_option []
               (Util.option_bind (Xml.member "OrganizationalUnitIds" xml)
                  OrganizationalUnitIdList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("OrganizationalUnitIds.member",
                   (OrganizationalUnitIdList.to_query
                      v.organizational_unit_ids)));
           Util.option_map v.permission_model
             (fun f ->
                Query.Pair ("PermissionModel", (PermissionModels.to_query f)));
           Util.option_map v.auto_deployment
             (fun f ->
                Query.Pair ("AutoDeployment", (AutoDeployment.to_query f)));
           Util.option_map v.stack_set_drift_detection_details
             (fun f ->
                Query.Pair
                  ("StackSetDriftDetectionDetails",
                    (StackSetDriftDetectionDetails.to_query f)));
           Util.option_map v.execution_role_name
             (fun f -> Query.Pair ("ExecutionRoleName", (String.to_query f)));
           Util.option_map v.administration_role_a_r_n
             (fun f ->
                Query.Pair ("AdministrationRoleARN", (String.to_query f)));
           Util.option_map v.stack_set_a_r_n
             (fun f -> Query.Pair ("StackSetARN", (String.to_query f)));
           Some (Query.Pair ("Tags.member", (Tags.to_query v.tags)));
           Some
             (Query.Pair
                ("Capabilities.member",
                  (Capabilities.to_query v.capabilities)));
           Some
             (Query.Pair
                ("Parameters.member", (Parameters.to_query v.parameters)));
           Util.option_map v.template_body
             (fun f -> Query.Pair ("TemplateBody", (String.to_query f)));
           Util.option_map v.status
             (fun f -> Query.Pair ("Status", (StackSetStatus.to_query f)));
           Util.option_map v.description
             (fun f -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.stack_set_id
             (fun f -> Query.Pair ("StackSetId", (String.to_query f)));
           Util.option_map v.stack_set_name
             (fun f -> Query.Pair ("StackSetName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("organizational_unit_ids",
                (OrganizationalUnitIdList.to_json v.organizational_unit_ids));
           Util.option_map v.permission_model
             (fun f -> ("permission_model", (PermissionModels.to_json f)));
           Util.option_map v.auto_deployment
             (fun f -> ("auto_deployment", (AutoDeployment.to_json f)));
           Util.option_map v.stack_set_drift_detection_details
             (fun f ->
                ("stack_set_drift_detection_details",
                  (StackSetDriftDetectionDetails.to_json f)));
           Util.option_map v.execution_role_name
             (fun f -> ("execution_role_name", (String.to_json f)));
           Util.option_map v.administration_role_a_r_n
             (fun f -> ("administration_role_a_r_n", (String.to_json f)));
           Util.option_map v.stack_set_a_r_n
             (fun f -> ("stack_set_a_r_n", (String.to_json f)));
           Some ("tags", (Tags.to_json v.tags));
           Some ("capabilities", (Capabilities.to_json v.capabilities));
           Some ("parameters", (Parameters.to_json v.parameters));
           Util.option_map v.template_body
             (fun f -> ("template_body", (String.to_json f)));
           Util.option_map v.status
             (fun f -> ("status", (StackSetStatus.to_json f)));
           Util.option_map v.description
             (fun f -> ("description", (String.to_json f)));
           Util.option_map v.stack_set_id
             (fun f -> ("stack_set_id", (String.to_json f)));
           Util.option_map v.stack_set_name
             (fun f -> ("stack_set_name", (String.to_json f)))])
    let of_json j =
      {
        stack_set_name =
          (Util.option_map (Json.lookup j "stack_set_name") String.of_json);
        stack_set_id =
          (Util.option_map (Json.lookup j "stack_set_id") String.of_json);
        description =
          (Util.option_map (Json.lookup j "description") String.of_json);
        status =
          (Util.option_map (Json.lookup j "status") StackSetStatus.of_json);
        template_body =
          (Util.option_map (Json.lookup j "template_body") String.of_json);
        parameters =
          (Parameters.of_json
             (Util.of_option_exn (Json.lookup j "parameters")));
        capabilities =
          (Capabilities.of_json
             (Util.of_option_exn (Json.lookup j "capabilities")));
        tags = (Tags.of_json (Util.of_option_exn (Json.lookup j "tags")));
        stack_set_a_r_n =
          (Util.option_map (Json.lookup j "stack_set_a_r_n") String.of_json);
        administration_role_a_r_n =
          (Util.option_map (Json.lookup j "administration_role_a_r_n")
             String.of_json);
        execution_role_name =
          (Util.option_map (Json.lookup j "execution_role_name")
             String.of_json);
        stack_set_drift_detection_details =
          (Util.option_map
             (Json.lookup j "stack_set_drift_detection_details")
             StackSetDriftDetectionDetails.of_json);
        auto_deployment =
          (Util.option_map (Json.lookup j "auto_deployment")
             AutoDeployment.of_json);
        permission_model =
          (Util.option_map (Json.lookup j "permission_model")
             PermissionModels.of_json);
        organizational_unit_ids =
          (OrganizationalUnitIdList.of_json
             (Util.of_option_exn (Json.lookup j "organizational_unit_ids")))
      }
  end
module DeprecatedStatus =
  struct
    type t =
      | LIVE 
      | DEPRECATED 
    let str_to_t = [("DEPRECATED", DEPRECATED); ("LIVE", LIVE)]
    let t_to_str = [(DEPRECATED, "DEPRECATED"); (LIVE, "LIVE")]
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
module ProvisioningType =
  struct
    type t =
      | NON_PROVISIONABLE 
      | IMMUTABLE 
      | FULLY_MUTABLE 
    let str_to_t =
      [("FULLY_MUTABLE", FULLY_MUTABLE);
      ("IMMUTABLE", IMMUTABLE);
      ("NON_PROVISIONABLE", NON_PROVISIONABLE)]
    let t_to_str =
      [(FULLY_MUTABLE, "FULLY_MUTABLE");
      (IMMUTABLE, "IMMUTABLE");
      (NON_PROVISIONABLE, "NON_PROVISIONABLE")]
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
module Visibility =
  struct
    type t =
      | PUBLIC 
      | PRIVATE 
    let str_to_t = [("PRIVATE", PRIVATE); ("PUBLIC", PUBLIC)]
    let t_to_str = [(PRIVATE, "PRIVATE"); (PUBLIC, "PUBLIC")]
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
module TypeVersionSummaries =
  struct
    type t = TypeVersionSummary.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map TypeVersionSummary.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list TypeVersionSummary.to_query v
    let to_json v = `List (List.map TypeVersionSummary.to_json v)
    let of_json j = Json.to_list TypeVersionSummary.of_json j
  end
module Imports =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module StackSetOperationSummaries =
  struct
    type t = StackSetOperationSummary.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map StackSetOperationSummary.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list StackSetOperationSummary.to_query v
    let to_json v = `List (List.map StackSetOperationSummary.to_json v)
    let of_json j = Json.to_list StackSetOperationSummary.of_json j
  end
module StackResourceSummaries =
  struct
    type t = StackResourceSummary.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map StackResourceSummary.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list StackResourceSummary.to_query v
    let to_json v = `List (List.map StackResourceSummary.to_json v)
    let of_json j = Json.to_list StackResourceSummary.of_json j
  end
module HandlerErrorCode =
  struct
    type t =
      | NotUpdatable 
      | InvalidRequest 
      | AccessDenied 
      | InvalidCredentials 
      | AlreadyExists 
      | NotFound 
      | ResourceConflict 
      | Throttling 
      | ServiceLimitExceeded 
      | NotStabilized 
      | GeneralServiceException 
      | ServiceInternalError 
      | NetworkFailure 
      | InternalFailure 
    let str_to_t =
      [("InternalFailure", InternalFailure);
      ("NetworkFailure", NetworkFailure);
      ("ServiceInternalError", ServiceInternalError);
      ("GeneralServiceException", GeneralServiceException);
      ("NotStabilized", NotStabilized);
      ("ServiceLimitExceeded", ServiceLimitExceeded);
      ("Throttling", Throttling);
      ("ResourceConflict", ResourceConflict);
      ("NotFound", NotFound);
      ("AlreadyExists", AlreadyExists);
      ("InvalidCredentials", InvalidCredentials);
      ("AccessDenied", AccessDenied);
      ("InvalidRequest", InvalidRequest);
      ("NotUpdatable", NotUpdatable)]
    let t_to_str =
      [(InternalFailure, "InternalFailure");
      (NetworkFailure, "NetworkFailure");
      (ServiceInternalError, "ServiceInternalError");
      (GeneralServiceException, "GeneralServiceException");
      (NotStabilized, "NotStabilized");
      (ServiceLimitExceeded, "ServiceLimitExceeded");
      (Throttling, "Throttling");
      (ResourceConflict, "ResourceConflict");
      (NotFound, "NotFound");
      (AlreadyExists, "AlreadyExists");
      (InvalidCredentials, "InvalidCredentials");
      (AccessDenied, "AccessDenied");
      (InvalidRequest, "InvalidRequest");
      (NotUpdatable, "NotUpdatable")]
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
module OperationStatus =
  struct
    type t =
      | PENDING 
      | IN_PROGRESS 
      | SUCCESS 
      | FAILED 
    let str_to_t =
      [("FAILED", FAILED);
      ("SUCCESS", SUCCESS);
      ("IN_PROGRESS", IN_PROGRESS);
      ("PENDING", PENDING)]
    let t_to_str =
      [(FAILED, "FAILED");
      (SUCCESS, "SUCCESS");
      (IN_PROGRESS, "IN_PROGRESS");
      (PENDING, "PENDING")]
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
module LoggingConfig =
  struct
    type t = {
      log_role_arn: String.t ;
      log_group_name: String.t }
    let make ~log_role_arn  ~log_group_name  () =
      { log_role_arn; log_group_name }
    let parse xml =
      Some
        {
          log_role_arn =
            (Xml.required "LogRoleArn"
               (Util.option_bind (Xml.member "LogRoleArn" xml) String.parse));
          log_group_name =
            (Xml.required "LogGroupName"
               (Util.option_bind (Xml.member "LogGroupName" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("LogGroupName", (String.to_query v.log_group_name)));
           Some (Query.Pair ("LogRoleArn", (String.to_query v.log_role_arn)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("log_group_name", (String.to_json v.log_group_name));
           Some ("log_role_arn", (String.to_json v.log_role_arn))])
    let of_json j =
      {
        log_role_arn =
          (String.of_json (Util.of_option_exn (Json.lookup j "log_role_arn")));
        log_group_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "log_group_name")))
      }
  end
module StackInstanceFilters =
  struct
    type t = StackInstanceFilter.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map StackInstanceFilter.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list StackInstanceFilter.to_query v
    let to_json v = `List (List.map StackInstanceFilter.to_json v)
    let of_json j = Json.to_list StackInstanceFilter.of_json j
  end
module ResourceSignalStatus =
  struct
    type t =
      | SUCCESS 
      | FAILURE 
    let str_to_t = [("FAILURE", FAILURE); ("SUCCESS", SUCCESS)]
    let t_to_str = [(FAILURE, "FAILURE"); (SUCCESS, "SUCCESS")]
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
module ParameterDeclarations =
  struct
    type t = ParameterDeclaration.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map ParameterDeclaration.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list ParameterDeclaration.to_query v
    let to_json v = `List (List.map ParameterDeclaration.to_json v)
    let of_json j = Json.to_list ParameterDeclaration.of_json j
  end
module ResourceIdentifierSummaries =
  struct
    type t = ResourceIdentifierSummary.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map ResourceIdentifierSummary.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list ResourceIdentifierSummary.to_query v
    let to_json v = `List (List.map ResourceIdentifierSummary.to_json v)
    let of_json j = Json.to_list ResourceIdentifierSummary.of_json j
  end
module StackInstanceSummaries =
  struct
    type t = StackInstanceSummary.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map StackInstanceSummary.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list StackInstanceSummary.to_query v
    let to_json v = `List (List.map StackInstanceSummary.to_json v)
    let of_json j = Json.to_list StackInstanceSummary.of_json j
  end
module Stacks =
  struct
    type t = Stack.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Stack.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list Stack.to_query v
    let to_json v = `List (List.map Stack.to_json v)
    let of_json j = Json.to_list Stack.of_json j
  end
module OnFailure =
  struct
    type t =
      | DO_NOTHING 
      | ROLLBACK 
      | DELETE 
    let str_to_t =
      [("DELETE", DELETE);
      ("ROLLBACK", ROLLBACK);
      ("DO_NOTHING", DO_NOTHING)]
    let t_to_str =
      [(DELETE, "DELETE");
      (ROLLBACK, "ROLLBACK");
      (DO_NOTHING, "DO_NOTHING")]
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
module RegistrationTokenList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module StackResources =
  struct
    type t = StackResource.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map StackResource.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list StackResource.to_query v
    let to_json v = `List (List.map StackResource.to_json v)
    let of_json j = Json.to_list StackResource.of_json j
  end
module StageList =
  struct
    type t = TemplateStage.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map TemplateStage.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list TemplateStage.to_query v
    let to_json v = `List (List.map TemplateStage.to_json v)
    let of_json j = Json.to_list TemplateStage.of_json j
  end
module DescribeStackResourceDriftsInput =
  struct
    type t =
      {
      stack_name: String.t ;
      stack_resource_drift_status_filters: StackResourceDriftStatusFilters.t ;
      next_token: String.t option ;
      max_results: Integer.t option }
    let make ~stack_name  ?(stack_resource_drift_status_filters= []) 
      ?next_token  ?max_results  () =
      {
        stack_name;
        stack_resource_drift_status_filters;
        next_token;
        max_results
      }
    let parse xml =
      Some
        {
          stack_name =
            (Xml.required "StackName"
               (Util.option_bind (Xml.member "StackName" xml) String.parse));
          stack_resource_drift_status_filters =
            (Util.of_option []
               (Util.option_bind
                  (Xml.member "StackResourceDriftStatusFilters" xml)
                  StackResourceDriftStatusFilters.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse);
          max_results =
            (Util.option_bind (Xml.member "MaxResults" xml) Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.max_results
              (fun f -> Query.Pair ("MaxResults", (Integer.to_query f)));
           Util.option_map v.next_token
             (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair
                ("StackResourceDriftStatusFilters.member",
                  (StackResourceDriftStatusFilters.to_query
                     v.stack_resource_drift_status_filters)));
           Some (Query.Pair ("StackName", (String.to_query v.stack_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.max_results
              (fun f -> ("max_results", (Integer.to_json f)));
           Util.option_map v.next_token
             (fun f -> ("next_token", (String.to_json f)));
           Some
             ("stack_resource_drift_status_filters",
               (StackResourceDriftStatusFilters.to_json
                  v.stack_resource_drift_status_filters));
           Some ("stack_name", (String.to_json v.stack_name))])
    let of_json j =
      {
        stack_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "stack_name")));
        stack_resource_drift_status_filters =
          (StackResourceDriftStatusFilters.of_json
             (Util.of_option_exn
                (Json.lookup j "stack_resource_drift_status_filters")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        max_results =
          (Util.option_map (Json.lookup j "max_results") Integer.of_json)
      }
  end
module NameAlreadyExistsException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ListStackSetOperationResultsOutput =
  struct
    type t =
      {
      summaries: StackSetOperationResultSummaries.t ;
      next_token: String.t option }
    let make ?(summaries= [])  ?next_token  () = { summaries; next_token }
    let parse xml =
      Some
        {
          summaries =
            (Util.of_option []
               (Util.option_bind (Xml.member "Summaries" xml)
                  StackSetOperationResultSummaries.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair
                ("Summaries.member",
                  (StackSetOperationResultSummaries.to_query v.summaries)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some
             ("summaries",
               (StackSetOperationResultSummaries.to_json v.summaries))])
    let of_json j =
      {
        summaries =
          (StackSetOperationResultSummaries.of_json
             (Util.of_option_exn (Json.lookup j "summaries")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module DescribeAccountLimitsOutput =
  struct
    type t =
      {
      account_limits: AccountLimitList.t ;
      next_token: String.t option }
    let make ?(account_limits= [])  ?next_token  () =
      { account_limits; next_token }
    let parse xml =
      Some
        {
          account_limits =
            (Util.of_option []
               (Util.option_bind (Xml.member "AccountLimits" xml)
                  AccountLimitList.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair
                ("AccountLimits.member",
                  (AccountLimitList.to_query v.account_limits)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some
             ("account_limits", (AccountLimitList.to_json v.account_limits))])
    let of_json j =
      {
        account_limits =
          (AccountLimitList.of_json
             (Util.of_option_exn (Json.lookup j "account_limits")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module CreateStackOutput =
  struct
    type t = {
      stack_id: String.t option }
    let make ?stack_id  () = { stack_id }
    let parse xml =
      Some
        {
          stack_id =
            (Util.option_bind (Xml.member "StackId" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.stack_id
              (fun f -> Query.Pair ("StackId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.stack_id
              (fun f -> ("stack_id", (String.to_json f)))])
    let of_json j =
      {
        stack_id =
          (Util.option_map (Json.lookup j "stack_id") String.of_json)
      }
  end
module DetectStackSetDriftOutput =
  struct
    type t = {
      operation_id: String.t option }
    let make ?operation_id  () = { operation_id }
    let parse xml =
      Some
        {
          operation_id =
            (Util.option_bind (Xml.member "OperationId" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.operation_id
              (fun f -> Query.Pair ("OperationId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.operation_id
              (fun f -> ("operation_id", (String.to_json f)))])
    let of_json j =
      {
        operation_id =
          (Util.option_map (Json.lookup j "operation_id") String.of_json)
      }
  end
module DescribeStackResourceInput =
  struct
    type t = {
      stack_name: String.t ;
      logical_resource_id: String.t }
    let make ~stack_name  ~logical_resource_id  () =
      { stack_name; logical_resource_id }
    let parse xml =
      Some
        {
          stack_name =
            (Xml.required "StackName"
               (Util.option_bind (Xml.member "StackName" xml) String.parse));
          logical_resource_id =
            (Xml.required "LogicalResourceId"
               (Util.option_bind (Xml.member "LogicalResourceId" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("LogicalResourceId",
                   (String.to_query v.logical_resource_id)));
           Some (Query.Pair ("StackName", (String.to_query v.stack_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("logical_resource_id", (String.to_json v.logical_resource_id));
           Some ("stack_name", (String.to_json v.stack_name))])
    let of_json j =
      {
        stack_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "stack_name")));
        logical_resource_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "logical_resource_id")))
      }
  end
module CreateChangeSetOutput =
  struct
    type t = {
      id: String.t option ;
      stack_id: String.t option }
    let make ?id  ?stack_id  () = { id; stack_id }
    let parse xml =
      Some
        {
          id = (Util.option_bind (Xml.member "Id" xml) String.parse);
          stack_id =
            (Util.option_bind (Xml.member "StackId" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.stack_id
              (fun f -> Query.Pair ("StackId", (String.to_query f)));
           Util.option_map v.id
             (fun f -> Query.Pair ("Id", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.stack_id
              (fun f -> ("stack_id", (String.to_json f)));
           Util.option_map v.id (fun f -> ("id", (String.to_json f)))])
    let of_json j =
      {
        id = (Util.option_map (Json.lookup j "id") String.of_json);
        stack_id =
          (Util.option_map (Json.lookup j "stack_id") String.of_json)
      }
  end
module ListStacksOutput =
  struct
    type t =
      {
      stack_summaries: StackSummaries.t ;
      next_token: String.t option }
    let make ?(stack_summaries= [])  ?next_token  () =
      { stack_summaries; next_token }
    let parse xml =
      Some
        {
          stack_summaries =
            (Util.of_option []
               (Util.option_bind (Xml.member "StackSummaries" xml)
                  StackSummaries.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair
                ("StackSummaries.member",
                  (StackSummaries.to_query v.stack_summaries)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some
             ("stack_summaries", (StackSummaries.to_json v.stack_summaries))])
    let of_json j =
      {
        stack_summaries =
          (StackSummaries.of_json
             (Util.of_option_exn (Json.lookup j "stack_summaries")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module DescribeStackResourceDriftsOutput =
  struct
    type t =
      {
      stack_resource_drifts: StackResourceDrifts.t ;
      next_token: String.t option }
    let make ~stack_resource_drifts  ?next_token  () =
      { stack_resource_drifts; next_token }
    let parse xml =
      Some
        {
          stack_resource_drifts =
            (Xml.required "StackResourceDrifts"
               (Util.option_bind (Xml.member "StackResourceDrifts" xml)
                  StackResourceDrifts.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair
                ("StackResourceDrifts.member",
                  (StackResourceDrifts.to_query v.stack_resource_drifts)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some
             ("stack_resource_drifts",
               (StackResourceDrifts.to_json v.stack_resource_drifts))])
    let of_json j =
      {
        stack_resource_drifts =
          (StackResourceDrifts.of_json
             (Util.of_option_exn (Json.lookup j "stack_resource_drifts")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module GetTemplateInput =
  struct
    type t =
      {
      stack_name: String.t option ;
      change_set_name: String.t option ;
      template_stage: TemplateStage.t option }
    let make ?stack_name  ?change_set_name  ?template_stage  () =
      { stack_name; change_set_name; template_stage }
    let parse xml =
      Some
        {
          stack_name =
            (Util.option_bind (Xml.member "StackName" xml) String.parse);
          change_set_name =
            (Util.option_bind (Xml.member "ChangeSetName" xml) String.parse);
          template_stage =
            (Util.option_bind (Xml.member "TemplateStage" xml)
               TemplateStage.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.template_stage
              (fun f ->
                 Query.Pair ("TemplateStage", (TemplateStage.to_query f)));
           Util.option_map v.change_set_name
             (fun f -> Query.Pair ("ChangeSetName", (String.to_query f)));
           Util.option_map v.stack_name
             (fun f -> Query.Pair ("StackName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.template_stage
              (fun f -> ("template_stage", (TemplateStage.to_json f)));
           Util.option_map v.change_set_name
             (fun f -> ("change_set_name", (String.to_json f)));
           Util.option_map v.stack_name
             (fun f -> ("stack_name", (String.to_json f)))])
    let of_json j =
      {
        stack_name =
          (Util.option_map (Json.lookup j "stack_name") String.of_json);
        change_set_name =
          (Util.option_map (Json.lookup j "change_set_name") String.of_json);
        template_stage =
          (Util.option_map (Json.lookup j "template_stage")
             TemplateStage.of_json)
      }
  end
module DeleteStackInstancesOutput =
  struct
    type t = {
      operation_id: String.t option }
    let make ?operation_id  () = { operation_id }
    let parse xml =
      Some
        {
          operation_id =
            (Util.option_bind (Xml.member "OperationId" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.operation_id
              (fun f -> Query.Pair ("OperationId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.operation_id
              (fun f -> ("operation_id", (String.to_json f)))])
    let of_json j =
      {
        operation_id =
          (Util.option_map (Json.lookup j "operation_id") String.of_json)
      }
  end
module ListStacksInput =
  struct
    type t =
      {
      next_token: String.t option ;
      stack_status_filter: StackStatusFilter.t }
    let make ?next_token  ?(stack_status_filter= [])  () =
      { next_token; stack_status_filter }
    let parse xml =
      Some
        {
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse);
          stack_status_filter =
            (Util.of_option []
               (Util.option_bind (Xml.member "StackStatusFilter" xml)
                  StackStatusFilter.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("StackStatusFilter.member",
                   (StackStatusFilter.to_query v.stack_status_filter)));
           Util.option_map v.next_token
             (fun f -> Query.Pair ("NextToken", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("stack_status_filter",
                (StackStatusFilter.to_json v.stack_status_filter));
           Util.option_map v.next_token
             (fun f -> ("next_token", (String.to_json f)))])
    let of_json j =
      {
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        stack_status_filter =
          (StackStatusFilter.of_json
             (Util.of_option_exn (Json.lookup j "stack_status_filter")))
      }
  end
module StaleRequestException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DescribeAccountLimitsInput =
  struct
    type t = {
      next_token: String.t option }
    let make ?next_token  () = { next_token }
    let parse xml =
      Some
        {
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)))])
    let of_json j =
      {
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module DescribeChangeSetOutput =
  struct
    type t =
      {
      change_set_name: String.t option ;
      change_set_id: String.t option ;
      stack_id: String.t option ;
      stack_name: String.t option ;
      description: String.t option ;
      parameters: Parameters.t ;
      creation_time: DateTime.t option ;
      execution_status: ExecutionStatus.t option ;
      status: ChangeSetStatus.t option ;
      status_reason: String.t option ;
      notification_a_r_ns: NotificationARNs.t ;
      rollback_configuration: RollbackConfiguration.t option ;
      capabilities: Capabilities.t ;
      tags: Tags.t ;
      changes: Changes.t ;
      next_token: String.t option }
    let make ?change_set_name  ?change_set_id  ?stack_id  ?stack_name 
      ?description  ?(parameters= [])  ?creation_time  ?execution_status 
      ?status  ?status_reason  ?(notification_a_r_ns= []) 
      ?rollback_configuration  ?(capabilities= [])  ?(tags= [])  ?(changes=
      [])  ?next_token  () =
      {
        change_set_name;
        change_set_id;
        stack_id;
        stack_name;
        description;
        parameters;
        creation_time;
        execution_status;
        status;
        status_reason;
        notification_a_r_ns;
        rollback_configuration;
        capabilities;
        tags;
        changes;
        next_token
      }
    let parse xml =
      Some
        {
          change_set_name =
            (Util.option_bind (Xml.member "ChangeSetName" xml) String.parse);
          change_set_id =
            (Util.option_bind (Xml.member "ChangeSetId" xml) String.parse);
          stack_id =
            (Util.option_bind (Xml.member "StackId" xml) String.parse);
          stack_name =
            (Util.option_bind (Xml.member "StackName" xml) String.parse);
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse);
          parameters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Parameters" xml)
                  Parameters.parse));
          creation_time =
            (Util.option_bind (Xml.member "CreationTime" xml) DateTime.parse);
          execution_status =
            (Util.option_bind (Xml.member "ExecutionStatus" xml)
               ExecutionStatus.parse);
          status =
            (Util.option_bind (Xml.member "Status" xml) ChangeSetStatus.parse);
          status_reason =
            (Util.option_bind (Xml.member "StatusReason" xml) String.parse);
          notification_a_r_ns =
            (Util.of_option []
               (Util.option_bind (Xml.member "NotificationARNs" xml)
                  NotificationARNs.parse));
          rollback_configuration =
            (Util.option_bind (Xml.member "RollbackConfiguration" xml)
               RollbackConfiguration.parse);
          capabilities =
            (Util.of_option []
               (Util.option_bind (Xml.member "Capabilities" xml)
                  Capabilities.parse));
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) Tags.parse));
          changes =
            (Util.of_option []
               (Util.option_bind (Xml.member "Changes" xml) Changes.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some (Query.Pair ("Changes.member", (Changes.to_query v.changes)));
           Some (Query.Pair ("Tags.member", (Tags.to_query v.tags)));
           Some
             (Query.Pair
                ("Capabilities.member",
                  (Capabilities.to_query v.capabilities)));
           Util.option_map v.rollback_configuration
             (fun f ->
                Query.Pair
                  ("RollbackConfiguration",
                    (RollbackConfiguration.to_query f)));
           Some
             (Query.Pair
                ("NotificationARNs.member",
                  (NotificationARNs.to_query v.notification_a_r_ns)));
           Util.option_map v.status_reason
             (fun f -> Query.Pair ("StatusReason", (String.to_query f)));
           Util.option_map v.status
             (fun f -> Query.Pair ("Status", (ChangeSetStatus.to_query f)));
           Util.option_map v.execution_status
             (fun f ->
                Query.Pair ("ExecutionStatus", (ExecutionStatus.to_query f)));
           Util.option_map v.creation_time
             (fun f -> Query.Pair ("CreationTime", (DateTime.to_query f)));
           Some
             (Query.Pair
                ("Parameters.member", (Parameters.to_query v.parameters)));
           Util.option_map v.description
             (fun f -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.stack_name
             (fun f -> Query.Pair ("StackName", (String.to_query f)));
           Util.option_map v.stack_id
             (fun f -> Query.Pair ("StackId", (String.to_query f)));
           Util.option_map v.change_set_id
             (fun f -> Query.Pair ("ChangeSetId", (String.to_query f)));
           Util.option_map v.change_set_name
             (fun f -> Query.Pair ("ChangeSetName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some ("changes", (Changes.to_json v.changes));
           Some ("tags", (Tags.to_json v.tags));
           Some ("capabilities", (Capabilities.to_json v.capabilities));
           Util.option_map v.rollback_configuration
             (fun f ->
                ("rollback_configuration", (RollbackConfiguration.to_json f)));
           Some
             ("notification_a_r_ns",
               (NotificationARNs.to_json v.notification_a_r_ns));
           Util.option_map v.status_reason
             (fun f -> ("status_reason", (String.to_json f)));
           Util.option_map v.status
             (fun f -> ("status", (ChangeSetStatus.to_json f)));
           Util.option_map v.execution_status
             (fun f -> ("execution_status", (ExecutionStatus.to_json f)));
           Util.option_map v.creation_time
             (fun f -> ("creation_time", (DateTime.to_json f)));
           Some ("parameters", (Parameters.to_json v.parameters));
           Util.option_map v.description
             (fun f -> ("description", (String.to_json f)));
           Util.option_map v.stack_name
             (fun f -> ("stack_name", (String.to_json f)));
           Util.option_map v.stack_id
             (fun f -> ("stack_id", (String.to_json f)));
           Util.option_map v.change_set_id
             (fun f -> ("change_set_id", (String.to_json f)));
           Util.option_map v.change_set_name
             (fun f -> ("change_set_name", (String.to_json f)))])
    let of_json j =
      {
        change_set_name =
          (Util.option_map (Json.lookup j "change_set_name") String.of_json);
        change_set_id =
          (Util.option_map (Json.lookup j "change_set_id") String.of_json);
        stack_id =
          (Util.option_map (Json.lookup j "stack_id") String.of_json);
        stack_name =
          (Util.option_map (Json.lookup j "stack_name") String.of_json);
        description =
          (Util.option_map (Json.lookup j "description") String.of_json);
        parameters =
          (Parameters.of_json
             (Util.of_option_exn (Json.lookup j "parameters")));
        creation_time =
          (Util.option_map (Json.lookup j "creation_time") DateTime.of_json);
        execution_status =
          (Util.option_map (Json.lookup j "execution_status")
             ExecutionStatus.of_json);
        status =
          (Util.option_map (Json.lookup j "status") ChangeSetStatus.of_json);
        status_reason =
          (Util.option_map (Json.lookup j "status_reason") String.of_json);
        notification_a_r_ns =
          (NotificationARNs.of_json
             (Util.of_option_exn (Json.lookup j "notification_a_r_ns")));
        rollback_configuration =
          (Util.option_map (Json.lookup j "rollback_configuration")
             RollbackConfiguration.of_json);
        capabilities =
          (Capabilities.of_json
             (Util.of_option_exn (Json.lookup j "capabilities")));
        tags = (Tags.of_json (Util.of_option_exn (Json.lookup j "tags")));
        changes =
          (Changes.of_json (Util.of_option_exn (Json.lookup j "changes")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module GetStackPolicyInput =
  struct
    type t = {
      stack_name: String.t }
    let make ~stack_name  () = { stack_name }
    let parse xml =
      Some
        {
          stack_name =
            (Xml.required "StackName"
               (Util.option_bind (Xml.member "StackName" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("StackName", (String.to_query v.stack_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("stack_name", (String.to_json v.stack_name))])
    let of_json j =
      {
        stack_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "stack_name")))
      }
  end
module GetStackPolicyOutput =
  struct
    type t = {
      stack_policy_body: String.t option }
    let make ?stack_policy_body  () = { stack_policy_body }
    let parse xml =
      Some
        {
          stack_policy_body =
            (Util.option_bind (Xml.member "StackPolicyBody" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.stack_policy_body
              (fun f -> Query.Pair ("StackPolicyBody", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.stack_policy_body
              (fun f -> ("stack_policy_body", (String.to_json f)))])
    let of_json j =
      {
        stack_policy_body =
          (Util.option_map (Json.lookup j "stack_policy_body") String.of_json)
      }
  end
module ListStackSetsInput =
  struct
    type t =
      {
      next_token: String.t option ;
      max_results: Integer.t option ;
      status: StackSetStatus.t option }
    let make ?next_token  ?max_results  ?status  () =
      { next_token; max_results; status }
    let parse xml =
      Some
        {
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse);
          max_results =
            (Util.option_bind (Xml.member "MaxResults" xml) Integer.parse);
          status =
            (Util.option_bind (Xml.member "Status" xml) StackSetStatus.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.status
              (fun f -> Query.Pair ("Status", (StackSetStatus.to_query f)));
           Util.option_map v.max_results
             (fun f -> Query.Pair ("MaxResults", (Integer.to_query f)));
           Util.option_map v.next_token
             (fun f -> Query.Pair ("NextToken", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.status
              (fun f -> ("status", (StackSetStatus.to_json f)));
           Util.option_map v.max_results
             (fun f -> ("max_results", (Integer.to_json f)));
           Util.option_map v.next_token
             (fun f -> ("next_token", (String.to_json f)))])
    let of_json j =
      {
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        max_results =
          (Util.option_map (Json.lookup j "max_results") Integer.of_json);
        status =
          (Util.option_map (Json.lookup j "status") StackSetStatus.of_json)
      }
  end
module StackInstanceNotFoundException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module OperationNotFoundException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module InsufficientCapabilitiesException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DescribeStackSetOperationInput =
  struct
    type t = {
      stack_set_name: String.t ;
      operation_id: String.t }
    let make ~stack_set_name  ~operation_id  () =
      { stack_set_name; operation_id }
    let parse xml =
      Some
        {
          stack_set_name =
            (Xml.required "StackSetName"
               (Util.option_bind (Xml.member "StackSetName" xml) String.parse));
          operation_id =
            (Xml.required "OperationId"
               (Util.option_bind (Xml.member "OperationId" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("OperationId", (String.to_query v.operation_id)));
           Some
             (Query.Pair ("StackSetName", (String.to_query v.stack_set_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("operation_id", (String.to_json v.operation_id));
           Some ("stack_set_name", (String.to_json v.stack_set_name))])
    let of_json j =
      {
        stack_set_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "stack_set_name")));
        operation_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "operation_id")))
      }
  end
module TypeNotFoundException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ListChangeSetsOutput =
  struct
    type t = {
      summaries: ChangeSetSummaries.t ;
      next_token: String.t option }
    let make ?(summaries= [])  ?next_token  () = { summaries; next_token }
    let parse xml =
      Some
        {
          summaries =
            (Util.of_option []
               (Util.option_bind (Xml.member "Summaries" xml)
                  ChangeSetSummaries.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair
                ("Summaries.member",
                  (ChangeSetSummaries.to_query v.summaries)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some ("summaries", (ChangeSetSummaries.to_json v.summaries))])
    let of_json j =
      {
        summaries =
          (ChangeSetSummaries.of_json
             (Util.of_option_exn (Json.lookup j "summaries")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module ListStackSetOperationsInput =
  struct
    type t =
      {
      stack_set_name: String.t ;
      next_token: String.t option ;
      max_results: Integer.t option }
    let make ~stack_set_name  ?next_token  ?max_results  () =
      { stack_set_name; next_token; max_results }
    let parse xml =
      Some
        {
          stack_set_name =
            (Xml.required "StackSetName"
               (Util.option_bind (Xml.member "StackSetName" xml) String.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse);
          max_results =
            (Util.option_bind (Xml.member "MaxResults" xml) Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.max_results
              (fun f -> Query.Pair ("MaxResults", (Integer.to_query f)));
           Util.option_map v.next_token
             (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair ("StackSetName", (String.to_query v.stack_set_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.max_results
              (fun f -> ("max_results", (Integer.to_json f)));
           Util.option_map v.next_token
             (fun f -> ("next_token", (String.to_json f)));
           Some ("stack_set_name", (String.to_json v.stack_set_name))])
    let of_json j =
      {
        stack_set_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "stack_set_name")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        max_results =
          (Util.option_map (Json.lookup j "max_results") Integer.of_json)
      }
  end
module ListExportsOutput =
  struct
    type t = {
      exports: Exports.t ;
      next_token: String.t option }
    let make ?(exports= [])  ?next_token  () = { exports; next_token }
    let parse xml =
      Some
        {
          exports =
            (Util.of_option []
               (Util.option_bind (Xml.member "Exports" xml) Exports.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some (Query.Pair ("Exports.member", (Exports.to_query v.exports)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some ("exports", (Exports.to_json v.exports))])
    let of_json j =
      {
        exports =
          (Exports.of_json (Util.of_option_exn (Json.lookup j "exports")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module DeleteStackInput =
  struct
    type t =
      {
      stack_name: String.t ;
      retain_resources: RetainResources.t ;
      role_a_r_n: String.t option ;
      client_request_token: String.t option }
    let make ~stack_name  ?(retain_resources= [])  ?role_a_r_n 
      ?client_request_token  () =
      { stack_name; retain_resources; role_a_r_n; client_request_token }
    let parse xml =
      Some
        {
          stack_name =
            (Xml.required "StackName"
               (Util.option_bind (Xml.member "StackName" xml) String.parse));
          retain_resources =
            (Util.of_option []
               (Util.option_bind (Xml.member "RetainResources" xml)
                  RetainResources.parse));
          role_a_r_n =
            (Util.option_bind (Xml.member "RoleARN" xml) String.parse);
          client_request_token =
            (Util.option_bind (Xml.member "ClientRequestToken" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.client_request_token
              (fun f ->
                 Query.Pair ("ClientRequestToken", (String.to_query f)));
           Util.option_map v.role_a_r_n
             (fun f -> Query.Pair ("RoleARN", (String.to_query f)));
           Some
             (Query.Pair
                ("RetainResources.member",
                  (RetainResources.to_query v.retain_resources)));
           Some (Query.Pair ("StackName", (String.to_query v.stack_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.client_request_token
              (fun f -> ("client_request_token", (String.to_json f)));
           Util.option_map v.role_a_r_n
             (fun f -> ("role_a_r_n", (String.to_json f)));
           Some
             ("retain_resources",
               (RetainResources.to_json v.retain_resources));
           Some ("stack_name", (String.to_json v.stack_name))])
    let of_json j =
      {
        stack_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "stack_name")));
        retain_resources =
          (RetainResources.of_json
             (Util.of_option_exn (Json.lookup j "retain_resources")));
        role_a_r_n =
          (Util.option_map (Json.lookup j "role_a_r_n") String.of_json);
        client_request_token =
          (Util.option_map (Json.lookup j "client_request_token")
             String.of_json)
      }
  end
module OperationIdAlreadyExistsException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DeleteStackInstancesInput =
  struct
    type t =
      {
      stack_set_name: String.t ;
      accounts: AccountList.t ;
      deployment_targets: DeploymentTargets.t option ;
      regions: RegionList.t ;
      operation_preferences: StackSetOperationPreferences.t option ;
      retain_stacks: Boolean.t ;
      operation_id: String.t option }
    let make ~stack_set_name  ?(accounts= [])  ?deployment_targets  ~regions 
      ?operation_preferences  ~retain_stacks  ?operation_id  () =
      {
        stack_set_name;
        accounts;
        deployment_targets;
        regions;
        operation_preferences;
        retain_stacks;
        operation_id
      }
    let parse xml =
      Some
        {
          stack_set_name =
            (Xml.required "StackSetName"
               (Util.option_bind (Xml.member "StackSetName" xml) String.parse));
          accounts =
            (Util.of_option []
               (Util.option_bind (Xml.member "Accounts" xml)
                  AccountList.parse));
          deployment_targets =
            (Util.option_bind (Xml.member "DeploymentTargets" xml)
               DeploymentTargets.parse);
          regions =
            (Xml.required "Regions"
               (Util.option_bind (Xml.member "Regions" xml) RegionList.parse));
          operation_preferences =
            (Util.option_bind (Xml.member "OperationPreferences" xml)
               StackSetOperationPreferences.parse);
          retain_stacks =
            (Xml.required "RetainStacks"
               (Util.option_bind (Xml.member "RetainStacks" xml)
                  Boolean.parse));
          operation_id =
            (Util.option_bind (Xml.member "OperationId" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.operation_id
              (fun f -> Query.Pair ("OperationId", (String.to_query f)));
           Some
             (Query.Pair ("RetainStacks", (Boolean.to_query v.retain_stacks)));
           Util.option_map v.operation_preferences
             (fun f ->
                Query.Pair
                  ("OperationPreferences",
                    (StackSetOperationPreferences.to_query f)));
           Some
             (Query.Pair ("Regions.member", (RegionList.to_query v.regions)));
           Util.option_map v.deployment_targets
             (fun f ->
                Query.Pair
                  ("DeploymentTargets", (DeploymentTargets.to_query f)));
           Some
             (Query.Pair
                ("Accounts.member", (AccountList.to_query v.accounts)));
           Some
             (Query.Pair ("StackSetName", (String.to_query v.stack_set_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.operation_id
              (fun f -> ("operation_id", (String.to_json f)));
           Some ("retain_stacks", (Boolean.to_json v.retain_stacks));
           Util.option_map v.operation_preferences
             (fun f ->
                ("operation_preferences",
                  (StackSetOperationPreferences.to_json f)));
           Some ("regions", (RegionList.to_json v.regions));
           Util.option_map v.deployment_targets
             (fun f -> ("deployment_targets", (DeploymentTargets.to_json f)));
           Some ("accounts", (AccountList.to_json v.accounts));
           Some ("stack_set_name", (String.to_json v.stack_set_name))])
    let of_json j =
      {
        stack_set_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "stack_set_name")));
        accounts =
          (AccountList.of_json
             (Util.of_option_exn (Json.lookup j "accounts")));
        deployment_targets =
          (Util.option_map (Json.lookup j "deployment_targets")
             DeploymentTargets.of_json);
        regions =
          (RegionList.of_json (Util.of_option_exn (Json.lookup j "regions")));
        operation_preferences =
          (Util.option_map (Json.lookup j "operation_preferences")
             StackSetOperationPreferences.of_json);
        retain_stacks =
          (Boolean.of_json
             (Util.of_option_exn (Json.lookup j "retain_stacks")));
        operation_id =
          (Util.option_map (Json.lookup j "operation_id") String.of_json)
      }
  end
module DeleteStackSetInput =
  struct
    type t = {
      stack_set_name: String.t }
    let make ~stack_set_name  () = { stack_set_name }
    let parse xml =
      Some
        {
          stack_set_name =
            (Xml.required "StackSetName"
               (Util.option_bind (Xml.member "StackSetName" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("StackSetName", (String.to_query v.stack_set_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("stack_set_name", (String.to_json v.stack_set_name))])
    let of_json j =
      {
        stack_set_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "stack_set_name")))
      }
  end
module CreateChangeSetInput =
  struct
    type t =
      {
      stack_name: String.t ;
      template_body: String.t option ;
      template_u_r_l: String.t option ;
      use_previous_template: Boolean.t option ;
      parameters: Parameters.t ;
      capabilities: Capabilities.t ;
      resource_types: ResourceTypes.t ;
      role_a_r_n: String.t option ;
      rollback_configuration: RollbackConfiguration.t option ;
      notification_a_r_ns: NotificationARNs.t ;
      tags: Tags.t ;
      change_set_name: String.t ;
      client_token: String.t option ;
      description: String.t option ;
      change_set_type: ChangeSetType.t option ;
      resources_to_import: ResourcesToImport.t }
    let make ~stack_name  ?template_body  ?template_u_r_l 
      ?use_previous_template  ?(parameters= [])  ?(capabilities= []) 
      ?(resource_types= [])  ?role_a_r_n  ?rollback_configuration 
      ?(notification_a_r_ns= [])  ?(tags= [])  ~change_set_name 
      ?client_token  ?description  ?change_set_type  ?(resources_to_import=
      [])  () =
      {
        stack_name;
        template_body;
        template_u_r_l;
        use_previous_template;
        parameters;
        capabilities;
        resource_types;
        role_a_r_n;
        rollback_configuration;
        notification_a_r_ns;
        tags;
        change_set_name;
        client_token;
        description;
        change_set_type;
        resources_to_import
      }
    let parse xml =
      Some
        {
          stack_name =
            (Xml.required "StackName"
               (Util.option_bind (Xml.member "StackName" xml) String.parse));
          template_body =
            (Util.option_bind (Xml.member "TemplateBody" xml) String.parse);
          template_u_r_l =
            (Util.option_bind (Xml.member "TemplateURL" xml) String.parse);
          use_previous_template =
            (Util.option_bind (Xml.member "UsePreviousTemplate" xml)
               Boolean.parse);
          parameters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Parameters" xml)
                  Parameters.parse));
          capabilities =
            (Util.of_option []
               (Util.option_bind (Xml.member "Capabilities" xml)
                  Capabilities.parse));
          resource_types =
            (Util.of_option []
               (Util.option_bind (Xml.member "ResourceTypes" xml)
                  ResourceTypes.parse));
          role_a_r_n =
            (Util.option_bind (Xml.member "RoleARN" xml) String.parse);
          rollback_configuration =
            (Util.option_bind (Xml.member "RollbackConfiguration" xml)
               RollbackConfiguration.parse);
          notification_a_r_ns =
            (Util.of_option []
               (Util.option_bind (Xml.member "NotificationARNs" xml)
                  NotificationARNs.parse));
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) Tags.parse));
          change_set_name =
            (Xml.required "ChangeSetName"
               (Util.option_bind (Xml.member "ChangeSetName" xml)
                  String.parse));
          client_token =
            (Util.option_bind (Xml.member "ClientToken" xml) String.parse);
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse);
          change_set_type =
            (Util.option_bind (Xml.member "ChangeSetType" xml)
               ChangeSetType.parse);
          resources_to_import =
            (Util.of_option []
               (Util.option_bind (Xml.member "ResourcesToImport" xml)
                  ResourcesToImport.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ResourcesToImport.member",
                   (ResourcesToImport.to_query v.resources_to_import)));
           Util.option_map v.change_set_type
             (fun f ->
                Query.Pair ("ChangeSetType", (ChangeSetType.to_query f)));
           Util.option_map v.description
             (fun f -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.client_token
             (fun f -> Query.Pair ("ClientToken", (String.to_query f)));
           Some
             (Query.Pair
                ("ChangeSetName", (String.to_query v.change_set_name)));
           Some (Query.Pair ("Tags.member", (Tags.to_query v.tags)));
           Some
             (Query.Pair
                ("NotificationARNs.member",
                  (NotificationARNs.to_query v.notification_a_r_ns)));
           Util.option_map v.rollback_configuration
             (fun f ->
                Query.Pair
                  ("RollbackConfiguration",
                    (RollbackConfiguration.to_query f)));
           Util.option_map v.role_a_r_n
             (fun f -> Query.Pair ("RoleARN", (String.to_query f)));
           Some
             (Query.Pair
                ("ResourceTypes.member",
                  (ResourceTypes.to_query v.resource_types)));
           Some
             (Query.Pair
                ("Capabilities.member",
                  (Capabilities.to_query v.capabilities)));
           Some
             (Query.Pair
                ("Parameters.member", (Parameters.to_query v.parameters)));
           Util.option_map v.use_previous_template
             (fun f ->
                Query.Pair ("UsePreviousTemplate", (Boolean.to_query f)));
           Util.option_map v.template_u_r_l
             (fun f -> Query.Pair ("TemplateURL", (String.to_query f)));
           Util.option_map v.template_body
             (fun f -> Query.Pair ("TemplateBody", (String.to_query f)));
           Some (Query.Pair ("StackName", (String.to_query v.stack_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("resources_to_import",
                (ResourcesToImport.to_json v.resources_to_import));
           Util.option_map v.change_set_type
             (fun f -> ("change_set_type", (ChangeSetType.to_json f)));
           Util.option_map v.description
             (fun f -> ("description", (String.to_json f)));
           Util.option_map v.client_token
             (fun f -> ("client_token", (String.to_json f)));
           Some ("change_set_name", (String.to_json v.change_set_name));
           Some ("tags", (Tags.to_json v.tags));
           Some
             ("notification_a_r_ns",
               (NotificationARNs.to_json v.notification_a_r_ns));
           Util.option_map v.rollback_configuration
             (fun f ->
                ("rollback_configuration", (RollbackConfiguration.to_json f)));
           Util.option_map v.role_a_r_n
             (fun f -> ("role_a_r_n", (String.to_json f)));
           Some ("resource_types", (ResourceTypes.to_json v.resource_types));
           Some ("capabilities", (Capabilities.to_json v.capabilities));
           Some ("parameters", (Parameters.to_json v.parameters));
           Util.option_map v.use_previous_template
             (fun f -> ("use_previous_template", (Boolean.to_json f)));
           Util.option_map v.template_u_r_l
             (fun f -> ("template_u_r_l", (String.to_json f)));
           Util.option_map v.template_body
             (fun f -> ("template_body", (String.to_json f)));
           Some ("stack_name", (String.to_json v.stack_name))])
    let of_json j =
      {
        stack_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "stack_name")));
        template_body =
          (Util.option_map (Json.lookup j "template_body") String.of_json);
        template_u_r_l =
          (Util.option_map (Json.lookup j "template_u_r_l") String.of_json);
        use_previous_template =
          (Util.option_map (Json.lookup j "use_previous_template")
             Boolean.of_json);
        parameters =
          (Parameters.of_json
             (Util.of_option_exn (Json.lookup j "parameters")));
        capabilities =
          (Capabilities.of_json
             (Util.of_option_exn (Json.lookup j "capabilities")));
        resource_types =
          (ResourceTypes.of_json
             (Util.of_option_exn (Json.lookup j "resource_types")));
        role_a_r_n =
          (Util.option_map (Json.lookup j "role_a_r_n") String.of_json);
        rollback_configuration =
          (Util.option_map (Json.lookup j "rollback_configuration")
             RollbackConfiguration.of_json);
        notification_a_r_ns =
          (NotificationARNs.of_json
             (Util.of_option_exn (Json.lookup j "notification_a_r_ns")));
        tags = (Tags.of_json (Util.of_option_exn (Json.lookup j "tags")));
        change_set_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "change_set_name")));
        client_token =
          (Util.option_map (Json.lookup j "client_token") String.of_json);
        description =
          (Util.option_map (Json.lookup j "description") String.of_json);
        change_set_type =
          (Util.option_map (Json.lookup j "change_set_type")
             ChangeSetType.of_json);
        resources_to_import =
          (ResourcesToImport.of_json
             (Util.of_option_exn (Json.lookup j "resources_to_import")))
      }
  end
module AlreadyExistsException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ContinueUpdateRollbackInput =
  struct
    type t =
      {
      stack_name: String.t ;
      role_a_r_n: String.t option ;
      resources_to_skip: ResourcesToSkip.t ;
      client_request_token: String.t option }
    let make ~stack_name  ?role_a_r_n  ?(resources_to_skip= []) 
      ?client_request_token  () =
      { stack_name; role_a_r_n; resources_to_skip; client_request_token }
    let parse xml =
      Some
        {
          stack_name =
            (Xml.required "StackName"
               (Util.option_bind (Xml.member "StackName" xml) String.parse));
          role_a_r_n =
            (Util.option_bind (Xml.member "RoleARN" xml) String.parse);
          resources_to_skip =
            (Util.of_option []
               (Util.option_bind (Xml.member "ResourcesToSkip" xml)
                  ResourcesToSkip.parse));
          client_request_token =
            (Util.option_bind (Xml.member "ClientRequestToken" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.client_request_token
              (fun f ->
                 Query.Pair ("ClientRequestToken", (String.to_query f)));
           Some
             (Query.Pair
                ("ResourcesToSkip.member",
                  (ResourcesToSkip.to_query v.resources_to_skip)));
           Util.option_map v.role_a_r_n
             (fun f -> Query.Pair ("RoleARN", (String.to_query f)));
           Some (Query.Pair ("StackName", (String.to_query v.stack_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.client_request_token
              (fun f -> ("client_request_token", (String.to_json f)));
           Some
             ("resources_to_skip",
               (ResourcesToSkip.to_json v.resources_to_skip));
           Util.option_map v.role_a_r_n
             (fun f -> ("role_a_r_n", (String.to_json f)));
           Some ("stack_name", (String.to_json v.stack_name))])
    let of_json j =
      {
        stack_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "stack_name")));
        role_a_r_n =
          (Util.option_map (Json.lookup j "role_a_r_n") String.of_json);
        resources_to_skip =
          (ResourcesToSkip.of_json
             (Util.of_option_exn (Json.lookup j "resources_to_skip")));
        client_request_token =
          (Util.option_map (Json.lookup j "client_request_token")
             String.of_json)
      }
  end
module CreateStackSetInput =
  struct
    type t =
      {
      stack_set_name: String.t ;
      description: String.t option ;
      template_body: String.t option ;
      template_u_r_l: String.t option ;
      parameters: Parameters.t ;
      capabilities: Capabilities.t ;
      tags: Tags.t ;
      administration_role_a_r_n: String.t option ;
      execution_role_name: String.t option ;
      permission_model: PermissionModels.t option ;
      auto_deployment: AutoDeployment.t option ;
      client_request_token: String.t option }
    let make ~stack_set_name  ?description  ?template_body  ?template_u_r_l 
      ?(parameters= [])  ?(capabilities= [])  ?(tags= []) 
      ?administration_role_a_r_n  ?execution_role_name  ?permission_model 
      ?auto_deployment  ?client_request_token  () =
      {
        stack_set_name;
        description;
        template_body;
        template_u_r_l;
        parameters;
        capabilities;
        tags;
        administration_role_a_r_n;
        execution_role_name;
        permission_model;
        auto_deployment;
        client_request_token
      }
    let parse xml =
      Some
        {
          stack_set_name =
            (Xml.required "StackSetName"
               (Util.option_bind (Xml.member "StackSetName" xml) String.parse));
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse);
          template_body =
            (Util.option_bind (Xml.member "TemplateBody" xml) String.parse);
          template_u_r_l =
            (Util.option_bind (Xml.member "TemplateURL" xml) String.parse);
          parameters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Parameters" xml)
                  Parameters.parse));
          capabilities =
            (Util.of_option []
               (Util.option_bind (Xml.member "Capabilities" xml)
                  Capabilities.parse));
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) Tags.parse));
          administration_role_a_r_n =
            (Util.option_bind (Xml.member "AdministrationRoleARN" xml)
               String.parse);
          execution_role_name =
            (Util.option_bind (Xml.member "ExecutionRoleName" xml)
               String.parse);
          permission_model =
            (Util.option_bind (Xml.member "PermissionModel" xml)
               PermissionModels.parse);
          auto_deployment =
            (Util.option_bind (Xml.member "AutoDeployment" xml)
               AutoDeployment.parse);
          client_request_token =
            (Util.option_bind (Xml.member "ClientRequestToken" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.client_request_token
              (fun f ->
                 Query.Pair ("ClientRequestToken", (String.to_query f)));
           Util.option_map v.auto_deployment
             (fun f ->
                Query.Pair ("AutoDeployment", (AutoDeployment.to_query f)));
           Util.option_map v.permission_model
             (fun f ->
                Query.Pair ("PermissionModel", (PermissionModels.to_query f)));
           Util.option_map v.execution_role_name
             (fun f -> Query.Pair ("ExecutionRoleName", (String.to_query f)));
           Util.option_map v.administration_role_a_r_n
             (fun f ->
                Query.Pair ("AdministrationRoleARN", (String.to_query f)));
           Some (Query.Pair ("Tags.member", (Tags.to_query v.tags)));
           Some
             (Query.Pair
                ("Capabilities.member",
                  (Capabilities.to_query v.capabilities)));
           Some
             (Query.Pair
                ("Parameters.member", (Parameters.to_query v.parameters)));
           Util.option_map v.template_u_r_l
             (fun f -> Query.Pair ("TemplateURL", (String.to_query f)));
           Util.option_map v.template_body
             (fun f -> Query.Pair ("TemplateBody", (String.to_query f)));
           Util.option_map v.description
             (fun f -> Query.Pair ("Description", (String.to_query f)));
           Some
             (Query.Pair ("StackSetName", (String.to_query v.stack_set_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.client_request_token
              (fun f -> ("client_request_token", (String.to_json f)));
           Util.option_map v.auto_deployment
             (fun f -> ("auto_deployment", (AutoDeployment.to_json f)));
           Util.option_map v.permission_model
             (fun f -> ("permission_model", (PermissionModels.to_json f)));
           Util.option_map v.execution_role_name
             (fun f -> ("execution_role_name", (String.to_json f)));
           Util.option_map v.administration_role_a_r_n
             (fun f -> ("administration_role_a_r_n", (String.to_json f)));
           Some ("tags", (Tags.to_json v.tags));
           Some ("capabilities", (Capabilities.to_json v.capabilities));
           Some ("parameters", (Parameters.to_json v.parameters));
           Util.option_map v.template_u_r_l
             (fun f -> ("template_u_r_l", (String.to_json f)));
           Util.option_map v.template_body
             (fun f -> ("template_body", (String.to_json f)));
           Util.option_map v.description
             (fun f -> ("description", (String.to_json f)));
           Some ("stack_set_name", (String.to_json v.stack_set_name))])
    let of_json j =
      {
        stack_set_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "stack_set_name")));
        description =
          (Util.option_map (Json.lookup j "description") String.of_json);
        template_body =
          (Util.option_map (Json.lookup j "template_body") String.of_json);
        template_u_r_l =
          (Util.option_map (Json.lookup j "template_u_r_l") String.of_json);
        parameters =
          (Parameters.of_json
             (Util.of_option_exn (Json.lookup j "parameters")));
        capabilities =
          (Capabilities.of_json
             (Util.of_option_exn (Json.lookup j "capabilities")));
        tags = (Tags.of_json (Util.of_option_exn (Json.lookup j "tags")));
        administration_role_a_r_n =
          (Util.option_map (Json.lookup j "administration_role_a_r_n")
             String.of_json);
        execution_role_name =
          (Util.option_map (Json.lookup j "execution_role_name")
             String.of_json);
        permission_model =
          (Util.option_map (Json.lookup j "permission_model")
             PermissionModels.of_json);
        auto_deployment =
          (Util.option_map (Json.lookup j "auto_deployment")
             AutoDeployment.of_json);
        client_request_token =
          (Util.option_map (Json.lookup j "client_request_token")
             String.of_json)
      }
  end
module ValidateTemplateInput =
  struct
    type t =
      {
      template_body: String.t option ;
      template_u_r_l: String.t option }
    let make ?template_body  ?template_u_r_l  () =
      { template_body; template_u_r_l }
    let parse xml =
      Some
        {
          template_body =
            (Util.option_bind (Xml.member "TemplateBody" xml) String.parse);
          template_u_r_l =
            (Util.option_bind (Xml.member "TemplateURL" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.template_u_r_l
              (fun f -> Query.Pair ("TemplateURL", (String.to_query f)));
           Util.option_map v.template_body
             (fun f -> Query.Pair ("TemplateBody", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.template_u_r_l
              (fun f -> ("template_u_r_l", (String.to_json f)));
           Util.option_map v.template_body
             (fun f -> ("template_body", (String.to_json f)))])
    let of_json j =
      {
        template_body =
          (Util.option_map (Json.lookup j "template_body") String.of_json);
        template_u_r_l =
          (Util.option_map (Json.lookup j "template_u_r_l") String.of_json)
      }
  end
module CreatedButModifiedException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ValidateTemplateOutput =
  struct
    type t =
      {
      parameters: TemplateParameters.t ;
      description: String.t option ;
      capabilities: Capabilities.t ;
      capabilities_reason: String.t option ;
      declared_transforms: TransformsList.t }
    let make ?(parameters= [])  ?description  ?(capabilities= []) 
      ?capabilities_reason  ?(declared_transforms= [])  () =
      {
        parameters;
        description;
        capabilities;
        capabilities_reason;
        declared_transforms
      }
    let parse xml =
      Some
        {
          parameters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Parameters" xml)
                  TemplateParameters.parse));
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse);
          capabilities =
            (Util.of_option []
               (Util.option_bind (Xml.member "Capabilities" xml)
                  Capabilities.parse));
          capabilities_reason =
            (Util.option_bind (Xml.member "CapabilitiesReason" xml)
               String.parse);
          declared_transforms =
            (Util.of_option []
               (Util.option_bind (Xml.member "DeclaredTransforms" xml)
                  TransformsList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("DeclaredTransforms.member",
                   (TransformsList.to_query v.declared_transforms)));
           Util.option_map v.capabilities_reason
             (fun f -> Query.Pair ("CapabilitiesReason", (String.to_query f)));
           Some
             (Query.Pair
                ("Capabilities.member",
                  (Capabilities.to_query v.capabilities)));
           Util.option_map v.description
             (fun f -> Query.Pair ("Description", (String.to_query f)));
           Some
             (Query.Pair
                ("Parameters.member",
                  (TemplateParameters.to_query v.parameters)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("declared_transforms",
                (TransformsList.to_json v.declared_transforms));
           Util.option_map v.capabilities_reason
             (fun f -> ("capabilities_reason", (String.to_json f)));
           Some ("capabilities", (Capabilities.to_json v.capabilities));
           Util.option_map v.description
             (fun f -> ("description", (String.to_json f)));
           Some ("parameters", (TemplateParameters.to_json v.parameters))])
    let of_json j =
      {
        parameters =
          (TemplateParameters.of_json
             (Util.of_option_exn (Json.lookup j "parameters")));
        description =
          (Util.option_map (Json.lookup j "description") String.of_json);
        capabilities =
          (Capabilities.of_json
             (Util.of_option_exn (Json.lookup j "capabilities")));
        capabilities_reason =
          (Util.option_map (Json.lookup j "capabilities_reason")
             String.of_json);
        declared_transforms =
          (TransformsList.of_json
             (Util.of_option_exn (Json.lookup j "declared_transforms")))
      }
  end
module DescribeStackInstanceOutput =
  struct
    type t = {
      stack_instance: StackInstance.t option }
    let make ?stack_instance  () = { stack_instance }
    let parse xml =
      Some
        {
          stack_instance =
            (Util.option_bind (Xml.member "StackInstance" xml)
               StackInstance.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.stack_instance
              (fun f ->
                 Query.Pair ("StackInstance", (StackInstance.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.stack_instance
              (fun f -> ("stack_instance", (StackInstance.to_json f)))])
    let of_json j =
      {
        stack_instance =
          (Util.option_map (Json.lookup j "stack_instance")
             StackInstance.of_json)
      }
  end
module CancelUpdateStackInput =
  struct
    type t = {
      stack_name: String.t ;
      client_request_token: String.t option }
    let make ~stack_name  ?client_request_token  () =
      { stack_name; client_request_token }
    let parse xml =
      Some
        {
          stack_name =
            (Xml.required "StackName"
               (Util.option_bind (Xml.member "StackName" xml) String.parse));
          client_request_token =
            (Util.option_bind (Xml.member "ClientRequestToken" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.client_request_token
              (fun f ->
                 Query.Pair ("ClientRequestToken", (String.to_query f)));
           Some (Query.Pair ("StackName", (String.to_query v.stack_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.client_request_token
              (fun f -> ("client_request_token", (String.to_json f)));
           Some ("stack_name", (String.to_json v.stack_name))])
    let of_json j =
      {
        stack_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "stack_name")));
        client_request_token =
          (Util.option_map (Json.lookup j "client_request_token")
             String.of_json)
      }
  end
module SetTypeDefaultVersionOutput =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ListStackSetsOutput =
  struct
    type t = {
      summaries: StackSetSummaries.t ;
      next_token: String.t option }
    let make ?(summaries= [])  ?next_token  () = { summaries; next_token }
    let parse xml =
      Some
        {
          summaries =
            (Util.of_option []
               (Util.option_bind (Xml.member "Summaries" xml)
                  StackSetSummaries.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair
                ("Summaries.member",
                  (StackSetSummaries.to_query v.summaries)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some ("summaries", (StackSetSummaries.to_json v.summaries))])
    let of_json j =
      {
        summaries =
          (StackSetSummaries.of_json
             (Util.of_option_exn (Json.lookup j "summaries")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module ListTypesOutput =
  struct
    type t = {
      type_summaries: TypeSummaries.t ;
      next_token: String.t option }
    let make ?(type_summaries= [])  ?next_token  () =
      { type_summaries; next_token }
    let parse xml =
      Some
        {
          type_summaries =
            (Util.of_option []
               (Util.option_bind (Xml.member "TypeSummaries" xml)
                  TypeSummaries.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair
                ("TypeSummaries.member",
                  (TypeSummaries.to_query v.type_summaries)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some ("type_summaries", (TypeSummaries.to_json v.type_summaries))])
    let of_json j =
      {
        type_summaries =
          (TypeSummaries.of_json
             (Util.of_option_exn (Json.lookup j "type_summaries")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module DescribeStackResourceOutput =
  struct
    type t = {
      stack_resource_detail: StackResourceDetail.t option }
    let make ?stack_resource_detail  () = { stack_resource_detail }
    let parse xml =
      Some
        {
          stack_resource_detail =
            (Util.option_bind (Xml.member "StackResourceDetail" xml)
               StackResourceDetail.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.stack_resource_detail
              (fun f ->
                 Query.Pair
                   ("StackResourceDetail", (StackResourceDetail.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.stack_resource_detail
              (fun f ->
                 ("stack_resource_detail", (StackResourceDetail.to_json f)))])
    let of_json j =
      {
        stack_resource_detail =
          (Util.option_map (Json.lookup j "stack_resource_detail")
             StackResourceDetail.of_json)
      }
  end
module DescribeStacksInput =
  struct
    type t = {
      stack_name: String.t option ;
      next_token: String.t option }
    let make ?stack_name  ?next_token  () = { stack_name; next_token }
    let parse xml =
      Some
        {
          stack_name =
            (Util.option_bind (Xml.member "StackName" xml) String.parse);
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Util.option_map v.stack_name
             (fun f -> Query.Pair ("StackName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Util.option_map v.stack_name
             (fun f -> ("stack_name", (String.to_json f)))])
    let of_json j =
      {
        stack_name =
          (Util.option_map (Json.lookup j "stack_name") String.of_json);
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module EstimateTemplateCostInput =
  struct
    type t =
      {
      template_body: String.t option ;
      template_u_r_l: String.t option ;
      parameters: Parameters.t }
    let make ?template_body  ?template_u_r_l  ?(parameters= [])  () =
      { template_body; template_u_r_l; parameters }
    let parse xml =
      Some
        {
          template_body =
            (Util.option_bind (Xml.member "TemplateBody" xml) String.parse);
          template_u_r_l =
            (Util.option_bind (Xml.member "TemplateURL" xml) String.parse);
          parameters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Parameters" xml)
                  Parameters.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Parameters.member", (Parameters.to_query v.parameters)));
           Util.option_map v.template_u_r_l
             (fun f -> Query.Pair ("TemplateURL", (String.to_query f)));
           Util.option_map v.template_body
             (fun f -> Query.Pair ("TemplateBody", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("parameters", (Parameters.to_json v.parameters));
           Util.option_map v.template_u_r_l
             (fun f -> ("template_u_r_l", (String.to_json f)));
           Util.option_map v.template_body
             (fun f -> ("template_body", (String.to_json f)))])
    let of_json j =
      {
        template_body =
          (Util.option_map (Json.lookup j "template_body") String.of_json);
        template_u_r_l =
          (Util.option_map (Json.lookup j "template_u_r_l") String.of_json);
        parameters =
          (Parameters.of_json
             (Util.of_option_exn (Json.lookup j "parameters")))
      }
  end
module DescribeTypeRegistrationInput =
  struct
    type t = {
      registration_token: String.t }
    let make ~registration_token  () = { registration_token }
    let parse xml =
      Some
        {
          registration_token =
            (Xml.required "RegistrationToken"
               (Util.option_bind (Xml.member "RegistrationToken" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("RegistrationToken",
                   (String.to_query v.registration_token)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("registration_token", (String.to_json v.registration_token))])
    let of_json j =
      {
        registration_token =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "registration_token")))
      }
  end
module DescribeStackDriftDetectionStatusOutput =
  struct
    type t =
      {
      stack_id: String.t ;
      stack_drift_detection_id: String.t ;
      stack_drift_status: StackDriftStatus.t option ;
      detection_status: StackDriftDetectionStatus.t ;
      detection_status_reason: String.t option ;
      drifted_stack_resource_count: Integer.t option ;
      timestamp: DateTime.t }
    let make ~stack_id  ~stack_drift_detection_id  ?stack_drift_status 
      ~detection_status  ?detection_status_reason 
      ?drifted_stack_resource_count  ~timestamp  () =
      {
        stack_id;
        stack_drift_detection_id;
        stack_drift_status;
        detection_status;
        detection_status_reason;
        drifted_stack_resource_count;
        timestamp
      }
    let parse xml =
      Some
        {
          stack_id =
            (Xml.required "StackId"
               (Util.option_bind (Xml.member "StackId" xml) String.parse));
          stack_drift_detection_id =
            (Xml.required "StackDriftDetectionId"
               (Util.option_bind (Xml.member "StackDriftDetectionId" xml)
                  String.parse));
          stack_drift_status =
            (Util.option_bind (Xml.member "StackDriftStatus" xml)
               StackDriftStatus.parse);
          detection_status =
            (Xml.required "DetectionStatus"
               (Util.option_bind (Xml.member "DetectionStatus" xml)
                  StackDriftDetectionStatus.parse));
          detection_status_reason =
            (Util.option_bind (Xml.member "DetectionStatusReason" xml)
               String.parse);
          drifted_stack_resource_count =
            (Util.option_bind (Xml.member "DriftedStackResourceCount" xml)
               Integer.parse);
          timestamp =
            (Xml.required "Timestamp"
               (Util.option_bind (Xml.member "Timestamp" xml) DateTime.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Timestamp", (DateTime.to_query v.timestamp)));
           Util.option_map v.drifted_stack_resource_count
             (fun f ->
                Query.Pair
                  ("DriftedStackResourceCount", (Integer.to_query f)));
           Util.option_map v.detection_status_reason
             (fun f ->
                Query.Pair ("DetectionStatusReason", (String.to_query f)));
           Some
             (Query.Pair
                ("DetectionStatus",
                  (StackDriftDetectionStatus.to_query v.detection_status)));
           Util.option_map v.stack_drift_status
             (fun f ->
                Query.Pair
                  ("StackDriftStatus", (StackDriftStatus.to_query f)));
           Some
             (Query.Pair
                ("StackDriftDetectionId",
                  (String.to_query v.stack_drift_detection_id)));
           Some (Query.Pair ("StackId", (String.to_query v.stack_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("timestamp", (DateTime.to_json v.timestamp));
           Util.option_map v.drifted_stack_resource_count
             (fun f -> ("drifted_stack_resource_count", (Integer.to_json f)));
           Util.option_map v.detection_status_reason
             (fun f -> ("detection_status_reason", (String.to_json f)));
           Some
             ("detection_status",
               (StackDriftDetectionStatus.to_json v.detection_status));
           Util.option_map v.stack_drift_status
             (fun f -> ("stack_drift_status", (StackDriftStatus.to_json f)));
           Some
             ("stack_drift_detection_id",
               (String.to_json v.stack_drift_detection_id));
           Some ("stack_id", (String.to_json v.stack_id))])
    let of_json j =
      {
        stack_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "stack_id")));
        stack_drift_detection_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "stack_drift_detection_id")));
        stack_drift_status =
          (Util.option_map (Json.lookup j "stack_drift_status")
             StackDriftStatus.of_json);
        detection_status =
          (StackDriftDetectionStatus.of_json
             (Util.of_option_exn (Json.lookup j "detection_status")));
        detection_status_reason =
          (Util.option_map (Json.lookup j "detection_status_reason")
             String.of_json);
        drifted_stack_resource_count =
          (Util.option_map (Json.lookup j "drifted_stack_resource_count")
             Integer.of_json);
        timestamp =
          (DateTime.of_json (Util.of_option_exn (Json.lookup j "timestamp")))
      }
  end
module ListTypeRegistrationsInput =
  struct
    type t =
      {
      type_: RegistryType.t option ;
      type_name: String.t option ;
      type_arn: String.t option ;
      registration_status_filter: RegistrationStatus.t option ;
      max_results: Integer.t option ;
      next_token: String.t option }
    let make ?type_  ?type_name  ?type_arn  ?registration_status_filter 
      ?max_results  ?next_token  () =
      {
        type_;
        type_name;
        type_arn;
        registration_status_filter;
        max_results;
        next_token
      }
    let parse xml =
      Some
        {
          type_ =
            (Util.option_bind (Xml.member "Type" xml) RegistryType.parse);
          type_name =
            (Util.option_bind (Xml.member "TypeName" xml) String.parse);
          type_arn =
            (Util.option_bind (Xml.member "TypeArn" xml) String.parse);
          registration_status_filter =
            (Util.option_bind (Xml.member "RegistrationStatusFilter" xml)
               RegistrationStatus.parse);
          max_results =
            (Util.option_bind (Xml.member "MaxResults" xml) Integer.parse);
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Util.option_map v.max_results
             (fun f -> Query.Pair ("MaxResults", (Integer.to_query f)));
           Util.option_map v.registration_status_filter
             (fun f ->
                Query.Pair
                  ("RegistrationStatusFilter",
                    (RegistrationStatus.to_query f)));
           Util.option_map v.type_arn
             (fun f -> Query.Pair ("TypeArn", (String.to_query f)));
           Util.option_map v.type_name
             (fun f -> Query.Pair ("TypeName", (String.to_query f)));
           Util.option_map v.type_
             (fun f -> Query.Pair ("Type", (RegistryType.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Util.option_map v.max_results
             (fun f -> ("max_results", (Integer.to_json f)));
           Util.option_map v.registration_status_filter
             (fun f ->
                ("registration_status_filter",
                  (RegistrationStatus.to_json f)));
           Util.option_map v.type_arn
             (fun f -> ("type_arn", (String.to_json f)));
           Util.option_map v.type_name
             (fun f -> ("type_name", (String.to_json f)));
           Util.option_map v.type_
             (fun f -> ("type_", (RegistryType.to_json f)))])
    let of_json j =
      {
        type_ =
          (Util.option_map (Json.lookup j "type_") RegistryType.of_json);
        type_name =
          (Util.option_map (Json.lookup j "type_name") String.of_json);
        type_arn =
          (Util.option_map (Json.lookup j "type_arn") String.of_json);
        registration_status_filter =
          (Util.option_map (Json.lookup j "registration_status_filter")
             RegistrationStatus.of_json);
        max_results =
          (Util.option_map (Json.lookup j "max_results") Integer.of_json);
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module CreateStackSetOutput =
  struct
    type t = {
      stack_set_id: String.t option }
    let make ?stack_set_id  () = { stack_set_id }
    let parse xml =
      Some
        {
          stack_set_id =
            (Util.option_bind (Xml.member "StackSetId" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.stack_set_id
              (fun f -> Query.Pair ("StackSetId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.stack_set_id
              (fun f -> ("stack_set_id", (String.to_json f)))])
    let of_json j =
      {
        stack_set_id =
          (Util.option_map (Json.lookup j "stack_set_id") String.of_json)
      }
  end
module DescribeStackSetOperationOutput =
  struct
    type t = {
      stack_set_operation: StackSetOperation.t option }
    let make ?stack_set_operation  () = { stack_set_operation }
    let parse xml =
      Some
        {
          stack_set_operation =
            (Util.option_bind (Xml.member "StackSetOperation" xml)
               StackSetOperation.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.stack_set_operation
              (fun f ->
                 Query.Pair
                   ("StackSetOperation", (StackSetOperation.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.stack_set_operation
              (fun f ->
                 ("stack_set_operation", (StackSetOperation.to_json f)))])
    let of_json j =
      {
        stack_set_operation =
          (Util.option_map (Json.lookup j "stack_set_operation")
             StackSetOperation.of_json)
      }
  end
module ContinueUpdateRollbackOutput =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DescribeStackEventsOutput =
  struct
    type t = {
      stack_events: StackEvents.t ;
      next_token: String.t option }
    let make ?(stack_events= [])  ?next_token  () =
      { stack_events; next_token }
    let parse xml =
      Some
        {
          stack_events =
            (Util.of_option []
               (Util.option_bind (Xml.member "StackEvents" xml)
                  StackEvents.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair
                ("StackEvents.member", (StackEvents.to_query v.stack_events)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some ("stack_events", (StackEvents.to_json v.stack_events))])
    let of_json j =
      {
        stack_events =
          (StackEvents.of_json
             (Util.of_option_exn (Json.lookup j "stack_events")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module DescribeStackSetOutput =
  struct
    type t = {
      stack_set: StackSet.t option }
    let make ?stack_set  () = { stack_set }
    let parse xml =
      Some
        {
          stack_set =
            (Util.option_bind (Xml.member "StackSet" xml) StackSet.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.stack_set
              (fun f -> Query.Pair ("StackSet", (StackSet.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.stack_set
              (fun f -> ("stack_set", (StackSet.to_json f)))])
    let of_json j =
      {
        stack_set =
          (Util.option_map (Json.lookup j "stack_set") StackSet.of_json)
      }
  end
module DetectStackDriftInput =
  struct
    type t =
      {
      stack_name: String.t ;
      logical_resource_ids: LogicalResourceIds.t }
    let make ~stack_name  ?(logical_resource_ids= [])  () =
      { stack_name; logical_resource_ids }
    let parse xml =
      Some
        {
          stack_name =
            (Xml.required "StackName"
               (Util.option_bind (Xml.member "StackName" xml) String.parse));
          logical_resource_ids =
            (Util.of_option []
               (Util.option_bind (Xml.member "LogicalResourceIds" xml)
                  LogicalResourceIds.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("LogicalResourceIds.member",
                   (LogicalResourceIds.to_query v.logical_resource_ids)));
           Some (Query.Pair ("StackName", (String.to_query v.stack_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("logical_resource_ids",
                (LogicalResourceIds.to_json v.logical_resource_ids));
           Some ("stack_name", (String.to_json v.stack_name))])
    let of_json j =
      {
        stack_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "stack_name")));
        logical_resource_ids =
          (LogicalResourceIds.of_json
             (Util.of_option_exn (Json.lookup j "logical_resource_ids")))
      }
  end
module ListTypesInput =
  struct
    type t =
      {
      visibility: Visibility.t option ;
      provisioning_type: ProvisioningType.t option ;
      deprecated_status: DeprecatedStatus.t option ;
      max_results: Integer.t option ;
      next_token: String.t option }
    let make ?visibility  ?provisioning_type  ?deprecated_status 
      ?max_results  ?next_token  () =
      {
        visibility;
        provisioning_type;
        deprecated_status;
        max_results;
        next_token
      }
    let parse xml =
      Some
        {
          visibility =
            (Util.option_bind (Xml.member "Visibility" xml) Visibility.parse);
          provisioning_type =
            (Util.option_bind (Xml.member "ProvisioningType" xml)
               ProvisioningType.parse);
          deprecated_status =
            (Util.option_bind (Xml.member "DeprecatedStatus" xml)
               DeprecatedStatus.parse);
          max_results =
            (Util.option_bind (Xml.member "MaxResults" xml) Integer.parse);
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Util.option_map v.max_results
             (fun f -> Query.Pair ("MaxResults", (Integer.to_query f)));
           Util.option_map v.deprecated_status
             (fun f ->
                Query.Pair
                  ("DeprecatedStatus", (DeprecatedStatus.to_query f)));
           Util.option_map v.provisioning_type
             (fun f ->
                Query.Pair
                  ("ProvisioningType", (ProvisioningType.to_query f)));
           Util.option_map v.visibility
             (fun f -> Query.Pair ("Visibility", (Visibility.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Util.option_map v.max_results
             (fun f -> ("max_results", (Integer.to_json f)));
           Util.option_map v.deprecated_status
             (fun f -> ("deprecated_status", (DeprecatedStatus.to_json f)));
           Util.option_map v.provisioning_type
             (fun f -> ("provisioning_type", (ProvisioningType.to_json f)));
           Util.option_map v.visibility
             (fun f -> ("visibility", (Visibility.to_json f)))])
    let of_json j =
      {
        visibility =
          (Util.option_map (Json.lookup j "visibility") Visibility.of_json);
        provisioning_type =
          (Util.option_map (Json.lookup j "provisioning_type")
             ProvisioningType.of_json);
        deprecated_status =
          (Util.option_map (Json.lookup j "deprecated_status")
             DeprecatedStatus.of_json);
        max_results =
          (Util.option_map (Json.lookup j "max_results") Integer.of_json);
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module ListTypeVersionsOutput =
  struct
    type t =
      {
      type_version_summaries: TypeVersionSummaries.t ;
      next_token: String.t option }
    let make ?(type_version_summaries= [])  ?next_token  () =
      { type_version_summaries; next_token }
    let parse xml =
      Some
        {
          type_version_summaries =
            (Util.of_option []
               (Util.option_bind (Xml.member "TypeVersionSummaries" xml)
                  TypeVersionSummaries.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair
                ("TypeVersionSummaries.member",
                  (TypeVersionSummaries.to_query v.type_version_summaries)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some
             ("type_version_summaries",
               (TypeVersionSummaries.to_json v.type_version_summaries))])
    let of_json j =
      {
        type_version_summaries =
          (TypeVersionSummaries.of_json
             (Util.of_option_exn (Json.lookup j "type_version_summaries")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module DeleteChangeSetOutput =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ListImportsOutput =
  struct
    type t = {
      imports: Imports.t ;
      next_token: String.t option }
    let make ?(imports= [])  ?next_token  () = { imports; next_token }
    let parse xml =
      Some
        {
          imports =
            (Util.of_option []
               (Util.option_bind (Xml.member "Imports" xml) Imports.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some (Query.Pair ("Imports.member", (Imports.to_query v.imports)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some ("imports", (Imports.to_json v.imports))])
    let of_json j =
      {
        imports =
          (Imports.of_json (Util.of_option_exn (Json.lookup j "imports")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module StopStackSetOperationInput =
  struct
    type t = {
      stack_set_name: String.t ;
      operation_id: String.t }
    let make ~stack_set_name  ~operation_id  () =
      { stack_set_name; operation_id }
    let parse xml =
      Some
        {
          stack_set_name =
            (Xml.required "StackSetName"
               (Util.option_bind (Xml.member "StackSetName" xml) String.parse));
          operation_id =
            (Xml.required "OperationId"
               (Util.option_bind (Xml.member "OperationId" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("OperationId", (String.to_query v.operation_id)));
           Some
             (Query.Pair ("StackSetName", (String.to_query v.stack_set_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("operation_id", (String.to_json v.operation_id));
           Some ("stack_set_name", (String.to_json v.stack_set_name))])
    let of_json j =
      {
        stack_set_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "stack_set_name")));
        operation_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "operation_id")))
      }
  end
module UpdateStackInstancesInput =
  struct
    type t =
      {
      stack_set_name: String.t ;
      accounts: AccountList.t ;
      deployment_targets: DeploymentTargets.t option ;
      regions: RegionList.t ;
      parameter_overrides: Parameters.t ;
      operation_preferences: StackSetOperationPreferences.t option ;
      operation_id: String.t option }
    let make ~stack_set_name  ?(accounts= [])  ?deployment_targets  ~regions 
      ?(parameter_overrides= [])  ?operation_preferences  ?operation_id  () =
      {
        stack_set_name;
        accounts;
        deployment_targets;
        regions;
        parameter_overrides;
        operation_preferences;
        operation_id
      }
    let parse xml =
      Some
        {
          stack_set_name =
            (Xml.required "StackSetName"
               (Util.option_bind (Xml.member "StackSetName" xml) String.parse));
          accounts =
            (Util.of_option []
               (Util.option_bind (Xml.member "Accounts" xml)
                  AccountList.parse));
          deployment_targets =
            (Util.option_bind (Xml.member "DeploymentTargets" xml)
               DeploymentTargets.parse);
          regions =
            (Xml.required "Regions"
               (Util.option_bind (Xml.member "Regions" xml) RegionList.parse));
          parameter_overrides =
            (Util.of_option []
               (Util.option_bind (Xml.member "ParameterOverrides" xml)
                  Parameters.parse));
          operation_preferences =
            (Util.option_bind (Xml.member "OperationPreferences" xml)
               StackSetOperationPreferences.parse);
          operation_id =
            (Util.option_bind (Xml.member "OperationId" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.operation_id
              (fun f -> Query.Pair ("OperationId", (String.to_query f)));
           Util.option_map v.operation_preferences
             (fun f ->
                Query.Pair
                  ("OperationPreferences",
                    (StackSetOperationPreferences.to_query f)));
           Some
             (Query.Pair
                ("ParameterOverrides.member",
                  (Parameters.to_query v.parameter_overrides)));
           Some
             (Query.Pair ("Regions.member", (RegionList.to_query v.regions)));
           Util.option_map v.deployment_targets
             (fun f ->
                Query.Pair
                  ("DeploymentTargets", (DeploymentTargets.to_query f)));
           Some
             (Query.Pair
                ("Accounts.member", (AccountList.to_query v.accounts)));
           Some
             (Query.Pair ("StackSetName", (String.to_query v.stack_set_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.operation_id
              (fun f -> ("operation_id", (String.to_json f)));
           Util.option_map v.operation_preferences
             (fun f ->
                ("operation_preferences",
                  (StackSetOperationPreferences.to_json f)));
           Some
             ("parameter_overrides",
               (Parameters.to_json v.parameter_overrides));
           Some ("regions", (RegionList.to_json v.regions));
           Util.option_map v.deployment_targets
             (fun f -> ("deployment_targets", (DeploymentTargets.to_json f)));
           Some ("accounts", (AccountList.to_json v.accounts));
           Some ("stack_set_name", (String.to_json v.stack_set_name))])
    let of_json j =
      {
        stack_set_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "stack_set_name")));
        accounts =
          (AccountList.of_json
             (Util.of_option_exn (Json.lookup j "accounts")));
        deployment_targets =
          (Util.option_map (Json.lookup j "deployment_targets")
             DeploymentTargets.of_json);
        regions =
          (RegionList.of_json (Util.of_option_exn (Json.lookup j "regions")));
        parameter_overrides =
          (Parameters.of_json
             (Util.of_option_exn (Json.lookup j "parameter_overrides")));
        operation_preferences =
          (Util.option_map (Json.lookup j "operation_preferences")
             StackSetOperationPreferences.of_json);
        operation_id =
          (Util.option_map (Json.lookup j "operation_id") String.of_json)
      }
  end
module ChangeSetNotFoundException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module InvalidOperationException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module RegisterTypeOutput =
  struct
    type t = {
      registration_token: String.t option }
    let make ?registration_token  () = { registration_token }
    let parse xml =
      Some
        {
          registration_token =
            (Util.option_bind (Xml.member "RegistrationToken" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.registration_token
              (fun f -> Query.Pair ("RegistrationToken", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.registration_token
              (fun f -> ("registration_token", (String.to_json f)))])
    let of_json j =
      {
        registration_token =
          (Util.option_map (Json.lookup j "registration_token")
             String.of_json)
      }
  end
module ListStackSetOperationsOutput =
  struct
    type t =
      {
      summaries: StackSetOperationSummaries.t ;
      next_token: String.t option }
    let make ?(summaries= [])  ?next_token  () = { summaries; next_token }
    let parse xml =
      Some
        {
          summaries =
            (Util.of_option []
               (Util.option_bind (Xml.member "Summaries" xml)
                  StackSetOperationSummaries.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair
                ("Summaries.member",
                  (StackSetOperationSummaries.to_query v.summaries)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some
             ("summaries", (StackSetOperationSummaries.to_json v.summaries))])
    let of_json j =
      {
        summaries =
          (StackSetOperationSummaries.of_json
             (Util.of_option_exn (Json.lookup j "summaries")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module ListTypeVersionsInput =
  struct
    type t =
      {
      type_: RegistryType.t option ;
      type_name: String.t option ;
      arn: String.t option ;
      max_results: Integer.t option ;
      next_token: String.t option ;
      deprecated_status: DeprecatedStatus.t option }
    let make ?type_  ?type_name  ?arn  ?max_results  ?next_token 
      ?deprecated_status  () =
      { type_; type_name; arn; max_results; next_token; deprecated_status }
    let parse xml =
      Some
        {
          type_ =
            (Util.option_bind (Xml.member "Type" xml) RegistryType.parse);
          type_name =
            (Util.option_bind (Xml.member "TypeName" xml) String.parse);
          arn = (Util.option_bind (Xml.member "Arn" xml) String.parse);
          max_results =
            (Util.option_bind (Xml.member "MaxResults" xml) Integer.parse);
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse);
          deprecated_status =
            (Util.option_bind (Xml.member "DeprecatedStatus" xml)
               DeprecatedStatus.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.deprecated_status
              (fun f ->
                 Query.Pair
                   ("DeprecatedStatus", (DeprecatedStatus.to_query f)));
           Util.option_map v.next_token
             (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Util.option_map v.max_results
             (fun f -> Query.Pair ("MaxResults", (Integer.to_query f)));
           Util.option_map v.arn
             (fun f -> Query.Pair ("Arn", (String.to_query f)));
           Util.option_map v.type_name
             (fun f -> Query.Pair ("TypeName", (String.to_query f)));
           Util.option_map v.type_
             (fun f -> Query.Pair ("Type", (RegistryType.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.deprecated_status
              (fun f -> ("deprecated_status", (DeprecatedStatus.to_json f)));
           Util.option_map v.next_token
             (fun f -> ("next_token", (String.to_json f)));
           Util.option_map v.max_results
             (fun f -> ("max_results", (Integer.to_json f)));
           Util.option_map v.arn (fun f -> ("arn", (String.to_json f)));
           Util.option_map v.type_name
             (fun f -> ("type_name", (String.to_json f)));
           Util.option_map v.type_
             (fun f -> ("type_", (RegistryType.to_json f)))])
    let of_json j =
      {
        type_ =
          (Util.option_map (Json.lookup j "type_") RegistryType.of_json);
        type_name =
          (Util.option_map (Json.lookup j "type_name") String.of_json);
        arn = (Util.option_map (Json.lookup j "arn") String.of_json);
        max_results =
          (Util.option_map (Json.lookup j "max_results") Integer.of_json);
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        deprecated_status =
          (Util.option_map (Json.lookup j "deprecated_status")
             DeprecatedStatus.of_json)
      }
  end
module ListImportsInput =
  struct
    type t = {
      export_name: String.t ;
      next_token: String.t option }
    let make ~export_name  ?next_token  () = { export_name; next_token }
    let parse xml =
      Some
        {
          export_name =
            (Xml.required "ExportName"
               (Util.option_bind (Xml.member "ExportName" xml) String.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some (Query.Pair ("ExportName", (String.to_query v.export_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some ("export_name", (String.to_json v.export_name))])
    let of_json j =
      {
        export_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "export_name")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module ListStackResourcesOutput =
  struct
    type t =
      {
      stack_resource_summaries: StackResourceSummaries.t ;
      next_token: String.t option }
    let make ?(stack_resource_summaries= [])  ?next_token  () =
      { stack_resource_summaries; next_token }
    let parse xml =
      Some
        {
          stack_resource_summaries =
            (Util.of_option []
               (Util.option_bind (Xml.member "StackResourceSummaries" xml)
                  StackResourceSummaries.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair
                ("StackResourceSummaries.member",
                  (StackResourceSummaries.to_query v.stack_resource_summaries)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some
             ("stack_resource_summaries",
               (StackResourceSummaries.to_json v.stack_resource_summaries))])
    let of_json j =
      {
        stack_resource_summaries =
          (StackResourceSummaries.of_json
             (Util.of_option_exn (Json.lookup j "stack_resource_summaries")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module ListStackSetOperationResultsInput =
  struct
    type t =
      {
      stack_set_name: String.t ;
      operation_id: String.t ;
      next_token: String.t option ;
      max_results: Integer.t option }
    let make ~stack_set_name  ~operation_id  ?next_token  ?max_results  () =
      { stack_set_name; operation_id; next_token; max_results }
    let parse xml =
      Some
        {
          stack_set_name =
            (Xml.required "StackSetName"
               (Util.option_bind (Xml.member "StackSetName" xml) String.parse));
          operation_id =
            (Xml.required "OperationId"
               (Util.option_bind (Xml.member "OperationId" xml) String.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse);
          max_results =
            (Util.option_bind (Xml.member "MaxResults" xml) Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.max_results
              (fun f -> Query.Pair ("MaxResults", (Integer.to_query f)));
           Util.option_map v.next_token
             (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair ("OperationId", (String.to_query v.operation_id)));
           Some
             (Query.Pair ("StackSetName", (String.to_query v.stack_set_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.max_results
              (fun f -> ("max_results", (Integer.to_json f)));
           Util.option_map v.next_token
             (fun f -> ("next_token", (String.to_json f)));
           Some ("operation_id", (String.to_json v.operation_id));
           Some ("stack_set_name", (String.to_json v.stack_set_name))])
    let of_json j =
      {
        stack_set_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "stack_set_name")));
        operation_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "operation_id")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        max_results =
          (Util.option_map (Json.lookup j "max_results") Integer.of_json)
      }
  end
module SetStackPolicyInput =
  struct
    type t =
      {
      stack_name: String.t ;
      stack_policy_body: String.t option ;
      stack_policy_u_r_l: String.t option }
    let make ~stack_name  ?stack_policy_body  ?stack_policy_u_r_l  () =
      { stack_name; stack_policy_body; stack_policy_u_r_l }
    let parse xml =
      Some
        {
          stack_name =
            (Xml.required "StackName"
               (Util.option_bind (Xml.member "StackName" xml) String.parse));
          stack_policy_body =
            (Util.option_bind (Xml.member "StackPolicyBody" xml) String.parse);
          stack_policy_u_r_l =
            (Util.option_bind (Xml.member "StackPolicyURL" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.stack_policy_u_r_l
              (fun f -> Query.Pair ("StackPolicyURL", (String.to_query f)));
           Util.option_map v.stack_policy_body
             (fun f -> Query.Pair ("StackPolicyBody", (String.to_query f)));
           Some (Query.Pair ("StackName", (String.to_query v.stack_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.stack_policy_u_r_l
              (fun f -> ("stack_policy_u_r_l", (String.to_json f)));
           Util.option_map v.stack_policy_body
             (fun f -> ("stack_policy_body", (String.to_json f)));
           Some ("stack_name", (String.to_json v.stack_name))])
    let of_json j =
      {
        stack_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "stack_name")));
        stack_policy_body =
          (Util.option_map (Json.lookup j "stack_policy_body") String.of_json);
        stack_policy_u_r_l =
          (Util.option_map (Json.lookup j "stack_policy_u_r_l")
             String.of_json)
      }
  end
module DescribeTypeRegistrationOutput =
  struct
    type t =
      {
      progress_status: RegistrationStatus.t option ;
      description: String.t option ;
      type_arn: String.t option ;
      type_version_arn: String.t option }
    let make ?progress_status  ?description  ?type_arn  ?type_version_arn  ()
      = { progress_status; description; type_arn; type_version_arn }
    let parse xml =
      Some
        {
          progress_status =
            (Util.option_bind (Xml.member "ProgressStatus" xml)
               RegistrationStatus.parse);
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse);
          type_arn =
            (Util.option_bind (Xml.member "TypeArn" xml) String.parse);
          type_version_arn =
            (Util.option_bind (Xml.member "TypeVersionArn" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.type_version_arn
              (fun f -> Query.Pair ("TypeVersionArn", (String.to_query f)));
           Util.option_map v.type_arn
             (fun f -> Query.Pair ("TypeArn", (String.to_query f)));
           Util.option_map v.description
             (fun f -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.progress_status
             (fun f ->
                Query.Pair
                  ("ProgressStatus", (RegistrationStatus.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.type_version_arn
              (fun f -> ("type_version_arn", (String.to_json f)));
           Util.option_map v.type_arn
             (fun f -> ("type_arn", (String.to_json f)));
           Util.option_map v.description
             (fun f -> ("description", (String.to_json f)));
           Util.option_map v.progress_status
             (fun f -> ("progress_status", (RegistrationStatus.to_json f)))])
    let of_json j =
      {
        progress_status =
          (Util.option_map (Json.lookup j "progress_status")
             RegistrationStatus.of_json);
        description =
          (Util.option_map (Json.lookup j "description") String.of_json);
        type_arn =
          (Util.option_map (Json.lookup j "type_arn") String.of_json);
        type_version_arn =
          (Util.option_map (Json.lookup j "type_version_arn") String.of_json)
      }
  end
module RecordHandlerProgressInput =
  struct
    type t =
      {
      bearer_token: String.t ;
      operation_status: OperationStatus.t ;
      current_operation_status: OperationStatus.t option ;
      status_message: String.t option ;
      error_code: HandlerErrorCode.t option ;
      resource_model: String.t option ;
      client_request_token: String.t option }
    let make ~bearer_token  ~operation_status  ?current_operation_status 
      ?status_message  ?error_code  ?resource_model  ?client_request_token 
      () =
      {
        bearer_token;
        operation_status;
        current_operation_status;
        status_message;
        error_code;
        resource_model;
        client_request_token
      }
    let parse xml =
      Some
        {
          bearer_token =
            (Xml.required "BearerToken"
               (Util.option_bind (Xml.member "BearerToken" xml) String.parse));
          operation_status =
            (Xml.required "OperationStatus"
               (Util.option_bind (Xml.member "OperationStatus" xml)
                  OperationStatus.parse));
          current_operation_status =
            (Util.option_bind (Xml.member "CurrentOperationStatus" xml)
               OperationStatus.parse);
          status_message =
            (Util.option_bind (Xml.member "StatusMessage" xml) String.parse);
          error_code =
            (Util.option_bind (Xml.member "ErrorCode" xml)
               HandlerErrorCode.parse);
          resource_model =
            (Util.option_bind (Xml.member "ResourceModel" xml) String.parse);
          client_request_token =
            (Util.option_bind (Xml.member "ClientRequestToken" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.client_request_token
              (fun f ->
                 Query.Pair ("ClientRequestToken", (String.to_query f)));
           Util.option_map v.resource_model
             (fun f -> Query.Pair ("ResourceModel", (String.to_query f)));
           Util.option_map v.error_code
             (fun f ->
                Query.Pair ("ErrorCode", (HandlerErrorCode.to_query f)));
           Util.option_map v.status_message
             (fun f -> Query.Pair ("StatusMessage", (String.to_query f)));
           Util.option_map v.current_operation_status
             (fun f ->
                Query.Pair
                  ("CurrentOperationStatus", (OperationStatus.to_query f)));
           Some
             (Query.Pair
                ("OperationStatus",
                  (OperationStatus.to_query v.operation_status)));
           Some
             (Query.Pair ("BearerToken", (String.to_query v.bearer_token)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.client_request_token
              (fun f -> ("client_request_token", (String.to_json f)));
           Util.option_map v.resource_model
             (fun f -> ("resource_model", (String.to_json f)));
           Util.option_map v.error_code
             (fun f -> ("error_code", (HandlerErrorCode.to_json f)));
           Util.option_map v.status_message
             (fun f -> ("status_message", (String.to_json f)));
           Util.option_map v.current_operation_status
             (fun f ->
                ("current_operation_status", (OperationStatus.to_json f)));
           Some
             ("operation_status",
               (OperationStatus.to_json v.operation_status));
           Some ("bearer_token", (String.to_json v.bearer_token))])
    let of_json j =
      {
        bearer_token =
          (String.of_json (Util.of_option_exn (Json.lookup j "bearer_token")));
        operation_status =
          (OperationStatus.of_json
             (Util.of_option_exn (Json.lookup j "operation_status")));
        current_operation_status =
          (Util.option_map (Json.lookup j "current_operation_status")
             OperationStatus.of_json);
        status_message =
          (Util.option_map (Json.lookup j "status_message") String.of_json);
        error_code =
          (Util.option_map (Json.lookup j "error_code")
             HandlerErrorCode.of_json);
        resource_model =
          (Util.option_map (Json.lookup j "resource_model") String.of_json);
        client_request_token =
          (Util.option_map (Json.lookup j "client_request_token")
             String.of_json)
      }
  end
module DetectStackSetDriftInput =
  struct
    type t =
      {
      stack_set_name: String.t ;
      operation_preferences: StackSetOperationPreferences.t option ;
      operation_id: String.t option }
    let make ~stack_set_name  ?operation_preferences  ?operation_id  () =
      { stack_set_name; operation_preferences; operation_id }
    let parse xml =
      Some
        {
          stack_set_name =
            (Xml.required "StackSetName"
               (Util.option_bind (Xml.member "StackSetName" xml) String.parse));
          operation_preferences =
            (Util.option_bind (Xml.member "OperationPreferences" xml)
               StackSetOperationPreferences.parse);
          operation_id =
            (Util.option_bind (Xml.member "OperationId" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.operation_id
              (fun f -> Query.Pair ("OperationId", (String.to_query f)));
           Util.option_map v.operation_preferences
             (fun f ->
                Query.Pair
                  ("OperationPreferences",
                    (StackSetOperationPreferences.to_query f)));
           Some
             (Query.Pair ("StackSetName", (String.to_query v.stack_set_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.operation_id
              (fun f -> ("operation_id", (String.to_json f)));
           Util.option_map v.operation_preferences
             (fun f ->
                ("operation_preferences",
                  (StackSetOperationPreferences.to_json f)));
           Some ("stack_set_name", (String.to_json v.stack_set_name))])
    let of_json j =
      {
        stack_set_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "stack_set_name")));
        operation_preferences =
          (Util.option_map (Json.lookup j "operation_preferences")
             StackSetOperationPreferences.of_json);
        operation_id =
          (Util.option_map (Json.lookup j "operation_id") String.of_json)
      }
  end
module OperationInProgressException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DescribeTypeInput =
  struct
    type t =
      {
      type_: RegistryType.t option ;
      type_name: String.t option ;
      arn: String.t option ;
      version_id: String.t option }
    let make ?type_  ?type_name  ?arn  ?version_id  () =
      { type_; type_name; arn; version_id }
    let parse xml =
      Some
        {
          type_ =
            (Util.option_bind (Xml.member "Type" xml) RegistryType.parse);
          type_name =
            (Util.option_bind (Xml.member "TypeName" xml) String.parse);
          arn = (Util.option_bind (Xml.member "Arn" xml) String.parse);
          version_id =
            (Util.option_bind (Xml.member "VersionId" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.version_id
              (fun f -> Query.Pair ("VersionId", (String.to_query f)));
           Util.option_map v.arn
             (fun f -> Query.Pair ("Arn", (String.to_query f)));
           Util.option_map v.type_name
             (fun f -> Query.Pair ("TypeName", (String.to_query f)));
           Util.option_map v.type_
             (fun f -> Query.Pair ("Type", (RegistryType.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.version_id
              (fun f -> ("version_id", (String.to_json f)));
           Util.option_map v.arn (fun f -> ("arn", (String.to_json f)));
           Util.option_map v.type_name
             (fun f -> ("type_name", (String.to_json f)));
           Util.option_map v.type_
             (fun f -> ("type_", (RegistryType.to_json f)))])
    let of_json j =
      {
        type_ =
          (Util.option_map (Json.lookup j "type_") RegistryType.of_json);
        type_name =
          (Util.option_map (Json.lookup j "type_name") String.of_json);
        arn = (Util.option_map (Json.lookup j "arn") String.of_json);
        version_id =
          (Util.option_map (Json.lookup j "version_id") String.of_json)
      }
  end
module StopStackSetOperationOutput =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module EstimateTemplateCostOutput =
  struct
    type t = {
      url: String.t option }
    let make ?url  () = { url }
    let parse xml =
      Some { url = (Util.option_bind (Xml.member "Url" xml) String.parse) }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.url
              (fun f -> Query.Pair ("Url", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.url (fun f -> ("url", (String.to_json f)))])
    let of_json j =
      { url = (Util.option_map (Json.lookup j "url") String.of_json) }
  end
module ListExportsInput =
  struct
    type t = {
      next_token: String.t option }
    let make ?next_token  () = { next_token }
    let parse xml =
      Some
        {
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)))])
    let of_json j =
      {
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module TokenAlreadyExistsException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DescribeTypeOutput =
  struct
    type t =
      {
      arn: String.t option ;
      type_: RegistryType.t option ;
      type_name: String.t option ;
      default_version_id: String.t option ;
      is_default_version: Boolean.t option ;
      description: String.t option ;
      schema: String.t option ;
      provisioning_type: ProvisioningType.t option ;
      deprecated_status: DeprecatedStatus.t option ;
      logging_config: LoggingConfig.t option ;
      execution_role_arn: String.t option ;
      visibility: Visibility.t option ;
      source_url: String.t option ;
      documentation_url: String.t option ;
      last_updated: DateTime.t option ;
      time_created: DateTime.t option }
    let make ?arn  ?type_  ?type_name  ?default_version_id 
      ?is_default_version  ?description  ?schema  ?provisioning_type 
      ?deprecated_status  ?logging_config  ?execution_role_arn  ?visibility 
      ?source_url  ?documentation_url  ?last_updated  ?time_created  () =
      {
        arn;
        type_;
        type_name;
        default_version_id;
        is_default_version;
        description;
        schema;
        provisioning_type;
        deprecated_status;
        logging_config;
        execution_role_arn;
        visibility;
        source_url;
        documentation_url;
        last_updated;
        time_created
      }
    let parse xml =
      Some
        {
          arn = (Util.option_bind (Xml.member "Arn" xml) String.parse);
          type_ =
            (Util.option_bind (Xml.member "Type" xml) RegistryType.parse);
          type_name =
            (Util.option_bind (Xml.member "TypeName" xml) String.parse);
          default_version_id =
            (Util.option_bind (Xml.member "DefaultVersionId" xml)
               String.parse);
          is_default_version =
            (Util.option_bind (Xml.member "IsDefaultVersion" xml)
               Boolean.parse);
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse);
          schema = (Util.option_bind (Xml.member "Schema" xml) String.parse);
          provisioning_type =
            (Util.option_bind (Xml.member "ProvisioningType" xml)
               ProvisioningType.parse);
          deprecated_status =
            (Util.option_bind (Xml.member "DeprecatedStatus" xml)
               DeprecatedStatus.parse);
          logging_config =
            (Util.option_bind (Xml.member "LoggingConfig" xml)
               LoggingConfig.parse);
          execution_role_arn =
            (Util.option_bind (Xml.member "ExecutionRoleArn" xml)
               String.parse);
          visibility =
            (Util.option_bind (Xml.member "Visibility" xml) Visibility.parse);
          source_url =
            (Util.option_bind (Xml.member "SourceUrl" xml) String.parse);
          documentation_url =
            (Util.option_bind (Xml.member "DocumentationUrl" xml)
               String.parse);
          last_updated =
            (Util.option_bind (Xml.member "LastUpdated" xml) DateTime.parse);
          time_created =
            (Util.option_bind (Xml.member "TimeCreated" xml) DateTime.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.time_created
              (fun f -> Query.Pair ("TimeCreated", (DateTime.to_query f)));
           Util.option_map v.last_updated
             (fun f -> Query.Pair ("LastUpdated", (DateTime.to_query f)));
           Util.option_map v.documentation_url
             (fun f -> Query.Pair ("DocumentationUrl", (String.to_query f)));
           Util.option_map v.source_url
             (fun f -> Query.Pair ("SourceUrl", (String.to_query f)));
           Util.option_map v.visibility
             (fun f -> Query.Pair ("Visibility", (Visibility.to_query f)));
           Util.option_map v.execution_role_arn
             (fun f -> Query.Pair ("ExecutionRoleArn", (String.to_query f)));
           Util.option_map v.logging_config
             (fun f ->
                Query.Pair ("LoggingConfig", (LoggingConfig.to_query f)));
           Util.option_map v.deprecated_status
             (fun f ->
                Query.Pair
                  ("DeprecatedStatus", (DeprecatedStatus.to_query f)));
           Util.option_map v.provisioning_type
             (fun f ->
                Query.Pair
                  ("ProvisioningType", (ProvisioningType.to_query f)));
           Util.option_map v.schema
             (fun f -> Query.Pair ("Schema", (String.to_query f)));
           Util.option_map v.description
             (fun f -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.is_default_version
             (fun f -> Query.Pair ("IsDefaultVersion", (Boolean.to_query f)));
           Util.option_map v.default_version_id
             (fun f -> Query.Pair ("DefaultVersionId", (String.to_query f)));
           Util.option_map v.type_name
             (fun f -> Query.Pair ("TypeName", (String.to_query f)));
           Util.option_map v.type_
             (fun f -> Query.Pair ("Type", (RegistryType.to_query f)));
           Util.option_map v.arn
             (fun f -> Query.Pair ("Arn", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.time_created
              (fun f -> ("time_created", (DateTime.to_json f)));
           Util.option_map v.last_updated
             (fun f -> ("last_updated", (DateTime.to_json f)));
           Util.option_map v.documentation_url
             (fun f -> ("documentation_url", (String.to_json f)));
           Util.option_map v.source_url
             (fun f -> ("source_url", (String.to_json f)));
           Util.option_map v.visibility
             (fun f -> ("visibility", (Visibility.to_json f)));
           Util.option_map v.execution_role_arn
             (fun f -> ("execution_role_arn", (String.to_json f)));
           Util.option_map v.logging_config
             (fun f -> ("logging_config", (LoggingConfig.to_json f)));
           Util.option_map v.deprecated_status
             (fun f -> ("deprecated_status", (DeprecatedStatus.to_json f)));
           Util.option_map v.provisioning_type
             (fun f -> ("provisioning_type", (ProvisioningType.to_json f)));
           Util.option_map v.schema (fun f -> ("schema", (String.to_json f)));
           Util.option_map v.description
             (fun f -> ("description", (String.to_json f)));
           Util.option_map v.is_default_version
             (fun f -> ("is_default_version", (Boolean.to_json f)));
           Util.option_map v.default_version_id
             (fun f -> ("default_version_id", (String.to_json f)));
           Util.option_map v.type_name
             (fun f -> ("type_name", (String.to_json f)));
           Util.option_map v.type_
             (fun f -> ("type_", (RegistryType.to_json f)));
           Util.option_map v.arn (fun f -> ("arn", (String.to_json f)))])
    let of_json j =
      {
        arn = (Util.option_map (Json.lookup j "arn") String.of_json);
        type_ =
          (Util.option_map (Json.lookup j "type_") RegistryType.of_json);
        type_name =
          (Util.option_map (Json.lookup j "type_name") String.of_json);
        default_version_id =
          (Util.option_map (Json.lookup j "default_version_id")
             String.of_json);
        is_default_version =
          (Util.option_map (Json.lookup j "is_default_version")
             Boolean.of_json);
        description =
          (Util.option_map (Json.lookup j "description") String.of_json);
        schema = (Util.option_map (Json.lookup j "schema") String.of_json);
        provisioning_type =
          (Util.option_map (Json.lookup j "provisioning_type")
             ProvisioningType.of_json);
        deprecated_status =
          (Util.option_map (Json.lookup j "deprecated_status")
             DeprecatedStatus.of_json);
        logging_config =
          (Util.option_map (Json.lookup j "logging_config")
             LoggingConfig.of_json);
        execution_role_arn =
          (Util.option_map (Json.lookup j "execution_role_arn")
             String.of_json);
        visibility =
          (Util.option_map (Json.lookup j "visibility") Visibility.of_json);
        source_url =
          (Util.option_map (Json.lookup j "source_url") String.of_json);
        documentation_url =
          (Util.option_map (Json.lookup j "documentation_url") String.of_json);
        last_updated =
          (Util.option_map (Json.lookup j "last_updated") DateTime.of_json);
        time_created =
          (Util.option_map (Json.lookup j "time_created") DateTime.of_json)
      }
  end
module DescribeChangeSetInput =
  struct
    type t =
      {
      change_set_name: String.t ;
      stack_name: String.t option ;
      next_token: String.t option }
    let make ~change_set_name  ?stack_name  ?next_token  () =
      { change_set_name; stack_name; next_token }
    let parse xml =
      Some
        {
          change_set_name =
            (Xml.required "ChangeSetName"
               (Util.option_bind (Xml.member "ChangeSetName" xml)
                  String.parse));
          stack_name =
            (Util.option_bind (Xml.member "StackName" xml) String.parse);
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Util.option_map v.stack_name
             (fun f -> Query.Pair ("StackName", (String.to_query f)));
           Some
             (Query.Pair
                ("ChangeSetName", (String.to_query v.change_set_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Util.option_map v.stack_name
             (fun f -> ("stack_name", (String.to_json f)));
           Some ("change_set_name", (String.to_json v.change_set_name))])
    let of_json j =
      {
        change_set_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "change_set_name")));
        stack_name =
          (Util.option_map (Json.lookup j "stack_name") String.of_json);
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module OperationStatusCheckFailedException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DeregisterTypeOutput =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ListStackInstancesInput =
  struct
    type t =
      {
      stack_set_name: String.t ;
      next_token: String.t option ;
      max_results: Integer.t option ;
      filters: StackInstanceFilters.t ;
      stack_instance_account: String.t option ;
      stack_instance_region: String.t option }
    let make ~stack_set_name  ?next_token  ?max_results  ?(filters= []) 
      ?stack_instance_account  ?stack_instance_region  () =
      {
        stack_set_name;
        next_token;
        max_results;
        filters;
        stack_instance_account;
        stack_instance_region
      }
    let parse xml =
      Some
        {
          stack_set_name =
            (Xml.required "StackSetName"
               (Util.option_bind (Xml.member "StackSetName" xml) String.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse);
          max_results =
            (Util.option_bind (Xml.member "MaxResults" xml) Integer.parse);
          filters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Filters" xml)
                  StackInstanceFilters.parse));
          stack_instance_account =
            (Util.option_bind (Xml.member "StackInstanceAccount" xml)
               String.parse);
          stack_instance_region =
            (Util.option_bind (Xml.member "StackInstanceRegion" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.stack_instance_region
              (fun f ->
                 Query.Pair ("StackInstanceRegion", (String.to_query f)));
           Util.option_map v.stack_instance_account
             (fun f ->
                Query.Pair ("StackInstanceAccount", (String.to_query f)));
           Some
             (Query.Pair
                ("Filters.member", (StackInstanceFilters.to_query v.filters)));
           Util.option_map v.max_results
             (fun f -> Query.Pair ("MaxResults", (Integer.to_query f)));
           Util.option_map v.next_token
             (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair ("StackSetName", (String.to_query v.stack_set_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.stack_instance_region
              (fun f -> ("stack_instance_region", (String.to_json f)));
           Util.option_map v.stack_instance_account
             (fun f -> ("stack_instance_account", (String.to_json f)));
           Some ("filters", (StackInstanceFilters.to_json v.filters));
           Util.option_map v.max_results
             (fun f -> ("max_results", (Integer.to_json f)));
           Util.option_map v.next_token
             (fun f -> ("next_token", (String.to_json f)));
           Some ("stack_set_name", (String.to_json v.stack_set_name))])
    let of_json j =
      {
        stack_set_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "stack_set_name")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        max_results =
          (Util.option_map (Json.lookup j "max_results") Integer.of_json);
        filters =
          (StackInstanceFilters.of_json
             (Util.of_option_exn (Json.lookup j "filters")));
        stack_instance_account =
          (Util.option_map (Json.lookup j "stack_instance_account")
             String.of_json);
        stack_instance_region =
          (Util.option_map (Json.lookup j "stack_instance_region")
             String.of_json)
      }
  end
module RegisterTypeInput =
  struct
    type t =
      {
      type_: RegistryType.t option ;
      type_name: String.t ;
      schema_handler_package: String.t ;
      logging_config: LoggingConfig.t option ;
      execution_role_arn: String.t option ;
      client_request_token: String.t option }
    let make ?type_  ~type_name  ~schema_handler_package  ?logging_config 
      ?execution_role_arn  ?client_request_token  () =
      {
        type_;
        type_name;
        schema_handler_package;
        logging_config;
        execution_role_arn;
        client_request_token
      }
    let parse xml =
      Some
        {
          type_ =
            (Util.option_bind (Xml.member "Type" xml) RegistryType.parse);
          type_name =
            (Xml.required "TypeName"
               (Util.option_bind (Xml.member "TypeName" xml) String.parse));
          schema_handler_package =
            (Xml.required "SchemaHandlerPackage"
               (Util.option_bind (Xml.member "SchemaHandlerPackage" xml)
                  String.parse));
          logging_config =
            (Util.option_bind (Xml.member "LoggingConfig" xml)
               LoggingConfig.parse);
          execution_role_arn =
            (Util.option_bind (Xml.member "ExecutionRoleArn" xml)
               String.parse);
          client_request_token =
            (Util.option_bind (Xml.member "ClientRequestToken" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.client_request_token
              (fun f ->
                 Query.Pair ("ClientRequestToken", (String.to_query f)));
           Util.option_map v.execution_role_arn
             (fun f -> Query.Pair ("ExecutionRoleArn", (String.to_query f)));
           Util.option_map v.logging_config
             (fun f ->
                Query.Pair ("LoggingConfig", (LoggingConfig.to_query f)));
           Some
             (Query.Pair
                ("SchemaHandlerPackage",
                  (String.to_query v.schema_handler_package)));
           Some (Query.Pair ("TypeName", (String.to_query v.type_name)));
           Util.option_map v.type_
             (fun f -> Query.Pair ("Type", (RegistryType.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.client_request_token
              (fun f -> ("client_request_token", (String.to_json f)));
           Util.option_map v.execution_role_arn
             (fun f -> ("execution_role_arn", (String.to_json f)));
           Util.option_map v.logging_config
             (fun f -> ("logging_config", (LoggingConfig.to_json f)));
           Some
             ("schema_handler_package",
               (String.to_json v.schema_handler_package));
           Some ("type_name", (String.to_json v.type_name));
           Util.option_map v.type_
             (fun f -> ("type_", (RegistryType.to_json f)))])
    let of_json j =
      {
        type_ =
          (Util.option_map (Json.lookup j "type_") RegistryType.of_json);
        type_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "type_name")));
        schema_handler_package =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "schema_handler_package")));
        logging_config =
          (Util.option_map (Json.lookup j "logging_config")
             LoggingConfig.of_json);
        execution_role_arn =
          (Util.option_map (Json.lookup j "execution_role_arn")
             String.of_json);
        client_request_token =
          (Util.option_map (Json.lookup j "client_request_token")
             String.of_json)
      }
  end
module DescribeStackSetInput =
  struct
    type t = {
      stack_set_name: String.t }
    let make ~stack_set_name  () = { stack_set_name }
    let parse xml =
      Some
        {
          stack_set_name =
            (Xml.required "StackSetName"
               (Util.option_bind (Xml.member "StackSetName" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("StackSetName", (String.to_query v.stack_set_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("stack_set_name", (String.to_json v.stack_set_name))])
    let of_json j =
      {
        stack_set_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "stack_set_name")))
      }
  end
module DescribeStackResourcesInput =
  struct
    type t =
      {
      stack_name: String.t option ;
      logical_resource_id: String.t option ;
      physical_resource_id: String.t option }
    let make ?stack_name  ?logical_resource_id  ?physical_resource_id  () =
      { stack_name; logical_resource_id; physical_resource_id }
    let parse xml =
      Some
        {
          stack_name =
            (Util.option_bind (Xml.member "StackName" xml) String.parse);
          logical_resource_id =
            (Util.option_bind (Xml.member "LogicalResourceId" xml)
               String.parse);
          physical_resource_id =
            (Util.option_bind (Xml.member "PhysicalResourceId" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.physical_resource_id
              (fun f ->
                 Query.Pair ("PhysicalResourceId", (String.to_query f)));
           Util.option_map v.logical_resource_id
             (fun f -> Query.Pair ("LogicalResourceId", (String.to_query f)));
           Util.option_map v.stack_name
             (fun f -> Query.Pair ("StackName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.physical_resource_id
              (fun f -> ("physical_resource_id", (String.to_json f)));
           Util.option_map v.logical_resource_id
             (fun f -> ("logical_resource_id", (String.to_json f)));
           Util.option_map v.stack_name
             (fun f -> ("stack_name", (String.to_json f)))])
    let of_json j =
      {
        stack_name =
          (Util.option_map (Json.lookup j "stack_name") String.of_json);
        logical_resource_id =
          (Util.option_map (Json.lookup j "logical_resource_id")
             String.of_json);
        physical_resource_id =
          (Util.option_map (Json.lookup j "physical_resource_id")
             String.of_json)
      }
  end
module DeleteChangeSetInput =
  struct
    type t = {
      change_set_name: String.t ;
      stack_name: String.t option }
    let make ~change_set_name  ?stack_name  () =
      { change_set_name; stack_name }
    let parse xml =
      Some
        {
          change_set_name =
            (Xml.required "ChangeSetName"
               (Util.option_bind (Xml.member "ChangeSetName" xml)
                  String.parse));
          stack_name =
            (Util.option_bind (Xml.member "StackName" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.stack_name
              (fun f -> Query.Pair ("StackName", (String.to_query f)));
           Some
             (Query.Pair
                ("ChangeSetName", (String.to_query v.change_set_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.stack_name
              (fun f -> ("stack_name", (String.to_json f)));
           Some ("change_set_name", (String.to_json v.change_set_name))])
    let of_json j =
      {
        change_set_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "change_set_name")));
        stack_name =
          (Util.option_map (Json.lookup j "stack_name") String.of_json)
      }
  end
module DetectStackResourceDriftInput =
  struct
    type t = {
      stack_name: String.t ;
      logical_resource_id: String.t }
    let make ~stack_name  ~logical_resource_id  () =
      { stack_name; logical_resource_id }
    let parse xml =
      Some
        {
          stack_name =
            (Xml.required "StackName"
               (Util.option_bind (Xml.member "StackName" xml) String.parse));
          logical_resource_id =
            (Xml.required "LogicalResourceId"
               (Util.option_bind (Xml.member "LogicalResourceId" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("LogicalResourceId",
                   (String.to_query v.logical_resource_id)));
           Some (Query.Pair ("StackName", (String.to_query v.stack_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("logical_resource_id", (String.to_json v.logical_resource_id));
           Some ("stack_name", (String.to_json v.stack_name))])
    let of_json j =
      {
        stack_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "stack_name")));
        logical_resource_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "logical_resource_id")))
      }
  end
module DetectStackDriftOutput =
  struct
    type t = {
      stack_drift_detection_id: String.t }
    let make ~stack_drift_detection_id  () = { stack_drift_detection_id }
    let parse xml =
      Some
        {
          stack_drift_detection_id =
            (Xml.required "StackDriftDetectionId"
               (Util.option_bind (Xml.member "StackDriftDetectionId" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("StackDriftDetectionId",
                   (String.to_query v.stack_drift_detection_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("stack_drift_detection_id",
                (String.to_json v.stack_drift_detection_id))])
    let of_json j =
      {
        stack_drift_detection_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "stack_drift_detection_id")))
      }
  end
module SignalResourceInput =
  struct
    type t =
      {
      stack_name: String.t ;
      logical_resource_id: String.t ;
      unique_id: String.t ;
      status: ResourceSignalStatus.t }
    let make ~stack_name  ~logical_resource_id  ~unique_id  ~status  () =
      { stack_name; logical_resource_id; unique_id; status }
    let parse xml =
      Some
        {
          stack_name =
            (Xml.required "StackName"
               (Util.option_bind (Xml.member "StackName" xml) String.parse));
          logical_resource_id =
            (Xml.required "LogicalResourceId"
               (Util.option_bind (Xml.member "LogicalResourceId" xml)
                  String.parse));
          unique_id =
            (Xml.required "UniqueId"
               (Util.option_bind (Xml.member "UniqueId" xml) String.parse));
          status =
            (Xml.required "Status"
               (Util.option_bind (Xml.member "Status" xml)
                  ResourceSignalStatus.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Status", (ResourceSignalStatus.to_query v.status)));
           Some (Query.Pair ("UniqueId", (String.to_query v.unique_id)));
           Some
             (Query.Pair
                ("LogicalResourceId",
                  (String.to_query v.logical_resource_id)));
           Some (Query.Pair ("StackName", (String.to_query v.stack_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("status", (ResourceSignalStatus.to_json v.status));
           Some ("unique_id", (String.to_json v.unique_id));
           Some
             ("logical_resource_id", (String.to_json v.logical_resource_id));
           Some ("stack_name", (String.to_json v.stack_name))])
    let of_json j =
      {
        stack_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "stack_name")));
        logical_resource_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "logical_resource_id")));
        unique_id =
          (String.of_json (Util.of_option_exn (Json.lookup j "unique_id")));
        status =
          (ResourceSignalStatus.of_json
             (Util.of_option_exn (Json.lookup j "status")))
      }
  end
module UpdateStackInstancesOutput =
  struct
    type t = {
      operation_id: String.t option }
    let make ?operation_id  () = { operation_id }
    let parse xml =
      Some
        {
          operation_id =
            (Util.option_bind (Xml.member "OperationId" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.operation_id
              (fun f -> Query.Pair ("OperationId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.operation_id
              (fun f -> ("operation_id", (String.to_json f)))])
    let of_json j =
      {
        operation_id =
          (Util.option_map (Json.lookup j "operation_id") String.of_json)
      }
  end
module GetTemplateSummaryOutput =
  struct
    type t =
      {
      parameters: ParameterDeclarations.t ;
      description: String.t option ;
      capabilities: Capabilities.t ;
      capabilities_reason: String.t option ;
      resource_types: ResourceTypes.t ;
      version: String.t option ;
      metadata: String.t option ;
      declared_transforms: TransformsList.t ;
      resource_identifier_summaries: ResourceIdentifierSummaries.t }
    let make ?(parameters= [])  ?description  ?(capabilities= []) 
      ?capabilities_reason  ?(resource_types= [])  ?version  ?metadata 
      ?(declared_transforms= [])  ?(resource_identifier_summaries= [])  () =
      {
        parameters;
        description;
        capabilities;
        capabilities_reason;
        resource_types;
        version;
        metadata;
        declared_transforms;
        resource_identifier_summaries
      }
    let parse xml =
      Some
        {
          parameters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Parameters" xml)
                  ParameterDeclarations.parse));
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse);
          capabilities =
            (Util.of_option []
               (Util.option_bind (Xml.member "Capabilities" xml)
                  Capabilities.parse));
          capabilities_reason =
            (Util.option_bind (Xml.member "CapabilitiesReason" xml)
               String.parse);
          resource_types =
            (Util.of_option []
               (Util.option_bind (Xml.member "ResourceTypes" xml)
                  ResourceTypes.parse));
          version =
            (Util.option_bind (Xml.member "Version" xml) String.parse);
          metadata =
            (Util.option_bind (Xml.member "Metadata" xml) String.parse);
          declared_transforms =
            (Util.of_option []
               (Util.option_bind (Xml.member "DeclaredTransforms" xml)
                  TransformsList.parse));
          resource_identifier_summaries =
            (Util.of_option []
               (Util.option_bind
                  (Xml.member "ResourceIdentifierSummaries" xml)
                  ResourceIdentifierSummaries.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ResourceIdentifierSummaries.member",
                   (ResourceIdentifierSummaries.to_query
                      v.resource_identifier_summaries)));
           Some
             (Query.Pair
                ("DeclaredTransforms.member",
                  (TransformsList.to_query v.declared_transforms)));
           Util.option_map v.metadata
             (fun f -> Query.Pair ("Metadata", (String.to_query f)));
           Util.option_map v.version
             (fun f -> Query.Pair ("Version", (String.to_query f)));
           Some
             (Query.Pair
                ("ResourceTypes.member",
                  (ResourceTypes.to_query v.resource_types)));
           Util.option_map v.capabilities_reason
             (fun f -> Query.Pair ("CapabilitiesReason", (String.to_query f)));
           Some
             (Query.Pair
                ("Capabilities.member",
                  (Capabilities.to_query v.capabilities)));
           Util.option_map v.description
             (fun f -> Query.Pair ("Description", (String.to_query f)));
           Some
             (Query.Pair
                ("Parameters.member",
                  (ParameterDeclarations.to_query v.parameters)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("resource_identifier_summaries",
                (ResourceIdentifierSummaries.to_json
                   v.resource_identifier_summaries));
           Some
             ("declared_transforms",
               (TransformsList.to_json v.declared_transforms));
           Util.option_map v.metadata
             (fun f -> ("metadata", (String.to_json f)));
           Util.option_map v.version
             (fun f -> ("version", (String.to_json f)));
           Some ("resource_types", (ResourceTypes.to_json v.resource_types));
           Util.option_map v.capabilities_reason
             (fun f -> ("capabilities_reason", (String.to_json f)));
           Some ("capabilities", (Capabilities.to_json v.capabilities));
           Util.option_map v.description
             (fun f -> ("description", (String.to_json f)));
           Some ("parameters", (ParameterDeclarations.to_json v.parameters))])
    let of_json j =
      {
        parameters =
          (ParameterDeclarations.of_json
             (Util.of_option_exn (Json.lookup j "parameters")));
        description =
          (Util.option_map (Json.lookup j "description") String.of_json);
        capabilities =
          (Capabilities.of_json
             (Util.of_option_exn (Json.lookup j "capabilities")));
        capabilities_reason =
          (Util.option_map (Json.lookup j "capabilities_reason")
             String.of_json);
        resource_types =
          (ResourceTypes.of_json
             (Util.of_option_exn (Json.lookup j "resource_types")));
        version = (Util.option_map (Json.lookup j "version") String.of_json);
        metadata =
          (Util.option_map (Json.lookup j "metadata") String.of_json);
        declared_transforms =
          (TransformsList.of_json
             (Util.of_option_exn (Json.lookup j "declared_transforms")));
        resource_identifier_summaries =
          (ResourceIdentifierSummaries.of_json
             (Util.of_option_exn
                (Json.lookup j "resource_identifier_summaries")))
      }
  end
module StackSetNotFoundException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DeleteStackSetOutput =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DeregisterTypeInput =
  struct
    type t =
      {
      arn: String.t option ;
      type_: RegistryType.t option ;
      type_name: String.t option ;
      version_id: String.t option }
    let make ?arn  ?type_  ?type_name  ?version_id  () =
      { arn; type_; type_name; version_id }
    let parse xml =
      Some
        {
          arn = (Util.option_bind (Xml.member "Arn" xml) String.parse);
          type_ =
            (Util.option_bind (Xml.member "Type" xml) RegistryType.parse);
          type_name =
            (Util.option_bind (Xml.member "TypeName" xml) String.parse);
          version_id =
            (Util.option_bind (Xml.member "VersionId" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.version_id
              (fun f -> Query.Pair ("VersionId", (String.to_query f)));
           Util.option_map v.type_name
             (fun f -> Query.Pair ("TypeName", (String.to_query f)));
           Util.option_map v.type_
             (fun f -> Query.Pair ("Type", (RegistryType.to_query f)));
           Util.option_map v.arn
             (fun f -> Query.Pair ("Arn", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.version_id
              (fun f -> ("version_id", (String.to_json f)));
           Util.option_map v.type_name
             (fun f -> ("type_name", (String.to_json f)));
           Util.option_map v.type_
             (fun f -> ("type_", (RegistryType.to_json f)));
           Util.option_map v.arn (fun f -> ("arn", (String.to_json f)))])
    let of_json j =
      {
        arn = (Util.option_map (Json.lookup j "arn") String.of_json);
        type_ =
          (Util.option_map (Json.lookup j "type_") RegistryType.of_json);
        type_name =
          (Util.option_map (Json.lookup j "type_name") String.of_json);
        version_id =
          (Util.option_map (Json.lookup j "version_id") String.of_json)
      }
  end
module ExecuteChangeSetOutput =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DescribeStackEventsInput =
  struct
    type t = {
      stack_name: String.t option ;
      next_token: String.t option }
    let make ?stack_name  ?next_token  () = { stack_name; next_token }
    let parse xml =
      Some
        {
          stack_name =
            (Util.option_bind (Xml.member "StackName" xml) String.parse);
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Util.option_map v.stack_name
             (fun f -> Query.Pair ("StackName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Util.option_map v.stack_name
             (fun f -> ("stack_name", (String.to_json f)))])
    let of_json j =
      {
        stack_name =
          (Util.option_map (Json.lookup j "stack_name") String.of_json);
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module SetTypeDefaultVersionInput =
  struct
    type t =
      {
      arn: String.t option ;
      type_: RegistryType.t option ;
      type_name: String.t option ;
      version_id: String.t option }
    let make ?arn  ?type_  ?type_name  ?version_id  () =
      { arn; type_; type_name; version_id }
    let parse xml =
      Some
        {
          arn = (Util.option_bind (Xml.member "Arn" xml) String.parse);
          type_ =
            (Util.option_bind (Xml.member "Type" xml) RegistryType.parse);
          type_name =
            (Util.option_bind (Xml.member "TypeName" xml) String.parse);
          version_id =
            (Util.option_bind (Xml.member "VersionId" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.version_id
              (fun f -> Query.Pair ("VersionId", (String.to_query f)));
           Util.option_map v.type_name
             (fun f -> Query.Pair ("TypeName", (String.to_query f)));
           Util.option_map v.type_
             (fun f -> Query.Pair ("Type", (RegistryType.to_query f)));
           Util.option_map v.arn
             (fun f -> Query.Pair ("Arn", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.version_id
              (fun f -> ("version_id", (String.to_json f)));
           Util.option_map v.type_name
             (fun f -> ("type_name", (String.to_json f)));
           Util.option_map v.type_
             (fun f -> ("type_", (RegistryType.to_json f)));
           Util.option_map v.arn (fun f -> ("arn", (String.to_json f)))])
    let of_json j =
      {
        arn = (Util.option_map (Json.lookup j "arn") String.of_json);
        type_ =
          (Util.option_map (Json.lookup j "type_") RegistryType.of_json);
        type_name =
          (Util.option_map (Json.lookup j "type_name") String.of_json);
        version_id =
          (Util.option_map (Json.lookup j "version_id") String.of_json)
      }
  end
module StackSetNotEmptyException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module UpdateTerminationProtectionOutput =
  struct
    type t = {
      stack_id: String.t option }
    let make ?stack_id  () = { stack_id }
    let parse xml =
      Some
        {
          stack_id =
            (Util.option_bind (Xml.member "StackId" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.stack_id
              (fun f -> Query.Pair ("StackId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.stack_id
              (fun f -> ("stack_id", (String.to_json f)))])
    let of_json j =
      {
        stack_id =
          (Util.option_map (Json.lookup j "stack_id") String.of_json)
      }
  end
module InvalidStateTransitionException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module UpdateStackSetInput =
  struct
    type t =
      {
      stack_set_name: String.t ;
      description: String.t option ;
      template_body: String.t option ;
      template_u_r_l: String.t option ;
      use_previous_template: Boolean.t option ;
      parameters: Parameters.t ;
      capabilities: Capabilities.t ;
      tags: Tags.t ;
      operation_preferences: StackSetOperationPreferences.t option ;
      administration_role_a_r_n: String.t option ;
      execution_role_name: String.t option ;
      deployment_targets: DeploymentTargets.t option ;
      permission_model: PermissionModels.t option ;
      auto_deployment: AutoDeployment.t option ;
      operation_id: String.t option ;
      accounts: AccountList.t ;
      regions: RegionList.t }
    let make ~stack_set_name  ?description  ?template_body  ?template_u_r_l 
      ?use_previous_template  ?(parameters= [])  ?(capabilities= [])  ?(tags=
      [])  ?operation_preferences  ?administration_role_a_r_n 
      ?execution_role_name  ?deployment_targets  ?permission_model 
      ?auto_deployment  ?operation_id  ?(accounts= [])  ?(regions= [])  () =
      {
        stack_set_name;
        description;
        template_body;
        template_u_r_l;
        use_previous_template;
        parameters;
        capabilities;
        tags;
        operation_preferences;
        administration_role_a_r_n;
        execution_role_name;
        deployment_targets;
        permission_model;
        auto_deployment;
        operation_id;
        accounts;
        regions
      }
    let parse xml =
      Some
        {
          stack_set_name =
            (Xml.required "StackSetName"
               (Util.option_bind (Xml.member "StackSetName" xml) String.parse));
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse);
          template_body =
            (Util.option_bind (Xml.member "TemplateBody" xml) String.parse);
          template_u_r_l =
            (Util.option_bind (Xml.member "TemplateURL" xml) String.parse);
          use_previous_template =
            (Util.option_bind (Xml.member "UsePreviousTemplate" xml)
               Boolean.parse);
          parameters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Parameters" xml)
                  Parameters.parse));
          capabilities =
            (Util.of_option []
               (Util.option_bind (Xml.member "Capabilities" xml)
                  Capabilities.parse));
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) Tags.parse));
          operation_preferences =
            (Util.option_bind (Xml.member "OperationPreferences" xml)
               StackSetOperationPreferences.parse);
          administration_role_a_r_n =
            (Util.option_bind (Xml.member "AdministrationRoleARN" xml)
               String.parse);
          execution_role_name =
            (Util.option_bind (Xml.member "ExecutionRoleName" xml)
               String.parse);
          deployment_targets =
            (Util.option_bind (Xml.member "DeploymentTargets" xml)
               DeploymentTargets.parse);
          permission_model =
            (Util.option_bind (Xml.member "PermissionModel" xml)
               PermissionModels.parse);
          auto_deployment =
            (Util.option_bind (Xml.member "AutoDeployment" xml)
               AutoDeployment.parse);
          operation_id =
            (Util.option_bind (Xml.member "OperationId" xml) String.parse);
          accounts =
            (Util.of_option []
               (Util.option_bind (Xml.member "Accounts" xml)
                  AccountList.parse));
          regions =
            (Util.of_option []
               (Util.option_bind (Xml.member "Regions" xml) RegionList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("Regions.member", (RegionList.to_query v.regions)));
           Some
             (Query.Pair
                ("Accounts.member", (AccountList.to_query v.accounts)));
           Util.option_map v.operation_id
             (fun f -> Query.Pair ("OperationId", (String.to_query f)));
           Util.option_map v.auto_deployment
             (fun f ->
                Query.Pair ("AutoDeployment", (AutoDeployment.to_query f)));
           Util.option_map v.permission_model
             (fun f ->
                Query.Pair ("PermissionModel", (PermissionModels.to_query f)));
           Util.option_map v.deployment_targets
             (fun f ->
                Query.Pair
                  ("DeploymentTargets", (DeploymentTargets.to_query f)));
           Util.option_map v.execution_role_name
             (fun f -> Query.Pair ("ExecutionRoleName", (String.to_query f)));
           Util.option_map v.administration_role_a_r_n
             (fun f ->
                Query.Pair ("AdministrationRoleARN", (String.to_query f)));
           Util.option_map v.operation_preferences
             (fun f ->
                Query.Pair
                  ("OperationPreferences",
                    (StackSetOperationPreferences.to_query f)));
           Some (Query.Pair ("Tags.member", (Tags.to_query v.tags)));
           Some
             (Query.Pair
                ("Capabilities.member",
                  (Capabilities.to_query v.capabilities)));
           Some
             (Query.Pair
                ("Parameters.member", (Parameters.to_query v.parameters)));
           Util.option_map v.use_previous_template
             (fun f ->
                Query.Pair ("UsePreviousTemplate", (Boolean.to_query f)));
           Util.option_map v.template_u_r_l
             (fun f -> Query.Pair ("TemplateURL", (String.to_query f)));
           Util.option_map v.template_body
             (fun f -> Query.Pair ("TemplateBody", (String.to_query f)));
           Util.option_map v.description
             (fun f -> Query.Pair ("Description", (String.to_query f)));
           Some
             (Query.Pair ("StackSetName", (String.to_query v.stack_set_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("regions", (RegionList.to_json v.regions));
           Some ("accounts", (AccountList.to_json v.accounts));
           Util.option_map v.operation_id
             (fun f -> ("operation_id", (String.to_json f)));
           Util.option_map v.auto_deployment
             (fun f -> ("auto_deployment", (AutoDeployment.to_json f)));
           Util.option_map v.permission_model
             (fun f -> ("permission_model", (PermissionModels.to_json f)));
           Util.option_map v.deployment_targets
             (fun f -> ("deployment_targets", (DeploymentTargets.to_json f)));
           Util.option_map v.execution_role_name
             (fun f -> ("execution_role_name", (String.to_json f)));
           Util.option_map v.administration_role_a_r_n
             (fun f -> ("administration_role_a_r_n", (String.to_json f)));
           Util.option_map v.operation_preferences
             (fun f ->
                ("operation_preferences",
                  (StackSetOperationPreferences.to_json f)));
           Some ("tags", (Tags.to_json v.tags));
           Some ("capabilities", (Capabilities.to_json v.capabilities));
           Some ("parameters", (Parameters.to_json v.parameters));
           Util.option_map v.use_previous_template
             (fun f -> ("use_previous_template", (Boolean.to_json f)));
           Util.option_map v.template_u_r_l
             (fun f -> ("template_u_r_l", (String.to_json f)));
           Util.option_map v.template_body
             (fun f -> ("template_body", (String.to_json f)));
           Util.option_map v.description
             (fun f -> ("description", (String.to_json f)));
           Some ("stack_set_name", (String.to_json v.stack_set_name))])
    let of_json j =
      {
        stack_set_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "stack_set_name")));
        description =
          (Util.option_map (Json.lookup j "description") String.of_json);
        template_body =
          (Util.option_map (Json.lookup j "template_body") String.of_json);
        template_u_r_l =
          (Util.option_map (Json.lookup j "template_u_r_l") String.of_json);
        use_previous_template =
          (Util.option_map (Json.lookup j "use_previous_template")
             Boolean.of_json);
        parameters =
          (Parameters.of_json
             (Util.of_option_exn (Json.lookup j "parameters")));
        capabilities =
          (Capabilities.of_json
             (Util.of_option_exn (Json.lookup j "capabilities")));
        tags = (Tags.of_json (Util.of_option_exn (Json.lookup j "tags")));
        operation_preferences =
          (Util.option_map (Json.lookup j "operation_preferences")
             StackSetOperationPreferences.of_json);
        administration_role_a_r_n =
          (Util.option_map (Json.lookup j "administration_role_a_r_n")
             String.of_json);
        execution_role_name =
          (Util.option_map (Json.lookup j "execution_role_name")
             String.of_json);
        deployment_targets =
          (Util.option_map (Json.lookup j "deployment_targets")
             DeploymentTargets.of_json);
        permission_model =
          (Util.option_map (Json.lookup j "permission_model")
             PermissionModels.of_json);
        auto_deployment =
          (Util.option_map (Json.lookup j "auto_deployment")
             AutoDeployment.of_json);
        operation_id =
          (Util.option_map (Json.lookup j "operation_id") String.of_json);
        accounts =
          (AccountList.of_json
             (Util.of_option_exn (Json.lookup j "accounts")));
        regions =
          (RegionList.of_json (Util.of_option_exn (Json.lookup j "regions")))
      }
  end
module GetTemplateSummaryInput =
  struct
    type t =
      {
      template_body: String.t option ;
      template_u_r_l: String.t option ;
      stack_name: String.t option ;
      stack_set_name: String.t option }
    let make ?template_body  ?template_u_r_l  ?stack_name  ?stack_set_name 
      () = { template_body; template_u_r_l; stack_name; stack_set_name }
    let parse xml =
      Some
        {
          template_body =
            (Util.option_bind (Xml.member "TemplateBody" xml) String.parse);
          template_u_r_l =
            (Util.option_bind (Xml.member "TemplateURL" xml) String.parse);
          stack_name =
            (Util.option_bind (Xml.member "StackName" xml) String.parse);
          stack_set_name =
            (Util.option_bind (Xml.member "StackSetName" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.stack_set_name
              (fun f -> Query.Pair ("StackSetName", (String.to_query f)));
           Util.option_map v.stack_name
             (fun f -> Query.Pair ("StackName", (String.to_query f)));
           Util.option_map v.template_u_r_l
             (fun f -> Query.Pair ("TemplateURL", (String.to_query f)));
           Util.option_map v.template_body
             (fun f -> Query.Pair ("TemplateBody", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.stack_set_name
              (fun f -> ("stack_set_name", (String.to_json f)));
           Util.option_map v.stack_name
             (fun f -> ("stack_name", (String.to_json f)));
           Util.option_map v.template_u_r_l
             (fun f -> ("template_u_r_l", (String.to_json f)));
           Util.option_map v.template_body
             (fun f -> ("template_body", (String.to_json f)))])
    let of_json j =
      {
        template_body =
          (Util.option_map (Json.lookup j "template_body") String.of_json);
        template_u_r_l =
          (Util.option_map (Json.lookup j "template_u_r_l") String.of_json);
        stack_name =
          (Util.option_map (Json.lookup j "stack_name") String.of_json);
        stack_set_name =
          (Util.option_map (Json.lookup j "stack_set_name") String.of_json)
      }
  end
module UpdateStackOutput =
  struct
    type t = {
      stack_id: String.t option }
    let make ?stack_id  () = { stack_id }
    let parse xml =
      Some
        {
          stack_id =
            (Util.option_bind (Xml.member "StackId" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.stack_id
              (fun f -> Query.Pair ("StackId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.stack_id
              (fun f -> ("stack_id", (String.to_json f)))])
    let of_json j =
      {
        stack_id =
          (Util.option_map (Json.lookup j "stack_id") String.of_json)
      }
  end
module ListStackInstancesOutput =
  struct
    type t =
      {
      summaries: StackInstanceSummaries.t ;
      next_token: String.t option }
    let make ?(summaries= [])  ?next_token  () = { summaries; next_token }
    let parse xml =
      Some
        {
          summaries =
            (Util.of_option []
               (Util.option_bind (Xml.member "Summaries" xml)
                  StackInstanceSummaries.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair
                ("Summaries.member",
                  (StackInstanceSummaries.to_query v.summaries)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some ("summaries", (StackInstanceSummaries.to_json v.summaries))])
    let of_json j =
      {
        summaries =
          (StackInstanceSummaries.of_json
             (Util.of_option_exn (Json.lookup j "summaries")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module CreateStackInstancesOutput =
  struct
    type t = {
      operation_id: String.t option }
    let make ?operation_id  () = { operation_id }
    let parse xml =
      Some
        {
          operation_id =
            (Util.option_bind (Xml.member "OperationId" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.operation_id
              (fun f -> Query.Pair ("OperationId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.operation_id
              (fun f -> ("operation_id", (String.to_json f)))])
    let of_json j =
      {
        operation_id =
          (Util.option_map (Json.lookup j "operation_id") String.of_json)
      }
  end
module DescribeStacksOutput =
  struct
    type t = {
      stacks: Stacks.t ;
      next_token: String.t option }
    let make ?(stacks= [])  ?next_token  () = { stacks; next_token }
    let parse xml =
      Some
        {
          stacks =
            (Util.of_option []
               (Util.option_bind (Xml.member "Stacks" xml) Stacks.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some (Query.Pair ("Stacks.member", (Stacks.to_query v.stacks)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some ("stacks", (Stacks.to_json v.stacks))])
    let of_json j =
      {
        stacks =
          (Stacks.of_json (Util.of_option_exn (Json.lookup j "stacks")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module LimitExceededException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module RecordHandlerProgressOutput =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module UpdateStackInput =
  struct
    type t =
      {
      stack_name: String.t ;
      template_body: String.t option ;
      template_u_r_l: String.t option ;
      use_previous_template: Boolean.t option ;
      stack_policy_during_update_body: String.t option ;
      stack_policy_during_update_u_r_l: String.t option ;
      parameters: Parameters.t ;
      capabilities: Capabilities.t ;
      resource_types: ResourceTypes.t ;
      role_a_r_n: String.t option ;
      rollback_configuration: RollbackConfiguration.t option ;
      stack_policy_body: String.t option ;
      stack_policy_u_r_l: String.t option ;
      notification_a_r_ns: NotificationARNs.t ;
      tags: Tags.t ;
      client_request_token: String.t option }
    let make ~stack_name  ?template_body  ?template_u_r_l 
      ?use_previous_template  ?stack_policy_during_update_body 
      ?stack_policy_during_update_u_r_l  ?(parameters= [])  ?(capabilities=
      [])  ?(resource_types= [])  ?role_a_r_n  ?rollback_configuration 
      ?stack_policy_body  ?stack_policy_u_r_l  ?(notification_a_r_ns= []) 
      ?(tags= [])  ?client_request_token  () =
      {
        stack_name;
        template_body;
        template_u_r_l;
        use_previous_template;
        stack_policy_during_update_body;
        stack_policy_during_update_u_r_l;
        parameters;
        capabilities;
        resource_types;
        role_a_r_n;
        rollback_configuration;
        stack_policy_body;
        stack_policy_u_r_l;
        notification_a_r_ns;
        tags;
        client_request_token
      }
    let parse xml =
      Some
        {
          stack_name =
            (Xml.required "StackName"
               (Util.option_bind (Xml.member "StackName" xml) String.parse));
          template_body =
            (Util.option_bind (Xml.member "TemplateBody" xml) String.parse);
          template_u_r_l =
            (Util.option_bind (Xml.member "TemplateURL" xml) String.parse);
          use_previous_template =
            (Util.option_bind (Xml.member "UsePreviousTemplate" xml)
               Boolean.parse);
          stack_policy_during_update_body =
            (Util.option_bind (Xml.member "StackPolicyDuringUpdateBody" xml)
               String.parse);
          stack_policy_during_update_u_r_l =
            (Util.option_bind (Xml.member "StackPolicyDuringUpdateURL" xml)
               String.parse);
          parameters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Parameters" xml)
                  Parameters.parse));
          capabilities =
            (Util.of_option []
               (Util.option_bind (Xml.member "Capabilities" xml)
                  Capabilities.parse));
          resource_types =
            (Util.of_option []
               (Util.option_bind (Xml.member "ResourceTypes" xml)
                  ResourceTypes.parse));
          role_a_r_n =
            (Util.option_bind (Xml.member "RoleARN" xml) String.parse);
          rollback_configuration =
            (Util.option_bind (Xml.member "RollbackConfiguration" xml)
               RollbackConfiguration.parse);
          stack_policy_body =
            (Util.option_bind (Xml.member "StackPolicyBody" xml) String.parse);
          stack_policy_u_r_l =
            (Util.option_bind (Xml.member "StackPolicyURL" xml) String.parse);
          notification_a_r_ns =
            (Util.of_option []
               (Util.option_bind (Xml.member "NotificationARNs" xml)
                  NotificationARNs.parse));
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) Tags.parse));
          client_request_token =
            (Util.option_bind (Xml.member "ClientRequestToken" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.client_request_token
              (fun f ->
                 Query.Pair ("ClientRequestToken", (String.to_query f)));
           Some (Query.Pair ("Tags.member", (Tags.to_query v.tags)));
           Some
             (Query.Pair
                ("NotificationARNs.member",
                  (NotificationARNs.to_query v.notification_a_r_ns)));
           Util.option_map v.stack_policy_u_r_l
             (fun f -> Query.Pair ("StackPolicyURL", (String.to_query f)));
           Util.option_map v.stack_policy_body
             (fun f -> Query.Pair ("StackPolicyBody", (String.to_query f)));
           Util.option_map v.rollback_configuration
             (fun f ->
                Query.Pair
                  ("RollbackConfiguration",
                    (RollbackConfiguration.to_query f)));
           Util.option_map v.role_a_r_n
             (fun f -> Query.Pair ("RoleARN", (String.to_query f)));
           Some
             (Query.Pair
                ("ResourceTypes.member",
                  (ResourceTypes.to_query v.resource_types)));
           Some
             (Query.Pair
                ("Capabilities.member",
                  (Capabilities.to_query v.capabilities)));
           Some
             (Query.Pair
                ("Parameters.member", (Parameters.to_query v.parameters)));
           Util.option_map v.stack_policy_during_update_u_r_l
             (fun f ->
                Query.Pair
                  ("StackPolicyDuringUpdateURL", (String.to_query f)));
           Util.option_map v.stack_policy_during_update_body
             (fun f ->
                Query.Pair
                  ("StackPolicyDuringUpdateBody", (String.to_query f)));
           Util.option_map v.use_previous_template
             (fun f ->
                Query.Pair ("UsePreviousTemplate", (Boolean.to_query f)));
           Util.option_map v.template_u_r_l
             (fun f -> Query.Pair ("TemplateURL", (String.to_query f)));
           Util.option_map v.template_body
             (fun f -> Query.Pair ("TemplateBody", (String.to_query f)));
           Some (Query.Pair ("StackName", (String.to_query v.stack_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.client_request_token
              (fun f -> ("client_request_token", (String.to_json f)));
           Some ("tags", (Tags.to_json v.tags));
           Some
             ("notification_a_r_ns",
               (NotificationARNs.to_json v.notification_a_r_ns));
           Util.option_map v.stack_policy_u_r_l
             (fun f -> ("stack_policy_u_r_l", (String.to_json f)));
           Util.option_map v.stack_policy_body
             (fun f -> ("stack_policy_body", (String.to_json f)));
           Util.option_map v.rollback_configuration
             (fun f ->
                ("rollback_configuration", (RollbackConfiguration.to_json f)));
           Util.option_map v.role_a_r_n
             (fun f -> ("role_a_r_n", (String.to_json f)));
           Some ("resource_types", (ResourceTypes.to_json v.resource_types));
           Some ("capabilities", (Capabilities.to_json v.capabilities));
           Some ("parameters", (Parameters.to_json v.parameters));
           Util.option_map v.stack_policy_during_update_u_r_l
             (fun f ->
                ("stack_policy_during_update_u_r_l", (String.to_json f)));
           Util.option_map v.stack_policy_during_update_body
             (fun f ->
                ("stack_policy_during_update_body", (String.to_json f)));
           Util.option_map v.use_previous_template
             (fun f -> ("use_previous_template", (Boolean.to_json f)));
           Util.option_map v.template_u_r_l
             (fun f -> ("template_u_r_l", (String.to_json f)));
           Util.option_map v.template_body
             (fun f -> ("template_body", (String.to_json f)));
           Some ("stack_name", (String.to_json v.stack_name))])
    let of_json j =
      {
        stack_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "stack_name")));
        template_body =
          (Util.option_map (Json.lookup j "template_body") String.of_json);
        template_u_r_l =
          (Util.option_map (Json.lookup j "template_u_r_l") String.of_json);
        use_previous_template =
          (Util.option_map (Json.lookup j "use_previous_template")
             Boolean.of_json);
        stack_policy_during_update_body =
          (Util.option_map (Json.lookup j "stack_policy_during_update_body")
             String.of_json);
        stack_policy_during_update_u_r_l =
          (Util.option_map (Json.lookup j "stack_policy_during_update_u_r_l")
             String.of_json);
        parameters =
          (Parameters.of_json
             (Util.of_option_exn (Json.lookup j "parameters")));
        capabilities =
          (Capabilities.of_json
             (Util.of_option_exn (Json.lookup j "capabilities")));
        resource_types =
          (ResourceTypes.of_json
             (Util.of_option_exn (Json.lookup j "resource_types")));
        role_a_r_n =
          (Util.option_map (Json.lookup j "role_a_r_n") String.of_json);
        rollback_configuration =
          (Util.option_map (Json.lookup j "rollback_configuration")
             RollbackConfiguration.of_json);
        stack_policy_body =
          (Util.option_map (Json.lookup j "stack_policy_body") String.of_json);
        stack_policy_u_r_l =
          (Util.option_map (Json.lookup j "stack_policy_u_r_l")
             String.of_json);
        notification_a_r_ns =
          (NotificationARNs.of_json
             (Util.of_option_exn (Json.lookup j "notification_a_r_ns")));
        tags = (Tags.of_json (Util.of_option_exn (Json.lookup j "tags")));
        client_request_token =
          (Util.option_map (Json.lookup j "client_request_token")
             String.of_json)
      }
  end
module ExecuteChangeSetInput =
  struct
    type t =
      {
      change_set_name: String.t ;
      stack_name: String.t option ;
      client_request_token: String.t option }
    let make ~change_set_name  ?stack_name  ?client_request_token  () =
      { change_set_name; stack_name; client_request_token }
    let parse xml =
      Some
        {
          change_set_name =
            (Xml.required "ChangeSetName"
               (Util.option_bind (Xml.member "ChangeSetName" xml)
                  String.parse));
          stack_name =
            (Util.option_bind (Xml.member "StackName" xml) String.parse);
          client_request_token =
            (Util.option_bind (Xml.member "ClientRequestToken" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.client_request_token
              (fun f ->
                 Query.Pair ("ClientRequestToken", (String.to_query f)));
           Util.option_map v.stack_name
             (fun f -> Query.Pair ("StackName", (String.to_query f)));
           Some
             (Query.Pair
                ("ChangeSetName", (String.to_query v.change_set_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.client_request_token
              (fun f -> ("client_request_token", (String.to_json f)));
           Util.option_map v.stack_name
             (fun f -> ("stack_name", (String.to_json f)));
           Some ("change_set_name", (String.to_json v.change_set_name))])
    let of_json j =
      {
        change_set_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "change_set_name")));
        stack_name =
          (Util.option_map (Json.lookup j "stack_name") String.of_json);
        client_request_token =
          (Util.option_map (Json.lookup j "client_request_token")
             String.of_json)
      }
  end
module DescribeStackInstanceInput =
  struct
    type t =
      {
      stack_set_name: String.t ;
      stack_instance_account: String.t ;
      stack_instance_region: String.t }
    let make ~stack_set_name  ~stack_instance_account  ~stack_instance_region
       () = { stack_set_name; stack_instance_account; stack_instance_region }
    let parse xml =
      Some
        {
          stack_set_name =
            (Xml.required "StackSetName"
               (Util.option_bind (Xml.member "StackSetName" xml) String.parse));
          stack_instance_account =
            (Xml.required "StackInstanceAccount"
               (Util.option_bind (Xml.member "StackInstanceAccount" xml)
                  String.parse));
          stack_instance_region =
            (Xml.required "StackInstanceRegion"
               (Util.option_bind (Xml.member "StackInstanceRegion" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("StackInstanceRegion",
                   (String.to_query v.stack_instance_region)));
           Some
             (Query.Pair
                ("StackInstanceAccount",
                  (String.to_query v.stack_instance_account)));
           Some
             (Query.Pair ("StackSetName", (String.to_query v.stack_set_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("stack_instance_region",
                (String.to_json v.stack_instance_region));
           Some
             ("stack_instance_account",
               (String.to_json v.stack_instance_account));
           Some ("stack_set_name", (String.to_json v.stack_set_name))])
    let of_json j =
      {
        stack_set_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "stack_set_name")));
        stack_instance_account =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "stack_instance_account")));
        stack_instance_region =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "stack_instance_region")))
      }
  end
module CreateStackInput =
  struct
    type t =
      {
      stack_name: String.t ;
      template_body: String.t option ;
      template_u_r_l: String.t option ;
      parameters: Parameters.t ;
      disable_rollback: Boolean.t option ;
      rollback_configuration: RollbackConfiguration.t option ;
      timeout_in_minutes: Integer.t option ;
      notification_a_r_ns: NotificationARNs.t ;
      capabilities: Capabilities.t ;
      resource_types: ResourceTypes.t ;
      role_a_r_n: String.t option ;
      on_failure: OnFailure.t option ;
      stack_policy_body: String.t option ;
      stack_policy_u_r_l: String.t option ;
      tags: Tags.t ;
      client_request_token: String.t option ;
      enable_termination_protection: Boolean.t option }
    let make ~stack_name  ?template_body  ?template_u_r_l  ?(parameters= []) 
      ?disable_rollback  ?rollback_configuration  ?timeout_in_minutes 
      ?(notification_a_r_ns= [])  ?(capabilities= [])  ?(resource_types= []) 
      ?role_a_r_n  ?on_failure  ?stack_policy_body  ?stack_policy_u_r_l 
      ?(tags= [])  ?client_request_token  ?enable_termination_protection  ()
      =
      {
        stack_name;
        template_body;
        template_u_r_l;
        parameters;
        disable_rollback;
        rollback_configuration;
        timeout_in_minutes;
        notification_a_r_ns;
        capabilities;
        resource_types;
        role_a_r_n;
        on_failure;
        stack_policy_body;
        stack_policy_u_r_l;
        tags;
        client_request_token;
        enable_termination_protection
      }
    let parse xml =
      Some
        {
          stack_name =
            (Xml.required "StackName"
               (Util.option_bind (Xml.member "StackName" xml) String.parse));
          template_body =
            (Util.option_bind (Xml.member "TemplateBody" xml) String.parse);
          template_u_r_l =
            (Util.option_bind (Xml.member "TemplateURL" xml) String.parse);
          parameters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Parameters" xml)
                  Parameters.parse));
          disable_rollback =
            (Util.option_bind (Xml.member "DisableRollback" xml)
               Boolean.parse);
          rollback_configuration =
            (Util.option_bind (Xml.member "RollbackConfiguration" xml)
               RollbackConfiguration.parse);
          timeout_in_minutes =
            (Util.option_bind (Xml.member "TimeoutInMinutes" xml)
               Integer.parse);
          notification_a_r_ns =
            (Util.of_option []
               (Util.option_bind (Xml.member "NotificationARNs" xml)
                  NotificationARNs.parse));
          capabilities =
            (Util.of_option []
               (Util.option_bind (Xml.member "Capabilities" xml)
                  Capabilities.parse));
          resource_types =
            (Util.of_option []
               (Util.option_bind (Xml.member "ResourceTypes" xml)
                  ResourceTypes.parse));
          role_a_r_n =
            (Util.option_bind (Xml.member "RoleARN" xml) String.parse);
          on_failure =
            (Util.option_bind (Xml.member "OnFailure" xml) OnFailure.parse);
          stack_policy_body =
            (Util.option_bind (Xml.member "StackPolicyBody" xml) String.parse);
          stack_policy_u_r_l =
            (Util.option_bind (Xml.member "StackPolicyURL" xml) String.parse);
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) Tags.parse));
          client_request_token =
            (Util.option_bind (Xml.member "ClientRequestToken" xml)
               String.parse);
          enable_termination_protection =
            (Util.option_bind (Xml.member "EnableTerminationProtection" xml)
               Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.enable_termination_protection
              (fun f ->
                 Query.Pair
                   ("EnableTerminationProtection", (Boolean.to_query f)));
           Util.option_map v.client_request_token
             (fun f -> Query.Pair ("ClientRequestToken", (String.to_query f)));
           Some (Query.Pair ("Tags.member", (Tags.to_query v.tags)));
           Util.option_map v.stack_policy_u_r_l
             (fun f -> Query.Pair ("StackPolicyURL", (String.to_query f)));
           Util.option_map v.stack_policy_body
             (fun f -> Query.Pair ("StackPolicyBody", (String.to_query f)));
           Util.option_map v.on_failure
             (fun f -> Query.Pair ("OnFailure", (OnFailure.to_query f)));
           Util.option_map v.role_a_r_n
             (fun f -> Query.Pair ("RoleARN", (String.to_query f)));
           Some
             (Query.Pair
                ("ResourceTypes.member",
                  (ResourceTypes.to_query v.resource_types)));
           Some
             (Query.Pair
                ("Capabilities.member",
                  (Capabilities.to_query v.capabilities)));
           Some
             (Query.Pair
                ("NotificationARNs.member",
                  (NotificationARNs.to_query v.notification_a_r_ns)));
           Util.option_map v.timeout_in_minutes
             (fun f -> Query.Pair ("TimeoutInMinutes", (Integer.to_query f)));
           Util.option_map v.rollback_configuration
             (fun f ->
                Query.Pair
                  ("RollbackConfiguration",
                    (RollbackConfiguration.to_query f)));
           Util.option_map v.disable_rollback
             (fun f -> Query.Pair ("DisableRollback", (Boolean.to_query f)));
           Some
             (Query.Pair
                ("Parameters.member", (Parameters.to_query v.parameters)));
           Util.option_map v.template_u_r_l
             (fun f -> Query.Pair ("TemplateURL", (String.to_query f)));
           Util.option_map v.template_body
             (fun f -> Query.Pair ("TemplateBody", (String.to_query f)));
           Some (Query.Pair ("StackName", (String.to_query v.stack_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.enable_termination_protection
              (fun f ->
                 ("enable_termination_protection", (Boolean.to_json f)));
           Util.option_map v.client_request_token
             (fun f -> ("client_request_token", (String.to_json f)));
           Some ("tags", (Tags.to_json v.tags));
           Util.option_map v.stack_policy_u_r_l
             (fun f -> ("stack_policy_u_r_l", (String.to_json f)));
           Util.option_map v.stack_policy_body
             (fun f -> ("stack_policy_body", (String.to_json f)));
           Util.option_map v.on_failure
             (fun f -> ("on_failure", (OnFailure.to_json f)));
           Util.option_map v.role_a_r_n
             (fun f -> ("role_a_r_n", (String.to_json f)));
           Some ("resource_types", (ResourceTypes.to_json v.resource_types));
           Some ("capabilities", (Capabilities.to_json v.capabilities));
           Some
             ("notification_a_r_ns",
               (NotificationARNs.to_json v.notification_a_r_ns));
           Util.option_map v.timeout_in_minutes
             (fun f -> ("timeout_in_minutes", (Integer.to_json f)));
           Util.option_map v.rollback_configuration
             (fun f ->
                ("rollback_configuration", (RollbackConfiguration.to_json f)));
           Util.option_map v.disable_rollback
             (fun f -> ("disable_rollback", (Boolean.to_json f)));
           Some ("parameters", (Parameters.to_json v.parameters));
           Util.option_map v.template_u_r_l
             (fun f -> ("template_u_r_l", (String.to_json f)));
           Util.option_map v.template_body
             (fun f -> ("template_body", (String.to_json f)));
           Some ("stack_name", (String.to_json v.stack_name))])
    let of_json j =
      {
        stack_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "stack_name")));
        template_body =
          (Util.option_map (Json.lookup j "template_body") String.of_json);
        template_u_r_l =
          (Util.option_map (Json.lookup j "template_u_r_l") String.of_json);
        parameters =
          (Parameters.of_json
             (Util.of_option_exn (Json.lookup j "parameters")));
        disable_rollback =
          (Util.option_map (Json.lookup j "disable_rollback") Boolean.of_json);
        rollback_configuration =
          (Util.option_map (Json.lookup j "rollback_configuration")
             RollbackConfiguration.of_json);
        timeout_in_minutes =
          (Util.option_map (Json.lookup j "timeout_in_minutes")
             Integer.of_json);
        notification_a_r_ns =
          (NotificationARNs.of_json
             (Util.of_option_exn (Json.lookup j "notification_a_r_ns")));
        capabilities =
          (Capabilities.of_json
             (Util.of_option_exn (Json.lookup j "capabilities")));
        resource_types =
          (ResourceTypes.of_json
             (Util.of_option_exn (Json.lookup j "resource_types")));
        role_a_r_n =
          (Util.option_map (Json.lookup j "role_a_r_n") String.of_json);
        on_failure =
          (Util.option_map (Json.lookup j "on_failure") OnFailure.of_json);
        stack_policy_body =
          (Util.option_map (Json.lookup j "stack_policy_body") String.of_json);
        stack_policy_u_r_l =
          (Util.option_map (Json.lookup j "stack_policy_u_r_l")
             String.of_json);
        tags = (Tags.of_json (Util.of_option_exn (Json.lookup j "tags")));
        client_request_token =
          (Util.option_map (Json.lookup j "client_request_token")
             String.of_json);
        enable_termination_protection =
          (Util.option_map (Json.lookup j "enable_termination_protection")
             Boolean.of_json)
      }
  end
module ListTypeRegistrationsOutput =
  struct
    type t =
      {
      registration_token_list: RegistrationTokenList.t ;
      next_token: String.t option }
    let make ?(registration_token_list= [])  ?next_token  () =
      { registration_token_list; next_token }
    let parse xml =
      Some
        {
          registration_token_list =
            (Util.of_option []
               (Util.option_bind (Xml.member "RegistrationTokenList" xml)
                  RegistrationTokenList.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair
                ("RegistrationTokenList.member",
                  (RegistrationTokenList.to_query v.registration_token_list)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some
             ("registration_token_list",
               (RegistrationTokenList.to_json v.registration_token_list))])
    let of_json j =
      {
        registration_token_list =
          (RegistrationTokenList.of_json
             (Util.of_option_exn (Json.lookup j "registration_token_list")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module InvalidChangeSetStatusException =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CreateStackInstancesInput =
  struct
    type t =
      {
      stack_set_name: String.t ;
      accounts: AccountList.t ;
      deployment_targets: DeploymentTargets.t option ;
      regions: RegionList.t ;
      parameter_overrides: Parameters.t ;
      operation_preferences: StackSetOperationPreferences.t option ;
      operation_id: String.t option }
    let make ~stack_set_name  ?(accounts= [])  ?deployment_targets  ~regions 
      ?(parameter_overrides= [])  ?operation_preferences  ?operation_id  () =
      {
        stack_set_name;
        accounts;
        deployment_targets;
        regions;
        parameter_overrides;
        operation_preferences;
        operation_id
      }
    let parse xml =
      Some
        {
          stack_set_name =
            (Xml.required "StackSetName"
               (Util.option_bind (Xml.member "StackSetName" xml) String.parse));
          accounts =
            (Util.of_option []
               (Util.option_bind (Xml.member "Accounts" xml)
                  AccountList.parse));
          deployment_targets =
            (Util.option_bind (Xml.member "DeploymentTargets" xml)
               DeploymentTargets.parse);
          regions =
            (Xml.required "Regions"
               (Util.option_bind (Xml.member "Regions" xml) RegionList.parse));
          parameter_overrides =
            (Util.of_option []
               (Util.option_bind (Xml.member "ParameterOverrides" xml)
                  Parameters.parse));
          operation_preferences =
            (Util.option_bind (Xml.member "OperationPreferences" xml)
               StackSetOperationPreferences.parse);
          operation_id =
            (Util.option_bind (Xml.member "OperationId" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.operation_id
              (fun f -> Query.Pair ("OperationId", (String.to_query f)));
           Util.option_map v.operation_preferences
             (fun f ->
                Query.Pair
                  ("OperationPreferences",
                    (StackSetOperationPreferences.to_query f)));
           Some
             (Query.Pair
                ("ParameterOverrides.member",
                  (Parameters.to_query v.parameter_overrides)));
           Some
             (Query.Pair ("Regions.member", (RegionList.to_query v.regions)));
           Util.option_map v.deployment_targets
             (fun f ->
                Query.Pair
                  ("DeploymentTargets", (DeploymentTargets.to_query f)));
           Some
             (Query.Pair
                ("Accounts.member", (AccountList.to_query v.accounts)));
           Some
             (Query.Pair ("StackSetName", (String.to_query v.stack_set_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.operation_id
              (fun f -> ("operation_id", (String.to_json f)));
           Util.option_map v.operation_preferences
             (fun f ->
                ("operation_preferences",
                  (StackSetOperationPreferences.to_json f)));
           Some
             ("parameter_overrides",
               (Parameters.to_json v.parameter_overrides));
           Some ("regions", (RegionList.to_json v.regions));
           Util.option_map v.deployment_targets
             (fun f -> ("deployment_targets", (DeploymentTargets.to_json f)));
           Some ("accounts", (AccountList.to_json v.accounts));
           Some ("stack_set_name", (String.to_json v.stack_set_name))])
    let of_json j =
      {
        stack_set_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "stack_set_name")));
        accounts =
          (AccountList.of_json
             (Util.of_option_exn (Json.lookup j "accounts")));
        deployment_targets =
          (Util.option_map (Json.lookup j "deployment_targets")
             DeploymentTargets.of_json);
        regions =
          (RegionList.of_json (Util.of_option_exn (Json.lookup j "regions")));
        parameter_overrides =
          (Parameters.of_json
             (Util.of_option_exn (Json.lookup j "parameter_overrides")));
        operation_preferences =
          (Util.option_map (Json.lookup j "operation_preferences")
             StackSetOperationPreferences.of_json);
        operation_id =
          (Util.option_map (Json.lookup j "operation_id") String.of_json)
      }
  end
module ListChangeSetsInput =
  struct
    type t = {
      stack_name: String.t ;
      next_token: String.t option }
    let make ~stack_name  ?next_token  () = { stack_name; next_token }
    let parse xml =
      Some
        {
          stack_name =
            (Xml.required "StackName"
               (Util.option_bind (Xml.member "StackName" xml) String.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some (Query.Pair ("StackName", (String.to_query v.stack_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some ("stack_name", (String.to_json v.stack_name))])
    let of_json j =
      {
        stack_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "stack_name")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module CFNRegistryException =
  struct
    type t = {
      message: String.t option }
    let make ?message  () = { message }
    let parse xml =
      Some
        {
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)))])
    let of_json j =
      { message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module DescribeStackResourcesOutput =
  struct
    type t = {
      stack_resources: StackResources.t }
    let make ?(stack_resources= [])  () = { stack_resources }
    let parse xml =
      Some
        {
          stack_resources =
            (Util.of_option []
               (Util.option_bind (Xml.member "StackResources" xml)
                  StackResources.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("StackResources.member",
                   (StackResources.to_query v.stack_resources)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("stack_resources", (StackResources.to_json v.stack_resources))])
    let of_json j =
      {
        stack_resources =
          (StackResources.of_json
             (Util.of_option_exn (Json.lookup j "stack_resources")))
      }
  end
module DetectStackResourceDriftOutput =
  struct
    type t = {
      stack_resource_drift: StackResourceDrift.t }
    let make ~stack_resource_drift  () = { stack_resource_drift }
    let parse xml =
      Some
        {
          stack_resource_drift =
            (Xml.required "StackResourceDrift"
               (Util.option_bind (Xml.member "StackResourceDrift" xml)
                  StackResourceDrift.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("StackResourceDrift",
                   (StackResourceDrift.to_query v.stack_resource_drift)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("stack_resource_drift",
                (StackResourceDrift.to_json v.stack_resource_drift))])
    let of_json j =
      {
        stack_resource_drift =
          (StackResourceDrift.of_json
             (Util.of_option_exn (Json.lookup j "stack_resource_drift")))
      }
  end
module ListStackResourcesInput =
  struct
    type t = {
      stack_name: String.t ;
      next_token: String.t option }
    let make ~stack_name  ?next_token  () = { stack_name; next_token }
    let parse xml =
      Some
        {
          stack_name =
            (Xml.required "StackName"
               (Util.option_bind (Xml.member "StackName" xml) String.parse));
          next_token =
            (Util.option_bind (Xml.member "NextToken" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> Query.Pair ("NextToken", (String.to_query f)));
           Some (Query.Pair ("StackName", (String.to_query v.stack_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f -> ("next_token", (String.to_json f)));
           Some ("stack_name", (String.to_json v.stack_name))])
    let of_json j =
      {
        stack_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "stack_name")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      }
  end
module DescribeStackDriftDetectionStatusInput =
  struct
    type t = {
      stack_drift_detection_id: String.t }
    let make ~stack_drift_detection_id  () = { stack_drift_detection_id }
    let parse xml =
      Some
        {
          stack_drift_detection_id =
            (Xml.required "StackDriftDetectionId"
               (Util.option_bind (Xml.member "StackDriftDetectionId" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("StackDriftDetectionId",
                   (String.to_query v.stack_drift_detection_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("stack_drift_detection_id",
                (String.to_json v.stack_drift_detection_id))])
    let of_json j =
      {
        stack_drift_detection_id =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "stack_drift_detection_id")))
      }
  end
module GetTemplateOutput =
  struct
    type t = {
      template_body: String.t option ;
      stages_available: StageList.t }
    let make ?template_body  ?(stages_available= [])  () =
      { template_body; stages_available }
    let parse xml =
      Some
        {
          template_body =
            (Util.option_bind (Xml.member "TemplateBody" xml) String.parse);
          stages_available =
            (Util.of_option []
               (Util.option_bind (Xml.member "StagesAvailable" xml)
                  StageList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("StagesAvailable.member",
                   (StageList.to_query v.stages_available)));
           Util.option_map v.template_body
             (fun f -> Query.Pair ("TemplateBody", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("stages_available", (StageList.to_json v.stages_available));
           Util.option_map v.template_body
             (fun f -> ("template_body", (String.to_json f)))])
    let of_json j =
      {
        template_body =
          (Util.option_map (Json.lookup j "template_body") String.of_json);
        stages_available =
          (StageList.of_json
             (Util.of_option_exn (Json.lookup j "stages_available")))
      }
  end
module UpdateTerminationProtectionInput =
  struct
    type t =
      {
      enable_termination_protection: Boolean.t ;
      stack_name: String.t }
    let make ~enable_termination_protection  ~stack_name  () =
      { enable_termination_protection; stack_name }
    let parse xml =
      Some
        {
          enable_termination_protection =
            (Xml.required "EnableTerminationProtection"
               (Util.option_bind
                  (Xml.member "EnableTerminationProtection" xml)
                  Boolean.parse));
          stack_name =
            (Xml.required "StackName"
               (Util.option_bind (Xml.member "StackName" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("StackName", (String.to_query v.stack_name)));
           Some
             (Query.Pair
                ("EnableTerminationProtection",
                  (Boolean.to_query v.enable_termination_protection)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("stack_name", (String.to_json v.stack_name));
           Some
             ("enable_termination_protection",
               (Boolean.to_json v.enable_termination_protection))])
    let of_json j =
      {
        enable_termination_protection =
          (Boolean.of_json
             (Util.of_option_exn
                (Json.lookup j "enable_termination_protection")));
        stack_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "stack_name")))
      }
  end
module UpdateStackSetOutput =
  struct
    type t = {
      operation_id: String.t option }
    let make ?operation_id  () = { operation_id }
    let parse xml =
      Some
        {
          operation_id =
            (Util.option_bind (Xml.member "OperationId" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.operation_id
              (fun f -> Query.Pair ("OperationId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.operation_id
              (fun f -> ("operation_id", (String.to_json f)))])
    let of_json j =
      {
        operation_id =
          (Util.option_map (Json.lookup j "operation_id") String.of_json)
      }
  end