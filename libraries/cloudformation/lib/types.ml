open Aws
open Aws.BaseTypes
open CalendarLib
type calendar = Calendar.t
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
module Capability =
  struct
    type t =
      | CAPABILITY_IAM 
    let str_to_t = [("CAPABILITY_IAM", CAPABILITY_IAM)] 
    let t_to_str = [(CAPABILITY_IAM, "CAPABILITY_IAM")] 
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
module Output =
  struct
    type t =
      {
      output_key: String.t option ;
      output_value: String.t option ;
      description: String.t option }
    let make ?output_key  ?output_value  ?description  () =
      { output_key; output_value; description } 
    let parse xml =
      Some
        {
          output_key =
            (Util.option_bind (Xml.member "OutputKey" xml) String.parse);
          output_value =
            (Util.option_bind (Xml.member "OutputValue" xml) String.parse);
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.description
              (fun f  -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.output_value
             (fun f  -> Query.Pair ("OutputValue", (String.to_query f)));
           Util.option_map v.output_key
             (fun f  -> Query.Pair ("OutputKey", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.description
              (fun f  -> ("description", (String.to_json f)));
           Util.option_map v.output_value
             (fun f  -> ("output_value", (String.to_json f)));
           Util.option_map v.output_key
             (fun f  -> ("output_key", (String.to_json f)))])
      
    let of_json j =
      {
        output_key =
          (Util.option_map (Json.lookup j "output_key") String.of_json);
        output_value =
          (Util.option_map (Json.lookup j "output_value") String.of_json);
        description =
          (Util.option_map (Json.lookup j "description") String.of_json)
      } 
  end
module Parameter =
  struct
    type t =
      {
      parameter_key: String.t option ;
      parameter_value: String.t option ;
      use_previous_value: Boolean.t option }
    let make ?parameter_key  ?parameter_value  ?use_previous_value  () =
      { parameter_key; parameter_value; use_previous_value } 
    let parse xml =
      Some
        {
          parameter_key =
            (Util.option_bind (Xml.member "ParameterKey" xml) String.parse);
          parameter_value =
            (Util.option_bind (Xml.member "ParameterValue" xml) String.parse);
          use_previous_value =
            (Util.option_bind (Xml.member "UsePreviousValue" xml)
               Boolean.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.use_previous_value
              (fun f  ->
                 Query.Pair ("UsePreviousValue", (Boolean.to_query f)));
           Util.option_map v.parameter_value
             (fun f  -> Query.Pair ("ParameterValue", (String.to_query f)));
           Util.option_map v.parameter_key
             (fun f  -> Query.Pair ("ParameterKey", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.use_previous_value
              (fun f  -> ("use_previous_value", (Boolean.to_json f)));
           Util.option_map v.parameter_value
             (fun f  -> ("parameter_value", (String.to_json f)));
           Util.option_map v.parameter_key
             (fun f  -> ("parameter_key", (String.to_json f)))])
      
    let of_json j =
      {
        parameter_key =
          (Util.option_map (Json.lookup j "parameter_key") String.of_json);
        parameter_value =
          (Util.option_map (Json.lookup j "parameter_value") String.of_json);
        use_previous_value =
          (Util.option_map (Json.lookup j "use_previous_value")
             Boolean.of_json)
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
    let str_to_t =
      [("UPDATE_COMPLETE", UPDATE_COMPLETE);
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
      [(UPDATE_COMPLETE, "UPDATE_COMPLETE");
      (UPDATE_FAILED, "UPDATE_FAILED");
      (UPDATE_IN_PROGRESS, "UPDATE_IN_PROGRESS");
      (DELETE_SKIPPED, "DELETE_SKIPPED");
      (DELETE_COMPLETE, "DELETE_COMPLETE");
      (DELETE_FAILED, "DELETE_FAILED");
      (DELETE_IN_PROGRESS, "DELETE_IN_PROGRESS");
      (CREATE_COMPLETE, "CREATE_COMPLETE");
      (CREATE_FAILED, "CREATE_FAILED");
      (CREATE_IN_PROGRESS, "CREATE_IN_PROGRESS")] 
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
    let str_to_t =
      [("UPDATE_ROLLBACK_COMPLETE", UPDATE_ROLLBACK_COMPLETE);
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
      [(UPDATE_ROLLBACK_COMPLETE, "UPDATE_ROLLBACK_COMPLETE");
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
module StackResourceSummary =
  struct
    type t =
      {
      logical_resource_id: String.t ;
      physical_resource_id: String.t option ;
      resource_type: String.t ;
      last_updated_timestamp: DateTime.t ;
      resource_status: ResourceStatus.t ;
      resource_status_reason: String.t option }
    let make ~logical_resource_id  ?physical_resource_id  ~resource_type 
      ~last_updated_timestamp  ~resource_status  ?resource_status_reason  ()
      =
      {
        logical_resource_id;
        physical_resource_id;
        resource_type;
        last_updated_timestamp;
        resource_status;
        resource_status_reason
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
               String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.resource_status_reason
              (fun f  ->
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
             (fun f  ->
                Query.Pair ("PhysicalResourceId", (String.to_query f)));
           Some
             (Query.Pair
                ("LogicalResourceId",
                  (String.to_query v.logical_resource_id)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.resource_status_reason
              (fun f  -> ("resource_status_reason", (String.to_json f)));
           Some
             ("resource_status", (ResourceStatus.to_json v.resource_status));
           Some
             ("last_updated_timestamp",
               (DateTime.to_json v.last_updated_timestamp));
           Some ("resource_type", (String.to_json v.resource_type));
           Util.option_map v.physical_resource_id
             (fun f  -> ("physical_resource_id", (String.to_json f)));
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
             String.of_json)
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
      stack_status_reason: String.t option }
    let make ?stack_id  ~stack_name  ?template_description  ~creation_time 
      ?last_updated_time  ?deletion_time  ~stack_status  ?stack_status_reason
       () =
      {
        stack_id;
        stack_name;
        template_description;
        creation_time;
        last_updated_time;
        deletion_time;
        stack_status;
        stack_status_reason
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
               String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.stack_status_reason
              (fun f  ->
                 Query.Pair ("StackStatusReason", (String.to_query f)));
           Some
             (Query.Pair
                ("StackStatus", (StackStatus.to_query v.stack_status)));
           Util.option_map v.deletion_time
             (fun f  -> Query.Pair ("DeletionTime", (DateTime.to_query f)));
           Util.option_map v.last_updated_time
             (fun f  -> Query.Pair ("LastUpdatedTime", (DateTime.to_query f)));
           Some
             (Query.Pair
                ("CreationTime", (DateTime.to_query v.creation_time)));
           Util.option_map v.template_description
             (fun f  ->
                Query.Pair ("TemplateDescription", (String.to_query f)));
           Some (Query.Pair ("StackName", (String.to_query v.stack_name)));
           Util.option_map v.stack_id
             (fun f  -> Query.Pair ("StackId", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.stack_status_reason
              (fun f  -> ("stack_status_reason", (String.to_json f)));
           Some ("stack_status", (StackStatus.to_json v.stack_status));
           Util.option_map v.deletion_time
             (fun f  -> ("deletion_time", (DateTime.to_json f)));
           Util.option_map v.last_updated_time
             (fun f  -> ("last_updated_time", (DateTime.to_json f)));
           Some ("creation_time", (DateTime.to_json v.creation_time));
           Util.option_map v.template_description
             (fun f  -> ("template_description", (String.to_json f)));
           Some ("stack_name", (String.to_json v.stack_name));
           Util.option_map v.stack_id
             (fun f  -> ("stack_id", (String.to_json f)))])
      
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
             String.of_json)
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
              (fun f  ->
                 Query.Pair
                   ("ParameterConstraints",
                     (ParameterConstraints.to_query f)));
           Util.option_map v.description
             (fun f  -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.no_echo
             (fun f  -> Query.Pair ("NoEcho", (Boolean.to_query f)));
           Util.option_map v.parameter_type
             (fun f  -> Query.Pair ("ParameterType", (String.to_query f)));
           Util.option_map v.default_value
             (fun f  -> Query.Pair ("DefaultValue", (String.to_query f)));
           Util.option_map v.parameter_key
             (fun f  -> Query.Pair ("ParameterKey", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.parameter_constraints
              (fun f  ->
                 ("parameter_constraints", (ParameterConstraints.to_json f)));
           Util.option_map v.description
             (fun f  -> ("description", (String.to_json f)));
           Util.option_map v.no_echo
             (fun f  -> ("no_echo", (Boolean.to_json f)));
           Util.option_map v.parameter_type
             (fun f  -> ("parameter_type", (String.to_json f)));
           Util.option_map v.default_value
             (fun f  -> ("default_value", (String.to_json f)));
           Util.option_map v.parameter_key
             (fun f  -> ("parameter_key", (String.to_json f)))])
      
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
              (fun f  -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.no_echo
             (fun f  -> Query.Pair ("NoEcho", (Boolean.to_query f)));
           Util.option_map v.default_value
             (fun f  -> Query.Pair ("DefaultValue", (String.to_query f)));
           Util.option_map v.parameter_key
             (fun f  -> Query.Pair ("ParameterKey", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.description
              (fun f  -> ("description", (String.to_json f)));
           Util.option_map v.no_echo
             (fun f  -> ("no_echo", (Boolean.to_json f)));
           Util.option_map v.default_value
             (fun f  -> ("default_value", (String.to_json f)));
           Util.option_map v.parameter_key
             (fun f  -> ("parameter_key", (String.to_json f)))])
      
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
module Stack =
  struct
    type t =
      {
      stack_id: String.t option ;
      stack_name: String.t ;
      description: String.t option ;
      parameters: Parameters.t ;
      creation_time: DateTime.t ;
      last_updated_time: DateTime.t option ;
      stack_status: StackStatus.t ;
      stack_status_reason: String.t option ;
      disable_rollback: Boolean.t option ;
      notification_a_r_ns: NotificationARNs.t ;
      timeout_in_minutes: Integer.t option ;
      capabilities: Capabilities.t ;
      outputs: Outputs.t ;
      tags: Tags.t }
    let make ?stack_id  ~stack_name  ?description  ?(parameters= []) 
      ~creation_time  ?last_updated_time  ~stack_status  ?stack_status_reason
       ?disable_rollback  ?(notification_a_r_ns= [])  ?timeout_in_minutes 
      ?(capabilities= [])  ?(outputs= [])  ?(tags= [])  () =
      {
        stack_id;
        stack_name;
        description;
        parameters;
        creation_time;
        last_updated_time;
        stack_status;
        stack_status_reason;
        disable_rollback;
        notification_a_r_ns;
        timeout_in_minutes;
        capabilities;
        outputs;
        tags
      } 
    let parse xml =
      Some
        {
          stack_id =
            (Util.option_bind (Xml.member "StackId" xml) String.parse);
          stack_name =
            (Xml.required "StackName"
               (Util.option_bind (Xml.member "StackName" xml) String.parse));
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
          last_updated_time =
            (Util.option_bind (Xml.member "LastUpdatedTime" xml)
               DateTime.parse);
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
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) Tags.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Tags.member", (Tags.to_query v.tags)));
           Some (Query.Pair ("Outputs.member", (Outputs.to_query v.outputs)));
           Some
             (Query.Pair
                ("Capabilities.member",
                  (Capabilities.to_query v.capabilities)));
           Util.option_map v.timeout_in_minutes
             (fun f  -> Query.Pair ("TimeoutInMinutes", (Integer.to_query f)));
           Some
             (Query.Pair
                ("NotificationARNs.member",
                  (NotificationARNs.to_query v.notification_a_r_ns)));
           Util.option_map v.disable_rollback
             (fun f  -> Query.Pair ("DisableRollback", (Boolean.to_query f)));
           Util.option_map v.stack_status_reason
             (fun f  -> Query.Pair ("StackStatusReason", (String.to_query f)));
           Some
             (Query.Pair
                ("StackStatus", (StackStatus.to_query v.stack_status)));
           Util.option_map v.last_updated_time
             (fun f  -> Query.Pair ("LastUpdatedTime", (DateTime.to_query f)));
           Some
             (Query.Pair
                ("CreationTime", (DateTime.to_query v.creation_time)));
           Some
             (Query.Pair
                ("Parameters.member", (Parameters.to_query v.parameters)));
           Util.option_map v.description
             (fun f  -> Query.Pair ("Description", (String.to_query f)));
           Some (Query.Pair ("StackName", (String.to_query v.stack_name)));
           Util.option_map v.stack_id
             (fun f  -> Query.Pair ("StackId", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tags", (Tags.to_json v.tags));
           Some ("outputs", (Outputs.to_json v.outputs));
           Some ("capabilities", (Capabilities.to_json v.capabilities));
           Util.option_map v.timeout_in_minutes
             (fun f  -> ("timeout_in_minutes", (Integer.to_json f)));
           Some
             ("notification_a_r_ns",
               (NotificationARNs.to_json v.notification_a_r_ns));
           Util.option_map v.disable_rollback
             (fun f  -> ("disable_rollback", (Boolean.to_json f)));
           Util.option_map v.stack_status_reason
             (fun f  -> ("stack_status_reason", (String.to_json f)));
           Some ("stack_status", (StackStatus.to_json v.stack_status));
           Util.option_map v.last_updated_time
             (fun f  -> ("last_updated_time", (DateTime.to_json f)));
           Some ("creation_time", (DateTime.to_json v.creation_time));
           Some ("parameters", (Parameters.to_json v.parameters));
           Util.option_map v.description
             (fun f  -> ("description", (String.to_json f)));
           Some ("stack_name", (String.to_json v.stack_name));
           Util.option_map v.stack_id
             (fun f  -> ("stack_id", (String.to_json f)))])
      
    let of_json j =
      {
        stack_id =
          (Util.option_map (Json.lookup j "stack_id") String.of_json);
        stack_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "stack_name")));
        description =
          (Util.option_map (Json.lookup j "description") String.of_json);
        parameters =
          (Parameters.of_json
             (Util.of_option_exn (Json.lookup j "parameters")));
        creation_time =
          (DateTime.of_json
             (Util.of_option_exn (Json.lookup j "creation_time")));
        last_updated_time =
          (Util.option_map (Json.lookup j "last_updated_time")
             DateTime.of_json);
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
        tags = (Tags.of_json (Util.of_option_exn (Json.lookup j "tags")))
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
      resource_properties: String.t option }
    let make ~stack_id  ~event_id  ~stack_name  ?logical_resource_id 
      ?physical_resource_id  ?resource_type  ~timestamp  ?resource_status 
      ?resource_status_reason  ?resource_properties  () =
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
        resource_properties
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
               String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.resource_properties
              (fun f  ->
                 Query.Pair ("ResourceProperties", (String.to_query f)));
           Util.option_map v.resource_status_reason
             (fun f  ->
                Query.Pair ("ResourceStatusReason", (String.to_query f)));
           Util.option_map v.resource_status
             (fun f  ->
                Query.Pair ("ResourceStatus", (ResourceStatus.to_query f)));
           Some (Query.Pair ("Timestamp", (DateTime.to_query v.timestamp)));
           Util.option_map v.resource_type
             (fun f  -> Query.Pair ("ResourceType", (String.to_query f)));
           Util.option_map v.physical_resource_id
             (fun f  ->
                Query.Pair ("PhysicalResourceId", (String.to_query f)));
           Util.option_map v.logical_resource_id
             (fun f  -> Query.Pair ("LogicalResourceId", (String.to_query f)));
           Some (Query.Pair ("StackName", (String.to_query v.stack_name)));
           Some (Query.Pair ("EventId", (String.to_query v.event_id)));
           Some (Query.Pair ("StackId", (String.to_query v.stack_id)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.resource_properties
              (fun f  -> ("resource_properties", (String.to_json f)));
           Util.option_map v.resource_status_reason
             (fun f  -> ("resource_status_reason", (String.to_json f)));
           Util.option_map v.resource_status
             (fun f  -> ("resource_status", (ResourceStatus.to_json f)));
           Some ("timestamp", (DateTime.to_json v.timestamp));
           Util.option_map v.resource_type
             (fun f  -> ("resource_type", (String.to_json f)));
           Util.option_map v.physical_resource_id
             (fun f  -> ("physical_resource_id", (String.to_json f)));
           Util.option_map v.logical_resource_id
             (fun f  -> ("logical_resource_id", (String.to_json f)));
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
             String.of_json)
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
      description: String.t option }
    let make ?stack_name  ?stack_id  ~logical_resource_id 
      ?physical_resource_id  ~resource_type  ~timestamp  ~resource_status 
      ?resource_status_reason  ?description  () =
      {
        stack_name;
        stack_id;
        logical_resource_id;
        physical_resource_id;
        resource_type;
        timestamp;
        resource_status;
        resource_status_reason;
        description
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
            (Util.option_bind (Xml.member "Description" xml) String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.description
              (fun f  -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.resource_status_reason
             (fun f  ->
                Query.Pair ("ResourceStatusReason", (String.to_query f)));
           Some
             (Query.Pair
                ("ResourceStatus",
                  (ResourceStatus.to_query v.resource_status)));
           Some (Query.Pair ("Timestamp", (DateTime.to_query v.timestamp)));
           Some
             (Query.Pair ("ResourceType", (String.to_query v.resource_type)));
           Util.option_map v.physical_resource_id
             (fun f  ->
                Query.Pair ("PhysicalResourceId", (String.to_query f)));
           Some
             (Query.Pair
                ("LogicalResourceId",
                  (String.to_query v.logical_resource_id)));
           Util.option_map v.stack_id
             (fun f  -> Query.Pair ("StackId", (String.to_query f)));
           Util.option_map v.stack_name
             (fun f  -> Query.Pair ("StackName", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.description
              (fun f  -> ("description", (String.to_json f)));
           Util.option_map v.resource_status_reason
             (fun f  -> ("resource_status_reason", (String.to_json f)));
           Some
             ("resource_status", (ResourceStatus.to_json v.resource_status));
           Some ("timestamp", (DateTime.to_json v.timestamp));
           Some ("resource_type", (String.to_json v.resource_type));
           Util.option_map v.physical_resource_id
             (fun f  -> ("physical_resource_id", (String.to_json f)));
           Some
             ("logical_resource_id", (String.to_json v.logical_resource_id));
           Util.option_map v.stack_id
             (fun f  -> ("stack_id", (String.to_json f)));
           Util.option_map v.stack_name
             (fun f  -> ("stack_name", (String.to_json f)))])
      
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
          (Util.option_map (Json.lookup j "description") String.of_json)
      } 
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
module ResourceSignalStatus =
  struct
    type t =
      | SUCCESS 
      | FAILURE 
    let str_to_t = [("FAILURE", FAILURE); ("SUCCESS", SUCCESS)] 
    let t_to_str = [(FAILURE, "FAILURE"); (SUCCESS, "SUCCESS")] 
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
      metadata: String.t option }
    let make ?stack_name  ?stack_id  ~logical_resource_id 
      ?physical_resource_id  ~resource_type  ~last_updated_timestamp 
      ~resource_status  ?resource_status_reason  ?description  ?metadata  ()
      =
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
        metadata
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
            (Util.option_bind (Xml.member "Metadata" xml) String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.metadata
              (fun f  -> Query.Pair ("Metadata", (String.to_query f)));
           Util.option_map v.description
             (fun f  -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.resource_status_reason
             (fun f  ->
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
             (fun f  ->
                Query.Pair ("PhysicalResourceId", (String.to_query f)));
           Some
             (Query.Pair
                ("LogicalResourceId",
                  (String.to_query v.logical_resource_id)));
           Util.option_map v.stack_id
             (fun f  -> Query.Pair ("StackId", (String.to_query f)));
           Util.option_map v.stack_name
             (fun f  -> Query.Pair ("StackName", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.metadata
              (fun f  -> ("metadata", (String.to_json f)));
           Util.option_map v.description
             (fun f  -> ("description", (String.to_json f)));
           Util.option_map v.resource_status_reason
             (fun f  -> ("resource_status_reason", (String.to_json f)));
           Some
             ("resource_status", (ResourceStatus.to_json v.resource_status));
           Some
             ("last_updated_timestamp",
               (DateTime.to_json v.last_updated_timestamp));
           Some ("resource_type", (String.to_json v.resource_type));
           Util.option_map v.physical_resource_id
             (fun f  -> ("physical_resource_id", (String.to_json f)));
           Some
             ("logical_resource_id", (String.to_json v.logical_resource_id));
           Util.option_map v.stack_id
             (fun f  -> ("stack_id", (String.to_json f)));
           Util.option_map v.stack_name
             (fun f  -> ("stack_name", (String.to_json f)))])
      
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
          (Util.option_map (Json.lookup j "metadata") String.of_json)
      } 
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
              (fun f  -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair
                ("StackResourceSummaries.member",
                  (StackResourceSummaries.to_query v.stack_resource_summaries)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f  -> ("next_token", (String.to_json f)));
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
              (fun f  -> Query.Pair ("StackPolicyURL", (String.to_query f)));
           Util.option_map v.stack_policy_body
             (fun f  -> Query.Pair ("StackPolicyBody", (String.to_query f)));
           Some (Query.Pair ("StackName", (String.to_query v.stack_name)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.stack_policy_u_r_l
              (fun f  -> ("stack_policy_u_r_l", (String.to_json f)));
           Util.option_map v.stack_policy_body
             (fun f  -> ("stack_policy_body", (String.to_json f)));
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
              (fun f  -> Query.Pair ("StackId", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.stack_id
              (fun f  -> ("stack_id", (String.to_json f)))])
      
    let of_json j =
      {
        stack_id =
          (Util.option_map (Json.lookup j "stack_id") String.of_json)
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
              (fun f  -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair
                ("StackSummaries.member",
                  (StackSummaries.to_query v.stack_summaries)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f  -> ("next_token", (String.to_json f)));
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
module GetTemplateInput =
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
              (fun f  -> Query.Pair ("Url", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.url (fun f  -> ("url", (String.to_json f)))])
      
    let of_json j =
      { url = (Util.option_map (Json.lookup j "url") String.of_json) } 
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
             (fun f  -> Query.Pair ("NextToken", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("stack_status_filter",
                (StackStatusFilter.to_json v.stack_status_filter));
           Util.option_map v.next_token
             (fun f  -> ("next_token", (String.to_json f)))])
      
    let of_json j =
      {
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json);
        stack_status_filter =
          (StackStatusFilter.of_json
             (Util.of_option_exn (Json.lookup j "stack_status_filter")))
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
              (fun f  -> Query.Pair ("StackPolicyBody", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.stack_policy_body
              (fun f  -> ("stack_policy_body", (String.to_json f)))])
      
    let of_json j =
      {
        stack_policy_body =
          (Util.option_map (Json.lookup j "stack_policy_body") String.of_json)
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
              (fun f  ->
                 Query.Pair ("PhysicalResourceId", (String.to_query f)));
           Util.option_map v.logical_resource_id
             (fun f  -> Query.Pair ("LogicalResourceId", (String.to_query f)));
           Util.option_map v.stack_name
             (fun f  -> Query.Pair ("StackName", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.physical_resource_id
              (fun f  -> ("physical_resource_id", (String.to_json f)));
           Util.option_map v.logical_resource_id
             (fun f  -> ("logical_resource_id", (String.to_json f)));
           Util.option_map v.stack_name
             (fun f  -> ("stack_name", (String.to_json f)))])
      
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
module DeleteStackInput =
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
module GetTemplateSummaryOutput =
  struct
    type t =
      {
      parameters: ParameterDeclarations.t ;
      description: String.t option ;
      capabilities: Capabilities.t ;
      capabilities_reason: String.t option ;
      version: String.t option ;
      metadata: String.t option }
    let make ?(parameters= [])  ?description  ?(capabilities= []) 
      ?capabilities_reason  ?version  ?metadata  () =
      {
        parameters;
        description;
        capabilities;
        capabilities_reason;
        version;
        metadata
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
          version =
            (Util.option_bind (Xml.member "Version" xml) String.parse);
          metadata =
            (Util.option_bind (Xml.member "Metadata" xml) String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.metadata
              (fun f  -> Query.Pair ("Metadata", (String.to_query f)));
           Util.option_map v.version
             (fun f  -> Query.Pair ("Version", (String.to_query f)));
           Util.option_map v.capabilities_reason
             (fun f  ->
                Query.Pair ("CapabilitiesReason", (String.to_query f)));
           Some
             (Query.Pair
                ("Capabilities.member",
                  (Capabilities.to_query v.capabilities)));
           Util.option_map v.description
             (fun f  -> Query.Pair ("Description", (String.to_query f)));
           Some
             (Query.Pair
                ("Parameters.member",
                  (ParameterDeclarations.to_query v.parameters)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.metadata
              (fun f  -> ("metadata", (String.to_json f)));
           Util.option_map v.version
             (fun f  -> ("version", (String.to_json f)));
           Util.option_map v.capabilities_reason
             (fun f  -> ("capabilities_reason", (String.to_json f)));
           Some ("capabilities", (Capabilities.to_json v.capabilities));
           Util.option_map v.description
             (fun f  -> ("description", (String.to_json f)));
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
        version = (Util.option_map (Json.lookup j "version") String.of_json);
        metadata =
          (Util.option_map (Json.lookup j "metadata") String.of_json)
      } 
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
              (fun f  -> Query.Pair ("NextToken", (String.to_query f)));
           Util.option_map v.stack_name
             (fun f  -> Query.Pair ("StackName", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f  -> ("next_token", (String.to_json f)));
           Util.option_map v.stack_name
             (fun f  -> ("stack_name", (String.to_json f)))])
      
    let of_json j =
      {
        stack_name =
          (Util.option_map (Json.lookup j "stack_name") String.of_json);
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
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
              (fun f  -> Query.Pair ("TemplateURL", (String.to_query f)));
           Util.option_map v.template_body
             (fun f  -> Query.Pair ("TemplateBody", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.template_u_r_l
              (fun f  -> ("template_u_r_l", (String.to_json f)));
           Util.option_map v.template_body
             (fun f  -> ("template_body", (String.to_json f)))])
      
    let of_json j =
      {
        template_body =
          (Util.option_map (Json.lookup j "template_body") String.of_json);
        template_u_r_l =
          (Util.option_map (Json.lookup j "template_u_r_l") String.of_json)
      } 
  end
module ValidateTemplateOutput =
  struct
    type t =
      {
      parameters: TemplateParameters.t ;
      description: String.t option ;
      capabilities: Capabilities.t ;
      capabilities_reason: String.t option }
    let make ?(parameters= [])  ?description  ?(capabilities= []) 
      ?capabilities_reason  () =
      { parameters; description; capabilities; capabilities_reason } 
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
               String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.capabilities_reason
              (fun f  ->
                 Query.Pair ("CapabilitiesReason", (String.to_query f)));
           Some
             (Query.Pair
                ("Capabilities.member",
                  (Capabilities.to_query v.capabilities)));
           Util.option_map v.description
             (fun f  -> Query.Pair ("Description", (String.to_query f)));
           Some
             (Query.Pair
                ("Parameters.member",
                  (TemplateParameters.to_query v.parameters)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.capabilities_reason
              (fun f  -> ("capabilities_reason", (String.to_json f)));
           Some ("capabilities", (Capabilities.to_json v.capabilities));
           Util.option_map v.description
             (fun f  -> ("description", (String.to_json f)));
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
             String.of_json)
      } 
  end
module GetTemplateSummaryInput =
  struct
    type t =
      {
      template_body: String.t option ;
      template_u_r_l: String.t option ;
      stack_name: String.t option }
    let make ?template_body  ?template_u_r_l  ?stack_name  () =
      { template_body; template_u_r_l; stack_name } 
    let parse xml =
      Some
        {
          template_body =
            (Util.option_bind (Xml.member "TemplateBody" xml) String.parse);
          template_u_r_l =
            (Util.option_bind (Xml.member "TemplateURL" xml) String.parse);
          stack_name =
            (Util.option_bind (Xml.member "StackName" xml) String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.stack_name
              (fun f  -> Query.Pair ("StackName", (String.to_query f)));
           Util.option_map v.template_u_r_l
             (fun f  -> Query.Pair ("TemplateURL", (String.to_query f)));
           Util.option_map v.template_body
             (fun f  -> Query.Pair ("TemplateBody", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.stack_name
              (fun f  -> ("stack_name", (String.to_json f)));
           Util.option_map v.template_u_r_l
             (fun f  -> ("template_u_r_l", (String.to_json f)));
           Util.option_map v.template_body
             (fun f  -> ("template_body", (String.to_json f)))])
      
    let of_json j =
      {
        template_body =
          (Util.option_map (Json.lookup j "template_body") String.of_json);
        template_u_r_l =
          (Util.option_map (Json.lookup j "template_u_r_l") String.of_json);
        stack_name =
          (Util.option_map (Json.lookup j "stack_name") String.of_json)
      } 
  end
module CancelUpdateStackInput =
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
              (fun f  -> Query.Pair ("StackId", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.stack_id
              (fun f  -> ("stack_id", (String.to_json f)))])
      
    let of_json j =
      {
        stack_id =
          (Util.option_map (Json.lookup j "stack_id") String.of_json)
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
              (fun f  -> Query.Pair ("NextToken", (String.to_query f)));
           Some (Query.Pair ("Stacks.member", (Stacks.to_query v.stacks)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f  -> ("next_token", (String.to_json f)));
           Some ("stacks", (Stacks.to_json v.stacks))])
      
    let of_json j =
      {
        stacks =
          (Stacks.of_json (Util.of_option_exn (Json.lookup j "stacks")));
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
              (fun f  ->
                 Query.Pair
                   ("StackResourceDetail", (StackResourceDetail.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.stack_resource_detail
              (fun f  ->
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
              (fun f  -> Query.Pair ("NextToken", (String.to_query f)));
           Util.option_map v.stack_name
             (fun f  -> Query.Pair ("StackName", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f  -> ("next_token", (String.to_json f)));
           Util.option_map v.stack_name
             (fun f  -> ("stack_name", (String.to_json f)))])
      
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
             (fun f  -> Query.Pair ("TemplateURL", (String.to_query f)));
           Util.option_map v.template_body
             (fun f  -> Query.Pair ("TemplateBody", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("parameters", (Parameters.to_json v.parameters));
           Util.option_map v.template_u_r_l
             (fun f  -> ("template_u_r_l", (String.to_json f)));
           Util.option_map v.template_body
             (fun f  -> ("template_body", (String.to_json f)))])
      
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
      stack_policy_body: String.t option ;
      stack_policy_u_r_l: String.t option ;
      notification_a_r_ns: NotificationARNs.t }
    let make ~stack_name  ?template_body  ?template_u_r_l 
      ?use_previous_template  ?stack_policy_during_update_body 
      ?stack_policy_during_update_u_r_l  ?(parameters= [])  ?(capabilities=
      [])  ?stack_policy_body  ?stack_policy_u_r_l  ?(notification_a_r_ns=
      [])  () =
      {
        stack_name;
        template_body;
        template_u_r_l;
        use_previous_template;
        stack_policy_during_update_body;
        stack_policy_during_update_u_r_l;
        parameters;
        capabilities;
        stack_policy_body;
        stack_policy_u_r_l;
        notification_a_r_ns
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
          stack_policy_body =
            (Util.option_bind (Xml.member "StackPolicyBody" xml) String.parse);
          stack_policy_u_r_l =
            (Util.option_bind (Xml.member "StackPolicyURL" xml) String.parse);
          notification_a_r_ns =
            (Util.of_option []
               (Util.option_bind (Xml.member "NotificationARNs" xml)
                  NotificationARNs.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("NotificationARNs.member",
                   (NotificationARNs.to_query v.notification_a_r_ns)));
           Util.option_map v.stack_policy_u_r_l
             (fun f  -> Query.Pair ("StackPolicyURL", (String.to_query f)));
           Util.option_map v.stack_policy_body
             (fun f  -> Query.Pair ("StackPolicyBody", (String.to_query f)));
           Some
             (Query.Pair
                ("Capabilities.member",
                  (Capabilities.to_query v.capabilities)));
           Some
             (Query.Pair
                ("Parameters.member", (Parameters.to_query v.parameters)));
           Util.option_map v.stack_policy_during_update_u_r_l
             (fun f  ->
                Query.Pair
                  ("StackPolicyDuringUpdateURL", (String.to_query f)));
           Util.option_map v.stack_policy_during_update_body
             (fun f  ->
                Query.Pair
                  ("StackPolicyDuringUpdateBody", (String.to_query f)));
           Util.option_map v.use_previous_template
             (fun f  ->
                Query.Pair ("UsePreviousTemplate", (Boolean.to_query f)));
           Util.option_map v.template_u_r_l
             (fun f  -> Query.Pair ("TemplateURL", (String.to_query f)));
           Util.option_map v.template_body
             (fun f  -> Query.Pair ("TemplateBody", (String.to_query f)));
           Some (Query.Pair ("StackName", (String.to_query v.stack_name)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("notification_a_r_ns",
                (NotificationARNs.to_json v.notification_a_r_ns));
           Util.option_map v.stack_policy_u_r_l
             (fun f  -> ("stack_policy_u_r_l", (String.to_json f)));
           Util.option_map v.stack_policy_body
             (fun f  -> ("stack_policy_body", (String.to_json f)));
           Some ("capabilities", (Capabilities.to_json v.capabilities));
           Some ("parameters", (Parameters.to_json v.parameters));
           Util.option_map v.stack_policy_during_update_u_r_l
             (fun f  ->
                ("stack_policy_during_update_u_r_l", (String.to_json f)));
           Util.option_map v.stack_policy_during_update_body
             (fun f  ->
                ("stack_policy_during_update_body", (String.to_json f)));
           Util.option_map v.use_previous_template
             (fun f  -> ("use_previous_template", (Boolean.to_json f)));
           Util.option_map v.template_u_r_l
             (fun f  -> ("template_u_r_l", (String.to_json f)));
           Util.option_map v.template_body
             (fun f  -> ("template_body", (String.to_json f)));
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
        stack_policy_body =
          (Util.option_map (Json.lookup j "stack_policy_body") String.of_json);
        stack_policy_u_r_l =
          (Util.option_map (Json.lookup j "stack_policy_u_r_l")
             String.of_json);
        notification_a_r_ns =
          (NotificationARNs.of_json
             (Util.of_option_exn (Json.lookup j "notification_a_r_ns")))
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
      timeout_in_minutes: Integer.t option ;
      notification_a_r_ns: NotificationARNs.t ;
      capabilities: Capabilities.t ;
      on_failure: OnFailure.t option ;
      stack_policy_body: String.t option ;
      stack_policy_u_r_l: String.t option ;
      tags: Tags.t }
    let make ~stack_name  ?template_body  ?template_u_r_l  ?(parameters= []) 
      ?disable_rollback  ?timeout_in_minutes  ?(notification_a_r_ns= []) 
      ?(capabilities= [])  ?on_failure  ?stack_policy_body 
      ?stack_policy_u_r_l  ?(tags= [])  () =
      {
        stack_name;
        template_body;
        template_u_r_l;
        parameters;
        disable_rollback;
        timeout_in_minutes;
        notification_a_r_ns;
        capabilities;
        on_failure;
        stack_policy_body;
        stack_policy_u_r_l;
        tags
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
          on_failure =
            (Util.option_bind (Xml.member "OnFailure" xml) OnFailure.parse);
          stack_policy_body =
            (Util.option_bind (Xml.member "StackPolicyBody" xml) String.parse);
          stack_policy_u_r_l =
            (Util.option_bind (Xml.member "StackPolicyURL" xml) String.parse);
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) Tags.parse))
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Tags.member", (Tags.to_query v.tags)));
           Util.option_map v.stack_policy_u_r_l
             (fun f  -> Query.Pair ("StackPolicyURL", (String.to_query f)));
           Util.option_map v.stack_policy_body
             (fun f  -> Query.Pair ("StackPolicyBody", (String.to_query f)));
           Util.option_map v.on_failure
             (fun f  -> Query.Pair ("OnFailure", (OnFailure.to_query f)));
           Some
             (Query.Pair
                ("Capabilities.member",
                  (Capabilities.to_query v.capabilities)));
           Some
             (Query.Pair
                ("NotificationARNs.member",
                  (NotificationARNs.to_query v.notification_a_r_ns)));
           Util.option_map v.timeout_in_minutes
             (fun f  -> Query.Pair ("TimeoutInMinutes", (Integer.to_query f)));
           Util.option_map v.disable_rollback
             (fun f  -> Query.Pair ("DisableRollback", (Boolean.to_query f)));
           Some
             (Query.Pair
                ("Parameters.member", (Parameters.to_query v.parameters)));
           Util.option_map v.template_u_r_l
             (fun f  -> Query.Pair ("TemplateURL", (String.to_query f)));
           Util.option_map v.template_body
             (fun f  -> Query.Pair ("TemplateBody", (String.to_query f)));
           Some (Query.Pair ("StackName", (String.to_query v.stack_name)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tags", (Tags.to_json v.tags));
           Util.option_map v.stack_policy_u_r_l
             (fun f  -> ("stack_policy_u_r_l", (String.to_json f)));
           Util.option_map v.stack_policy_body
             (fun f  -> ("stack_policy_body", (String.to_json f)));
           Util.option_map v.on_failure
             (fun f  -> ("on_failure", (OnFailure.to_json f)));
           Some ("capabilities", (Capabilities.to_json v.capabilities));
           Some
             ("notification_a_r_ns",
               (NotificationARNs.to_json v.notification_a_r_ns));
           Util.option_map v.timeout_in_minutes
             (fun f  -> ("timeout_in_minutes", (Integer.to_json f)));
           Util.option_map v.disable_rollback
             (fun f  -> ("disable_rollback", (Boolean.to_json f)));
           Some ("parameters", (Parameters.to_json v.parameters));
           Util.option_map v.template_u_r_l
             (fun f  -> ("template_u_r_l", (String.to_json f)));
           Util.option_map v.template_body
             (fun f  -> ("template_body", (String.to_json f)));
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
        timeout_in_minutes =
          (Util.option_map (Json.lookup j "timeout_in_minutes")
             Integer.of_json);
        notification_a_r_ns =
          (NotificationARNs.of_json
             (Util.of_option_exn (Json.lookup j "notification_a_r_ns")));
        capabilities =
          (Capabilities.of_json
             (Util.of_option_exn (Json.lookup j "capabilities")));
        on_failure =
          (Util.option_map (Json.lookup j "on_failure") OnFailure.of_json);
        stack_policy_body =
          (Util.option_map (Json.lookup j "stack_policy_body") String.of_json);
        stack_policy_u_r_l =
          (Util.option_map (Json.lookup j "stack_policy_u_r_l")
             String.of_json);
        tags = (Tags.of_json (Util.of_option_exn (Json.lookup j "tags")))
      } 
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
              (fun f  -> Query.Pair ("NextToken", (String.to_query f)));
           Some
             (Query.Pair
                ("StackEvents.member", (StackEvents.to_query v.stack_events)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f  -> ("next_token", (String.to_json f)));
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
              (fun f  -> Query.Pair ("NextToken", (String.to_query f)));
           Some (Query.Pair ("StackName", (String.to_query v.stack_name)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.next_token
              (fun f  -> ("next_token", (String.to_json f)));
           Some ("stack_name", (String.to_json v.stack_name))])
      
    let of_json j =
      {
        stack_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "stack_name")));
        next_token =
          (Util.option_map (Json.lookup j "next_token") String.of_json)
      } 
  end
module GetTemplateOutput =
  struct
    type t = {
      template_body: String.t option }
    let make ?template_body  () = { template_body } 
    let parse xml =
      Some
        {
          template_body =
            (Util.option_bind (Xml.member "TemplateBody" xml) String.parse)
        }
      
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.template_body
              (fun f  -> Query.Pair ("TemplateBody", (String.to_query f)))])
      
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.template_body
              (fun f  -> ("template_body", (String.to_json f)))])
      
    let of_json j =
      {
        template_body =
          (Util.option_map (Json.lookup j "template_body") String.of_json)
      } 
  end