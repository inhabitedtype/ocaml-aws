open Aws.BaseTypes

type calendar = CalendarLib.Calendar.t

module UpdateStackSetOutput = struct
  type t = { operation_id : String.t option }

  let make ?operation_id () = { operation_id }

  let parse xml =
    Some
      { operation_id =
          Aws.Util.option_bind (Aws.Xml.member "OperationId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.operation_id (fun f ->
               Aws.Query.Pair ("OperationId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.operation_id (fun f -> "OperationId", String.to_json f) ])

  let of_json j =
    { operation_id = Aws.Util.option_map (Aws.Json.lookup j "OperationId") String.of_json
    }
end

module RetainResources = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module RollbackTrigger = struct
  type t =
    { arn : String.t
    ; type_ : String.t
    }

  let make ~arn ~type_ () = { arn; type_ }

  let parse xml =
    Some
      { arn =
          Aws.Xml.required
            "Arn"
            (Aws.Util.option_bind (Aws.Xml.member "Arn" xml) String.parse)
      ; type_ =
          Aws.Xml.required
            "Type"
            (Aws.Util.option_bind (Aws.Xml.member "Type" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Type", String.to_query v.type_))
         ; Some (Aws.Query.Pair ("Arn", String.to_query v.arn))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Type", String.to_json v.type_); Some ("Arn", String.to_json v.arn) ])

  let of_json j =
    { arn = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Arn"))
    ; type_ = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Type"))
    }
end

module RollbackTriggers = struct
  type t = RollbackTrigger.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map RollbackTrigger.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list RollbackTrigger.to_query v

  let to_json v = `List (List.map RollbackTrigger.to_json v)

  let of_json j = Aws.Json.to_list RollbackTrigger.of_json j
end

module UpdateTerminationProtectionInput = struct
  type t =
    { enable_termination_protection : Boolean.t
    ; stack_name : String.t
    }

  let make ~enable_termination_protection ~stack_name () =
    { enable_termination_protection; stack_name }

  let parse xml =
    Some
      { enable_termination_protection =
          Aws.Xml.required
            "EnableTerminationProtection"
            (Aws.Util.option_bind
               (Aws.Xml.member "EnableTerminationProtection" xml)
               Boolean.parse)
      ; stack_name =
          Aws.Xml.required
            "StackName"
            (Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("StackName", String.to_query v.stack_name))
         ; Some
             (Aws.Query.Pair
                ( "EnableTerminationProtection"
                , Boolean.to_query v.enable_termination_protection ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("StackName", String.to_json v.stack_name)
         ; Some
             ( "EnableTerminationProtection"
             , Boolean.to_json v.enable_termination_protection )
         ])

  let of_json j =
    { enable_termination_protection =
        Boolean.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "EnableTerminationProtection"))
    ; stack_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackName"))
    }
end

module StackSetOperationResultStatus = struct
  type t =
    | PENDING
    | RUNNING
    | SUCCEEDED
    | FAILED
    | CANCELLED

  let str_to_t =
    [ "CANCELLED", CANCELLED
    ; "FAILED", FAILED
    ; "SUCCEEDED", SUCCEEDED
    ; "RUNNING", RUNNING
    ; "PENDING", PENDING
    ]

  let t_to_str =
    [ CANCELLED, "CANCELLED"
    ; FAILED, "FAILED"
    ; SUCCEEDED, "SUCCEEDED"
    ; RUNNING, "RUNNING"
    ; PENDING, "PENDING"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module AccountGateStatus = struct
  type t =
    | SUCCEEDED
    | FAILED
    | SKIPPED

  let str_to_t = [ "SKIPPED", SKIPPED; "FAILED", FAILED; "SUCCEEDED", SUCCEEDED ]

  let t_to_str = [ SKIPPED, "SKIPPED"; FAILED, "FAILED"; SUCCEEDED, "SUCCEEDED" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module AccountGateResult = struct
  type t =
    { status : AccountGateStatus.t option
    ; status_reason : String.t option
    }

  let make ?status ?status_reason () = { status; status_reason }

  let parse xml =
    Some
      { status =
          Aws.Util.option_bind (Aws.Xml.member "Status" xml) AccountGateStatus.parse
      ; status_reason =
          Aws.Util.option_bind (Aws.Xml.member "StatusReason" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.status_reason (fun f ->
               Aws.Query.Pair ("StatusReason", String.to_query f))
         ; Aws.Util.option_map v.status (fun f ->
               Aws.Query.Pair ("Status", AccountGateStatus.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.status_reason (fun f -> "StatusReason", String.to_json f)
         ; Aws.Util.option_map v.status (fun f -> "Status", AccountGateStatus.to_json f)
         ])

  let of_json j =
    { status = Aws.Util.option_map (Aws.Json.lookup j "Status") AccountGateStatus.of_json
    ; status_reason =
        Aws.Util.option_map (Aws.Json.lookup j "StatusReason") String.of_json
    }
end

module StackSetOperationResultSummary = struct
  type t =
    { account : String.t option
    ; region : String.t option
    ; status : StackSetOperationResultStatus.t option
    ; status_reason : String.t option
    ; account_gate_result : AccountGateResult.t option
    ; organizational_unit_id : String.t option
    }

  let make
      ?account
      ?region
      ?status
      ?status_reason
      ?account_gate_result
      ?organizational_unit_id
      () =
    { account
    ; region
    ; status
    ; status_reason
    ; account_gate_result
    ; organizational_unit_id
    }

  let parse xml =
    Some
      { account = Aws.Util.option_bind (Aws.Xml.member "Account" xml) String.parse
      ; region = Aws.Util.option_bind (Aws.Xml.member "Region" xml) String.parse
      ; status =
          Aws.Util.option_bind
            (Aws.Xml.member "Status" xml)
            StackSetOperationResultStatus.parse
      ; status_reason =
          Aws.Util.option_bind (Aws.Xml.member "StatusReason" xml) String.parse
      ; account_gate_result =
          Aws.Util.option_bind
            (Aws.Xml.member "AccountGateResult" xml)
            AccountGateResult.parse
      ; organizational_unit_id =
          Aws.Util.option_bind (Aws.Xml.member "OrganizationalUnitId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.organizational_unit_id (fun f ->
               Aws.Query.Pair ("OrganizationalUnitId", String.to_query f))
         ; Aws.Util.option_map v.account_gate_result (fun f ->
               Aws.Query.Pair ("AccountGateResult", AccountGateResult.to_query f))
         ; Aws.Util.option_map v.status_reason (fun f ->
               Aws.Query.Pair ("StatusReason", String.to_query f))
         ; Aws.Util.option_map v.status (fun f ->
               Aws.Query.Pair ("Status", StackSetOperationResultStatus.to_query f))
         ; Aws.Util.option_map v.region (fun f ->
               Aws.Query.Pair ("Region", String.to_query f))
         ; Aws.Util.option_map v.account (fun f ->
               Aws.Query.Pair ("Account", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.organizational_unit_id (fun f ->
               "OrganizationalUnitId", String.to_json f)
         ; Aws.Util.option_map v.account_gate_result (fun f ->
               "AccountGateResult", AccountGateResult.to_json f)
         ; Aws.Util.option_map v.status_reason (fun f -> "StatusReason", String.to_json f)
         ; Aws.Util.option_map v.status (fun f ->
               "Status", StackSetOperationResultStatus.to_json f)
         ; Aws.Util.option_map v.region (fun f -> "Region", String.to_json f)
         ; Aws.Util.option_map v.account (fun f -> "Account", String.to_json f)
         ])

  let of_json j =
    { account = Aws.Util.option_map (Aws.Json.lookup j "Account") String.of_json
    ; region = Aws.Util.option_map (Aws.Json.lookup j "Region") String.of_json
    ; status =
        Aws.Util.option_map
          (Aws.Json.lookup j "Status")
          StackSetOperationResultStatus.of_json
    ; status_reason =
        Aws.Util.option_map (Aws.Json.lookup j "StatusReason") String.of_json
    ; account_gate_result =
        Aws.Util.option_map
          (Aws.Json.lookup j "AccountGateResult")
          AccountGateResult.of_json
    ; organizational_unit_id =
        Aws.Util.option_map (Aws.Json.lookup j "OrganizationalUnitId") String.of_json
    }
end

module TemplateStage = struct
  type t =
    | Original
    | Processed

  let str_to_t = [ "Processed", Processed; "Original", Original ]

  let t_to_str = [ Processed, "Processed"; Original, "Original" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module StageList = struct
  type t = TemplateStage.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map TemplateStage.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list TemplateStage.to_query v

  let to_json v = `List (List.map TemplateStage.to_json v)

  let of_json j = Aws.Json.to_list TemplateStage.of_json j
end

module GetTemplateOutput = struct
  type t =
    { template_body : String.t option
    ; stages_available : StageList.t
    }

  let make ?template_body ?(stages_available = []) () =
    { template_body; stages_available }

  let parse xml =
    Some
      { template_body =
          Aws.Util.option_bind (Aws.Xml.member "TemplateBody" xml) String.parse
      ; stages_available =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "StagesAvailable" xml) StageList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("StagesAvailable.member", StageList.to_query v.stages_available))
         ; Aws.Util.option_map v.template_body (fun f ->
               Aws.Query.Pair ("TemplateBody", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("StagesAvailable", StageList.to_json v.stages_available)
         ; Aws.Util.option_map v.template_body (fun f -> "TemplateBody", String.to_json f)
         ])

  let of_json j =
    { template_body =
        Aws.Util.option_map (Aws.Json.lookup j "TemplateBody") String.of_json
    ; stages_available =
        StageList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StagesAvailable"))
    }
end

module AllowedValues = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module ParameterConstraints = struct
  type t = { allowed_values : AllowedValues.t }

  let make ?(allowed_values = []) () = { allowed_values }

  let parse xml =
    Some
      { allowed_values =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "AllowedValues" xml)
               AllowedValues.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("AllowedValues.member", AllowedValues.to_query v.allowed_values))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("AllowedValues", AllowedValues.to_json v.allowed_values) ])

  let of_json j =
    { allowed_values =
        AllowedValues.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AllowedValues"))
    }
end

module DescribeStackDriftDetectionStatusInput = struct
  type t = { stack_drift_detection_id : String.t }

  let make ~stack_drift_detection_id () = { stack_drift_detection_id }

  let parse xml =
    Some
      { stack_drift_detection_id =
          Aws.Xml.required
            "StackDriftDetectionId"
            (Aws.Util.option_bind
               (Aws.Xml.member "StackDriftDetectionId" xml)
               String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("StackDriftDetectionId", String.to_query v.stack_drift_detection_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("StackDriftDetectionId", String.to_json v.stack_drift_detection_id) ])

  let of_json j =
    { stack_drift_detection_id =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "StackDriftDetectionId"))
    }
end

module RegionList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module ListStackResourcesInput = struct
  type t =
    { stack_name : String.t
    ; next_token : String.t option
    }

  let make ~stack_name ?next_token () = { stack_name; next_token }

  let parse xml =
    Some
      { stack_name =
          Aws.Xml.required
            "StackName"
            (Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some (Aws.Query.Pair ("StackName", String.to_query v.stack_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("StackName", String.to_json v.stack_name)
         ])

  let of_json j =
    { stack_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackName"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module StackResourceDriftStatus = struct
  type t =
    | IN_SYNC
    | MODIFIED
    | DELETED
    | NOT_CHECKED

  let str_to_t =
    [ "NOT_CHECKED", NOT_CHECKED
    ; "DELETED", DELETED
    ; "MODIFIED", MODIFIED
    ; "IN_SYNC", IN_SYNC
    ]

  let t_to_str =
    [ NOT_CHECKED, "NOT_CHECKED"
    ; DELETED, "DELETED"
    ; MODIFIED, "MODIFIED"
    ; IN_SYNC, "IN_SYNC"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module DifferenceType = struct
  type t =
    | ADD
    | REMOVE
    | NOT_EQUAL

  let str_to_t = [ "NOT_EQUAL", NOT_EQUAL; "REMOVE", REMOVE; "ADD", ADD ]

  let t_to_str = [ NOT_EQUAL, "NOT_EQUAL"; REMOVE, "REMOVE"; ADD, "ADD" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module PropertyDifference = struct
  type t =
    { property_path : String.t
    ; expected_value : String.t
    ; actual_value : String.t
    ; difference_type : DifferenceType.t
    }

  let make ~property_path ~expected_value ~actual_value ~difference_type () =
    { property_path; expected_value; actual_value; difference_type }

  let parse xml =
    Some
      { property_path =
          Aws.Xml.required
            "PropertyPath"
            (Aws.Util.option_bind (Aws.Xml.member "PropertyPath" xml) String.parse)
      ; expected_value =
          Aws.Xml.required
            "ExpectedValue"
            (Aws.Util.option_bind (Aws.Xml.member "ExpectedValue" xml) String.parse)
      ; actual_value =
          Aws.Xml.required
            "ActualValue"
            (Aws.Util.option_bind (Aws.Xml.member "ActualValue" xml) String.parse)
      ; difference_type =
          Aws.Xml.required
            "DifferenceType"
            (Aws.Util.option_bind
               (Aws.Xml.member "DifferenceType" xml)
               DifferenceType.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("DifferenceType", DifferenceType.to_query v.difference_type))
         ; Some (Aws.Query.Pair ("ActualValue", String.to_query v.actual_value))
         ; Some (Aws.Query.Pair ("ExpectedValue", String.to_query v.expected_value))
         ; Some (Aws.Query.Pair ("PropertyPath", String.to_query v.property_path))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("DifferenceType", DifferenceType.to_json v.difference_type)
         ; Some ("ActualValue", String.to_json v.actual_value)
         ; Some ("ExpectedValue", String.to_json v.expected_value)
         ; Some ("PropertyPath", String.to_json v.property_path)
         ])

  let of_json j =
    { property_path =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PropertyPath"))
    ; expected_value =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ExpectedValue"))
    ; actual_value =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ActualValue"))
    ; difference_type =
        DifferenceType.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "DifferenceType"))
    }
end

module PropertyDifferences = struct
  type t = PropertyDifference.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map PropertyDifference.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list PropertyDifference.to_query v

  let to_json v = `List (List.map PropertyDifference.to_json v)

  let of_json j = Aws.Json.to_list PropertyDifference.of_json j
end

module PhysicalResourceIdContextKeyValuePair = struct
  type t =
    { key : String.t
    ; value : String.t
    }

  let make ~key ~value () = { key; value }

  let parse xml =
    Some
      { key =
          Aws.Xml.required
            "Key"
            (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse)
      ; value =
          Aws.Xml.required
            "Value"
            (Aws.Util.option_bind (Aws.Xml.member "Value" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Value", String.to_query v.value))
         ; Some (Aws.Query.Pair ("Key", String.to_query v.key))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Value", String.to_json v.value); Some ("Key", String.to_json v.key) ])

  let of_json j =
    { key = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key"))
    ; value = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Value"))
    }
end

module PhysicalResourceIdContext = struct
  type t = PhysicalResourceIdContextKeyValuePair.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map
         PhysicalResourceIdContextKeyValuePair.parse
         (Aws.Xml.members "member" xml))

  let to_query v =
    Aws.Query.to_query_list PhysicalResourceIdContextKeyValuePair.to_query v

  let to_json v = `List (List.map PhysicalResourceIdContextKeyValuePair.to_json v)

  let of_json j = Aws.Json.to_list PhysicalResourceIdContextKeyValuePair.of_json j
end

module StackResourceDrift = struct
  type t =
    { stack_id : String.t
    ; logical_resource_id : String.t
    ; physical_resource_id : String.t option
    ; physical_resource_id_context : PhysicalResourceIdContext.t
    ; resource_type : String.t
    ; expected_properties : String.t option
    ; actual_properties : String.t option
    ; property_differences : PropertyDifferences.t
    ; stack_resource_drift_status : StackResourceDriftStatus.t
    ; timestamp : DateTime.t
    }

  let make
      ~stack_id
      ~logical_resource_id
      ?physical_resource_id
      ?(physical_resource_id_context = [])
      ~resource_type
      ?expected_properties
      ?actual_properties
      ?(property_differences = [])
      ~stack_resource_drift_status
      ~timestamp
      () =
    { stack_id
    ; logical_resource_id
    ; physical_resource_id
    ; physical_resource_id_context
    ; resource_type
    ; expected_properties
    ; actual_properties
    ; property_differences
    ; stack_resource_drift_status
    ; timestamp
    }

  let parse xml =
    Some
      { stack_id =
          Aws.Xml.required
            "StackId"
            (Aws.Util.option_bind (Aws.Xml.member "StackId" xml) String.parse)
      ; logical_resource_id =
          Aws.Xml.required
            "LogicalResourceId"
            (Aws.Util.option_bind (Aws.Xml.member "LogicalResourceId" xml) String.parse)
      ; physical_resource_id =
          Aws.Util.option_bind (Aws.Xml.member "PhysicalResourceId" xml) String.parse
      ; physical_resource_id_context =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "PhysicalResourceIdContext" xml)
               PhysicalResourceIdContext.parse)
      ; resource_type =
          Aws.Xml.required
            "ResourceType"
            (Aws.Util.option_bind (Aws.Xml.member "ResourceType" xml) String.parse)
      ; expected_properties =
          Aws.Util.option_bind (Aws.Xml.member "ExpectedProperties" xml) String.parse
      ; actual_properties =
          Aws.Util.option_bind (Aws.Xml.member "ActualProperties" xml) String.parse
      ; property_differences =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "PropertyDifferences" xml)
               PropertyDifferences.parse)
      ; stack_resource_drift_status =
          Aws.Xml.required
            "StackResourceDriftStatus"
            (Aws.Util.option_bind
               (Aws.Xml.member "StackResourceDriftStatus" xml)
               StackResourceDriftStatus.parse)
      ; timestamp =
          Aws.Xml.required
            "Timestamp"
            (Aws.Util.option_bind (Aws.Xml.member "Timestamp" xml) DateTime.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Timestamp", DateTime.to_query v.timestamp))
         ; Some
             (Aws.Query.Pair
                ( "StackResourceDriftStatus"
                , StackResourceDriftStatus.to_query v.stack_resource_drift_status ))
         ; Some
             (Aws.Query.Pair
                ( "PropertyDifferences.member"
                , PropertyDifferences.to_query v.property_differences ))
         ; Aws.Util.option_map v.actual_properties (fun f ->
               Aws.Query.Pair ("ActualProperties", String.to_query f))
         ; Aws.Util.option_map v.expected_properties (fun f ->
               Aws.Query.Pair ("ExpectedProperties", String.to_query f))
         ; Some (Aws.Query.Pair ("ResourceType", String.to_query v.resource_type))
         ; Some
             (Aws.Query.Pair
                ( "PhysicalResourceIdContext.member"
                , PhysicalResourceIdContext.to_query v.physical_resource_id_context ))
         ; Aws.Util.option_map v.physical_resource_id (fun f ->
               Aws.Query.Pair ("PhysicalResourceId", String.to_query f))
         ; Some
             (Aws.Query.Pair ("LogicalResourceId", String.to_query v.logical_resource_id))
         ; Some (Aws.Query.Pair ("StackId", String.to_query v.stack_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Timestamp", DateTime.to_json v.timestamp)
         ; Some
             ( "StackResourceDriftStatus"
             , StackResourceDriftStatus.to_json v.stack_resource_drift_status )
         ; Some ("PropertyDifferences", PropertyDifferences.to_json v.property_differences)
         ; Aws.Util.option_map v.actual_properties (fun f ->
               "ActualProperties", String.to_json f)
         ; Aws.Util.option_map v.expected_properties (fun f ->
               "ExpectedProperties", String.to_json f)
         ; Some ("ResourceType", String.to_json v.resource_type)
         ; Some
             ( "PhysicalResourceIdContext"
             , PhysicalResourceIdContext.to_json v.physical_resource_id_context )
         ; Aws.Util.option_map v.physical_resource_id (fun f ->
               "PhysicalResourceId", String.to_json f)
         ; Some ("LogicalResourceId", String.to_json v.logical_resource_id)
         ; Some ("StackId", String.to_json v.stack_id)
         ])

  let of_json j =
    { stack_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackId"))
    ; logical_resource_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LogicalResourceId"))
    ; physical_resource_id =
        Aws.Util.option_map (Aws.Json.lookup j "PhysicalResourceId") String.of_json
    ; physical_resource_id_context =
        PhysicalResourceIdContext.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "PhysicalResourceIdContext"))
    ; resource_type =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceType"))
    ; expected_properties =
        Aws.Util.option_map (Aws.Json.lookup j "ExpectedProperties") String.of_json
    ; actual_properties =
        Aws.Util.option_map (Aws.Json.lookup j "ActualProperties") String.of_json
    ; property_differences =
        PropertyDifferences.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "PropertyDifferences"))
    ; stack_resource_drift_status =
        StackResourceDriftStatus.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "StackResourceDriftStatus"))
    ; timestamp =
        DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Timestamp"))
    }
end

module DetectStackResourceDriftOutput = struct
  type t = { stack_resource_drift : StackResourceDrift.t }

  let make ~stack_resource_drift () = { stack_resource_drift }

  let parse xml =
    Some
      { stack_resource_drift =
          Aws.Xml.required
            "StackResourceDrift"
            (Aws.Util.option_bind
               (Aws.Xml.member "StackResourceDrift" xml)
               StackResourceDrift.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("StackResourceDrift", StackResourceDrift.to_query v.stack_resource_drift))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("StackResourceDrift", StackResourceDrift.to_json v.stack_resource_drift)
         ])

  let of_json j =
    { stack_resource_drift =
        StackResourceDrift.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "StackResourceDrift"))
    }
end

module LogicalResourceIds = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module HandlerErrorCode = struct
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
    [ "InternalFailure", InternalFailure
    ; "NetworkFailure", NetworkFailure
    ; "ServiceInternalError", ServiceInternalError
    ; "GeneralServiceException", GeneralServiceException
    ; "NotStabilized", NotStabilized
    ; "ServiceLimitExceeded", ServiceLimitExceeded
    ; "Throttling", Throttling
    ; "ResourceConflict", ResourceConflict
    ; "NotFound", NotFound
    ; "AlreadyExists", AlreadyExists
    ; "InvalidCredentials", InvalidCredentials
    ; "AccessDenied", AccessDenied
    ; "InvalidRequest", InvalidRequest
    ; "NotUpdatable", NotUpdatable
    ]

  let t_to_str =
    [ InternalFailure, "InternalFailure"
    ; NetworkFailure, "NetworkFailure"
    ; ServiceInternalError, "ServiceInternalError"
    ; GeneralServiceException, "GeneralServiceException"
    ; NotStabilized, "NotStabilized"
    ; ServiceLimitExceeded, "ServiceLimitExceeded"
    ; Throttling, "Throttling"
    ; ResourceConflict, "ResourceConflict"
    ; NotFound, "NotFound"
    ; AlreadyExists, "AlreadyExists"
    ; InvalidCredentials, "InvalidCredentials"
    ; AccessDenied, "AccessDenied"
    ; InvalidRequest, "InvalidRequest"
    ; NotUpdatable, "NotUpdatable"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module StackResourceDriftInformation = struct
  type t =
    { stack_resource_drift_status : StackResourceDriftStatus.t
    ; last_check_timestamp : DateTime.t option
    }

  let make ~stack_resource_drift_status ?last_check_timestamp () =
    { stack_resource_drift_status; last_check_timestamp }

  let parse xml =
    Some
      { stack_resource_drift_status =
          Aws.Xml.required
            "StackResourceDriftStatus"
            (Aws.Util.option_bind
               (Aws.Xml.member "StackResourceDriftStatus" xml)
               StackResourceDriftStatus.parse)
      ; last_check_timestamp =
          Aws.Util.option_bind (Aws.Xml.member "LastCheckTimestamp" xml) DateTime.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.last_check_timestamp (fun f ->
               Aws.Query.Pair ("LastCheckTimestamp", DateTime.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "StackResourceDriftStatus"
                , StackResourceDriftStatus.to_query v.stack_resource_drift_status ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.last_check_timestamp (fun f ->
               "LastCheckTimestamp", DateTime.to_json f)
         ; Some
             ( "StackResourceDriftStatus"
             , StackResourceDriftStatus.to_json v.stack_resource_drift_status )
         ])

  let of_json j =
    { stack_resource_drift_status =
        StackResourceDriftStatus.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "StackResourceDriftStatus"))
    ; last_check_timestamp =
        Aws.Util.option_map (Aws.Json.lookup j "LastCheckTimestamp") DateTime.of_json
    }
end

module ResourceStatus = struct
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
    [ "IMPORT_ROLLBACK_COMPLETE", IMPORT_ROLLBACK_COMPLETE
    ; "IMPORT_ROLLBACK_FAILED", IMPORT_ROLLBACK_FAILED
    ; "IMPORT_ROLLBACK_IN_PROGRESS", IMPORT_ROLLBACK_IN_PROGRESS
    ; "IMPORT_IN_PROGRESS", IMPORT_IN_PROGRESS
    ; "IMPORT_COMPLETE", IMPORT_COMPLETE
    ; "IMPORT_FAILED", IMPORT_FAILED
    ; "UPDATE_COMPLETE", UPDATE_COMPLETE
    ; "UPDATE_FAILED", UPDATE_FAILED
    ; "UPDATE_IN_PROGRESS", UPDATE_IN_PROGRESS
    ; "DELETE_SKIPPED", DELETE_SKIPPED
    ; "DELETE_COMPLETE", DELETE_COMPLETE
    ; "DELETE_FAILED", DELETE_FAILED
    ; "DELETE_IN_PROGRESS", DELETE_IN_PROGRESS
    ; "CREATE_COMPLETE", CREATE_COMPLETE
    ; "CREATE_FAILED", CREATE_FAILED
    ; "CREATE_IN_PROGRESS", CREATE_IN_PROGRESS
    ]

  let t_to_str =
    [ IMPORT_ROLLBACK_COMPLETE, "IMPORT_ROLLBACK_COMPLETE"
    ; IMPORT_ROLLBACK_FAILED, "IMPORT_ROLLBACK_FAILED"
    ; IMPORT_ROLLBACK_IN_PROGRESS, "IMPORT_ROLLBACK_IN_PROGRESS"
    ; IMPORT_IN_PROGRESS, "IMPORT_IN_PROGRESS"
    ; IMPORT_COMPLETE, "IMPORT_COMPLETE"
    ; IMPORT_FAILED, "IMPORT_FAILED"
    ; UPDATE_COMPLETE, "UPDATE_COMPLETE"
    ; UPDATE_FAILED, "UPDATE_FAILED"
    ; UPDATE_IN_PROGRESS, "UPDATE_IN_PROGRESS"
    ; DELETE_SKIPPED, "DELETE_SKIPPED"
    ; DELETE_COMPLETE, "DELETE_COMPLETE"
    ; DELETE_FAILED, "DELETE_FAILED"
    ; DELETE_IN_PROGRESS, "DELETE_IN_PROGRESS"
    ; CREATE_COMPLETE, "CREATE_COMPLETE"
    ; CREATE_FAILED, "CREATE_FAILED"
    ; CREATE_IN_PROGRESS, "CREATE_IN_PROGRESS"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module StackResource = struct
  type t =
    { stack_name : String.t option
    ; stack_id : String.t option
    ; logical_resource_id : String.t
    ; physical_resource_id : String.t option
    ; resource_type : String.t
    ; timestamp : DateTime.t
    ; resource_status : ResourceStatus.t
    ; resource_status_reason : String.t option
    ; description : String.t option
    ; drift_information : StackResourceDriftInformation.t option
    }

  let make
      ?stack_name
      ?stack_id
      ~logical_resource_id
      ?physical_resource_id
      ~resource_type
      ~timestamp
      ~resource_status
      ?resource_status_reason
      ?description
      ?drift_information
      () =
    { stack_name
    ; stack_id
    ; logical_resource_id
    ; physical_resource_id
    ; resource_type
    ; timestamp
    ; resource_status
    ; resource_status_reason
    ; description
    ; drift_information
    }

  let parse xml =
    Some
      { stack_name = Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse
      ; stack_id = Aws.Util.option_bind (Aws.Xml.member "StackId" xml) String.parse
      ; logical_resource_id =
          Aws.Xml.required
            "LogicalResourceId"
            (Aws.Util.option_bind (Aws.Xml.member "LogicalResourceId" xml) String.parse)
      ; physical_resource_id =
          Aws.Util.option_bind (Aws.Xml.member "PhysicalResourceId" xml) String.parse
      ; resource_type =
          Aws.Xml.required
            "ResourceType"
            (Aws.Util.option_bind (Aws.Xml.member "ResourceType" xml) String.parse)
      ; timestamp =
          Aws.Xml.required
            "Timestamp"
            (Aws.Util.option_bind (Aws.Xml.member "Timestamp" xml) DateTime.parse)
      ; resource_status =
          Aws.Xml.required
            "ResourceStatus"
            (Aws.Util.option_bind
               (Aws.Xml.member "ResourceStatus" xml)
               ResourceStatus.parse)
      ; resource_status_reason =
          Aws.Util.option_bind (Aws.Xml.member "ResourceStatusReason" xml) String.parse
      ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
      ; drift_information =
          Aws.Util.option_bind
            (Aws.Xml.member "DriftInformation" xml)
            StackResourceDriftInformation.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.drift_information (fun f ->
               Aws.Query.Pair
                 ("DriftInformation", StackResourceDriftInformation.to_query f))
         ; Aws.Util.option_map v.description (fun f ->
               Aws.Query.Pair ("Description", String.to_query f))
         ; Aws.Util.option_map v.resource_status_reason (fun f ->
               Aws.Query.Pair ("ResourceStatusReason", String.to_query f))
         ; Some
             (Aws.Query.Pair ("ResourceStatus", ResourceStatus.to_query v.resource_status))
         ; Some (Aws.Query.Pair ("Timestamp", DateTime.to_query v.timestamp))
         ; Some (Aws.Query.Pair ("ResourceType", String.to_query v.resource_type))
         ; Aws.Util.option_map v.physical_resource_id (fun f ->
               Aws.Query.Pair ("PhysicalResourceId", String.to_query f))
         ; Some
             (Aws.Query.Pair ("LogicalResourceId", String.to_query v.logical_resource_id))
         ; Aws.Util.option_map v.stack_id (fun f ->
               Aws.Query.Pair ("StackId", String.to_query f))
         ; Aws.Util.option_map v.stack_name (fun f ->
               Aws.Query.Pair ("StackName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.drift_information (fun f ->
               "DriftInformation", StackResourceDriftInformation.to_json f)
         ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
         ; Aws.Util.option_map v.resource_status_reason (fun f ->
               "ResourceStatusReason", String.to_json f)
         ; Some ("ResourceStatus", ResourceStatus.to_json v.resource_status)
         ; Some ("Timestamp", DateTime.to_json v.timestamp)
         ; Some ("ResourceType", String.to_json v.resource_type)
         ; Aws.Util.option_map v.physical_resource_id (fun f ->
               "PhysicalResourceId", String.to_json f)
         ; Some ("LogicalResourceId", String.to_json v.logical_resource_id)
         ; Aws.Util.option_map v.stack_id (fun f -> "StackId", String.to_json f)
         ; Aws.Util.option_map v.stack_name (fun f -> "StackName", String.to_json f)
         ])

  let of_json j =
    { stack_name = Aws.Util.option_map (Aws.Json.lookup j "StackName") String.of_json
    ; stack_id = Aws.Util.option_map (Aws.Json.lookup j "StackId") String.of_json
    ; logical_resource_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LogicalResourceId"))
    ; physical_resource_id =
        Aws.Util.option_map (Aws.Json.lookup j "PhysicalResourceId") String.of_json
    ; resource_type =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceType"))
    ; timestamp =
        DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Timestamp"))
    ; resource_status =
        ResourceStatus.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceStatus"))
    ; resource_status_reason =
        Aws.Util.option_map (Aws.Json.lookup j "ResourceStatusReason") String.of_json
    ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
    ; drift_information =
        Aws.Util.option_map
          (Aws.Json.lookup j "DriftInformation")
          StackResourceDriftInformation.of_json
    }
end

module StackResources = struct
  type t = StackResource.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map StackResource.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list StackResource.to_query v

  let to_json v = `List (List.map StackResource.to_json v)

  let of_json j = Aws.Json.to_list StackResource.of_json j
end

module DescribeStackResourcesOutput = struct
  type t = { stack_resources : StackResources.t }

  let make ?(stack_resources = []) () = { stack_resources }

  let parse xml =
    Some
      { stack_resources =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "StackResources" xml)
               StackResources.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("StackResources.member", StackResources.to_query v.stack_resources))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("StackResources", StackResources.to_json v.stack_resources) ])

  let of_json j =
    { stack_resources =
        StackResources.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "StackResources"))
    }
end

module CFNRegistryException = struct
  type t = { message : String.t option }

  let make ?message () = { message }

  let parse xml =
    Some { message = Aws.Util.option_bind (Aws.Xml.member "Message" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f ->
               Aws.Query.Pair ("Message", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.message (fun f -> "Message", String.to_json f) ])

  let of_json j =
    { message = Aws.Util.option_map (Aws.Json.lookup j "Message") String.of_json }
end

module StackSetStatus = struct
  type t =
    | ACTIVE
    | DELETED

  let str_to_t = [ "DELETED", DELETED; "ACTIVE", ACTIVE ]

  let t_to_str = [ DELETED, "DELETED"; ACTIVE, "ACTIVE" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module StackDriftStatus = struct
  type t =
    | DRIFTED
    | IN_SYNC
    | UNKNOWN
    | NOT_CHECKED

  let str_to_t =
    [ "NOT_CHECKED", NOT_CHECKED
    ; "UNKNOWN", UNKNOWN
    ; "IN_SYNC", IN_SYNC
    ; "DRIFTED", DRIFTED
    ]

  let t_to_str =
    [ NOT_CHECKED, "NOT_CHECKED"
    ; UNKNOWN, "UNKNOWN"
    ; IN_SYNC, "IN_SYNC"
    ; DRIFTED, "DRIFTED"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module PermissionModels = struct
  type t =
    | SERVICE_MANAGED
    | SELF_MANAGED

  let str_to_t = [ "SELF_MANAGED", SELF_MANAGED; "SERVICE_MANAGED", SERVICE_MANAGED ]

  let t_to_str = [ SELF_MANAGED, "SELF_MANAGED"; SERVICE_MANAGED, "SERVICE_MANAGED" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module AutoDeployment = struct
  type t =
    { enabled : Boolean.t option
    ; retain_stacks_on_account_removal : Boolean.t option
    }

  let make ?enabled ?retain_stacks_on_account_removal () =
    { enabled; retain_stacks_on_account_removal }

  let parse xml =
    Some
      { enabled = Aws.Util.option_bind (Aws.Xml.member "Enabled" xml) Boolean.parse
      ; retain_stacks_on_account_removal =
          Aws.Util.option_bind
            (Aws.Xml.member "RetainStacksOnAccountRemoval" xml)
            Boolean.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.retain_stacks_on_account_removal (fun f ->
               Aws.Query.Pair ("RetainStacksOnAccountRemoval", Boolean.to_query f))
         ; Aws.Util.option_map v.enabled (fun f ->
               Aws.Query.Pair ("Enabled", Boolean.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.retain_stacks_on_account_removal (fun f ->
               "RetainStacksOnAccountRemoval", Boolean.to_json f)
         ; Aws.Util.option_map v.enabled (fun f -> "Enabled", Boolean.to_json f)
         ])

  let of_json j =
    { enabled = Aws.Util.option_map (Aws.Json.lookup j "Enabled") Boolean.of_json
    ; retain_stacks_on_account_removal =
        Aws.Util.option_map
          (Aws.Json.lookup j "RetainStacksOnAccountRemoval")
          Boolean.of_json
    }
end

module StackSetSummary = struct
  type t =
    { stack_set_name : String.t option
    ; stack_set_id : String.t option
    ; description : String.t option
    ; status : StackSetStatus.t option
    ; auto_deployment : AutoDeployment.t option
    ; permission_model : PermissionModels.t option
    ; drift_status : StackDriftStatus.t option
    ; last_drift_check_timestamp : DateTime.t option
    }

  let make
      ?stack_set_name
      ?stack_set_id
      ?description
      ?status
      ?auto_deployment
      ?permission_model
      ?drift_status
      ?last_drift_check_timestamp
      () =
    { stack_set_name
    ; stack_set_id
    ; description
    ; status
    ; auto_deployment
    ; permission_model
    ; drift_status
    ; last_drift_check_timestamp
    }

  let parse xml =
    Some
      { stack_set_name =
          Aws.Util.option_bind (Aws.Xml.member "StackSetName" xml) String.parse
      ; stack_set_id = Aws.Util.option_bind (Aws.Xml.member "StackSetId" xml) String.parse
      ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
      ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) StackSetStatus.parse
      ; auto_deployment =
          Aws.Util.option_bind (Aws.Xml.member "AutoDeployment" xml) AutoDeployment.parse
      ; permission_model =
          Aws.Util.option_bind
            (Aws.Xml.member "PermissionModel" xml)
            PermissionModels.parse
      ; drift_status =
          Aws.Util.option_bind (Aws.Xml.member "DriftStatus" xml) StackDriftStatus.parse
      ; last_drift_check_timestamp =
          Aws.Util.option_bind
            (Aws.Xml.member "LastDriftCheckTimestamp" xml)
            DateTime.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.last_drift_check_timestamp (fun f ->
               Aws.Query.Pair ("LastDriftCheckTimestamp", DateTime.to_query f))
         ; Aws.Util.option_map v.drift_status (fun f ->
               Aws.Query.Pair ("DriftStatus", StackDriftStatus.to_query f))
         ; Aws.Util.option_map v.permission_model (fun f ->
               Aws.Query.Pair ("PermissionModel", PermissionModels.to_query f))
         ; Aws.Util.option_map v.auto_deployment (fun f ->
               Aws.Query.Pair ("AutoDeployment", AutoDeployment.to_query f))
         ; Aws.Util.option_map v.status (fun f ->
               Aws.Query.Pair ("Status", StackSetStatus.to_query f))
         ; Aws.Util.option_map v.description (fun f ->
               Aws.Query.Pair ("Description", String.to_query f))
         ; Aws.Util.option_map v.stack_set_id (fun f ->
               Aws.Query.Pair ("StackSetId", String.to_query f))
         ; Aws.Util.option_map v.stack_set_name (fun f ->
               Aws.Query.Pair ("StackSetName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.last_drift_check_timestamp (fun f ->
               "LastDriftCheckTimestamp", DateTime.to_json f)
         ; Aws.Util.option_map v.drift_status (fun f ->
               "DriftStatus", StackDriftStatus.to_json f)
         ; Aws.Util.option_map v.permission_model (fun f ->
               "PermissionModel", PermissionModels.to_json f)
         ; Aws.Util.option_map v.auto_deployment (fun f ->
               "AutoDeployment", AutoDeployment.to_json f)
         ; Aws.Util.option_map v.status (fun f -> "Status", StackSetStatus.to_json f)
         ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
         ; Aws.Util.option_map v.stack_set_id (fun f -> "StackSetId", String.to_json f)
         ; Aws.Util.option_map v.stack_set_name (fun f ->
               "StackSetName", String.to_json f)
         ])

  let of_json j =
    { stack_set_name =
        Aws.Util.option_map (Aws.Json.lookup j "StackSetName") String.of_json
    ; stack_set_id = Aws.Util.option_map (Aws.Json.lookup j "StackSetId") String.of_json
    ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
    ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") StackSetStatus.of_json
    ; auto_deployment =
        Aws.Util.option_map (Aws.Json.lookup j "AutoDeployment") AutoDeployment.of_json
    ; permission_model =
        Aws.Util.option_map (Aws.Json.lookup j "PermissionModel") PermissionModels.of_json
    ; drift_status =
        Aws.Util.option_map (Aws.Json.lookup j "DriftStatus") StackDriftStatus.of_json
    ; last_drift_check_timestamp =
        Aws.Util.option_map (Aws.Json.lookup j "LastDriftCheckTimestamp") DateTime.of_json
    }
end

module ListChangeSetsInput = struct
  type t =
    { stack_name : String.t
    ; next_token : String.t option
    }

  let make ~stack_name ?next_token () = { stack_name; next_token }

  let parse xml =
    Some
      { stack_name =
          Aws.Xml.required
            "StackName"
            (Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some (Aws.Query.Pair ("StackName", String.to_query v.stack_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("StackName", String.to_json v.stack_name)
         ])

  let of_json j =
    { stack_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackName"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module StackStatus = struct
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
    [ "IMPORT_ROLLBACK_COMPLETE", IMPORT_ROLLBACK_COMPLETE
    ; "IMPORT_ROLLBACK_FAILED", IMPORT_ROLLBACK_FAILED
    ; "IMPORT_ROLLBACK_IN_PROGRESS", IMPORT_ROLLBACK_IN_PROGRESS
    ; "IMPORT_COMPLETE", IMPORT_COMPLETE
    ; "IMPORT_IN_PROGRESS", IMPORT_IN_PROGRESS
    ; "REVIEW_IN_PROGRESS", REVIEW_IN_PROGRESS
    ; "UPDATE_ROLLBACK_COMPLETE", UPDATE_ROLLBACK_COMPLETE
    ; ( "UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS"
      , UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS )
    ; "UPDATE_ROLLBACK_FAILED", UPDATE_ROLLBACK_FAILED
    ; "UPDATE_ROLLBACK_IN_PROGRESS", UPDATE_ROLLBACK_IN_PROGRESS
    ; "UPDATE_COMPLETE", UPDATE_COMPLETE
    ; "UPDATE_COMPLETE_CLEANUP_IN_PROGRESS", UPDATE_COMPLETE_CLEANUP_IN_PROGRESS
    ; "UPDATE_IN_PROGRESS", UPDATE_IN_PROGRESS
    ; "DELETE_COMPLETE", DELETE_COMPLETE
    ; "DELETE_FAILED", DELETE_FAILED
    ; "DELETE_IN_PROGRESS", DELETE_IN_PROGRESS
    ; "ROLLBACK_COMPLETE", ROLLBACK_COMPLETE
    ; "ROLLBACK_FAILED", ROLLBACK_FAILED
    ; "ROLLBACK_IN_PROGRESS", ROLLBACK_IN_PROGRESS
    ; "CREATE_COMPLETE", CREATE_COMPLETE
    ; "CREATE_FAILED", CREATE_FAILED
    ; "CREATE_IN_PROGRESS", CREATE_IN_PROGRESS
    ]

  let t_to_str =
    [ IMPORT_ROLLBACK_COMPLETE, "IMPORT_ROLLBACK_COMPLETE"
    ; IMPORT_ROLLBACK_FAILED, "IMPORT_ROLLBACK_FAILED"
    ; IMPORT_ROLLBACK_IN_PROGRESS, "IMPORT_ROLLBACK_IN_PROGRESS"
    ; IMPORT_COMPLETE, "IMPORT_COMPLETE"
    ; IMPORT_IN_PROGRESS, "IMPORT_IN_PROGRESS"
    ; REVIEW_IN_PROGRESS, "REVIEW_IN_PROGRESS"
    ; UPDATE_ROLLBACK_COMPLETE, "UPDATE_ROLLBACK_COMPLETE"
    ; ( UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS
      , "UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS" )
    ; UPDATE_ROLLBACK_FAILED, "UPDATE_ROLLBACK_FAILED"
    ; UPDATE_ROLLBACK_IN_PROGRESS, "UPDATE_ROLLBACK_IN_PROGRESS"
    ; UPDATE_COMPLETE, "UPDATE_COMPLETE"
    ; UPDATE_COMPLETE_CLEANUP_IN_PROGRESS, "UPDATE_COMPLETE_CLEANUP_IN_PROGRESS"
    ; UPDATE_IN_PROGRESS, "UPDATE_IN_PROGRESS"
    ; DELETE_COMPLETE, "DELETE_COMPLETE"
    ; DELETE_FAILED, "DELETE_FAILED"
    ; DELETE_IN_PROGRESS, "DELETE_IN_PROGRESS"
    ; ROLLBACK_COMPLETE, "ROLLBACK_COMPLETE"
    ; ROLLBACK_FAILED, "ROLLBACK_FAILED"
    ; ROLLBACK_IN_PROGRESS, "ROLLBACK_IN_PROGRESS"
    ; CREATE_COMPLETE, "CREATE_COMPLETE"
    ; CREATE_FAILED, "CREATE_FAILED"
    ; CREATE_IN_PROGRESS, "CREATE_IN_PROGRESS"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module StackDriftInformationSummary = struct
  type t =
    { stack_drift_status : StackDriftStatus.t
    ; last_check_timestamp : DateTime.t option
    }

  let make ~stack_drift_status ?last_check_timestamp () =
    { stack_drift_status; last_check_timestamp }

  let parse xml =
    Some
      { stack_drift_status =
          Aws.Xml.required
            "StackDriftStatus"
            (Aws.Util.option_bind
               (Aws.Xml.member "StackDriftStatus" xml)
               StackDriftStatus.parse)
      ; last_check_timestamp =
          Aws.Util.option_bind (Aws.Xml.member "LastCheckTimestamp" xml) DateTime.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.last_check_timestamp (fun f ->
               Aws.Query.Pair ("LastCheckTimestamp", DateTime.to_query f))
         ; Some
             (Aws.Query.Pair
                ("StackDriftStatus", StackDriftStatus.to_query v.stack_drift_status))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.last_check_timestamp (fun f ->
               "LastCheckTimestamp", DateTime.to_json f)
         ; Some ("StackDriftStatus", StackDriftStatus.to_json v.stack_drift_status)
         ])

  let of_json j =
    { stack_drift_status =
        StackDriftStatus.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "StackDriftStatus"))
    ; last_check_timestamp =
        Aws.Util.option_map (Aws.Json.lookup j "LastCheckTimestamp") DateTime.of_json
    }
end

module StackSummary = struct
  type t =
    { stack_id : String.t option
    ; stack_name : String.t
    ; template_description : String.t option
    ; creation_time : DateTime.t
    ; last_updated_time : DateTime.t option
    ; deletion_time : DateTime.t option
    ; stack_status : StackStatus.t
    ; stack_status_reason : String.t option
    ; parent_id : String.t option
    ; root_id : String.t option
    ; drift_information : StackDriftInformationSummary.t option
    }

  let make
      ?stack_id
      ~stack_name
      ?template_description
      ~creation_time
      ?last_updated_time
      ?deletion_time
      ~stack_status
      ?stack_status_reason
      ?parent_id
      ?root_id
      ?drift_information
      () =
    { stack_id
    ; stack_name
    ; template_description
    ; creation_time
    ; last_updated_time
    ; deletion_time
    ; stack_status
    ; stack_status_reason
    ; parent_id
    ; root_id
    ; drift_information
    }

  let parse xml =
    Some
      { stack_id = Aws.Util.option_bind (Aws.Xml.member "StackId" xml) String.parse
      ; stack_name =
          Aws.Xml.required
            "StackName"
            (Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse)
      ; template_description =
          Aws.Util.option_bind (Aws.Xml.member "TemplateDescription" xml) String.parse
      ; creation_time =
          Aws.Xml.required
            "CreationTime"
            (Aws.Util.option_bind (Aws.Xml.member "CreationTime" xml) DateTime.parse)
      ; last_updated_time =
          Aws.Util.option_bind (Aws.Xml.member "LastUpdatedTime" xml) DateTime.parse
      ; deletion_time =
          Aws.Util.option_bind (Aws.Xml.member "DeletionTime" xml) DateTime.parse
      ; stack_status =
          Aws.Xml.required
            "StackStatus"
            (Aws.Util.option_bind (Aws.Xml.member "StackStatus" xml) StackStatus.parse)
      ; stack_status_reason =
          Aws.Util.option_bind (Aws.Xml.member "StackStatusReason" xml) String.parse
      ; parent_id = Aws.Util.option_bind (Aws.Xml.member "ParentId" xml) String.parse
      ; root_id = Aws.Util.option_bind (Aws.Xml.member "RootId" xml) String.parse
      ; drift_information =
          Aws.Util.option_bind
            (Aws.Xml.member "DriftInformation" xml)
            StackDriftInformationSummary.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.drift_information (fun f ->
               Aws.Query.Pair ("DriftInformation", StackDriftInformationSummary.to_query f))
         ; Aws.Util.option_map v.root_id (fun f ->
               Aws.Query.Pair ("RootId", String.to_query f))
         ; Aws.Util.option_map v.parent_id (fun f ->
               Aws.Query.Pair ("ParentId", String.to_query f))
         ; Aws.Util.option_map v.stack_status_reason (fun f ->
               Aws.Query.Pair ("StackStatusReason", String.to_query f))
         ; Some (Aws.Query.Pair ("StackStatus", StackStatus.to_query v.stack_status))
         ; Aws.Util.option_map v.deletion_time (fun f ->
               Aws.Query.Pair ("DeletionTime", DateTime.to_query f))
         ; Aws.Util.option_map v.last_updated_time (fun f ->
               Aws.Query.Pair ("LastUpdatedTime", DateTime.to_query f))
         ; Some (Aws.Query.Pair ("CreationTime", DateTime.to_query v.creation_time))
         ; Aws.Util.option_map v.template_description (fun f ->
               Aws.Query.Pair ("TemplateDescription", String.to_query f))
         ; Some (Aws.Query.Pair ("StackName", String.to_query v.stack_name))
         ; Aws.Util.option_map v.stack_id (fun f ->
               Aws.Query.Pair ("StackId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.drift_information (fun f ->
               "DriftInformation", StackDriftInformationSummary.to_json f)
         ; Aws.Util.option_map v.root_id (fun f -> "RootId", String.to_json f)
         ; Aws.Util.option_map v.parent_id (fun f -> "ParentId", String.to_json f)
         ; Aws.Util.option_map v.stack_status_reason (fun f ->
               "StackStatusReason", String.to_json f)
         ; Some ("StackStatus", StackStatus.to_json v.stack_status)
         ; Aws.Util.option_map v.deletion_time (fun f ->
               "DeletionTime", DateTime.to_json f)
         ; Aws.Util.option_map v.last_updated_time (fun f ->
               "LastUpdatedTime", DateTime.to_json f)
         ; Some ("CreationTime", DateTime.to_json v.creation_time)
         ; Aws.Util.option_map v.template_description (fun f ->
               "TemplateDescription", String.to_json f)
         ; Some ("StackName", String.to_json v.stack_name)
         ; Aws.Util.option_map v.stack_id (fun f -> "StackId", String.to_json f)
         ])

  let of_json j =
    { stack_id = Aws.Util.option_map (Aws.Json.lookup j "StackId") String.of_json
    ; stack_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackName"))
    ; template_description =
        Aws.Util.option_map (Aws.Json.lookup j "TemplateDescription") String.of_json
    ; creation_time =
        DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CreationTime"))
    ; last_updated_time =
        Aws.Util.option_map (Aws.Json.lookup j "LastUpdatedTime") DateTime.of_json
    ; deletion_time =
        Aws.Util.option_map (Aws.Json.lookup j "DeletionTime") DateTime.of_json
    ; stack_status =
        StackStatus.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackStatus"))
    ; stack_status_reason =
        Aws.Util.option_map (Aws.Json.lookup j "StackStatusReason") String.of_json
    ; parent_id = Aws.Util.option_map (Aws.Json.lookup j "ParentId") String.of_json
    ; root_id = Aws.Util.option_map (Aws.Json.lookup j "RootId") String.of_json
    ; drift_information =
        Aws.Util.option_map
          (Aws.Json.lookup j "DriftInformation")
          StackDriftInformationSummary.of_json
    }
end

module StackInstanceFilterName = struct
  type t = DETAILED_STATUS

  let str_to_t = [ "DETAILED_STATUS", DETAILED_STATUS ]

  let t_to_str = [ DETAILED_STATUS, "DETAILED_STATUS" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module StackInstanceFilter = struct
  type t =
    { name : StackInstanceFilterName.t option
    ; values : String.t option
    }

  let make ?name ?values () = { name; values }

  let parse xml =
    Some
      { name =
          Aws.Util.option_bind (Aws.Xml.member "Name" xml) StackInstanceFilterName.parse
      ; values = Aws.Util.option_bind (Aws.Xml.member "Values" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.values (fun f ->
               Aws.Query.Pair ("Values", String.to_query f))
         ; Aws.Util.option_map v.name (fun f ->
               Aws.Query.Pair ("Name", StackInstanceFilterName.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.values (fun f -> "Values", String.to_json f)
         ; Aws.Util.option_map v.name (fun f -> "Name", StackInstanceFilterName.to_json f)
         ])

  let of_json j =
    { name =
        Aws.Util.option_map (Aws.Json.lookup j "Name") StackInstanceFilterName.of_json
    ; values = Aws.Util.option_map (Aws.Json.lookup j "Values") String.of_json
    }
end

module StackSetOperationPreferences = struct
  type t =
    { region_order : RegionList.t
    ; failure_tolerance_count : Integer.t option
    ; failure_tolerance_percentage : Integer.t option
    ; max_concurrent_count : Integer.t option
    ; max_concurrent_percentage : Integer.t option
    }

  let make
      ?(region_order = [])
      ?failure_tolerance_count
      ?failure_tolerance_percentage
      ?max_concurrent_count
      ?max_concurrent_percentage
      () =
    { region_order
    ; failure_tolerance_count
    ; failure_tolerance_percentage
    ; max_concurrent_count
    ; max_concurrent_percentage
    }

  let parse xml =
    Some
      { region_order =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "RegionOrder" xml) RegionList.parse)
      ; failure_tolerance_count =
          Aws.Util.option_bind (Aws.Xml.member "FailureToleranceCount" xml) Integer.parse
      ; failure_tolerance_percentage =
          Aws.Util.option_bind
            (Aws.Xml.member "FailureTolerancePercentage" xml)
            Integer.parse
      ; max_concurrent_count =
          Aws.Util.option_bind (Aws.Xml.member "MaxConcurrentCount" xml) Integer.parse
      ; max_concurrent_percentage =
          Aws.Util.option_bind
            (Aws.Xml.member "MaxConcurrentPercentage" xml)
            Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_concurrent_percentage (fun f ->
               Aws.Query.Pair ("MaxConcurrentPercentage", Integer.to_query f))
         ; Aws.Util.option_map v.max_concurrent_count (fun f ->
               Aws.Query.Pair ("MaxConcurrentCount", Integer.to_query f))
         ; Aws.Util.option_map v.failure_tolerance_percentage (fun f ->
               Aws.Query.Pair ("FailureTolerancePercentage", Integer.to_query f))
         ; Aws.Util.option_map v.failure_tolerance_count (fun f ->
               Aws.Query.Pair ("FailureToleranceCount", Integer.to_query f))
         ; Some
             (Aws.Query.Pair ("RegionOrder.member", RegionList.to_query v.region_order))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_concurrent_percentage (fun f ->
               "MaxConcurrentPercentage", Integer.to_json f)
         ; Aws.Util.option_map v.max_concurrent_count (fun f ->
               "MaxConcurrentCount", Integer.to_json f)
         ; Aws.Util.option_map v.failure_tolerance_percentage (fun f ->
               "FailureTolerancePercentage", Integer.to_json f)
         ; Aws.Util.option_map v.failure_tolerance_count (fun f ->
               "FailureToleranceCount", Integer.to_json f)
         ; Some ("RegionOrder", RegionList.to_json v.region_order)
         ])

  let of_json j =
    { region_order =
        RegionList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RegionOrder"))
    ; failure_tolerance_count =
        Aws.Util.option_map (Aws.Json.lookup j "FailureToleranceCount") Integer.of_json
    ; failure_tolerance_percentage =
        Aws.Util.option_map
          (Aws.Json.lookup j "FailureTolerancePercentage")
          Integer.of_json
    ; max_concurrent_count =
        Aws.Util.option_map (Aws.Json.lookup j "MaxConcurrentCount") Integer.of_json
    ; max_concurrent_percentage =
        Aws.Util.option_map (Aws.Json.lookup j "MaxConcurrentPercentage") Integer.of_json
    }
end

module Parameter = struct
  type t =
    { parameter_key : String.t option
    ; parameter_value : String.t option
    ; use_previous_value : Boolean.t option
    ; resolved_value : String.t option
    }

  let make ?parameter_key ?parameter_value ?use_previous_value ?resolved_value () =
    { parameter_key; parameter_value; use_previous_value; resolved_value }

  let parse xml =
    Some
      { parameter_key =
          Aws.Util.option_bind (Aws.Xml.member "ParameterKey" xml) String.parse
      ; parameter_value =
          Aws.Util.option_bind (Aws.Xml.member "ParameterValue" xml) String.parse
      ; use_previous_value =
          Aws.Util.option_bind (Aws.Xml.member "UsePreviousValue" xml) Boolean.parse
      ; resolved_value =
          Aws.Util.option_bind (Aws.Xml.member "ResolvedValue" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.resolved_value (fun f ->
               Aws.Query.Pair ("ResolvedValue", String.to_query f))
         ; Aws.Util.option_map v.use_previous_value (fun f ->
               Aws.Query.Pair ("UsePreviousValue", Boolean.to_query f))
         ; Aws.Util.option_map v.parameter_value (fun f ->
               Aws.Query.Pair ("ParameterValue", String.to_query f))
         ; Aws.Util.option_map v.parameter_key (fun f ->
               Aws.Query.Pair ("ParameterKey", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.resolved_value (fun f ->
               "ResolvedValue", String.to_json f)
         ; Aws.Util.option_map v.use_previous_value (fun f ->
               "UsePreviousValue", Boolean.to_json f)
         ; Aws.Util.option_map v.parameter_value (fun f ->
               "ParameterValue", String.to_json f)
         ; Aws.Util.option_map v.parameter_key (fun f -> "ParameterKey", String.to_json f)
         ])

  let of_json j =
    { parameter_key =
        Aws.Util.option_map (Aws.Json.lookup j "ParameterKey") String.of_json
    ; parameter_value =
        Aws.Util.option_map (Aws.Json.lookup j "ParameterValue") String.of_json
    ; use_previous_value =
        Aws.Util.option_map (Aws.Json.lookup j "UsePreviousValue") Boolean.of_json
    ; resolved_value =
        Aws.Util.option_map (Aws.Json.lookup j "ResolvedValue") String.of_json
    }
end

module Parameters = struct
  type t = Parameter.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map Parameter.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list Parameter.to_query v

  let to_json v = `List (List.map Parameter.to_json v)

  let of_json j = Aws.Json.to_list Parameter.of_json j
end

module OrganizationalUnitIdList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module AccountList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module DeploymentTargets = struct
  type t =
    { accounts : AccountList.t
    ; organizational_unit_ids : OrganizationalUnitIdList.t
    }

  let make ?(accounts = []) ?(organizational_unit_ids = []) () =
    { accounts; organizational_unit_ids }

  let parse xml =
    Some
      { accounts =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Accounts" xml) AccountList.parse)
      ; organizational_unit_ids =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "OrganizationalUnitIds" xml)
               OrganizationalUnitIdList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "OrganizationalUnitIds.member"
                , OrganizationalUnitIdList.to_query v.organizational_unit_ids ))
         ; Some (Aws.Query.Pair ("Accounts.member", AccountList.to_query v.accounts))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "OrganizationalUnitIds"
             , OrganizationalUnitIdList.to_json v.organizational_unit_ids )
         ; Some ("Accounts", AccountList.to_json v.accounts)
         ])

  let of_json j =
    { accounts =
        AccountList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Accounts"))
    ; organizational_unit_ids =
        OrganizationalUnitIdList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "OrganizationalUnitIds"))
    }
end

module CreateStackInstancesInput = struct
  type t =
    { stack_set_name : String.t
    ; accounts : AccountList.t
    ; deployment_targets : DeploymentTargets.t option
    ; regions : RegionList.t
    ; parameter_overrides : Parameters.t
    ; operation_preferences : StackSetOperationPreferences.t option
    ; operation_id : String.t option
    }

  let make
      ~stack_set_name
      ?(accounts = [])
      ?deployment_targets
      ~regions
      ?(parameter_overrides = [])
      ?operation_preferences
      ?operation_id
      () =
    { stack_set_name
    ; accounts
    ; deployment_targets
    ; regions
    ; parameter_overrides
    ; operation_preferences
    ; operation_id
    }

  let parse xml =
    Some
      { stack_set_name =
          Aws.Xml.required
            "StackSetName"
            (Aws.Util.option_bind (Aws.Xml.member "StackSetName" xml) String.parse)
      ; accounts =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Accounts" xml) AccountList.parse)
      ; deployment_targets =
          Aws.Util.option_bind
            (Aws.Xml.member "DeploymentTargets" xml)
            DeploymentTargets.parse
      ; regions =
          Aws.Xml.required
            "Regions"
            (Aws.Util.option_bind (Aws.Xml.member "Regions" xml) RegionList.parse)
      ; parameter_overrides =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ParameterOverrides" xml)
               Parameters.parse)
      ; operation_preferences =
          Aws.Util.option_bind
            (Aws.Xml.member "OperationPreferences" xml)
            StackSetOperationPreferences.parse
      ; operation_id =
          Aws.Util.option_bind (Aws.Xml.member "OperationId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.operation_id (fun f ->
               Aws.Query.Pair ("OperationId", String.to_query f))
         ; Aws.Util.option_map v.operation_preferences (fun f ->
               Aws.Query.Pair
                 ("OperationPreferences", StackSetOperationPreferences.to_query f))
         ; Some
             (Aws.Query.Pair
                ("ParameterOverrides.member", Parameters.to_query v.parameter_overrides))
         ; Some (Aws.Query.Pair ("Regions.member", RegionList.to_query v.regions))
         ; Aws.Util.option_map v.deployment_targets (fun f ->
               Aws.Query.Pair ("DeploymentTargets", DeploymentTargets.to_query f))
         ; Some (Aws.Query.Pair ("Accounts.member", AccountList.to_query v.accounts))
         ; Some (Aws.Query.Pair ("StackSetName", String.to_query v.stack_set_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.operation_id (fun f -> "OperationId", String.to_json f)
         ; Aws.Util.option_map v.operation_preferences (fun f ->
               "OperationPreferences", StackSetOperationPreferences.to_json f)
         ; Some ("ParameterOverrides", Parameters.to_json v.parameter_overrides)
         ; Some ("Regions", RegionList.to_json v.regions)
         ; Aws.Util.option_map v.deployment_targets (fun f ->
               "DeploymentTargets", DeploymentTargets.to_json f)
         ; Some ("Accounts", AccountList.to_json v.accounts)
         ; Some ("StackSetName", String.to_json v.stack_set_name)
         ])

  let of_json j =
    { stack_set_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackSetName"))
    ; accounts =
        AccountList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Accounts"))
    ; deployment_targets =
        Aws.Util.option_map
          (Aws.Json.lookup j "DeploymentTargets")
          DeploymentTargets.of_json
    ; regions = RegionList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Regions"))
    ; parameter_overrides =
        Parameters.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ParameterOverrides"))
    ; operation_preferences =
        Aws.Util.option_map
          (Aws.Json.lookup j "OperationPreferences")
          StackSetOperationPreferences.of_json
    ; operation_id = Aws.Util.option_map (Aws.Json.lookup j "OperationId") String.of_json
    }
end

module InvalidChangeSetStatusException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module RegistrationTokenList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module ListTypeRegistrationsOutput = struct
  type t =
    { registration_token_list : RegistrationTokenList.t
    ; next_token : String.t option
    }

  let make ?(registration_token_list = []) ?next_token () =
    { registration_token_list; next_token }

  let parse xml =
    Some
      { registration_token_list =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "RegistrationTokenList" xml)
               RegistrationTokenList.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "RegistrationTokenList.member"
                , RegistrationTokenList.to_query v.registration_token_list ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some
             ( "RegistrationTokenList"
             , RegistrationTokenList.to_json v.registration_token_list )
         ])

  let of_json j =
    { registration_token_list =
        RegistrationTokenList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "RegistrationTokenList"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module StackEvent = struct
  type t =
    { stack_id : String.t
    ; event_id : String.t
    ; stack_name : String.t
    ; logical_resource_id : String.t option
    ; physical_resource_id : String.t option
    ; resource_type : String.t option
    ; timestamp : DateTime.t
    ; resource_status : ResourceStatus.t option
    ; resource_status_reason : String.t option
    ; resource_properties : String.t option
    ; client_request_token : String.t option
    }

  let make
      ~stack_id
      ~event_id
      ~stack_name
      ?logical_resource_id
      ?physical_resource_id
      ?resource_type
      ~timestamp
      ?resource_status
      ?resource_status_reason
      ?resource_properties
      ?client_request_token
      () =
    { stack_id
    ; event_id
    ; stack_name
    ; logical_resource_id
    ; physical_resource_id
    ; resource_type
    ; timestamp
    ; resource_status
    ; resource_status_reason
    ; resource_properties
    ; client_request_token
    }

  let parse xml =
    Some
      { stack_id =
          Aws.Xml.required
            "StackId"
            (Aws.Util.option_bind (Aws.Xml.member "StackId" xml) String.parse)
      ; event_id =
          Aws.Xml.required
            "EventId"
            (Aws.Util.option_bind (Aws.Xml.member "EventId" xml) String.parse)
      ; stack_name =
          Aws.Xml.required
            "StackName"
            (Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse)
      ; logical_resource_id =
          Aws.Util.option_bind (Aws.Xml.member "LogicalResourceId" xml) String.parse
      ; physical_resource_id =
          Aws.Util.option_bind (Aws.Xml.member "PhysicalResourceId" xml) String.parse
      ; resource_type =
          Aws.Util.option_bind (Aws.Xml.member "ResourceType" xml) String.parse
      ; timestamp =
          Aws.Xml.required
            "Timestamp"
            (Aws.Util.option_bind (Aws.Xml.member "Timestamp" xml) DateTime.parse)
      ; resource_status =
          Aws.Util.option_bind (Aws.Xml.member "ResourceStatus" xml) ResourceStatus.parse
      ; resource_status_reason =
          Aws.Util.option_bind (Aws.Xml.member "ResourceStatusReason" xml) String.parse
      ; resource_properties =
          Aws.Util.option_bind (Aws.Xml.member "ResourceProperties" xml) String.parse
      ; client_request_token =
          Aws.Util.option_bind (Aws.Xml.member "ClientRequestToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.client_request_token (fun f ->
               Aws.Query.Pair ("ClientRequestToken", String.to_query f))
         ; Aws.Util.option_map v.resource_properties (fun f ->
               Aws.Query.Pair ("ResourceProperties", String.to_query f))
         ; Aws.Util.option_map v.resource_status_reason (fun f ->
               Aws.Query.Pair ("ResourceStatusReason", String.to_query f))
         ; Aws.Util.option_map v.resource_status (fun f ->
               Aws.Query.Pair ("ResourceStatus", ResourceStatus.to_query f))
         ; Some (Aws.Query.Pair ("Timestamp", DateTime.to_query v.timestamp))
         ; Aws.Util.option_map v.resource_type (fun f ->
               Aws.Query.Pair ("ResourceType", String.to_query f))
         ; Aws.Util.option_map v.physical_resource_id (fun f ->
               Aws.Query.Pair ("PhysicalResourceId", String.to_query f))
         ; Aws.Util.option_map v.logical_resource_id (fun f ->
               Aws.Query.Pair ("LogicalResourceId", String.to_query f))
         ; Some (Aws.Query.Pair ("StackName", String.to_query v.stack_name))
         ; Some (Aws.Query.Pair ("EventId", String.to_query v.event_id))
         ; Some (Aws.Query.Pair ("StackId", String.to_query v.stack_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.client_request_token (fun f ->
               "ClientRequestToken", String.to_json f)
         ; Aws.Util.option_map v.resource_properties (fun f ->
               "ResourceProperties", String.to_json f)
         ; Aws.Util.option_map v.resource_status_reason (fun f ->
               "ResourceStatusReason", String.to_json f)
         ; Aws.Util.option_map v.resource_status (fun f ->
               "ResourceStatus", ResourceStatus.to_json f)
         ; Some ("Timestamp", DateTime.to_json v.timestamp)
         ; Aws.Util.option_map v.resource_type (fun f -> "ResourceType", String.to_json f)
         ; Aws.Util.option_map v.physical_resource_id (fun f ->
               "PhysicalResourceId", String.to_json f)
         ; Aws.Util.option_map v.logical_resource_id (fun f ->
               "LogicalResourceId", String.to_json f)
         ; Some ("StackName", String.to_json v.stack_name)
         ; Some ("EventId", String.to_json v.event_id)
         ; Some ("StackId", String.to_json v.stack_id)
         ])

  let of_json j =
    { stack_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackId"))
    ; event_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "EventId"))
    ; stack_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackName"))
    ; logical_resource_id =
        Aws.Util.option_map (Aws.Json.lookup j "LogicalResourceId") String.of_json
    ; physical_resource_id =
        Aws.Util.option_map (Aws.Json.lookup j "PhysicalResourceId") String.of_json
    ; resource_type =
        Aws.Util.option_map (Aws.Json.lookup j "ResourceType") String.of_json
    ; timestamp =
        DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Timestamp"))
    ; resource_status =
        Aws.Util.option_map (Aws.Json.lookup j "ResourceStatus") ResourceStatus.of_json
    ; resource_status_reason =
        Aws.Util.option_map (Aws.Json.lookup j "ResourceStatusReason") String.of_json
    ; resource_properties =
        Aws.Util.option_map (Aws.Json.lookup j "ResourceProperties") String.of_json
    ; client_request_token =
        Aws.Util.option_map (Aws.Json.lookup j "ClientRequestToken") String.of_json
    }
end

module StackEvents = struct
  type t = StackEvent.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map StackEvent.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list StackEvent.to_query v

  let to_json v = `List (List.map StackEvent.to_json v)

  let of_json j = Aws.Json.to_list StackEvent.of_json j
end

module RollbackConfiguration = struct
  type t =
    { rollback_triggers : RollbackTriggers.t
    ; monitoring_time_in_minutes : Integer.t option
    }

  let make ?(rollback_triggers = []) ?monitoring_time_in_minutes () =
    { rollback_triggers; monitoring_time_in_minutes }

  let parse xml =
    Some
      { rollback_triggers =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "RollbackTriggers" xml)
               RollbackTriggers.parse)
      ; monitoring_time_in_minutes =
          Aws.Util.option_bind
            (Aws.Xml.member "MonitoringTimeInMinutes" xml)
            Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.monitoring_time_in_minutes (fun f ->
               Aws.Query.Pair ("MonitoringTimeInMinutes", Integer.to_query f))
         ; Some
             (Aws.Query.Pair
                ("RollbackTriggers.member", RollbackTriggers.to_query v.rollback_triggers))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.monitoring_time_in_minutes (fun f ->
               "MonitoringTimeInMinutes", Integer.to_json f)
         ; Some ("RollbackTriggers", RollbackTriggers.to_json v.rollback_triggers)
         ])

  let of_json j =
    { rollback_triggers =
        RollbackTriggers.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "RollbackTriggers"))
    ; monitoring_time_in_minutes =
        Aws.Util.option_map (Aws.Json.lookup j "MonitoringTimeInMinutes") Integer.of_json
    }
end

module ChangeAction = struct
  type t =
    | Add
    | Modify
    | Remove
    | Import

  let str_to_t = [ "Import", Import; "Remove", Remove; "Modify", Modify; "Add", Add ]

  let t_to_str = [ Import, "Import"; Remove, "Remove"; Modify, "Modify"; Add, "Add" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module Tag = struct
  type t =
    { key : String.t
    ; value : String.t
    }

  let make ~key ~value () = { key; value }

  let parse xml =
    Some
      { key =
          Aws.Xml.required
            "Key"
            (Aws.Util.option_bind (Aws.Xml.member "Key" xml) String.parse)
      ; value =
          Aws.Xml.required
            "Value"
            (Aws.Util.option_bind (Aws.Xml.member "Value" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Value", String.to_query v.value))
         ; Some (Aws.Query.Pair ("Key", String.to_query v.key))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Value", String.to_json v.value); Some ("Key", String.to_json v.key) ])

  let of_json j =
    { key = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Key"))
    ; value = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Value"))
    }
end

module Tags = struct
  type t = Tag.t list

  let make elems () = elems

  let parse xml = Aws.Util.option_all (List.map Tag.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list Tag.to_query v

  let to_json v = `List (List.map Tag.to_json v)

  let of_json j = Aws.Json.to_list Tag.of_json j
end

module ResourceTypes = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module OnFailure = struct
  type t =
    | DO_NOTHING
    | ROLLBACK
    | DELETE

  let str_to_t = [ "DELETE", DELETE; "ROLLBACK", ROLLBACK; "DO_NOTHING", DO_NOTHING ]

  let t_to_str = [ DELETE, "DELETE"; ROLLBACK, "ROLLBACK"; DO_NOTHING, "DO_NOTHING" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module NotificationARNs = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module Capability = struct
  type t =
    | CAPABILITY_IAM
    | CAPABILITY_NAMED_IAM
    | CAPABILITY_AUTO_EXPAND

  let str_to_t =
    [ "CAPABILITY_AUTO_EXPAND", CAPABILITY_AUTO_EXPAND
    ; "CAPABILITY_NAMED_IAM", CAPABILITY_NAMED_IAM
    ; "CAPABILITY_IAM", CAPABILITY_IAM
    ]

  let t_to_str =
    [ CAPABILITY_AUTO_EXPAND, "CAPABILITY_AUTO_EXPAND"
    ; CAPABILITY_NAMED_IAM, "CAPABILITY_NAMED_IAM"
    ; CAPABILITY_IAM, "CAPABILITY_IAM"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module Capabilities = struct
  type t = Capability.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map Capability.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list Capability.to_query v

  let to_json v = `List (List.map Capability.to_json v)

  let of_json j = Aws.Json.to_list Capability.of_json j
end

module CreateStackInput = struct
  type t =
    { stack_name : String.t
    ; template_body : String.t option
    ; template_u_r_l : String.t option
    ; parameters : Parameters.t
    ; disable_rollback : Boolean.t option
    ; rollback_configuration : RollbackConfiguration.t option
    ; timeout_in_minutes : Integer.t option
    ; notification_a_r_ns : NotificationARNs.t
    ; capabilities : Capabilities.t
    ; resource_types : ResourceTypes.t
    ; role_a_r_n : String.t option
    ; on_failure : OnFailure.t option
    ; stack_policy_body : String.t option
    ; stack_policy_u_r_l : String.t option
    ; tags : Tags.t
    ; client_request_token : String.t option
    ; enable_termination_protection : Boolean.t option
    }

  let make
      ~stack_name
      ?template_body
      ?template_u_r_l
      ?(parameters = [])
      ?disable_rollback
      ?rollback_configuration
      ?timeout_in_minutes
      ?(notification_a_r_ns = [])
      ?(capabilities = [])
      ?(resource_types = [])
      ?role_a_r_n
      ?on_failure
      ?stack_policy_body
      ?stack_policy_u_r_l
      ?(tags = [])
      ?client_request_token
      ?enable_termination_protection
      () =
    { stack_name
    ; template_body
    ; template_u_r_l
    ; parameters
    ; disable_rollback
    ; rollback_configuration
    ; timeout_in_minutes
    ; notification_a_r_ns
    ; capabilities
    ; resource_types
    ; role_a_r_n
    ; on_failure
    ; stack_policy_body
    ; stack_policy_u_r_l
    ; tags
    ; client_request_token
    ; enable_termination_protection
    }

  let parse xml =
    Some
      { stack_name =
          Aws.Xml.required
            "StackName"
            (Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse)
      ; template_body =
          Aws.Util.option_bind (Aws.Xml.member "TemplateBody" xml) String.parse
      ; template_u_r_l =
          Aws.Util.option_bind (Aws.Xml.member "TemplateURL" xml) String.parse
      ; parameters =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Parameters" xml) Parameters.parse)
      ; disable_rollback =
          Aws.Util.option_bind (Aws.Xml.member "DisableRollback" xml) Boolean.parse
      ; rollback_configuration =
          Aws.Util.option_bind
            (Aws.Xml.member "RollbackConfiguration" xml)
            RollbackConfiguration.parse
      ; timeout_in_minutes =
          Aws.Util.option_bind (Aws.Xml.member "TimeoutInMinutes" xml) Integer.parse
      ; notification_a_r_ns =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "NotificationARNs" xml)
               NotificationARNs.parse)
      ; capabilities =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Capabilities" xml) Capabilities.parse)
      ; resource_types =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ResourceTypes" xml)
               ResourceTypes.parse)
      ; role_a_r_n = Aws.Util.option_bind (Aws.Xml.member "RoleARN" xml) String.parse
      ; on_failure = Aws.Util.option_bind (Aws.Xml.member "OnFailure" xml) OnFailure.parse
      ; stack_policy_body =
          Aws.Util.option_bind (Aws.Xml.member "StackPolicyBody" xml) String.parse
      ; stack_policy_u_r_l =
          Aws.Util.option_bind (Aws.Xml.member "StackPolicyURL" xml) String.parse
      ; tags =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) Tags.parse)
      ; client_request_token =
          Aws.Util.option_bind (Aws.Xml.member "ClientRequestToken" xml) String.parse
      ; enable_termination_protection =
          Aws.Util.option_bind
            (Aws.Xml.member "EnableTerminationProtection" xml)
            Boolean.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.enable_termination_protection (fun f ->
               Aws.Query.Pair ("EnableTerminationProtection", Boolean.to_query f))
         ; Aws.Util.option_map v.client_request_token (fun f ->
               Aws.Query.Pair ("ClientRequestToken", String.to_query f))
         ; Some (Aws.Query.Pair ("Tags.member", Tags.to_query v.tags))
         ; Aws.Util.option_map v.stack_policy_u_r_l (fun f ->
               Aws.Query.Pair ("StackPolicyURL", String.to_query f))
         ; Aws.Util.option_map v.stack_policy_body (fun f ->
               Aws.Query.Pair ("StackPolicyBody", String.to_query f))
         ; Aws.Util.option_map v.on_failure (fun f ->
               Aws.Query.Pair ("OnFailure", OnFailure.to_query f))
         ; Aws.Util.option_map v.role_a_r_n (fun f ->
               Aws.Query.Pair ("RoleARN", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("ResourceTypes.member", ResourceTypes.to_query v.resource_types))
         ; Some
             (Aws.Query.Pair ("Capabilities.member", Capabilities.to_query v.capabilities))
         ; Some
             (Aws.Query.Pair
                ( "NotificationARNs.member"
                , NotificationARNs.to_query v.notification_a_r_ns ))
         ; Aws.Util.option_map v.timeout_in_minutes (fun f ->
               Aws.Query.Pair ("TimeoutInMinutes", Integer.to_query f))
         ; Aws.Util.option_map v.rollback_configuration (fun f ->
               Aws.Query.Pair ("RollbackConfiguration", RollbackConfiguration.to_query f))
         ; Aws.Util.option_map v.disable_rollback (fun f ->
               Aws.Query.Pair ("DisableRollback", Boolean.to_query f))
         ; Some (Aws.Query.Pair ("Parameters.member", Parameters.to_query v.parameters))
         ; Aws.Util.option_map v.template_u_r_l (fun f ->
               Aws.Query.Pair ("TemplateURL", String.to_query f))
         ; Aws.Util.option_map v.template_body (fun f ->
               Aws.Query.Pair ("TemplateBody", String.to_query f))
         ; Some (Aws.Query.Pair ("StackName", String.to_query v.stack_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.enable_termination_protection (fun f ->
               "EnableTerminationProtection", Boolean.to_json f)
         ; Aws.Util.option_map v.client_request_token (fun f ->
               "ClientRequestToken", String.to_json f)
         ; Some ("Tags", Tags.to_json v.tags)
         ; Aws.Util.option_map v.stack_policy_u_r_l (fun f ->
               "StackPolicyURL", String.to_json f)
         ; Aws.Util.option_map v.stack_policy_body (fun f ->
               "StackPolicyBody", String.to_json f)
         ; Aws.Util.option_map v.on_failure (fun f -> "OnFailure", OnFailure.to_json f)
         ; Aws.Util.option_map v.role_a_r_n (fun f -> "RoleARN", String.to_json f)
         ; Some ("ResourceTypes", ResourceTypes.to_json v.resource_types)
         ; Some ("Capabilities", Capabilities.to_json v.capabilities)
         ; Some ("NotificationARNs", NotificationARNs.to_json v.notification_a_r_ns)
         ; Aws.Util.option_map v.timeout_in_minutes (fun f ->
               "TimeoutInMinutes", Integer.to_json f)
         ; Aws.Util.option_map v.rollback_configuration (fun f ->
               "RollbackConfiguration", RollbackConfiguration.to_json f)
         ; Aws.Util.option_map v.disable_rollback (fun f ->
               "DisableRollback", Boolean.to_json f)
         ; Some ("Parameters", Parameters.to_json v.parameters)
         ; Aws.Util.option_map v.template_u_r_l (fun f -> "TemplateURL", String.to_json f)
         ; Aws.Util.option_map v.template_body (fun f -> "TemplateBody", String.to_json f)
         ; Some ("StackName", String.to_json v.stack_name)
         ])

  let of_json j =
    { stack_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackName"))
    ; template_body =
        Aws.Util.option_map (Aws.Json.lookup j "TemplateBody") String.of_json
    ; template_u_r_l =
        Aws.Util.option_map (Aws.Json.lookup j "TemplateURL") String.of_json
    ; parameters =
        Parameters.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Parameters"))
    ; disable_rollback =
        Aws.Util.option_map (Aws.Json.lookup j "DisableRollback") Boolean.of_json
    ; rollback_configuration =
        Aws.Util.option_map
          (Aws.Json.lookup j "RollbackConfiguration")
          RollbackConfiguration.of_json
    ; timeout_in_minutes =
        Aws.Util.option_map (Aws.Json.lookup j "TimeoutInMinutes") Integer.of_json
    ; notification_a_r_ns =
        NotificationARNs.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "NotificationARNs"))
    ; capabilities =
        Capabilities.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Capabilities"))
    ; resource_types =
        ResourceTypes.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceTypes"))
    ; role_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "RoleARN") String.of_json
    ; on_failure = Aws.Util.option_map (Aws.Json.lookup j "OnFailure") OnFailure.of_json
    ; stack_policy_body =
        Aws.Util.option_map (Aws.Json.lookup j "StackPolicyBody") String.of_json
    ; stack_policy_u_r_l =
        Aws.Util.option_map (Aws.Json.lookup j "StackPolicyURL") String.of_json
    ; tags = Tags.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
    ; client_request_token =
        Aws.Util.option_map (Aws.Json.lookup j "ClientRequestToken") String.of_json
    ; enable_termination_protection =
        Aws.Util.option_map
          (Aws.Json.lookup j "EnableTerminationProtection")
          Boolean.of_json
    }
end

module DescribeStackInstanceInput = struct
  type t =
    { stack_set_name : String.t
    ; stack_instance_account : String.t
    ; stack_instance_region : String.t
    }

  let make ~stack_set_name ~stack_instance_account ~stack_instance_region () =
    { stack_set_name; stack_instance_account; stack_instance_region }

  let parse xml =
    Some
      { stack_set_name =
          Aws.Xml.required
            "StackSetName"
            (Aws.Util.option_bind (Aws.Xml.member "StackSetName" xml) String.parse)
      ; stack_instance_account =
          Aws.Xml.required
            "StackInstanceAccount"
            (Aws.Util.option_bind
               (Aws.Xml.member "StackInstanceAccount" xml)
               String.parse)
      ; stack_instance_region =
          Aws.Xml.required
            "StackInstanceRegion"
            (Aws.Util.option_bind (Aws.Xml.member "StackInstanceRegion" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("StackInstanceRegion", String.to_query v.stack_instance_region))
         ; Some
             (Aws.Query.Pair
                ("StackInstanceAccount", String.to_query v.stack_instance_account))
         ; Some (Aws.Query.Pair ("StackSetName", String.to_query v.stack_set_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("StackInstanceRegion", String.to_json v.stack_instance_region)
         ; Some ("StackInstanceAccount", String.to_json v.stack_instance_account)
         ; Some ("StackSetName", String.to_json v.stack_set_name)
         ])

  let of_json j =
    { stack_set_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackSetName"))
    ; stack_instance_account =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackInstanceAccount"))
    ; stack_instance_region =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackInstanceRegion"))
    }
end

module AccountLimit = struct
  type t =
    { name : String.t option
    ; value : Integer.t option
    }

  let make ?name ?value () = { name; value }

  let parse xml =
    Some
      { name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
      ; value = Aws.Util.option_bind (Aws.Xml.member "Value" xml) Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.value (fun f ->
               Aws.Query.Pair ("Value", Integer.to_query f))
         ; Aws.Util.option_map v.name (fun f ->
               Aws.Query.Pair ("Name", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.value (fun f -> "Value", Integer.to_json f)
         ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
         ])

  let of_json j =
    { name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
    ; value = Aws.Util.option_map (Aws.Json.lookup j "Value") Integer.of_json
    }
end

module AccountLimitList = struct
  type t = AccountLimit.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map AccountLimit.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list AccountLimit.to_query v

  let to_json v = `List (List.map AccountLimit.to_json v)

  let of_json j = Aws.Json.to_list AccountLimit.of_json j
end

module ExecuteChangeSetInput = struct
  type t =
    { change_set_name : String.t
    ; stack_name : String.t option
    ; client_request_token : String.t option
    }

  let make ~change_set_name ?stack_name ?client_request_token () =
    { change_set_name; stack_name; client_request_token }

  let parse xml =
    Some
      { change_set_name =
          Aws.Xml.required
            "ChangeSetName"
            (Aws.Util.option_bind (Aws.Xml.member "ChangeSetName" xml) String.parse)
      ; stack_name = Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse
      ; client_request_token =
          Aws.Util.option_bind (Aws.Xml.member "ClientRequestToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.client_request_token (fun f ->
               Aws.Query.Pair ("ClientRequestToken", String.to_query f))
         ; Aws.Util.option_map v.stack_name (fun f ->
               Aws.Query.Pair ("StackName", String.to_query f))
         ; Some (Aws.Query.Pair ("ChangeSetName", String.to_query v.change_set_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.client_request_token (fun f ->
               "ClientRequestToken", String.to_json f)
         ; Aws.Util.option_map v.stack_name (fun f -> "StackName", String.to_json f)
         ; Some ("ChangeSetName", String.to_json v.change_set_name)
         ])

  let of_json j =
    { change_set_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ChangeSetName"))
    ; stack_name = Aws.Util.option_map (Aws.Json.lookup j "StackName") String.of_json
    ; client_request_token =
        Aws.Util.option_map (Aws.Json.lookup j "ClientRequestToken") String.of_json
    }
end

module UpdateStackInput = struct
  type t =
    { stack_name : String.t
    ; template_body : String.t option
    ; template_u_r_l : String.t option
    ; use_previous_template : Boolean.t option
    ; stack_policy_during_update_body : String.t option
    ; stack_policy_during_update_u_r_l : String.t option
    ; parameters : Parameters.t
    ; capabilities : Capabilities.t
    ; resource_types : ResourceTypes.t
    ; role_a_r_n : String.t option
    ; rollback_configuration : RollbackConfiguration.t option
    ; stack_policy_body : String.t option
    ; stack_policy_u_r_l : String.t option
    ; notification_a_r_ns : NotificationARNs.t
    ; tags : Tags.t
    ; client_request_token : String.t option
    }

  let make
      ~stack_name
      ?template_body
      ?template_u_r_l
      ?use_previous_template
      ?stack_policy_during_update_body
      ?stack_policy_during_update_u_r_l
      ?(parameters = [])
      ?(capabilities = [])
      ?(resource_types = [])
      ?role_a_r_n
      ?rollback_configuration
      ?stack_policy_body
      ?stack_policy_u_r_l
      ?(notification_a_r_ns = [])
      ?(tags = [])
      ?client_request_token
      () =
    { stack_name
    ; template_body
    ; template_u_r_l
    ; use_previous_template
    ; stack_policy_during_update_body
    ; stack_policy_during_update_u_r_l
    ; parameters
    ; capabilities
    ; resource_types
    ; role_a_r_n
    ; rollback_configuration
    ; stack_policy_body
    ; stack_policy_u_r_l
    ; notification_a_r_ns
    ; tags
    ; client_request_token
    }

  let parse xml =
    Some
      { stack_name =
          Aws.Xml.required
            "StackName"
            (Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse)
      ; template_body =
          Aws.Util.option_bind (Aws.Xml.member "TemplateBody" xml) String.parse
      ; template_u_r_l =
          Aws.Util.option_bind (Aws.Xml.member "TemplateURL" xml) String.parse
      ; use_previous_template =
          Aws.Util.option_bind (Aws.Xml.member "UsePreviousTemplate" xml) Boolean.parse
      ; stack_policy_during_update_body =
          Aws.Util.option_bind
            (Aws.Xml.member "StackPolicyDuringUpdateBody" xml)
            String.parse
      ; stack_policy_during_update_u_r_l =
          Aws.Util.option_bind
            (Aws.Xml.member "StackPolicyDuringUpdateURL" xml)
            String.parse
      ; parameters =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Parameters" xml) Parameters.parse)
      ; capabilities =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Capabilities" xml) Capabilities.parse)
      ; resource_types =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ResourceTypes" xml)
               ResourceTypes.parse)
      ; role_a_r_n = Aws.Util.option_bind (Aws.Xml.member "RoleARN" xml) String.parse
      ; rollback_configuration =
          Aws.Util.option_bind
            (Aws.Xml.member "RollbackConfiguration" xml)
            RollbackConfiguration.parse
      ; stack_policy_body =
          Aws.Util.option_bind (Aws.Xml.member "StackPolicyBody" xml) String.parse
      ; stack_policy_u_r_l =
          Aws.Util.option_bind (Aws.Xml.member "StackPolicyURL" xml) String.parse
      ; notification_a_r_ns =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "NotificationARNs" xml)
               NotificationARNs.parse)
      ; tags =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) Tags.parse)
      ; client_request_token =
          Aws.Util.option_bind (Aws.Xml.member "ClientRequestToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.client_request_token (fun f ->
               Aws.Query.Pair ("ClientRequestToken", String.to_query f))
         ; Some (Aws.Query.Pair ("Tags.member", Tags.to_query v.tags))
         ; Some
             (Aws.Query.Pair
                ( "NotificationARNs.member"
                , NotificationARNs.to_query v.notification_a_r_ns ))
         ; Aws.Util.option_map v.stack_policy_u_r_l (fun f ->
               Aws.Query.Pair ("StackPolicyURL", String.to_query f))
         ; Aws.Util.option_map v.stack_policy_body (fun f ->
               Aws.Query.Pair ("StackPolicyBody", String.to_query f))
         ; Aws.Util.option_map v.rollback_configuration (fun f ->
               Aws.Query.Pair ("RollbackConfiguration", RollbackConfiguration.to_query f))
         ; Aws.Util.option_map v.role_a_r_n (fun f ->
               Aws.Query.Pair ("RoleARN", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("ResourceTypes.member", ResourceTypes.to_query v.resource_types))
         ; Some
             (Aws.Query.Pair ("Capabilities.member", Capabilities.to_query v.capabilities))
         ; Some (Aws.Query.Pair ("Parameters.member", Parameters.to_query v.parameters))
         ; Aws.Util.option_map v.stack_policy_during_update_u_r_l (fun f ->
               Aws.Query.Pair ("StackPolicyDuringUpdateURL", String.to_query f))
         ; Aws.Util.option_map v.stack_policy_during_update_body (fun f ->
               Aws.Query.Pair ("StackPolicyDuringUpdateBody", String.to_query f))
         ; Aws.Util.option_map v.use_previous_template (fun f ->
               Aws.Query.Pair ("UsePreviousTemplate", Boolean.to_query f))
         ; Aws.Util.option_map v.template_u_r_l (fun f ->
               Aws.Query.Pair ("TemplateURL", String.to_query f))
         ; Aws.Util.option_map v.template_body (fun f ->
               Aws.Query.Pair ("TemplateBody", String.to_query f))
         ; Some (Aws.Query.Pair ("StackName", String.to_query v.stack_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.client_request_token (fun f ->
               "ClientRequestToken", String.to_json f)
         ; Some ("Tags", Tags.to_json v.tags)
         ; Some ("NotificationARNs", NotificationARNs.to_json v.notification_a_r_ns)
         ; Aws.Util.option_map v.stack_policy_u_r_l (fun f ->
               "StackPolicyURL", String.to_json f)
         ; Aws.Util.option_map v.stack_policy_body (fun f ->
               "StackPolicyBody", String.to_json f)
         ; Aws.Util.option_map v.rollback_configuration (fun f ->
               "RollbackConfiguration", RollbackConfiguration.to_json f)
         ; Aws.Util.option_map v.role_a_r_n (fun f -> "RoleARN", String.to_json f)
         ; Some ("ResourceTypes", ResourceTypes.to_json v.resource_types)
         ; Some ("Capabilities", Capabilities.to_json v.capabilities)
         ; Some ("Parameters", Parameters.to_json v.parameters)
         ; Aws.Util.option_map v.stack_policy_during_update_u_r_l (fun f ->
               "StackPolicyDuringUpdateURL", String.to_json f)
         ; Aws.Util.option_map v.stack_policy_during_update_body (fun f ->
               "StackPolicyDuringUpdateBody", String.to_json f)
         ; Aws.Util.option_map v.use_previous_template (fun f ->
               "UsePreviousTemplate", Boolean.to_json f)
         ; Aws.Util.option_map v.template_u_r_l (fun f -> "TemplateURL", String.to_json f)
         ; Aws.Util.option_map v.template_body (fun f -> "TemplateBody", String.to_json f)
         ; Some ("StackName", String.to_json v.stack_name)
         ])

  let of_json j =
    { stack_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackName"))
    ; template_body =
        Aws.Util.option_map (Aws.Json.lookup j "TemplateBody") String.of_json
    ; template_u_r_l =
        Aws.Util.option_map (Aws.Json.lookup j "TemplateURL") String.of_json
    ; use_previous_template =
        Aws.Util.option_map (Aws.Json.lookup j "UsePreviousTemplate") Boolean.of_json
    ; stack_policy_during_update_body =
        Aws.Util.option_map
          (Aws.Json.lookup j "StackPolicyDuringUpdateBody")
          String.of_json
    ; stack_policy_during_update_u_r_l =
        Aws.Util.option_map
          (Aws.Json.lookup j "StackPolicyDuringUpdateURL")
          String.of_json
    ; parameters =
        Parameters.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Parameters"))
    ; capabilities =
        Capabilities.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Capabilities"))
    ; resource_types =
        ResourceTypes.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceTypes"))
    ; role_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "RoleARN") String.of_json
    ; rollback_configuration =
        Aws.Util.option_map
          (Aws.Json.lookup j "RollbackConfiguration")
          RollbackConfiguration.of_json
    ; stack_policy_body =
        Aws.Util.option_map (Aws.Json.lookup j "StackPolicyBody") String.of_json
    ; stack_policy_u_r_l =
        Aws.Util.option_map (Aws.Json.lookup j "StackPolicyURL") String.of_json
    ; notification_a_r_ns =
        NotificationARNs.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "NotificationARNs"))
    ; tags = Tags.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
    ; client_request_token =
        Aws.Util.option_map (Aws.Json.lookup j "ClientRequestToken") String.of_json
    }
end

module StackInstanceDetailedStatus = struct
  type t =
    | PENDING
    | RUNNING
    | SUCCEEDED
    | FAILED
    | CANCELLED
    | INOPERABLE

  let str_to_t =
    [ "INOPERABLE", INOPERABLE
    ; "CANCELLED", CANCELLED
    ; "FAILED", FAILED
    ; "SUCCEEDED", SUCCEEDED
    ; "RUNNING", RUNNING
    ; "PENDING", PENDING
    ]

  let t_to_str =
    [ INOPERABLE, "INOPERABLE"
    ; CANCELLED, "CANCELLED"
    ; FAILED, "FAILED"
    ; SUCCEEDED, "SUCCEEDED"
    ; RUNNING, "RUNNING"
    ; PENDING, "PENDING"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module RecordHandlerProgressOutput = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module LimitExceededException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ChangeType = struct
  type t = Resource

  let str_to_t = [ "Resource", Resource ]

  let t_to_str = [ Resource, "Resource" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module StackDriftInformation = struct
  type t =
    { stack_drift_status : StackDriftStatus.t
    ; last_check_timestamp : DateTime.t option
    }

  let make ~stack_drift_status ?last_check_timestamp () =
    { stack_drift_status; last_check_timestamp }

  let parse xml =
    Some
      { stack_drift_status =
          Aws.Xml.required
            "StackDriftStatus"
            (Aws.Util.option_bind
               (Aws.Xml.member "StackDriftStatus" xml)
               StackDriftStatus.parse)
      ; last_check_timestamp =
          Aws.Util.option_bind (Aws.Xml.member "LastCheckTimestamp" xml) DateTime.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.last_check_timestamp (fun f ->
               Aws.Query.Pair ("LastCheckTimestamp", DateTime.to_query f))
         ; Some
             (Aws.Query.Pair
                ("StackDriftStatus", StackDriftStatus.to_query v.stack_drift_status))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.last_check_timestamp (fun f ->
               "LastCheckTimestamp", DateTime.to_json f)
         ; Some ("StackDriftStatus", StackDriftStatus.to_json v.stack_drift_status)
         ])

  let of_json j =
    { stack_drift_status =
        StackDriftStatus.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "StackDriftStatus"))
    ; last_check_timestamp =
        Aws.Util.option_map (Aws.Json.lookup j "LastCheckTimestamp") DateTime.of_json
    }
end

module Output = struct
  type t =
    { output_key : String.t option
    ; output_value : String.t option
    ; description : String.t option
    ; export_name : String.t option
    }

  let make ?output_key ?output_value ?description ?export_name () =
    { output_key; output_value; description; export_name }

  let parse xml =
    Some
      { output_key = Aws.Util.option_bind (Aws.Xml.member "OutputKey" xml) String.parse
      ; output_value =
          Aws.Util.option_bind (Aws.Xml.member "OutputValue" xml) String.parse
      ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
      ; export_name = Aws.Util.option_bind (Aws.Xml.member "ExportName" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.export_name (fun f ->
               Aws.Query.Pair ("ExportName", String.to_query f))
         ; Aws.Util.option_map v.description (fun f ->
               Aws.Query.Pair ("Description", String.to_query f))
         ; Aws.Util.option_map v.output_value (fun f ->
               Aws.Query.Pair ("OutputValue", String.to_query f))
         ; Aws.Util.option_map v.output_key (fun f ->
               Aws.Query.Pair ("OutputKey", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.export_name (fun f -> "ExportName", String.to_json f)
         ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
         ; Aws.Util.option_map v.output_value (fun f -> "OutputValue", String.to_json f)
         ; Aws.Util.option_map v.output_key (fun f -> "OutputKey", String.to_json f)
         ])

  let of_json j =
    { output_key = Aws.Util.option_map (Aws.Json.lookup j "OutputKey") String.of_json
    ; output_value = Aws.Util.option_map (Aws.Json.lookup j "OutputValue") String.of_json
    ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
    ; export_name = Aws.Util.option_map (Aws.Json.lookup j "ExportName") String.of_json
    }
end

module Outputs = struct
  type t = Output.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map Output.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list Output.to_query v

  let to_json v = `List (List.map Output.to_json v)

  let of_json j = Aws.Json.to_list Output.of_json j
end

module Stack = struct
  type t =
    { stack_id : String.t option
    ; stack_name : String.t
    ; change_set_id : String.t option
    ; description : String.t option
    ; parameters : Parameters.t
    ; creation_time : DateTime.t
    ; deletion_time : DateTime.t option
    ; last_updated_time : DateTime.t option
    ; rollback_configuration : RollbackConfiguration.t option
    ; stack_status : StackStatus.t
    ; stack_status_reason : String.t option
    ; disable_rollback : Boolean.t option
    ; notification_a_r_ns : NotificationARNs.t
    ; timeout_in_minutes : Integer.t option
    ; capabilities : Capabilities.t
    ; outputs : Outputs.t
    ; role_a_r_n : String.t option
    ; tags : Tags.t
    ; enable_termination_protection : Boolean.t option
    ; parent_id : String.t option
    ; root_id : String.t option
    ; drift_information : StackDriftInformation.t option
    }

  let make
      ?stack_id
      ~stack_name
      ?change_set_id
      ?description
      ?(parameters = [])
      ~creation_time
      ?deletion_time
      ?last_updated_time
      ?rollback_configuration
      ~stack_status
      ?stack_status_reason
      ?disable_rollback
      ?(notification_a_r_ns = [])
      ?timeout_in_minutes
      ?(capabilities = [])
      ?(outputs = [])
      ?role_a_r_n
      ?(tags = [])
      ?enable_termination_protection
      ?parent_id
      ?root_id
      ?drift_information
      () =
    { stack_id
    ; stack_name
    ; change_set_id
    ; description
    ; parameters
    ; creation_time
    ; deletion_time
    ; last_updated_time
    ; rollback_configuration
    ; stack_status
    ; stack_status_reason
    ; disable_rollback
    ; notification_a_r_ns
    ; timeout_in_minutes
    ; capabilities
    ; outputs
    ; role_a_r_n
    ; tags
    ; enable_termination_protection
    ; parent_id
    ; root_id
    ; drift_information
    }

  let parse xml =
    Some
      { stack_id = Aws.Util.option_bind (Aws.Xml.member "StackId" xml) String.parse
      ; stack_name =
          Aws.Xml.required
            "StackName"
            (Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse)
      ; change_set_id =
          Aws.Util.option_bind (Aws.Xml.member "ChangeSetId" xml) String.parse
      ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
      ; parameters =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Parameters" xml) Parameters.parse)
      ; creation_time =
          Aws.Xml.required
            "CreationTime"
            (Aws.Util.option_bind (Aws.Xml.member "CreationTime" xml) DateTime.parse)
      ; deletion_time =
          Aws.Util.option_bind (Aws.Xml.member "DeletionTime" xml) DateTime.parse
      ; last_updated_time =
          Aws.Util.option_bind (Aws.Xml.member "LastUpdatedTime" xml) DateTime.parse
      ; rollback_configuration =
          Aws.Util.option_bind
            (Aws.Xml.member "RollbackConfiguration" xml)
            RollbackConfiguration.parse
      ; stack_status =
          Aws.Xml.required
            "StackStatus"
            (Aws.Util.option_bind (Aws.Xml.member "StackStatus" xml) StackStatus.parse)
      ; stack_status_reason =
          Aws.Util.option_bind (Aws.Xml.member "StackStatusReason" xml) String.parse
      ; disable_rollback =
          Aws.Util.option_bind (Aws.Xml.member "DisableRollback" xml) Boolean.parse
      ; notification_a_r_ns =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "NotificationARNs" xml)
               NotificationARNs.parse)
      ; timeout_in_minutes =
          Aws.Util.option_bind (Aws.Xml.member "TimeoutInMinutes" xml) Integer.parse
      ; capabilities =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Capabilities" xml) Capabilities.parse)
      ; outputs =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Outputs" xml) Outputs.parse)
      ; role_a_r_n = Aws.Util.option_bind (Aws.Xml.member "RoleARN" xml) String.parse
      ; tags =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) Tags.parse)
      ; enable_termination_protection =
          Aws.Util.option_bind
            (Aws.Xml.member "EnableTerminationProtection" xml)
            Boolean.parse
      ; parent_id = Aws.Util.option_bind (Aws.Xml.member "ParentId" xml) String.parse
      ; root_id = Aws.Util.option_bind (Aws.Xml.member "RootId" xml) String.parse
      ; drift_information =
          Aws.Util.option_bind
            (Aws.Xml.member "DriftInformation" xml)
            StackDriftInformation.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.drift_information (fun f ->
               Aws.Query.Pair ("DriftInformation", StackDriftInformation.to_query f))
         ; Aws.Util.option_map v.root_id (fun f ->
               Aws.Query.Pair ("RootId", String.to_query f))
         ; Aws.Util.option_map v.parent_id (fun f ->
               Aws.Query.Pair ("ParentId", String.to_query f))
         ; Aws.Util.option_map v.enable_termination_protection (fun f ->
               Aws.Query.Pair ("EnableTerminationProtection", Boolean.to_query f))
         ; Some (Aws.Query.Pair ("Tags.member", Tags.to_query v.tags))
         ; Aws.Util.option_map v.role_a_r_n (fun f ->
               Aws.Query.Pair ("RoleARN", String.to_query f))
         ; Some (Aws.Query.Pair ("Outputs.member", Outputs.to_query v.outputs))
         ; Some
             (Aws.Query.Pair ("Capabilities.member", Capabilities.to_query v.capabilities))
         ; Aws.Util.option_map v.timeout_in_minutes (fun f ->
               Aws.Query.Pair ("TimeoutInMinutes", Integer.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "NotificationARNs.member"
                , NotificationARNs.to_query v.notification_a_r_ns ))
         ; Aws.Util.option_map v.disable_rollback (fun f ->
               Aws.Query.Pair ("DisableRollback", Boolean.to_query f))
         ; Aws.Util.option_map v.stack_status_reason (fun f ->
               Aws.Query.Pair ("StackStatusReason", String.to_query f))
         ; Some (Aws.Query.Pair ("StackStatus", StackStatus.to_query v.stack_status))
         ; Aws.Util.option_map v.rollback_configuration (fun f ->
               Aws.Query.Pair ("RollbackConfiguration", RollbackConfiguration.to_query f))
         ; Aws.Util.option_map v.last_updated_time (fun f ->
               Aws.Query.Pair ("LastUpdatedTime", DateTime.to_query f))
         ; Aws.Util.option_map v.deletion_time (fun f ->
               Aws.Query.Pair ("DeletionTime", DateTime.to_query f))
         ; Some (Aws.Query.Pair ("CreationTime", DateTime.to_query v.creation_time))
         ; Some (Aws.Query.Pair ("Parameters.member", Parameters.to_query v.parameters))
         ; Aws.Util.option_map v.description (fun f ->
               Aws.Query.Pair ("Description", String.to_query f))
         ; Aws.Util.option_map v.change_set_id (fun f ->
               Aws.Query.Pair ("ChangeSetId", String.to_query f))
         ; Some (Aws.Query.Pair ("StackName", String.to_query v.stack_name))
         ; Aws.Util.option_map v.stack_id (fun f ->
               Aws.Query.Pair ("StackId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.drift_information (fun f ->
               "DriftInformation", StackDriftInformation.to_json f)
         ; Aws.Util.option_map v.root_id (fun f -> "RootId", String.to_json f)
         ; Aws.Util.option_map v.parent_id (fun f -> "ParentId", String.to_json f)
         ; Aws.Util.option_map v.enable_termination_protection (fun f ->
               "EnableTerminationProtection", Boolean.to_json f)
         ; Some ("Tags", Tags.to_json v.tags)
         ; Aws.Util.option_map v.role_a_r_n (fun f -> "RoleARN", String.to_json f)
         ; Some ("Outputs", Outputs.to_json v.outputs)
         ; Some ("Capabilities", Capabilities.to_json v.capabilities)
         ; Aws.Util.option_map v.timeout_in_minutes (fun f ->
               "TimeoutInMinutes", Integer.to_json f)
         ; Some ("NotificationARNs", NotificationARNs.to_json v.notification_a_r_ns)
         ; Aws.Util.option_map v.disable_rollback (fun f ->
               "DisableRollback", Boolean.to_json f)
         ; Aws.Util.option_map v.stack_status_reason (fun f ->
               "StackStatusReason", String.to_json f)
         ; Some ("StackStatus", StackStatus.to_json v.stack_status)
         ; Aws.Util.option_map v.rollback_configuration (fun f ->
               "RollbackConfiguration", RollbackConfiguration.to_json f)
         ; Aws.Util.option_map v.last_updated_time (fun f ->
               "LastUpdatedTime", DateTime.to_json f)
         ; Aws.Util.option_map v.deletion_time (fun f ->
               "DeletionTime", DateTime.to_json f)
         ; Some ("CreationTime", DateTime.to_json v.creation_time)
         ; Some ("Parameters", Parameters.to_json v.parameters)
         ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
         ; Aws.Util.option_map v.change_set_id (fun f -> "ChangeSetId", String.to_json f)
         ; Some ("StackName", String.to_json v.stack_name)
         ; Aws.Util.option_map v.stack_id (fun f -> "StackId", String.to_json f)
         ])

  let of_json j =
    { stack_id = Aws.Util.option_map (Aws.Json.lookup j "StackId") String.of_json
    ; stack_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackName"))
    ; change_set_id = Aws.Util.option_map (Aws.Json.lookup j "ChangeSetId") String.of_json
    ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
    ; parameters =
        Parameters.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Parameters"))
    ; creation_time =
        DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CreationTime"))
    ; deletion_time =
        Aws.Util.option_map (Aws.Json.lookup j "DeletionTime") DateTime.of_json
    ; last_updated_time =
        Aws.Util.option_map (Aws.Json.lookup j "LastUpdatedTime") DateTime.of_json
    ; rollback_configuration =
        Aws.Util.option_map
          (Aws.Json.lookup j "RollbackConfiguration")
          RollbackConfiguration.of_json
    ; stack_status =
        StackStatus.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackStatus"))
    ; stack_status_reason =
        Aws.Util.option_map (Aws.Json.lookup j "StackStatusReason") String.of_json
    ; disable_rollback =
        Aws.Util.option_map (Aws.Json.lookup j "DisableRollback") Boolean.of_json
    ; notification_a_r_ns =
        NotificationARNs.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "NotificationARNs"))
    ; timeout_in_minutes =
        Aws.Util.option_map (Aws.Json.lookup j "TimeoutInMinutes") Integer.of_json
    ; capabilities =
        Capabilities.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Capabilities"))
    ; outputs = Outputs.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Outputs"))
    ; role_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "RoleARN") String.of_json
    ; tags = Tags.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
    ; enable_termination_protection =
        Aws.Util.option_map
          (Aws.Json.lookup j "EnableTerminationProtection")
          Boolean.of_json
    ; parent_id = Aws.Util.option_map (Aws.Json.lookup j "ParentId") String.of_json
    ; root_id = Aws.Util.option_map (Aws.Json.lookup j "RootId") String.of_json
    ; drift_information =
        Aws.Util.option_map
          (Aws.Json.lookup j "DriftInformation")
          StackDriftInformation.of_json
    }
end

module Stacks = struct
  type t = Stack.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map Stack.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list Stack.to_query v

  let to_json v = `List (List.map Stack.to_json v)

  let of_json j = Aws.Json.to_list Stack.of_json j
end

module DescribeStacksOutput = struct
  type t =
    { stacks : Stacks.t
    ; next_token : String.t option
    }

  let make ?(stacks = []) ?next_token () = { stacks; next_token }

  let parse xml =
    Some
      { stacks =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Stacks" xml) Stacks.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some (Aws.Query.Pair ("Stacks.member", Stacks.to_query v.stacks))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("Stacks", Stacks.to_json v.stacks)
         ])

  let of_json j =
    { stacks = Stacks.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Stacks"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module CreateStackInstancesOutput = struct
  type t = { operation_id : String.t option }

  let make ?operation_id () = { operation_id }

  let parse xml =
    Some
      { operation_id =
          Aws.Util.option_bind (Aws.Xml.member "OperationId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.operation_id (fun f ->
               Aws.Query.Pair ("OperationId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.operation_id (fun f -> "OperationId", String.to_json f) ])

  let of_json j =
    { operation_id = Aws.Util.option_map (Aws.Json.lookup j "OperationId") String.of_json
    }
end

module ResourceSignalStatus = struct
  type t =
    | SUCCESS
    | FAILURE

  let str_to_t = [ "FAILURE", FAILURE; "SUCCESS", SUCCESS ]

  let t_to_str = [ FAILURE, "FAILURE"; SUCCESS, "SUCCESS" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module StackInstanceStatus = struct
  type t =
    | CURRENT
    | OUTDATED
    | INOPERABLE

  let str_to_t = [ "INOPERABLE", INOPERABLE; "OUTDATED", OUTDATED; "CURRENT", CURRENT ]

  let t_to_str = [ INOPERABLE, "INOPERABLE"; OUTDATED, "OUTDATED"; CURRENT, "CURRENT" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module StackInstanceComprehensiveStatus = struct
  type t = { detailed_status : StackInstanceDetailedStatus.t option }

  let make ?detailed_status () = { detailed_status }

  let parse xml =
    Some
      { detailed_status =
          Aws.Util.option_bind
            (Aws.Xml.member "DetailedStatus" xml)
            StackInstanceDetailedStatus.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.detailed_status (fun f ->
               Aws.Query.Pair ("DetailedStatus", StackInstanceDetailedStatus.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.detailed_status (fun f ->
               "DetailedStatus", StackInstanceDetailedStatus.to_json f)
         ])

  let of_json j =
    { detailed_status =
        Aws.Util.option_map
          (Aws.Json.lookup j "DetailedStatus")
          StackInstanceDetailedStatus.of_json
    }
end

module StackInstanceSummary = struct
  type t =
    { stack_set_id : String.t option
    ; region : String.t option
    ; account : String.t option
    ; stack_id : String.t option
    ; status : StackInstanceStatus.t option
    ; status_reason : String.t option
    ; stack_instance_status : StackInstanceComprehensiveStatus.t option
    ; organizational_unit_id : String.t option
    ; drift_status : StackDriftStatus.t option
    ; last_drift_check_timestamp : DateTime.t option
    }

  let make
      ?stack_set_id
      ?region
      ?account
      ?stack_id
      ?status
      ?status_reason
      ?stack_instance_status
      ?organizational_unit_id
      ?drift_status
      ?last_drift_check_timestamp
      () =
    { stack_set_id
    ; region
    ; account
    ; stack_id
    ; status
    ; status_reason
    ; stack_instance_status
    ; organizational_unit_id
    ; drift_status
    ; last_drift_check_timestamp
    }

  let parse xml =
    Some
      { stack_set_id = Aws.Util.option_bind (Aws.Xml.member "StackSetId" xml) String.parse
      ; region = Aws.Util.option_bind (Aws.Xml.member "Region" xml) String.parse
      ; account = Aws.Util.option_bind (Aws.Xml.member "Account" xml) String.parse
      ; stack_id = Aws.Util.option_bind (Aws.Xml.member "StackId" xml) String.parse
      ; status =
          Aws.Util.option_bind (Aws.Xml.member "Status" xml) StackInstanceStatus.parse
      ; status_reason =
          Aws.Util.option_bind (Aws.Xml.member "StatusReason" xml) String.parse
      ; stack_instance_status =
          Aws.Util.option_bind
            (Aws.Xml.member "StackInstanceStatus" xml)
            StackInstanceComprehensiveStatus.parse
      ; organizational_unit_id =
          Aws.Util.option_bind (Aws.Xml.member "OrganizationalUnitId" xml) String.parse
      ; drift_status =
          Aws.Util.option_bind (Aws.Xml.member "DriftStatus" xml) StackDriftStatus.parse
      ; last_drift_check_timestamp =
          Aws.Util.option_bind
            (Aws.Xml.member "LastDriftCheckTimestamp" xml)
            DateTime.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.last_drift_check_timestamp (fun f ->
               Aws.Query.Pair ("LastDriftCheckTimestamp", DateTime.to_query f))
         ; Aws.Util.option_map v.drift_status (fun f ->
               Aws.Query.Pair ("DriftStatus", StackDriftStatus.to_query f))
         ; Aws.Util.option_map v.organizational_unit_id (fun f ->
               Aws.Query.Pair ("OrganizationalUnitId", String.to_query f))
         ; Aws.Util.option_map v.stack_instance_status (fun f ->
               Aws.Query.Pair
                 ("StackInstanceStatus", StackInstanceComprehensiveStatus.to_query f))
         ; Aws.Util.option_map v.status_reason (fun f ->
               Aws.Query.Pair ("StatusReason", String.to_query f))
         ; Aws.Util.option_map v.status (fun f ->
               Aws.Query.Pair ("Status", StackInstanceStatus.to_query f))
         ; Aws.Util.option_map v.stack_id (fun f ->
               Aws.Query.Pair ("StackId", String.to_query f))
         ; Aws.Util.option_map v.account (fun f ->
               Aws.Query.Pair ("Account", String.to_query f))
         ; Aws.Util.option_map v.region (fun f ->
               Aws.Query.Pair ("Region", String.to_query f))
         ; Aws.Util.option_map v.stack_set_id (fun f ->
               Aws.Query.Pair ("StackSetId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.last_drift_check_timestamp (fun f ->
               "LastDriftCheckTimestamp", DateTime.to_json f)
         ; Aws.Util.option_map v.drift_status (fun f ->
               "DriftStatus", StackDriftStatus.to_json f)
         ; Aws.Util.option_map v.organizational_unit_id (fun f ->
               "OrganizationalUnitId", String.to_json f)
         ; Aws.Util.option_map v.stack_instance_status (fun f ->
               "StackInstanceStatus", StackInstanceComprehensiveStatus.to_json f)
         ; Aws.Util.option_map v.status_reason (fun f -> "StatusReason", String.to_json f)
         ; Aws.Util.option_map v.status (fun f -> "Status", StackInstanceStatus.to_json f)
         ; Aws.Util.option_map v.stack_id (fun f -> "StackId", String.to_json f)
         ; Aws.Util.option_map v.account (fun f -> "Account", String.to_json f)
         ; Aws.Util.option_map v.region (fun f -> "Region", String.to_json f)
         ; Aws.Util.option_map v.stack_set_id (fun f -> "StackSetId", String.to_json f)
         ])

  let of_json j =
    { stack_set_id = Aws.Util.option_map (Aws.Json.lookup j "StackSetId") String.of_json
    ; region = Aws.Util.option_map (Aws.Json.lookup j "Region") String.of_json
    ; account = Aws.Util.option_map (Aws.Json.lookup j "Account") String.of_json
    ; stack_id = Aws.Util.option_map (Aws.Json.lookup j "StackId") String.of_json
    ; status =
        Aws.Util.option_map (Aws.Json.lookup j "Status") StackInstanceStatus.of_json
    ; status_reason =
        Aws.Util.option_map (Aws.Json.lookup j "StatusReason") String.of_json
    ; stack_instance_status =
        Aws.Util.option_map
          (Aws.Json.lookup j "StackInstanceStatus")
          StackInstanceComprehensiveStatus.of_json
    ; organizational_unit_id =
        Aws.Util.option_map (Aws.Json.lookup j "OrganizationalUnitId") String.of_json
    ; drift_status =
        Aws.Util.option_map (Aws.Json.lookup j "DriftStatus") StackDriftStatus.of_json
    ; last_drift_check_timestamp =
        Aws.Util.option_map (Aws.Json.lookup j "LastDriftCheckTimestamp") DateTime.of_json
    }
end

module StackInstanceSummaries = struct
  type t = StackInstanceSummary.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map StackInstanceSummary.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list StackInstanceSummary.to_query v

  let to_json v = `List (List.map StackInstanceSummary.to_json v)

  let of_json j = Aws.Json.to_list StackInstanceSummary.of_json j
end

module ListStackInstancesOutput = struct
  type t =
    { summaries : StackInstanceSummaries.t
    ; next_token : String.t option
    }

  let make ?(summaries = []) ?next_token () = { summaries; next_token }

  let parse xml =
    Some
      { summaries =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "Summaries" xml)
               StackInstanceSummaries.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("Summaries.member", StackInstanceSummaries.to_query v.summaries))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("Summaries", StackInstanceSummaries.to_json v.summaries)
         ])

  let of_json j =
    { summaries =
        StackInstanceSummaries.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Summaries"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module UpdateStackOutput = struct
  type t = { stack_id : String.t option }

  let make ?stack_id () = { stack_id }

  let parse xml =
    Some { stack_id = Aws.Util.option_bind (Aws.Xml.member "StackId" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.stack_id (fun f ->
               Aws.Query.Pair ("StackId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.stack_id (fun f -> "StackId", String.to_json f) ])

  let of_json j =
    { stack_id = Aws.Util.option_map (Aws.Json.lookup j "StackId") String.of_json }
end

module GetTemplateSummaryInput = struct
  type t =
    { template_body : String.t option
    ; template_u_r_l : String.t option
    ; stack_name : String.t option
    ; stack_set_name : String.t option
    }

  let make ?template_body ?template_u_r_l ?stack_name ?stack_set_name () =
    { template_body; template_u_r_l; stack_name; stack_set_name }

  let parse xml =
    Some
      { template_body =
          Aws.Util.option_bind (Aws.Xml.member "TemplateBody" xml) String.parse
      ; template_u_r_l =
          Aws.Util.option_bind (Aws.Xml.member "TemplateURL" xml) String.parse
      ; stack_name = Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse
      ; stack_set_name =
          Aws.Util.option_bind (Aws.Xml.member "StackSetName" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.stack_set_name (fun f ->
               Aws.Query.Pair ("StackSetName", String.to_query f))
         ; Aws.Util.option_map v.stack_name (fun f ->
               Aws.Query.Pair ("StackName", String.to_query f))
         ; Aws.Util.option_map v.template_u_r_l (fun f ->
               Aws.Query.Pair ("TemplateURL", String.to_query f))
         ; Aws.Util.option_map v.template_body (fun f ->
               Aws.Query.Pair ("TemplateBody", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.stack_set_name (fun f ->
               "StackSetName", String.to_json f)
         ; Aws.Util.option_map v.stack_name (fun f -> "StackName", String.to_json f)
         ; Aws.Util.option_map v.template_u_r_l (fun f -> "TemplateURL", String.to_json f)
         ; Aws.Util.option_map v.template_body (fun f -> "TemplateBody", String.to_json f)
         ])

  let of_json j =
    { template_body =
        Aws.Util.option_map (Aws.Json.lookup j "TemplateBody") String.of_json
    ; template_u_r_l =
        Aws.Util.option_map (Aws.Json.lookup j "TemplateURL") String.of_json
    ; stack_name = Aws.Util.option_map (Aws.Json.lookup j "StackName") String.of_json
    ; stack_set_name =
        Aws.Util.option_map (Aws.Json.lookup j "StackSetName") String.of_json
    }
end

module ResourceIdentifiers = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module ResourceIdentifierSummary = struct
  type t =
    { resource_type : String.t option
    ; logical_resource_ids : LogicalResourceIds.t
    ; resource_identifiers : ResourceIdentifiers.t
    }

  let make ?resource_type ?(logical_resource_ids = []) ?(resource_identifiers = []) () =
    { resource_type; logical_resource_ids; resource_identifiers }

  let parse xml =
    Some
      { resource_type =
          Aws.Util.option_bind (Aws.Xml.member "ResourceType" xml) String.parse
      ; logical_resource_ids =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "LogicalResourceIds" xml)
               LogicalResourceIds.parse)
      ; resource_identifiers =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ResourceIdentifiers" xml)
               ResourceIdentifiers.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "ResourceIdentifiers.member"
                , ResourceIdentifiers.to_query v.resource_identifiers ))
         ; Some
             (Aws.Query.Pair
                ( "LogicalResourceIds.member"
                , LogicalResourceIds.to_query v.logical_resource_ids ))
         ; Aws.Util.option_map v.resource_type (fun f ->
               Aws.Query.Pair ("ResourceType", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("ResourceIdentifiers", ResourceIdentifiers.to_json v.resource_identifiers)
         ; Some ("LogicalResourceIds", LogicalResourceIds.to_json v.logical_resource_ids)
         ; Aws.Util.option_map v.resource_type (fun f -> "ResourceType", String.to_json f)
         ])

  let of_json j =
    { resource_type =
        Aws.Util.option_map (Aws.Json.lookup j "ResourceType") String.of_json
    ; logical_resource_ids =
        LogicalResourceIds.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "LogicalResourceIds"))
    ; resource_identifiers =
        ResourceIdentifiers.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceIdentifiers"))
    }
end

module ResourceIdentifierSummaries = struct
  type t = ResourceIdentifierSummary.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map ResourceIdentifierSummary.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list ResourceIdentifierSummary.to_query v

  let to_json v = `List (List.map ResourceIdentifierSummary.to_json v)

  let of_json j = Aws.Json.to_list ResourceIdentifierSummary.of_json j
end

module ResourceAttribute = struct
  type t =
    | Properties
    | Metadata
    | CreationPolicy
    | UpdatePolicy
    | DeletionPolicy
    | Tags

  let str_to_t =
    [ "Tags", Tags
    ; "DeletionPolicy", DeletionPolicy
    ; "UpdatePolicy", UpdatePolicy
    ; "CreationPolicy", CreationPolicy
    ; "Metadata", Metadata
    ; "Properties", Properties
    ]

  let t_to_str =
    [ Tags, "Tags"
    ; DeletionPolicy, "DeletionPolicy"
    ; UpdatePolicy, "UpdatePolicy"
    ; CreationPolicy, "CreationPolicy"
    ; Metadata, "Metadata"
    ; Properties, "Properties"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module RequiresRecreation = struct
  type t =
    | Never
    | Conditionally
    | Always

  let str_to_t = [ "Always", Always; "Conditionally", Conditionally; "Never", Never ]

  let t_to_str = [ Always, "Always"; Conditionally, "Conditionally"; Never, "Never" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module ResourceTargetDefinition = struct
  type t =
    { attribute : ResourceAttribute.t option
    ; name : String.t option
    ; requires_recreation : RequiresRecreation.t option
    }

  let make ?attribute ?name ?requires_recreation () =
    { attribute; name; requires_recreation }

  let parse xml =
    Some
      { attribute =
          Aws.Util.option_bind (Aws.Xml.member "Attribute" xml) ResourceAttribute.parse
      ; name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
      ; requires_recreation =
          Aws.Util.option_bind
            (Aws.Xml.member "RequiresRecreation" xml)
            RequiresRecreation.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.requires_recreation (fun f ->
               Aws.Query.Pair ("RequiresRecreation", RequiresRecreation.to_query f))
         ; Aws.Util.option_map v.name (fun f ->
               Aws.Query.Pair ("Name", String.to_query f))
         ; Aws.Util.option_map v.attribute (fun f ->
               Aws.Query.Pair ("Attribute", ResourceAttribute.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.requires_recreation (fun f ->
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
    }
end

module EvaluationType = struct
  type t =
    | Static
    | Dynamic

  let str_to_t = [ "Dynamic", Dynamic; "Static", Static ]

  let t_to_str = [ Dynamic, "Dynamic"; Static, "Static" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module ChangeSource = struct
  type t =
    | ResourceReference
    | ParameterReference
    | ResourceAttribute
    | DirectModification
    | Automatic

  let str_to_t =
    [ "Automatic", Automatic
    ; "DirectModification", DirectModification
    ; "ResourceAttribute", ResourceAttribute
    ; "ParameterReference", ParameterReference
    ; "ResourceReference", ResourceReference
    ]

  let t_to_str =
    [ Automatic, "Automatic"
    ; DirectModification, "DirectModification"
    ; ResourceAttribute, "ResourceAttribute"
    ; ParameterReference, "ParameterReference"
    ; ResourceReference, "ResourceReference"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module ResourceChangeDetail = struct
  type t =
    { target : ResourceTargetDefinition.t option
    ; evaluation : EvaluationType.t option
    ; change_source : ChangeSource.t option
    ; causing_entity : String.t option
    }

  let make ?target ?evaluation ?change_source ?causing_entity () =
    { target; evaluation; change_source; causing_entity }

  let parse xml =
    Some
      { target =
          Aws.Util.option_bind
            (Aws.Xml.member "Target" xml)
            ResourceTargetDefinition.parse
      ; evaluation =
          Aws.Util.option_bind (Aws.Xml.member "Evaluation" xml) EvaluationType.parse
      ; change_source =
          Aws.Util.option_bind (Aws.Xml.member "ChangeSource" xml) ChangeSource.parse
      ; causing_entity =
          Aws.Util.option_bind (Aws.Xml.member "CausingEntity" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.causing_entity (fun f ->
               Aws.Query.Pair ("CausingEntity", String.to_query f))
         ; Aws.Util.option_map v.change_source (fun f ->
               Aws.Query.Pair ("ChangeSource", ChangeSource.to_query f))
         ; Aws.Util.option_map v.evaluation (fun f ->
               Aws.Query.Pair ("Evaluation", EvaluationType.to_query f))
         ; Aws.Util.option_map v.target (fun f ->
               Aws.Query.Pair ("Target", ResourceTargetDefinition.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.causing_entity (fun f ->
               "CausingEntity", String.to_json f)
         ; Aws.Util.option_map v.change_source (fun f ->
               "ChangeSource", ChangeSource.to_json f)
         ; Aws.Util.option_map v.evaluation (fun f ->
               "Evaluation", EvaluationType.to_json f)
         ; Aws.Util.option_map v.target (fun f ->
               "Target", ResourceTargetDefinition.to_json f)
         ])

  let of_json j =
    { target =
        Aws.Util.option_map (Aws.Json.lookup j "Target") ResourceTargetDefinition.of_json
    ; evaluation =
        Aws.Util.option_map (Aws.Json.lookup j "Evaluation") EvaluationType.of_json
    ; change_source =
        Aws.Util.option_map (Aws.Json.lookup j "ChangeSource") ChangeSource.of_json
    ; causing_entity =
        Aws.Util.option_map (Aws.Json.lookup j "CausingEntity") String.of_json
    }
end

module UpdateStackSetInput = struct
  type t =
    { stack_set_name : String.t
    ; description : String.t option
    ; template_body : String.t option
    ; template_u_r_l : String.t option
    ; use_previous_template : Boolean.t option
    ; parameters : Parameters.t
    ; capabilities : Capabilities.t
    ; tags : Tags.t
    ; operation_preferences : StackSetOperationPreferences.t option
    ; administration_role_a_r_n : String.t option
    ; execution_role_name : String.t option
    ; deployment_targets : DeploymentTargets.t option
    ; permission_model : PermissionModels.t option
    ; auto_deployment : AutoDeployment.t option
    ; operation_id : String.t option
    ; accounts : AccountList.t
    ; regions : RegionList.t
    }

  let make
      ~stack_set_name
      ?description
      ?template_body
      ?template_u_r_l
      ?use_previous_template
      ?(parameters = [])
      ?(capabilities = [])
      ?(tags = [])
      ?operation_preferences
      ?administration_role_a_r_n
      ?execution_role_name
      ?deployment_targets
      ?permission_model
      ?auto_deployment
      ?operation_id
      ?(accounts = [])
      ?(regions = [])
      () =
    { stack_set_name
    ; description
    ; template_body
    ; template_u_r_l
    ; use_previous_template
    ; parameters
    ; capabilities
    ; tags
    ; operation_preferences
    ; administration_role_a_r_n
    ; execution_role_name
    ; deployment_targets
    ; permission_model
    ; auto_deployment
    ; operation_id
    ; accounts
    ; regions
    }

  let parse xml =
    Some
      { stack_set_name =
          Aws.Xml.required
            "StackSetName"
            (Aws.Util.option_bind (Aws.Xml.member "StackSetName" xml) String.parse)
      ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
      ; template_body =
          Aws.Util.option_bind (Aws.Xml.member "TemplateBody" xml) String.parse
      ; template_u_r_l =
          Aws.Util.option_bind (Aws.Xml.member "TemplateURL" xml) String.parse
      ; use_previous_template =
          Aws.Util.option_bind (Aws.Xml.member "UsePreviousTemplate" xml) Boolean.parse
      ; parameters =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Parameters" xml) Parameters.parse)
      ; capabilities =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Capabilities" xml) Capabilities.parse)
      ; tags =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) Tags.parse)
      ; operation_preferences =
          Aws.Util.option_bind
            (Aws.Xml.member "OperationPreferences" xml)
            StackSetOperationPreferences.parse
      ; administration_role_a_r_n =
          Aws.Util.option_bind (Aws.Xml.member "AdministrationRoleARN" xml) String.parse
      ; execution_role_name =
          Aws.Util.option_bind (Aws.Xml.member "ExecutionRoleName" xml) String.parse
      ; deployment_targets =
          Aws.Util.option_bind
            (Aws.Xml.member "DeploymentTargets" xml)
            DeploymentTargets.parse
      ; permission_model =
          Aws.Util.option_bind
            (Aws.Xml.member "PermissionModel" xml)
            PermissionModels.parse
      ; auto_deployment =
          Aws.Util.option_bind (Aws.Xml.member "AutoDeployment" xml) AutoDeployment.parse
      ; operation_id =
          Aws.Util.option_bind (Aws.Xml.member "OperationId" xml) String.parse
      ; accounts =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Accounts" xml) AccountList.parse)
      ; regions =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Regions" xml) RegionList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Regions.member", RegionList.to_query v.regions))
         ; Some (Aws.Query.Pair ("Accounts.member", AccountList.to_query v.accounts))
         ; Aws.Util.option_map v.operation_id (fun f ->
               Aws.Query.Pair ("OperationId", String.to_query f))
         ; Aws.Util.option_map v.auto_deployment (fun f ->
               Aws.Query.Pair ("AutoDeployment", AutoDeployment.to_query f))
         ; Aws.Util.option_map v.permission_model (fun f ->
               Aws.Query.Pair ("PermissionModel", PermissionModels.to_query f))
         ; Aws.Util.option_map v.deployment_targets (fun f ->
               Aws.Query.Pair ("DeploymentTargets", DeploymentTargets.to_query f))
         ; Aws.Util.option_map v.execution_role_name (fun f ->
               Aws.Query.Pair ("ExecutionRoleName", String.to_query f))
         ; Aws.Util.option_map v.administration_role_a_r_n (fun f ->
               Aws.Query.Pair ("AdministrationRoleARN", String.to_query f))
         ; Aws.Util.option_map v.operation_preferences (fun f ->
               Aws.Query.Pair
                 ("OperationPreferences", StackSetOperationPreferences.to_query f))
         ; Some (Aws.Query.Pair ("Tags.member", Tags.to_query v.tags))
         ; Some
             (Aws.Query.Pair ("Capabilities.member", Capabilities.to_query v.capabilities))
         ; Some (Aws.Query.Pair ("Parameters.member", Parameters.to_query v.parameters))
         ; Aws.Util.option_map v.use_previous_template (fun f ->
               Aws.Query.Pair ("UsePreviousTemplate", Boolean.to_query f))
         ; Aws.Util.option_map v.template_u_r_l (fun f ->
               Aws.Query.Pair ("TemplateURL", String.to_query f))
         ; Aws.Util.option_map v.template_body (fun f ->
               Aws.Query.Pair ("TemplateBody", String.to_query f))
         ; Aws.Util.option_map v.description (fun f ->
               Aws.Query.Pair ("Description", String.to_query f))
         ; Some (Aws.Query.Pair ("StackSetName", String.to_query v.stack_set_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Regions", RegionList.to_json v.regions)
         ; Some ("Accounts", AccountList.to_json v.accounts)
         ; Aws.Util.option_map v.operation_id (fun f -> "OperationId", String.to_json f)
         ; Aws.Util.option_map v.auto_deployment (fun f ->
               "AutoDeployment", AutoDeployment.to_json f)
         ; Aws.Util.option_map v.permission_model (fun f ->
               "PermissionModel", PermissionModels.to_json f)
         ; Aws.Util.option_map v.deployment_targets (fun f ->
               "DeploymentTargets", DeploymentTargets.to_json f)
         ; Aws.Util.option_map v.execution_role_name (fun f ->
               "ExecutionRoleName", String.to_json f)
         ; Aws.Util.option_map v.administration_role_a_r_n (fun f ->
               "AdministrationRoleARN", String.to_json f)
         ; Aws.Util.option_map v.operation_preferences (fun f ->
               "OperationPreferences", StackSetOperationPreferences.to_json f)
         ; Some ("Tags", Tags.to_json v.tags)
         ; Some ("Capabilities", Capabilities.to_json v.capabilities)
         ; Some ("Parameters", Parameters.to_json v.parameters)
         ; Aws.Util.option_map v.use_previous_template (fun f ->
               "UsePreviousTemplate", Boolean.to_json f)
         ; Aws.Util.option_map v.template_u_r_l (fun f -> "TemplateURL", String.to_json f)
         ; Aws.Util.option_map v.template_body (fun f -> "TemplateBody", String.to_json f)
         ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
         ; Some ("StackSetName", String.to_json v.stack_set_name)
         ])

  let of_json j =
    { stack_set_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackSetName"))
    ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
    ; template_body =
        Aws.Util.option_map (Aws.Json.lookup j "TemplateBody") String.of_json
    ; template_u_r_l =
        Aws.Util.option_map (Aws.Json.lookup j "TemplateURL") String.of_json
    ; use_previous_template =
        Aws.Util.option_map (Aws.Json.lookup j "UsePreviousTemplate") Boolean.of_json
    ; parameters =
        Parameters.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Parameters"))
    ; capabilities =
        Capabilities.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Capabilities"))
    ; tags = Tags.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
    ; operation_preferences =
        Aws.Util.option_map
          (Aws.Json.lookup j "OperationPreferences")
          StackSetOperationPreferences.of_json
    ; administration_role_a_r_n =
        Aws.Util.option_map (Aws.Json.lookup j "AdministrationRoleARN") String.of_json
    ; execution_role_name =
        Aws.Util.option_map (Aws.Json.lookup j "ExecutionRoleName") String.of_json
    ; deployment_targets =
        Aws.Util.option_map
          (Aws.Json.lookup j "DeploymentTargets")
          DeploymentTargets.of_json
    ; permission_model =
        Aws.Util.option_map (Aws.Json.lookup j "PermissionModel") PermissionModels.of_json
    ; auto_deployment =
        Aws.Util.option_map (Aws.Json.lookup j "AutoDeployment") AutoDeployment.of_json
    ; operation_id = Aws.Util.option_map (Aws.Json.lookup j "OperationId") String.of_json
    ; accounts =
        AccountList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Accounts"))
    ; regions = RegionList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Regions"))
    }
end

module InvalidStateTransitionException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ResourceChangeDetails = struct
  type t = ResourceChangeDetail.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map ResourceChangeDetail.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list ResourceChangeDetail.to_query v

  let to_json v = `List (List.map ResourceChangeDetail.to_json v)

  let of_json j = Aws.Json.to_list ResourceChangeDetail.of_json j
end

module UpdateTerminationProtectionOutput = struct
  type t = { stack_id : String.t option }

  let make ?stack_id () = { stack_id }

  let parse xml =
    Some { stack_id = Aws.Util.option_bind (Aws.Xml.member "StackId" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.stack_id (fun f ->
               Aws.Query.Pair ("StackId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.stack_id (fun f -> "StackId", String.to_json f) ])

  let of_json j =
    { stack_id = Aws.Util.option_map (Aws.Json.lookup j "StackId") String.of_json }
end

module RegistryType = struct
  type t = RESOURCE

  let str_to_t = [ "RESOURCE", RESOURCE ]

  let t_to_str = [ RESOURCE, "RESOURCE" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module TypeVersionSummary = struct
  type t =
    { type_ : RegistryType.t option
    ; type_name : String.t option
    ; version_id : String.t option
    ; is_default_version : Boolean.t option
    ; arn : String.t option
    ; time_created : DateTime.t option
    ; description : String.t option
    }

  let make
      ?type_
      ?type_name
      ?version_id
      ?is_default_version
      ?arn
      ?time_created
      ?description
      () =
    { type_; type_name; version_id; is_default_version; arn; time_created; description }

  let parse xml =
    Some
      { type_ = Aws.Util.option_bind (Aws.Xml.member "Type" xml) RegistryType.parse
      ; type_name = Aws.Util.option_bind (Aws.Xml.member "TypeName" xml) String.parse
      ; version_id = Aws.Util.option_bind (Aws.Xml.member "VersionId" xml) String.parse
      ; is_default_version =
          Aws.Util.option_bind (Aws.Xml.member "IsDefaultVersion" xml) Boolean.parse
      ; arn = Aws.Util.option_bind (Aws.Xml.member "Arn" xml) String.parse
      ; time_created =
          Aws.Util.option_bind (Aws.Xml.member "TimeCreated" xml) DateTime.parse
      ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.description (fun f ->
               Aws.Query.Pair ("Description", String.to_query f))
         ; Aws.Util.option_map v.time_created (fun f ->
               Aws.Query.Pair ("TimeCreated", DateTime.to_query f))
         ; Aws.Util.option_map v.arn (fun f -> Aws.Query.Pair ("Arn", String.to_query f))
         ; Aws.Util.option_map v.is_default_version (fun f ->
               Aws.Query.Pair ("IsDefaultVersion", Boolean.to_query f))
         ; Aws.Util.option_map v.version_id (fun f ->
               Aws.Query.Pair ("VersionId", String.to_query f))
         ; Aws.Util.option_map v.type_name (fun f ->
               Aws.Query.Pair ("TypeName", String.to_query f))
         ; Aws.Util.option_map v.type_ (fun f ->
               Aws.Query.Pair ("Type", RegistryType.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
         ; Aws.Util.option_map v.time_created (fun f -> "TimeCreated", DateTime.to_json f)
         ; Aws.Util.option_map v.arn (fun f -> "Arn", String.to_json f)
         ; Aws.Util.option_map v.is_default_version (fun f ->
               "IsDefaultVersion", Boolean.to_json f)
         ; Aws.Util.option_map v.version_id (fun f -> "VersionId", String.to_json f)
         ; Aws.Util.option_map v.type_name (fun f -> "TypeName", String.to_json f)
         ; Aws.Util.option_map v.type_ (fun f -> "Type", RegistryType.to_json f)
         ])

  let of_json j =
    { type_ = Aws.Util.option_map (Aws.Json.lookup j "Type") RegistryType.of_json
    ; type_name = Aws.Util.option_map (Aws.Json.lookup j "TypeName") String.of_json
    ; version_id = Aws.Util.option_map (Aws.Json.lookup j "VersionId") String.of_json
    ; is_default_version =
        Aws.Util.option_map (Aws.Json.lookup j "IsDefaultVersion") Boolean.of_json
    ; arn = Aws.Util.option_map (Aws.Json.lookup j "Arn") String.of_json
    ; time_created =
        Aws.Util.option_map (Aws.Json.lookup j "TimeCreated") DateTime.of_json
    ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
    }
end

module LoggingConfig = struct
  type t =
    { log_role_arn : String.t
    ; log_group_name : String.t
    }

  let make ~log_role_arn ~log_group_name () = { log_role_arn; log_group_name }

  let parse xml =
    Some
      { log_role_arn =
          Aws.Xml.required
            "LogRoleArn"
            (Aws.Util.option_bind (Aws.Xml.member "LogRoleArn" xml) String.parse)
      ; log_group_name =
          Aws.Xml.required
            "LogGroupName"
            (Aws.Util.option_bind (Aws.Xml.member "LogGroupName" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("LogGroupName", String.to_query v.log_group_name))
         ; Some (Aws.Query.Pair ("LogRoleArn", String.to_query v.log_role_arn))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("LogGroupName", String.to_json v.log_group_name)
         ; Some ("LogRoleArn", String.to_json v.log_role_arn)
         ])

  let of_json j =
    { log_role_arn =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LogRoleArn"))
    ; log_group_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LogGroupName"))
    }
end

module StackSetNotEmptyException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module SetTypeDefaultVersionInput = struct
  type t =
    { arn : String.t option
    ; type_ : RegistryType.t option
    ; type_name : String.t option
    ; version_id : String.t option
    }

  let make ?arn ?type_ ?type_name ?version_id () = { arn; type_; type_name; version_id }

  let parse xml =
    Some
      { arn = Aws.Util.option_bind (Aws.Xml.member "Arn" xml) String.parse
      ; type_ = Aws.Util.option_bind (Aws.Xml.member "Type" xml) RegistryType.parse
      ; type_name = Aws.Util.option_bind (Aws.Xml.member "TypeName" xml) String.parse
      ; version_id = Aws.Util.option_bind (Aws.Xml.member "VersionId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.version_id (fun f ->
               Aws.Query.Pair ("VersionId", String.to_query f))
         ; Aws.Util.option_map v.type_name (fun f ->
               Aws.Query.Pair ("TypeName", String.to_query f))
         ; Aws.Util.option_map v.type_ (fun f ->
               Aws.Query.Pair ("Type", RegistryType.to_query f))
         ; Aws.Util.option_map v.arn (fun f -> Aws.Query.Pair ("Arn", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.version_id (fun f -> "VersionId", String.to_json f)
         ; Aws.Util.option_map v.type_name (fun f -> "TypeName", String.to_json f)
         ; Aws.Util.option_map v.type_ (fun f -> "Type", RegistryType.to_json f)
         ; Aws.Util.option_map v.arn (fun f -> "Arn", String.to_json f)
         ])

  let of_json j =
    { arn = Aws.Util.option_map (Aws.Json.lookup j "Arn") String.of_json
    ; type_ = Aws.Util.option_map (Aws.Json.lookup j "Type") RegistryType.of_json
    ; type_name = Aws.Util.option_map (Aws.Json.lookup j "TypeName") String.of_json
    ; version_id = Aws.Util.option_map (Aws.Json.lookup j "VersionId") String.of_json
    }
end

module ParameterDeclaration = struct
  type t =
    { parameter_key : String.t option
    ; default_value : String.t option
    ; parameter_type : String.t option
    ; no_echo : Boolean.t option
    ; description : String.t option
    ; parameter_constraints : ParameterConstraints.t option
    }

  let make
      ?parameter_key
      ?default_value
      ?parameter_type
      ?no_echo
      ?description
      ?parameter_constraints
      () =
    { parameter_key
    ; default_value
    ; parameter_type
    ; no_echo
    ; description
    ; parameter_constraints
    }

  let parse xml =
    Some
      { parameter_key =
          Aws.Util.option_bind (Aws.Xml.member "ParameterKey" xml) String.parse
      ; default_value =
          Aws.Util.option_bind (Aws.Xml.member "DefaultValue" xml) String.parse
      ; parameter_type =
          Aws.Util.option_bind (Aws.Xml.member "ParameterType" xml) String.parse
      ; no_echo = Aws.Util.option_bind (Aws.Xml.member "NoEcho" xml) Boolean.parse
      ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
      ; parameter_constraints =
          Aws.Util.option_bind
            (Aws.Xml.member "ParameterConstraints" xml)
            ParameterConstraints.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.parameter_constraints (fun f ->
               Aws.Query.Pair ("ParameterConstraints", ParameterConstraints.to_query f))
         ; Aws.Util.option_map v.description (fun f ->
               Aws.Query.Pair ("Description", String.to_query f))
         ; Aws.Util.option_map v.no_echo (fun f ->
               Aws.Query.Pair ("NoEcho", Boolean.to_query f))
         ; Aws.Util.option_map v.parameter_type (fun f ->
               Aws.Query.Pair ("ParameterType", String.to_query f))
         ; Aws.Util.option_map v.default_value (fun f ->
               Aws.Query.Pair ("DefaultValue", String.to_query f))
         ; Aws.Util.option_map v.parameter_key (fun f ->
               Aws.Query.Pair ("ParameterKey", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.parameter_constraints (fun f ->
               "ParameterConstraints", ParameterConstraints.to_json f)
         ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
         ; Aws.Util.option_map v.no_echo (fun f -> "NoEcho", Boolean.to_json f)
         ; Aws.Util.option_map v.parameter_type (fun f ->
               "ParameterType", String.to_json f)
         ; Aws.Util.option_map v.default_value (fun f -> "DefaultValue", String.to_json f)
         ; Aws.Util.option_map v.parameter_key (fun f -> "ParameterKey", String.to_json f)
         ])

  let of_json j =
    { parameter_key =
        Aws.Util.option_map (Aws.Json.lookup j "ParameterKey") String.of_json
    ; default_value =
        Aws.Util.option_map (Aws.Json.lookup j "DefaultValue") String.of_json
    ; parameter_type =
        Aws.Util.option_map (Aws.Json.lookup j "ParameterType") String.of_json
    ; no_echo = Aws.Util.option_map (Aws.Json.lookup j "NoEcho") Boolean.of_json
    ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
    ; parameter_constraints =
        Aws.Util.option_map
          (Aws.Json.lookup j "ParameterConstraints")
          ParameterConstraints.of_json
    }
end

module StackSetDriftStatus = struct
  type t =
    | DRIFTED
    | IN_SYNC
    | NOT_CHECKED

  let str_to_t = [ "NOT_CHECKED", NOT_CHECKED; "IN_SYNC", IN_SYNC; "DRIFTED", DRIFTED ]

  let t_to_str = [ NOT_CHECKED, "NOT_CHECKED"; IN_SYNC, "IN_SYNC"; DRIFTED, "DRIFTED" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module StackSetDriftDetectionStatus = struct
  type t =
    | COMPLETED
    | FAILED
    | PARTIAL_SUCCESS
    | IN_PROGRESS
    | STOPPED

  let str_to_t =
    [ "STOPPED", STOPPED
    ; "IN_PROGRESS", IN_PROGRESS
    ; "PARTIAL_SUCCESS", PARTIAL_SUCCESS
    ; "FAILED", FAILED
    ; "COMPLETED", COMPLETED
    ]

  let t_to_str =
    [ STOPPED, "STOPPED"
    ; IN_PROGRESS, "IN_PROGRESS"
    ; PARTIAL_SUCCESS, "PARTIAL_SUCCESS"
    ; FAILED, "FAILED"
    ; COMPLETED, "COMPLETED"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module StackSetDriftDetectionDetails = struct
  type t =
    { drift_status : StackSetDriftStatus.t option
    ; drift_detection_status : StackSetDriftDetectionStatus.t option
    ; last_drift_check_timestamp : DateTime.t option
    ; total_stack_instances_count : Integer.t option
    ; drifted_stack_instances_count : Integer.t option
    ; in_sync_stack_instances_count : Integer.t option
    ; in_progress_stack_instances_count : Integer.t option
    ; failed_stack_instances_count : Integer.t option
    }

  let make
      ?drift_status
      ?drift_detection_status
      ?last_drift_check_timestamp
      ?total_stack_instances_count
      ?drifted_stack_instances_count
      ?in_sync_stack_instances_count
      ?in_progress_stack_instances_count
      ?failed_stack_instances_count
      () =
    { drift_status
    ; drift_detection_status
    ; last_drift_check_timestamp
    ; total_stack_instances_count
    ; drifted_stack_instances_count
    ; in_sync_stack_instances_count
    ; in_progress_stack_instances_count
    ; failed_stack_instances_count
    }

  let parse xml =
    Some
      { drift_status =
          Aws.Util.option_bind
            (Aws.Xml.member "DriftStatus" xml)
            StackSetDriftStatus.parse
      ; drift_detection_status =
          Aws.Util.option_bind
            (Aws.Xml.member "DriftDetectionStatus" xml)
            StackSetDriftDetectionStatus.parse
      ; last_drift_check_timestamp =
          Aws.Util.option_bind
            (Aws.Xml.member "LastDriftCheckTimestamp" xml)
            DateTime.parse
      ; total_stack_instances_count =
          Aws.Util.option_bind
            (Aws.Xml.member "TotalStackInstancesCount" xml)
            Integer.parse
      ; drifted_stack_instances_count =
          Aws.Util.option_bind
            (Aws.Xml.member "DriftedStackInstancesCount" xml)
            Integer.parse
      ; in_sync_stack_instances_count =
          Aws.Util.option_bind
            (Aws.Xml.member "InSyncStackInstancesCount" xml)
            Integer.parse
      ; in_progress_stack_instances_count =
          Aws.Util.option_bind
            (Aws.Xml.member "InProgressStackInstancesCount" xml)
            Integer.parse
      ; failed_stack_instances_count =
          Aws.Util.option_bind
            (Aws.Xml.member "FailedStackInstancesCount" xml)
            Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.failed_stack_instances_count (fun f ->
               Aws.Query.Pair ("FailedStackInstancesCount", Integer.to_query f))
         ; Aws.Util.option_map v.in_progress_stack_instances_count (fun f ->
               Aws.Query.Pair ("InProgressStackInstancesCount", Integer.to_query f))
         ; Aws.Util.option_map v.in_sync_stack_instances_count (fun f ->
               Aws.Query.Pair ("InSyncStackInstancesCount", Integer.to_query f))
         ; Aws.Util.option_map v.drifted_stack_instances_count (fun f ->
               Aws.Query.Pair ("DriftedStackInstancesCount", Integer.to_query f))
         ; Aws.Util.option_map v.total_stack_instances_count (fun f ->
               Aws.Query.Pair ("TotalStackInstancesCount", Integer.to_query f))
         ; Aws.Util.option_map v.last_drift_check_timestamp (fun f ->
               Aws.Query.Pair ("LastDriftCheckTimestamp", DateTime.to_query f))
         ; Aws.Util.option_map v.drift_detection_status (fun f ->
               Aws.Query.Pair
                 ("DriftDetectionStatus", StackSetDriftDetectionStatus.to_query f))
         ; Aws.Util.option_map v.drift_status (fun f ->
               Aws.Query.Pair ("DriftStatus", StackSetDriftStatus.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.failed_stack_instances_count (fun f ->
               "FailedStackInstancesCount", Integer.to_json f)
         ; Aws.Util.option_map v.in_progress_stack_instances_count (fun f ->
               "InProgressStackInstancesCount", Integer.to_json f)
         ; Aws.Util.option_map v.in_sync_stack_instances_count (fun f ->
               "InSyncStackInstancesCount", Integer.to_json f)
         ; Aws.Util.option_map v.drifted_stack_instances_count (fun f ->
               "DriftedStackInstancesCount", Integer.to_json f)
         ; Aws.Util.option_map v.total_stack_instances_count (fun f ->
               "TotalStackInstancesCount", Integer.to_json f)
         ; Aws.Util.option_map v.last_drift_check_timestamp (fun f ->
               "LastDriftCheckTimestamp", DateTime.to_json f)
         ; Aws.Util.option_map v.drift_detection_status (fun f ->
               "DriftDetectionStatus", StackSetDriftDetectionStatus.to_json f)
         ; Aws.Util.option_map v.drift_status (fun f ->
               "DriftStatus", StackSetDriftStatus.to_json f)
         ])

  let of_json j =
    { drift_status =
        Aws.Util.option_map (Aws.Json.lookup j "DriftStatus") StackSetDriftStatus.of_json
    ; drift_detection_status =
        Aws.Util.option_map
          (Aws.Json.lookup j "DriftDetectionStatus")
          StackSetDriftDetectionStatus.of_json
    ; last_drift_check_timestamp =
        Aws.Util.option_map (Aws.Json.lookup j "LastDriftCheckTimestamp") DateTime.of_json
    ; total_stack_instances_count =
        Aws.Util.option_map (Aws.Json.lookup j "TotalStackInstancesCount") Integer.of_json
    ; drifted_stack_instances_count =
        Aws.Util.option_map
          (Aws.Json.lookup j "DriftedStackInstancesCount")
          Integer.of_json
    ; in_sync_stack_instances_count =
        Aws.Util.option_map
          (Aws.Json.lookup j "InSyncStackInstancesCount")
          Integer.of_json
    ; in_progress_stack_instances_count =
        Aws.Util.option_map
          (Aws.Json.lookup j "InProgressStackInstancesCount")
          Integer.of_json
    ; failed_stack_instances_count =
        Aws.Util.option_map
          (Aws.Json.lookup j "FailedStackInstancesCount")
          Integer.of_json
    }
end

module StackSet = struct
  type t =
    { stack_set_name : String.t option
    ; stack_set_id : String.t option
    ; description : String.t option
    ; status : StackSetStatus.t option
    ; template_body : String.t option
    ; parameters : Parameters.t
    ; capabilities : Capabilities.t
    ; tags : Tags.t
    ; stack_set_a_r_n : String.t option
    ; administration_role_a_r_n : String.t option
    ; execution_role_name : String.t option
    ; stack_set_drift_detection_details : StackSetDriftDetectionDetails.t option
    ; auto_deployment : AutoDeployment.t option
    ; permission_model : PermissionModels.t option
    ; organizational_unit_ids : OrganizationalUnitIdList.t
    }

  let make
      ?stack_set_name
      ?stack_set_id
      ?description
      ?status
      ?template_body
      ?(parameters = [])
      ?(capabilities = [])
      ?(tags = [])
      ?stack_set_a_r_n
      ?administration_role_a_r_n
      ?execution_role_name
      ?stack_set_drift_detection_details
      ?auto_deployment
      ?permission_model
      ?(organizational_unit_ids = [])
      () =
    { stack_set_name
    ; stack_set_id
    ; description
    ; status
    ; template_body
    ; parameters
    ; capabilities
    ; tags
    ; stack_set_a_r_n
    ; administration_role_a_r_n
    ; execution_role_name
    ; stack_set_drift_detection_details
    ; auto_deployment
    ; permission_model
    ; organizational_unit_ids
    }

  let parse xml =
    Some
      { stack_set_name =
          Aws.Util.option_bind (Aws.Xml.member "StackSetName" xml) String.parse
      ; stack_set_id = Aws.Util.option_bind (Aws.Xml.member "StackSetId" xml) String.parse
      ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
      ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) StackSetStatus.parse
      ; template_body =
          Aws.Util.option_bind (Aws.Xml.member "TemplateBody" xml) String.parse
      ; parameters =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Parameters" xml) Parameters.parse)
      ; capabilities =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Capabilities" xml) Capabilities.parse)
      ; tags =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) Tags.parse)
      ; stack_set_a_r_n =
          Aws.Util.option_bind (Aws.Xml.member "StackSetARN" xml) String.parse
      ; administration_role_a_r_n =
          Aws.Util.option_bind (Aws.Xml.member "AdministrationRoleARN" xml) String.parse
      ; execution_role_name =
          Aws.Util.option_bind (Aws.Xml.member "ExecutionRoleName" xml) String.parse
      ; stack_set_drift_detection_details =
          Aws.Util.option_bind
            (Aws.Xml.member "StackSetDriftDetectionDetails" xml)
            StackSetDriftDetectionDetails.parse
      ; auto_deployment =
          Aws.Util.option_bind (Aws.Xml.member "AutoDeployment" xml) AutoDeployment.parse
      ; permission_model =
          Aws.Util.option_bind
            (Aws.Xml.member "PermissionModel" xml)
            PermissionModels.parse
      ; organizational_unit_ids =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "OrganizationalUnitIds" xml)
               OrganizationalUnitIdList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "OrganizationalUnitIds.member"
                , OrganizationalUnitIdList.to_query v.organizational_unit_ids ))
         ; Aws.Util.option_map v.permission_model (fun f ->
               Aws.Query.Pair ("PermissionModel", PermissionModels.to_query f))
         ; Aws.Util.option_map v.auto_deployment (fun f ->
               Aws.Query.Pair ("AutoDeployment", AutoDeployment.to_query f))
         ; Aws.Util.option_map v.stack_set_drift_detection_details (fun f ->
               Aws.Query.Pair
                 ( "StackSetDriftDetectionDetails"
                 , StackSetDriftDetectionDetails.to_query f ))
         ; Aws.Util.option_map v.execution_role_name (fun f ->
               Aws.Query.Pair ("ExecutionRoleName", String.to_query f))
         ; Aws.Util.option_map v.administration_role_a_r_n (fun f ->
               Aws.Query.Pair ("AdministrationRoleARN", String.to_query f))
         ; Aws.Util.option_map v.stack_set_a_r_n (fun f ->
               Aws.Query.Pair ("StackSetARN", String.to_query f))
         ; Some (Aws.Query.Pair ("Tags.member", Tags.to_query v.tags))
         ; Some
             (Aws.Query.Pair ("Capabilities.member", Capabilities.to_query v.capabilities))
         ; Some (Aws.Query.Pair ("Parameters.member", Parameters.to_query v.parameters))
         ; Aws.Util.option_map v.template_body (fun f ->
               Aws.Query.Pair ("TemplateBody", String.to_query f))
         ; Aws.Util.option_map v.status (fun f ->
               Aws.Query.Pair ("Status", StackSetStatus.to_query f))
         ; Aws.Util.option_map v.description (fun f ->
               Aws.Query.Pair ("Description", String.to_query f))
         ; Aws.Util.option_map v.stack_set_id (fun f ->
               Aws.Query.Pair ("StackSetId", String.to_query f))
         ; Aws.Util.option_map v.stack_set_name (fun f ->
               Aws.Query.Pair ("StackSetName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "OrganizationalUnitIds"
             , OrganizationalUnitIdList.to_json v.organizational_unit_ids )
         ; Aws.Util.option_map v.permission_model (fun f ->
               "PermissionModel", PermissionModels.to_json f)
         ; Aws.Util.option_map v.auto_deployment (fun f ->
               "AutoDeployment", AutoDeployment.to_json f)
         ; Aws.Util.option_map v.stack_set_drift_detection_details (fun f ->
               "StackSetDriftDetectionDetails", StackSetDriftDetectionDetails.to_json f)
         ; Aws.Util.option_map v.execution_role_name (fun f ->
               "ExecutionRoleName", String.to_json f)
         ; Aws.Util.option_map v.administration_role_a_r_n (fun f ->
               "AdministrationRoleARN", String.to_json f)
         ; Aws.Util.option_map v.stack_set_a_r_n (fun f ->
               "StackSetARN", String.to_json f)
         ; Some ("Tags", Tags.to_json v.tags)
         ; Some ("Capabilities", Capabilities.to_json v.capabilities)
         ; Some ("Parameters", Parameters.to_json v.parameters)
         ; Aws.Util.option_map v.template_body (fun f -> "TemplateBody", String.to_json f)
         ; Aws.Util.option_map v.status (fun f -> "Status", StackSetStatus.to_json f)
         ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
         ; Aws.Util.option_map v.stack_set_id (fun f -> "StackSetId", String.to_json f)
         ; Aws.Util.option_map v.stack_set_name (fun f ->
               "StackSetName", String.to_json f)
         ])

  let of_json j =
    { stack_set_name =
        Aws.Util.option_map (Aws.Json.lookup j "StackSetName") String.of_json
    ; stack_set_id = Aws.Util.option_map (Aws.Json.lookup j "StackSetId") String.of_json
    ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
    ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") StackSetStatus.of_json
    ; template_body =
        Aws.Util.option_map (Aws.Json.lookup j "TemplateBody") String.of_json
    ; parameters =
        Parameters.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Parameters"))
    ; capabilities =
        Capabilities.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Capabilities"))
    ; tags = Tags.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
    ; stack_set_a_r_n =
        Aws.Util.option_map (Aws.Json.lookup j "StackSetARN") String.of_json
    ; administration_role_a_r_n =
        Aws.Util.option_map (Aws.Json.lookup j "AdministrationRoleARN") String.of_json
    ; execution_role_name =
        Aws.Util.option_map (Aws.Json.lookup j "ExecutionRoleName") String.of_json
    ; stack_set_drift_detection_details =
        Aws.Util.option_map
          (Aws.Json.lookup j "StackSetDriftDetectionDetails")
          StackSetDriftDetectionDetails.of_json
    ; auto_deployment =
        Aws.Util.option_map (Aws.Json.lookup j "AutoDeployment") AutoDeployment.of_json
    ; permission_model =
        Aws.Util.option_map (Aws.Json.lookup j "PermissionModel") PermissionModels.of_json
    ; organizational_unit_ids =
        OrganizationalUnitIdList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "OrganizationalUnitIds"))
    }
end

module StackResourceDetail = struct
  type t =
    { stack_name : String.t option
    ; stack_id : String.t option
    ; logical_resource_id : String.t
    ; physical_resource_id : String.t option
    ; resource_type : String.t
    ; last_updated_timestamp : DateTime.t
    ; resource_status : ResourceStatus.t
    ; resource_status_reason : String.t option
    ; description : String.t option
    ; metadata : String.t option
    ; drift_information : StackResourceDriftInformation.t option
    }

  let make
      ?stack_name
      ?stack_id
      ~logical_resource_id
      ?physical_resource_id
      ~resource_type
      ~last_updated_timestamp
      ~resource_status
      ?resource_status_reason
      ?description
      ?metadata
      ?drift_information
      () =
    { stack_name
    ; stack_id
    ; logical_resource_id
    ; physical_resource_id
    ; resource_type
    ; last_updated_timestamp
    ; resource_status
    ; resource_status_reason
    ; description
    ; metadata
    ; drift_information
    }

  let parse xml =
    Some
      { stack_name = Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse
      ; stack_id = Aws.Util.option_bind (Aws.Xml.member "StackId" xml) String.parse
      ; logical_resource_id =
          Aws.Xml.required
            "LogicalResourceId"
            (Aws.Util.option_bind (Aws.Xml.member "LogicalResourceId" xml) String.parse)
      ; physical_resource_id =
          Aws.Util.option_bind (Aws.Xml.member "PhysicalResourceId" xml) String.parse
      ; resource_type =
          Aws.Xml.required
            "ResourceType"
            (Aws.Util.option_bind (Aws.Xml.member "ResourceType" xml) String.parse)
      ; last_updated_timestamp =
          Aws.Xml.required
            "LastUpdatedTimestamp"
            (Aws.Util.option_bind
               (Aws.Xml.member "LastUpdatedTimestamp" xml)
               DateTime.parse)
      ; resource_status =
          Aws.Xml.required
            "ResourceStatus"
            (Aws.Util.option_bind
               (Aws.Xml.member "ResourceStatus" xml)
               ResourceStatus.parse)
      ; resource_status_reason =
          Aws.Util.option_bind (Aws.Xml.member "ResourceStatusReason" xml) String.parse
      ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
      ; metadata = Aws.Util.option_bind (Aws.Xml.member "Metadata" xml) String.parse
      ; drift_information =
          Aws.Util.option_bind
            (Aws.Xml.member "DriftInformation" xml)
            StackResourceDriftInformation.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.drift_information (fun f ->
               Aws.Query.Pair
                 ("DriftInformation", StackResourceDriftInformation.to_query f))
         ; Aws.Util.option_map v.metadata (fun f ->
               Aws.Query.Pair ("Metadata", String.to_query f))
         ; Aws.Util.option_map v.description (fun f ->
               Aws.Query.Pair ("Description", String.to_query f))
         ; Aws.Util.option_map v.resource_status_reason (fun f ->
               Aws.Query.Pair ("ResourceStatusReason", String.to_query f))
         ; Some
             (Aws.Query.Pair ("ResourceStatus", ResourceStatus.to_query v.resource_status))
         ; Some
             (Aws.Query.Pair
                ("LastUpdatedTimestamp", DateTime.to_query v.last_updated_timestamp))
         ; Some (Aws.Query.Pair ("ResourceType", String.to_query v.resource_type))
         ; Aws.Util.option_map v.physical_resource_id (fun f ->
               Aws.Query.Pair ("PhysicalResourceId", String.to_query f))
         ; Some
             (Aws.Query.Pair ("LogicalResourceId", String.to_query v.logical_resource_id))
         ; Aws.Util.option_map v.stack_id (fun f ->
               Aws.Query.Pair ("StackId", String.to_query f))
         ; Aws.Util.option_map v.stack_name (fun f ->
               Aws.Query.Pair ("StackName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.drift_information (fun f ->
               "DriftInformation", StackResourceDriftInformation.to_json f)
         ; Aws.Util.option_map v.metadata (fun f -> "Metadata", String.to_json f)
         ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
         ; Aws.Util.option_map v.resource_status_reason (fun f ->
               "ResourceStatusReason", String.to_json f)
         ; Some ("ResourceStatus", ResourceStatus.to_json v.resource_status)
         ; Some ("LastUpdatedTimestamp", DateTime.to_json v.last_updated_timestamp)
         ; Some ("ResourceType", String.to_json v.resource_type)
         ; Aws.Util.option_map v.physical_resource_id (fun f ->
               "PhysicalResourceId", String.to_json f)
         ; Some ("LogicalResourceId", String.to_json v.logical_resource_id)
         ; Aws.Util.option_map v.stack_id (fun f -> "StackId", String.to_json f)
         ; Aws.Util.option_map v.stack_name (fun f -> "StackName", String.to_json f)
         ])

  let of_json j =
    { stack_name = Aws.Util.option_map (Aws.Json.lookup j "StackName") String.of_json
    ; stack_id = Aws.Util.option_map (Aws.Json.lookup j "StackId") String.of_json
    ; logical_resource_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LogicalResourceId"))
    ; physical_resource_id =
        Aws.Util.option_map (Aws.Json.lookup j "PhysicalResourceId") String.of_json
    ; resource_type =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceType"))
    ; last_updated_timestamp =
        DateTime.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "LastUpdatedTimestamp"))
    ; resource_status =
        ResourceStatus.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceStatus"))
    ; resource_status_reason =
        Aws.Util.option_map (Aws.Json.lookup j "ResourceStatusReason") String.of_json
    ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
    ; metadata = Aws.Util.option_map (Aws.Json.lookup j "Metadata") String.of_json
    ; drift_information =
        Aws.Util.option_map
          (Aws.Json.lookup j "DriftInformation")
          StackResourceDriftInformation.of_json
    }
end

module DescribeStackEventsInput = struct
  type t =
    { stack_name : String.t option
    ; next_token : String.t option
    }

  let make ?stack_name ?next_token () = { stack_name; next_token }

  let parse xml =
    Some
      { stack_name = Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Aws.Util.option_map v.stack_name (fun f ->
               Aws.Query.Pair ("StackName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Aws.Util.option_map v.stack_name (fun f -> "StackName", String.to_json f)
         ])

  let of_json j =
    { stack_name = Aws.Util.option_map (Aws.Json.lookup j "StackName") String.of_json
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module ExecuteChangeSetOutput = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module StackInstanceFilters = struct
  type t = StackInstanceFilter.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map StackInstanceFilter.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list StackInstanceFilter.to_query v

  let to_json v = `List (List.map StackInstanceFilter.to_json v)

  let of_json j = Aws.Json.to_list StackInstanceFilter.of_json j
end

module DeregisterTypeInput = struct
  type t =
    { arn : String.t option
    ; type_ : RegistryType.t option
    ; type_name : String.t option
    ; version_id : String.t option
    }

  let make ?arn ?type_ ?type_name ?version_id () = { arn; type_; type_name; version_id }

  let parse xml =
    Some
      { arn = Aws.Util.option_bind (Aws.Xml.member "Arn" xml) String.parse
      ; type_ = Aws.Util.option_bind (Aws.Xml.member "Type" xml) RegistryType.parse
      ; type_name = Aws.Util.option_bind (Aws.Xml.member "TypeName" xml) String.parse
      ; version_id = Aws.Util.option_bind (Aws.Xml.member "VersionId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.version_id (fun f ->
               Aws.Query.Pair ("VersionId", String.to_query f))
         ; Aws.Util.option_map v.type_name (fun f ->
               Aws.Query.Pair ("TypeName", String.to_query f))
         ; Aws.Util.option_map v.type_ (fun f ->
               Aws.Query.Pair ("Type", RegistryType.to_query f))
         ; Aws.Util.option_map v.arn (fun f -> Aws.Query.Pair ("Arn", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.version_id (fun f -> "VersionId", String.to_json f)
         ; Aws.Util.option_map v.type_name (fun f -> "TypeName", String.to_json f)
         ; Aws.Util.option_map v.type_ (fun f -> "Type", RegistryType.to_json f)
         ; Aws.Util.option_map v.arn (fun f -> "Arn", String.to_json f)
         ])

  let of_json j =
    { arn = Aws.Util.option_map (Aws.Json.lookup j "Arn") String.of_json
    ; type_ = Aws.Util.option_map (Aws.Json.lookup j "Type") RegistryType.of_json
    ; type_name = Aws.Util.option_map (Aws.Json.lookup j "TypeName") String.of_json
    ; version_id = Aws.Util.option_map (Aws.Json.lookup j "VersionId") String.of_json
    }
end

module DeleteStackSetOutput = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ParameterDeclarations = struct
  type t = ParameterDeclaration.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map ParameterDeclaration.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list ParameterDeclaration.to_query v

  let to_json v = `List (List.map ParameterDeclaration.to_json v)

  let of_json j = Aws.Json.to_list ParameterDeclaration.of_json j
end

module StackSetNotFoundException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module TransformsList = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module GetTemplateSummaryOutput = struct
  type t =
    { parameters : ParameterDeclarations.t
    ; description : String.t option
    ; capabilities : Capabilities.t
    ; capabilities_reason : String.t option
    ; resource_types : ResourceTypes.t
    ; version : String.t option
    ; metadata : String.t option
    ; declared_transforms : TransformsList.t
    ; resource_identifier_summaries : ResourceIdentifierSummaries.t
    }

  let make
      ?(parameters = [])
      ?description
      ?(capabilities = [])
      ?capabilities_reason
      ?(resource_types = [])
      ?version
      ?metadata
      ?(declared_transforms = [])
      ?(resource_identifier_summaries = [])
      () =
    { parameters
    ; description
    ; capabilities
    ; capabilities_reason
    ; resource_types
    ; version
    ; metadata
    ; declared_transforms
    ; resource_identifier_summaries
    }

  let parse xml =
    Some
      { parameters =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "Parameters" xml)
               ParameterDeclarations.parse)
      ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
      ; capabilities =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Capabilities" xml) Capabilities.parse)
      ; capabilities_reason =
          Aws.Util.option_bind (Aws.Xml.member "CapabilitiesReason" xml) String.parse
      ; resource_types =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ResourceTypes" xml)
               ResourceTypes.parse)
      ; version = Aws.Util.option_bind (Aws.Xml.member "Version" xml) String.parse
      ; metadata = Aws.Util.option_bind (Aws.Xml.member "Metadata" xml) String.parse
      ; declared_transforms =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "DeclaredTransforms" xml)
               TransformsList.parse)
      ; resource_identifier_summaries =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ResourceIdentifierSummaries" xml)
               ResourceIdentifierSummaries.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "ResourceIdentifierSummaries.member"
                , ResourceIdentifierSummaries.to_query v.resource_identifier_summaries ))
         ; Some
             (Aws.Query.Pair
                ( "DeclaredTransforms.member"
                , TransformsList.to_query v.declared_transforms ))
         ; Aws.Util.option_map v.metadata (fun f ->
               Aws.Query.Pair ("Metadata", String.to_query f))
         ; Aws.Util.option_map v.version (fun f ->
               Aws.Query.Pair ("Version", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("ResourceTypes.member", ResourceTypes.to_query v.resource_types))
         ; Aws.Util.option_map v.capabilities_reason (fun f ->
               Aws.Query.Pair ("CapabilitiesReason", String.to_query f))
         ; Some
             (Aws.Query.Pair ("Capabilities.member", Capabilities.to_query v.capabilities))
         ; Aws.Util.option_map v.description (fun f ->
               Aws.Query.Pair ("Description", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("Parameters.member", ParameterDeclarations.to_query v.parameters))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "ResourceIdentifierSummaries"
             , ResourceIdentifierSummaries.to_json v.resource_identifier_summaries )
         ; Some ("DeclaredTransforms", TransformsList.to_json v.declared_transforms)
         ; Aws.Util.option_map v.metadata (fun f -> "Metadata", String.to_json f)
         ; Aws.Util.option_map v.version (fun f -> "Version", String.to_json f)
         ; Some ("ResourceTypes", ResourceTypes.to_json v.resource_types)
         ; Aws.Util.option_map v.capabilities_reason (fun f ->
               "CapabilitiesReason", String.to_json f)
         ; Some ("Capabilities", Capabilities.to_json v.capabilities)
         ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
         ; Some ("Parameters", ParameterDeclarations.to_json v.parameters)
         ])

  let of_json j =
    { parameters =
        ParameterDeclarations.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Parameters"))
    ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
    ; capabilities =
        Capabilities.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Capabilities"))
    ; capabilities_reason =
        Aws.Util.option_map (Aws.Json.lookup j "CapabilitiesReason") String.of_json
    ; resource_types =
        ResourceTypes.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceTypes"))
    ; version = Aws.Util.option_map (Aws.Json.lookup j "Version") String.of_json
    ; metadata = Aws.Util.option_map (Aws.Json.lookup j "Metadata") String.of_json
    ; declared_transforms =
        TransformsList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "DeclaredTransforms"))
    ; resource_identifier_summaries =
        ResourceIdentifierSummaries.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceIdentifierSummaries"))
    }
end

module UpdateStackInstancesOutput = struct
  type t = { operation_id : String.t option }

  let make ?operation_id () = { operation_id }

  let parse xml =
    Some
      { operation_id =
          Aws.Util.option_bind (Aws.Xml.member "OperationId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.operation_id (fun f ->
               Aws.Query.Pair ("OperationId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.operation_id (fun f -> "OperationId", String.to_json f) ])

  let of_json j =
    { operation_id = Aws.Util.option_map (Aws.Json.lookup j "OperationId") String.of_json
    }
end

module SignalResourceInput = struct
  type t =
    { stack_name : String.t
    ; logical_resource_id : String.t
    ; unique_id : String.t
    ; status : ResourceSignalStatus.t
    }

  let make ~stack_name ~logical_resource_id ~unique_id ~status () =
    { stack_name; logical_resource_id; unique_id; status }

  let parse xml =
    Some
      { stack_name =
          Aws.Xml.required
            "StackName"
            (Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse)
      ; logical_resource_id =
          Aws.Xml.required
            "LogicalResourceId"
            (Aws.Util.option_bind (Aws.Xml.member "LogicalResourceId" xml) String.parse)
      ; unique_id =
          Aws.Xml.required
            "UniqueId"
            (Aws.Util.option_bind (Aws.Xml.member "UniqueId" xml) String.parse)
      ; status =
          Aws.Xml.required
            "Status"
            (Aws.Util.option_bind
               (Aws.Xml.member "Status" xml)
               ResourceSignalStatus.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Status", ResourceSignalStatus.to_query v.status))
         ; Some (Aws.Query.Pair ("UniqueId", String.to_query v.unique_id))
         ; Some
             (Aws.Query.Pair ("LogicalResourceId", String.to_query v.logical_resource_id))
         ; Some (Aws.Query.Pair ("StackName", String.to_query v.stack_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Status", ResourceSignalStatus.to_json v.status)
         ; Some ("UniqueId", String.to_json v.unique_id)
         ; Some ("LogicalResourceId", String.to_json v.logical_resource_id)
         ; Some ("StackName", String.to_json v.stack_name)
         ])

  let of_json j =
    { stack_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackName"))
    ; logical_resource_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LogicalResourceId"))
    ; unique_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UniqueId"))
    ; status =
        ResourceSignalStatus.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Status"))
    }
end

module DetectStackDriftOutput = struct
  type t = { stack_drift_detection_id : String.t }

  let make ~stack_drift_detection_id () = { stack_drift_detection_id }

  let parse xml =
    Some
      { stack_drift_detection_id =
          Aws.Xml.required
            "StackDriftDetectionId"
            (Aws.Util.option_bind
               (Aws.Xml.member "StackDriftDetectionId" xml)
               String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ("StackDriftDetectionId", String.to_query v.stack_drift_detection_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("StackDriftDetectionId", String.to_json v.stack_drift_detection_id) ])

  let of_json j =
    { stack_drift_detection_id =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "StackDriftDetectionId"))
    }
end

module DetectStackResourceDriftInput = struct
  type t =
    { stack_name : String.t
    ; logical_resource_id : String.t
    }

  let make ~stack_name ~logical_resource_id () = { stack_name; logical_resource_id }

  let parse xml =
    Some
      { stack_name =
          Aws.Xml.required
            "StackName"
            (Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse)
      ; logical_resource_id =
          Aws.Xml.required
            "LogicalResourceId"
            (Aws.Util.option_bind (Aws.Xml.member "LogicalResourceId" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("LogicalResourceId", String.to_query v.logical_resource_id))
         ; Some (Aws.Query.Pair ("StackName", String.to_query v.stack_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("LogicalResourceId", String.to_json v.logical_resource_id)
         ; Some ("StackName", String.to_json v.stack_name)
         ])

  let of_json j =
    { stack_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackName"))
    ; logical_resource_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LogicalResourceId"))
    }
end

module StackDriftDetectionStatus = struct
  type t =
    | DETECTION_IN_PROGRESS
    | DETECTION_FAILED
    | DETECTION_COMPLETE

  let str_to_t =
    [ "DETECTION_COMPLETE", DETECTION_COMPLETE
    ; "DETECTION_FAILED", DETECTION_FAILED
    ; "DETECTION_IN_PROGRESS", DETECTION_IN_PROGRESS
    ]

  let t_to_str =
    [ DETECTION_COMPLETE, "DETECTION_COMPLETE"
    ; DETECTION_FAILED, "DETECTION_FAILED"
    ; DETECTION_IN_PROGRESS, "DETECTION_IN_PROGRESS"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module DeleteChangeSetInput = struct
  type t =
    { change_set_name : String.t
    ; stack_name : String.t option
    }

  let make ~change_set_name ?stack_name () = { change_set_name; stack_name }

  let parse xml =
    Some
      { change_set_name =
          Aws.Xml.required
            "ChangeSetName"
            (Aws.Util.option_bind (Aws.Xml.member "ChangeSetName" xml) String.parse)
      ; stack_name = Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.stack_name (fun f ->
               Aws.Query.Pair ("StackName", String.to_query f))
         ; Some (Aws.Query.Pair ("ChangeSetName", String.to_query v.change_set_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.stack_name (fun f -> "StackName", String.to_json f)
         ; Some ("ChangeSetName", String.to_json v.change_set_name)
         ])

  let of_json j =
    { change_set_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ChangeSetName"))
    ; stack_name = Aws.Util.option_map (Aws.Json.lookup j "StackName") String.of_json
    }
end

module DescribeStackResourcesInput = struct
  type t =
    { stack_name : String.t option
    ; logical_resource_id : String.t option
    ; physical_resource_id : String.t option
    }

  let make ?stack_name ?logical_resource_id ?physical_resource_id () =
    { stack_name; logical_resource_id; physical_resource_id }

  let parse xml =
    Some
      { stack_name = Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse
      ; logical_resource_id =
          Aws.Util.option_bind (Aws.Xml.member "LogicalResourceId" xml) String.parse
      ; physical_resource_id =
          Aws.Util.option_bind (Aws.Xml.member "PhysicalResourceId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.physical_resource_id (fun f ->
               Aws.Query.Pair ("PhysicalResourceId", String.to_query f))
         ; Aws.Util.option_map v.logical_resource_id (fun f ->
               Aws.Query.Pair ("LogicalResourceId", String.to_query f))
         ; Aws.Util.option_map v.stack_name (fun f ->
               Aws.Query.Pair ("StackName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.physical_resource_id (fun f ->
               "PhysicalResourceId", String.to_json f)
         ; Aws.Util.option_map v.logical_resource_id (fun f ->
               "LogicalResourceId", String.to_json f)
         ; Aws.Util.option_map v.stack_name (fun f -> "StackName", String.to_json f)
         ])

  let of_json j =
    { stack_name = Aws.Util.option_map (Aws.Json.lookup j "StackName") String.of_json
    ; logical_resource_id =
        Aws.Util.option_map (Aws.Json.lookup j "LogicalResourceId") String.of_json
    ; physical_resource_id =
        Aws.Util.option_map (Aws.Json.lookup j "PhysicalResourceId") String.of_json
    }
end

module DescribeStackSetInput = struct
  type t = { stack_set_name : String.t }

  let make ~stack_set_name () = { stack_set_name }

  let parse xml =
    Some
      { stack_set_name =
          Aws.Xml.required
            "StackSetName"
            (Aws.Util.option_bind (Aws.Xml.member "StackSetName" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("StackSetName", String.to_query v.stack_set_name)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("StackSetName", String.to_json v.stack_set_name) ])

  let of_json j =
    { stack_set_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackSetName"))
    }
end

module RegisterTypeInput = struct
  type t =
    { type_ : RegistryType.t option
    ; type_name : String.t
    ; schema_handler_package : String.t
    ; logging_config : LoggingConfig.t option
    ; execution_role_arn : String.t option
    ; client_request_token : String.t option
    }

  let make
      ?type_
      ~type_name
      ~schema_handler_package
      ?logging_config
      ?execution_role_arn
      ?client_request_token
      () =
    { type_
    ; type_name
    ; schema_handler_package
    ; logging_config
    ; execution_role_arn
    ; client_request_token
    }

  let parse xml =
    Some
      { type_ = Aws.Util.option_bind (Aws.Xml.member "Type" xml) RegistryType.parse
      ; type_name =
          Aws.Xml.required
            "TypeName"
            (Aws.Util.option_bind (Aws.Xml.member "TypeName" xml) String.parse)
      ; schema_handler_package =
          Aws.Xml.required
            "SchemaHandlerPackage"
            (Aws.Util.option_bind
               (Aws.Xml.member "SchemaHandlerPackage" xml)
               String.parse)
      ; logging_config =
          Aws.Util.option_bind (Aws.Xml.member "LoggingConfig" xml) LoggingConfig.parse
      ; execution_role_arn =
          Aws.Util.option_bind (Aws.Xml.member "ExecutionRoleArn" xml) String.parse
      ; client_request_token =
          Aws.Util.option_bind (Aws.Xml.member "ClientRequestToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.client_request_token (fun f ->
               Aws.Query.Pair ("ClientRequestToken", String.to_query f))
         ; Aws.Util.option_map v.execution_role_arn (fun f ->
               Aws.Query.Pair ("ExecutionRoleArn", String.to_query f))
         ; Aws.Util.option_map v.logging_config (fun f ->
               Aws.Query.Pair ("LoggingConfig", LoggingConfig.to_query f))
         ; Some
             (Aws.Query.Pair
                ("SchemaHandlerPackage", String.to_query v.schema_handler_package))
         ; Some (Aws.Query.Pair ("TypeName", String.to_query v.type_name))
         ; Aws.Util.option_map v.type_ (fun f ->
               Aws.Query.Pair ("Type", RegistryType.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.client_request_token (fun f ->
               "ClientRequestToken", String.to_json f)
         ; Aws.Util.option_map v.execution_role_arn (fun f ->
               "ExecutionRoleArn", String.to_json f)
         ; Aws.Util.option_map v.logging_config (fun f ->
               "LoggingConfig", LoggingConfig.to_json f)
         ; Some ("SchemaHandlerPackage", String.to_json v.schema_handler_package)
         ; Some ("TypeName", String.to_json v.type_name)
         ; Aws.Util.option_map v.type_ (fun f -> "Type", RegistryType.to_json f)
         ])

  let of_json j =
    { type_ = Aws.Util.option_map (Aws.Json.lookup j "Type") RegistryType.of_json
    ; type_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TypeName"))
    ; schema_handler_package =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SchemaHandlerPackage"))
    ; logging_config =
        Aws.Util.option_map (Aws.Json.lookup j "LoggingConfig") LoggingConfig.of_json
    ; execution_role_arn =
        Aws.Util.option_map (Aws.Json.lookup j "ExecutionRoleArn") String.of_json
    ; client_request_token =
        Aws.Util.option_map (Aws.Json.lookup j "ClientRequestToken") String.of_json
    }
end

module ListStackInstancesInput = struct
  type t =
    { stack_set_name : String.t
    ; next_token : String.t option
    ; max_results : Integer.t option
    ; filters : StackInstanceFilters.t
    ; stack_instance_account : String.t option
    ; stack_instance_region : String.t option
    }

  let make
      ~stack_set_name
      ?next_token
      ?max_results
      ?(filters = [])
      ?stack_instance_account
      ?stack_instance_region
      () =
    { stack_set_name
    ; next_token
    ; max_results
    ; filters
    ; stack_instance_account
    ; stack_instance_region
    }

  let parse xml =
    Some
      { stack_set_name =
          Aws.Xml.required
            "StackSetName"
            (Aws.Util.option_bind (Aws.Xml.member "StackSetName" xml) String.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
      ; filters =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "Filters" xml)
               StackInstanceFilters.parse)
      ; stack_instance_account =
          Aws.Util.option_bind (Aws.Xml.member "StackInstanceAccount" xml) String.parse
      ; stack_instance_region =
          Aws.Util.option_bind (Aws.Xml.member "StackInstanceRegion" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.stack_instance_region (fun f ->
               Aws.Query.Pair ("StackInstanceRegion", String.to_query f))
         ; Aws.Util.option_map v.stack_instance_account (fun f ->
               Aws.Query.Pair ("StackInstanceAccount", String.to_query f))
         ; Some
             (Aws.Query.Pair ("Filters.member", StackInstanceFilters.to_query v.filters))
         ; Aws.Util.option_map v.max_results (fun f ->
               Aws.Query.Pair ("MaxResults", Integer.to_query f))
         ; Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some (Aws.Query.Pair ("StackSetName", String.to_query v.stack_set_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.stack_instance_region (fun f ->
               "StackInstanceRegion", String.to_json f)
         ; Aws.Util.option_map v.stack_instance_account (fun f ->
               "StackInstanceAccount", String.to_json f)
         ; Some ("Filters", StackInstanceFilters.to_json v.filters)
         ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
         ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("StackSetName", String.to_json v.stack_set_name)
         ])

  let of_json j =
    { stack_set_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackSetName"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
    ; filters =
        StackInstanceFilters.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Filters"))
    ; stack_instance_account =
        Aws.Util.option_map (Aws.Json.lookup j "StackInstanceAccount") String.of_json
    ; stack_instance_region =
        Aws.Util.option_map (Aws.Json.lookup j "StackInstanceRegion") String.of_json
    }
end

module DeregisterTypeOutput = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module OperationStatusCheckFailedException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module DescribeChangeSetInput = struct
  type t =
    { change_set_name : String.t
    ; stack_name : String.t option
    ; next_token : String.t option
    }

  let make ~change_set_name ?stack_name ?next_token () =
    { change_set_name; stack_name; next_token }

  let parse xml =
    Some
      { change_set_name =
          Aws.Xml.required
            "ChangeSetName"
            (Aws.Util.option_bind (Aws.Xml.member "ChangeSetName" xml) String.parse)
      ; stack_name = Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Aws.Util.option_map v.stack_name (fun f ->
               Aws.Query.Pair ("StackName", String.to_query f))
         ; Some (Aws.Query.Pair ("ChangeSetName", String.to_query v.change_set_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Aws.Util.option_map v.stack_name (fun f -> "StackName", String.to_json f)
         ; Some ("ChangeSetName", String.to_json v.change_set_name)
         ])

  let of_json j =
    { change_set_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ChangeSetName"))
    ; stack_name = Aws.Util.option_map (Aws.Json.lookup j "StackName") String.of_json
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module Visibility = struct
  type t =
    | PUBLIC
    | PRIVATE

  let str_to_t = [ "PRIVATE", PRIVATE; "PUBLIC", PUBLIC ]

  let t_to_str = [ PRIVATE, "PRIVATE"; PUBLIC, "PUBLIC" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module ProvisioningType = struct
  type t =
    | NON_PROVISIONABLE
    | IMMUTABLE
    | FULLY_MUTABLE

  let str_to_t =
    [ "FULLY_MUTABLE", FULLY_MUTABLE
    ; "IMMUTABLE", IMMUTABLE
    ; "NON_PROVISIONABLE", NON_PROVISIONABLE
    ]

  let t_to_str =
    [ FULLY_MUTABLE, "FULLY_MUTABLE"
    ; IMMUTABLE, "IMMUTABLE"
    ; NON_PROVISIONABLE, "NON_PROVISIONABLE"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module DeprecatedStatus = struct
  type t =
    | LIVE
    | DEPRECATED

  let str_to_t = [ "DEPRECATED", DEPRECATED; "LIVE", LIVE ]

  let t_to_str = [ DEPRECATED, "DEPRECATED"; LIVE, "LIVE" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module DescribeTypeOutput = struct
  type t =
    { arn : String.t option
    ; type_ : RegistryType.t option
    ; type_name : String.t option
    ; default_version_id : String.t option
    ; is_default_version : Boolean.t option
    ; description : String.t option
    ; schema : String.t option
    ; provisioning_type : ProvisioningType.t option
    ; deprecated_status : DeprecatedStatus.t option
    ; logging_config : LoggingConfig.t option
    ; execution_role_arn : String.t option
    ; visibility : Visibility.t option
    ; source_url : String.t option
    ; documentation_url : String.t option
    ; last_updated : DateTime.t option
    ; time_created : DateTime.t option
    }

  let make
      ?arn
      ?type_
      ?type_name
      ?default_version_id
      ?is_default_version
      ?description
      ?schema
      ?provisioning_type
      ?deprecated_status
      ?logging_config
      ?execution_role_arn
      ?visibility
      ?source_url
      ?documentation_url
      ?last_updated
      ?time_created
      () =
    { arn
    ; type_
    ; type_name
    ; default_version_id
    ; is_default_version
    ; description
    ; schema
    ; provisioning_type
    ; deprecated_status
    ; logging_config
    ; execution_role_arn
    ; visibility
    ; source_url
    ; documentation_url
    ; last_updated
    ; time_created
    }

  let parse xml =
    Some
      { arn = Aws.Util.option_bind (Aws.Xml.member "Arn" xml) String.parse
      ; type_ = Aws.Util.option_bind (Aws.Xml.member "Type" xml) RegistryType.parse
      ; type_name = Aws.Util.option_bind (Aws.Xml.member "TypeName" xml) String.parse
      ; default_version_id =
          Aws.Util.option_bind (Aws.Xml.member "DefaultVersionId" xml) String.parse
      ; is_default_version =
          Aws.Util.option_bind (Aws.Xml.member "IsDefaultVersion" xml) Boolean.parse
      ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
      ; schema = Aws.Util.option_bind (Aws.Xml.member "Schema" xml) String.parse
      ; provisioning_type =
          Aws.Util.option_bind
            (Aws.Xml.member "ProvisioningType" xml)
            ProvisioningType.parse
      ; deprecated_status =
          Aws.Util.option_bind
            (Aws.Xml.member "DeprecatedStatus" xml)
            DeprecatedStatus.parse
      ; logging_config =
          Aws.Util.option_bind (Aws.Xml.member "LoggingConfig" xml) LoggingConfig.parse
      ; execution_role_arn =
          Aws.Util.option_bind (Aws.Xml.member "ExecutionRoleArn" xml) String.parse
      ; visibility =
          Aws.Util.option_bind (Aws.Xml.member "Visibility" xml) Visibility.parse
      ; source_url = Aws.Util.option_bind (Aws.Xml.member "SourceUrl" xml) String.parse
      ; documentation_url =
          Aws.Util.option_bind (Aws.Xml.member "DocumentationUrl" xml) String.parse
      ; last_updated =
          Aws.Util.option_bind (Aws.Xml.member "LastUpdated" xml) DateTime.parse
      ; time_created =
          Aws.Util.option_bind (Aws.Xml.member "TimeCreated" xml) DateTime.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.time_created (fun f ->
               Aws.Query.Pair ("TimeCreated", DateTime.to_query f))
         ; Aws.Util.option_map v.last_updated (fun f ->
               Aws.Query.Pair ("LastUpdated", DateTime.to_query f))
         ; Aws.Util.option_map v.documentation_url (fun f ->
               Aws.Query.Pair ("DocumentationUrl", String.to_query f))
         ; Aws.Util.option_map v.source_url (fun f ->
               Aws.Query.Pair ("SourceUrl", String.to_query f))
         ; Aws.Util.option_map v.visibility (fun f ->
               Aws.Query.Pair ("Visibility", Visibility.to_query f))
         ; Aws.Util.option_map v.execution_role_arn (fun f ->
               Aws.Query.Pair ("ExecutionRoleArn", String.to_query f))
         ; Aws.Util.option_map v.logging_config (fun f ->
               Aws.Query.Pair ("LoggingConfig", LoggingConfig.to_query f))
         ; Aws.Util.option_map v.deprecated_status (fun f ->
               Aws.Query.Pair ("DeprecatedStatus", DeprecatedStatus.to_query f))
         ; Aws.Util.option_map v.provisioning_type (fun f ->
               Aws.Query.Pair ("ProvisioningType", ProvisioningType.to_query f))
         ; Aws.Util.option_map v.schema (fun f ->
               Aws.Query.Pair ("Schema", String.to_query f))
         ; Aws.Util.option_map v.description (fun f ->
               Aws.Query.Pair ("Description", String.to_query f))
         ; Aws.Util.option_map v.is_default_version (fun f ->
               Aws.Query.Pair ("IsDefaultVersion", Boolean.to_query f))
         ; Aws.Util.option_map v.default_version_id (fun f ->
               Aws.Query.Pair ("DefaultVersionId", String.to_query f))
         ; Aws.Util.option_map v.type_name (fun f ->
               Aws.Query.Pair ("TypeName", String.to_query f))
         ; Aws.Util.option_map v.type_ (fun f ->
               Aws.Query.Pair ("Type", RegistryType.to_query f))
         ; Aws.Util.option_map v.arn (fun f -> Aws.Query.Pair ("Arn", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.time_created (fun f -> "TimeCreated", DateTime.to_json f)
         ; Aws.Util.option_map v.last_updated (fun f -> "LastUpdated", DateTime.to_json f)
         ; Aws.Util.option_map v.documentation_url (fun f ->
               "DocumentationUrl", String.to_json f)
         ; Aws.Util.option_map v.source_url (fun f -> "SourceUrl", String.to_json f)
         ; Aws.Util.option_map v.visibility (fun f -> "Visibility", Visibility.to_json f)
         ; Aws.Util.option_map v.execution_role_arn (fun f ->
               "ExecutionRoleArn", String.to_json f)
         ; Aws.Util.option_map v.logging_config (fun f ->
               "LoggingConfig", LoggingConfig.to_json f)
         ; Aws.Util.option_map v.deprecated_status (fun f ->
               "DeprecatedStatus", DeprecatedStatus.to_json f)
         ; Aws.Util.option_map v.provisioning_type (fun f ->
               "ProvisioningType", ProvisioningType.to_json f)
         ; Aws.Util.option_map v.schema (fun f -> "Schema", String.to_json f)
         ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
         ; Aws.Util.option_map v.is_default_version (fun f ->
               "IsDefaultVersion", Boolean.to_json f)
         ; Aws.Util.option_map v.default_version_id (fun f ->
               "DefaultVersionId", String.to_json f)
         ; Aws.Util.option_map v.type_name (fun f -> "TypeName", String.to_json f)
         ; Aws.Util.option_map v.type_ (fun f -> "Type", RegistryType.to_json f)
         ; Aws.Util.option_map v.arn (fun f -> "Arn", String.to_json f)
         ])

  let of_json j =
    { arn = Aws.Util.option_map (Aws.Json.lookup j "Arn") String.of_json
    ; type_ = Aws.Util.option_map (Aws.Json.lookup j "Type") RegistryType.of_json
    ; type_name = Aws.Util.option_map (Aws.Json.lookup j "TypeName") String.of_json
    ; default_version_id =
        Aws.Util.option_map (Aws.Json.lookup j "DefaultVersionId") String.of_json
    ; is_default_version =
        Aws.Util.option_map (Aws.Json.lookup j "IsDefaultVersion") Boolean.of_json
    ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
    ; schema = Aws.Util.option_map (Aws.Json.lookup j "Schema") String.of_json
    ; provisioning_type =
        Aws.Util.option_map
          (Aws.Json.lookup j "ProvisioningType")
          ProvisioningType.of_json
    ; deprecated_status =
        Aws.Util.option_map
          (Aws.Json.lookup j "DeprecatedStatus")
          DeprecatedStatus.of_json
    ; logging_config =
        Aws.Util.option_map (Aws.Json.lookup j "LoggingConfig") LoggingConfig.of_json
    ; execution_role_arn =
        Aws.Util.option_map (Aws.Json.lookup j "ExecutionRoleArn") String.of_json
    ; visibility = Aws.Util.option_map (Aws.Json.lookup j "Visibility") Visibility.of_json
    ; source_url = Aws.Util.option_map (Aws.Json.lookup j "SourceUrl") String.of_json
    ; documentation_url =
        Aws.Util.option_map (Aws.Json.lookup j "DocumentationUrl") String.of_json
    ; last_updated =
        Aws.Util.option_map (Aws.Json.lookup j "LastUpdated") DateTime.of_json
    ; time_created =
        Aws.Util.option_map (Aws.Json.lookup j "TimeCreated") DateTime.of_json
    }
end

module StackResourceDriftInformationSummary = struct
  type t =
    { stack_resource_drift_status : StackResourceDriftStatus.t
    ; last_check_timestamp : DateTime.t option
    }

  let make ~stack_resource_drift_status ?last_check_timestamp () =
    { stack_resource_drift_status; last_check_timestamp }

  let parse xml =
    Some
      { stack_resource_drift_status =
          Aws.Xml.required
            "StackResourceDriftStatus"
            (Aws.Util.option_bind
               (Aws.Xml.member "StackResourceDriftStatus" xml)
               StackResourceDriftStatus.parse)
      ; last_check_timestamp =
          Aws.Util.option_bind (Aws.Xml.member "LastCheckTimestamp" xml) DateTime.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.last_check_timestamp (fun f ->
               Aws.Query.Pair ("LastCheckTimestamp", DateTime.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "StackResourceDriftStatus"
                , StackResourceDriftStatus.to_query v.stack_resource_drift_status ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.last_check_timestamp (fun f ->
               "LastCheckTimestamp", DateTime.to_json f)
         ; Some
             ( "StackResourceDriftStatus"
             , StackResourceDriftStatus.to_json v.stack_resource_drift_status )
         ])

  let of_json j =
    { stack_resource_drift_status =
        StackResourceDriftStatus.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "StackResourceDriftStatus"))
    ; last_check_timestamp =
        Aws.Util.option_map (Aws.Json.lookup j "LastCheckTimestamp") DateTime.of_json
    }
end

module StackResourceSummary = struct
  type t =
    { logical_resource_id : String.t
    ; physical_resource_id : String.t option
    ; resource_type : String.t
    ; last_updated_timestamp : DateTime.t
    ; resource_status : ResourceStatus.t
    ; resource_status_reason : String.t option
    ; drift_information : StackResourceDriftInformationSummary.t option
    }

  let make
      ~logical_resource_id
      ?physical_resource_id
      ~resource_type
      ~last_updated_timestamp
      ~resource_status
      ?resource_status_reason
      ?drift_information
      () =
    { logical_resource_id
    ; physical_resource_id
    ; resource_type
    ; last_updated_timestamp
    ; resource_status
    ; resource_status_reason
    ; drift_information
    }

  let parse xml =
    Some
      { logical_resource_id =
          Aws.Xml.required
            "LogicalResourceId"
            (Aws.Util.option_bind (Aws.Xml.member "LogicalResourceId" xml) String.parse)
      ; physical_resource_id =
          Aws.Util.option_bind (Aws.Xml.member "PhysicalResourceId" xml) String.parse
      ; resource_type =
          Aws.Xml.required
            "ResourceType"
            (Aws.Util.option_bind (Aws.Xml.member "ResourceType" xml) String.parse)
      ; last_updated_timestamp =
          Aws.Xml.required
            "LastUpdatedTimestamp"
            (Aws.Util.option_bind
               (Aws.Xml.member "LastUpdatedTimestamp" xml)
               DateTime.parse)
      ; resource_status =
          Aws.Xml.required
            "ResourceStatus"
            (Aws.Util.option_bind
               (Aws.Xml.member "ResourceStatus" xml)
               ResourceStatus.parse)
      ; resource_status_reason =
          Aws.Util.option_bind (Aws.Xml.member "ResourceStatusReason" xml) String.parse
      ; drift_information =
          Aws.Util.option_bind
            (Aws.Xml.member "DriftInformation" xml)
            StackResourceDriftInformationSummary.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.drift_information (fun f ->
               Aws.Query.Pair
                 ("DriftInformation", StackResourceDriftInformationSummary.to_query f))
         ; Aws.Util.option_map v.resource_status_reason (fun f ->
               Aws.Query.Pair ("ResourceStatusReason", String.to_query f))
         ; Some
             (Aws.Query.Pair ("ResourceStatus", ResourceStatus.to_query v.resource_status))
         ; Some
             (Aws.Query.Pair
                ("LastUpdatedTimestamp", DateTime.to_query v.last_updated_timestamp))
         ; Some (Aws.Query.Pair ("ResourceType", String.to_query v.resource_type))
         ; Aws.Util.option_map v.physical_resource_id (fun f ->
               Aws.Query.Pair ("PhysicalResourceId", String.to_query f))
         ; Some
             (Aws.Query.Pair ("LogicalResourceId", String.to_query v.logical_resource_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.drift_information (fun f ->
               "DriftInformation", StackResourceDriftInformationSummary.to_json f)
         ; Aws.Util.option_map v.resource_status_reason (fun f ->
               "ResourceStatusReason", String.to_json f)
         ; Some ("ResourceStatus", ResourceStatus.to_json v.resource_status)
         ; Some ("LastUpdatedTimestamp", DateTime.to_json v.last_updated_timestamp)
         ; Some ("ResourceType", String.to_json v.resource_type)
         ; Aws.Util.option_map v.physical_resource_id (fun f ->
               "PhysicalResourceId", String.to_json f)
         ; Some ("LogicalResourceId", String.to_json v.logical_resource_id)
         ])

  let of_json j =
    { logical_resource_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LogicalResourceId"))
    ; physical_resource_id =
        Aws.Util.option_map (Aws.Json.lookup j "PhysicalResourceId") String.of_json
    ; resource_type =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceType"))
    ; last_updated_timestamp =
        DateTime.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "LastUpdatedTimestamp"))
    ; resource_status =
        ResourceStatus.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceStatus"))
    ; resource_status_reason =
        Aws.Util.option_map (Aws.Json.lookup j "ResourceStatusReason") String.of_json
    ; drift_information =
        Aws.Util.option_map
          (Aws.Json.lookup j "DriftInformation")
          StackResourceDriftInformationSummary.of_json
    }
end

module StackResourceSummaries = struct
  type t = StackResourceSummary.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map StackResourceSummary.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list StackResourceSummary.to_query v

  let to_json v = `List (List.map StackResourceSummary.to_json v)

  let of_json j = Aws.Json.to_list StackResourceSummary.of_json j
end

module TokenAlreadyExistsException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ListExportsInput = struct
  type t = { next_token : String.t option }

  let make ?next_token () = { next_token }

  let parse xml =
    Some
      { next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f) ])

  let of_json j =
    { next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json }
end

module EstimateTemplateCostOutput = struct
  type t = { url : String.t option }

  let make ?url () = { url }

  let parse xml =
    Some { url = Aws.Util.option_bind (Aws.Xml.member "Url" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.url (fun f -> Aws.Query.Pair ("Url", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.url (fun f -> "Url", String.to_json f) ])

  let of_json j = { url = Aws.Util.option_map (Aws.Json.lookup j "Url") String.of_json }
end

module TemplateParameter = struct
  type t =
    { parameter_key : String.t option
    ; default_value : String.t option
    ; no_echo : Boolean.t option
    ; description : String.t option
    }

  let make ?parameter_key ?default_value ?no_echo ?description () =
    { parameter_key; default_value; no_echo; description }

  let parse xml =
    Some
      { parameter_key =
          Aws.Util.option_bind (Aws.Xml.member "ParameterKey" xml) String.parse
      ; default_value =
          Aws.Util.option_bind (Aws.Xml.member "DefaultValue" xml) String.parse
      ; no_echo = Aws.Util.option_bind (Aws.Xml.member "NoEcho" xml) Boolean.parse
      ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.description (fun f ->
               Aws.Query.Pair ("Description", String.to_query f))
         ; Aws.Util.option_map v.no_echo (fun f ->
               Aws.Query.Pair ("NoEcho", Boolean.to_query f))
         ; Aws.Util.option_map v.default_value (fun f ->
               Aws.Query.Pair ("DefaultValue", String.to_query f))
         ; Aws.Util.option_map v.parameter_key (fun f ->
               Aws.Query.Pair ("ParameterKey", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
         ; Aws.Util.option_map v.no_echo (fun f -> "NoEcho", Boolean.to_json f)
         ; Aws.Util.option_map v.default_value (fun f -> "DefaultValue", String.to_json f)
         ; Aws.Util.option_map v.parameter_key (fun f -> "ParameterKey", String.to_json f)
         ])

  let of_json j =
    { parameter_key =
        Aws.Util.option_map (Aws.Json.lookup j "ParameterKey") String.of_json
    ; default_value =
        Aws.Util.option_map (Aws.Json.lookup j "DefaultValue") String.of_json
    ; no_echo = Aws.Util.option_map (Aws.Json.lookup j "NoEcho") Boolean.of_json
    ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
    }
end

module TemplateParameters = struct
  type t = TemplateParameter.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map TemplateParameter.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list TemplateParameter.to_query v

  let to_json v = `List (List.map TemplateParameter.to_json v)

  let of_json j = Aws.Json.to_list TemplateParameter.of_json j
end

module StopStackSetOperationOutput = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ChangeSetType = struct
  type t =
    | CREATE
    | UPDATE
    | IMPORT

  let str_to_t = [ "IMPORT", IMPORT; "UPDATE", UPDATE; "CREATE", CREATE ]

  let t_to_str = [ IMPORT, "IMPORT"; UPDATE, "UPDATE"; CREATE, "CREATE" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module DescribeTypeInput = struct
  type t =
    { type_ : RegistryType.t option
    ; type_name : String.t option
    ; arn : String.t option
    ; version_id : String.t option
    }

  let make ?type_ ?type_name ?arn ?version_id () = { type_; type_name; arn; version_id }

  let parse xml =
    Some
      { type_ = Aws.Util.option_bind (Aws.Xml.member "Type" xml) RegistryType.parse
      ; type_name = Aws.Util.option_bind (Aws.Xml.member "TypeName" xml) String.parse
      ; arn = Aws.Util.option_bind (Aws.Xml.member "Arn" xml) String.parse
      ; version_id = Aws.Util.option_bind (Aws.Xml.member "VersionId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.version_id (fun f ->
               Aws.Query.Pair ("VersionId", String.to_query f))
         ; Aws.Util.option_map v.arn (fun f -> Aws.Query.Pair ("Arn", String.to_query f))
         ; Aws.Util.option_map v.type_name (fun f ->
               Aws.Query.Pair ("TypeName", String.to_query f))
         ; Aws.Util.option_map v.type_ (fun f ->
               Aws.Query.Pair ("Type", RegistryType.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.version_id (fun f -> "VersionId", String.to_json f)
         ; Aws.Util.option_map v.arn (fun f -> "Arn", String.to_json f)
         ; Aws.Util.option_map v.type_name (fun f -> "TypeName", String.to_json f)
         ; Aws.Util.option_map v.type_ (fun f -> "Type", RegistryType.to_json f)
         ])

  let of_json j =
    { type_ = Aws.Util.option_map (Aws.Json.lookup j "Type") RegistryType.of_json
    ; type_name = Aws.Util.option_map (Aws.Json.lookup j "TypeName") String.of_json
    ; arn = Aws.Util.option_map (Aws.Json.lookup j "Arn") String.of_json
    ; version_id = Aws.Util.option_map (Aws.Json.lookup j "VersionId") String.of_json
    }
end

module OperationInProgressException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module DetectStackSetDriftInput = struct
  type t =
    { stack_set_name : String.t
    ; operation_preferences : StackSetOperationPreferences.t option
    ; operation_id : String.t option
    }

  let make ~stack_set_name ?operation_preferences ?operation_id () =
    { stack_set_name; operation_preferences; operation_id }

  let parse xml =
    Some
      { stack_set_name =
          Aws.Xml.required
            "StackSetName"
            (Aws.Util.option_bind (Aws.Xml.member "StackSetName" xml) String.parse)
      ; operation_preferences =
          Aws.Util.option_bind
            (Aws.Xml.member "OperationPreferences" xml)
            StackSetOperationPreferences.parse
      ; operation_id =
          Aws.Util.option_bind (Aws.Xml.member "OperationId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.operation_id (fun f ->
               Aws.Query.Pair ("OperationId", String.to_query f))
         ; Aws.Util.option_map v.operation_preferences (fun f ->
               Aws.Query.Pair
                 ("OperationPreferences", StackSetOperationPreferences.to_query f))
         ; Some (Aws.Query.Pair ("StackSetName", String.to_query v.stack_set_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.operation_id (fun f -> "OperationId", String.to_json f)
         ; Aws.Util.option_map v.operation_preferences (fun f ->
               "OperationPreferences", StackSetOperationPreferences.to_json f)
         ; Some ("StackSetName", String.to_json v.stack_set_name)
         ])

  let of_json j =
    { stack_set_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackSetName"))
    ; operation_preferences =
        Aws.Util.option_map
          (Aws.Json.lookup j "OperationPreferences")
          StackSetOperationPreferences.of_json
    ; operation_id = Aws.Util.option_map (Aws.Json.lookup j "OperationId") String.of_json
    }
end

module ExecutionStatus = struct
  type t =
    | UNAVAILABLE
    | AVAILABLE
    | EXECUTE_IN_PROGRESS
    | EXECUTE_COMPLETE
    | EXECUTE_FAILED
    | OBSOLETE

  let str_to_t =
    [ "OBSOLETE", OBSOLETE
    ; "EXECUTE_FAILED", EXECUTE_FAILED
    ; "EXECUTE_COMPLETE", EXECUTE_COMPLETE
    ; "EXECUTE_IN_PROGRESS", EXECUTE_IN_PROGRESS
    ; "AVAILABLE", AVAILABLE
    ; "UNAVAILABLE", UNAVAILABLE
    ]

  let t_to_str =
    [ OBSOLETE, "OBSOLETE"
    ; EXECUTE_FAILED, "EXECUTE_FAILED"
    ; EXECUTE_COMPLETE, "EXECUTE_COMPLETE"
    ; EXECUTE_IN_PROGRESS, "EXECUTE_IN_PROGRESS"
    ; AVAILABLE, "AVAILABLE"
    ; UNAVAILABLE, "UNAVAILABLE"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module Scope = struct
  type t = ResourceAttribute.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map ResourceAttribute.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list ResourceAttribute.to_query v

  let to_json v = `List (List.map ResourceAttribute.to_json v)

  let of_json j = Aws.Json.to_list ResourceAttribute.of_json j
end

module Replacement = struct
  type t =
    | True
    | False
    | Conditional

  let str_to_t = [ "Conditional", Conditional; "False", False; "True", True ]

  let t_to_str = [ Conditional, "Conditional"; False, "False"; True, "True" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module ResourceChange = struct
  type t =
    { action : ChangeAction.t option
    ; logical_resource_id : String.t option
    ; physical_resource_id : String.t option
    ; resource_type : String.t option
    ; replacement : Replacement.t option
    ; scope : Scope.t
    ; details : ResourceChangeDetails.t
    }

  let make
      ?action
      ?logical_resource_id
      ?physical_resource_id
      ?resource_type
      ?replacement
      ?(scope = [])
      ?(details = [])
      () =
    { action
    ; logical_resource_id
    ; physical_resource_id
    ; resource_type
    ; replacement
    ; scope
    ; details
    }

  let parse xml =
    Some
      { action = Aws.Util.option_bind (Aws.Xml.member "Action" xml) ChangeAction.parse
      ; logical_resource_id =
          Aws.Util.option_bind (Aws.Xml.member "LogicalResourceId" xml) String.parse
      ; physical_resource_id =
          Aws.Util.option_bind (Aws.Xml.member "PhysicalResourceId" xml) String.parse
      ; resource_type =
          Aws.Util.option_bind (Aws.Xml.member "ResourceType" xml) String.parse
      ; replacement =
          Aws.Util.option_bind (Aws.Xml.member "Replacement" xml) Replacement.parse
      ; scope =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Scope" xml) Scope.parse)
      ; details =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "Details" xml)
               ResourceChangeDetails.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("Details.member", ResourceChangeDetails.to_query v.details))
         ; Some (Aws.Query.Pair ("Scope.member", Scope.to_query v.scope))
         ; Aws.Util.option_map v.replacement (fun f ->
               Aws.Query.Pair ("Replacement", Replacement.to_query f))
         ; Aws.Util.option_map v.resource_type (fun f ->
               Aws.Query.Pair ("ResourceType", String.to_query f))
         ; Aws.Util.option_map v.physical_resource_id (fun f ->
               Aws.Query.Pair ("PhysicalResourceId", String.to_query f))
         ; Aws.Util.option_map v.logical_resource_id (fun f ->
               Aws.Query.Pair ("LogicalResourceId", String.to_query f))
         ; Aws.Util.option_map v.action (fun f ->
               Aws.Query.Pair ("Action", ChangeAction.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Details", ResourceChangeDetails.to_json v.details)
         ; Some ("Scope", Scope.to_json v.scope)
         ; Aws.Util.option_map v.replacement (fun f ->
               "Replacement", Replacement.to_json f)
         ; Aws.Util.option_map v.resource_type (fun f -> "ResourceType", String.to_json f)
         ; Aws.Util.option_map v.physical_resource_id (fun f ->
               "PhysicalResourceId", String.to_json f)
         ; Aws.Util.option_map v.logical_resource_id (fun f ->
               "LogicalResourceId", String.to_json f)
         ; Aws.Util.option_map v.action (fun f -> "Action", ChangeAction.to_json f)
         ])

  let of_json j =
    { action = Aws.Util.option_map (Aws.Json.lookup j "Action") ChangeAction.of_json
    ; logical_resource_id =
        Aws.Util.option_map (Aws.Json.lookup j "LogicalResourceId") String.of_json
    ; physical_resource_id =
        Aws.Util.option_map (Aws.Json.lookup j "PhysicalResourceId") String.of_json
    ; resource_type =
        Aws.Util.option_map (Aws.Json.lookup j "ResourceType") String.of_json
    ; replacement =
        Aws.Util.option_map (Aws.Json.lookup j "Replacement") Replacement.of_json
    ; scope = Scope.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Scope"))
    ; details =
        ResourceChangeDetails.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Details"))
    }
end

module Change = struct
  type t =
    { type_ : ChangeType.t option
    ; resource_change : ResourceChange.t option
    }

  let make ?type_ ?resource_change () = { type_; resource_change }

  let parse xml =
    Some
      { type_ = Aws.Util.option_bind (Aws.Xml.member "Type" xml) ChangeType.parse
      ; resource_change =
          Aws.Util.option_bind (Aws.Xml.member "ResourceChange" xml) ResourceChange.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.resource_change (fun f ->
               Aws.Query.Pair ("ResourceChange", ResourceChange.to_query f))
         ; Aws.Util.option_map v.type_ (fun f ->
               Aws.Query.Pair ("Type", ChangeType.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.resource_change (fun f ->
               "ResourceChange", ResourceChange.to_json f)
         ; Aws.Util.option_map v.type_ (fun f -> "Type", ChangeType.to_json f)
         ])

  let of_json j =
    { type_ = Aws.Util.option_map (Aws.Json.lookup j "Type") ChangeType.of_json
    ; resource_change =
        Aws.Util.option_map (Aws.Json.lookup j "ResourceChange") ResourceChange.of_json
    }
end

module RegistrationStatus = struct
  type t =
    | COMPLETE
    | IN_PROGRESS
    | FAILED

  let str_to_t = [ "FAILED", FAILED; "IN_PROGRESS", IN_PROGRESS; "COMPLETE", COMPLETE ]

  let t_to_str = [ FAILED, "FAILED"; IN_PROGRESS, "IN_PROGRESS"; COMPLETE, "COMPLETE" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module ResourceIdentifierProperties = struct
  type t = (String.t, String.t) Hashtbl.t

  let make elems () = elems

  let parse xml = None

  let to_query v = Aws.Query.to_query_hashtbl String.to_string String.to_query v

  let to_json v =
    `Assoc
      (Hashtbl.fold (fun k v acc -> (String.to_string k, String.to_json v) :: acc) v [])

  let of_json j = Aws.Json.to_hashtbl String.of_string String.of_json j
end

module ResourceToImport = struct
  type t =
    { resource_type : String.t
    ; logical_resource_id : String.t
    ; resource_identifier : ResourceIdentifierProperties.t
    }

  let make ~resource_type ~logical_resource_id ~resource_identifier () =
    { resource_type; logical_resource_id; resource_identifier }

  let parse xml =
    Some
      { resource_type =
          Aws.Xml.required
            "ResourceType"
            (Aws.Util.option_bind (Aws.Xml.member "ResourceType" xml) String.parse)
      ; logical_resource_id =
          Aws.Xml.required
            "LogicalResourceId"
            (Aws.Util.option_bind (Aws.Xml.member "LogicalResourceId" xml) String.parse)
      ; resource_identifier =
          Aws.Xml.required
            "ResourceIdentifier"
            (Aws.Util.option_bind
               (Aws.Xml.member "ResourceIdentifier" xml)
               ResourceIdentifierProperties.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "ResourceIdentifier"
                , ResourceIdentifierProperties.to_query v.resource_identifier ))
         ; Some
             (Aws.Query.Pair ("LogicalResourceId", String.to_query v.logical_resource_id))
         ; Some (Aws.Query.Pair ("ResourceType", String.to_query v.resource_type))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some
             ( "ResourceIdentifier"
             , ResourceIdentifierProperties.to_json v.resource_identifier )
         ; Some ("LogicalResourceId", String.to_json v.logical_resource_id)
         ; Some ("ResourceType", String.to_json v.resource_type)
         ])

  let of_json j =
    { resource_type =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceType"))
    ; logical_resource_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LogicalResourceId"))
    ; resource_identifier =
        ResourceIdentifierProperties.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceIdentifier"))
    }
end

module ResourcesToImport = struct
  type t = ResourceToImport.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map ResourceToImport.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list ResourceToImport.to_query v

  let to_json v = `List (List.map ResourceToImport.to_json v)

  let of_json j = Aws.Json.to_list ResourceToImport.of_json j
end

module OperationStatus = struct
  type t =
    | PENDING
    | IN_PROGRESS
    | SUCCESS
    | FAILED

  let str_to_t =
    [ "FAILED", FAILED
    ; "SUCCESS", SUCCESS
    ; "IN_PROGRESS", IN_PROGRESS
    ; "PENDING", PENDING
    ]

  let t_to_str =
    [ FAILED, "FAILED"
    ; SUCCESS, "SUCCESS"
    ; IN_PROGRESS, "IN_PROGRESS"
    ; PENDING, "PENDING"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module RecordHandlerProgressInput = struct
  type t =
    { bearer_token : String.t
    ; operation_status : OperationStatus.t
    ; current_operation_status : OperationStatus.t option
    ; status_message : String.t option
    ; error_code : HandlerErrorCode.t option
    ; resource_model : String.t option
    ; client_request_token : String.t option
    }

  let make
      ~bearer_token
      ~operation_status
      ?current_operation_status
      ?status_message
      ?error_code
      ?resource_model
      ?client_request_token
      () =
    { bearer_token
    ; operation_status
    ; current_operation_status
    ; status_message
    ; error_code
    ; resource_model
    ; client_request_token
    }

  let parse xml =
    Some
      { bearer_token =
          Aws.Xml.required
            "BearerToken"
            (Aws.Util.option_bind (Aws.Xml.member "BearerToken" xml) String.parse)
      ; operation_status =
          Aws.Xml.required
            "OperationStatus"
            (Aws.Util.option_bind
               (Aws.Xml.member "OperationStatus" xml)
               OperationStatus.parse)
      ; current_operation_status =
          Aws.Util.option_bind
            (Aws.Xml.member "CurrentOperationStatus" xml)
            OperationStatus.parse
      ; status_message =
          Aws.Util.option_bind (Aws.Xml.member "StatusMessage" xml) String.parse
      ; error_code =
          Aws.Util.option_bind (Aws.Xml.member "ErrorCode" xml) HandlerErrorCode.parse
      ; resource_model =
          Aws.Util.option_bind (Aws.Xml.member "ResourceModel" xml) String.parse
      ; client_request_token =
          Aws.Util.option_bind (Aws.Xml.member "ClientRequestToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.client_request_token (fun f ->
               Aws.Query.Pair ("ClientRequestToken", String.to_query f))
         ; Aws.Util.option_map v.resource_model (fun f ->
               Aws.Query.Pair ("ResourceModel", String.to_query f))
         ; Aws.Util.option_map v.error_code (fun f ->
               Aws.Query.Pair ("ErrorCode", HandlerErrorCode.to_query f))
         ; Aws.Util.option_map v.status_message (fun f ->
               Aws.Query.Pair ("StatusMessage", String.to_query f))
         ; Aws.Util.option_map v.current_operation_status (fun f ->
               Aws.Query.Pair ("CurrentOperationStatus", OperationStatus.to_query f))
         ; Some
             (Aws.Query.Pair
                ("OperationStatus", OperationStatus.to_query v.operation_status))
         ; Some (Aws.Query.Pair ("BearerToken", String.to_query v.bearer_token))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.client_request_token (fun f ->
               "ClientRequestToken", String.to_json f)
         ; Aws.Util.option_map v.resource_model (fun f ->
               "ResourceModel", String.to_json f)
         ; Aws.Util.option_map v.error_code (fun f ->
               "ErrorCode", HandlerErrorCode.to_json f)
         ; Aws.Util.option_map v.status_message (fun f ->
               "StatusMessage", String.to_json f)
         ; Aws.Util.option_map v.current_operation_status (fun f ->
               "CurrentOperationStatus", OperationStatus.to_json f)
         ; Some ("OperationStatus", OperationStatus.to_json v.operation_status)
         ; Some ("BearerToken", String.to_json v.bearer_token)
         ])

  let of_json j =
    { bearer_token =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "BearerToken"))
    ; operation_status =
        OperationStatus.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "OperationStatus"))
    ; current_operation_status =
        Aws.Util.option_map
          (Aws.Json.lookup j "CurrentOperationStatus")
          OperationStatus.of_json
    ; status_message =
        Aws.Util.option_map (Aws.Json.lookup j "StatusMessage") String.of_json
    ; error_code =
        Aws.Util.option_map (Aws.Json.lookup j "ErrorCode") HandlerErrorCode.of_json
    ; resource_model =
        Aws.Util.option_map (Aws.Json.lookup j "ResourceModel") String.of_json
    ; client_request_token =
        Aws.Util.option_map (Aws.Json.lookup j "ClientRequestToken") String.of_json
    }
end

module DescribeTypeRegistrationOutput = struct
  type t =
    { progress_status : RegistrationStatus.t option
    ; description : String.t option
    ; type_arn : String.t option
    ; type_version_arn : String.t option
    }

  let make ?progress_status ?description ?type_arn ?type_version_arn () =
    { progress_status; description; type_arn; type_version_arn }

  let parse xml =
    Some
      { progress_status =
          Aws.Util.option_bind
            (Aws.Xml.member "ProgressStatus" xml)
            RegistrationStatus.parse
      ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
      ; type_arn = Aws.Util.option_bind (Aws.Xml.member "TypeArn" xml) String.parse
      ; type_version_arn =
          Aws.Util.option_bind (Aws.Xml.member "TypeVersionArn" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.type_version_arn (fun f ->
               Aws.Query.Pair ("TypeVersionArn", String.to_query f))
         ; Aws.Util.option_map v.type_arn (fun f ->
               Aws.Query.Pair ("TypeArn", String.to_query f))
         ; Aws.Util.option_map v.description (fun f ->
               Aws.Query.Pair ("Description", String.to_query f))
         ; Aws.Util.option_map v.progress_status (fun f ->
               Aws.Query.Pair ("ProgressStatus", RegistrationStatus.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.type_version_arn (fun f ->
               "TypeVersionArn", String.to_json f)
         ; Aws.Util.option_map v.type_arn (fun f -> "TypeArn", String.to_json f)
         ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
         ; Aws.Util.option_map v.progress_status (fun f ->
               "ProgressStatus", RegistrationStatus.to_json f)
         ])

  let of_json j =
    { progress_status =
        Aws.Util.option_map
          (Aws.Json.lookup j "ProgressStatus")
          RegistrationStatus.of_json
    ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
    ; type_arn = Aws.Util.option_map (Aws.Json.lookup j "TypeArn") String.of_json
    ; type_version_arn =
        Aws.Util.option_map (Aws.Json.lookup j "TypeVersionArn") String.of_json
    }
end

module SetStackPolicyInput = struct
  type t =
    { stack_name : String.t
    ; stack_policy_body : String.t option
    ; stack_policy_u_r_l : String.t option
    }

  let make ~stack_name ?stack_policy_body ?stack_policy_u_r_l () =
    { stack_name; stack_policy_body; stack_policy_u_r_l }

  let parse xml =
    Some
      { stack_name =
          Aws.Xml.required
            "StackName"
            (Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse)
      ; stack_policy_body =
          Aws.Util.option_bind (Aws.Xml.member "StackPolicyBody" xml) String.parse
      ; stack_policy_u_r_l =
          Aws.Util.option_bind (Aws.Xml.member "StackPolicyURL" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.stack_policy_u_r_l (fun f ->
               Aws.Query.Pair ("StackPolicyURL", String.to_query f))
         ; Aws.Util.option_map v.stack_policy_body (fun f ->
               Aws.Query.Pair ("StackPolicyBody", String.to_query f))
         ; Some (Aws.Query.Pair ("StackName", String.to_query v.stack_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.stack_policy_u_r_l (fun f ->
               "StackPolicyURL", String.to_json f)
         ; Aws.Util.option_map v.stack_policy_body (fun f ->
               "StackPolicyBody", String.to_json f)
         ; Some ("StackName", String.to_json v.stack_name)
         ])

  let of_json j =
    { stack_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackName"))
    ; stack_policy_body =
        Aws.Util.option_map (Aws.Json.lookup j "StackPolicyBody") String.of_json
    ; stack_policy_u_r_l =
        Aws.Util.option_map (Aws.Json.lookup j "StackPolicyURL") String.of_json
    }
end

module StackSetSummaries = struct
  type t = StackSetSummary.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map StackSetSummary.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list StackSetSummary.to_query v

  let to_json v = `List (List.map StackSetSummary.to_json v)

  let of_json j = Aws.Json.to_list StackSetSummary.of_json j
end

module ListStackSetOperationResultsInput = struct
  type t =
    { stack_set_name : String.t
    ; operation_id : String.t
    ; next_token : String.t option
    ; max_results : Integer.t option
    }

  let make ~stack_set_name ~operation_id ?next_token ?max_results () =
    { stack_set_name; operation_id; next_token; max_results }

  let parse xml =
    Some
      { stack_set_name =
          Aws.Xml.required
            "StackSetName"
            (Aws.Util.option_bind (Aws.Xml.member "StackSetName" xml) String.parse)
      ; operation_id =
          Aws.Xml.required
            "OperationId"
            (Aws.Util.option_bind (Aws.Xml.member "OperationId" xml) String.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_results (fun f ->
               Aws.Query.Pair ("MaxResults", Integer.to_query f))
         ; Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some (Aws.Query.Pair ("OperationId", String.to_query v.operation_id))
         ; Some (Aws.Query.Pair ("StackSetName", String.to_query v.stack_set_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
         ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("OperationId", String.to_json v.operation_id)
         ; Some ("StackSetName", String.to_json v.stack_set_name)
         ])

  let of_json j =
    { stack_set_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackSetName"))
    ; operation_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "OperationId"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
    }
end

module StackSetOperationStatus = struct
  type t =
    | RUNNING
    | SUCCEEDED
    | FAILED
    | STOPPING
    | STOPPED
    | QUEUED

  let str_to_t =
    [ "QUEUED", QUEUED
    ; "STOPPED", STOPPED
    ; "STOPPING", STOPPING
    ; "FAILED", FAILED
    ; "SUCCEEDED", SUCCEEDED
    ; "RUNNING", RUNNING
    ]

  let t_to_str =
    [ QUEUED, "QUEUED"
    ; STOPPED, "STOPPED"
    ; STOPPING, "STOPPING"
    ; FAILED, "FAILED"
    ; SUCCEEDED, "SUCCEEDED"
    ; RUNNING, "RUNNING"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module StackSetOperationAction = struct
  type t =
    | CREATE
    | UPDATE
    | DELETE
    | DETECT_DRIFT

  let str_to_t =
    [ "DETECT_DRIFT", DETECT_DRIFT; "DELETE", DELETE; "UPDATE", UPDATE; "CREATE", CREATE ]

  let t_to_str =
    [ DETECT_DRIFT, "DETECT_DRIFT"; DELETE, "DELETE"; UPDATE, "UPDATE"; CREATE, "CREATE" ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module StackSetOperationSummary = struct
  type t =
    { operation_id : String.t option
    ; action : StackSetOperationAction.t option
    ; status : StackSetOperationStatus.t option
    ; creation_timestamp : DateTime.t option
    ; end_timestamp : DateTime.t option
    }

  let make ?operation_id ?action ?status ?creation_timestamp ?end_timestamp () =
    { operation_id; action; status; creation_timestamp; end_timestamp }

  let parse xml =
    Some
      { operation_id =
          Aws.Util.option_bind (Aws.Xml.member "OperationId" xml) String.parse
      ; action =
          Aws.Util.option_bind (Aws.Xml.member "Action" xml) StackSetOperationAction.parse
      ; status =
          Aws.Util.option_bind (Aws.Xml.member "Status" xml) StackSetOperationStatus.parse
      ; creation_timestamp =
          Aws.Util.option_bind (Aws.Xml.member "CreationTimestamp" xml) DateTime.parse
      ; end_timestamp =
          Aws.Util.option_bind (Aws.Xml.member "EndTimestamp" xml) DateTime.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.end_timestamp (fun f ->
               Aws.Query.Pair ("EndTimestamp", DateTime.to_query f))
         ; Aws.Util.option_map v.creation_timestamp (fun f ->
               Aws.Query.Pair ("CreationTimestamp", DateTime.to_query f))
         ; Aws.Util.option_map v.status (fun f ->
               Aws.Query.Pair ("Status", StackSetOperationStatus.to_query f))
         ; Aws.Util.option_map v.action (fun f ->
               Aws.Query.Pair ("Action", StackSetOperationAction.to_query f))
         ; Aws.Util.option_map v.operation_id (fun f ->
               Aws.Query.Pair ("OperationId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.end_timestamp (fun f ->
               "EndTimestamp", DateTime.to_json f)
         ; Aws.Util.option_map v.creation_timestamp (fun f ->
               "CreationTimestamp", DateTime.to_json f)
         ; Aws.Util.option_map v.status (fun f ->
               "Status", StackSetOperationStatus.to_json f)
         ; Aws.Util.option_map v.action (fun f ->
               "Action", StackSetOperationAction.to_json f)
         ; Aws.Util.option_map v.operation_id (fun f -> "OperationId", String.to_json f)
         ])

  let of_json j =
    { operation_id = Aws.Util.option_map (Aws.Json.lookup j "OperationId") String.of_json
    ; action =
        Aws.Util.option_map (Aws.Json.lookup j "Action") StackSetOperationAction.of_json
    ; status =
        Aws.Util.option_map (Aws.Json.lookup j "Status") StackSetOperationStatus.of_json
    ; creation_timestamp =
        Aws.Util.option_map (Aws.Json.lookup j "CreationTimestamp") DateTime.of_json
    ; end_timestamp =
        Aws.Util.option_map (Aws.Json.lookup j "EndTimestamp") DateTime.of_json
    }
end

module StackSetOperationSummaries = struct
  type t = StackSetOperationSummary.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map StackSetOperationSummary.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list StackSetOperationSummary.to_query v

  let to_json v = `List (List.map StackSetOperationSummary.to_json v)

  let of_json j = Aws.Json.to_list StackSetOperationSummary.of_json j
end

module Export = struct
  type t =
    { exporting_stack_id : String.t option
    ; name : String.t option
    ; value : String.t option
    }

  let make ?exporting_stack_id ?name ?value () = { exporting_stack_id; name; value }

  let parse xml =
    Some
      { exporting_stack_id =
          Aws.Util.option_bind (Aws.Xml.member "ExportingStackId" xml) String.parse
      ; name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse
      ; value = Aws.Util.option_bind (Aws.Xml.member "Value" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.value (fun f ->
               Aws.Query.Pair ("Value", String.to_query f))
         ; Aws.Util.option_map v.name (fun f ->
               Aws.Query.Pair ("Name", String.to_query f))
         ; Aws.Util.option_map v.exporting_stack_id (fun f ->
               Aws.Query.Pair ("ExportingStackId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.value (fun f -> "Value", String.to_json f)
         ; Aws.Util.option_map v.name (fun f -> "Name", String.to_json f)
         ; Aws.Util.option_map v.exporting_stack_id (fun f ->
               "ExportingStackId", String.to_json f)
         ])

  let of_json j =
    { exporting_stack_id =
        Aws.Util.option_map (Aws.Json.lookup j "ExportingStackId") String.of_json
    ; name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json
    ; value = Aws.Util.option_map (Aws.Json.lookup j "Value") String.of_json
    }
end

module ListStackResourcesOutput = struct
  type t =
    { stack_resource_summaries : StackResourceSummaries.t
    ; next_token : String.t option
    }

  let make ?(stack_resource_summaries = []) ?next_token () =
    { stack_resource_summaries; next_token }

  let parse xml =
    Some
      { stack_resource_summaries =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "StackResourceSummaries" xml)
               StackResourceSummaries.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "StackResourceSummaries.member"
                , StackResourceSummaries.to_query v.stack_resource_summaries ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some
             ( "StackResourceSummaries"
             , StackResourceSummaries.to_json v.stack_resource_summaries )
         ])

  let of_json j =
    { stack_resource_summaries =
        StackResourceSummaries.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "StackResourceSummaries"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module ListImportsInput = struct
  type t =
    { export_name : String.t
    ; next_token : String.t option
    }

  let make ~export_name ?next_token () = { export_name; next_token }

  let parse xml =
    Some
      { export_name =
          Aws.Xml.required
            "ExportName"
            (Aws.Util.option_bind (Aws.Xml.member "ExportName" xml) String.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some (Aws.Query.Pair ("ExportName", String.to_query v.export_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("ExportName", String.to_json v.export_name)
         ])

  let of_json j =
    { export_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ExportName"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module ListTypeVersionsInput = struct
  type t =
    { type_ : RegistryType.t option
    ; type_name : String.t option
    ; arn : String.t option
    ; max_results : Integer.t option
    ; next_token : String.t option
    ; deprecated_status : DeprecatedStatus.t option
    }

  let make ?type_ ?type_name ?arn ?max_results ?next_token ?deprecated_status () =
    { type_; type_name; arn; max_results; next_token; deprecated_status }

  let parse xml =
    Some
      { type_ = Aws.Util.option_bind (Aws.Xml.member "Type" xml) RegistryType.parse
      ; type_name = Aws.Util.option_bind (Aws.Xml.member "TypeName" xml) String.parse
      ; arn = Aws.Util.option_bind (Aws.Xml.member "Arn" xml) String.parse
      ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      ; deprecated_status =
          Aws.Util.option_bind
            (Aws.Xml.member "DeprecatedStatus" xml)
            DeprecatedStatus.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.deprecated_status (fun f ->
               Aws.Query.Pair ("DeprecatedStatus", DeprecatedStatus.to_query f))
         ; Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Aws.Util.option_map v.max_results (fun f ->
               Aws.Query.Pair ("MaxResults", Integer.to_query f))
         ; Aws.Util.option_map v.arn (fun f -> Aws.Query.Pair ("Arn", String.to_query f))
         ; Aws.Util.option_map v.type_name (fun f ->
               Aws.Query.Pair ("TypeName", String.to_query f))
         ; Aws.Util.option_map v.type_ (fun f ->
               Aws.Query.Pair ("Type", RegistryType.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.deprecated_status (fun f ->
               "DeprecatedStatus", DeprecatedStatus.to_json f)
         ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
         ; Aws.Util.option_map v.arn (fun f -> "Arn", String.to_json f)
         ; Aws.Util.option_map v.type_name (fun f -> "TypeName", String.to_json f)
         ; Aws.Util.option_map v.type_ (fun f -> "Type", RegistryType.to_json f)
         ])

  let of_json j =
    { type_ = Aws.Util.option_map (Aws.Json.lookup j "Type") RegistryType.of_json
    ; type_name = Aws.Util.option_map (Aws.Json.lookup j "TypeName") String.of_json
    ; arn = Aws.Util.option_map (Aws.Json.lookup j "Arn") String.of_json
    ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    ; deprecated_status =
        Aws.Util.option_map
          (Aws.Json.lookup j "DeprecatedStatus")
          DeprecatedStatus.of_json
    }
end

module StackSetOperationResultSummaries = struct
  type t = StackSetOperationResultSummary.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map StackSetOperationResultSummary.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list StackSetOperationResultSummary.to_query v

  let to_json v = `List (List.map StackSetOperationResultSummary.to_json v)

  let of_json j = Aws.Json.to_list StackSetOperationResultSummary.of_json j
end

module ListStackSetOperationsOutput = struct
  type t =
    { summaries : StackSetOperationSummaries.t
    ; next_token : String.t option
    }

  let make ?(summaries = []) ?next_token () = { summaries; next_token }

  let parse xml =
    Some
      { summaries =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "Summaries" xml)
               StackSetOperationSummaries.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("Summaries.member", StackSetOperationSummaries.to_query v.summaries))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("Summaries", StackSetOperationSummaries.to_json v.summaries)
         ])

  let of_json j =
    { summaries =
        StackSetOperationSummaries.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Summaries"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module RegisterTypeOutput = struct
  type t = { registration_token : String.t option }

  let make ?registration_token () = { registration_token }

  let parse xml =
    Some
      { registration_token =
          Aws.Util.option_bind (Aws.Xml.member "RegistrationToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.registration_token (fun f ->
               Aws.Query.Pair ("RegistrationToken", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.registration_token (fun f ->
               "RegistrationToken", String.to_json f)
         ])

  let of_json j =
    { registration_token =
        Aws.Util.option_map (Aws.Json.lookup j "RegistrationToken") String.of_json
    }
end

module InvalidOperationException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ChangeSetNotFoundException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module StackStatusFilter = struct
  type t = StackStatus.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map StackStatus.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list StackStatus.to_query v

  let to_json v = `List (List.map StackStatus.to_json v)

  let of_json j = Aws.Json.to_list StackStatus.of_json j
end

module UpdateStackInstancesInput = struct
  type t =
    { stack_set_name : String.t
    ; accounts : AccountList.t
    ; deployment_targets : DeploymentTargets.t option
    ; regions : RegionList.t
    ; parameter_overrides : Parameters.t
    ; operation_preferences : StackSetOperationPreferences.t option
    ; operation_id : String.t option
    }

  let make
      ~stack_set_name
      ?(accounts = [])
      ?deployment_targets
      ~regions
      ?(parameter_overrides = [])
      ?operation_preferences
      ?operation_id
      () =
    { stack_set_name
    ; accounts
    ; deployment_targets
    ; regions
    ; parameter_overrides
    ; operation_preferences
    ; operation_id
    }

  let parse xml =
    Some
      { stack_set_name =
          Aws.Xml.required
            "StackSetName"
            (Aws.Util.option_bind (Aws.Xml.member "StackSetName" xml) String.parse)
      ; accounts =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Accounts" xml) AccountList.parse)
      ; deployment_targets =
          Aws.Util.option_bind
            (Aws.Xml.member "DeploymentTargets" xml)
            DeploymentTargets.parse
      ; regions =
          Aws.Xml.required
            "Regions"
            (Aws.Util.option_bind (Aws.Xml.member "Regions" xml) RegionList.parse)
      ; parameter_overrides =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ParameterOverrides" xml)
               Parameters.parse)
      ; operation_preferences =
          Aws.Util.option_bind
            (Aws.Xml.member "OperationPreferences" xml)
            StackSetOperationPreferences.parse
      ; operation_id =
          Aws.Util.option_bind (Aws.Xml.member "OperationId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.operation_id (fun f ->
               Aws.Query.Pair ("OperationId", String.to_query f))
         ; Aws.Util.option_map v.operation_preferences (fun f ->
               Aws.Query.Pair
                 ("OperationPreferences", StackSetOperationPreferences.to_query f))
         ; Some
             (Aws.Query.Pair
                ("ParameterOverrides.member", Parameters.to_query v.parameter_overrides))
         ; Some (Aws.Query.Pair ("Regions.member", RegionList.to_query v.regions))
         ; Aws.Util.option_map v.deployment_targets (fun f ->
               Aws.Query.Pair ("DeploymentTargets", DeploymentTargets.to_query f))
         ; Some (Aws.Query.Pair ("Accounts.member", AccountList.to_query v.accounts))
         ; Some (Aws.Query.Pair ("StackSetName", String.to_query v.stack_set_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.operation_id (fun f -> "OperationId", String.to_json f)
         ; Aws.Util.option_map v.operation_preferences (fun f ->
               "OperationPreferences", StackSetOperationPreferences.to_json f)
         ; Some ("ParameterOverrides", Parameters.to_json v.parameter_overrides)
         ; Some ("Regions", RegionList.to_json v.regions)
         ; Aws.Util.option_map v.deployment_targets (fun f ->
               "DeploymentTargets", DeploymentTargets.to_json f)
         ; Some ("Accounts", AccountList.to_json v.accounts)
         ; Some ("StackSetName", String.to_json v.stack_set_name)
         ])

  let of_json j =
    { stack_set_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackSetName"))
    ; accounts =
        AccountList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Accounts"))
    ; deployment_targets =
        Aws.Util.option_map
          (Aws.Json.lookup j "DeploymentTargets")
          DeploymentTargets.of_json
    ; regions = RegionList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Regions"))
    ; parameter_overrides =
        Parameters.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ParameterOverrides"))
    ; operation_preferences =
        Aws.Util.option_map
          (Aws.Json.lookup j "OperationPreferences")
          StackSetOperationPreferences.of_json
    ; operation_id = Aws.Util.option_map (Aws.Json.lookup j "OperationId") String.of_json
    }
end

module StopStackSetOperationInput = struct
  type t =
    { stack_set_name : String.t
    ; operation_id : String.t
    }

  let make ~stack_set_name ~operation_id () = { stack_set_name; operation_id }

  let parse xml =
    Some
      { stack_set_name =
          Aws.Xml.required
            "StackSetName"
            (Aws.Util.option_bind (Aws.Xml.member "StackSetName" xml) String.parse)
      ; operation_id =
          Aws.Xml.required
            "OperationId"
            (Aws.Util.option_bind (Aws.Xml.member "OperationId" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("OperationId", String.to_query v.operation_id))
         ; Some (Aws.Query.Pair ("StackSetName", String.to_query v.stack_set_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("OperationId", String.to_json v.operation_id)
         ; Some ("StackSetName", String.to_json v.stack_set_name)
         ])

  let of_json j =
    { stack_set_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackSetName"))
    ; operation_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "OperationId"))
    }
end

module Imports = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module ListImportsOutput = struct
  type t =
    { imports : Imports.t
    ; next_token : String.t option
    }

  let make ?(imports = []) ?next_token () = { imports; next_token }

  let parse xml =
    Some
      { imports =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Imports" xml) Imports.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some (Aws.Query.Pair ("Imports.member", Imports.to_query v.imports))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("Imports", Imports.to_json v.imports)
         ])

  let of_json j =
    { imports = Imports.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Imports"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module DeleteChangeSetOutput = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module TypeVersionSummaries = struct
  type t = TypeVersionSummary.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map TypeVersionSummary.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list TypeVersionSummary.to_query v

  let to_json v = `List (List.map TypeVersionSummary.to_json v)

  let of_json j = Aws.Json.to_list TypeVersionSummary.of_json j
end

module ListTypeVersionsOutput = struct
  type t =
    { type_version_summaries : TypeVersionSummaries.t
    ; next_token : String.t option
    }

  let make ?(type_version_summaries = []) ?next_token () =
    { type_version_summaries; next_token }

  let parse xml =
    Some
      { type_version_summaries =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "TypeVersionSummaries" xml)
               TypeVersionSummaries.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "TypeVersionSummaries.member"
                , TypeVersionSummaries.to_query v.type_version_summaries ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some
             ( "TypeVersionSummaries"
             , TypeVersionSummaries.to_json v.type_version_summaries )
         ])

  let of_json j =
    { type_version_summaries =
        TypeVersionSummaries.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "TypeVersionSummaries"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module ListTypesInput = struct
  type t =
    { visibility : Visibility.t option
    ; provisioning_type : ProvisioningType.t option
    ; deprecated_status : DeprecatedStatus.t option
    ; max_results : Integer.t option
    ; next_token : String.t option
    }

  let make ?visibility ?provisioning_type ?deprecated_status ?max_results ?next_token () =
    { visibility; provisioning_type; deprecated_status; max_results; next_token }

  let parse xml =
    Some
      { visibility =
          Aws.Util.option_bind (Aws.Xml.member "Visibility" xml) Visibility.parse
      ; provisioning_type =
          Aws.Util.option_bind
            (Aws.Xml.member "ProvisioningType" xml)
            ProvisioningType.parse
      ; deprecated_status =
          Aws.Util.option_bind
            (Aws.Xml.member "DeprecatedStatus" xml)
            DeprecatedStatus.parse
      ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Aws.Util.option_map v.max_results (fun f ->
               Aws.Query.Pair ("MaxResults", Integer.to_query f))
         ; Aws.Util.option_map v.deprecated_status (fun f ->
               Aws.Query.Pair ("DeprecatedStatus", DeprecatedStatus.to_query f))
         ; Aws.Util.option_map v.provisioning_type (fun f ->
               Aws.Query.Pair ("ProvisioningType", ProvisioningType.to_query f))
         ; Aws.Util.option_map v.visibility (fun f ->
               Aws.Query.Pair ("Visibility", Visibility.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
         ; Aws.Util.option_map v.deprecated_status (fun f ->
               "DeprecatedStatus", DeprecatedStatus.to_json f)
         ; Aws.Util.option_map v.provisioning_type (fun f ->
               "ProvisioningType", ProvisioningType.to_json f)
         ; Aws.Util.option_map v.visibility (fun f -> "Visibility", Visibility.to_json f)
         ])

  let of_json j =
    { visibility = Aws.Util.option_map (Aws.Json.lookup j "Visibility") Visibility.of_json
    ; provisioning_type =
        Aws.Util.option_map
          (Aws.Json.lookup j "ProvisioningType")
          ProvisioningType.of_json
    ; deprecated_status =
        Aws.Util.option_map
          (Aws.Json.lookup j "DeprecatedStatus")
          DeprecatedStatus.of_json
    ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module Exports = struct
  type t = Export.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map Export.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list Export.to_query v

  let to_json v = `List (List.map Export.to_json v)

  let of_json j = Aws.Json.to_list Export.of_json j
end

module DetectStackDriftInput = struct
  type t =
    { stack_name : String.t
    ; logical_resource_ids : LogicalResourceIds.t
    }

  let make ~stack_name ?(logical_resource_ids = []) () =
    { stack_name; logical_resource_ids }

  let parse xml =
    Some
      { stack_name =
          Aws.Xml.required
            "StackName"
            (Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse)
      ; logical_resource_ids =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "LogicalResourceIds" xml)
               LogicalResourceIds.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "LogicalResourceIds.member"
                , LogicalResourceIds.to_query v.logical_resource_ids ))
         ; Some (Aws.Query.Pair ("StackName", String.to_query v.stack_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("LogicalResourceIds", LogicalResourceIds.to_json v.logical_resource_ids)
         ; Some ("StackName", String.to_json v.stack_name)
         ])

  let of_json j =
    { stack_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackName"))
    ; logical_resource_ids =
        LogicalResourceIds.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "LogicalResourceIds"))
    }
end

module DescribeStackSetOutput = struct
  type t = { stack_set : StackSet.t option }

  let make ?stack_set () = { stack_set }

  let parse xml =
    Some
      { stack_set = Aws.Util.option_bind (Aws.Xml.member "StackSet" xml) StackSet.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.stack_set (fun f ->
               Aws.Query.Pair ("StackSet", StackSet.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.stack_set (fun f -> "StackSet", StackSet.to_json f) ])

  let of_json j =
    { stack_set = Aws.Util.option_map (Aws.Json.lookup j "StackSet") StackSet.of_json }
end

module DescribeStackEventsOutput = struct
  type t =
    { stack_events : StackEvents.t
    ; next_token : String.t option
    }

  let make ?(stack_events = []) ?next_token () = { stack_events; next_token }

  let parse xml =
    Some
      { stack_events =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "StackEvents" xml) StackEvents.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair ("StackEvents.member", StackEvents.to_query v.stack_events))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("StackEvents", StackEvents.to_json v.stack_events)
         ])

  let of_json j =
    { stack_events =
        StackEvents.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackEvents"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module ContinueUpdateRollbackOutput = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module StackSetOperation = struct
  type t =
    { operation_id : String.t option
    ; stack_set_id : String.t option
    ; action : StackSetOperationAction.t option
    ; status : StackSetOperationStatus.t option
    ; operation_preferences : StackSetOperationPreferences.t option
    ; retain_stacks : Boolean.t option
    ; administration_role_a_r_n : String.t option
    ; execution_role_name : String.t option
    ; creation_timestamp : DateTime.t option
    ; end_timestamp : DateTime.t option
    ; deployment_targets : DeploymentTargets.t option
    ; stack_set_drift_detection_details : StackSetDriftDetectionDetails.t option
    }

  let make
      ?operation_id
      ?stack_set_id
      ?action
      ?status
      ?operation_preferences
      ?retain_stacks
      ?administration_role_a_r_n
      ?execution_role_name
      ?creation_timestamp
      ?end_timestamp
      ?deployment_targets
      ?stack_set_drift_detection_details
      () =
    { operation_id
    ; stack_set_id
    ; action
    ; status
    ; operation_preferences
    ; retain_stacks
    ; administration_role_a_r_n
    ; execution_role_name
    ; creation_timestamp
    ; end_timestamp
    ; deployment_targets
    ; stack_set_drift_detection_details
    }

  let parse xml =
    Some
      { operation_id =
          Aws.Util.option_bind (Aws.Xml.member "OperationId" xml) String.parse
      ; stack_set_id = Aws.Util.option_bind (Aws.Xml.member "StackSetId" xml) String.parse
      ; action =
          Aws.Util.option_bind (Aws.Xml.member "Action" xml) StackSetOperationAction.parse
      ; status =
          Aws.Util.option_bind (Aws.Xml.member "Status" xml) StackSetOperationStatus.parse
      ; operation_preferences =
          Aws.Util.option_bind
            (Aws.Xml.member "OperationPreferences" xml)
            StackSetOperationPreferences.parse
      ; retain_stacks =
          Aws.Util.option_bind (Aws.Xml.member "RetainStacks" xml) Boolean.parse
      ; administration_role_a_r_n =
          Aws.Util.option_bind (Aws.Xml.member "AdministrationRoleARN" xml) String.parse
      ; execution_role_name =
          Aws.Util.option_bind (Aws.Xml.member "ExecutionRoleName" xml) String.parse
      ; creation_timestamp =
          Aws.Util.option_bind (Aws.Xml.member "CreationTimestamp" xml) DateTime.parse
      ; end_timestamp =
          Aws.Util.option_bind (Aws.Xml.member "EndTimestamp" xml) DateTime.parse
      ; deployment_targets =
          Aws.Util.option_bind
            (Aws.Xml.member "DeploymentTargets" xml)
            DeploymentTargets.parse
      ; stack_set_drift_detection_details =
          Aws.Util.option_bind
            (Aws.Xml.member "StackSetDriftDetectionDetails" xml)
            StackSetDriftDetectionDetails.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.stack_set_drift_detection_details (fun f ->
               Aws.Query.Pair
                 ( "StackSetDriftDetectionDetails"
                 , StackSetDriftDetectionDetails.to_query f ))
         ; Aws.Util.option_map v.deployment_targets (fun f ->
               Aws.Query.Pair ("DeploymentTargets", DeploymentTargets.to_query f))
         ; Aws.Util.option_map v.end_timestamp (fun f ->
               Aws.Query.Pair ("EndTimestamp", DateTime.to_query f))
         ; Aws.Util.option_map v.creation_timestamp (fun f ->
               Aws.Query.Pair ("CreationTimestamp", DateTime.to_query f))
         ; Aws.Util.option_map v.execution_role_name (fun f ->
               Aws.Query.Pair ("ExecutionRoleName", String.to_query f))
         ; Aws.Util.option_map v.administration_role_a_r_n (fun f ->
               Aws.Query.Pair ("AdministrationRoleARN", String.to_query f))
         ; Aws.Util.option_map v.retain_stacks (fun f ->
               Aws.Query.Pair ("RetainStacks", Boolean.to_query f))
         ; Aws.Util.option_map v.operation_preferences (fun f ->
               Aws.Query.Pair
                 ("OperationPreferences", StackSetOperationPreferences.to_query f))
         ; Aws.Util.option_map v.status (fun f ->
               Aws.Query.Pair ("Status", StackSetOperationStatus.to_query f))
         ; Aws.Util.option_map v.action (fun f ->
               Aws.Query.Pair ("Action", StackSetOperationAction.to_query f))
         ; Aws.Util.option_map v.stack_set_id (fun f ->
               Aws.Query.Pair ("StackSetId", String.to_query f))
         ; Aws.Util.option_map v.operation_id (fun f ->
               Aws.Query.Pair ("OperationId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.stack_set_drift_detection_details (fun f ->
               "StackSetDriftDetectionDetails", StackSetDriftDetectionDetails.to_json f)
         ; Aws.Util.option_map v.deployment_targets (fun f ->
               "DeploymentTargets", DeploymentTargets.to_json f)
         ; Aws.Util.option_map v.end_timestamp (fun f ->
               "EndTimestamp", DateTime.to_json f)
         ; Aws.Util.option_map v.creation_timestamp (fun f ->
               "CreationTimestamp", DateTime.to_json f)
         ; Aws.Util.option_map v.execution_role_name (fun f ->
               "ExecutionRoleName", String.to_json f)
         ; Aws.Util.option_map v.administration_role_a_r_n (fun f ->
               "AdministrationRoleARN", String.to_json f)
         ; Aws.Util.option_map v.retain_stacks (fun f ->
               "RetainStacks", Boolean.to_json f)
         ; Aws.Util.option_map v.operation_preferences (fun f ->
               "OperationPreferences", StackSetOperationPreferences.to_json f)
         ; Aws.Util.option_map v.status (fun f ->
               "Status", StackSetOperationStatus.to_json f)
         ; Aws.Util.option_map v.action (fun f ->
               "Action", StackSetOperationAction.to_json f)
         ; Aws.Util.option_map v.stack_set_id (fun f -> "StackSetId", String.to_json f)
         ; Aws.Util.option_map v.operation_id (fun f -> "OperationId", String.to_json f)
         ])

  let of_json j =
    { operation_id = Aws.Util.option_map (Aws.Json.lookup j "OperationId") String.of_json
    ; stack_set_id = Aws.Util.option_map (Aws.Json.lookup j "StackSetId") String.of_json
    ; action =
        Aws.Util.option_map (Aws.Json.lookup j "Action") StackSetOperationAction.of_json
    ; status =
        Aws.Util.option_map (Aws.Json.lookup j "Status") StackSetOperationStatus.of_json
    ; operation_preferences =
        Aws.Util.option_map
          (Aws.Json.lookup j "OperationPreferences")
          StackSetOperationPreferences.of_json
    ; retain_stacks =
        Aws.Util.option_map (Aws.Json.lookup j "RetainStacks") Boolean.of_json
    ; administration_role_a_r_n =
        Aws.Util.option_map (Aws.Json.lookup j "AdministrationRoleARN") String.of_json
    ; execution_role_name =
        Aws.Util.option_map (Aws.Json.lookup j "ExecutionRoleName") String.of_json
    ; creation_timestamp =
        Aws.Util.option_map (Aws.Json.lookup j "CreationTimestamp") DateTime.of_json
    ; end_timestamp =
        Aws.Util.option_map (Aws.Json.lookup j "EndTimestamp") DateTime.of_json
    ; deployment_targets =
        Aws.Util.option_map
          (Aws.Json.lookup j "DeploymentTargets")
          DeploymentTargets.of_json
    ; stack_set_drift_detection_details =
        Aws.Util.option_map
          (Aws.Json.lookup j "StackSetDriftDetectionDetails")
          StackSetDriftDetectionDetails.of_json
    }
end

module DescribeStackSetOperationOutput = struct
  type t = { stack_set_operation : StackSetOperation.t option }

  let make ?stack_set_operation () = { stack_set_operation }

  let parse xml =
    Some
      { stack_set_operation =
          Aws.Util.option_bind
            (Aws.Xml.member "StackSetOperation" xml)
            StackSetOperation.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.stack_set_operation (fun f ->
               Aws.Query.Pair ("StackSetOperation", StackSetOperation.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.stack_set_operation (fun f ->
               "StackSetOperation", StackSetOperation.to_json f)
         ])

  let of_json j =
    { stack_set_operation =
        Aws.Util.option_map
          (Aws.Json.lookup j "StackSetOperation")
          StackSetOperation.of_json
    }
end

module TypeSummary = struct
  type t =
    { type_ : RegistryType.t option
    ; type_name : String.t option
    ; default_version_id : String.t option
    ; type_arn : String.t option
    ; last_updated : DateTime.t option
    ; description : String.t option
    }

  let make ?type_ ?type_name ?default_version_id ?type_arn ?last_updated ?description () =
    { type_; type_name; default_version_id; type_arn; last_updated; description }

  let parse xml =
    Some
      { type_ = Aws.Util.option_bind (Aws.Xml.member "Type" xml) RegistryType.parse
      ; type_name = Aws.Util.option_bind (Aws.Xml.member "TypeName" xml) String.parse
      ; default_version_id =
          Aws.Util.option_bind (Aws.Xml.member "DefaultVersionId" xml) String.parse
      ; type_arn = Aws.Util.option_bind (Aws.Xml.member "TypeArn" xml) String.parse
      ; last_updated =
          Aws.Util.option_bind (Aws.Xml.member "LastUpdated" xml) DateTime.parse
      ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.description (fun f ->
               Aws.Query.Pair ("Description", String.to_query f))
         ; Aws.Util.option_map v.last_updated (fun f ->
               Aws.Query.Pair ("LastUpdated", DateTime.to_query f))
         ; Aws.Util.option_map v.type_arn (fun f ->
               Aws.Query.Pair ("TypeArn", String.to_query f))
         ; Aws.Util.option_map v.default_version_id (fun f ->
               Aws.Query.Pair ("DefaultVersionId", String.to_query f))
         ; Aws.Util.option_map v.type_name (fun f ->
               Aws.Query.Pair ("TypeName", String.to_query f))
         ; Aws.Util.option_map v.type_ (fun f ->
               Aws.Query.Pair ("Type", RegistryType.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
         ; Aws.Util.option_map v.last_updated (fun f -> "LastUpdated", DateTime.to_json f)
         ; Aws.Util.option_map v.type_arn (fun f -> "TypeArn", String.to_json f)
         ; Aws.Util.option_map v.default_version_id (fun f ->
               "DefaultVersionId", String.to_json f)
         ; Aws.Util.option_map v.type_name (fun f -> "TypeName", String.to_json f)
         ; Aws.Util.option_map v.type_ (fun f -> "Type", RegistryType.to_json f)
         ])

  let of_json j =
    { type_ = Aws.Util.option_map (Aws.Json.lookup j "Type") RegistryType.of_json
    ; type_name = Aws.Util.option_map (Aws.Json.lookup j "TypeName") String.of_json
    ; default_version_id =
        Aws.Util.option_map (Aws.Json.lookup j "DefaultVersionId") String.of_json
    ; type_arn = Aws.Util.option_map (Aws.Json.lookup j "TypeArn") String.of_json
    ; last_updated =
        Aws.Util.option_map (Aws.Json.lookup j "LastUpdated") DateTime.of_json
    ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
    }
end

module TypeSummaries = struct
  type t = TypeSummary.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map TypeSummary.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list TypeSummary.to_query v

  let to_json v = `List (List.map TypeSummary.to_json v)

  let of_json j = Aws.Json.to_list TypeSummary.of_json j
end

module CreateStackSetOutput = struct
  type t = { stack_set_id : String.t option }

  let make ?stack_set_id () = { stack_set_id }

  let parse xml =
    Some
      { stack_set_id = Aws.Util.option_bind (Aws.Xml.member "StackSetId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.stack_set_id (fun f ->
               Aws.Query.Pair ("StackSetId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.stack_set_id (fun f -> "StackSetId", String.to_json f) ])

  let of_json j =
    { stack_set_id = Aws.Util.option_map (Aws.Json.lookup j "StackSetId") String.of_json }
end

module ListTypeRegistrationsInput = struct
  type t =
    { type_ : RegistryType.t option
    ; type_name : String.t option
    ; type_arn : String.t option
    ; registration_status_filter : RegistrationStatus.t option
    ; max_results : Integer.t option
    ; next_token : String.t option
    }

  let make
      ?type_
      ?type_name
      ?type_arn
      ?registration_status_filter
      ?max_results
      ?next_token
      () =
    { type_; type_name; type_arn; registration_status_filter; max_results; next_token }

  let parse xml =
    Some
      { type_ = Aws.Util.option_bind (Aws.Xml.member "Type" xml) RegistryType.parse
      ; type_name = Aws.Util.option_bind (Aws.Xml.member "TypeName" xml) String.parse
      ; type_arn = Aws.Util.option_bind (Aws.Xml.member "TypeArn" xml) String.parse
      ; registration_status_filter =
          Aws.Util.option_bind
            (Aws.Xml.member "RegistrationStatusFilter" xml)
            RegistrationStatus.parse
      ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Aws.Util.option_map v.max_results (fun f ->
               Aws.Query.Pair ("MaxResults", Integer.to_query f))
         ; Aws.Util.option_map v.registration_status_filter (fun f ->
               Aws.Query.Pair ("RegistrationStatusFilter", RegistrationStatus.to_query f))
         ; Aws.Util.option_map v.type_arn (fun f ->
               Aws.Query.Pair ("TypeArn", String.to_query f))
         ; Aws.Util.option_map v.type_name (fun f ->
               Aws.Query.Pair ("TypeName", String.to_query f))
         ; Aws.Util.option_map v.type_ (fun f ->
               Aws.Query.Pair ("Type", RegistryType.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
         ; Aws.Util.option_map v.registration_status_filter (fun f ->
               "RegistrationStatusFilter", RegistrationStatus.to_json f)
         ; Aws.Util.option_map v.type_arn (fun f -> "TypeArn", String.to_json f)
         ; Aws.Util.option_map v.type_name (fun f -> "TypeName", String.to_json f)
         ; Aws.Util.option_map v.type_ (fun f -> "Type", RegistryType.to_json f)
         ])

  let of_json j =
    { type_ = Aws.Util.option_map (Aws.Json.lookup j "Type") RegistryType.of_json
    ; type_name = Aws.Util.option_map (Aws.Json.lookup j "TypeName") String.of_json
    ; type_arn = Aws.Util.option_map (Aws.Json.lookup j "TypeArn") String.of_json
    ; registration_status_filter =
        Aws.Util.option_map
          (Aws.Json.lookup j "RegistrationStatusFilter")
          RegistrationStatus.of_json
    ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module DescribeStackDriftDetectionStatusOutput = struct
  type t =
    { stack_id : String.t
    ; stack_drift_detection_id : String.t
    ; stack_drift_status : StackDriftStatus.t option
    ; detection_status : StackDriftDetectionStatus.t
    ; detection_status_reason : String.t option
    ; drifted_stack_resource_count : Integer.t option
    ; timestamp : DateTime.t
    }

  let make
      ~stack_id
      ~stack_drift_detection_id
      ?stack_drift_status
      ~detection_status
      ?detection_status_reason
      ?drifted_stack_resource_count
      ~timestamp
      () =
    { stack_id
    ; stack_drift_detection_id
    ; stack_drift_status
    ; detection_status
    ; detection_status_reason
    ; drifted_stack_resource_count
    ; timestamp
    }

  let parse xml =
    Some
      { stack_id =
          Aws.Xml.required
            "StackId"
            (Aws.Util.option_bind (Aws.Xml.member "StackId" xml) String.parse)
      ; stack_drift_detection_id =
          Aws.Xml.required
            "StackDriftDetectionId"
            (Aws.Util.option_bind
               (Aws.Xml.member "StackDriftDetectionId" xml)
               String.parse)
      ; stack_drift_status =
          Aws.Util.option_bind
            (Aws.Xml.member "StackDriftStatus" xml)
            StackDriftStatus.parse
      ; detection_status =
          Aws.Xml.required
            "DetectionStatus"
            (Aws.Util.option_bind
               (Aws.Xml.member "DetectionStatus" xml)
               StackDriftDetectionStatus.parse)
      ; detection_status_reason =
          Aws.Util.option_bind (Aws.Xml.member "DetectionStatusReason" xml) String.parse
      ; drifted_stack_resource_count =
          Aws.Util.option_bind
            (Aws.Xml.member "DriftedStackResourceCount" xml)
            Integer.parse
      ; timestamp =
          Aws.Xml.required
            "Timestamp"
            (Aws.Util.option_bind (Aws.Xml.member "Timestamp" xml) DateTime.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Timestamp", DateTime.to_query v.timestamp))
         ; Aws.Util.option_map v.drifted_stack_resource_count (fun f ->
               Aws.Query.Pair ("DriftedStackResourceCount", Integer.to_query f))
         ; Aws.Util.option_map v.detection_status_reason (fun f ->
               Aws.Query.Pair ("DetectionStatusReason", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("DetectionStatus", StackDriftDetectionStatus.to_query v.detection_status))
         ; Aws.Util.option_map v.stack_drift_status (fun f ->
               Aws.Query.Pair ("StackDriftStatus", StackDriftStatus.to_query f))
         ; Some
             (Aws.Query.Pair
                ("StackDriftDetectionId", String.to_query v.stack_drift_detection_id))
         ; Some (Aws.Query.Pair ("StackId", String.to_query v.stack_id))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Timestamp", DateTime.to_json v.timestamp)
         ; Aws.Util.option_map v.drifted_stack_resource_count (fun f ->
               "DriftedStackResourceCount", Integer.to_json f)
         ; Aws.Util.option_map v.detection_status_reason (fun f ->
               "DetectionStatusReason", String.to_json f)
         ; Some ("DetectionStatus", StackDriftDetectionStatus.to_json v.detection_status)
         ; Aws.Util.option_map v.stack_drift_status (fun f ->
               "StackDriftStatus", StackDriftStatus.to_json f)
         ; Some ("StackDriftDetectionId", String.to_json v.stack_drift_detection_id)
         ; Some ("StackId", String.to_json v.stack_id)
         ])

  let of_json j =
    { stack_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackId"))
    ; stack_drift_detection_id =
        String.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "StackDriftDetectionId"))
    ; stack_drift_status =
        Aws.Util.option_map
          (Aws.Json.lookup j "StackDriftStatus")
          StackDriftStatus.of_json
    ; detection_status =
        StackDriftDetectionStatus.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "DetectionStatus"))
    ; detection_status_reason =
        Aws.Util.option_map (Aws.Json.lookup j "DetectionStatusReason") String.of_json
    ; drifted_stack_resource_count =
        Aws.Util.option_map
          (Aws.Json.lookup j "DriftedStackResourceCount")
          Integer.of_json
    ; timestamp =
        DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Timestamp"))
    }
end

module DescribeTypeRegistrationInput = struct
  type t = { registration_token : String.t }

  let make ~registration_token () = { registration_token }

  let parse xml =
    Some
      { registration_token =
          Aws.Xml.required
            "RegistrationToken"
            (Aws.Util.option_bind (Aws.Xml.member "RegistrationToken" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("RegistrationToken", String.to_query v.registration_token))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("RegistrationToken", String.to_json v.registration_token) ])

  let of_json j =
    { registration_token =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RegistrationToken"))
    }
end

module EstimateTemplateCostInput = struct
  type t =
    { template_body : String.t option
    ; template_u_r_l : String.t option
    ; parameters : Parameters.t
    }

  let make ?template_body ?template_u_r_l ?(parameters = []) () =
    { template_body; template_u_r_l; parameters }

  let parse xml =
    Some
      { template_body =
          Aws.Util.option_bind (Aws.Xml.member "TemplateBody" xml) String.parse
      ; template_u_r_l =
          Aws.Util.option_bind (Aws.Xml.member "TemplateURL" xml) String.parse
      ; parameters =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Parameters" xml) Parameters.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("Parameters.member", Parameters.to_query v.parameters))
         ; Aws.Util.option_map v.template_u_r_l (fun f ->
               Aws.Query.Pair ("TemplateURL", String.to_query f))
         ; Aws.Util.option_map v.template_body (fun f ->
               Aws.Query.Pair ("TemplateBody", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("Parameters", Parameters.to_json v.parameters)
         ; Aws.Util.option_map v.template_u_r_l (fun f -> "TemplateURL", String.to_json f)
         ; Aws.Util.option_map v.template_body (fun f -> "TemplateBody", String.to_json f)
         ])

  let of_json j =
    { template_body =
        Aws.Util.option_map (Aws.Json.lookup j "TemplateBody") String.of_json
    ; template_u_r_l =
        Aws.Util.option_map (Aws.Json.lookup j "TemplateURL") String.of_json
    ; parameters =
        Parameters.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Parameters"))
    }
end

module DescribeStacksInput = struct
  type t =
    { stack_name : String.t option
    ; next_token : String.t option
    }

  let make ?stack_name ?next_token () = { stack_name; next_token }

  let parse xml =
    Some
      { stack_name = Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Aws.Util.option_map v.stack_name (fun f ->
               Aws.Query.Pair ("StackName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Aws.Util.option_map v.stack_name (fun f -> "StackName", String.to_json f)
         ])

  let of_json j =
    { stack_name = Aws.Util.option_map (Aws.Json.lookup j "StackName") String.of_json
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module DescribeStackResourceOutput = struct
  type t = { stack_resource_detail : StackResourceDetail.t option }

  let make ?stack_resource_detail () = { stack_resource_detail }

  let parse xml =
    Some
      { stack_resource_detail =
          Aws.Util.option_bind
            (Aws.Xml.member "StackResourceDetail" xml)
            StackResourceDetail.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.stack_resource_detail (fun f ->
               Aws.Query.Pair ("StackResourceDetail", StackResourceDetail.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.stack_resource_detail (fun f ->
               "StackResourceDetail", StackResourceDetail.to_json f)
         ])

  let of_json j =
    { stack_resource_detail =
        Aws.Util.option_map
          (Aws.Json.lookup j "StackResourceDetail")
          StackResourceDetail.of_json
    }
end

module ListTypesOutput = struct
  type t =
    { type_summaries : TypeSummaries.t
    ; next_token : String.t option
    }

  let make ?(type_summaries = []) ?next_token () = { type_summaries; next_token }

  let parse xml =
    Some
      { type_summaries =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "TypeSummaries" xml)
               TypeSummaries.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("TypeSummaries.member", TypeSummaries.to_query v.type_summaries))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("TypeSummaries", TypeSummaries.to_json v.type_summaries)
         ])

  let of_json j =
    { type_summaries =
        TypeSummaries.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TypeSummaries"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module ListStackSetsOutput = struct
  type t =
    { summaries : StackSetSummaries.t
    ; next_token : String.t option
    }

  let make ?(summaries = []) ?next_token () = { summaries; next_token }

  let parse xml =
    Some
      { summaries =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "Summaries" xml)
               StackSetSummaries.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair ("Summaries.member", StackSetSummaries.to_query v.summaries))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("Summaries", StackSetSummaries.to_json v.summaries)
         ])

  let of_json j =
    { summaries =
        StackSetSummaries.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Summaries"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module SetTypeDefaultVersionOutput = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module CancelUpdateStackInput = struct
  type t =
    { stack_name : String.t
    ; client_request_token : String.t option
    }

  let make ~stack_name ?client_request_token () = { stack_name; client_request_token }

  let parse xml =
    Some
      { stack_name =
          Aws.Xml.required
            "StackName"
            (Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse)
      ; client_request_token =
          Aws.Util.option_bind (Aws.Xml.member "ClientRequestToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.client_request_token (fun f ->
               Aws.Query.Pair ("ClientRequestToken", String.to_query f))
         ; Some (Aws.Query.Pair ("StackName", String.to_query v.stack_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.client_request_token (fun f ->
               "ClientRequestToken", String.to_json f)
         ; Some ("StackName", String.to_json v.stack_name)
         ])

  let of_json j =
    { stack_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackName"))
    ; client_request_token =
        Aws.Util.option_map (Aws.Json.lookup j "ClientRequestToken") String.of_json
    }
end

module StackInstance = struct
  type t =
    { stack_set_id : String.t option
    ; region : String.t option
    ; account : String.t option
    ; stack_id : String.t option
    ; parameter_overrides : Parameters.t
    ; status : StackInstanceStatus.t option
    ; stack_instance_status : StackInstanceComprehensiveStatus.t option
    ; status_reason : String.t option
    ; organizational_unit_id : String.t option
    ; drift_status : StackDriftStatus.t option
    ; last_drift_check_timestamp : DateTime.t option
    }

  let make
      ?stack_set_id
      ?region
      ?account
      ?stack_id
      ?(parameter_overrides = [])
      ?status
      ?stack_instance_status
      ?status_reason
      ?organizational_unit_id
      ?drift_status
      ?last_drift_check_timestamp
      () =
    { stack_set_id
    ; region
    ; account
    ; stack_id
    ; parameter_overrides
    ; status
    ; stack_instance_status
    ; status_reason
    ; organizational_unit_id
    ; drift_status
    ; last_drift_check_timestamp
    }

  let parse xml =
    Some
      { stack_set_id = Aws.Util.option_bind (Aws.Xml.member "StackSetId" xml) String.parse
      ; region = Aws.Util.option_bind (Aws.Xml.member "Region" xml) String.parse
      ; account = Aws.Util.option_bind (Aws.Xml.member "Account" xml) String.parse
      ; stack_id = Aws.Util.option_bind (Aws.Xml.member "StackId" xml) String.parse
      ; parameter_overrides =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ParameterOverrides" xml)
               Parameters.parse)
      ; status =
          Aws.Util.option_bind (Aws.Xml.member "Status" xml) StackInstanceStatus.parse
      ; stack_instance_status =
          Aws.Util.option_bind
            (Aws.Xml.member "StackInstanceStatus" xml)
            StackInstanceComprehensiveStatus.parse
      ; status_reason =
          Aws.Util.option_bind (Aws.Xml.member "StatusReason" xml) String.parse
      ; organizational_unit_id =
          Aws.Util.option_bind (Aws.Xml.member "OrganizationalUnitId" xml) String.parse
      ; drift_status =
          Aws.Util.option_bind (Aws.Xml.member "DriftStatus" xml) StackDriftStatus.parse
      ; last_drift_check_timestamp =
          Aws.Util.option_bind
            (Aws.Xml.member "LastDriftCheckTimestamp" xml)
            DateTime.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.last_drift_check_timestamp (fun f ->
               Aws.Query.Pair ("LastDriftCheckTimestamp", DateTime.to_query f))
         ; Aws.Util.option_map v.drift_status (fun f ->
               Aws.Query.Pair ("DriftStatus", StackDriftStatus.to_query f))
         ; Aws.Util.option_map v.organizational_unit_id (fun f ->
               Aws.Query.Pair ("OrganizationalUnitId", String.to_query f))
         ; Aws.Util.option_map v.status_reason (fun f ->
               Aws.Query.Pair ("StatusReason", String.to_query f))
         ; Aws.Util.option_map v.stack_instance_status (fun f ->
               Aws.Query.Pair
                 ("StackInstanceStatus", StackInstanceComprehensiveStatus.to_query f))
         ; Aws.Util.option_map v.status (fun f ->
               Aws.Query.Pair ("Status", StackInstanceStatus.to_query f))
         ; Some
             (Aws.Query.Pair
                ("ParameterOverrides.member", Parameters.to_query v.parameter_overrides))
         ; Aws.Util.option_map v.stack_id (fun f ->
               Aws.Query.Pair ("StackId", String.to_query f))
         ; Aws.Util.option_map v.account (fun f ->
               Aws.Query.Pair ("Account", String.to_query f))
         ; Aws.Util.option_map v.region (fun f ->
               Aws.Query.Pair ("Region", String.to_query f))
         ; Aws.Util.option_map v.stack_set_id (fun f ->
               Aws.Query.Pair ("StackSetId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.last_drift_check_timestamp (fun f ->
               "LastDriftCheckTimestamp", DateTime.to_json f)
         ; Aws.Util.option_map v.drift_status (fun f ->
               "DriftStatus", StackDriftStatus.to_json f)
         ; Aws.Util.option_map v.organizational_unit_id (fun f ->
               "OrganizationalUnitId", String.to_json f)
         ; Aws.Util.option_map v.status_reason (fun f -> "StatusReason", String.to_json f)
         ; Aws.Util.option_map v.stack_instance_status (fun f ->
               "StackInstanceStatus", StackInstanceComprehensiveStatus.to_json f)
         ; Aws.Util.option_map v.status (fun f -> "Status", StackInstanceStatus.to_json f)
         ; Some ("ParameterOverrides", Parameters.to_json v.parameter_overrides)
         ; Aws.Util.option_map v.stack_id (fun f -> "StackId", String.to_json f)
         ; Aws.Util.option_map v.account (fun f -> "Account", String.to_json f)
         ; Aws.Util.option_map v.region (fun f -> "Region", String.to_json f)
         ; Aws.Util.option_map v.stack_set_id (fun f -> "StackSetId", String.to_json f)
         ])

  let of_json j =
    { stack_set_id = Aws.Util.option_map (Aws.Json.lookup j "StackSetId") String.of_json
    ; region = Aws.Util.option_map (Aws.Json.lookup j "Region") String.of_json
    ; account = Aws.Util.option_map (Aws.Json.lookup j "Account") String.of_json
    ; stack_id = Aws.Util.option_map (Aws.Json.lookup j "StackId") String.of_json
    ; parameter_overrides =
        Parameters.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ParameterOverrides"))
    ; status =
        Aws.Util.option_map (Aws.Json.lookup j "Status") StackInstanceStatus.of_json
    ; stack_instance_status =
        Aws.Util.option_map
          (Aws.Json.lookup j "StackInstanceStatus")
          StackInstanceComprehensiveStatus.of_json
    ; status_reason =
        Aws.Util.option_map (Aws.Json.lookup j "StatusReason") String.of_json
    ; organizational_unit_id =
        Aws.Util.option_map (Aws.Json.lookup j "OrganizationalUnitId") String.of_json
    ; drift_status =
        Aws.Util.option_map (Aws.Json.lookup j "DriftStatus") StackDriftStatus.of_json
    ; last_drift_check_timestamp =
        Aws.Util.option_map (Aws.Json.lookup j "LastDriftCheckTimestamp") DateTime.of_json
    }
end

module DescribeStackInstanceOutput = struct
  type t = { stack_instance : StackInstance.t option }

  let make ?stack_instance () = { stack_instance }

  let parse xml =
    Some
      { stack_instance =
          Aws.Util.option_bind (Aws.Xml.member "StackInstance" xml) StackInstance.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.stack_instance (fun f ->
               Aws.Query.Pair ("StackInstance", StackInstance.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.stack_instance (fun f ->
               "StackInstance", StackInstance.to_json f)
         ])

  let of_json j =
    { stack_instance =
        Aws.Util.option_map (Aws.Json.lookup j "StackInstance") StackInstance.of_json
    }
end

module ValidateTemplateOutput = struct
  type t =
    { parameters : TemplateParameters.t
    ; description : String.t option
    ; capabilities : Capabilities.t
    ; capabilities_reason : String.t option
    ; declared_transforms : TransformsList.t
    }

  let make
      ?(parameters = [])
      ?description
      ?(capabilities = [])
      ?capabilities_reason
      ?(declared_transforms = [])
      () =
    { parameters; description; capabilities; capabilities_reason; declared_transforms }

  let parse xml =
    Some
      { parameters =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "Parameters" xml)
               TemplateParameters.parse)
      ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
      ; capabilities =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Capabilities" xml) Capabilities.parse)
      ; capabilities_reason =
          Aws.Util.option_bind (Aws.Xml.member "CapabilitiesReason" xml) String.parse
      ; declared_transforms =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "DeclaredTransforms" xml)
               TransformsList.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "DeclaredTransforms.member"
                , TransformsList.to_query v.declared_transforms ))
         ; Aws.Util.option_map v.capabilities_reason (fun f ->
               Aws.Query.Pair ("CapabilitiesReason", String.to_query f))
         ; Some
             (Aws.Query.Pair ("Capabilities.member", Capabilities.to_query v.capabilities))
         ; Aws.Util.option_map v.description (fun f ->
               Aws.Query.Pair ("Description", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("Parameters.member", TemplateParameters.to_query v.parameters))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("DeclaredTransforms", TransformsList.to_json v.declared_transforms)
         ; Aws.Util.option_map v.capabilities_reason (fun f ->
               "CapabilitiesReason", String.to_json f)
         ; Some ("Capabilities", Capabilities.to_json v.capabilities)
         ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
         ; Some ("Parameters", TemplateParameters.to_json v.parameters)
         ])

  let of_json j =
    { parameters =
        TemplateParameters.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Parameters"))
    ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
    ; capabilities =
        Capabilities.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Capabilities"))
    ; capabilities_reason =
        Aws.Util.option_map (Aws.Json.lookup j "CapabilitiesReason") String.of_json
    ; declared_transforms =
        TransformsList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "DeclaredTransforms"))
    }
end

module CreatedButModifiedException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ValidateTemplateInput = struct
  type t =
    { template_body : String.t option
    ; template_u_r_l : String.t option
    }

  let make ?template_body ?template_u_r_l () = { template_body; template_u_r_l }

  let parse xml =
    Some
      { template_body =
          Aws.Util.option_bind (Aws.Xml.member "TemplateBody" xml) String.parse
      ; template_u_r_l =
          Aws.Util.option_bind (Aws.Xml.member "TemplateURL" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.template_u_r_l (fun f ->
               Aws.Query.Pair ("TemplateURL", String.to_query f))
         ; Aws.Util.option_map v.template_body (fun f ->
               Aws.Query.Pair ("TemplateBody", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.template_u_r_l (fun f -> "TemplateURL", String.to_json f)
         ; Aws.Util.option_map v.template_body (fun f -> "TemplateBody", String.to_json f)
         ])

  let of_json j =
    { template_body =
        Aws.Util.option_map (Aws.Json.lookup j "TemplateBody") String.of_json
    ; template_u_r_l =
        Aws.Util.option_map (Aws.Json.lookup j "TemplateURL") String.of_json
    }
end

module CreateStackSetInput = struct
  type t =
    { stack_set_name : String.t
    ; description : String.t option
    ; template_body : String.t option
    ; template_u_r_l : String.t option
    ; parameters : Parameters.t
    ; capabilities : Capabilities.t
    ; tags : Tags.t
    ; administration_role_a_r_n : String.t option
    ; execution_role_name : String.t option
    ; permission_model : PermissionModels.t option
    ; auto_deployment : AutoDeployment.t option
    ; client_request_token : String.t option
    }

  let make
      ~stack_set_name
      ?description
      ?template_body
      ?template_u_r_l
      ?(parameters = [])
      ?(capabilities = [])
      ?(tags = [])
      ?administration_role_a_r_n
      ?execution_role_name
      ?permission_model
      ?auto_deployment
      ?client_request_token
      () =
    { stack_set_name
    ; description
    ; template_body
    ; template_u_r_l
    ; parameters
    ; capabilities
    ; tags
    ; administration_role_a_r_n
    ; execution_role_name
    ; permission_model
    ; auto_deployment
    ; client_request_token
    }

  let parse xml =
    Some
      { stack_set_name =
          Aws.Xml.required
            "StackSetName"
            (Aws.Util.option_bind (Aws.Xml.member "StackSetName" xml) String.parse)
      ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
      ; template_body =
          Aws.Util.option_bind (Aws.Xml.member "TemplateBody" xml) String.parse
      ; template_u_r_l =
          Aws.Util.option_bind (Aws.Xml.member "TemplateURL" xml) String.parse
      ; parameters =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Parameters" xml) Parameters.parse)
      ; capabilities =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Capabilities" xml) Capabilities.parse)
      ; tags =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) Tags.parse)
      ; administration_role_a_r_n =
          Aws.Util.option_bind (Aws.Xml.member "AdministrationRoleARN" xml) String.parse
      ; execution_role_name =
          Aws.Util.option_bind (Aws.Xml.member "ExecutionRoleName" xml) String.parse
      ; permission_model =
          Aws.Util.option_bind
            (Aws.Xml.member "PermissionModel" xml)
            PermissionModels.parse
      ; auto_deployment =
          Aws.Util.option_bind (Aws.Xml.member "AutoDeployment" xml) AutoDeployment.parse
      ; client_request_token =
          Aws.Util.option_bind (Aws.Xml.member "ClientRequestToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.client_request_token (fun f ->
               Aws.Query.Pair ("ClientRequestToken", String.to_query f))
         ; Aws.Util.option_map v.auto_deployment (fun f ->
               Aws.Query.Pair ("AutoDeployment", AutoDeployment.to_query f))
         ; Aws.Util.option_map v.permission_model (fun f ->
               Aws.Query.Pair ("PermissionModel", PermissionModels.to_query f))
         ; Aws.Util.option_map v.execution_role_name (fun f ->
               Aws.Query.Pair ("ExecutionRoleName", String.to_query f))
         ; Aws.Util.option_map v.administration_role_a_r_n (fun f ->
               Aws.Query.Pair ("AdministrationRoleARN", String.to_query f))
         ; Some (Aws.Query.Pair ("Tags.member", Tags.to_query v.tags))
         ; Some
             (Aws.Query.Pair ("Capabilities.member", Capabilities.to_query v.capabilities))
         ; Some (Aws.Query.Pair ("Parameters.member", Parameters.to_query v.parameters))
         ; Aws.Util.option_map v.template_u_r_l (fun f ->
               Aws.Query.Pair ("TemplateURL", String.to_query f))
         ; Aws.Util.option_map v.template_body (fun f ->
               Aws.Query.Pair ("TemplateBody", String.to_query f))
         ; Aws.Util.option_map v.description (fun f ->
               Aws.Query.Pair ("Description", String.to_query f))
         ; Some (Aws.Query.Pair ("StackSetName", String.to_query v.stack_set_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.client_request_token (fun f ->
               "ClientRequestToken", String.to_json f)
         ; Aws.Util.option_map v.auto_deployment (fun f ->
               "AutoDeployment", AutoDeployment.to_json f)
         ; Aws.Util.option_map v.permission_model (fun f ->
               "PermissionModel", PermissionModels.to_json f)
         ; Aws.Util.option_map v.execution_role_name (fun f ->
               "ExecutionRoleName", String.to_json f)
         ; Aws.Util.option_map v.administration_role_a_r_n (fun f ->
               "AdministrationRoleARN", String.to_json f)
         ; Some ("Tags", Tags.to_json v.tags)
         ; Some ("Capabilities", Capabilities.to_json v.capabilities)
         ; Some ("Parameters", Parameters.to_json v.parameters)
         ; Aws.Util.option_map v.template_u_r_l (fun f -> "TemplateURL", String.to_json f)
         ; Aws.Util.option_map v.template_body (fun f -> "TemplateBody", String.to_json f)
         ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
         ; Some ("StackSetName", String.to_json v.stack_set_name)
         ])

  let of_json j =
    { stack_set_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackSetName"))
    ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
    ; template_body =
        Aws.Util.option_map (Aws.Json.lookup j "TemplateBody") String.of_json
    ; template_u_r_l =
        Aws.Util.option_map (Aws.Json.lookup j "TemplateURL") String.of_json
    ; parameters =
        Parameters.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Parameters"))
    ; capabilities =
        Capabilities.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Capabilities"))
    ; tags = Tags.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
    ; administration_role_a_r_n =
        Aws.Util.option_map (Aws.Json.lookup j "AdministrationRoleARN") String.of_json
    ; execution_role_name =
        Aws.Util.option_map (Aws.Json.lookup j "ExecutionRoleName") String.of_json
    ; permission_model =
        Aws.Util.option_map (Aws.Json.lookup j "PermissionModel") PermissionModels.of_json
    ; auto_deployment =
        Aws.Util.option_map (Aws.Json.lookup j "AutoDeployment") AutoDeployment.of_json
    ; client_request_token =
        Aws.Util.option_map (Aws.Json.lookup j "ClientRequestToken") String.of_json
    }
end

module ChangeSetStatus = struct
  type t =
    | CREATE_PENDING
    | CREATE_IN_PROGRESS
    | CREATE_COMPLETE
    | DELETE_COMPLETE
    | FAILED

  let str_to_t =
    [ "FAILED", FAILED
    ; "DELETE_COMPLETE", DELETE_COMPLETE
    ; "CREATE_COMPLETE", CREATE_COMPLETE
    ; "CREATE_IN_PROGRESS", CREATE_IN_PROGRESS
    ; "CREATE_PENDING", CREATE_PENDING
    ]

  let t_to_str =
    [ FAILED, "FAILED"
    ; DELETE_COMPLETE, "DELETE_COMPLETE"
    ; CREATE_COMPLETE, "CREATE_COMPLETE"
    ; CREATE_IN_PROGRESS, "CREATE_IN_PROGRESS"
    ; CREATE_PENDING, "CREATE_PENDING"
    ]

  let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)

  let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)

  let make v () = v

  let parse xml =
    Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

  let to_query v =
    Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

  let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))

  let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
end

module ChangeSetSummary = struct
  type t =
    { stack_id : String.t option
    ; stack_name : String.t option
    ; change_set_id : String.t option
    ; change_set_name : String.t option
    ; execution_status : ExecutionStatus.t option
    ; status : ChangeSetStatus.t option
    ; status_reason : String.t option
    ; creation_time : DateTime.t option
    ; description : String.t option
    }

  let make
      ?stack_id
      ?stack_name
      ?change_set_id
      ?change_set_name
      ?execution_status
      ?status
      ?status_reason
      ?creation_time
      ?description
      () =
    { stack_id
    ; stack_name
    ; change_set_id
    ; change_set_name
    ; execution_status
    ; status
    ; status_reason
    ; creation_time
    ; description
    }

  let parse xml =
    Some
      { stack_id = Aws.Util.option_bind (Aws.Xml.member "StackId" xml) String.parse
      ; stack_name = Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse
      ; change_set_id =
          Aws.Util.option_bind (Aws.Xml.member "ChangeSetId" xml) String.parse
      ; change_set_name =
          Aws.Util.option_bind (Aws.Xml.member "ChangeSetName" xml) String.parse
      ; execution_status =
          Aws.Util.option_bind
            (Aws.Xml.member "ExecutionStatus" xml)
            ExecutionStatus.parse
      ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) ChangeSetStatus.parse
      ; status_reason =
          Aws.Util.option_bind (Aws.Xml.member "StatusReason" xml) String.parse
      ; creation_time =
          Aws.Util.option_bind (Aws.Xml.member "CreationTime" xml) DateTime.parse
      ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.description (fun f ->
               Aws.Query.Pair ("Description", String.to_query f))
         ; Aws.Util.option_map v.creation_time (fun f ->
               Aws.Query.Pair ("CreationTime", DateTime.to_query f))
         ; Aws.Util.option_map v.status_reason (fun f ->
               Aws.Query.Pair ("StatusReason", String.to_query f))
         ; Aws.Util.option_map v.status (fun f ->
               Aws.Query.Pair ("Status", ChangeSetStatus.to_query f))
         ; Aws.Util.option_map v.execution_status (fun f ->
               Aws.Query.Pair ("ExecutionStatus", ExecutionStatus.to_query f))
         ; Aws.Util.option_map v.change_set_name (fun f ->
               Aws.Query.Pair ("ChangeSetName", String.to_query f))
         ; Aws.Util.option_map v.change_set_id (fun f ->
               Aws.Query.Pair ("ChangeSetId", String.to_query f))
         ; Aws.Util.option_map v.stack_name (fun f ->
               Aws.Query.Pair ("StackName", String.to_query f))
         ; Aws.Util.option_map v.stack_id (fun f ->
               Aws.Query.Pair ("StackId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
         ; Aws.Util.option_map v.creation_time (fun f ->
               "CreationTime", DateTime.to_json f)
         ; Aws.Util.option_map v.status_reason (fun f -> "StatusReason", String.to_json f)
         ; Aws.Util.option_map v.status (fun f -> "Status", ChangeSetStatus.to_json f)
         ; Aws.Util.option_map v.execution_status (fun f ->
               "ExecutionStatus", ExecutionStatus.to_json f)
         ; Aws.Util.option_map v.change_set_name (fun f ->
               "ChangeSetName", String.to_json f)
         ; Aws.Util.option_map v.change_set_id (fun f -> "ChangeSetId", String.to_json f)
         ; Aws.Util.option_map v.stack_name (fun f -> "StackName", String.to_json f)
         ; Aws.Util.option_map v.stack_id (fun f -> "StackId", String.to_json f)
         ])

  let of_json j =
    { stack_id = Aws.Util.option_map (Aws.Json.lookup j "StackId") String.of_json
    ; stack_name = Aws.Util.option_map (Aws.Json.lookup j "StackName") String.of_json
    ; change_set_id = Aws.Util.option_map (Aws.Json.lookup j "ChangeSetId") String.of_json
    ; change_set_name =
        Aws.Util.option_map (Aws.Json.lookup j "ChangeSetName") String.of_json
    ; execution_status =
        Aws.Util.option_map (Aws.Json.lookup j "ExecutionStatus") ExecutionStatus.of_json
    ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") ChangeSetStatus.of_json
    ; status_reason =
        Aws.Util.option_map (Aws.Json.lookup j "StatusReason") String.of_json
    ; creation_time =
        Aws.Util.option_map (Aws.Json.lookup j "CreationTime") DateTime.of_json
    ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
    }
end

module StackSummaries = struct
  type t = StackSummary.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map StackSummary.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list StackSummary.to_query v

  let to_json v = `List (List.map StackSummary.to_json v)

  let of_json j = Aws.Json.to_list StackSummary.of_json j
end

module ResourcesToSkip = struct
  type t = String.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map String.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list String.to_query v

  let to_json v = `List (List.map String.to_json v)

  let of_json j = Aws.Json.to_list String.of_json j
end

module ContinueUpdateRollbackInput = struct
  type t =
    { stack_name : String.t
    ; role_a_r_n : String.t option
    ; resources_to_skip : ResourcesToSkip.t
    ; client_request_token : String.t option
    }

  let make ~stack_name ?role_a_r_n ?(resources_to_skip = []) ?client_request_token () =
    { stack_name; role_a_r_n; resources_to_skip; client_request_token }

  let parse xml =
    Some
      { stack_name =
          Aws.Xml.required
            "StackName"
            (Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse)
      ; role_a_r_n = Aws.Util.option_bind (Aws.Xml.member "RoleARN" xml) String.parse
      ; resources_to_skip =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ResourcesToSkip" xml)
               ResourcesToSkip.parse)
      ; client_request_token =
          Aws.Util.option_bind (Aws.Xml.member "ClientRequestToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.client_request_token (fun f ->
               Aws.Query.Pair ("ClientRequestToken", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("ResourcesToSkip.member", ResourcesToSkip.to_query v.resources_to_skip))
         ; Aws.Util.option_map v.role_a_r_n (fun f ->
               Aws.Query.Pair ("RoleARN", String.to_query f))
         ; Some (Aws.Query.Pair ("StackName", String.to_query v.stack_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.client_request_token (fun f ->
               "ClientRequestToken", String.to_json f)
         ; Some ("ResourcesToSkip", ResourcesToSkip.to_json v.resources_to_skip)
         ; Aws.Util.option_map v.role_a_r_n (fun f -> "RoleARN", String.to_json f)
         ; Some ("StackName", String.to_json v.stack_name)
         ])

  let of_json j =
    { stack_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackName"))
    ; role_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "RoleARN") String.of_json
    ; resources_to_skip =
        ResourcesToSkip.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourcesToSkip"))
    ; client_request_token =
        Aws.Util.option_map (Aws.Json.lookup j "ClientRequestToken") String.of_json
    }
end

module AlreadyExistsException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module CreateChangeSetInput = struct
  type t =
    { stack_name : String.t
    ; template_body : String.t option
    ; template_u_r_l : String.t option
    ; use_previous_template : Boolean.t option
    ; parameters : Parameters.t
    ; capabilities : Capabilities.t
    ; resource_types : ResourceTypes.t
    ; role_a_r_n : String.t option
    ; rollback_configuration : RollbackConfiguration.t option
    ; notification_a_r_ns : NotificationARNs.t
    ; tags : Tags.t
    ; change_set_name : String.t
    ; client_token : String.t option
    ; description : String.t option
    ; change_set_type : ChangeSetType.t option
    ; resources_to_import : ResourcesToImport.t
    }

  let make
      ~stack_name
      ?template_body
      ?template_u_r_l
      ?use_previous_template
      ?(parameters = [])
      ?(capabilities = [])
      ?(resource_types = [])
      ?role_a_r_n
      ?rollback_configuration
      ?(notification_a_r_ns = [])
      ?(tags = [])
      ~change_set_name
      ?client_token
      ?description
      ?change_set_type
      ?(resources_to_import = [])
      () =
    { stack_name
    ; template_body
    ; template_u_r_l
    ; use_previous_template
    ; parameters
    ; capabilities
    ; resource_types
    ; role_a_r_n
    ; rollback_configuration
    ; notification_a_r_ns
    ; tags
    ; change_set_name
    ; client_token
    ; description
    ; change_set_type
    ; resources_to_import
    }

  let parse xml =
    Some
      { stack_name =
          Aws.Xml.required
            "StackName"
            (Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse)
      ; template_body =
          Aws.Util.option_bind (Aws.Xml.member "TemplateBody" xml) String.parse
      ; template_u_r_l =
          Aws.Util.option_bind (Aws.Xml.member "TemplateURL" xml) String.parse
      ; use_previous_template =
          Aws.Util.option_bind (Aws.Xml.member "UsePreviousTemplate" xml) Boolean.parse
      ; parameters =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Parameters" xml) Parameters.parse)
      ; capabilities =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Capabilities" xml) Capabilities.parse)
      ; resource_types =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ResourceTypes" xml)
               ResourceTypes.parse)
      ; role_a_r_n = Aws.Util.option_bind (Aws.Xml.member "RoleARN" xml) String.parse
      ; rollback_configuration =
          Aws.Util.option_bind
            (Aws.Xml.member "RollbackConfiguration" xml)
            RollbackConfiguration.parse
      ; notification_a_r_ns =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "NotificationARNs" xml)
               NotificationARNs.parse)
      ; tags =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) Tags.parse)
      ; change_set_name =
          Aws.Xml.required
            "ChangeSetName"
            (Aws.Util.option_bind (Aws.Xml.member "ChangeSetName" xml) String.parse)
      ; client_token =
          Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
      ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
      ; change_set_type =
          Aws.Util.option_bind (Aws.Xml.member "ChangeSetType" xml) ChangeSetType.parse
      ; resources_to_import =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "ResourcesToImport" xml)
               ResourcesToImport.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "ResourcesToImport.member"
                , ResourcesToImport.to_query v.resources_to_import ))
         ; Aws.Util.option_map v.change_set_type (fun f ->
               Aws.Query.Pair ("ChangeSetType", ChangeSetType.to_query f))
         ; Aws.Util.option_map v.description (fun f ->
               Aws.Query.Pair ("Description", String.to_query f))
         ; Aws.Util.option_map v.client_token (fun f ->
               Aws.Query.Pair ("ClientToken", String.to_query f))
         ; Some (Aws.Query.Pair ("ChangeSetName", String.to_query v.change_set_name))
         ; Some (Aws.Query.Pair ("Tags.member", Tags.to_query v.tags))
         ; Some
             (Aws.Query.Pair
                ( "NotificationARNs.member"
                , NotificationARNs.to_query v.notification_a_r_ns ))
         ; Aws.Util.option_map v.rollback_configuration (fun f ->
               Aws.Query.Pair ("RollbackConfiguration", RollbackConfiguration.to_query f))
         ; Aws.Util.option_map v.role_a_r_n (fun f ->
               Aws.Query.Pair ("RoleARN", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("ResourceTypes.member", ResourceTypes.to_query v.resource_types))
         ; Some
             (Aws.Query.Pair ("Capabilities.member", Capabilities.to_query v.capabilities))
         ; Some (Aws.Query.Pair ("Parameters.member", Parameters.to_query v.parameters))
         ; Aws.Util.option_map v.use_previous_template (fun f ->
               Aws.Query.Pair ("UsePreviousTemplate", Boolean.to_query f))
         ; Aws.Util.option_map v.template_u_r_l (fun f ->
               Aws.Query.Pair ("TemplateURL", String.to_query f))
         ; Aws.Util.option_map v.template_body (fun f ->
               Aws.Query.Pair ("TemplateBody", String.to_query f))
         ; Some (Aws.Query.Pair ("StackName", String.to_query v.stack_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("ResourcesToImport", ResourcesToImport.to_json v.resources_to_import)
         ; Aws.Util.option_map v.change_set_type (fun f ->
               "ChangeSetType", ChangeSetType.to_json f)
         ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
         ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
         ; Some ("ChangeSetName", String.to_json v.change_set_name)
         ; Some ("Tags", Tags.to_json v.tags)
         ; Some ("NotificationARNs", NotificationARNs.to_json v.notification_a_r_ns)
         ; Aws.Util.option_map v.rollback_configuration (fun f ->
               "RollbackConfiguration", RollbackConfiguration.to_json f)
         ; Aws.Util.option_map v.role_a_r_n (fun f -> "RoleARN", String.to_json f)
         ; Some ("ResourceTypes", ResourceTypes.to_json v.resource_types)
         ; Some ("Capabilities", Capabilities.to_json v.capabilities)
         ; Some ("Parameters", Parameters.to_json v.parameters)
         ; Aws.Util.option_map v.use_previous_template (fun f ->
               "UsePreviousTemplate", Boolean.to_json f)
         ; Aws.Util.option_map v.template_u_r_l (fun f -> "TemplateURL", String.to_json f)
         ; Aws.Util.option_map v.template_body (fun f -> "TemplateBody", String.to_json f)
         ; Some ("StackName", String.to_json v.stack_name)
         ])

  let of_json j =
    { stack_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackName"))
    ; template_body =
        Aws.Util.option_map (Aws.Json.lookup j "TemplateBody") String.of_json
    ; template_u_r_l =
        Aws.Util.option_map (Aws.Json.lookup j "TemplateURL") String.of_json
    ; use_previous_template =
        Aws.Util.option_map (Aws.Json.lookup j "UsePreviousTemplate") Boolean.of_json
    ; parameters =
        Parameters.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Parameters"))
    ; capabilities =
        Capabilities.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Capabilities"))
    ; resource_types =
        ResourceTypes.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceTypes"))
    ; role_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "RoleARN") String.of_json
    ; rollback_configuration =
        Aws.Util.option_map
          (Aws.Json.lookup j "RollbackConfiguration")
          RollbackConfiguration.of_json
    ; notification_a_r_ns =
        NotificationARNs.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "NotificationARNs"))
    ; tags = Tags.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
    ; change_set_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ChangeSetName"))
    ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
    ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
    ; change_set_type =
        Aws.Util.option_map (Aws.Json.lookup j "ChangeSetType") ChangeSetType.of_json
    ; resources_to_import =
        ResourcesToImport.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourcesToImport"))
    }
end

module DeleteStackSetInput = struct
  type t = { stack_set_name : String.t }

  let make ~stack_set_name () = { stack_set_name }

  let parse xml =
    Some
      { stack_set_name =
          Aws.Xml.required
            "StackSetName"
            (Aws.Util.option_bind (Aws.Xml.member "StackSetName" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("StackSetName", String.to_query v.stack_set_name)) ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("StackSetName", String.to_json v.stack_set_name) ])

  let of_json j =
    { stack_set_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackSetName"))
    }
end

module DeleteStackInstancesInput = struct
  type t =
    { stack_set_name : String.t
    ; accounts : AccountList.t
    ; deployment_targets : DeploymentTargets.t option
    ; regions : RegionList.t
    ; operation_preferences : StackSetOperationPreferences.t option
    ; retain_stacks : Boolean.t
    ; operation_id : String.t option
    }

  let make
      ~stack_set_name
      ?(accounts = [])
      ?deployment_targets
      ~regions
      ?operation_preferences
      ~retain_stacks
      ?operation_id
      () =
    { stack_set_name
    ; accounts
    ; deployment_targets
    ; regions
    ; operation_preferences
    ; retain_stacks
    ; operation_id
    }

  let parse xml =
    Some
      { stack_set_name =
          Aws.Xml.required
            "StackSetName"
            (Aws.Util.option_bind (Aws.Xml.member "StackSetName" xml) String.parse)
      ; accounts =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Accounts" xml) AccountList.parse)
      ; deployment_targets =
          Aws.Util.option_bind
            (Aws.Xml.member "DeploymentTargets" xml)
            DeploymentTargets.parse
      ; regions =
          Aws.Xml.required
            "Regions"
            (Aws.Util.option_bind (Aws.Xml.member "Regions" xml) RegionList.parse)
      ; operation_preferences =
          Aws.Util.option_bind
            (Aws.Xml.member "OperationPreferences" xml)
            StackSetOperationPreferences.parse
      ; retain_stacks =
          Aws.Xml.required
            "RetainStacks"
            (Aws.Util.option_bind (Aws.Xml.member "RetainStacks" xml) Boolean.parse)
      ; operation_id =
          Aws.Util.option_bind (Aws.Xml.member "OperationId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.operation_id (fun f ->
               Aws.Query.Pair ("OperationId", String.to_query f))
         ; Some (Aws.Query.Pair ("RetainStacks", Boolean.to_query v.retain_stacks))
         ; Aws.Util.option_map v.operation_preferences (fun f ->
               Aws.Query.Pair
                 ("OperationPreferences", StackSetOperationPreferences.to_query f))
         ; Some (Aws.Query.Pair ("Regions.member", RegionList.to_query v.regions))
         ; Aws.Util.option_map v.deployment_targets (fun f ->
               Aws.Query.Pair ("DeploymentTargets", DeploymentTargets.to_query f))
         ; Some (Aws.Query.Pair ("Accounts.member", AccountList.to_query v.accounts))
         ; Some (Aws.Query.Pair ("StackSetName", String.to_query v.stack_set_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.operation_id (fun f -> "OperationId", String.to_json f)
         ; Some ("RetainStacks", Boolean.to_json v.retain_stacks)
         ; Aws.Util.option_map v.operation_preferences (fun f ->
               "OperationPreferences", StackSetOperationPreferences.to_json f)
         ; Some ("Regions", RegionList.to_json v.regions)
         ; Aws.Util.option_map v.deployment_targets (fun f ->
               "DeploymentTargets", DeploymentTargets.to_json f)
         ; Some ("Accounts", AccountList.to_json v.accounts)
         ; Some ("StackSetName", String.to_json v.stack_set_name)
         ])

  let of_json j =
    { stack_set_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackSetName"))
    ; accounts =
        AccountList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Accounts"))
    ; deployment_targets =
        Aws.Util.option_map
          (Aws.Json.lookup j "DeploymentTargets")
          DeploymentTargets.of_json
    ; regions = RegionList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Regions"))
    ; operation_preferences =
        Aws.Util.option_map
          (Aws.Json.lookup j "OperationPreferences")
          StackSetOperationPreferences.of_json
    ; retain_stacks =
        Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RetainStacks"))
    ; operation_id = Aws.Util.option_map (Aws.Json.lookup j "OperationId") String.of_json
    }
end

module OperationIdAlreadyExistsException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module DeleteStackInput = struct
  type t =
    { stack_name : String.t
    ; retain_resources : RetainResources.t
    ; role_a_r_n : String.t option
    ; client_request_token : String.t option
    }

  let make ~stack_name ?(retain_resources = []) ?role_a_r_n ?client_request_token () =
    { stack_name; retain_resources; role_a_r_n; client_request_token }

  let parse xml =
    Some
      { stack_name =
          Aws.Xml.required
            "StackName"
            (Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse)
      ; retain_resources =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "RetainResources" xml)
               RetainResources.parse)
      ; role_a_r_n = Aws.Util.option_bind (Aws.Xml.member "RoleARN" xml) String.parse
      ; client_request_token =
          Aws.Util.option_bind (Aws.Xml.member "ClientRequestToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.client_request_token (fun f ->
               Aws.Query.Pair ("ClientRequestToken", String.to_query f))
         ; Aws.Util.option_map v.role_a_r_n (fun f ->
               Aws.Query.Pair ("RoleARN", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("RetainResources.member", RetainResources.to_query v.retain_resources))
         ; Some (Aws.Query.Pair ("StackName", String.to_query v.stack_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.client_request_token (fun f ->
               "ClientRequestToken", String.to_json f)
         ; Aws.Util.option_map v.role_a_r_n (fun f -> "RoleARN", String.to_json f)
         ; Some ("RetainResources", RetainResources.to_json v.retain_resources)
         ; Some ("StackName", String.to_json v.stack_name)
         ])

  let of_json j =
    { stack_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackName"))
    ; retain_resources =
        RetainResources.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "RetainResources"))
    ; role_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "RoleARN") String.of_json
    ; client_request_token =
        Aws.Util.option_map (Aws.Json.lookup j "ClientRequestToken") String.of_json
    }
end

module ListExportsOutput = struct
  type t =
    { exports : Exports.t
    ; next_token : String.t option
    }

  let make ?(exports = []) ?next_token () = { exports; next_token }

  let parse xml =
    Some
      { exports =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Exports" xml) Exports.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some (Aws.Query.Pair ("Exports.member", Exports.to_query v.exports))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("Exports", Exports.to_json v.exports)
         ])

  let of_json j =
    { exports = Exports.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Exports"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module ListStackSetOperationsInput = struct
  type t =
    { stack_set_name : String.t
    ; next_token : String.t option
    ; max_results : Integer.t option
    }

  let make ~stack_set_name ?next_token ?max_results () =
    { stack_set_name; next_token; max_results }

  let parse xml =
    Some
      { stack_set_name =
          Aws.Xml.required
            "StackSetName"
            (Aws.Util.option_bind (Aws.Xml.member "StackSetName" xml) String.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_results (fun f ->
               Aws.Query.Pair ("MaxResults", Integer.to_query f))
         ; Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some (Aws.Query.Pair ("StackSetName", String.to_query v.stack_set_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
         ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("StackSetName", String.to_json v.stack_set_name)
         ])

  let of_json j =
    { stack_set_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackSetName"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
    }
end

module ChangeSetSummaries = struct
  type t = ChangeSetSummary.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map ChangeSetSummary.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list ChangeSetSummary.to_query v

  let to_json v = `List (List.map ChangeSetSummary.to_json v)

  let of_json j = Aws.Json.to_list ChangeSetSummary.of_json j
end

module ListChangeSetsOutput = struct
  type t =
    { summaries : ChangeSetSummaries.t
    ; next_token : String.t option
    }

  let make ?(summaries = []) ?next_token () = { summaries; next_token }

  let parse xml =
    Some
      { summaries =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "Summaries" xml)
               ChangeSetSummaries.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair ("Summaries.member", ChangeSetSummaries.to_query v.summaries))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("Summaries", ChangeSetSummaries.to_json v.summaries)
         ])

  let of_json j =
    { summaries =
        ChangeSetSummaries.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Summaries"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module TypeNotFoundException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module DescribeStackSetOperationInput = struct
  type t =
    { stack_set_name : String.t
    ; operation_id : String.t
    }

  let make ~stack_set_name ~operation_id () = { stack_set_name; operation_id }

  let parse xml =
    Some
      { stack_set_name =
          Aws.Xml.required
            "StackSetName"
            (Aws.Util.option_bind (Aws.Xml.member "StackSetName" xml) String.parse)
      ; operation_id =
          Aws.Xml.required
            "OperationId"
            (Aws.Util.option_bind (Aws.Xml.member "OperationId" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("OperationId", String.to_query v.operation_id))
         ; Some (Aws.Query.Pair ("StackSetName", String.to_query v.stack_set_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("OperationId", String.to_json v.operation_id)
         ; Some ("StackSetName", String.to_json v.stack_set_name)
         ])

  let of_json j =
    { stack_set_name =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackSetName"))
    ; operation_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "OperationId"))
    }
end

module InsufficientCapabilitiesException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module OperationNotFoundException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module StackResourceDrifts = struct
  type t = StackResourceDrift.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map StackResourceDrift.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list StackResourceDrift.to_query v

  let to_json v = `List (List.map StackResourceDrift.to_json v)

  let of_json j = Aws.Json.to_list StackResourceDrift.of_json j
end

module StackInstanceNotFoundException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ListStackSetsInput = struct
  type t =
    { next_token : String.t option
    ; max_results : Integer.t option
    ; status : StackSetStatus.t option
    }

  let make ?next_token ?max_results ?status () = { next_token; max_results; status }

  let parse xml =
    Some
      { next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
      ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) StackSetStatus.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.status (fun f ->
               Aws.Query.Pair ("Status", StackSetStatus.to_query f))
         ; Aws.Util.option_map v.max_results (fun f ->
               Aws.Query.Pair ("MaxResults", Integer.to_query f))
         ; Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.status (fun f -> "Status", StackSetStatus.to_json f)
         ; Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
         ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ])

  let of_json j =
    { next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
    ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") StackSetStatus.of_json
    }
end

module GetStackPolicyOutput = struct
  type t = { stack_policy_body : String.t option }

  let make ?stack_policy_body () = { stack_policy_body }

  let parse xml =
    Some
      { stack_policy_body =
          Aws.Util.option_bind (Aws.Xml.member "StackPolicyBody" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.stack_policy_body (fun f ->
               Aws.Query.Pair ("StackPolicyBody", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.stack_policy_body (fun f ->
               "StackPolicyBody", String.to_json f)
         ])

  let of_json j =
    { stack_policy_body =
        Aws.Util.option_map (Aws.Json.lookup j "StackPolicyBody") String.of_json
    }
end

module GetStackPolicyInput = struct
  type t = { stack_name : String.t }

  let make ~stack_name () = { stack_name }

  let parse xml =
    Some
      { stack_name =
          Aws.Xml.required
            "StackName"
            (Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some (Aws.Query.Pair ("StackName", String.to_query v.stack_name)) ])

  let to_json v =
    `Assoc (Aws.Util.list_filter_opt [ Some ("StackName", String.to_json v.stack_name) ])

  let of_json j =
    { stack_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackName"))
    }
end

module Changes = struct
  type t = Change.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all (List.map Change.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list Change.to_query v

  let to_json v = `List (List.map Change.to_json v)

  let of_json j = Aws.Json.to_list Change.of_json j
end

module DescribeChangeSetOutput = struct
  type t =
    { change_set_name : String.t option
    ; change_set_id : String.t option
    ; stack_id : String.t option
    ; stack_name : String.t option
    ; description : String.t option
    ; parameters : Parameters.t
    ; creation_time : DateTime.t option
    ; execution_status : ExecutionStatus.t option
    ; status : ChangeSetStatus.t option
    ; status_reason : String.t option
    ; notification_a_r_ns : NotificationARNs.t
    ; rollback_configuration : RollbackConfiguration.t option
    ; capabilities : Capabilities.t
    ; tags : Tags.t
    ; changes : Changes.t
    ; next_token : String.t option
    }

  let make
      ?change_set_name
      ?change_set_id
      ?stack_id
      ?stack_name
      ?description
      ?(parameters = [])
      ?creation_time
      ?execution_status
      ?status
      ?status_reason
      ?(notification_a_r_ns = [])
      ?rollback_configuration
      ?(capabilities = [])
      ?(tags = [])
      ?(changes = [])
      ?next_token
      () =
    { change_set_name
    ; change_set_id
    ; stack_id
    ; stack_name
    ; description
    ; parameters
    ; creation_time
    ; execution_status
    ; status
    ; status_reason
    ; notification_a_r_ns
    ; rollback_configuration
    ; capabilities
    ; tags
    ; changes
    ; next_token
    }

  let parse xml =
    Some
      { change_set_name =
          Aws.Util.option_bind (Aws.Xml.member "ChangeSetName" xml) String.parse
      ; change_set_id =
          Aws.Util.option_bind (Aws.Xml.member "ChangeSetId" xml) String.parse
      ; stack_id = Aws.Util.option_bind (Aws.Xml.member "StackId" xml) String.parse
      ; stack_name = Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse
      ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
      ; parameters =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Parameters" xml) Parameters.parse)
      ; creation_time =
          Aws.Util.option_bind (Aws.Xml.member "CreationTime" xml) DateTime.parse
      ; execution_status =
          Aws.Util.option_bind
            (Aws.Xml.member "ExecutionStatus" xml)
            ExecutionStatus.parse
      ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) ChangeSetStatus.parse
      ; status_reason =
          Aws.Util.option_bind (Aws.Xml.member "StatusReason" xml) String.parse
      ; notification_a_r_ns =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "NotificationARNs" xml)
               NotificationARNs.parse)
      ; rollback_configuration =
          Aws.Util.option_bind
            (Aws.Xml.member "RollbackConfiguration" xml)
            RollbackConfiguration.parse
      ; capabilities =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Capabilities" xml) Capabilities.parse)
      ; tags =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) Tags.parse)
      ; changes =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind (Aws.Xml.member "Changes" xml) Changes.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some (Aws.Query.Pair ("Changes.member", Changes.to_query v.changes))
         ; Some (Aws.Query.Pair ("Tags.member", Tags.to_query v.tags))
         ; Some
             (Aws.Query.Pair ("Capabilities.member", Capabilities.to_query v.capabilities))
         ; Aws.Util.option_map v.rollback_configuration (fun f ->
               Aws.Query.Pair ("RollbackConfiguration", RollbackConfiguration.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "NotificationARNs.member"
                , NotificationARNs.to_query v.notification_a_r_ns ))
         ; Aws.Util.option_map v.status_reason (fun f ->
               Aws.Query.Pair ("StatusReason", String.to_query f))
         ; Aws.Util.option_map v.status (fun f ->
               Aws.Query.Pair ("Status", ChangeSetStatus.to_query f))
         ; Aws.Util.option_map v.execution_status (fun f ->
               Aws.Query.Pair ("ExecutionStatus", ExecutionStatus.to_query f))
         ; Aws.Util.option_map v.creation_time (fun f ->
               Aws.Query.Pair ("CreationTime", DateTime.to_query f))
         ; Some (Aws.Query.Pair ("Parameters.member", Parameters.to_query v.parameters))
         ; Aws.Util.option_map v.description (fun f ->
               Aws.Query.Pair ("Description", String.to_query f))
         ; Aws.Util.option_map v.stack_name (fun f ->
               Aws.Query.Pair ("StackName", String.to_query f))
         ; Aws.Util.option_map v.stack_id (fun f ->
               Aws.Query.Pair ("StackId", String.to_query f))
         ; Aws.Util.option_map v.change_set_id (fun f ->
               Aws.Query.Pair ("ChangeSetId", String.to_query f))
         ; Aws.Util.option_map v.change_set_name (fun f ->
               Aws.Query.Pair ("ChangeSetName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("Changes", Changes.to_json v.changes)
         ; Some ("Tags", Tags.to_json v.tags)
         ; Some ("Capabilities", Capabilities.to_json v.capabilities)
         ; Aws.Util.option_map v.rollback_configuration (fun f ->
               "RollbackConfiguration", RollbackConfiguration.to_json f)
         ; Some ("NotificationARNs", NotificationARNs.to_json v.notification_a_r_ns)
         ; Aws.Util.option_map v.status_reason (fun f -> "StatusReason", String.to_json f)
         ; Aws.Util.option_map v.status (fun f -> "Status", ChangeSetStatus.to_json f)
         ; Aws.Util.option_map v.execution_status (fun f ->
               "ExecutionStatus", ExecutionStatus.to_json f)
         ; Aws.Util.option_map v.creation_time (fun f ->
               "CreationTime", DateTime.to_json f)
         ; Some ("Parameters", Parameters.to_json v.parameters)
         ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
         ; Aws.Util.option_map v.stack_name (fun f -> "StackName", String.to_json f)
         ; Aws.Util.option_map v.stack_id (fun f -> "StackId", String.to_json f)
         ; Aws.Util.option_map v.change_set_id (fun f -> "ChangeSetId", String.to_json f)
         ; Aws.Util.option_map v.change_set_name (fun f ->
               "ChangeSetName", String.to_json f)
         ])

  let of_json j =
    { change_set_name =
        Aws.Util.option_map (Aws.Json.lookup j "ChangeSetName") String.of_json
    ; change_set_id = Aws.Util.option_map (Aws.Json.lookup j "ChangeSetId") String.of_json
    ; stack_id = Aws.Util.option_map (Aws.Json.lookup j "StackId") String.of_json
    ; stack_name = Aws.Util.option_map (Aws.Json.lookup j "StackName") String.of_json
    ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
    ; parameters =
        Parameters.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Parameters"))
    ; creation_time =
        Aws.Util.option_map (Aws.Json.lookup j "CreationTime") DateTime.of_json
    ; execution_status =
        Aws.Util.option_map (Aws.Json.lookup j "ExecutionStatus") ExecutionStatus.of_json
    ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") ChangeSetStatus.of_json
    ; status_reason =
        Aws.Util.option_map (Aws.Json.lookup j "StatusReason") String.of_json
    ; notification_a_r_ns =
        NotificationARNs.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "NotificationARNs"))
    ; rollback_configuration =
        Aws.Util.option_map
          (Aws.Json.lookup j "RollbackConfiguration")
          RollbackConfiguration.of_json
    ; capabilities =
        Capabilities.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Capabilities"))
    ; tags = Tags.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
    ; changes = Changes.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Changes"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module DescribeAccountLimitsInput = struct
  type t = { next_token : String.t option }

  let make ?next_token () = { next_token }

  let parse xml =
    Some
      { next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f) ])

  let of_json j =
    { next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json }
end

module StaleRequestException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module ListStacksInput = struct
  type t =
    { next_token : String.t option
    ; stack_status_filter : StackStatusFilter.t
    }

  let make ?next_token ?(stack_status_filter = []) () =
    { next_token; stack_status_filter }

  let parse xml =
    Some
      { next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      ; stack_status_filter =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "StackStatusFilter" xml)
               StackStatusFilter.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair
                ( "StackStatusFilter.member"
                , StackStatusFilter.to_query v.stack_status_filter ))
         ; Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("StackStatusFilter", StackStatusFilter.to_json v.stack_status_filter)
         ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ])

  let of_json j =
    { next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    ; stack_status_filter =
        StackStatusFilter.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "StackStatusFilter"))
    }
end

module DeleteStackInstancesOutput = struct
  type t = { operation_id : String.t option }

  let make ?operation_id () = { operation_id }

  let parse xml =
    Some
      { operation_id =
          Aws.Util.option_bind (Aws.Xml.member "OperationId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.operation_id (fun f ->
               Aws.Query.Pair ("OperationId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.operation_id (fun f -> "OperationId", String.to_json f) ])

  let of_json j =
    { operation_id = Aws.Util.option_map (Aws.Json.lookup j "OperationId") String.of_json
    }
end

module GetTemplateInput = struct
  type t =
    { stack_name : String.t option
    ; change_set_name : String.t option
    ; template_stage : TemplateStage.t option
    }

  let make ?stack_name ?change_set_name ?template_stage () =
    { stack_name; change_set_name; template_stage }

  let parse xml =
    Some
      { stack_name = Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse
      ; change_set_name =
          Aws.Util.option_bind (Aws.Xml.member "ChangeSetName" xml) String.parse
      ; template_stage =
          Aws.Util.option_bind (Aws.Xml.member "TemplateStage" xml) TemplateStage.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.template_stage (fun f ->
               Aws.Query.Pair ("TemplateStage", TemplateStage.to_query f))
         ; Aws.Util.option_map v.change_set_name (fun f ->
               Aws.Query.Pair ("ChangeSetName", String.to_query f))
         ; Aws.Util.option_map v.stack_name (fun f ->
               Aws.Query.Pair ("StackName", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.template_stage (fun f ->
               "TemplateStage", TemplateStage.to_json f)
         ; Aws.Util.option_map v.change_set_name (fun f ->
               "ChangeSetName", String.to_json f)
         ; Aws.Util.option_map v.stack_name (fun f -> "StackName", String.to_json f)
         ])

  let of_json j =
    { stack_name = Aws.Util.option_map (Aws.Json.lookup j "StackName") String.of_json
    ; change_set_name =
        Aws.Util.option_map (Aws.Json.lookup j "ChangeSetName") String.of_json
    ; template_stage =
        Aws.Util.option_map (Aws.Json.lookup j "TemplateStage") TemplateStage.of_json
    }
end

module DescribeStackResourceDriftsOutput = struct
  type t =
    { stack_resource_drifts : StackResourceDrifts.t
    ; next_token : String.t option
    }

  let make ~stack_resource_drifts ?next_token () = { stack_resource_drifts; next_token }

  let parse xml =
    Some
      { stack_resource_drifts =
          Aws.Xml.required
            "StackResourceDrifts"
            (Aws.Util.option_bind
               (Aws.Xml.member "StackResourceDrifts" xml)
               StackResourceDrifts.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "StackResourceDrifts.member"
                , StackResourceDrifts.to_query v.stack_resource_drifts ))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some
             ("StackResourceDrifts", StackResourceDrifts.to_json v.stack_resource_drifts)
         ])

  let of_json j =
    { stack_resource_drifts =
        StackResourceDrifts.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "StackResourceDrifts"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module ListStacksOutput = struct
  type t =
    { stack_summaries : StackSummaries.t
    ; next_token : String.t option
    }

  let make ?(stack_summaries = []) ?next_token () = { stack_summaries; next_token }

  let parse xml =
    Some
      { stack_summaries =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "StackSummaries" xml)
               StackSummaries.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("StackSummaries.member", StackSummaries.to_query v.stack_summaries))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("StackSummaries", StackSummaries.to_json v.stack_summaries)
         ])

  let of_json j =
    { stack_summaries =
        StackSummaries.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "StackSummaries"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module CreateChangeSetOutput = struct
  type t =
    { id : String.t option
    ; stack_id : String.t option
    }

  let make ?id ?stack_id () = { id; stack_id }

  let parse xml =
    Some
      { id = Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse
      ; stack_id = Aws.Util.option_bind (Aws.Xml.member "StackId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.stack_id (fun f ->
               Aws.Query.Pair ("StackId", String.to_query f))
         ; Aws.Util.option_map v.id (fun f -> Aws.Query.Pair ("Id", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.stack_id (fun f -> "StackId", String.to_json f)
         ; Aws.Util.option_map v.id (fun f -> "Id", String.to_json f)
         ])

  let of_json j =
    { id = Aws.Util.option_map (Aws.Json.lookup j "Id") String.of_json
    ; stack_id = Aws.Util.option_map (Aws.Json.lookup j "StackId") String.of_json
    }
end

module DescribeStackResourceInput = struct
  type t =
    { stack_name : String.t
    ; logical_resource_id : String.t
    }

  let make ~stack_name ~logical_resource_id () = { stack_name; logical_resource_id }

  let parse xml =
    Some
      { stack_name =
          Aws.Xml.required
            "StackName"
            (Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse)
      ; logical_resource_id =
          Aws.Xml.required
            "LogicalResourceId"
            (Aws.Util.option_bind (Aws.Xml.member "LogicalResourceId" xml) String.parse)
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Some
             (Aws.Query.Pair ("LogicalResourceId", String.to_query v.logical_resource_id))
         ; Some (Aws.Query.Pair ("StackName", String.to_query v.stack_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Some ("LogicalResourceId", String.to_json v.logical_resource_id)
         ; Some ("StackName", String.to_json v.stack_name)
         ])

  let of_json j =
    { stack_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackName"))
    ; logical_resource_id =
        String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LogicalResourceId"))
    }
end

module DetectStackSetDriftOutput = struct
  type t = { operation_id : String.t option }

  let make ?operation_id () = { operation_id }

  let parse xml =
    Some
      { operation_id =
          Aws.Util.option_bind (Aws.Xml.member "OperationId" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.operation_id (fun f ->
               Aws.Query.Pair ("OperationId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.operation_id (fun f -> "OperationId", String.to_json f) ])

  let of_json j =
    { operation_id = Aws.Util.option_map (Aws.Json.lookup j "OperationId") String.of_json
    }
end

module CreateStackOutput = struct
  type t = { stack_id : String.t option }

  let make ?stack_id () = { stack_id }

  let parse xml =
    Some { stack_id = Aws.Util.option_bind (Aws.Xml.member "StackId" xml) String.parse }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.stack_id (fun f ->
               Aws.Query.Pair ("StackId", String.to_query f))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.stack_id (fun f -> "StackId", String.to_json f) ])

  let of_json j =
    { stack_id = Aws.Util.option_map (Aws.Json.lookup j "StackId") String.of_json }
end

module DescribeAccountLimitsOutput = struct
  type t =
    { account_limits : AccountLimitList.t
    ; next_token : String.t option
    }

  let make ?(account_limits = []) ?next_token () = { account_limits; next_token }

  let parse xml =
    Some
      { account_limits =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "AccountLimits" xml)
               AccountLimitList.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("AccountLimits.member", AccountLimitList.to_query v.account_limits))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("AccountLimits", AccountLimitList.to_json v.account_limits)
         ])

  let of_json j =
    { account_limits =
        AccountLimitList.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "AccountLimits"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module ListStackSetOperationResultsOutput = struct
  type t =
    { summaries : StackSetOperationResultSummaries.t
    ; next_token : String.t option
    }

  let make ?(summaries = []) ?next_token () = { summaries; next_token }

  let parse xml =
    Some
      { summaries =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "Summaries" xml)
               StackSetOperationResultSummaries.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ("Summaries.member", StackSetOperationResultSummaries.to_query v.summaries))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some ("Summaries", StackSetOperationResultSummaries.to_json v.summaries)
         ])

  let of_json j =
    { summaries =
        StackSetOperationResultSummaries.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "Summaries"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    }
end

module NameAlreadyExistsException = struct
  type t = unit

  let make () = ()

  let parse xml = Some ()

  let to_query v = Aws.Query.List (Aws.Util.list_filter_opt [])

  let to_json v = `Assoc (Aws.Util.list_filter_opt [])

  let of_json j = ()
end

module StackResourceDriftStatusFilters = struct
  type t = StackResourceDriftStatus.t list

  let make elems () = elems

  let parse xml =
    Aws.Util.option_all
      (List.map StackResourceDriftStatus.parse (Aws.Xml.members "member" xml))

  let to_query v = Aws.Query.to_query_list StackResourceDriftStatus.to_query v

  let to_json v = `List (List.map StackResourceDriftStatus.to_json v)

  let of_json j = Aws.Json.to_list StackResourceDriftStatus.of_json j
end

module DescribeStackResourceDriftsInput = struct
  type t =
    { stack_name : String.t
    ; stack_resource_drift_status_filters : StackResourceDriftStatusFilters.t
    ; next_token : String.t option
    ; max_results : Integer.t option
    }

  let make
      ~stack_name
      ?(stack_resource_drift_status_filters = [])
      ?next_token
      ?max_results
      () =
    { stack_name; stack_resource_drift_status_filters; next_token; max_results }

  let parse xml =
    Some
      { stack_name =
          Aws.Xml.required
            "StackName"
            (Aws.Util.option_bind (Aws.Xml.member "StackName" xml) String.parse)
      ; stack_resource_drift_status_filters =
          Aws.Util.of_option
            []
            (Aws.Util.option_bind
               (Aws.Xml.member "StackResourceDriftStatusFilters" xml)
               StackResourceDriftStatusFilters.parse)
      ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
      ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
      }

  let to_query v =
    Aws.Query.List
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_results (fun f ->
               Aws.Query.Pair ("MaxResults", Integer.to_query f))
         ; Aws.Util.option_map v.next_token (fun f ->
               Aws.Query.Pair ("NextToken", String.to_query f))
         ; Some
             (Aws.Query.Pair
                ( "StackResourceDriftStatusFilters.member"
                , StackResourceDriftStatusFilters.to_query
                    v.stack_resource_drift_status_filters ))
         ; Some (Aws.Query.Pair ("StackName", String.to_query v.stack_name))
         ])

  let to_json v =
    `Assoc
      (Aws.Util.list_filter_opt
         [ Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
         ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
         ; Some
             ( "StackResourceDriftStatusFilters"
             , StackResourceDriftStatusFilters.to_json
                 v.stack_resource_drift_status_filters )
         ; Some ("StackName", String.to_json v.stack_name)
         ])

  let of_json j =
    { stack_name = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackName"))
    ; stack_resource_drift_status_filters =
        StackResourceDriftStatusFilters.of_json
          (Aws.Util.of_option_exn (Aws.Json.lookup j "StackResourceDriftStatusFilters"))
    ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
    ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
    }
end
