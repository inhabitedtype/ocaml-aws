open Aws
open Aws.BaseTypes
open CalendarLib
type calendar = Calendar.t
module AvailabilityZone =
  struct
    type t = {
      name: String.t option }
    let make ?name  () = { name }
    let parse xml =
      Some { name = (Util.option_bind (Xml.member "Name" xml) String.parse) }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.name
              (fun f -> Query.Pair ("Name", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.name (fun f -> ("name", (String.to_json f)))])
    let of_json j =
      { name = (Util.option_map (Json.lookup j "name") String.of_json) }
  end
module DBSecurityGroupMembership =
  struct
    type t =
      {
      d_b_security_group_name: String.t option ;
      status: String.t option }
    let make ?d_b_security_group_name  ?status  () =
      { d_b_security_group_name; status }
    let parse xml =
      Some
        {
          d_b_security_group_name =
            (Util.option_bind (Xml.member "DBSecurityGroupName" xml)
               String.parse);
          status = (Util.option_bind (Xml.member "Status" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.status
              (fun f -> Query.Pair ("Status", (String.to_query f)));
           Util.option_map v.d_b_security_group_name
             (fun f ->
                Query.Pair ("DBSecurityGroupName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.status
              (fun f -> ("status", (String.to_json f)));
           Util.option_map v.d_b_security_group_name
             (fun f -> ("d_b_security_group_name", (String.to_json f)))])
    let of_json j =
      {
        d_b_security_group_name =
          (Util.option_map (Json.lookup j "d_b_security_group_name")
             String.of_json);
        status = (Util.option_map (Json.lookup j "status") String.of_json)
      }
  end
module OptionSetting =
  struct
    type t =
      {
      name: String.t option ;
      value: String.t option ;
      default_value: String.t option ;
      description: String.t option ;
      apply_type: String.t option ;
      data_type: String.t option ;
      allowed_values: String.t option ;
      is_modifiable: Boolean.t option ;
      is_collection: Boolean.t option }
    let make ?name  ?value  ?default_value  ?description  ?apply_type 
      ?data_type  ?allowed_values  ?is_modifiable  ?is_collection  () =
      {
        name;
        value;
        default_value;
        description;
        apply_type;
        data_type;
        allowed_values;
        is_modifiable;
        is_collection
      }
    let parse xml =
      Some
        {
          name = (Util.option_bind (Xml.member "Name" xml) String.parse);
          value = (Util.option_bind (Xml.member "Value" xml) String.parse);
          default_value =
            (Util.option_bind (Xml.member "DefaultValue" xml) String.parse);
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse);
          apply_type =
            (Util.option_bind (Xml.member "ApplyType" xml) String.parse);
          data_type =
            (Util.option_bind (Xml.member "DataType" xml) String.parse);
          allowed_values =
            (Util.option_bind (Xml.member "AllowedValues" xml) String.parse);
          is_modifiable =
            (Util.option_bind (Xml.member "IsModifiable" xml) Boolean.parse);
          is_collection =
            (Util.option_bind (Xml.member "IsCollection" xml) Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.is_collection
              (fun f -> Query.Pair ("IsCollection", (Boolean.to_query f)));
           Util.option_map v.is_modifiable
             (fun f -> Query.Pair ("IsModifiable", (Boolean.to_query f)));
           Util.option_map v.allowed_values
             (fun f -> Query.Pair ("AllowedValues", (String.to_query f)));
           Util.option_map v.data_type
             (fun f -> Query.Pair ("DataType", (String.to_query f)));
           Util.option_map v.apply_type
             (fun f -> Query.Pair ("ApplyType", (String.to_query f)));
           Util.option_map v.description
             (fun f -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.default_value
             (fun f -> Query.Pair ("DefaultValue", (String.to_query f)));
           Util.option_map v.value
             (fun f -> Query.Pair ("Value", (String.to_query f)));
           Util.option_map v.name
             (fun f -> Query.Pair ("Name", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.is_collection
              (fun f -> ("is_collection", (Boolean.to_json f)));
           Util.option_map v.is_modifiable
             (fun f -> ("is_modifiable", (Boolean.to_json f)));
           Util.option_map v.allowed_values
             (fun f -> ("allowed_values", (String.to_json f)));
           Util.option_map v.data_type
             (fun f -> ("data_type", (String.to_json f)));
           Util.option_map v.apply_type
             (fun f -> ("apply_type", (String.to_json f)));
           Util.option_map v.description
             (fun f -> ("description", (String.to_json f)));
           Util.option_map v.default_value
             (fun f -> ("default_value", (String.to_json f)));
           Util.option_map v.value (fun f -> ("value", (String.to_json f)));
           Util.option_map v.name (fun f -> ("name", (String.to_json f)))])
    let of_json j =
      {
        name = (Util.option_map (Json.lookup j "name") String.of_json);
        value = (Util.option_map (Json.lookup j "value") String.of_json);
        default_value =
          (Util.option_map (Json.lookup j "default_value") String.of_json);
        description =
          (Util.option_map (Json.lookup j "description") String.of_json);
        apply_type =
          (Util.option_map (Json.lookup j "apply_type") String.of_json);
        data_type =
          (Util.option_map (Json.lookup j "data_type") String.of_json);
        allowed_values =
          (Util.option_map (Json.lookup j "allowed_values") String.of_json);
        is_modifiable =
          (Util.option_map (Json.lookup j "is_modifiable") Boolean.of_json);
        is_collection =
          (Util.option_map (Json.lookup j "is_collection") Boolean.of_json)
      }
  end
module VpcSecurityGroupMembership =
  struct
    type t =
      {
      vpc_security_group_id: String.t option ;
      status: String.t option }
    let make ?vpc_security_group_id  ?status  () =
      { vpc_security_group_id; status }
    let parse xml =
      Some
        {
          vpc_security_group_id =
            (Util.option_bind (Xml.member "VpcSecurityGroupId" xml)
               String.parse);
          status = (Util.option_bind (Xml.member "Status" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.status
              (fun f -> Query.Pair ("Status", (String.to_query f)));
           Util.option_map v.vpc_security_group_id
             (fun f -> Query.Pair ("VpcSecurityGroupId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.status
              (fun f -> ("status", (String.to_json f)));
           Util.option_map v.vpc_security_group_id
             (fun f -> ("vpc_security_group_id", (String.to_json f)))])
    let of_json j =
      {
        vpc_security_group_id =
          (Util.option_map (Json.lookup j "vpc_security_group_id")
             String.of_json);
        status = (Util.option_map (Json.lookup j "status") String.of_json)
      }
  end
module Subnet =
  struct
    type t =
      {
      subnet_identifier: String.t option ;
      subnet_availability_zone: AvailabilityZone.t option ;
      subnet_status: String.t option }
    let make ?subnet_identifier  ?subnet_availability_zone  ?subnet_status 
      () = { subnet_identifier; subnet_availability_zone; subnet_status }
    let parse xml =
      Some
        {
          subnet_identifier =
            (Util.option_bind (Xml.member "SubnetIdentifier" xml)
               String.parse);
          subnet_availability_zone =
            (Util.option_bind (Xml.member "SubnetAvailabilityZone" xml)
               AvailabilityZone.parse);
          subnet_status =
            (Util.option_bind (Xml.member "SubnetStatus" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.subnet_status
              (fun f -> Query.Pair ("SubnetStatus", (String.to_query f)));
           Util.option_map v.subnet_availability_zone
             (fun f ->
                Query.Pair
                  ("SubnetAvailabilityZone", (AvailabilityZone.to_query f)));
           Util.option_map v.subnet_identifier
             (fun f -> Query.Pair ("SubnetIdentifier", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.subnet_status
              (fun f -> ("subnet_status", (String.to_json f)));
           Util.option_map v.subnet_availability_zone
             (fun f ->
                ("subnet_availability_zone", (AvailabilityZone.to_json f)));
           Util.option_map v.subnet_identifier
             (fun f -> ("subnet_identifier", (String.to_json f)))])
    let of_json j =
      {
        subnet_identifier =
          (Util.option_map (Json.lookup j "subnet_identifier") String.of_json);
        subnet_availability_zone =
          (Util.option_map (Json.lookup j "subnet_availability_zone")
             AvailabilityZone.of_json);
        subnet_status =
          (Util.option_map (Json.lookup j "subnet_status") String.of_json)
      }
  end
module DBSecurityGroupMembershipList =
  struct
    type t = DBSecurityGroupMembership.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map DBSecurityGroupMembership.parse
           (Xml.members "DBSecurityGroup" xml))
    let to_query v = Query.to_query_list DBSecurityGroupMembership.to_query v
    let to_json v = `List (List.map DBSecurityGroupMembership.to_json v)
    let of_json j = Json.to_list DBSecurityGroupMembership.of_json j
  end
module OptionSettingConfigurationList =
  struct
    type t = OptionSetting.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map OptionSetting.parse (Xml.members "OptionSetting" xml))
    let to_query v = Query.to_query_list OptionSetting.to_query v
    let to_json v = `List (List.map OptionSetting.to_json v)
    let of_json j = Json.to_list OptionSetting.of_json j
  end
module VpcSecurityGroupMembershipList =
  struct
    type t = VpcSecurityGroupMembership.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map VpcSecurityGroupMembership.parse
           (Xml.members "VpcSecurityGroupMembership" xml))
    let to_query v =
      Query.to_query_list VpcSecurityGroupMembership.to_query v
    let to_json v = `List (List.map VpcSecurityGroupMembership.to_json v)
    let of_json j = Json.to_list VpcSecurityGroupMembership.of_json j
  end
module OptionGroupOptionSetting =
  struct
    type t =
      {
      setting_name: String.t option ;
      setting_description: String.t option ;
      default_value: String.t option ;
      apply_type: String.t option ;
      allowed_values: String.t option ;
      is_modifiable: Boolean.t option }
    let make ?setting_name  ?setting_description  ?default_value  ?apply_type
       ?allowed_values  ?is_modifiable  () =
      {
        setting_name;
        setting_description;
        default_value;
        apply_type;
        allowed_values;
        is_modifiable
      }
    let parse xml =
      Some
        {
          setting_name =
            (Util.option_bind (Xml.member "SettingName" xml) String.parse);
          setting_description =
            (Util.option_bind (Xml.member "SettingDescription" xml)
               String.parse);
          default_value =
            (Util.option_bind (Xml.member "DefaultValue" xml) String.parse);
          apply_type =
            (Util.option_bind (Xml.member "ApplyType" xml) String.parse);
          allowed_values =
            (Util.option_bind (Xml.member "AllowedValues" xml) String.parse);
          is_modifiable =
            (Util.option_bind (Xml.member "IsModifiable" xml) Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.is_modifiable
              (fun f -> Query.Pair ("IsModifiable", (Boolean.to_query f)));
           Util.option_map v.allowed_values
             (fun f -> Query.Pair ("AllowedValues", (String.to_query f)));
           Util.option_map v.apply_type
             (fun f -> Query.Pair ("ApplyType", (String.to_query f)));
           Util.option_map v.default_value
             (fun f -> Query.Pair ("DefaultValue", (String.to_query f)));
           Util.option_map v.setting_description
             (fun f -> Query.Pair ("SettingDescription", (String.to_query f)));
           Util.option_map v.setting_name
             (fun f -> Query.Pair ("SettingName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.is_modifiable
              (fun f -> ("is_modifiable", (Boolean.to_json f)));
           Util.option_map v.allowed_values
             (fun f -> ("allowed_values", (String.to_json f)));
           Util.option_map v.apply_type
             (fun f -> ("apply_type", (String.to_json f)));
           Util.option_map v.default_value
             (fun f -> ("default_value", (String.to_json f)));
           Util.option_map v.setting_description
             (fun f -> ("setting_description", (String.to_json f)));
           Util.option_map v.setting_name
             (fun f -> ("setting_name", (String.to_json f)))])
    let of_json j =
      {
        setting_name =
          (Util.option_map (Json.lookup j "setting_name") String.of_json);
        setting_description =
          (Util.option_map (Json.lookup j "setting_description")
             String.of_json);
        default_value =
          (Util.option_map (Json.lookup j "default_value") String.of_json);
        apply_type =
          (Util.option_map (Json.lookup j "apply_type") String.of_json);
        allowed_values =
          (Util.option_map (Json.lookup j "allowed_values") String.of_json);
        is_modifiable =
          (Util.option_map (Json.lookup j "is_modifiable") Boolean.of_json)
      }
  end
module ApplyMethod =
  struct
    type t =
      | Immediate 
      | Pending_reboot 
    let str_to_t =
      [("pending-reboot", Pending_reboot); ("immediate", Immediate)]
    let t_to_str =
      [(Pending_reboot, "pending-reboot"); (Immediate, "immediate")]
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
module RecurringCharge =
  struct
    type t =
      {
      recurring_charge_amount: Double.t option ;
      recurring_charge_frequency: String.t option }
    let make ?recurring_charge_amount  ?recurring_charge_frequency  () =
      { recurring_charge_amount; recurring_charge_frequency }
    let parse xml =
      Some
        {
          recurring_charge_amount =
            (Util.option_bind (Xml.member "RecurringChargeAmount" xml)
               Double.parse);
          recurring_charge_frequency =
            (Util.option_bind (Xml.member "RecurringChargeFrequency" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.recurring_charge_frequency
              (fun f ->
                 Query.Pair ("RecurringChargeFrequency", (String.to_query f)));
           Util.option_map v.recurring_charge_amount
             (fun f ->
                Query.Pair ("RecurringChargeAmount", (Double.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.recurring_charge_frequency
              (fun f -> ("recurring_charge_frequency", (String.to_json f)));
           Util.option_map v.recurring_charge_amount
             (fun f -> ("recurring_charge_amount", (Double.to_json f)))])
    let of_json j =
      {
        recurring_charge_amount =
          (Util.option_map (Json.lookup j "recurring_charge_amount")
             Double.of_json);
        recurring_charge_frequency =
          (Util.option_map (Json.lookup j "recurring_charge_frequency")
             String.of_json)
      }
  end
module PendingMaintenanceAction =
  struct
    type t =
      {
      action: String.t option ;
      auto_applied_after_date: DateTime.t option ;
      forced_apply_date: DateTime.t option ;
      opt_in_status: String.t option ;
      current_apply_date: DateTime.t option ;
      description: String.t option }
    let make ?action  ?auto_applied_after_date  ?forced_apply_date 
      ?opt_in_status  ?current_apply_date  ?description  () =
      {
        action;
        auto_applied_after_date;
        forced_apply_date;
        opt_in_status;
        current_apply_date;
        description
      }
    let parse xml =
      Some
        {
          action = (Util.option_bind (Xml.member "Action" xml) String.parse);
          auto_applied_after_date =
            (Util.option_bind (Xml.member "AutoAppliedAfterDate" xml)
               DateTime.parse);
          forced_apply_date =
            (Util.option_bind (Xml.member "ForcedApplyDate" xml)
               DateTime.parse);
          opt_in_status =
            (Util.option_bind (Xml.member "OptInStatus" xml) String.parse);
          current_apply_date =
            (Util.option_bind (Xml.member "CurrentApplyDate" xml)
               DateTime.parse);
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.description
              (fun f -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.current_apply_date
             (fun f -> Query.Pair ("CurrentApplyDate", (DateTime.to_query f)));
           Util.option_map v.opt_in_status
             (fun f -> Query.Pair ("OptInStatus", (String.to_query f)));
           Util.option_map v.forced_apply_date
             (fun f -> Query.Pair ("ForcedApplyDate", (DateTime.to_query f)));
           Util.option_map v.auto_applied_after_date
             (fun f ->
                Query.Pair ("AutoAppliedAfterDate", (DateTime.to_query f)));
           Util.option_map v.action
             (fun f -> Query.Pair ("Action", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.description
              (fun f -> ("description", (String.to_json f)));
           Util.option_map v.current_apply_date
             (fun f -> ("current_apply_date", (DateTime.to_json f)));
           Util.option_map v.opt_in_status
             (fun f -> ("opt_in_status", (String.to_json f)));
           Util.option_map v.forced_apply_date
             (fun f -> ("forced_apply_date", (DateTime.to_json f)));
           Util.option_map v.auto_applied_after_date
             (fun f -> ("auto_applied_after_date", (DateTime.to_json f)));
           Util.option_map v.action (fun f -> ("action", (String.to_json f)))])
    let of_json j =
      {
        action = (Util.option_map (Json.lookup j "action") String.of_json);
        auto_applied_after_date =
          (Util.option_map (Json.lookup j "auto_applied_after_date")
             DateTime.of_json);
        forced_apply_date =
          (Util.option_map (Json.lookup j "forced_apply_date")
             DateTime.of_json);
        opt_in_status =
          (Util.option_map (Json.lookup j "opt_in_status") String.of_json);
        current_apply_date =
          (Util.option_map (Json.lookup j "current_apply_date")
             DateTime.of_json);
        description =
          (Util.option_map (Json.lookup j "description") String.of_json)
      }
  end
module CharacterSet =
  struct
    type t =
      {
      character_set_name: String.t option ;
      character_set_description: String.t option }
    let make ?character_set_name  ?character_set_description  () =
      { character_set_name; character_set_description }
    let parse xml =
      Some
        {
          character_set_name =
            (Util.option_bind (Xml.member "CharacterSetName" xml)
               String.parse);
          character_set_description =
            (Util.option_bind (Xml.member "CharacterSetDescription" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.character_set_description
              (fun f ->
                 Query.Pair ("CharacterSetDescription", (String.to_query f)));
           Util.option_map v.character_set_name
             (fun f -> Query.Pair ("CharacterSetName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.character_set_description
              (fun f -> ("character_set_description", (String.to_json f)));
           Util.option_map v.character_set_name
             (fun f -> ("character_set_name", (String.to_json f)))])
    let of_json j =
      {
        character_set_name =
          (Util.option_map (Json.lookup j "character_set_name")
             String.of_json);
        character_set_description =
          (Util.option_map (Json.lookup j "character_set_description")
             String.of_json)
      }
  end
module DBInstanceStatusInfo =
  struct
    type t =
      {
      status_type: String.t option ;
      normal: Boolean.t option ;
      status: String.t option ;
      message: String.t option }
    let make ?status_type  ?normal  ?status  ?message  () =
      { status_type; normal; status; message }
    let parse xml =
      Some
        {
          status_type =
            (Util.option_bind (Xml.member "StatusType" xml) String.parse);
          normal = (Util.option_bind (Xml.member "Normal" xml) Boolean.parse);
          status = (Util.option_bind (Xml.member "Status" xml) String.parse);
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> Query.Pair ("Message", (String.to_query f)));
           Util.option_map v.status
             (fun f -> Query.Pair ("Status", (String.to_query f)));
           Util.option_map v.normal
             (fun f -> Query.Pair ("Normal", (Boolean.to_query f)));
           Util.option_map v.status_type
             (fun f -> Query.Pair ("StatusType", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.message
              (fun f -> ("message", (String.to_json f)));
           Util.option_map v.status (fun f -> ("status", (String.to_json f)));
           Util.option_map v.normal
             (fun f -> ("normal", (Boolean.to_json f)));
           Util.option_map v.status_type
             (fun f -> ("status_type", (String.to_json f)))])
    let of_json j =
      {
        status_type =
          (Util.option_map (Json.lookup j "status_type") String.of_json);
        normal = (Util.option_map (Json.lookup j "normal") Boolean.of_json);
        status = (Util.option_map (Json.lookup j "status") String.of_json);
        message = (Util.option_map (Json.lookup j "message") String.of_json)
      }
  end
module DBParameterGroupStatus =
  struct
    type t =
      {
      d_b_parameter_group_name: String.t option ;
      parameter_apply_status: String.t option }
    let make ?d_b_parameter_group_name  ?parameter_apply_status  () =
      { d_b_parameter_group_name; parameter_apply_status }
    let parse xml =
      Some
        {
          d_b_parameter_group_name =
            (Util.option_bind (Xml.member "DBParameterGroupName" xml)
               String.parse);
          parameter_apply_status =
            (Util.option_bind (Xml.member "ParameterApplyStatus" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.parameter_apply_status
              (fun f ->
                 Query.Pair ("ParameterApplyStatus", (String.to_query f)));
           Util.option_map v.d_b_parameter_group_name
             (fun f ->
                Query.Pair ("DBParameterGroupName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.parameter_apply_status
              (fun f -> ("parameter_apply_status", (String.to_json f)));
           Util.option_map v.d_b_parameter_group_name
             (fun f -> ("d_b_parameter_group_name", (String.to_json f)))])
    let of_json j =
      {
        d_b_parameter_group_name =
          (Util.option_map (Json.lookup j "d_b_parameter_group_name")
             String.of_json);
        parameter_apply_status =
          (Util.option_map (Json.lookup j "parameter_apply_status")
             String.of_json)
      }
  end
module SubnetList =
  struct
    type t = Subnet.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Subnet.parse (Xml.members "Subnet" xml))
    let to_query v = Query.to_query_list Subnet.to_query v
    let to_json v = `List (List.map Subnet.to_json v)
    let of_json j = Json.to_list Subnet.of_json j
  end
module DomainMembership =
  struct
    type t =
      {
      domain: String.t option ;
      status: String.t option ;
      connectivity: String.t option }
    let make ?domain  ?status  ?connectivity  () =
      { domain; status; connectivity }
    let parse xml =
      Some
        {
          domain = (Util.option_bind (Xml.member "Domain" xml) String.parse);
          status = (Util.option_bind (Xml.member "Status" xml) String.parse);
          connectivity =
            (Util.option_bind (Xml.member "Connectivity" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.connectivity
              (fun f -> Query.Pair ("Connectivity", (String.to_query f)));
           Util.option_map v.status
             (fun f -> Query.Pair ("Status", (String.to_query f)));
           Util.option_map v.domain
             (fun f -> Query.Pair ("Domain", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.connectivity
              (fun f -> ("connectivity", (String.to_json f)));
           Util.option_map v.status (fun f -> ("status", (String.to_json f)));
           Util.option_map v.domain (fun f -> ("domain", (String.to_json f)))])
    let of_json j =
      {
        domain = (Util.option_map (Json.lookup j "domain") String.of_json);
        status = (Util.option_map (Json.lookup j "status") String.of_json);
        connectivity =
          (Util.option_map (Json.lookup j "connectivity") String.of_json)
      }
  end
module OptionGroupMembership =
  struct
    type t = {
      option_group_name: String.t option ;
      status: String.t option }
    let make ?option_group_name  ?status  () = { option_group_name; status }
    let parse xml =
      Some
        {
          option_group_name =
            (Util.option_bind (Xml.member "OptionGroupName" xml) String.parse);
          status = (Util.option_bind (Xml.member "Status" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.status
              (fun f -> Query.Pair ("Status", (String.to_query f)));
           Util.option_map v.option_group_name
             (fun f -> Query.Pair ("OptionGroupName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.status
              (fun f -> ("status", (String.to_json f)));
           Util.option_map v.option_group_name
             (fun f -> ("option_group_name", (String.to_json f)))])
    let of_json j =
      {
        option_group_name =
          (Util.option_map (Json.lookup j "option_group_name") String.of_json);
        status = (Util.option_map (Json.lookup j "status") String.of_json)
      }
  end
module Option =
  struct
    type t =
      {
      option_name: String.t option ;
      option_description: String.t option ;
      persistent: Boolean.t option ;
      permanent: Boolean.t option ;
      port: Integer.t option ;
      option_settings: OptionSettingConfigurationList.t ;
      d_b_security_group_memberships: DBSecurityGroupMembershipList.t ;
      vpc_security_group_memberships: VpcSecurityGroupMembershipList.t }
    let make ?option_name  ?option_description  ?persistent  ?permanent 
      ?port  ?(option_settings= [])  ?(d_b_security_group_memberships= []) 
      ?(vpc_security_group_memberships= [])  () =
      {
        option_name;
        option_description;
        persistent;
        permanent;
        port;
        option_settings;
        d_b_security_group_memberships;
        vpc_security_group_memberships
      }
    let parse xml =
      Some
        {
          option_name =
            (Util.option_bind (Xml.member "OptionName" xml) String.parse);
          option_description =
            (Util.option_bind (Xml.member "OptionDescription" xml)
               String.parse);
          persistent =
            (Util.option_bind (Xml.member "Persistent" xml) Boolean.parse);
          permanent =
            (Util.option_bind (Xml.member "Permanent" xml) Boolean.parse);
          port = (Util.option_bind (Xml.member "Port" xml) Integer.parse);
          option_settings =
            (Util.of_option []
               (Util.option_bind (Xml.member "OptionSettings" xml)
                  OptionSettingConfigurationList.parse));
          d_b_security_group_memberships =
            (Util.of_option []
               (Util.option_bind
                  (Xml.member "DBSecurityGroupMemberships" xml)
                  DBSecurityGroupMembershipList.parse));
          vpc_security_group_memberships =
            (Util.of_option []
               (Util.option_bind
                  (Xml.member "VpcSecurityGroupMemberships" xml)
                  VpcSecurityGroupMembershipList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("VpcSecurityGroupMemberships.member",
                   (VpcSecurityGroupMembershipList.to_query
                      v.vpc_security_group_memberships)));
           Some
             (Query.Pair
                ("DBSecurityGroupMemberships.member",
                  (DBSecurityGroupMembershipList.to_query
                     v.d_b_security_group_memberships)));
           Some
             (Query.Pair
                ("OptionSettings.member",
                  (OptionSettingConfigurationList.to_query v.option_settings)));
           Util.option_map v.port
             (fun f -> Query.Pair ("Port", (Integer.to_query f)));
           Util.option_map v.permanent
             (fun f -> Query.Pair ("Permanent", (Boolean.to_query f)));
           Util.option_map v.persistent
             (fun f -> Query.Pair ("Persistent", (Boolean.to_query f)));
           Util.option_map v.option_description
             (fun f -> Query.Pair ("OptionDescription", (String.to_query f)));
           Util.option_map v.option_name
             (fun f -> Query.Pair ("OptionName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("vpc_security_group_memberships",
                (VpcSecurityGroupMembershipList.to_json
                   v.vpc_security_group_memberships));
           Some
             ("d_b_security_group_memberships",
               (DBSecurityGroupMembershipList.to_json
                  v.d_b_security_group_memberships));
           Some
             ("option_settings",
               (OptionSettingConfigurationList.to_json v.option_settings));
           Util.option_map v.port (fun f -> ("port", (Integer.to_json f)));
           Util.option_map v.permanent
             (fun f -> ("permanent", (Boolean.to_json f)));
           Util.option_map v.persistent
             (fun f -> ("persistent", (Boolean.to_json f)));
           Util.option_map v.option_description
             (fun f -> ("option_description", (String.to_json f)));
           Util.option_map v.option_name
             (fun f -> ("option_name", (String.to_json f)))])
    let of_json j =
      {
        option_name =
          (Util.option_map (Json.lookup j "option_name") String.of_json);
        option_description =
          (Util.option_map (Json.lookup j "option_description")
             String.of_json);
        persistent =
          (Util.option_map (Json.lookup j "persistent") Boolean.of_json);
        permanent =
          (Util.option_map (Json.lookup j "permanent") Boolean.of_json);
        port = (Util.option_map (Json.lookup j "port") Integer.of_json);
        option_settings =
          (OptionSettingConfigurationList.of_json
             (Util.of_option_exn (Json.lookup j "option_settings")));
        d_b_security_group_memberships =
          (DBSecurityGroupMembershipList.of_json
             (Util.of_option_exn
                (Json.lookup j "d_b_security_group_memberships")));
        vpc_security_group_memberships =
          (VpcSecurityGroupMembershipList.of_json
             (Util.of_option_exn
                (Json.lookup j "vpc_security_group_memberships")))
      }
  end
module DBClusterMember =
  struct
    type t =
      {
      d_b_instance_identifier: String.t option ;
      is_cluster_writer: Boolean.t option ;
      d_b_cluster_parameter_group_status: String.t option }
    let make ?d_b_instance_identifier  ?is_cluster_writer 
      ?d_b_cluster_parameter_group_status  () =
      {
        d_b_instance_identifier;
        is_cluster_writer;
        d_b_cluster_parameter_group_status
      }
    let parse xml =
      Some
        {
          d_b_instance_identifier =
            (Util.option_bind (Xml.member "DBInstanceIdentifier" xml)
               String.parse);
          is_cluster_writer =
            (Util.option_bind (Xml.member "IsClusterWriter" xml)
               Boolean.parse);
          d_b_cluster_parameter_group_status =
            (Util.option_bind
               (Xml.member "DBClusterParameterGroupStatus" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.d_b_cluster_parameter_group_status
              (fun f ->
                 Query.Pair
                   ("DBClusterParameterGroupStatus", (String.to_query f)));
           Util.option_map v.is_cluster_writer
             (fun f -> Query.Pair ("IsClusterWriter", (Boolean.to_query f)));
           Util.option_map v.d_b_instance_identifier
             (fun f ->
                Query.Pair ("DBInstanceIdentifier", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.d_b_cluster_parameter_group_status
              (fun f ->
                 ("d_b_cluster_parameter_group_status", (String.to_json f)));
           Util.option_map v.is_cluster_writer
             (fun f -> ("is_cluster_writer", (Boolean.to_json f)));
           Util.option_map v.d_b_instance_identifier
             (fun f -> ("d_b_instance_identifier", (String.to_json f)))])
    let of_json j =
      {
        d_b_instance_identifier =
          (Util.option_map (Json.lookup j "d_b_instance_identifier")
             String.of_json);
        is_cluster_writer =
          (Util.option_map (Json.lookup j "is_cluster_writer")
             Boolean.of_json);
        d_b_cluster_parameter_group_status =
          (Util.option_map
             (Json.lookup j "d_b_cluster_parameter_group_status")
             String.of_json)
      }
  end
module DBClusterOptionGroupStatus =
  struct
    type t =
      {
      d_b_cluster_option_group_name: String.t option ;
      status: String.t option }
    let make ?d_b_cluster_option_group_name  ?status  () =
      { d_b_cluster_option_group_name; status }
    let parse xml =
      Some
        {
          d_b_cluster_option_group_name =
            (Util.option_bind (Xml.member "DBClusterOptionGroupName" xml)
               String.parse);
          status = (Util.option_bind (Xml.member "Status" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.status
              (fun f -> Query.Pair ("Status", (String.to_query f)));
           Util.option_map v.d_b_cluster_option_group_name
             (fun f ->
                Query.Pair ("DBClusterOptionGroupName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.status
              (fun f -> ("status", (String.to_json f)));
           Util.option_map v.d_b_cluster_option_group_name
             (fun f -> ("d_b_cluster_option_group_name", (String.to_json f)))])
    let of_json j =
      {
        d_b_cluster_option_group_name =
          (Util.option_map (Json.lookup j "d_b_cluster_option_group_name")
             String.of_json);
        status = (Util.option_map (Json.lookup j "status") String.of_json)
      }
  end
module EC2SecurityGroup =
  struct
    type t =
      {
      status: String.t option ;
      e_c2_security_group_name: String.t option ;
      e_c2_security_group_id: String.t option ;
      e_c2_security_group_owner_id: String.t option }
    let make ?status  ?e_c2_security_group_name  ?e_c2_security_group_id 
      ?e_c2_security_group_owner_id  () =
      {
        status;
        e_c2_security_group_name;
        e_c2_security_group_id;
        e_c2_security_group_owner_id
      }
    let parse xml =
      Some
        {
          status = (Util.option_bind (Xml.member "Status" xml) String.parse);
          e_c2_security_group_name =
            (Util.option_bind (Xml.member "EC2SecurityGroupName" xml)
               String.parse);
          e_c2_security_group_id =
            (Util.option_bind (Xml.member "EC2SecurityGroupId" xml)
               String.parse);
          e_c2_security_group_owner_id =
            (Util.option_bind (Xml.member "EC2SecurityGroupOwnerId" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.e_c2_security_group_owner_id
              (fun f ->
                 Query.Pair ("EC2SecurityGroupOwnerId", (String.to_query f)));
           Util.option_map v.e_c2_security_group_id
             (fun f -> Query.Pair ("EC2SecurityGroupId", (String.to_query f)));
           Util.option_map v.e_c2_security_group_name
             (fun f ->
                Query.Pair ("EC2SecurityGroupName", (String.to_query f)));
           Util.option_map v.status
             (fun f -> Query.Pair ("Status", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.e_c2_security_group_owner_id
              (fun f -> ("e_c2_security_group_owner_id", (String.to_json f)));
           Util.option_map v.e_c2_security_group_id
             (fun f -> ("e_c2_security_group_id", (String.to_json f)));
           Util.option_map v.e_c2_security_group_name
             (fun f -> ("e_c2_security_group_name", (String.to_json f)));
           Util.option_map v.status (fun f -> ("status", (String.to_json f)))])
    let of_json j =
      {
        status = (Util.option_map (Json.lookup j "status") String.of_json);
        e_c2_security_group_name =
          (Util.option_map (Json.lookup j "e_c2_security_group_name")
             String.of_json);
        e_c2_security_group_id =
          (Util.option_map (Json.lookup j "e_c2_security_group_id")
             String.of_json);
        e_c2_security_group_owner_id =
          (Util.option_map (Json.lookup j "e_c2_security_group_owner_id")
             String.of_json)
      }
  end
module IPRange =
  struct
    type t = {
      status: String.t option ;
      c_i_d_r_i_p: String.t option }
    let make ?status  ?c_i_d_r_i_p  () = { status; c_i_d_r_i_p }
    let parse xml =
      Some
        {
          status = (Util.option_bind (Xml.member "Status" xml) String.parse);
          c_i_d_r_i_p =
            (Util.option_bind (Xml.member "CIDRIP" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.c_i_d_r_i_p
              (fun f -> Query.Pair ("CIDRIP", (String.to_query f)));
           Util.option_map v.status
             (fun f -> Query.Pair ("Status", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.c_i_d_r_i_p
              (fun f -> ("c_i_d_r_i_p", (String.to_json f)));
           Util.option_map v.status (fun f -> ("status", (String.to_json f)))])
    let of_json j =
      {
        status = (Util.option_map (Json.lookup j "status") String.of_json);
        c_i_d_r_i_p =
          (Util.option_map (Json.lookup j "c_i_d_r_i_p") String.of_json)
      }
  end
module OptionGroupOptionSettingsList =
  struct
    type t = OptionGroupOptionSetting.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map OptionGroupOptionSetting.parse
           (Xml.members "OptionGroupOptionSetting" xml))
    let to_query v = Query.to_query_list OptionGroupOptionSetting.to_query v
    let to_json v = `List (List.map OptionGroupOptionSetting.to_json v)
    let of_json j = Json.to_list OptionGroupOptionSetting.of_json j
  end
module OptionsDependedOn =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "OptionName" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module FilterValueList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "Value" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module Parameter =
  struct
    type t =
      {
      parameter_name: String.t option ;
      parameter_value: String.t option ;
      description: String.t option ;
      source: String.t option ;
      apply_type: String.t option ;
      data_type: String.t option ;
      allowed_values: String.t option ;
      is_modifiable: Boolean.t option ;
      minimum_engine_version: String.t option ;
      apply_method: ApplyMethod.t option }
    let make ?parameter_name  ?parameter_value  ?description  ?source 
      ?apply_type  ?data_type  ?allowed_values  ?is_modifiable 
      ?minimum_engine_version  ?apply_method  () =
      {
        parameter_name;
        parameter_value;
        description;
        source;
        apply_type;
        data_type;
        allowed_values;
        is_modifiable;
        minimum_engine_version;
        apply_method
      }
    let parse xml =
      Some
        {
          parameter_name =
            (Util.option_bind (Xml.member "ParameterName" xml) String.parse);
          parameter_value =
            (Util.option_bind (Xml.member "ParameterValue" xml) String.parse);
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse);
          source = (Util.option_bind (Xml.member "Source" xml) String.parse);
          apply_type =
            (Util.option_bind (Xml.member "ApplyType" xml) String.parse);
          data_type =
            (Util.option_bind (Xml.member "DataType" xml) String.parse);
          allowed_values =
            (Util.option_bind (Xml.member "AllowedValues" xml) String.parse);
          is_modifiable =
            (Util.option_bind (Xml.member "IsModifiable" xml) Boolean.parse);
          minimum_engine_version =
            (Util.option_bind (Xml.member "MinimumEngineVersion" xml)
               String.parse);
          apply_method =
            (Util.option_bind (Xml.member "ApplyMethod" xml)
               ApplyMethod.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.apply_method
              (fun f -> Query.Pair ("ApplyMethod", (ApplyMethod.to_query f)));
           Util.option_map v.minimum_engine_version
             (fun f ->
                Query.Pair ("MinimumEngineVersion", (String.to_query f)));
           Util.option_map v.is_modifiable
             (fun f -> Query.Pair ("IsModifiable", (Boolean.to_query f)));
           Util.option_map v.allowed_values
             (fun f -> Query.Pair ("AllowedValues", (String.to_query f)));
           Util.option_map v.data_type
             (fun f -> Query.Pair ("DataType", (String.to_query f)));
           Util.option_map v.apply_type
             (fun f -> Query.Pair ("ApplyType", (String.to_query f)));
           Util.option_map v.source
             (fun f -> Query.Pair ("Source", (String.to_query f)));
           Util.option_map v.description
             (fun f -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.parameter_value
             (fun f -> Query.Pair ("ParameterValue", (String.to_query f)));
           Util.option_map v.parameter_name
             (fun f -> Query.Pair ("ParameterName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.apply_method
              (fun f -> ("apply_method", (ApplyMethod.to_json f)));
           Util.option_map v.minimum_engine_version
             (fun f -> ("minimum_engine_version", (String.to_json f)));
           Util.option_map v.is_modifiable
             (fun f -> ("is_modifiable", (Boolean.to_json f)));
           Util.option_map v.allowed_values
             (fun f -> ("allowed_values", (String.to_json f)));
           Util.option_map v.data_type
             (fun f -> ("data_type", (String.to_json f)));
           Util.option_map v.apply_type
             (fun f -> ("apply_type", (String.to_json f)));
           Util.option_map v.source (fun f -> ("source", (String.to_json f)));
           Util.option_map v.description
             (fun f -> ("description", (String.to_json f)));
           Util.option_map v.parameter_value
             (fun f -> ("parameter_value", (String.to_json f)));
           Util.option_map v.parameter_name
             (fun f -> ("parameter_name", (String.to_json f)))])
    let of_json j =
      {
        parameter_name =
          (Util.option_map (Json.lookup j "parameter_name") String.of_json);
        parameter_value =
          (Util.option_map (Json.lookup j "parameter_value") String.of_json);
        description =
          (Util.option_map (Json.lookup j "description") String.of_json);
        source = (Util.option_map (Json.lookup j "source") String.of_json);
        apply_type =
          (Util.option_map (Json.lookup j "apply_type") String.of_json);
        data_type =
          (Util.option_map (Json.lookup j "data_type") String.of_json);
        allowed_values =
          (Util.option_map (Json.lookup j "allowed_values") String.of_json);
        is_modifiable =
          (Util.option_map (Json.lookup j "is_modifiable") Boolean.of_json);
        minimum_engine_version =
          (Util.option_map (Json.lookup j "minimum_engine_version")
             String.of_json);
        apply_method =
          (Util.option_map (Json.lookup j "apply_method") ApplyMethod.of_json)
      }
  end
module EventCategoriesList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map String.parse (Xml.members "EventCategory" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module SourceIdsList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "SourceId" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module DBSecurityGroupNameList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map String.parse (Xml.members "DBSecurityGroupName" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module OptionSettingsList =
  struct
    type t = OptionSetting.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map OptionSetting.parse (Xml.members "OptionSetting" xml))
    let to_query v = Query.to_query_list OptionSetting.to_query v
    let to_json v = `List (List.map OptionSetting.to_json v)
    let of_json j = Json.to_list OptionSetting.of_json j
  end
module VpcSecurityGroupIdList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map String.parse (Xml.members "VpcSecurityGroupId" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module RecurringChargeList =
  struct
    type t = RecurringCharge.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map RecurringCharge.parse (Xml.members "RecurringCharge" xml))
    let to_query v = Query.to_query_list RecurringCharge.to_query v
    let to_json v = `List (List.map RecurringCharge.to_json v)
    let of_json j = Json.to_list RecurringCharge.of_json j
  end
module PendingMaintenanceActionDetails =
  struct
    type t = PendingMaintenanceAction.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map PendingMaintenanceAction.parse
           (Xml.members "PendingMaintenanceAction" xml))
    let to_query v = Query.to_query_list PendingMaintenanceAction.to_query v
    let to_json v = `List (List.map PendingMaintenanceAction.to_json v)
    let of_json j = Json.to_list PendingMaintenanceAction.of_json j
  end
module SupportedCharacterSetsList =
  struct
    type t = CharacterSet.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map CharacterSet.parse (Xml.members "CharacterSet" xml))
    let to_query v = Query.to_query_list CharacterSet.to_query v
    let to_json v = `List (List.map CharacterSet.to_json v)
    let of_json j = Json.to_list CharacterSet.of_json j
  end
module DBInstanceStatusInfoList =
  struct
    type t = DBInstanceStatusInfo.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map DBInstanceStatusInfo.parse
           (Xml.members "DBInstanceStatusInfo" xml))
    let to_query v = Query.to_query_list DBInstanceStatusInfo.to_query v
    let to_json v = `List (List.map DBInstanceStatusInfo.to_json v)
    let of_json j = Json.to_list DBInstanceStatusInfo.of_json j
  end
module DBParameterGroupStatusList =
  struct
    type t = DBParameterGroupStatus.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map DBParameterGroupStatus.parse
           (Xml.members "DBParameterGroup" xml))
    let to_query v = Query.to_query_list DBParameterGroupStatus.to_query v
    let to_json v = `List (List.map DBParameterGroupStatus.to_json v)
    let of_json j = Json.to_list DBParameterGroupStatus.of_json j
  end
module DBSubnetGroup =
  struct
    type t =
      {
      d_b_subnet_group_name: String.t option ;
      d_b_subnet_group_description: String.t option ;
      vpc_id: String.t option ;
      subnet_group_status: String.t option ;
      subnets: SubnetList.t }
    let make ?d_b_subnet_group_name  ?d_b_subnet_group_description  ?vpc_id 
      ?subnet_group_status  ?(subnets= [])  () =
      {
        d_b_subnet_group_name;
        d_b_subnet_group_description;
        vpc_id;
        subnet_group_status;
        subnets
      }
    let parse xml =
      Some
        {
          d_b_subnet_group_name =
            (Util.option_bind (Xml.member "DBSubnetGroupName" xml)
               String.parse);
          d_b_subnet_group_description =
            (Util.option_bind (Xml.member "DBSubnetGroupDescription" xml)
               String.parse);
          vpc_id = (Util.option_bind (Xml.member "VpcId" xml) String.parse);
          subnet_group_status =
            (Util.option_bind (Xml.member "SubnetGroupStatus" xml)
               String.parse);
          subnets =
            (Util.of_option []
               (Util.option_bind (Xml.member "Subnets" xml) SubnetList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("Subnets.member", (SubnetList.to_query v.subnets)));
           Util.option_map v.subnet_group_status
             (fun f -> Query.Pair ("SubnetGroupStatus", (String.to_query f)));
           Util.option_map v.vpc_id
             (fun f -> Query.Pair ("VpcId", (String.to_query f)));
           Util.option_map v.d_b_subnet_group_description
             (fun f ->
                Query.Pair ("DBSubnetGroupDescription", (String.to_query f)));
           Util.option_map v.d_b_subnet_group_name
             (fun f -> Query.Pair ("DBSubnetGroupName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("subnets", (SubnetList.to_json v.subnets));
           Util.option_map v.subnet_group_status
             (fun f -> ("subnet_group_status", (String.to_json f)));
           Util.option_map v.vpc_id (fun f -> ("vpc_id", (String.to_json f)));
           Util.option_map v.d_b_subnet_group_description
             (fun f -> ("d_b_subnet_group_description", (String.to_json f)));
           Util.option_map v.d_b_subnet_group_name
             (fun f -> ("d_b_subnet_group_name", (String.to_json f)))])
    let of_json j =
      {
        d_b_subnet_group_name =
          (Util.option_map (Json.lookup j "d_b_subnet_group_name")
             String.of_json);
        d_b_subnet_group_description =
          (Util.option_map (Json.lookup j "d_b_subnet_group_description")
             String.of_json);
        vpc_id = (Util.option_map (Json.lookup j "vpc_id") String.of_json);
        subnet_group_status =
          (Util.option_map (Json.lookup j "subnet_group_status")
             String.of_json);
        subnets =
          (SubnetList.of_json (Util.of_option_exn (Json.lookup j "subnets")))
      }
  end
module DomainMembershipList =
  struct
    type t = DomainMembership.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map DomainMembership.parse (Xml.members "DomainMembership" xml))
    let to_query v = Query.to_query_list DomainMembership.to_query v
    let to_json v = `List (List.map DomainMembership.to_json v)
    let of_json j = Json.to_list DomainMembership.of_json j
  end
module Endpoint =
  struct
    type t = {
      address: String.t option ;
      port: Integer.t option }
    let make ?address  ?port  () = { address; port }
    let parse xml =
      Some
        {
          address =
            (Util.option_bind (Xml.member "Address" xml) String.parse);
          port = (Util.option_bind (Xml.member "Port" xml) Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.port
              (fun f -> Query.Pair ("Port", (Integer.to_query f)));
           Util.option_map v.address
             (fun f -> Query.Pair ("Address", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.port (fun f -> ("port", (Integer.to_json f)));
           Util.option_map v.address
             (fun f -> ("address", (String.to_json f)))])
    let of_json j =
      {
        address = (Util.option_map (Json.lookup j "address") String.of_json);
        port = (Util.option_map (Json.lookup j "port") Integer.of_json)
      }
  end
module OptionGroupMembershipList =
  struct
    type t = OptionGroupMembership.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map OptionGroupMembership.parse
           (Xml.members "OptionGroupMembership" xml))
    let to_query v = Query.to_query_list OptionGroupMembership.to_query v
    let to_json v = `List (List.map OptionGroupMembership.to_json v)
    let of_json j = Json.to_list OptionGroupMembership.of_json j
  end
module PendingModifiedValues =
  struct
    type t =
      {
      d_b_instance_class: String.t option ;
      allocated_storage: Integer.t option ;
      master_user_password: String.t option ;
      port: Integer.t option ;
      backup_retention_period: Integer.t option ;
      multi_a_z: Boolean.t option ;
      engine_version: String.t option ;
      iops: Integer.t option ;
      d_b_instance_identifier: String.t option ;
      storage_type: String.t option ;
      c_a_certificate_identifier: String.t option }
    let make ?d_b_instance_class  ?allocated_storage  ?master_user_password 
      ?port  ?backup_retention_period  ?multi_a_z  ?engine_version  ?iops 
      ?d_b_instance_identifier  ?storage_type  ?c_a_certificate_identifier 
      () =
      {
        d_b_instance_class;
        allocated_storage;
        master_user_password;
        port;
        backup_retention_period;
        multi_a_z;
        engine_version;
        iops;
        d_b_instance_identifier;
        storage_type;
        c_a_certificate_identifier
      }
    let parse xml =
      Some
        {
          d_b_instance_class =
            (Util.option_bind (Xml.member "DBInstanceClass" xml) String.parse);
          allocated_storage =
            (Util.option_bind (Xml.member "AllocatedStorage" xml)
               Integer.parse);
          master_user_password =
            (Util.option_bind (Xml.member "MasterUserPassword" xml)
               String.parse);
          port = (Util.option_bind (Xml.member "Port" xml) Integer.parse);
          backup_retention_period =
            (Util.option_bind (Xml.member "BackupRetentionPeriod" xml)
               Integer.parse);
          multi_a_z =
            (Util.option_bind (Xml.member "MultiAZ" xml) Boolean.parse);
          engine_version =
            (Util.option_bind (Xml.member "EngineVersion" xml) String.parse);
          iops = (Util.option_bind (Xml.member "Iops" xml) Integer.parse);
          d_b_instance_identifier =
            (Util.option_bind (Xml.member "DBInstanceIdentifier" xml)
               String.parse);
          storage_type =
            (Util.option_bind (Xml.member "StorageType" xml) String.parse);
          c_a_certificate_identifier =
            (Util.option_bind (Xml.member "CACertificateIdentifier" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.c_a_certificate_identifier
              (fun f ->
                 Query.Pair ("CACertificateIdentifier", (String.to_query f)));
           Util.option_map v.storage_type
             (fun f -> Query.Pair ("StorageType", (String.to_query f)));
           Util.option_map v.d_b_instance_identifier
             (fun f ->
                Query.Pair ("DBInstanceIdentifier", (String.to_query f)));
           Util.option_map v.iops
             (fun f -> Query.Pair ("Iops", (Integer.to_query f)));
           Util.option_map v.engine_version
             (fun f -> Query.Pair ("EngineVersion", (String.to_query f)));
           Util.option_map v.multi_a_z
             (fun f -> Query.Pair ("MultiAZ", (Boolean.to_query f)));
           Util.option_map v.backup_retention_period
             (fun f ->
                Query.Pair ("BackupRetentionPeriod", (Integer.to_query f)));
           Util.option_map v.port
             (fun f -> Query.Pair ("Port", (Integer.to_query f)));
           Util.option_map v.master_user_password
             (fun f -> Query.Pair ("MasterUserPassword", (String.to_query f)));
           Util.option_map v.allocated_storage
             (fun f -> Query.Pair ("AllocatedStorage", (Integer.to_query f)));
           Util.option_map v.d_b_instance_class
             (fun f -> Query.Pair ("DBInstanceClass", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.c_a_certificate_identifier
              (fun f -> ("c_a_certificate_identifier", (String.to_json f)));
           Util.option_map v.storage_type
             (fun f -> ("storage_type", (String.to_json f)));
           Util.option_map v.d_b_instance_identifier
             (fun f -> ("d_b_instance_identifier", (String.to_json f)));
           Util.option_map v.iops (fun f -> ("iops", (Integer.to_json f)));
           Util.option_map v.engine_version
             (fun f -> ("engine_version", (String.to_json f)));
           Util.option_map v.multi_a_z
             (fun f -> ("multi_a_z", (Boolean.to_json f)));
           Util.option_map v.backup_retention_period
             (fun f -> ("backup_retention_period", (Integer.to_json f)));
           Util.option_map v.port (fun f -> ("port", (Integer.to_json f)));
           Util.option_map v.master_user_password
             (fun f -> ("master_user_password", (String.to_json f)));
           Util.option_map v.allocated_storage
             (fun f -> ("allocated_storage", (Integer.to_json f)));
           Util.option_map v.d_b_instance_class
             (fun f -> ("d_b_instance_class", (String.to_json f)))])
    let of_json j =
      {
        d_b_instance_class =
          (Util.option_map (Json.lookup j "d_b_instance_class")
             String.of_json);
        allocated_storage =
          (Util.option_map (Json.lookup j "allocated_storage")
             Integer.of_json);
        master_user_password =
          (Util.option_map (Json.lookup j "master_user_password")
             String.of_json);
        port = (Util.option_map (Json.lookup j "port") Integer.of_json);
        backup_retention_period =
          (Util.option_map (Json.lookup j "backup_retention_period")
             Integer.of_json);
        multi_a_z =
          (Util.option_map (Json.lookup j "multi_a_z") Boolean.of_json);
        engine_version =
          (Util.option_map (Json.lookup j "engine_version") String.of_json);
        iops = (Util.option_map (Json.lookup j "iops") Integer.of_json);
        d_b_instance_identifier =
          (Util.option_map (Json.lookup j "d_b_instance_identifier")
             String.of_json);
        storage_type =
          (Util.option_map (Json.lookup j "storage_type") String.of_json);
        c_a_certificate_identifier =
          (Util.option_map (Json.lookup j "c_a_certificate_identifier")
             String.of_json)
      }
  end
module ReadReplicaDBInstanceIdentifierList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map String.parse
           (Xml.members "ReadReplicaDBInstanceIdentifier" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module SourceType =
  struct
    type t =
      | Db_instance 
      | Db_parameter_group 
      | Db_security_group 
      | Db_snapshot 
    let str_to_t =
      [("db-snapshot", Db_snapshot);
      ("db-security-group", Db_security_group);
      ("db-parameter-group", Db_parameter_group);
      ("db-instance", Db_instance)]
    let t_to_str =
      [(Db_snapshot, "db-snapshot");
      (Db_security_group, "db-security-group");
      (Db_parameter_group, "db-parameter-group");
      (Db_instance, "db-instance")]
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
module OptionsList =
  struct
    type t = Option.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Option.parse (Xml.members "Option" xml))
    let to_query v = Query.to_query_list Option.to_query v
    let to_json v = `List (List.map Option.to_json v)
    let of_json j = Json.to_list Option.of_json j
  end
module AvailabilityZoneList =
  struct
    type t = AvailabilityZone.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map AvailabilityZone.parse (Xml.members "AvailabilityZone" xml))
    let to_query v = Query.to_query_list AvailabilityZone.to_query v
    let to_json v = `List (List.map AvailabilityZone.to_json v)
    let of_json j = Json.to_list AvailabilityZone.of_json j
  end
module AvailabilityZones =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map String.parse (Xml.members "AvailabilityZone" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module DBClusterMemberList =
  struct
    type t = DBClusterMember.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map DBClusterMember.parse (Xml.members "DBClusterMember" xml))
    let to_query v = Query.to_query_list DBClusterMember.to_query v
    let to_json v = `List (List.map DBClusterMember.to_json v)
    let of_json j = Json.to_list DBClusterMember.of_json j
  end
module DBClusterOptionGroupMemberships =
  struct
    type t = DBClusterOptionGroupStatus.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map DBClusterOptionGroupStatus.parse
           (Xml.members "DBClusterOptionGroup" xml))
    let to_query v =
      Query.to_query_list DBClusterOptionGroupStatus.to_query v
    let to_json v = `List (List.map DBClusterOptionGroupStatus.to_json v)
    let of_json j = Json.to_list DBClusterOptionGroupStatus.of_json j
  end
module EC2SecurityGroupList =
  struct
    type t = EC2SecurityGroup.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map EC2SecurityGroup.parse (Xml.members "EC2SecurityGroup" xml))
    let to_query v = Query.to_query_list EC2SecurityGroup.to_query v
    let to_json v = `List (List.map EC2SecurityGroup.to_json v)
    let of_json j = Json.to_list EC2SecurityGroup.of_json j
  end
module IPRangeList =
  struct
    type t = IPRange.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map IPRange.parse (Xml.members "IPRange" xml))
    let to_query v = Query.to_query_list IPRange.to_query v
    let to_json v = `List (List.map IPRange.to_json v)
    let of_json j = Json.to_list IPRange.of_json j
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
              (fun f -> Query.Pair ("Value", (String.to_query f)));
           Util.option_map v.key
             (fun f -> Query.Pair ("Key", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.value (fun f -> ("value", (String.to_json f)));
           Util.option_map v.key (fun f -> ("key", (String.to_json f)))])
    let of_json j =
      {
        key = (Util.option_map (Json.lookup j "key") String.of_json);
        value = (Util.option_map (Json.lookup j "value") String.of_json)
      }
  end
module OptionGroupOption =
  struct
    type t =
      {
      name: String.t option ;
      description: String.t option ;
      engine_name: String.t option ;
      major_engine_version: String.t option ;
      minimum_required_minor_engine_version: String.t option ;
      port_required: Boolean.t option ;
      default_port: Integer.t option ;
      options_depended_on: OptionsDependedOn.t ;
      persistent: Boolean.t option ;
      permanent: Boolean.t option ;
      option_group_option_settings: OptionGroupOptionSettingsList.t }
    let make ?name  ?description  ?engine_name  ?major_engine_version 
      ?minimum_required_minor_engine_version  ?port_required  ?default_port 
      ?(options_depended_on= [])  ?persistent  ?permanent 
      ?(option_group_option_settings= [])  () =
      {
        name;
        description;
        engine_name;
        major_engine_version;
        minimum_required_minor_engine_version;
        port_required;
        default_port;
        options_depended_on;
        persistent;
        permanent;
        option_group_option_settings
      }
    let parse xml =
      Some
        {
          name = (Util.option_bind (Xml.member "Name" xml) String.parse);
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse);
          engine_name =
            (Util.option_bind (Xml.member "EngineName" xml) String.parse);
          major_engine_version =
            (Util.option_bind (Xml.member "MajorEngineVersion" xml)
               String.parse);
          minimum_required_minor_engine_version =
            (Util.option_bind
               (Xml.member "MinimumRequiredMinorEngineVersion" xml)
               String.parse);
          port_required =
            (Util.option_bind (Xml.member "PortRequired" xml) Boolean.parse);
          default_port =
            (Util.option_bind (Xml.member "DefaultPort" xml) Integer.parse);
          options_depended_on =
            (Util.of_option []
               (Util.option_bind (Xml.member "OptionsDependedOn" xml)
                  OptionsDependedOn.parse));
          persistent =
            (Util.option_bind (Xml.member "Persistent" xml) Boolean.parse);
          permanent =
            (Util.option_bind (Xml.member "Permanent" xml) Boolean.parse);
          option_group_option_settings =
            (Util.of_option []
               (Util.option_bind (Xml.member "OptionGroupOptionSettings" xml)
                  OptionGroupOptionSettingsList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("OptionGroupOptionSettings.member",
                   (OptionGroupOptionSettingsList.to_query
                      v.option_group_option_settings)));
           Util.option_map v.permanent
             (fun f -> Query.Pair ("Permanent", (Boolean.to_query f)));
           Util.option_map v.persistent
             (fun f -> Query.Pair ("Persistent", (Boolean.to_query f)));
           Some
             (Query.Pair
                ("OptionsDependedOn.member",
                  (OptionsDependedOn.to_query v.options_depended_on)));
           Util.option_map v.default_port
             (fun f -> Query.Pair ("DefaultPort", (Integer.to_query f)));
           Util.option_map v.port_required
             (fun f -> Query.Pair ("PortRequired", (Boolean.to_query f)));
           Util.option_map v.minimum_required_minor_engine_version
             (fun f ->
                Query.Pair
                  ("MinimumRequiredMinorEngineVersion", (String.to_query f)));
           Util.option_map v.major_engine_version
             (fun f -> Query.Pair ("MajorEngineVersion", (String.to_query f)));
           Util.option_map v.engine_name
             (fun f -> Query.Pair ("EngineName", (String.to_query f)));
           Util.option_map v.description
             (fun f -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.name
             (fun f -> Query.Pair ("Name", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("option_group_option_settings",
                (OptionGroupOptionSettingsList.to_json
                   v.option_group_option_settings));
           Util.option_map v.permanent
             (fun f -> ("permanent", (Boolean.to_json f)));
           Util.option_map v.persistent
             (fun f -> ("persistent", (Boolean.to_json f)));
           Some
             ("options_depended_on",
               (OptionsDependedOn.to_json v.options_depended_on));
           Util.option_map v.default_port
             (fun f -> ("default_port", (Integer.to_json f)));
           Util.option_map v.port_required
             (fun f -> ("port_required", (Boolean.to_json f)));
           Util.option_map v.minimum_required_minor_engine_version
             (fun f ->
                ("minimum_required_minor_engine_version", (String.to_json f)));
           Util.option_map v.major_engine_version
             (fun f -> ("major_engine_version", (String.to_json f)));
           Util.option_map v.engine_name
             (fun f -> ("engine_name", (String.to_json f)));
           Util.option_map v.description
             (fun f -> ("description", (String.to_json f)));
           Util.option_map v.name (fun f -> ("name", (String.to_json f)))])
    let of_json j =
      {
        name = (Util.option_map (Json.lookup j "name") String.of_json);
        description =
          (Util.option_map (Json.lookup j "description") String.of_json);
        engine_name =
          (Util.option_map (Json.lookup j "engine_name") String.of_json);
        major_engine_version =
          (Util.option_map (Json.lookup j "major_engine_version")
             String.of_json);
        minimum_required_minor_engine_version =
          (Util.option_map
             (Json.lookup j "minimum_required_minor_engine_version")
             String.of_json);
        port_required =
          (Util.option_map (Json.lookup j "port_required") Boolean.of_json);
        default_port =
          (Util.option_map (Json.lookup j "default_port") Integer.of_json);
        options_depended_on =
          (OptionsDependedOn.of_json
             (Util.of_option_exn (Json.lookup j "options_depended_on")));
        persistent =
          (Util.option_map (Json.lookup j "persistent") Boolean.of_json);
        permanent =
          (Util.option_map (Json.lookup j "permanent") Boolean.of_json);
        option_group_option_settings =
          (OptionGroupOptionSettingsList.of_json
             (Util.of_option_exn
                (Json.lookup j "option_group_option_settings")))
      }
  end
module Filter =
  struct
    type t = {
      name: String.t ;
      values: FilterValueList.t }
    let make ~name  ~values  () = { name; values }
    let parse xml =
      Some
        {
          name =
            (Xml.required "Name"
               (Util.option_bind (Xml.member "Name" xml) String.parse));
          values =
            (Xml.required "Values"
               (Util.option_bind (Xml.member "Values" xml)
                  FilterValueList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Values.member", (FilterValueList.to_query v.values)));
           Some (Query.Pair ("Name", (String.to_query v.name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("values", (FilterValueList.to_json v.values));
           Some ("name", (String.to_json v.name))])
    let of_json j =
      {
        name = (String.of_json (Util.of_option_exn (Json.lookup j "name")));
        values =
          (FilterValueList.of_json
             (Util.of_option_exn (Json.lookup j "values")))
      }
  end
module ParametersList =
  struct
    type t = Parameter.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map Parameter.parse (Xml.members "Parameter" xml))
    let to_query v = Query.to_query_list Parameter.to_query v
    let to_json v = `List (List.map Parameter.to_json v)
    let of_json j = Json.to_list Parameter.of_json j
  end
module EventSubscription =
  struct
    type t =
      {
      customer_aws_id: String.t option ;
      cust_subscription_id: String.t option ;
      sns_topic_arn: String.t option ;
      status: String.t option ;
      subscription_creation_time: String.t option ;
      source_type: String.t option ;
      source_ids_list: SourceIdsList.t ;
      event_categories_list: EventCategoriesList.t ;
      enabled: Boolean.t option }
    let make ?customer_aws_id  ?cust_subscription_id  ?sns_topic_arn  ?status
       ?subscription_creation_time  ?source_type  ?(source_ids_list= []) 
      ?(event_categories_list= [])  ?enabled  () =
      {
        customer_aws_id;
        cust_subscription_id;
        sns_topic_arn;
        status;
        subscription_creation_time;
        source_type;
        source_ids_list;
        event_categories_list;
        enabled
      }
    let parse xml =
      Some
        {
          customer_aws_id =
            (Util.option_bind (Xml.member "CustomerAwsId" xml) String.parse);
          cust_subscription_id =
            (Util.option_bind (Xml.member "CustSubscriptionId" xml)
               String.parse);
          sns_topic_arn =
            (Util.option_bind (Xml.member "SnsTopicArn" xml) String.parse);
          status = (Util.option_bind (Xml.member "Status" xml) String.parse);
          subscription_creation_time =
            (Util.option_bind (Xml.member "SubscriptionCreationTime" xml)
               String.parse);
          source_type =
            (Util.option_bind (Xml.member "SourceType" xml) String.parse);
          source_ids_list =
            (Util.of_option []
               (Util.option_bind (Xml.member "SourceIdsList" xml)
                  SourceIdsList.parse));
          event_categories_list =
            (Util.of_option []
               (Util.option_bind (Xml.member "EventCategoriesList" xml)
                  EventCategoriesList.parse));
          enabled =
            (Util.option_bind (Xml.member "Enabled" xml) Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.enabled
              (fun f -> Query.Pair ("Enabled", (Boolean.to_query f)));
           Some
             (Query.Pair
                ("EventCategoriesList.member",
                  (EventCategoriesList.to_query v.event_categories_list)));
           Some
             (Query.Pair
                ("SourceIdsList.member",
                  (SourceIdsList.to_query v.source_ids_list)));
           Util.option_map v.source_type
             (fun f -> Query.Pair ("SourceType", (String.to_query f)));
           Util.option_map v.subscription_creation_time
             (fun f ->
                Query.Pair ("SubscriptionCreationTime", (String.to_query f)));
           Util.option_map v.status
             (fun f -> Query.Pair ("Status", (String.to_query f)));
           Util.option_map v.sns_topic_arn
             (fun f -> Query.Pair ("SnsTopicArn", (String.to_query f)));
           Util.option_map v.cust_subscription_id
             (fun f -> Query.Pair ("CustSubscriptionId", (String.to_query f)));
           Util.option_map v.customer_aws_id
             (fun f -> Query.Pair ("CustomerAwsId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.enabled
              (fun f -> ("enabled", (Boolean.to_json f)));
           Some
             ("event_categories_list",
               (EventCategoriesList.to_json v.event_categories_list));
           Some
             ("source_ids_list", (SourceIdsList.to_json v.source_ids_list));
           Util.option_map v.source_type
             (fun f -> ("source_type", (String.to_json f)));
           Util.option_map v.subscription_creation_time
             (fun f -> ("subscription_creation_time", (String.to_json f)));
           Util.option_map v.status (fun f -> ("status", (String.to_json f)));
           Util.option_map v.sns_topic_arn
             (fun f -> ("sns_topic_arn", (String.to_json f)));
           Util.option_map v.cust_subscription_id
             (fun f -> ("cust_subscription_id", (String.to_json f)));
           Util.option_map v.customer_aws_id
             (fun f -> ("customer_aws_id", (String.to_json f)))])
    let of_json j =
      {
        customer_aws_id =
          (Util.option_map (Json.lookup j "customer_aws_id") String.of_json);
        cust_subscription_id =
          (Util.option_map (Json.lookup j "cust_subscription_id")
             String.of_json);
        sns_topic_arn =
          (Util.option_map (Json.lookup j "sns_topic_arn") String.of_json);
        status = (Util.option_map (Json.lookup j "status") String.of_json);
        subscription_creation_time =
          (Util.option_map (Json.lookup j "subscription_creation_time")
             String.of_json);
        source_type =
          (Util.option_map (Json.lookup j "source_type") String.of_json);
        source_ids_list =
          (SourceIdsList.of_json
             (Util.of_option_exn (Json.lookup j "source_ids_list")));
        event_categories_list =
          (EventCategoriesList.of_json
             (Util.of_option_exn (Json.lookup j "event_categories_list")));
        enabled = (Util.option_map (Json.lookup j "enabled") Boolean.of_json)
      }
  end
module OptionConfiguration =
  struct
    type t =
      {
      option_name: String.t ;
      port: Integer.t option ;
      d_b_security_group_memberships: DBSecurityGroupNameList.t ;
      vpc_security_group_memberships: VpcSecurityGroupIdList.t ;
      option_settings: OptionSettingsList.t }
    let make ~option_name  ?port  ?(d_b_security_group_memberships= []) 
      ?(vpc_security_group_memberships= [])  ?(option_settings= [])  () =
      {
        option_name;
        port;
        d_b_security_group_memberships;
        vpc_security_group_memberships;
        option_settings
      }
    let parse xml =
      Some
        {
          option_name =
            (Xml.required "OptionName"
               (Util.option_bind (Xml.member "OptionName" xml) String.parse));
          port = (Util.option_bind (Xml.member "Port" xml) Integer.parse);
          d_b_security_group_memberships =
            (Util.of_option []
               (Util.option_bind
                  (Xml.member "DBSecurityGroupMemberships" xml)
                  DBSecurityGroupNameList.parse));
          vpc_security_group_memberships =
            (Util.of_option []
               (Util.option_bind
                  (Xml.member "VpcSecurityGroupMemberships" xml)
                  VpcSecurityGroupIdList.parse));
          option_settings =
            (Util.of_option []
               (Util.option_bind (Xml.member "OptionSettings" xml)
                  OptionSettingsList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("OptionSettings.member",
                   (OptionSettingsList.to_query v.option_settings)));
           Some
             (Query.Pair
                ("VpcSecurityGroupMemberships.member",
                  (VpcSecurityGroupIdList.to_query
                     v.vpc_security_group_memberships)));
           Some
             (Query.Pair
                ("DBSecurityGroupMemberships.member",
                  (DBSecurityGroupNameList.to_query
                     v.d_b_security_group_memberships)));
           Util.option_map v.port
             (fun f -> Query.Pair ("Port", (Integer.to_query f)));
           Some (Query.Pair ("OptionName", (String.to_query v.option_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("option_settings",
                (OptionSettingsList.to_json v.option_settings));
           Some
             ("vpc_security_group_memberships",
               (VpcSecurityGroupIdList.to_json
                  v.vpc_security_group_memberships));
           Some
             ("d_b_security_group_memberships",
               (DBSecurityGroupNameList.to_json
                  v.d_b_security_group_memberships));
           Util.option_map v.port (fun f -> ("port", (Integer.to_json f)));
           Some ("option_name", (String.to_json v.option_name))])
    let of_json j =
      {
        option_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "option_name")));
        port = (Util.option_map (Json.lookup j "port") Integer.of_json);
        d_b_security_group_memberships =
          (DBSecurityGroupNameList.of_json
             (Util.of_option_exn
                (Json.lookup j "d_b_security_group_memberships")));
        vpc_security_group_memberships =
          (VpcSecurityGroupIdList.of_json
             (Util.of_option_exn
                (Json.lookup j "vpc_security_group_memberships")));
        option_settings =
          (OptionSettingsList.of_json
             (Util.of_option_exn (Json.lookup j "option_settings")))
      }
  end
module ReservedDBInstance =
  struct
    type t =
      {
      reserved_d_b_instance_id: String.t option ;
      reserved_d_b_instances_offering_id: String.t option ;
      d_b_instance_class: String.t option ;
      start_time: DateTime.t option ;
      duration: Integer.t option ;
      fixed_price: Double.t option ;
      usage_price: Double.t option ;
      currency_code: String.t option ;
      d_b_instance_count: Integer.t option ;
      product_description: String.t option ;
      offering_type: String.t option ;
      multi_a_z: Boolean.t option ;
      state: String.t option ;
      recurring_charges: RecurringChargeList.t }
    let make ?reserved_d_b_instance_id  ?reserved_d_b_instances_offering_id 
      ?d_b_instance_class  ?start_time  ?duration  ?fixed_price  ?usage_price
       ?currency_code  ?d_b_instance_count  ?product_description 
      ?offering_type  ?multi_a_z  ?state  ?(recurring_charges= [])  () =
      {
        reserved_d_b_instance_id;
        reserved_d_b_instances_offering_id;
        d_b_instance_class;
        start_time;
        duration;
        fixed_price;
        usage_price;
        currency_code;
        d_b_instance_count;
        product_description;
        offering_type;
        multi_a_z;
        state;
        recurring_charges
      }
    let parse xml =
      Some
        {
          reserved_d_b_instance_id =
            (Util.option_bind (Xml.member "ReservedDBInstanceId" xml)
               String.parse);
          reserved_d_b_instances_offering_id =
            (Util.option_bind
               (Xml.member "ReservedDBInstancesOfferingId" xml) String.parse);
          d_b_instance_class =
            (Util.option_bind (Xml.member "DBInstanceClass" xml) String.parse);
          start_time =
            (Util.option_bind (Xml.member "StartTime" xml) DateTime.parse);
          duration =
            (Util.option_bind (Xml.member "Duration" xml) Integer.parse);
          fixed_price =
            (Util.option_bind (Xml.member "FixedPrice" xml) Double.parse);
          usage_price =
            (Util.option_bind (Xml.member "UsagePrice" xml) Double.parse);
          currency_code =
            (Util.option_bind (Xml.member "CurrencyCode" xml) String.parse);
          d_b_instance_count =
            (Util.option_bind (Xml.member "DBInstanceCount" xml)
               Integer.parse);
          product_description =
            (Util.option_bind (Xml.member "ProductDescription" xml)
               String.parse);
          offering_type =
            (Util.option_bind (Xml.member "OfferingType" xml) String.parse);
          multi_a_z =
            (Util.option_bind (Xml.member "MultiAZ" xml) Boolean.parse);
          state = (Util.option_bind (Xml.member "State" xml) String.parse);
          recurring_charges =
            (Util.of_option []
               (Util.option_bind (Xml.member "RecurringCharges" xml)
                  RecurringChargeList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("RecurringCharges.member",
                   (RecurringChargeList.to_query v.recurring_charges)));
           Util.option_map v.state
             (fun f -> Query.Pair ("State", (String.to_query f)));
           Util.option_map v.multi_a_z
             (fun f -> Query.Pair ("MultiAZ", (Boolean.to_query f)));
           Util.option_map v.offering_type
             (fun f -> Query.Pair ("OfferingType", (String.to_query f)));
           Util.option_map v.product_description
             (fun f -> Query.Pair ("ProductDescription", (String.to_query f)));
           Util.option_map v.d_b_instance_count
             (fun f -> Query.Pair ("DBInstanceCount", (Integer.to_query f)));
           Util.option_map v.currency_code
             (fun f -> Query.Pair ("CurrencyCode", (String.to_query f)));
           Util.option_map v.usage_price
             (fun f -> Query.Pair ("UsagePrice", (Double.to_query f)));
           Util.option_map v.fixed_price
             (fun f -> Query.Pair ("FixedPrice", (Double.to_query f)));
           Util.option_map v.duration
             (fun f -> Query.Pair ("Duration", (Integer.to_query f)));
           Util.option_map v.start_time
             (fun f -> Query.Pair ("StartTime", (DateTime.to_query f)));
           Util.option_map v.d_b_instance_class
             (fun f -> Query.Pair ("DBInstanceClass", (String.to_query f)));
           Util.option_map v.reserved_d_b_instances_offering_id
             (fun f ->
                Query.Pair
                  ("ReservedDBInstancesOfferingId", (String.to_query f)));
           Util.option_map v.reserved_d_b_instance_id
             (fun f ->
                Query.Pair ("ReservedDBInstanceId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("recurring_charges",
                (RecurringChargeList.to_json v.recurring_charges));
           Util.option_map v.state (fun f -> ("state", (String.to_json f)));
           Util.option_map v.multi_a_z
             (fun f -> ("multi_a_z", (Boolean.to_json f)));
           Util.option_map v.offering_type
             (fun f -> ("offering_type", (String.to_json f)));
           Util.option_map v.product_description
             (fun f -> ("product_description", (String.to_json f)));
           Util.option_map v.d_b_instance_count
             (fun f -> ("d_b_instance_count", (Integer.to_json f)));
           Util.option_map v.currency_code
             (fun f -> ("currency_code", (String.to_json f)));
           Util.option_map v.usage_price
             (fun f -> ("usage_price", (Double.to_json f)));
           Util.option_map v.fixed_price
             (fun f -> ("fixed_price", (Double.to_json f)));
           Util.option_map v.duration
             (fun f -> ("duration", (Integer.to_json f)));
           Util.option_map v.start_time
             (fun f -> ("start_time", (DateTime.to_json f)));
           Util.option_map v.d_b_instance_class
             (fun f -> ("d_b_instance_class", (String.to_json f)));
           Util.option_map v.reserved_d_b_instances_offering_id
             (fun f ->
                ("reserved_d_b_instances_offering_id", (String.to_json f)));
           Util.option_map v.reserved_d_b_instance_id
             (fun f -> ("reserved_d_b_instance_id", (String.to_json f)))])
    let of_json j =
      {
        reserved_d_b_instance_id =
          (Util.option_map (Json.lookup j "reserved_d_b_instance_id")
             String.of_json);
        reserved_d_b_instances_offering_id =
          (Util.option_map
             (Json.lookup j "reserved_d_b_instances_offering_id")
             String.of_json);
        d_b_instance_class =
          (Util.option_map (Json.lookup j "d_b_instance_class")
             String.of_json);
        start_time =
          (Util.option_map (Json.lookup j "start_time") DateTime.of_json);
        duration =
          (Util.option_map (Json.lookup j "duration") Integer.of_json);
        fixed_price =
          (Util.option_map (Json.lookup j "fixed_price") Double.of_json);
        usage_price =
          (Util.option_map (Json.lookup j "usage_price") Double.of_json);
        currency_code =
          (Util.option_map (Json.lookup j "currency_code") String.of_json);
        d_b_instance_count =
          (Util.option_map (Json.lookup j "d_b_instance_count")
             Integer.of_json);
        product_description =
          (Util.option_map (Json.lookup j "product_description")
             String.of_json);
        offering_type =
          (Util.option_map (Json.lookup j "offering_type") String.of_json);
        multi_a_z =
          (Util.option_map (Json.lookup j "multi_a_z") Boolean.of_json);
        state = (Util.option_map (Json.lookup j "state") String.of_json);
        recurring_charges =
          (RecurringChargeList.of_json
             (Util.of_option_exn (Json.lookup j "recurring_charges")))
      }
  end
module ResourcePendingMaintenanceActions =
  struct
    type t =
      {
      resource_identifier: String.t option ;
      pending_maintenance_action_details: PendingMaintenanceActionDetails.t }
    let make ?resource_identifier  ?(pending_maintenance_action_details= []) 
      () = { resource_identifier; pending_maintenance_action_details }
    let parse xml =
      Some
        {
          resource_identifier =
            (Util.option_bind (Xml.member "ResourceIdentifier" xml)
               String.parse);
          pending_maintenance_action_details =
            (Util.of_option []
               (Util.option_bind
                  (Xml.member "PendingMaintenanceActionDetails" xml)
                  PendingMaintenanceActionDetails.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("PendingMaintenanceActionDetails.member",
                   (PendingMaintenanceActionDetails.to_query
                      v.pending_maintenance_action_details)));
           Util.option_map v.resource_identifier
             (fun f -> Query.Pair ("ResourceIdentifier", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("pending_maintenance_action_details",
                (PendingMaintenanceActionDetails.to_json
                   v.pending_maintenance_action_details));
           Util.option_map v.resource_identifier
             (fun f -> ("resource_identifier", (String.to_json f)))])
    let of_json j =
      {
        resource_identifier =
          (Util.option_map (Json.lookup j "resource_identifier")
             String.of_json);
        pending_maintenance_action_details =
          (PendingMaintenanceActionDetails.of_json
             (Util.of_option_exn
                (Json.lookup j "pending_maintenance_action_details")))
      }
  end
module DBEngineVersion =
  struct
    type t =
      {
      engine: String.t option ;
      engine_version: String.t option ;
      d_b_parameter_group_family: String.t option ;
      d_b_engine_description: String.t option ;
      d_b_engine_version_description: String.t option ;
      default_character_set: CharacterSet.t option ;
      supported_character_sets: SupportedCharacterSetsList.t }
    let make ?engine  ?engine_version  ?d_b_parameter_group_family 
      ?d_b_engine_description  ?d_b_engine_version_description 
      ?default_character_set  ?(supported_character_sets= [])  () =
      {
        engine;
        engine_version;
        d_b_parameter_group_family;
        d_b_engine_description;
        d_b_engine_version_description;
        default_character_set;
        supported_character_sets
      }
    let parse xml =
      Some
        {
          engine = (Util.option_bind (Xml.member "Engine" xml) String.parse);
          engine_version =
            (Util.option_bind (Xml.member "EngineVersion" xml) String.parse);
          d_b_parameter_group_family =
            (Util.option_bind (Xml.member "DBParameterGroupFamily" xml)
               String.parse);
          d_b_engine_description =
            (Util.option_bind (Xml.member "DBEngineDescription" xml)
               String.parse);
          d_b_engine_version_description =
            (Util.option_bind (Xml.member "DBEngineVersionDescription" xml)
               String.parse);
          default_character_set =
            (Util.option_bind (Xml.member "DefaultCharacterSet" xml)
               CharacterSet.parse);
          supported_character_sets =
            (Util.of_option []
               (Util.option_bind (Xml.member "SupportedCharacterSets" xml)
                  SupportedCharacterSetsList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("SupportedCharacterSets.member",
                   (SupportedCharacterSetsList.to_query
                      v.supported_character_sets)));
           Util.option_map v.default_character_set
             (fun f ->
                Query.Pair ("DefaultCharacterSet", (CharacterSet.to_query f)));
           Util.option_map v.d_b_engine_version_description
             (fun f ->
                Query.Pair
                  ("DBEngineVersionDescription", (String.to_query f)));
           Util.option_map v.d_b_engine_description
             (fun f ->
                Query.Pair ("DBEngineDescription", (String.to_query f)));
           Util.option_map v.d_b_parameter_group_family
             (fun f ->
                Query.Pair ("DBParameterGroupFamily", (String.to_query f)));
           Util.option_map v.engine_version
             (fun f -> Query.Pair ("EngineVersion", (String.to_query f)));
           Util.option_map v.engine
             (fun f -> Query.Pair ("Engine", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("supported_character_sets",
                (SupportedCharacterSetsList.to_json
                   v.supported_character_sets));
           Util.option_map v.default_character_set
             (fun f -> ("default_character_set", (CharacterSet.to_json f)));
           Util.option_map v.d_b_engine_version_description
             (fun f -> ("d_b_engine_version_description", (String.to_json f)));
           Util.option_map v.d_b_engine_description
             (fun f -> ("d_b_engine_description", (String.to_json f)));
           Util.option_map v.d_b_parameter_group_family
             (fun f -> ("d_b_parameter_group_family", (String.to_json f)));
           Util.option_map v.engine_version
             (fun f -> ("engine_version", (String.to_json f)));
           Util.option_map v.engine (fun f -> ("engine", (String.to_json f)))])
    let of_json j =
      {
        engine = (Util.option_map (Json.lookup j "engine") String.of_json);
        engine_version =
          (Util.option_map (Json.lookup j "engine_version") String.of_json);
        d_b_parameter_group_family =
          (Util.option_map (Json.lookup j "d_b_parameter_group_family")
             String.of_json);
        d_b_engine_description =
          (Util.option_map (Json.lookup j "d_b_engine_description")
             String.of_json);
        d_b_engine_version_description =
          (Util.option_map (Json.lookup j "d_b_engine_version_description")
             String.of_json);
        default_character_set =
          (Util.option_map (Json.lookup j "default_character_set")
             CharacterSet.of_json);
        supported_character_sets =
          (SupportedCharacterSetsList.of_json
             (Util.of_option_exn (Json.lookup j "supported_character_sets")))
      }
  end
module DBSnapshot =
  struct
    type t =
      {
      d_b_snapshot_identifier: String.t option ;
      d_b_instance_identifier: String.t option ;
      snapshot_create_time: DateTime.t option ;
      engine: String.t option ;
      allocated_storage: Integer.t option ;
      status: String.t option ;
      port: Integer.t option ;
      availability_zone: String.t option ;
      vpc_id: String.t option ;
      instance_create_time: DateTime.t option ;
      master_username: String.t option ;
      engine_version: String.t option ;
      license_model: String.t option ;
      snapshot_type: String.t option ;
      iops: Integer.t option ;
      option_group_name: String.t option ;
      percent_progress: Integer.t option ;
      source_region: String.t option ;
      source_d_b_snapshot_identifier: String.t option ;
      storage_type: String.t option ;
      tde_credential_arn: String.t option ;
      encrypted: Boolean.t option ;
      kms_key_id: String.t option }
    let make ?d_b_snapshot_identifier  ?d_b_instance_identifier 
      ?snapshot_create_time  ?engine  ?allocated_storage  ?status  ?port 
      ?availability_zone  ?vpc_id  ?instance_create_time  ?master_username 
      ?engine_version  ?license_model  ?snapshot_type  ?iops 
      ?option_group_name  ?percent_progress  ?source_region 
      ?source_d_b_snapshot_identifier  ?storage_type  ?tde_credential_arn 
      ?encrypted  ?kms_key_id  () =
      {
        d_b_snapshot_identifier;
        d_b_instance_identifier;
        snapshot_create_time;
        engine;
        allocated_storage;
        status;
        port;
        availability_zone;
        vpc_id;
        instance_create_time;
        master_username;
        engine_version;
        license_model;
        snapshot_type;
        iops;
        option_group_name;
        percent_progress;
        source_region;
        source_d_b_snapshot_identifier;
        storage_type;
        tde_credential_arn;
        encrypted;
        kms_key_id
      }
    let parse xml =
      Some
        {
          d_b_snapshot_identifier =
            (Util.option_bind (Xml.member "DBSnapshotIdentifier" xml)
               String.parse);
          d_b_instance_identifier =
            (Util.option_bind (Xml.member "DBInstanceIdentifier" xml)
               String.parse);
          snapshot_create_time =
            (Util.option_bind (Xml.member "SnapshotCreateTime" xml)
               DateTime.parse);
          engine = (Util.option_bind (Xml.member "Engine" xml) String.parse);
          allocated_storage =
            (Util.option_bind (Xml.member "AllocatedStorage" xml)
               Integer.parse);
          status = (Util.option_bind (Xml.member "Status" xml) String.parse);
          port = (Util.option_bind (Xml.member "Port" xml) Integer.parse);
          availability_zone =
            (Util.option_bind (Xml.member "AvailabilityZone" xml)
               String.parse);
          vpc_id = (Util.option_bind (Xml.member "VpcId" xml) String.parse);
          instance_create_time =
            (Util.option_bind (Xml.member "InstanceCreateTime" xml)
               DateTime.parse);
          master_username =
            (Util.option_bind (Xml.member "MasterUsername" xml) String.parse);
          engine_version =
            (Util.option_bind (Xml.member "EngineVersion" xml) String.parse);
          license_model =
            (Util.option_bind (Xml.member "LicenseModel" xml) String.parse);
          snapshot_type =
            (Util.option_bind (Xml.member "SnapshotType" xml) String.parse);
          iops = (Util.option_bind (Xml.member "Iops" xml) Integer.parse);
          option_group_name =
            (Util.option_bind (Xml.member "OptionGroupName" xml) String.parse);
          percent_progress =
            (Util.option_bind (Xml.member "PercentProgress" xml)
               Integer.parse);
          source_region =
            (Util.option_bind (Xml.member "SourceRegion" xml) String.parse);
          source_d_b_snapshot_identifier =
            (Util.option_bind (Xml.member "SourceDBSnapshotIdentifier" xml)
               String.parse);
          storage_type =
            (Util.option_bind (Xml.member "StorageType" xml) String.parse);
          tde_credential_arn =
            (Util.option_bind (Xml.member "TdeCredentialArn" xml)
               String.parse);
          encrypted =
            (Util.option_bind (Xml.member "Encrypted" xml) Boolean.parse);
          kms_key_id =
            (Util.option_bind (Xml.member "KmsKeyId" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.kms_key_id
              (fun f -> Query.Pair ("KmsKeyId", (String.to_query f)));
           Util.option_map v.encrypted
             (fun f -> Query.Pair ("Encrypted", (Boolean.to_query f)));
           Util.option_map v.tde_credential_arn
             (fun f -> Query.Pair ("TdeCredentialArn", (String.to_query f)));
           Util.option_map v.storage_type
             (fun f -> Query.Pair ("StorageType", (String.to_query f)));
           Util.option_map v.source_d_b_snapshot_identifier
             (fun f ->
                Query.Pair
                  ("SourceDBSnapshotIdentifier", (String.to_query f)));
           Util.option_map v.source_region
             (fun f -> Query.Pair ("SourceRegion", (String.to_query f)));
           Util.option_map v.percent_progress
             (fun f -> Query.Pair ("PercentProgress", (Integer.to_query f)));
           Util.option_map v.option_group_name
             (fun f -> Query.Pair ("OptionGroupName", (String.to_query f)));
           Util.option_map v.iops
             (fun f -> Query.Pair ("Iops", (Integer.to_query f)));
           Util.option_map v.snapshot_type
             (fun f -> Query.Pair ("SnapshotType", (String.to_query f)));
           Util.option_map v.license_model
             (fun f -> Query.Pair ("LicenseModel", (String.to_query f)));
           Util.option_map v.engine_version
             (fun f -> Query.Pair ("EngineVersion", (String.to_query f)));
           Util.option_map v.master_username
             (fun f -> Query.Pair ("MasterUsername", (String.to_query f)));
           Util.option_map v.instance_create_time
             (fun f ->
                Query.Pair ("InstanceCreateTime", (DateTime.to_query f)));
           Util.option_map v.vpc_id
             (fun f -> Query.Pair ("VpcId", (String.to_query f)));
           Util.option_map v.availability_zone
             (fun f -> Query.Pair ("AvailabilityZone", (String.to_query f)));
           Util.option_map v.port
             (fun f -> Query.Pair ("Port", (Integer.to_query f)));
           Util.option_map v.status
             (fun f -> Query.Pair ("Status", (String.to_query f)));
           Util.option_map v.allocated_storage
             (fun f -> Query.Pair ("AllocatedStorage", (Integer.to_query f)));
           Util.option_map v.engine
             (fun f -> Query.Pair ("Engine", (String.to_query f)));
           Util.option_map v.snapshot_create_time
             (fun f ->
                Query.Pair ("SnapshotCreateTime", (DateTime.to_query f)));
           Util.option_map v.d_b_instance_identifier
             (fun f ->
                Query.Pair ("DBInstanceIdentifier", (String.to_query f)));
           Util.option_map v.d_b_snapshot_identifier
             (fun f ->
                Query.Pair ("DBSnapshotIdentifier", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.kms_key_id
              (fun f -> ("kms_key_id", (String.to_json f)));
           Util.option_map v.encrypted
             (fun f -> ("encrypted", (Boolean.to_json f)));
           Util.option_map v.tde_credential_arn
             (fun f -> ("tde_credential_arn", (String.to_json f)));
           Util.option_map v.storage_type
             (fun f -> ("storage_type", (String.to_json f)));
           Util.option_map v.source_d_b_snapshot_identifier
             (fun f -> ("source_d_b_snapshot_identifier", (String.to_json f)));
           Util.option_map v.source_region
             (fun f -> ("source_region", (String.to_json f)));
           Util.option_map v.percent_progress
             (fun f -> ("percent_progress", (Integer.to_json f)));
           Util.option_map v.option_group_name
             (fun f -> ("option_group_name", (String.to_json f)));
           Util.option_map v.iops (fun f -> ("iops", (Integer.to_json f)));
           Util.option_map v.snapshot_type
             (fun f -> ("snapshot_type", (String.to_json f)));
           Util.option_map v.license_model
             (fun f -> ("license_model", (String.to_json f)));
           Util.option_map v.engine_version
             (fun f -> ("engine_version", (String.to_json f)));
           Util.option_map v.master_username
             (fun f -> ("master_username", (String.to_json f)));
           Util.option_map v.instance_create_time
             (fun f -> ("instance_create_time", (DateTime.to_json f)));
           Util.option_map v.vpc_id (fun f -> ("vpc_id", (String.to_json f)));
           Util.option_map v.availability_zone
             (fun f -> ("availability_zone", (String.to_json f)));
           Util.option_map v.port (fun f -> ("port", (Integer.to_json f)));
           Util.option_map v.status (fun f -> ("status", (String.to_json f)));
           Util.option_map v.allocated_storage
             (fun f -> ("allocated_storage", (Integer.to_json f)));
           Util.option_map v.engine (fun f -> ("engine", (String.to_json f)));
           Util.option_map v.snapshot_create_time
             (fun f -> ("snapshot_create_time", (DateTime.to_json f)));
           Util.option_map v.d_b_instance_identifier
             (fun f -> ("d_b_instance_identifier", (String.to_json f)));
           Util.option_map v.d_b_snapshot_identifier
             (fun f -> ("d_b_snapshot_identifier", (String.to_json f)))])
    let of_json j =
      {
        d_b_snapshot_identifier =
          (Util.option_map (Json.lookup j "d_b_snapshot_identifier")
             String.of_json);
        d_b_instance_identifier =
          (Util.option_map (Json.lookup j "d_b_instance_identifier")
             String.of_json);
        snapshot_create_time =
          (Util.option_map (Json.lookup j "snapshot_create_time")
             DateTime.of_json);
        engine = (Util.option_map (Json.lookup j "engine") String.of_json);
        allocated_storage =
          (Util.option_map (Json.lookup j "allocated_storage")
             Integer.of_json);
        status = (Util.option_map (Json.lookup j "status") String.of_json);
        port = (Util.option_map (Json.lookup j "port") Integer.of_json);
        availability_zone =
          (Util.option_map (Json.lookup j "availability_zone") String.of_json);
        vpc_id = (Util.option_map (Json.lookup j "vpc_id") String.of_json);
        instance_create_time =
          (Util.option_map (Json.lookup j "instance_create_time")
             DateTime.of_json);
        master_username =
          (Util.option_map (Json.lookup j "master_username") String.of_json);
        engine_version =
          (Util.option_map (Json.lookup j "engine_version") String.of_json);
        license_model =
          (Util.option_map (Json.lookup j "license_model") String.of_json);
        snapshot_type =
          (Util.option_map (Json.lookup j "snapshot_type") String.of_json);
        iops = (Util.option_map (Json.lookup j "iops") Integer.of_json);
        option_group_name =
          (Util.option_map (Json.lookup j "option_group_name") String.of_json);
        percent_progress =
          (Util.option_map (Json.lookup j "percent_progress") Integer.of_json);
        source_region =
          (Util.option_map (Json.lookup j "source_region") String.of_json);
        source_d_b_snapshot_identifier =
          (Util.option_map (Json.lookup j "source_d_b_snapshot_identifier")
             String.of_json);
        storage_type =
          (Util.option_map (Json.lookup j "storage_type") String.of_json);
        tde_credential_arn =
          (Util.option_map (Json.lookup j "tde_credential_arn")
             String.of_json);
        encrypted =
          (Util.option_map (Json.lookup j "encrypted") Boolean.of_json);
        kms_key_id =
          (Util.option_map (Json.lookup j "kms_key_id") String.of_json)
      }
  end
module EventCategoriesMap =
  struct
    type t =
      {
      source_type: String.t option ;
      event_categories: EventCategoriesList.t }
    let make ?source_type  ?(event_categories= [])  () =
      { source_type; event_categories }
    let parse xml =
      Some
        {
          source_type =
            (Util.option_bind (Xml.member "SourceType" xml) String.parse);
          event_categories =
            (Util.of_option []
               (Util.option_bind (Xml.member "EventCategories" xml)
                  EventCategoriesList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("EventCategories.member",
                   (EventCategoriesList.to_query v.event_categories)));
           Util.option_map v.source_type
             (fun f -> Query.Pair ("SourceType", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("event_categories",
                (EventCategoriesList.to_json v.event_categories));
           Util.option_map v.source_type
             (fun f -> ("source_type", (String.to_json f)))])
    let of_json j =
      {
        source_type =
          (Util.option_map (Json.lookup j "source_type") String.of_json);
        event_categories =
          (EventCategoriesList.of_json
             (Util.of_option_exn (Json.lookup j "event_categories")))
      }
  end
module DBInstance =
  struct
    type t =
      {
      d_b_instance_identifier: String.t option ;
      d_b_instance_class: String.t option ;
      engine: String.t option ;
      d_b_instance_status: String.t option ;
      master_username: String.t option ;
      d_b_name: String.t option ;
      endpoint: Endpoint.t option ;
      allocated_storage: Integer.t option ;
      instance_create_time: DateTime.t option ;
      preferred_backup_window: String.t option ;
      backup_retention_period: Integer.t option ;
      d_b_security_groups: DBSecurityGroupMembershipList.t ;
      vpc_security_groups: VpcSecurityGroupMembershipList.t ;
      d_b_parameter_groups: DBParameterGroupStatusList.t ;
      availability_zone: String.t option ;
      d_b_subnet_group: DBSubnetGroup.t option ;
      preferred_maintenance_window: String.t option ;
      pending_modified_values: PendingModifiedValues.t option ;
      latest_restorable_time: DateTime.t option ;
      multi_a_z: Boolean.t option ;
      engine_version: String.t option ;
      auto_minor_version_upgrade: Boolean.t option ;
      read_replica_source_d_b_instance_identifier: String.t option ;
      read_replica_d_b_instance_identifiers:
        ReadReplicaDBInstanceIdentifierList.t ;
      license_model: String.t option ;
      iops: Integer.t option ;
      option_group_memberships: OptionGroupMembershipList.t ;
      character_set_name: String.t option ;
      secondary_availability_zone: String.t option ;
      publicly_accessible: Boolean.t option ;
      status_infos: DBInstanceStatusInfoList.t ;
      storage_type: String.t option ;
      tde_credential_arn: String.t option ;
      db_instance_port: Integer.t option ;
      d_b_cluster_identifier: String.t option ;
      storage_encrypted: Boolean.t option ;
      kms_key_id: String.t option ;
      dbi_resource_id: String.t option ;
      c_a_certificate_identifier: String.t option ;
      domain_memberships: DomainMembershipList.t ;
      copy_tags_to_snapshot: Boolean.t option }
    let make ?d_b_instance_identifier  ?d_b_instance_class  ?engine 
      ?d_b_instance_status  ?master_username  ?d_b_name  ?endpoint 
      ?allocated_storage  ?instance_create_time  ?preferred_backup_window 
      ?backup_retention_period  ?(d_b_security_groups= []) 
      ?(vpc_security_groups= [])  ?(d_b_parameter_groups= []) 
      ?availability_zone  ?d_b_subnet_group  ?preferred_maintenance_window 
      ?pending_modified_values  ?latest_restorable_time  ?multi_a_z 
      ?engine_version  ?auto_minor_version_upgrade 
      ?read_replica_source_d_b_instance_identifier 
      ?(read_replica_d_b_instance_identifiers= [])  ?license_model  ?iops 
      ?(option_group_memberships= [])  ?character_set_name 
      ?secondary_availability_zone  ?publicly_accessible  ?(status_infos= [])
       ?storage_type  ?tde_credential_arn  ?db_instance_port 
      ?d_b_cluster_identifier  ?storage_encrypted  ?kms_key_id 
      ?dbi_resource_id  ?c_a_certificate_identifier  ?(domain_memberships=
      [])  ?copy_tags_to_snapshot  () =
      {
        d_b_instance_identifier;
        d_b_instance_class;
        engine;
        d_b_instance_status;
        master_username;
        d_b_name;
        endpoint;
        allocated_storage;
        instance_create_time;
        preferred_backup_window;
        backup_retention_period;
        d_b_security_groups;
        vpc_security_groups;
        d_b_parameter_groups;
        availability_zone;
        d_b_subnet_group;
        preferred_maintenance_window;
        pending_modified_values;
        latest_restorable_time;
        multi_a_z;
        engine_version;
        auto_minor_version_upgrade;
        read_replica_source_d_b_instance_identifier;
        read_replica_d_b_instance_identifiers;
        license_model;
        iops;
        option_group_memberships;
        character_set_name;
        secondary_availability_zone;
        publicly_accessible;
        status_infos;
        storage_type;
        tde_credential_arn;
        db_instance_port;
        d_b_cluster_identifier;
        storage_encrypted;
        kms_key_id;
        dbi_resource_id;
        c_a_certificate_identifier;
        domain_memberships;
        copy_tags_to_snapshot
      }
    let parse xml =
      Some
        {
          d_b_instance_identifier =
            (Util.option_bind (Xml.member "DBInstanceIdentifier" xml)
               String.parse);
          d_b_instance_class =
            (Util.option_bind (Xml.member "DBInstanceClass" xml) String.parse);
          engine = (Util.option_bind (Xml.member "Engine" xml) String.parse);
          d_b_instance_status =
            (Util.option_bind (Xml.member "DBInstanceStatus" xml)
               String.parse);
          master_username =
            (Util.option_bind (Xml.member "MasterUsername" xml) String.parse);
          d_b_name =
            (Util.option_bind (Xml.member "DBName" xml) String.parse);
          endpoint =
            (Util.option_bind (Xml.member "Endpoint" xml) Endpoint.parse);
          allocated_storage =
            (Util.option_bind (Xml.member "AllocatedStorage" xml)
               Integer.parse);
          instance_create_time =
            (Util.option_bind (Xml.member "InstanceCreateTime" xml)
               DateTime.parse);
          preferred_backup_window =
            (Util.option_bind (Xml.member "PreferredBackupWindow" xml)
               String.parse);
          backup_retention_period =
            (Util.option_bind (Xml.member "BackupRetentionPeriod" xml)
               Integer.parse);
          d_b_security_groups =
            (Util.of_option []
               (Util.option_bind (Xml.member "DBSecurityGroups" xml)
                  DBSecurityGroupMembershipList.parse));
          vpc_security_groups =
            (Util.of_option []
               (Util.option_bind (Xml.member "VpcSecurityGroups" xml)
                  VpcSecurityGroupMembershipList.parse));
          d_b_parameter_groups =
            (Util.of_option []
               (Util.option_bind (Xml.member "DBParameterGroups" xml)
                  DBParameterGroupStatusList.parse));
          availability_zone =
            (Util.option_bind (Xml.member "AvailabilityZone" xml)
               String.parse);
          d_b_subnet_group =
            (Util.option_bind (Xml.member "DBSubnetGroup" xml)
               DBSubnetGroup.parse);
          preferred_maintenance_window =
            (Util.option_bind (Xml.member "PreferredMaintenanceWindow" xml)
               String.parse);
          pending_modified_values =
            (Util.option_bind (Xml.member "PendingModifiedValues" xml)
               PendingModifiedValues.parse);
          latest_restorable_time =
            (Util.option_bind (Xml.member "LatestRestorableTime" xml)
               DateTime.parse);
          multi_a_z =
            (Util.option_bind (Xml.member "MultiAZ" xml) Boolean.parse);
          engine_version =
            (Util.option_bind (Xml.member "EngineVersion" xml) String.parse);
          auto_minor_version_upgrade =
            (Util.option_bind (Xml.member "AutoMinorVersionUpgrade" xml)
               Boolean.parse);
          read_replica_source_d_b_instance_identifier =
            (Util.option_bind
               (Xml.member "ReadReplicaSourceDBInstanceIdentifier" xml)
               String.parse);
          read_replica_d_b_instance_identifiers =
            (Util.of_option []
               (Util.option_bind
                  (Xml.member "ReadReplicaDBInstanceIdentifiers" xml)
                  ReadReplicaDBInstanceIdentifierList.parse));
          license_model =
            (Util.option_bind (Xml.member "LicenseModel" xml) String.parse);
          iops = (Util.option_bind (Xml.member "Iops" xml) Integer.parse);
          option_group_memberships =
            (Util.of_option []
               (Util.option_bind (Xml.member "OptionGroupMemberships" xml)
                  OptionGroupMembershipList.parse));
          character_set_name =
            (Util.option_bind (Xml.member "CharacterSetName" xml)
               String.parse);
          secondary_availability_zone =
            (Util.option_bind (Xml.member "SecondaryAvailabilityZone" xml)
               String.parse);
          publicly_accessible =
            (Util.option_bind (Xml.member "PubliclyAccessible" xml)
               Boolean.parse);
          status_infos =
            (Util.of_option []
               (Util.option_bind (Xml.member "StatusInfos" xml)
                  DBInstanceStatusInfoList.parse));
          storage_type =
            (Util.option_bind (Xml.member "StorageType" xml) String.parse);
          tde_credential_arn =
            (Util.option_bind (Xml.member "TdeCredentialArn" xml)
               String.parse);
          db_instance_port =
            (Util.option_bind (Xml.member "DbInstancePort" xml) Integer.parse);
          d_b_cluster_identifier =
            (Util.option_bind (Xml.member "DBClusterIdentifier" xml)
               String.parse);
          storage_encrypted =
            (Util.option_bind (Xml.member "StorageEncrypted" xml)
               Boolean.parse);
          kms_key_id =
            (Util.option_bind (Xml.member "KmsKeyId" xml) String.parse);
          dbi_resource_id =
            (Util.option_bind (Xml.member "DbiResourceId" xml) String.parse);
          c_a_certificate_identifier =
            (Util.option_bind (Xml.member "CACertificateIdentifier" xml)
               String.parse);
          domain_memberships =
            (Util.of_option []
               (Util.option_bind (Xml.member "DomainMemberships" xml)
                  DomainMembershipList.parse));
          copy_tags_to_snapshot =
            (Util.option_bind (Xml.member "CopyTagsToSnapshot" xml)
               Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.copy_tags_to_snapshot
              (fun f ->
                 Query.Pair ("CopyTagsToSnapshot", (Boolean.to_query f)));
           Some
             (Query.Pair
                ("DomainMemberships.member",
                  (DomainMembershipList.to_query v.domain_memberships)));
           Util.option_map v.c_a_certificate_identifier
             (fun f ->
                Query.Pair ("CACertificateIdentifier", (String.to_query f)));
           Util.option_map v.dbi_resource_id
             (fun f -> Query.Pair ("DbiResourceId", (String.to_query f)));
           Util.option_map v.kms_key_id
             (fun f -> Query.Pair ("KmsKeyId", (String.to_query f)));
           Util.option_map v.storage_encrypted
             (fun f -> Query.Pair ("StorageEncrypted", (Boolean.to_query f)));
           Util.option_map v.d_b_cluster_identifier
             (fun f ->
                Query.Pair ("DBClusterIdentifier", (String.to_query f)));
           Util.option_map v.db_instance_port
             (fun f -> Query.Pair ("DbInstancePort", (Integer.to_query f)));
           Util.option_map v.tde_credential_arn
             (fun f -> Query.Pair ("TdeCredentialArn", (String.to_query f)));
           Util.option_map v.storage_type
             (fun f -> Query.Pair ("StorageType", (String.to_query f)));
           Some
             (Query.Pair
                ("StatusInfos.member",
                  (DBInstanceStatusInfoList.to_query v.status_infos)));
           Util.option_map v.publicly_accessible
             (fun f ->
                Query.Pair ("PubliclyAccessible", (Boolean.to_query f)));
           Util.option_map v.secondary_availability_zone
             (fun f ->
                Query.Pair ("SecondaryAvailabilityZone", (String.to_query f)));
           Util.option_map v.character_set_name
             (fun f -> Query.Pair ("CharacterSetName", (String.to_query f)));
           Some
             (Query.Pair
                ("OptionGroupMemberships.member",
                  (OptionGroupMembershipList.to_query
                     v.option_group_memberships)));
           Util.option_map v.iops
             (fun f -> Query.Pair ("Iops", (Integer.to_query f)));
           Util.option_map v.license_model
             (fun f -> Query.Pair ("LicenseModel", (String.to_query f)));
           Some
             (Query.Pair
                ("ReadReplicaDBInstanceIdentifiers.member",
                  (ReadReplicaDBInstanceIdentifierList.to_query
                     v.read_replica_d_b_instance_identifiers)));
           Util.option_map v.read_replica_source_d_b_instance_identifier
             (fun f ->
                Query.Pair
                  ("ReadReplicaSourceDBInstanceIdentifier",
                    (String.to_query f)));
           Util.option_map v.auto_minor_version_upgrade
             (fun f ->
                Query.Pair ("AutoMinorVersionUpgrade", (Boolean.to_query f)));
           Util.option_map v.engine_version
             (fun f -> Query.Pair ("EngineVersion", (String.to_query f)));
           Util.option_map v.multi_a_z
             (fun f -> Query.Pair ("MultiAZ", (Boolean.to_query f)));
           Util.option_map v.latest_restorable_time
             (fun f ->
                Query.Pair ("LatestRestorableTime", (DateTime.to_query f)));
           Util.option_map v.pending_modified_values
             (fun f ->
                Query.Pair
                  ("PendingModifiedValues",
                    (PendingModifiedValues.to_query f)));
           Util.option_map v.preferred_maintenance_window
             (fun f ->
                Query.Pair
                  ("PreferredMaintenanceWindow", (String.to_query f)));
           Util.option_map v.d_b_subnet_group
             (fun f ->
                Query.Pair ("DBSubnetGroup", (DBSubnetGroup.to_query f)));
           Util.option_map v.availability_zone
             (fun f -> Query.Pair ("AvailabilityZone", (String.to_query f)));
           Some
             (Query.Pair
                ("DBParameterGroups.member",
                  (DBParameterGroupStatusList.to_query v.d_b_parameter_groups)));
           Some
             (Query.Pair
                ("VpcSecurityGroups.member",
                  (VpcSecurityGroupMembershipList.to_query
                     v.vpc_security_groups)));
           Some
             (Query.Pair
                ("DBSecurityGroups.member",
                  (DBSecurityGroupMembershipList.to_query
                     v.d_b_security_groups)));
           Util.option_map v.backup_retention_period
             (fun f ->
                Query.Pair ("BackupRetentionPeriod", (Integer.to_query f)));
           Util.option_map v.preferred_backup_window
             (fun f ->
                Query.Pair ("PreferredBackupWindow", (String.to_query f)));
           Util.option_map v.instance_create_time
             (fun f ->
                Query.Pair ("InstanceCreateTime", (DateTime.to_query f)));
           Util.option_map v.allocated_storage
             (fun f -> Query.Pair ("AllocatedStorage", (Integer.to_query f)));
           Util.option_map v.endpoint
             (fun f -> Query.Pair ("Endpoint", (Endpoint.to_query f)));
           Util.option_map v.d_b_name
             (fun f -> Query.Pair ("DBName", (String.to_query f)));
           Util.option_map v.master_username
             (fun f -> Query.Pair ("MasterUsername", (String.to_query f)));
           Util.option_map v.d_b_instance_status
             (fun f -> Query.Pair ("DBInstanceStatus", (String.to_query f)));
           Util.option_map v.engine
             (fun f -> Query.Pair ("Engine", (String.to_query f)));
           Util.option_map v.d_b_instance_class
             (fun f -> Query.Pair ("DBInstanceClass", (String.to_query f)));
           Util.option_map v.d_b_instance_identifier
             (fun f ->
                Query.Pair ("DBInstanceIdentifier", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.copy_tags_to_snapshot
              (fun f -> ("copy_tags_to_snapshot", (Boolean.to_json f)));
           Some
             ("domain_memberships",
               (DomainMembershipList.to_json v.domain_memberships));
           Util.option_map v.c_a_certificate_identifier
             (fun f -> ("c_a_certificate_identifier", (String.to_json f)));
           Util.option_map v.dbi_resource_id
             (fun f -> ("dbi_resource_id", (String.to_json f)));
           Util.option_map v.kms_key_id
             (fun f -> ("kms_key_id", (String.to_json f)));
           Util.option_map v.storage_encrypted
             (fun f -> ("storage_encrypted", (Boolean.to_json f)));
           Util.option_map v.d_b_cluster_identifier
             (fun f -> ("d_b_cluster_identifier", (String.to_json f)));
           Util.option_map v.db_instance_port
             (fun f -> ("db_instance_port", (Integer.to_json f)));
           Util.option_map v.tde_credential_arn
             (fun f -> ("tde_credential_arn", (String.to_json f)));
           Util.option_map v.storage_type
             (fun f -> ("storage_type", (String.to_json f)));
           Some
             ("status_infos",
               (DBInstanceStatusInfoList.to_json v.status_infos));
           Util.option_map v.publicly_accessible
             (fun f -> ("publicly_accessible", (Boolean.to_json f)));
           Util.option_map v.secondary_availability_zone
             (fun f -> ("secondary_availability_zone", (String.to_json f)));
           Util.option_map v.character_set_name
             (fun f -> ("character_set_name", (String.to_json f)));
           Some
             ("option_group_memberships",
               (OptionGroupMembershipList.to_json v.option_group_memberships));
           Util.option_map v.iops (fun f -> ("iops", (Integer.to_json f)));
           Util.option_map v.license_model
             (fun f -> ("license_model", (String.to_json f)));
           Some
             ("read_replica_d_b_instance_identifiers",
               (ReadReplicaDBInstanceIdentifierList.to_json
                  v.read_replica_d_b_instance_identifiers));
           Util.option_map v.read_replica_source_d_b_instance_identifier
             (fun f ->
                ("read_replica_source_d_b_instance_identifier",
                  (String.to_json f)));
           Util.option_map v.auto_minor_version_upgrade
             (fun f -> ("auto_minor_version_upgrade", (Boolean.to_json f)));
           Util.option_map v.engine_version
             (fun f -> ("engine_version", (String.to_json f)));
           Util.option_map v.multi_a_z
             (fun f -> ("multi_a_z", (Boolean.to_json f)));
           Util.option_map v.latest_restorable_time
             (fun f -> ("latest_restorable_time", (DateTime.to_json f)));
           Util.option_map v.pending_modified_values
             (fun f ->
                ("pending_modified_values",
                  (PendingModifiedValues.to_json f)));
           Util.option_map v.preferred_maintenance_window
             (fun f -> ("preferred_maintenance_window", (String.to_json f)));
           Util.option_map v.d_b_subnet_group
             (fun f -> ("d_b_subnet_group", (DBSubnetGroup.to_json f)));
           Util.option_map v.availability_zone
             (fun f -> ("availability_zone", (String.to_json f)));
           Some
             ("d_b_parameter_groups",
               (DBParameterGroupStatusList.to_json v.d_b_parameter_groups));
           Some
             ("vpc_security_groups",
               (VpcSecurityGroupMembershipList.to_json v.vpc_security_groups));
           Some
             ("d_b_security_groups",
               (DBSecurityGroupMembershipList.to_json v.d_b_security_groups));
           Util.option_map v.backup_retention_period
             (fun f -> ("backup_retention_period", (Integer.to_json f)));
           Util.option_map v.preferred_backup_window
             (fun f -> ("preferred_backup_window", (String.to_json f)));
           Util.option_map v.instance_create_time
             (fun f -> ("instance_create_time", (DateTime.to_json f)));
           Util.option_map v.allocated_storage
             (fun f -> ("allocated_storage", (Integer.to_json f)));
           Util.option_map v.endpoint
             (fun f -> ("endpoint", (Endpoint.to_json f)));
           Util.option_map v.d_b_name
             (fun f -> ("d_b_name", (String.to_json f)));
           Util.option_map v.master_username
             (fun f -> ("master_username", (String.to_json f)));
           Util.option_map v.d_b_instance_status
             (fun f -> ("d_b_instance_status", (String.to_json f)));
           Util.option_map v.engine (fun f -> ("engine", (String.to_json f)));
           Util.option_map v.d_b_instance_class
             (fun f -> ("d_b_instance_class", (String.to_json f)));
           Util.option_map v.d_b_instance_identifier
             (fun f -> ("d_b_instance_identifier", (String.to_json f)))])
    let of_json j =
      {
        d_b_instance_identifier =
          (Util.option_map (Json.lookup j "d_b_instance_identifier")
             String.of_json);
        d_b_instance_class =
          (Util.option_map (Json.lookup j "d_b_instance_class")
             String.of_json);
        engine = (Util.option_map (Json.lookup j "engine") String.of_json);
        d_b_instance_status =
          (Util.option_map (Json.lookup j "d_b_instance_status")
             String.of_json);
        master_username =
          (Util.option_map (Json.lookup j "master_username") String.of_json);
        d_b_name =
          (Util.option_map (Json.lookup j "d_b_name") String.of_json);
        endpoint =
          (Util.option_map (Json.lookup j "endpoint") Endpoint.of_json);
        allocated_storage =
          (Util.option_map (Json.lookup j "allocated_storage")
             Integer.of_json);
        instance_create_time =
          (Util.option_map (Json.lookup j "instance_create_time")
             DateTime.of_json);
        preferred_backup_window =
          (Util.option_map (Json.lookup j "preferred_backup_window")
             String.of_json);
        backup_retention_period =
          (Util.option_map (Json.lookup j "backup_retention_period")
             Integer.of_json);
        d_b_security_groups =
          (DBSecurityGroupMembershipList.of_json
             (Util.of_option_exn (Json.lookup j "d_b_security_groups")));
        vpc_security_groups =
          (VpcSecurityGroupMembershipList.of_json
             (Util.of_option_exn (Json.lookup j "vpc_security_groups")));
        d_b_parameter_groups =
          (DBParameterGroupStatusList.of_json
             (Util.of_option_exn (Json.lookup j "d_b_parameter_groups")));
        availability_zone =
          (Util.option_map (Json.lookup j "availability_zone") String.of_json);
        d_b_subnet_group =
          (Util.option_map (Json.lookup j "d_b_subnet_group")
             DBSubnetGroup.of_json);
        preferred_maintenance_window =
          (Util.option_map (Json.lookup j "preferred_maintenance_window")
             String.of_json);
        pending_modified_values =
          (Util.option_map (Json.lookup j "pending_modified_values")
             PendingModifiedValues.of_json);
        latest_restorable_time =
          (Util.option_map (Json.lookup j "latest_restorable_time")
             DateTime.of_json);
        multi_a_z =
          (Util.option_map (Json.lookup j "multi_a_z") Boolean.of_json);
        engine_version =
          (Util.option_map (Json.lookup j "engine_version") String.of_json);
        auto_minor_version_upgrade =
          (Util.option_map (Json.lookup j "auto_minor_version_upgrade")
             Boolean.of_json);
        read_replica_source_d_b_instance_identifier =
          (Util.option_map
             (Json.lookup j "read_replica_source_d_b_instance_identifier")
             String.of_json);
        read_replica_d_b_instance_identifiers =
          (ReadReplicaDBInstanceIdentifierList.of_json
             (Util.of_option_exn
                (Json.lookup j "read_replica_d_b_instance_identifiers")));
        license_model =
          (Util.option_map (Json.lookup j "license_model") String.of_json);
        iops = (Util.option_map (Json.lookup j "iops") Integer.of_json);
        option_group_memberships =
          (OptionGroupMembershipList.of_json
             (Util.of_option_exn (Json.lookup j "option_group_memberships")));
        character_set_name =
          (Util.option_map (Json.lookup j "character_set_name")
             String.of_json);
        secondary_availability_zone =
          (Util.option_map (Json.lookup j "secondary_availability_zone")
             String.of_json);
        publicly_accessible =
          (Util.option_map (Json.lookup j "publicly_accessible")
             Boolean.of_json);
        status_infos =
          (DBInstanceStatusInfoList.of_json
             (Util.of_option_exn (Json.lookup j "status_infos")));
        storage_type =
          (Util.option_map (Json.lookup j "storage_type") String.of_json);
        tde_credential_arn =
          (Util.option_map (Json.lookup j "tde_credential_arn")
             String.of_json);
        db_instance_port =
          (Util.option_map (Json.lookup j "db_instance_port") Integer.of_json);
        d_b_cluster_identifier =
          (Util.option_map (Json.lookup j "d_b_cluster_identifier")
             String.of_json);
        storage_encrypted =
          (Util.option_map (Json.lookup j "storage_encrypted")
             Boolean.of_json);
        kms_key_id =
          (Util.option_map (Json.lookup j "kms_key_id") String.of_json);
        dbi_resource_id =
          (Util.option_map (Json.lookup j "dbi_resource_id") String.of_json);
        c_a_certificate_identifier =
          (Util.option_map (Json.lookup j "c_a_certificate_identifier")
             String.of_json);
        domain_memberships =
          (DomainMembershipList.of_json
             (Util.of_option_exn (Json.lookup j "domain_memberships")));
        copy_tags_to_snapshot =
          (Util.option_map (Json.lookup j "copy_tags_to_snapshot")
             Boolean.of_json)
      }
  end
module AccountQuota =
  struct
    type t =
      {
      account_quota_name: String.t option ;
      used: Long.t option ;
      max: Long.t option }
    let make ?account_quota_name  ?used  ?max  () =
      { account_quota_name; used; max }
    let parse xml =
      Some
        {
          account_quota_name =
            (Util.option_bind (Xml.member "AccountQuotaName" xml)
               String.parse);
          used = (Util.option_bind (Xml.member "Used" xml) Long.parse);
          max = (Util.option_bind (Xml.member "Max" xml) Long.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.max
              (fun f -> Query.Pair ("Max", (Long.to_query f)));
           Util.option_map v.used
             (fun f -> Query.Pair ("Used", (Long.to_query f)));
           Util.option_map v.account_quota_name
             (fun f -> Query.Pair ("AccountQuotaName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.max (fun f -> ("max", (Long.to_json f)));
           Util.option_map v.used (fun f -> ("used", (Long.to_json f)));
           Util.option_map v.account_quota_name
             (fun f -> ("account_quota_name", (String.to_json f)))])
    let of_json j =
      {
        account_quota_name =
          (Util.option_map (Json.lookup j "account_quota_name")
             String.of_json);
        used = (Util.option_map (Json.lookup j "used") Long.of_json);
        max = (Util.option_map (Json.lookup j "max") Long.of_json)
      }
  end
module DBClusterParameterGroup =
  struct
    type t =
      {
      d_b_cluster_parameter_group_name: String.t option ;
      d_b_parameter_group_family: String.t option ;
      description: String.t option }
    let make ?d_b_cluster_parameter_group_name  ?d_b_parameter_group_family 
      ?description  () =
      {
        d_b_cluster_parameter_group_name;
        d_b_parameter_group_family;
        description
      }
    let parse xml =
      Some
        {
          d_b_cluster_parameter_group_name =
            (Util.option_bind (Xml.member "DBClusterParameterGroupName" xml)
               String.parse);
          d_b_parameter_group_family =
            (Util.option_bind (Xml.member "DBParameterGroupFamily" xml)
               String.parse);
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.description
              (fun f -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.d_b_parameter_group_family
             (fun f ->
                Query.Pair ("DBParameterGroupFamily", (String.to_query f)));
           Util.option_map v.d_b_cluster_parameter_group_name
             (fun f ->
                Query.Pair
                  ("DBClusterParameterGroupName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.description
              (fun f -> ("description", (String.to_json f)));
           Util.option_map v.d_b_parameter_group_family
             (fun f -> ("d_b_parameter_group_family", (String.to_json f)));
           Util.option_map v.d_b_cluster_parameter_group_name
             (fun f ->
                ("d_b_cluster_parameter_group_name", (String.to_json f)))])
    let of_json j =
      {
        d_b_cluster_parameter_group_name =
          (Util.option_map (Json.lookup j "d_b_cluster_parameter_group_name")
             String.of_json);
        d_b_parameter_group_family =
          (Util.option_map (Json.lookup j "d_b_parameter_group_family")
             String.of_json);
        description =
          (Util.option_map (Json.lookup j "description") String.of_json)
      }
  end
module DescribeDBLogFilesDetails =
  struct
    type t =
      {
      log_file_name: String.t option ;
      last_written: Long.t option ;
      size: Long.t option }
    let make ?log_file_name  ?last_written  ?size  () =
      { log_file_name; last_written; size }
    let parse xml =
      Some
        {
          log_file_name =
            (Util.option_bind (Xml.member "LogFileName" xml) String.parse);
          last_written =
            (Util.option_bind (Xml.member "LastWritten" xml) Long.parse);
          size = (Util.option_bind (Xml.member "Size" xml) Long.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.size
              (fun f -> Query.Pair ("Size", (Long.to_query f)));
           Util.option_map v.last_written
             (fun f -> Query.Pair ("LastWritten", (Long.to_query f)));
           Util.option_map v.log_file_name
             (fun f -> Query.Pair ("LogFileName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.size (fun f -> ("size", (Long.to_json f)));
           Util.option_map v.last_written
             (fun f -> ("last_written", (Long.to_json f)));
           Util.option_map v.log_file_name
             (fun f -> ("log_file_name", (String.to_json f)))])
    let of_json j =
      {
        log_file_name =
          (Util.option_map (Json.lookup j "log_file_name") String.of_json);
        last_written =
          (Util.option_map (Json.lookup j "last_written") Long.of_json);
        size = (Util.option_map (Json.lookup j "size") Long.of_json)
      }
  end
module DBParameterGroup =
  struct
    type t =
      {
      d_b_parameter_group_name: String.t option ;
      d_b_parameter_group_family: String.t option ;
      description: String.t option }
    let make ?d_b_parameter_group_name  ?d_b_parameter_group_family 
      ?description  () =
      { d_b_parameter_group_name; d_b_parameter_group_family; description }
    let parse xml =
      Some
        {
          d_b_parameter_group_name =
            (Util.option_bind (Xml.member "DBParameterGroupName" xml)
               String.parse);
          d_b_parameter_group_family =
            (Util.option_bind (Xml.member "DBParameterGroupFamily" xml)
               String.parse);
          description =
            (Util.option_bind (Xml.member "Description" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.description
              (fun f -> Query.Pair ("Description", (String.to_query f)));
           Util.option_map v.d_b_parameter_group_family
             (fun f ->
                Query.Pair ("DBParameterGroupFamily", (String.to_query f)));
           Util.option_map v.d_b_parameter_group_name
             (fun f ->
                Query.Pair ("DBParameterGroupName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.description
              (fun f -> ("description", (String.to_json f)));
           Util.option_map v.d_b_parameter_group_family
             (fun f -> ("d_b_parameter_group_family", (String.to_json f)));
           Util.option_map v.d_b_parameter_group_name
             (fun f -> ("d_b_parameter_group_name", (String.to_json f)))])
    let of_json j =
      {
        d_b_parameter_group_name =
          (Util.option_map (Json.lookup j "d_b_parameter_group_name")
             String.of_json);
        d_b_parameter_group_family =
          (Util.option_map (Json.lookup j "d_b_parameter_group_family")
             String.of_json);
        description =
          (Util.option_map (Json.lookup j "description") String.of_json)
      }
  end
module Event =
  struct
    type t =
      {
      source_identifier: String.t option ;
      source_type: SourceType.t option ;
      message: String.t option ;
      event_categories: EventCategoriesList.t ;
      date: DateTime.t option }
    let make ?source_identifier  ?source_type  ?message  ?(event_categories=
      [])  ?date  () =
      { source_identifier; source_type; message; event_categories; date }
    let parse xml =
      Some
        {
          source_identifier =
            (Util.option_bind (Xml.member "SourceIdentifier" xml)
               String.parse);
          source_type =
            (Util.option_bind (Xml.member "SourceType" xml) SourceType.parse);
          message =
            (Util.option_bind (Xml.member "Message" xml) String.parse);
          event_categories =
            (Util.of_option []
               (Util.option_bind (Xml.member "EventCategories" xml)
                  EventCategoriesList.parse));
          date = (Util.option_bind (Xml.member "Date" xml) DateTime.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.date
              (fun f -> Query.Pair ("Date", (DateTime.to_query f)));
           Some
             (Query.Pair
                ("EventCategories.member",
                  (EventCategoriesList.to_query v.event_categories)));
           Util.option_map v.message
             (fun f -> Query.Pair ("Message", (String.to_query f)));
           Util.option_map v.source_type
             (fun f -> Query.Pair ("SourceType", (SourceType.to_query f)));
           Util.option_map v.source_identifier
             (fun f -> Query.Pair ("SourceIdentifier", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.date (fun f -> ("date", (DateTime.to_json f)));
           Some
             ("event_categories",
               (EventCategoriesList.to_json v.event_categories));
           Util.option_map v.message
             (fun f -> ("message", (String.to_json f)));
           Util.option_map v.source_type
             (fun f -> ("source_type", (SourceType.to_json f)));
           Util.option_map v.source_identifier
             (fun f -> ("source_identifier", (String.to_json f)))])
    let of_json j =
      {
        source_identifier =
          (Util.option_map (Json.lookup j "source_identifier") String.of_json);
        source_type =
          (Util.option_map (Json.lookup j "source_type") SourceType.of_json);
        message = (Util.option_map (Json.lookup j "message") String.of_json);
        event_categories =
          (EventCategoriesList.of_json
             (Util.of_option_exn (Json.lookup j "event_categories")));
        date = (Util.option_map (Json.lookup j "date") DateTime.of_json)
      }
  end
module Certificate =
  struct
    type t =
      {
      certificate_identifier: String.t option ;
      certificate_type: String.t option ;
      thumbprint: String.t option ;
      valid_from: DateTime.t option ;
      valid_till: DateTime.t option }
    let make ?certificate_identifier  ?certificate_type  ?thumbprint 
      ?valid_from  ?valid_till  () =
      {
        certificate_identifier;
        certificate_type;
        thumbprint;
        valid_from;
        valid_till
      }
    let parse xml =
      Some
        {
          certificate_identifier =
            (Util.option_bind (Xml.member "CertificateIdentifier" xml)
               String.parse);
          certificate_type =
            (Util.option_bind (Xml.member "CertificateType" xml) String.parse);
          thumbprint =
            (Util.option_bind (Xml.member "Thumbprint" xml) String.parse);
          valid_from =
            (Util.option_bind (Xml.member "ValidFrom" xml) DateTime.parse);
          valid_till =
            (Util.option_bind (Xml.member "ValidTill" xml) DateTime.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.valid_till
              (fun f -> Query.Pair ("ValidTill", (DateTime.to_query f)));
           Util.option_map v.valid_from
             (fun f -> Query.Pair ("ValidFrom", (DateTime.to_query f)));
           Util.option_map v.thumbprint
             (fun f -> Query.Pair ("Thumbprint", (String.to_query f)));
           Util.option_map v.certificate_type
             (fun f -> Query.Pair ("CertificateType", (String.to_query f)));
           Util.option_map v.certificate_identifier
             (fun f ->
                Query.Pair ("CertificateIdentifier", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.valid_till
              (fun f -> ("valid_till", (DateTime.to_json f)));
           Util.option_map v.valid_from
             (fun f -> ("valid_from", (DateTime.to_json f)));
           Util.option_map v.thumbprint
             (fun f -> ("thumbprint", (String.to_json f)));
           Util.option_map v.certificate_type
             (fun f -> ("certificate_type", (String.to_json f)));
           Util.option_map v.certificate_identifier
             (fun f -> ("certificate_identifier", (String.to_json f)))])
    let of_json j =
      {
        certificate_identifier =
          (Util.option_map (Json.lookup j "certificate_identifier")
             String.of_json);
        certificate_type =
          (Util.option_map (Json.lookup j "certificate_type") String.of_json);
        thumbprint =
          (Util.option_map (Json.lookup j "thumbprint") String.of_json);
        valid_from =
          (Util.option_map (Json.lookup j "valid_from") DateTime.of_json);
        valid_till =
          (Util.option_map (Json.lookup j "valid_till") DateTime.of_json)
      }
  end
module OptionGroup =
  struct
    type t =
      {
      option_group_name: String.t option ;
      option_group_description: String.t option ;
      engine_name: String.t option ;
      major_engine_version: String.t option ;
      options: OptionsList.t ;
      allows_vpc_and_non_vpc_instance_memberships: Boolean.t option ;
      vpc_id: String.t option }
    let make ?option_group_name  ?option_group_description  ?engine_name 
      ?major_engine_version  ?(options= []) 
      ?allows_vpc_and_non_vpc_instance_memberships  ?vpc_id  () =
      {
        option_group_name;
        option_group_description;
        engine_name;
        major_engine_version;
        options;
        allows_vpc_and_non_vpc_instance_memberships;
        vpc_id
      }
    let parse xml =
      Some
        {
          option_group_name =
            (Util.option_bind (Xml.member "OptionGroupName" xml) String.parse);
          option_group_description =
            (Util.option_bind (Xml.member "OptionGroupDescription" xml)
               String.parse);
          engine_name =
            (Util.option_bind (Xml.member "EngineName" xml) String.parse);
          major_engine_version =
            (Util.option_bind (Xml.member "MajorEngineVersion" xml)
               String.parse);
          options =
            (Util.of_option []
               (Util.option_bind (Xml.member "Options" xml) OptionsList.parse));
          allows_vpc_and_non_vpc_instance_memberships =
            (Util.option_bind
               (Xml.member "AllowsVpcAndNonVpcInstanceMemberships" xml)
               Boolean.parse);
          vpc_id = (Util.option_bind (Xml.member "VpcId" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.vpc_id
              (fun f -> Query.Pair ("VpcId", (String.to_query f)));
           Util.option_map v.allows_vpc_and_non_vpc_instance_memberships
             (fun f ->
                Query.Pair
                  ("AllowsVpcAndNonVpcInstanceMemberships",
                    (Boolean.to_query f)));
           Some
             (Query.Pair ("Options.member", (OptionsList.to_query v.options)));
           Util.option_map v.major_engine_version
             (fun f -> Query.Pair ("MajorEngineVersion", (String.to_query f)));
           Util.option_map v.engine_name
             (fun f -> Query.Pair ("EngineName", (String.to_query f)));
           Util.option_map v.option_group_description
             (fun f ->
                Query.Pair ("OptionGroupDescription", (String.to_query f)));
           Util.option_map v.option_group_name
             (fun f -> Query.Pair ("OptionGroupName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.vpc_id
              (fun f -> ("vpc_id", (String.to_json f)));
           Util.option_map v.allows_vpc_and_non_vpc_instance_memberships
             (fun f ->
                ("allows_vpc_and_non_vpc_instance_memberships",
                  (Boolean.to_json f)));
           Some ("options", (OptionsList.to_json v.options));
           Util.option_map v.major_engine_version
             (fun f -> ("major_engine_version", (String.to_json f)));
           Util.option_map v.engine_name
             (fun f -> ("engine_name", (String.to_json f)));
           Util.option_map v.option_group_description
             (fun f -> ("option_group_description", (String.to_json f)));
           Util.option_map v.option_group_name
             (fun f -> ("option_group_name", (String.to_json f)))])
    let of_json j =
      {
        option_group_name =
          (Util.option_map (Json.lookup j "option_group_name") String.of_json);
        option_group_description =
          (Util.option_map (Json.lookup j "option_group_description")
             String.of_json);
        engine_name =
          (Util.option_map (Json.lookup j "engine_name") String.of_json);
        major_engine_version =
          (Util.option_map (Json.lookup j "major_engine_version")
             String.of_json);
        options =
          (OptionsList.of_json (Util.of_option_exn (Json.lookup j "options")));
        allows_vpc_and_non_vpc_instance_memberships =
          (Util.option_map
             (Json.lookup j "allows_vpc_and_non_vpc_instance_memberships")
             Boolean.of_json);
        vpc_id = (Util.option_map (Json.lookup j "vpc_id") String.of_json)
      }
  end
module OrderableDBInstanceOption =
  struct
    type t =
      {
      engine: String.t option ;
      engine_version: String.t option ;
      d_b_instance_class: String.t option ;
      license_model: String.t option ;
      availability_zones: AvailabilityZoneList.t ;
      multi_a_z_capable: Boolean.t option ;
      read_replica_capable: Boolean.t option ;
      vpc: Boolean.t option ;
      supports_storage_encryption: Boolean.t option ;
      storage_type: String.t option ;
      supports_iops: Boolean.t option }
    let make ?engine  ?engine_version  ?d_b_instance_class  ?license_model 
      ?(availability_zones= [])  ?multi_a_z_capable  ?read_replica_capable 
      ?vpc  ?supports_storage_encryption  ?storage_type  ?supports_iops  () =
      {
        engine;
        engine_version;
        d_b_instance_class;
        license_model;
        availability_zones;
        multi_a_z_capable;
        read_replica_capable;
        vpc;
        supports_storage_encryption;
        storage_type;
        supports_iops
      }
    let parse xml =
      Some
        {
          engine = (Util.option_bind (Xml.member "Engine" xml) String.parse);
          engine_version =
            (Util.option_bind (Xml.member "EngineVersion" xml) String.parse);
          d_b_instance_class =
            (Util.option_bind (Xml.member "DBInstanceClass" xml) String.parse);
          license_model =
            (Util.option_bind (Xml.member "LicenseModel" xml) String.parse);
          availability_zones =
            (Util.of_option []
               (Util.option_bind (Xml.member "AvailabilityZones" xml)
                  AvailabilityZoneList.parse));
          multi_a_z_capable =
            (Util.option_bind (Xml.member "MultiAZCapable" xml) Boolean.parse);
          read_replica_capable =
            (Util.option_bind (Xml.member "ReadReplicaCapable" xml)
               Boolean.parse);
          vpc = (Util.option_bind (Xml.member "Vpc" xml) Boolean.parse);
          supports_storage_encryption =
            (Util.option_bind (Xml.member "SupportsStorageEncryption" xml)
               Boolean.parse);
          storage_type =
            (Util.option_bind (Xml.member "StorageType" xml) String.parse);
          supports_iops =
            (Util.option_bind (Xml.member "SupportsIops" xml) Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.supports_iops
              (fun f -> Query.Pair ("SupportsIops", (Boolean.to_query f)));
           Util.option_map v.storage_type
             (fun f -> Query.Pair ("StorageType", (String.to_query f)));
           Util.option_map v.supports_storage_encryption
             (fun f ->
                Query.Pair
                  ("SupportsStorageEncryption", (Boolean.to_query f)));
           Util.option_map v.vpc
             (fun f -> Query.Pair ("Vpc", (Boolean.to_query f)));
           Util.option_map v.read_replica_capable
             (fun f ->
                Query.Pair ("ReadReplicaCapable", (Boolean.to_query f)));
           Util.option_map v.multi_a_z_capable
             (fun f -> Query.Pair ("MultiAZCapable", (Boolean.to_query f)));
           Some
             (Query.Pair
                ("AvailabilityZones.member",
                  (AvailabilityZoneList.to_query v.availability_zones)));
           Util.option_map v.license_model
             (fun f -> Query.Pair ("LicenseModel", (String.to_query f)));
           Util.option_map v.d_b_instance_class
             (fun f -> Query.Pair ("DBInstanceClass", (String.to_query f)));
           Util.option_map v.engine_version
             (fun f -> Query.Pair ("EngineVersion", (String.to_query f)));
           Util.option_map v.engine
             (fun f -> Query.Pair ("Engine", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.supports_iops
              (fun f -> ("supports_iops", (Boolean.to_json f)));
           Util.option_map v.storage_type
             (fun f -> ("storage_type", (String.to_json f)));
           Util.option_map v.supports_storage_encryption
             (fun f -> ("supports_storage_encryption", (Boolean.to_json f)));
           Util.option_map v.vpc (fun f -> ("vpc", (Boolean.to_json f)));
           Util.option_map v.read_replica_capable
             (fun f -> ("read_replica_capable", (Boolean.to_json f)));
           Util.option_map v.multi_a_z_capable
             (fun f -> ("multi_a_z_capable", (Boolean.to_json f)));
           Some
             ("availability_zones",
               (AvailabilityZoneList.to_json v.availability_zones));
           Util.option_map v.license_model
             (fun f -> ("license_model", (String.to_json f)));
           Util.option_map v.d_b_instance_class
             (fun f -> ("d_b_instance_class", (String.to_json f)));
           Util.option_map v.engine_version
             (fun f -> ("engine_version", (String.to_json f)));
           Util.option_map v.engine (fun f -> ("engine", (String.to_json f)))])
    let of_json j =
      {
        engine = (Util.option_map (Json.lookup j "engine") String.of_json);
        engine_version =
          (Util.option_map (Json.lookup j "engine_version") String.of_json);
        d_b_instance_class =
          (Util.option_map (Json.lookup j "d_b_instance_class")
             String.of_json);
        license_model =
          (Util.option_map (Json.lookup j "license_model") String.of_json);
        availability_zones =
          (AvailabilityZoneList.of_json
             (Util.of_option_exn (Json.lookup j "availability_zones")));
        multi_a_z_capable =
          (Util.option_map (Json.lookup j "multi_a_z_capable")
             Boolean.of_json);
        read_replica_capable =
          (Util.option_map (Json.lookup j "read_replica_capable")
             Boolean.of_json);
        vpc = (Util.option_map (Json.lookup j "vpc") Boolean.of_json);
        supports_storage_encryption =
          (Util.option_map (Json.lookup j "supports_storage_encryption")
             Boolean.of_json);
        storage_type =
          (Util.option_map (Json.lookup j "storage_type") String.of_json);
        supports_iops =
          (Util.option_map (Json.lookup j "supports_iops") Boolean.of_json)
      }
  end
module DBClusterSnapshot =
  struct
    type t =
      {
      availability_zones: AvailabilityZones.t ;
      d_b_cluster_snapshot_identifier: String.t option ;
      d_b_cluster_identifier: String.t option ;
      snapshot_create_time: DateTime.t option ;
      engine: String.t option ;
      allocated_storage: Integer.t option ;
      status: String.t option ;
      port: Integer.t option ;
      vpc_id: String.t option ;
      cluster_create_time: DateTime.t option ;
      master_username: String.t option ;
      engine_version: String.t option ;
      license_model: String.t option ;
      snapshot_type: String.t option ;
      percent_progress: Integer.t option }
    let make ?(availability_zones= [])  ?d_b_cluster_snapshot_identifier 
      ?d_b_cluster_identifier  ?snapshot_create_time  ?engine 
      ?allocated_storage  ?status  ?port  ?vpc_id  ?cluster_create_time 
      ?master_username  ?engine_version  ?license_model  ?snapshot_type 
      ?percent_progress  () =
      {
        availability_zones;
        d_b_cluster_snapshot_identifier;
        d_b_cluster_identifier;
        snapshot_create_time;
        engine;
        allocated_storage;
        status;
        port;
        vpc_id;
        cluster_create_time;
        master_username;
        engine_version;
        license_model;
        snapshot_type;
        percent_progress
      }
    let parse xml =
      Some
        {
          availability_zones =
            (Util.of_option []
               (Util.option_bind (Xml.member "AvailabilityZones" xml)
                  AvailabilityZones.parse));
          d_b_cluster_snapshot_identifier =
            (Util.option_bind (Xml.member "DBClusterSnapshotIdentifier" xml)
               String.parse);
          d_b_cluster_identifier =
            (Util.option_bind (Xml.member "DBClusterIdentifier" xml)
               String.parse);
          snapshot_create_time =
            (Util.option_bind (Xml.member "SnapshotCreateTime" xml)
               DateTime.parse);
          engine = (Util.option_bind (Xml.member "Engine" xml) String.parse);
          allocated_storage =
            (Util.option_bind (Xml.member "AllocatedStorage" xml)
               Integer.parse);
          status = (Util.option_bind (Xml.member "Status" xml) String.parse);
          port = (Util.option_bind (Xml.member "Port" xml) Integer.parse);
          vpc_id = (Util.option_bind (Xml.member "VpcId" xml) String.parse);
          cluster_create_time =
            (Util.option_bind (Xml.member "ClusterCreateTime" xml)
               DateTime.parse);
          master_username =
            (Util.option_bind (Xml.member "MasterUsername" xml) String.parse);
          engine_version =
            (Util.option_bind (Xml.member "EngineVersion" xml) String.parse);
          license_model =
            (Util.option_bind (Xml.member "LicenseModel" xml) String.parse);
          snapshot_type =
            (Util.option_bind (Xml.member "SnapshotType" xml) String.parse);
          percent_progress =
            (Util.option_bind (Xml.member "PercentProgress" xml)
               Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.percent_progress
              (fun f -> Query.Pair ("PercentProgress", (Integer.to_query f)));
           Util.option_map v.snapshot_type
             (fun f -> Query.Pair ("SnapshotType", (String.to_query f)));
           Util.option_map v.license_model
             (fun f -> Query.Pair ("LicenseModel", (String.to_query f)));
           Util.option_map v.engine_version
             (fun f -> Query.Pair ("EngineVersion", (String.to_query f)));
           Util.option_map v.master_username
             (fun f -> Query.Pair ("MasterUsername", (String.to_query f)));
           Util.option_map v.cluster_create_time
             (fun f ->
                Query.Pair ("ClusterCreateTime", (DateTime.to_query f)));
           Util.option_map v.vpc_id
             (fun f -> Query.Pair ("VpcId", (String.to_query f)));
           Util.option_map v.port
             (fun f -> Query.Pair ("Port", (Integer.to_query f)));
           Util.option_map v.status
             (fun f -> Query.Pair ("Status", (String.to_query f)));
           Util.option_map v.allocated_storage
             (fun f -> Query.Pair ("AllocatedStorage", (Integer.to_query f)));
           Util.option_map v.engine
             (fun f -> Query.Pair ("Engine", (String.to_query f)));
           Util.option_map v.snapshot_create_time
             (fun f ->
                Query.Pair ("SnapshotCreateTime", (DateTime.to_query f)));
           Util.option_map v.d_b_cluster_identifier
             (fun f ->
                Query.Pair ("DBClusterIdentifier", (String.to_query f)));
           Util.option_map v.d_b_cluster_snapshot_identifier
             (fun f ->
                Query.Pair
                  ("DBClusterSnapshotIdentifier", (String.to_query f)));
           Some
             (Query.Pair
                ("AvailabilityZones.member",
                  (AvailabilityZones.to_query v.availability_zones)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.percent_progress
              (fun f -> ("percent_progress", (Integer.to_json f)));
           Util.option_map v.snapshot_type
             (fun f -> ("snapshot_type", (String.to_json f)));
           Util.option_map v.license_model
             (fun f -> ("license_model", (String.to_json f)));
           Util.option_map v.engine_version
             (fun f -> ("engine_version", (String.to_json f)));
           Util.option_map v.master_username
             (fun f -> ("master_username", (String.to_json f)));
           Util.option_map v.cluster_create_time
             (fun f -> ("cluster_create_time", (DateTime.to_json f)));
           Util.option_map v.vpc_id (fun f -> ("vpc_id", (String.to_json f)));
           Util.option_map v.port (fun f -> ("port", (Integer.to_json f)));
           Util.option_map v.status (fun f -> ("status", (String.to_json f)));
           Util.option_map v.allocated_storage
             (fun f -> ("allocated_storage", (Integer.to_json f)));
           Util.option_map v.engine (fun f -> ("engine", (String.to_json f)));
           Util.option_map v.snapshot_create_time
             (fun f -> ("snapshot_create_time", (DateTime.to_json f)));
           Util.option_map v.d_b_cluster_identifier
             (fun f -> ("d_b_cluster_identifier", (String.to_json f)));
           Util.option_map v.d_b_cluster_snapshot_identifier
             (fun f ->
                ("d_b_cluster_snapshot_identifier", (String.to_json f)));
           Some
             ("availability_zones",
               (AvailabilityZones.to_json v.availability_zones))])
    let of_json j =
      {
        availability_zones =
          (AvailabilityZones.of_json
             (Util.of_option_exn (Json.lookup j "availability_zones")));
        d_b_cluster_snapshot_identifier =
          (Util.option_map (Json.lookup j "d_b_cluster_snapshot_identifier")
             String.of_json);
        d_b_cluster_identifier =
          (Util.option_map (Json.lookup j "d_b_cluster_identifier")
             String.of_json);
        snapshot_create_time =
          (Util.option_map (Json.lookup j "snapshot_create_time")
             DateTime.of_json);
        engine = (Util.option_map (Json.lookup j "engine") String.of_json);
        allocated_storage =
          (Util.option_map (Json.lookup j "allocated_storage")
             Integer.of_json);
        status = (Util.option_map (Json.lookup j "status") String.of_json);
        port = (Util.option_map (Json.lookup j "port") Integer.of_json);
        vpc_id = (Util.option_map (Json.lookup j "vpc_id") String.of_json);
        cluster_create_time =
          (Util.option_map (Json.lookup j "cluster_create_time")
             DateTime.of_json);
        master_username =
          (Util.option_map (Json.lookup j "master_username") String.of_json);
        engine_version =
          (Util.option_map (Json.lookup j "engine_version") String.of_json);
        license_model =
          (Util.option_map (Json.lookup j "license_model") String.of_json);
        snapshot_type =
          (Util.option_map (Json.lookup j "snapshot_type") String.of_json);
        percent_progress =
          (Util.option_map (Json.lookup j "percent_progress") Integer.of_json)
      }
  end
module ReservedDBInstancesOffering =
  struct
    type t =
      {
      reserved_d_b_instances_offering_id: String.t option ;
      d_b_instance_class: String.t option ;
      duration: Integer.t option ;
      fixed_price: Double.t option ;
      usage_price: Double.t option ;
      currency_code: String.t option ;
      product_description: String.t option ;
      offering_type: String.t option ;
      multi_a_z: Boolean.t option ;
      recurring_charges: RecurringChargeList.t }
    let make ?reserved_d_b_instances_offering_id  ?d_b_instance_class 
      ?duration  ?fixed_price  ?usage_price  ?currency_code 
      ?product_description  ?offering_type  ?multi_a_z  ?(recurring_charges=
      [])  () =
      {
        reserved_d_b_instances_offering_id;
        d_b_instance_class;
        duration;
        fixed_price;
        usage_price;
        currency_code;
        product_description;
        offering_type;
        multi_a_z;
        recurring_charges
      }
    let parse xml =
      Some
        {
          reserved_d_b_instances_offering_id =
            (Util.option_bind
               (Xml.member "ReservedDBInstancesOfferingId" xml) String.parse);
          d_b_instance_class =
            (Util.option_bind (Xml.member "DBInstanceClass" xml) String.parse);
          duration =
            (Util.option_bind (Xml.member "Duration" xml) Integer.parse);
          fixed_price =
            (Util.option_bind (Xml.member "FixedPrice" xml) Double.parse);
          usage_price =
            (Util.option_bind (Xml.member "UsagePrice" xml) Double.parse);
          currency_code =
            (Util.option_bind (Xml.member "CurrencyCode" xml) String.parse);
          product_description =
            (Util.option_bind (Xml.member "ProductDescription" xml)
               String.parse);
          offering_type =
            (Util.option_bind (Xml.member "OfferingType" xml) String.parse);
          multi_a_z =
            (Util.option_bind (Xml.member "MultiAZ" xml) Boolean.parse);
          recurring_charges =
            (Util.of_option []
               (Util.option_bind (Xml.member "RecurringCharges" xml)
                  RecurringChargeList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("RecurringCharges.member",
                   (RecurringChargeList.to_query v.recurring_charges)));
           Util.option_map v.multi_a_z
             (fun f -> Query.Pair ("MultiAZ", (Boolean.to_query f)));
           Util.option_map v.offering_type
             (fun f -> Query.Pair ("OfferingType", (String.to_query f)));
           Util.option_map v.product_description
             (fun f -> Query.Pair ("ProductDescription", (String.to_query f)));
           Util.option_map v.currency_code
             (fun f -> Query.Pair ("CurrencyCode", (String.to_query f)));
           Util.option_map v.usage_price
             (fun f -> Query.Pair ("UsagePrice", (Double.to_query f)));
           Util.option_map v.fixed_price
             (fun f -> Query.Pair ("FixedPrice", (Double.to_query f)));
           Util.option_map v.duration
             (fun f -> Query.Pair ("Duration", (Integer.to_query f)));
           Util.option_map v.d_b_instance_class
             (fun f -> Query.Pair ("DBInstanceClass", (String.to_query f)));
           Util.option_map v.reserved_d_b_instances_offering_id
             (fun f ->
                Query.Pair
                  ("ReservedDBInstancesOfferingId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("recurring_charges",
                (RecurringChargeList.to_json v.recurring_charges));
           Util.option_map v.multi_a_z
             (fun f -> ("multi_a_z", (Boolean.to_json f)));
           Util.option_map v.offering_type
             (fun f -> ("offering_type", (String.to_json f)));
           Util.option_map v.product_description
             (fun f -> ("product_description", (String.to_json f)));
           Util.option_map v.currency_code
             (fun f -> ("currency_code", (String.to_json f)));
           Util.option_map v.usage_price
             (fun f -> ("usage_price", (Double.to_json f)));
           Util.option_map v.fixed_price
             (fun f -> ("fixed_price", (Double.to_json f)));
           Util.option_map v.duration
             (fun f -> ("duration", (Integer.to_json f)));
           Util.option_map v.d_b_instance_class
             (fun f -> ("d_b_instance_class", (String.to_json f)));
           Util.option_map v.reserved_d_b_instances_offering_id
             (fun f ->
                ("reserved_d_b_instances_offering_id", (String.to_json f)))])
    let of_json j =
      {
        reserved_d_b_instances_offering_id =
          (Util.option_map
             (Json.lookup j "reserved_d_b_instances_offering_id")
             String.of_json);
        d_b_instance_class =
          (Util.option_map (Json.lookup j "d_b_instance_class")
             String.of_json);
        duration =
          (Util.option_map (Json.lookup j "duration") Integer.of_json);
        fixed_price =
          (Util.option_map (Json.lookup j "fixed_price") Double.of_json);
        usage_price =
          (Util.option_map (Json.lookup j "usage_price") Double.of_json);
        currency_code =
          (Util.option_map (Json.lookup j "currency_code") String.of_json);
        product_description =
          (Util.option_map (Json.lookup j "product_description")
             String.of_json);
        offering_type =
          (Util.option_map (Json.lookup j "offering_type") String.of_json);
        multi_a_z =
          (Util.option_map (Json.lookup j "multi_a_z") Boolean.of_json);
        recurring_charges =
          (RecurringChargeList.of_json
             (Util.of_option_exn (Json.lookup j "recurring_charges")))
      }
  end
module DBCluster =
  struct
    type t =
      {
      allocated_storage: Integer.t option ;
      availability_zones: AvailabilityZones.t ;
      backup_retention_period: Integer.t option ;
      character_set_name: String.t option ;
      database_name: String.t option ;
      d_b_cluster_identifier: String.t option ;
      d_b_cluster_parameter_group: String.t option ;
      d_b_subnet_group: String.t option ;
      status: String.t option ;
      percent_progress: String.t option ;
      earliest_restorable_time: DateTime.t option ;
      endpoint: String.t option ;
      engine: String.t option ;
      engine_version: String.t option ;
      latest_restorable_time: DateTime.t option ;
      port: Integer.t option ;
      master_username: String.t option ;
      d_b_cluster_option_group_memberships: DBClusterOptionGroupMemberships.t ;
      preferred_backup_window: String.t option ;
      preferred_maintenance_window: String.t option ;
      d_b_cluster_members: DBClusterMemberList.t ;
      vpc_security_groups: VpcSecurityGroupMembershipList.t }
    let make ?allocated_storage  ?(availability_zones= []) 
      ?backup_retention_period  ?character_set_name  ?database_name 
      ?d_b_cluster_identifier  ?d_b_cluster_parameter_group 
      ?d_b_subnet_group  ?status  ?percent_progress 
      ?earliest_restorable_time  ?endpoint  ?engine  ?engine_version 
      ?latest_restorable_time  ?port  ?master_username 
      ?(d_b_cluster_option_group_memberships= [])  ?preferred_backup_window 
      ?preferred_maintenance_window  ?(d_b_cluster_members= []) 
      ?(vpc_security_groups= [])  () =
      {
        allocated_storage;
        availability_zones;
        backup_retention_period;
        character_set_name;
        database_name;
        d_b_cluster_identifier;
        d_b_cluster_parameter_group;
        d_b_subnet_group;
        status;
        percent_progress;
        earliest_restorable_time;
        endpoint;
        engine;
        engine_version;
        latest_restorable_time;
        port;
        master_username;
        d_b_cluster_option_group_memberships;
        preferred_backup_window;
        preferred_maintenance_window;
        d_b_cluster_members;
        vpc_security_groups
      }
    let parse xml =
      Some
        {
          allocated_storage =
            (Util.option_bind (Xml.member "AllocatedStorage" xml)
               Integer.parse);
          availability_zones =
            (Util.of_option []
               (Util.option_bind (Xml.member "AvailabilityZones" xml)
                  AvailabilityZones.parse));
          backup_retention_period =
            (Util.option_bind (Xml.member "BackupRetentionPeriod" xml)
               Integer.parse);
          character_set_name =
            (Util.option_bind (Xml.member "CharacterSetName" xml)
               String.parse);
          database_name =
            (Util.option_bind (Xml.member "DatabaseName" xml) String.parse);
          d_b_cluster_identifier =
            (Util.option_bind (Xml.member "DBClusterIdentifier" xml)
               String.parse);
          d_b_cluster_parameter_group =
            (Util.option_bind (Xml.member "DBClusterParameterGroup" xml)
               String.parse);
          d_b_subnet_group =
            (Util.option_bind (Xml.member "DBSubnetGroup" xml) String.parse);
          status = (Util.option_bind (Xml.member "Status" xml) String.parse);
          percent_progress =
            (Util.option_bind (Xml.member "PercentProgress" xml) String.parse);
          earliest_restorable_time =
            (Util.option_bind (Xml.member "EarliestRestorableTime" xml)
               DateTime.parse);
          endpoint =
            (Util.option_bind (Xml.member "Endpoint" xml) String.parse);
          engine = (Util.option_bind (Xml.member "Engine" xml) String.parse);
          engine_version =
            (Util.option_bind (Xml.member "EngineVersion" xml) String.parse);
          latest_restorable_time =
            (Util.option_bind (Xml.member "LatestRestorableTime" xml)
               DateTime.parse);
          port = (Util.option_bind (Xml.member "Port" xml) Integer.parse);
          master_username =
            (Util.option_bind (Xml.member "MasterUsername" xml) String.parse);
          d_b_cluster_option_group_memberships =
            (Util.of_option []
               (Util.option_bind
                  (Xml.member "DBClusterOptionGroupMemberships" xml)
                  DBClusterOptionGroupMemberships.parse));
          preferred_backup_window =
            (Util.option_bind (Xml.member "PreferredBackupWindow" xml)
               String.parse);
          preferred_maintenance_window =
            (Util.option_bind (Xml.member "PreferredMaintenanceWindow" xml)
               String.parse);
          d_b_cluster_members =
            (Util.of_option []
               (Util.option_bind (Xml.member "DBClusterMembers" xml)
                  DBClusterMemberList.parse));
          vpc_security_groups =
            (Util.of_option []
               (Util.option_bind (Xml.member "VpcSecurityGroups" xml)
                  VpcSecurityGroupMembershipList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("VpcSecurityGroups.member",
                   (VpcSecurityGroupMembershipList.to_query
                      v.vpc_security_groups)));
           Some
             (Query.Pair
                ("DBClusterMembers.member",
                  (DBClusterMemberList.to_query v.d_b_cluster_members)));
           Util.option_map v.preferred_maintenance_window
             (fun f ->
                Query.Pair
                  ("PreferredMaintenanceWindow", (String.to_query f)));
           Util.option_map v.preferred_backup_window
             (fun f ->
                Query.Pair ("PreferredBackupWindow", (String.to_query f)));
           Some
             (Query.Pair
                ("DBClusterOptionGroupMemberships.member",
                  (DBClusterOptionGroupMemberships.to_query
                     v.d_b_cluster_option_group_memberships)));
           Util.option_map v.master_username
             (fun f -> Query.Pair ("MasterUsername", (String.to_query f)));
           Util.option_map v.port
             (fun f -> Query.Pair ("Port", (Integer.to_query f)));
           Util.option_map v.latest_restorable_time
             (fun f ->
                Query.Pair ("LatestRestorableTime", (DateTime.to_query f)));
           Util.option_map v.engine_version
             (fun f -> Query.Pair ("EngineVersion", (String.to_query f)));
           Util.option_map v.engine
             (fun f -> Query.Pair ("Engine", (String.to_query f)));
           Util.option_map v.endpoint
             (fun f -> Query.Pair ("Endpoint", (String.to_query f)));
           Util.option_map v.earliest_restorable_time
             (fun f ->
                Query.Pair ("EarliestRestorableTime", (DateTime.to_query f)));
           Util.option_map v.percent_progress
             (fun f -> Query.Pair ("PercentProgress", (String.to_query f)));
           Util.option_map v.status
             (fun f -> Query.Pair ("Status", (String.to_query f)));
           Util.option_map v.d_b_subnet_group
             (fun f -> Query.Pair ("DBSubnetGroup", (String.to_query f)));
           Util.option_map v.d_b_cluster_parameter_group
             (fun f ->
                Query.Pair ("DBClusterParameterGroup", (String.to_query f)));
           Util.option_map v.d_b_cluster_identifier
             (fun f ->
                Query.Pair ("DBClusterIdentifier", (String.to_query f)));
           Util.option_map v.database_name
             (fun f -> Query.Pair ("DatabaseName", (String.to_query f)));
           Util.option_map v.character_set_name
             (fun f -> Query.Pair ("CharacterSetName", (String.to_query f)));
           Util.option_map v.backup_retention_period
             (fun f ->
                Query.Pair ("BackupRetentionPeriod", (Integer.to_query f)));
           Some
             (Query.Pair
                ("AvailabilityZones.member",
                  (AvailabilityZones.to_query v.availability_zones)));
           Util.option_map v.allocated_storage
             (fun f -> Query.Pair ("AllocatedStorage", (Integer.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("vpc_security_groups",
                (VpcSecurityGroupMembershipList.to_json v.vpc_security_groups));
           Some
             ("d_b_cluster_members",
               (DBClusterMemberList.to_json v.d_b_cluster_members));
           Util.option_map v.preferred_maintenance_window
             (fun f -> ("preferred_maintenance_window", (String.to_json f)));
           Util.option_map v.preferred_backup_window
             (fun f -> ("preferred_backup_window", (String.to_json f)));
           Some
             ("d_b_cluster_option_group_memberships",
               (DBClusterOptionGroupMemberships.to_json
                  v.d_b_cluster_option_group_memberships));
           Util.option_map v.master_username
             (fun f -> ("master_username", (String.to_json f)));
           Util.option_map v.port (fun f -> ("port", (Integer.to_json f)));
           Util.option_map v.latest_restorable_time
             (fun f -> ("latest_restorable_time", (DateTime.to_json f)));
           Util.option_map v.engine_version
             (fun f -> ("engine_version", (String.to_json f)));
           Util.option_map v.engine (fun f -> ("engine", (String.to_json f)));
           Util.option_map v.endpoint
             (fun f -> ("endpoint", (String.to_json f)));
           Util.option_map v.earliest_restorable_time
             (fun f -> ("earliest_restorable_time", (DateTime.to_json f)));
           Util.option_map v.percent_progress
             (fun f -> ("percent_progress", (String.to_json f)));
           Util.option_map v.status (fun f -> ("status", (String.to_json f)));
           Util.option_map v.d_b_subnet_group
             (fun f -> ("d_b_subnet_group", (String.to_json f)));
           Util.option_map v.d_b_cluster_parameter_group
             (fun f -> ("d_b_cluster_parameter_group", (String.to_json f)));
           Util.option_map v.d_b_cluster_identifier
             (fun f -> ("d_b_cluster_identifier", (String.to_json f)));
           Util.option_map v.database_name
             (fun f -> ("database_name", (String.to_json f)));
           Util.option_map v.character_set_name
             (fun f -> ("character_set_name", (String.to_json f)));
           Util.option_map v.backup_retention_period
             (fun f -> ("backup_retention_period", (Integer.to_json f)));
           Some
             ("availability_zones",
               (AvailabilityZones.to_json v.availability_zones));
           Util.option_map v.allocated_storage
             (fun f -> ("allocated_storage", (Integer.to_json f)))])
    let of_json j =
      {
        allocated_storage =
          (Util.option_map (Json.lookup j "allocated_storage")
             Integer.of_json);
        availability_zones =
          (AvailabilityZones.of_json
             (Util.of_option_exn (Json.lookup j "availability_zones")));
        backup_retention_period =
          (Util.option_map (Json.lookup j "backup_retention_period")
             Integer.of_json);
        character_set_name =
          (Util.option_map (Json.lookup j "character_set_name")
             String.of_json);
        database_name =
          (Util.option_map (Json.lookup j "database_name") String.of_json);
        d_b_cluster_identifier =
          (Util.option_map (Json.lookup j "d_b_cluster_identifier")
             String.of_json);
        d_b_cluster_parameter_group =
          (Util.option_map (Json.lookup j "d_b_cluster_parameter_group")
             String.of_json);
        d_b_subnet_group =
          (Util.option_map (Json.lookup j "d_b_subnet_group") String.of_json);
        status = (Util.option_map (Json.lookup j "status") String.of_json);
        percent_progress =
          (Util.option_map (Json.lookup j "percent_progress") String.of_json);
        earliest_restorable_time =
          (Util.option_map (Json.lookup j "earliest_restorable_time")
             DateTime.of_json);
        endpoint =
          (Util.option_map (Json.lookup j "endpoint") String.of_json);
        engine = (Util.option_map (Json.lookup j "engine") String.of_json);
        engine_version =
          (Util.option_map (Json.lookup j "engine_version") String.of_json);
        latest_restorable_time =
          (Util.option_map (Json.lookup j "latest_restorable_time")
             DateTime.of_json);
        port = (Util.option_map (Json.lookup j "port") Integer.of_json);
        master_username =
          (Util.option_map (Json.lookup j "master_username") String.of_json);
        d_b_cluster_option_group_memberships =
          (DBClusterOptionGroupMemberships.of_json
             (Util.of_option_exn
                (Json.lookup j "d_b_cluster_option_group_memberships")));
        preferred_backup_window =
          (Util.option_map (Json.lookup j "preferred_backup_window")
             String.of_json);
        preferred_maintenance_window =
          (Util.option_map (Json.lookup j "preferred_maintenance_window")
             String.of_json);
        d_b_cluster_members =
          (DBClusterMemberList.of_json
             (Util.of_option_exn (Json.lookup j "d_b_cluster_members")));
        vpc_security_groups =
          (VpcSecurityGroupMembershipList.of_json
             (Util.of_option_exn (Json.lookup j "vpc_security_groups")))
      }
  end
module DBSecurityGroup =
  struct
    type t =
      {
      owner_id: String.t option ;
      d_b_security_group_name: String.t option ;
      d_b_security_group_description: String.t option ;
      vpc_id: String.t option ;
      e_c2_security_groups: EC2SecurityGroupList.t ;
      i_p_ranges: IPRangeList.t }
    let make ?owner_id  ?d_b_security_group_name 
      ?d_b_security_group_description  ?vpc_id  ?(e_c2_security_groups= []) 
      ?(i_p_ranges= [])  () =
      {
        owner_id;
        d_b_security_group_name;
        d_b_security_group_description;
        vpc_id;
        e_c2_security_groups;
        i_p_ranges
      }
    let parse xml =
      Some
        {
          owner_id =
            (Util.option_bind (Xml.member "OwnerId" xml) String.parse);
          d_b_security_group_name =
            (Util.option_bind (Xml.member "DBSecurityGroupName" xml)
               String.parse);
          d_b_security_group_description =
            (Util.option_bind (Xml.member "DBSecurityGroupDescription" xml)
               String.parse);
          vpc_id = (Util.option_bind (Xml.member "VpcId" xml) String.parse);
          e_c2_security_groups =
            (Util.of_option []
               (Util.option_bind (Xml.member "EC2SecurityGroups" xml)
                  EC2SecurityGroupList.parse));
          i_p_ranges =
            (Util.of_option []
               (Util.option_bind (Xml.member "IPRanges" xml)
                  IPRangeList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("IPRanges.member", (IPRangeList.to_query v.i_p_ranges)));
           Some
             (Query.Pair
                ("EC2SecurityGroups.member",
                  (EC2SecurityGroupList.to_query v.e_c2_security_groups)));
           Util.option_map v.vpc_id
             (fun f -> Query.Pair ("VpcId", (String.to_query f)));
           Util.option_map v.d_b_security_group_description
             (fun f ->
                Query.Pair
                  ("DBSecurityGroupDescription", (String.to_query f)));
           Util.option_map v.d_b_security_group_name
             (fun f ->
                Query.Pair ("DBSecurityGroupName", (String.to_query f)));
           Util.option_map v.owner_id
             (fun f -> Query.Pair ("OwnerId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("i_p_ranges", (IPRangeList.to_json v.i_p_ranges));
           Some
             ("e_c2_security_groups",
               (EC2SecurityGroupList.to_json v.e_c2_security_groups));
           Util.option_map v.vpc_id (fun f -> ("vpc_id", (String.to_json f)));
           Util.option_map v.d_b_security_group_description
             (fun f -> ("d_b_security_group_description", (String.to_json f)));
           Util.option_map v.d_b_security_group_name
             (fun f -> ("d_b_security_group_name", (String.to_json f)));
           Util.option_map v.owner_id
             (fun f -> ("owner_id", (String.to_json f)))])
    let of_json j =
      {
        owner_id =
          (Util.option_map (Json.lookup j "owner_id") String.of_json);
        d_b_security_group_name =
          (Util.option_map (Json.lookup j "d_b_security_group_name")
             String.of_json);
        d_b_security_group_description =
          (Util.option_map (Json.lookup j "d_b_security_group_description")
             String.of_json);
        vpc_id = (Util.option_map (Json.lookup j "vpc_id") String.of_json);
        e_c2_security_groups =
          (EC2SecurityGroupList.of_json
             (Util.of_option_exn (Json.lookup j "e_c2_security_groups")));
        i_p_ranges =
          (IPRangeList.of_json
             (Util.of_option_exn (Json.lookup j "i_p_ranges")))
      }
  end
module TagList =
  struct
    type t = Tag.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Tag.parse (Xml.members "Tag" xml))
    let to_query v = Query.to_query_list Tag.to_query v
    let to_json v = `List (List.map Tag.to_json v)
    let of_json j = Json.to_list Tag.of_json j
  end
module OptionGroupOptionsList =
  struct
    type t = OptionGroupOption.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map OptionGroupOption.parse
           (Xml.members "OptionGroupOption" xml))
    let to_query v = Query.to_query_list OptionGroupOption.to_query v
    let to_json v = `List (List.map OptionGroupOption.to_json v)
    let of_json j = Json.to_list OptionGroupOption.of_json j
  end
module FilterList =
  struct
    type t = Filter.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Filter.parse (Xml.members "Filter" xml))
    let to_query v = Query.to_query_list Filter.to_query v
    let to_json v = `List (List.map Filter.to_json v)
    let of_json j = Json.to_list Filter.of_json j
  end
module SubnetIdentifierList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map String.parse (Xml.members "SubnetIdentifier" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module EngineDefaults =
  struct
    type t =
      {
      d_b_parameter_group_family: String.t option ;
      marker: String.t option ;
      parameters: ParametersList.t }
    let make ?d_b_parameter_group_family  ?marker  ?(parameters= [])  () =
      { d_b_parameter_group_family; marker; parameters }
    let parse xml =
      Some
        {
          d_b_parameter_group_family =
            (Util.option_bind (Xml.member "DBParameterGroupFamily" xml)
               String.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          parameters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Parameters" xml)
                  ParametersList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Parameters.member",
                   (ParametersList.to_query v.parameters)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.d_b_parameter_group_family
             (fun f ->
                Query.Pair ("DBParameterGroupFamily", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("parameters", (ParametersList.to_json v.parameters));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.d_b_parameter_group_family
             (fun f -> ("d_b_parameter_group_family", (String.to_json f)))])
    let of_json j =
      {
        d_b_parameter_group_family =
          (Util.option_map (Json.lookup j "d_b_parameter_group_family")
             String.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        parameters =
          (ParametersList.of_json
             (Util.of_option_exn (Json.lookup j "parameters")))
      }
  end
module EventSubscriptionsList =
  struct
    type t = EventSubscription.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map EventSubscription.parse
           (Xml.members "EventSubscription" xml))
    let to_query v = Query.to_query_list EventSubscription.to_query v
    let to_json v = `List (List.map EventSubscription.to_json v)
    let of_json j = Json.to_list EventSubscription.of_json j
  end
module OptionConfigurationList =
  struct
    type t = OptionConfiguration.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map OptionConfiguration.parse
           (Xml.members "OptionConfiguration" xml))
    let to_query v = Query.to_query_list OptionConfiguration.to_query v
    let to_json v = `List (List.map OptionConfiguration.to_json v)
    let of_json j = Json.to_list OptionConfiguration.of_json j
  end
module OptionNamesList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module ReservedDBInstanceList =
  struct
    type t = ReservedDBInstance.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map ReservedDBInstance.parse
           (Xml.members "ReservedDBInstance" xml))
    let to_query v = Query.to_query_list ReservedDBInstance.to_query v
    let to_json v = `List (List.map ReservedDBInstance.to_json v)
    let of_json j = Json.to_list ReservedDBInstance.of_json j
  end
module PendingMaintenanceActions =
  struct
    type t = ResourcePendingMaintenanceActions.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map ResourcePendingMaintenanceActions.parse
           (Xml.members "ResourcePendingMaintenanceActions" xml))
    let to_query v =
      Query.to_query_list ResourcePendingMaintenanceActions.to_query v
    let to_json v =
      `List (List.map ResourcePendingMaintenanceActions.to_json v)
    let of_json j = Json.to_list ResourcePendingMaintenanceActions.of_json j
  end
module DBSubnetGroups =
  struct
    type t = DBSubnetGroup.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map DBSubnetGroup.parse (Xml.members "DBSubnetGroup" xml))
    let to_query v = Query.to_query_list DBSubnetGroup.to_query v
    let to_json v = `List (List.map DBSubnetGroup.to_json v)
    let of_json j = Json.to_list DBSubnetGroup.of_json j
  end
module DBEngineVersionList =
  struct
    type t = DBEngineVersion.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map DBEngineVersion.parse (Xml.members "DBEngineVersion" xml))
    let to_query v = Query.to_query_list DBEngineVersion.to_query v
    let to_json v = `List (List.map DBEngineVersion.to_json v)
    let of_json j = Json.to_list DBEngineVersion.of_json j
  end
module DBSnapshotList =
  struct
    type t = DBSnapshot.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map DBSnapshot.parse (Xml.members "DBSnapshot" xml))
    let to_query v = Query.to_query_list DBSnapshot.to_query v
    let to_json v = `List (List.map DBSnapshot.to_json v)
    let of_json j = Json.to_list DBSnapshot.of_json j
  end
module EventCategoriesMapList =
  struct
    type t = EventCategoriesMap.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map EventCategoriesMap.parse
           (Xml.members "EventCategoriesMap" xml))
    let to_query v = Query.to_query_list EventCategoriesMap.to_query v
    let to_json v = `List (List.map EventCategoriesMap.to_json v)
    let of_json j = Json.to_list EventCategoriesMap.of_json j
  end
module DBInstanceList =
  struct
    type t = DBInstance.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map DBInstance.parse (Xml.members "DBInstance" xml))
    let to_query v = Query.to_query_list DBInstance.to_query v
    let to_json v = `List (List.map DBInstance.to_json v)
    let of_json j = Json.to_list DBInstance.of_json j
  end
module AccountQuotaList =
  struct
    type t = AccountQuota.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map AccountQuota.parse (Xml.members "AccountQuota" xml))
    let to_query v = Query.to_query_list AccountQuota.to_query v
    let to_json v = `List (List.map AccountQuota.to_json v)
    let of_json j = Json.to_list AccountQuota.of_json j
  end
module DBClusterParameterGroupList =
  struct
    type t = DBClusterParameterGroup.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map DBClusterParameterGroup.parse
           (Xml.members "DBClusterParameterGroup" xml))
    let to_query v = Query.to_query_list DBClusterParameterGroup.to_query v
    let to_json v = `List (List.map DBClusterParameterGroup.to_json v)
    let of_json j = Json.to_list DBClusterParameterGroup.of_json j
  end
module DescribeDBLogFilesList =
  struct
    type t = DescribeDBLogFilesDetails.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map DescribeDBLogFilesDetails.parse
           (Xml.members "DescribeDBLogFilesDetails" xml))
    let to_query v = Query.to_query_list DescribeDBLogFilesDetails.to_query v
    let to_json v = `List (List.map DescribeDBLogFilesDetails.to_json v)
    let of_json j = Json.to_list DescribeDBLogFilesDetails.of_json j
  end
module DBParameterGroupList =
  struct
    type t = DBParameterGroup.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map DBParameterGroup.parse (Xml.members "DBParameterGroup" xml))
    let to_query v = Query.to_query_list DBParameterGroup.to_query v
    let to_json v = `List (List.map DBParameterGroup.to_json v)
    let of_json j = Json.to_list DBParameterGroup.of_json j
  end
module EventList =
  struct
    type t = Event.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map Event.parse (Xml.members "Event" xml))
    let to_query v = Query.to_query_list Event.to_query v
    let to_json v = `List (List.map Event.to_json v)
    let of_json j = Json.to_list Event.of_json j
  end
module CertificateList =
  struct
    type t = Certificate.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map Certificate.parse (Xml.members "Certificate" xml))
    let to_query v = Query.to_query_list Certificate.to_query v
    let to_json v = `List (List.map Certificate.to_json v)
    let of_json j = Json.to_list Certificate.of_json j
  end
module OptionGroupsList =
  struct
    type t = OptionGroup.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map OptionGroup.parse (Xml.members "OptionGroup" xml))
    let to_query v = Query.to_query_list OptionGroup.to_query v
    let to_json v = `List (List.map OptionGroup.to_json v)
    let of_json j = Json.to_list OptionGroup.of_json j
  end
module OrderableDBInstanceOptionsList =
  struct
    type t = OrderableDBInstanceOption.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map OrderableDBInstanceOption.parse
           (Xml.members "OrderableDBInstanceOption" xml))
    let to_query v = Query.to_query_list OrderableDBInstanceOption.to_query v
    let to_json v = `List (List.map OrderableDBInstanceOption.to_json v)
    let of_json j = Json.to_list OrderableDBInstanceOption.of_json j
  end
module KeyList =
  struct
    type t = String.t list
    let make elems () = elems
    let parse xml =
      Util.option_all (List.map String.parse (Xml.members "member" xml))
    let to_query v = Query.to_query_list String.to_query v
    let to_json v = `List (List.map String.to_json v)
    let of_json j = Json.to_list String.of_json j
  end
module DBClusterSnapshotList =
  struct
    type t = DBClusterSnapshot.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map DBClusterSnapshot.parse
           (Xml.members "DBClusterSnapshot" xml))
    let to_query v = Query.to_query_list DBClusterSnapshot.to_query v
    let to_json v = `List (List.map DBClusterSnapshot.to_json v)
    let of_json j = Json.to_list DBClusterSnapshot.of_json j
  end
module ReservedDBInstancesOfferingList =
  struct
    type t = ReservedDBInstancesOffering.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map ReservedDBInstancesOffering.parse
           (Xml.members "ReservedDBInstancesOffering" xml))
    let to_query v =
      Query.to_query_list ReservedDBInstancesOffering.to_query v
    let to_json v = `List (List.map ReservedDBInstancesOffering.to_json v)
    let of_json j = Json.to_list ReservedDBInstancesOffering.of_json j
  end
module DBClusterList =
  struct
    type t = DBCluster.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map DBCluster.parse (Xml.members "DBCluster" xml))
    let to_query v = Query.to_query_list DBCluster.to_query v
    let to_json v = `List (List.map DBCluster.to_json v)
    let of_json j = Json.to_list DBCluster.of_json j
  end
module DBSecurityGroups =
  struct
    type t = DBSecurityGroup.t list
    let make elems () = elems
    let parse xml =
      Util.option_all
        (List.map DBSecurityGroup.parse (Xml.members "DBSecurityGroup" xml))
    let to_query v = Query.to_query_list DBSecurityGroup.to_query v
    let to_json v = `List (List.map DBSecurityGroup.to_json v)
    let of_json j = Json.to_list DBSecurityGroup.of_json j
  end
module ModifyDBClusterResult =
  struct
    type t = {
      d_b_cluster: DBCluster.t option }
    let make ?d_b_cluster  () = { d_b_cluster }
    let parse xml =
      Some
        {
          d_b_cluster =
            (Util.option_bind (Xml.member "DBCluster" xml) DBCluster.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.d_b_cluster
              (fun f -> Query.Pair ("DBCluster", (DBCluster.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.d_b_cluster
              (fun f -> ("d_b_cluster", (DBCluster.to_json f)))])
    let of_json j =
      {
        d_b_cluster =
          (Util.option_map (Json.lookup j "d_b_cluster") DBCluster.of_json)
      }
  end
module CopyOptionGroupMessage =
  struct
    type t =
      {
      source_option_group_identifier: String.t ;
      target_option_group_identifier: String.t ;
      target_option_group_description: String.t ;
      tags: TagList.t }
    let make ~source_option_group_identifier  ~target_option_group_identifier
       ~target_option_group_description  ?(tags= [])  () =
      {
        source_option_group_identifier;
        target_option_group_identifier;
        target_option_group_description;
        tags
      }
    let parse xml =
      Some
        {
          source_option_group_identifier =
            (Xml.required "SourceOptionGroupIdentifier"
               (Util.option_bind
                  (Xml.member "SourceOptionGroupIdentifier" xml) String.parse));
          target_option_group_identifier =
            (Xml.required "TargetOptionGroupIdentifier"
               (Util.option_bind
                  (Xml.member "TargetOptionGroupIdentifier" xml) String.parse));
          target_option_group_description =
            (Xml.required "TargetOptionGroupDescription"
               (Util.option_bind
                  (Xml.member "TargetOptionGroupDescription" xml)
                  String.parse));
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) TagList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Tags.member", (TagList.to_query v.tags)));
           Some
             (Query.Pair
                ("TargetOptionGroupDescription",
                  (String.to_query v.target_option_group_description)));
           Some
             (Query.Pair
                ("TargetOptionGroupIdentifier",
                  (String.to_query v.target_option_group_identifier)));
           Some
             (Query.Pair
                ("SourceOptionGroupIdentifier",
                  (String.to_query v.source_option_group_identifier)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tags", (TagList.to_json v.tags));
           Some
             ("target_option_group_description",
               (String.to_json v.target_option_group_description));
           Some
             ("target_option_group_identifier",
               (String.to_json v.target_option_group_identifier));
           Some
             ("source_option_group_identifier",
               (String.to_json v.source_option_group_identifier))])
    let of_json j =
      {
        source_option_group_identifier =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "source_option_group_identifier")));
        target_option_group_identifier =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "target_option_group_identifier")));
        target_option_group_description =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "target_option_group_description")));
        tags = (TagList.of_json (Util.of_option_exn (Json.lookup j "tags")))
      }
  end
module DeleteDBInstanceMessage =
  struct
    type t =
      {
      d_b_instance_identifier: String.t ;
      skip_final_snapshot: Boolean.t option ;
      final_d_b_snapshot_identifier: String.t option }
    let make ~d_b_instance_identifier  ?skip_final_snapshot 
      ?final_d_b_snapshot_identifier  () =
      {
        d_b_instance_identifier;
        skip_final_snapshot;
        final_d_b_snapshot_identifier
      }
    let parse xml =
      Some
        {
          d_b_instance_identifier =
            (Xml.required "DBInstanceIdentifier"
               (Util.option_bind (Xml.member "DBInstanceIdentifier" xml)
                  String.parse));
          skip_final_snapshot =
            (Util.option_bind (Xml.member "SkipFinalSnapshot" xml)
               Boolean.parse);
          final_d_b_snapshot_identifier =
            (Util.option_bind (Xml.member "FinalDBSnapshotIdentifier" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.final_d_b_snapshot_identifier
              (fun f ->
                 Query.Pair
                   ("FinalDBSnapshotIdentifier", (String.to_query f)));
           Util.option_map v.skip_final_snapshot
             (fun f -> Query.Pair ("SkipFinalSnapshot", (Boolean.to_query f)));
           Some
             (Query.Pair
                ("DBInstanceIdentifier",
                  (String.to_query v.d_b_instance_identifier)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.final_d_b_snapshot_identifier
              (fun f -> ("final_d_b_snapshot_identifier", (String.to_json f)));
           Util.option_map v.skip_final_snapshot
             (fun f -> ("skip_final_snapshot", (Boolean.to_json f)));
           Some
             ("d_b_instance_identifier",
               (String.to_json v.d_b_instance_identifier))])
    let of_json j =
      {
        d_b_instance_identifier =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "d_b_instance_identifier")));
        skip_final_snapshot =
          (Util.option_map (Json.lookup j "skip_final_snapshot")
             Boolean.of_json);
        final_d_b_snapshot_identifier =
          (Util.option_map (Json.lookup j "final_d_b_snapshot_identifier")
             String.of_json)
      }
  end
module PromoteReadReplicaMessage =
  struct
    type t =
      {
      d_b_instance_identifier: String.t ;
      backup_retention_period: Integer.t option ;
      preferred_backup_window: String.t option }
    let make ~d_b_instance_identifier  ?backup_retention_period 
      ?preferred_backup_window  () =
      {
        d_b_instance_identifier;
        backup_retention_period;
        preferred_backup_window
      }
    let parse xml =
      Some
        {
          d_b_instance_identifier =
            (Xml.required "DBInstanceIdentifier"
               (Util.option_bind (Xml.member "DBInstanceIdentifier" xml)
                  String.parse));
          backup_retention_period =
            (Util.option_bind (Xml.member "BackupRetentionPeriod" xml)
               Integer.parse);
          preferred_backup_window =
            (Util.option_bind (Xml.member "PreferredBackupWindow" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.preferred_backup_window
              (fun f ->
                 Query.Pair ("PreferredBackupWindow", (String.to_query f)));
           Util.option_map v.backup_retention_period
             (fun f ->
                Query.Pair ("BackupRetentionPeriod", (Integer.to_query f)));
           Some
             (Query.Pair
                ("DBInstanceIdentifier",
                  (String.to_query v.d_b_instance_identifier)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.preferred_backup_window
              (fun f -> ("preferred_backup_window", (String.to_json f)));
           Util.option_map v.backup_retention_period
             (fun f -> ("backup_retention_period", (Integer.to_json f)));
           Some
             ("d_b_instance_identifier",
               (String.to_json v.d_b_instance_identifier))])
    let of_json j =
      {
        d_b_instance_identifier =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "d_b_instance_identifier")));
        backup_retention_period =
          (Util.option_map (Json.lookup j "backup_retention_period")
             Integer.of_json);
        preferred_backup_window =
          (Util.option_map (Json.lookup j "preferred_backup_window")
             String.of_json)
      }
  end
module ModifyEventSubscriptionResult =
  struct
    type t = {
      event_subscription: EventSubscription.t option }
    let make ?event_subscription  () = { event_subscription }
    let parse xml =
      Some
        {
          event_subscription =
            (Util.option_bind (Xml.member "EventSubscription" xml)
               EventSubscription.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.event_subscription
              (fun f ->
                 Query.Pair
                   ("EventSubscription", (EventSubscription.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.event_subscription
              (fun f -> ("event_subscription", (EventSubscription.to_json f)))])
    let of_json j =
      {
        event_subscription =
          (Util.option_map (Json.lookup j "event_subscription")
             EventSubscription.of_json)
      }
  end
module DBParameterGroupAlreadyExistsFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module FailoverDBClusterResult =
  struct
    type t = {
      d_b_cluster: DBCluster.t option }
    let make ?d_b_cluster  () = { d_b_cluster }
    let parse xml =
      Some
        {
          d_b_cluster =
            (Util.option_bind (Xml.member "DBCluster" xml) DBCluster.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.d_b_cluster
              (fun f -> Query.Pair ("DBCluster", (DBCluster.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.d_b_cluster
              (fun f -> ("d_b_cluster", (DBCluster.to_json f)))])
    let of_json j =
      {
        d_b_cluster =
          (Util.option_map (Json.lookup j "d_b_cluster") DBCluster.of_json)
      }
  end
module OptionGroupOptionsMessage =
  struct
    type t =
      {
      option_group_options: OptionGroupOptionsList.t ;
      marker: String.t option }
    let make ?(option_group_options= [])  ?marker  () =
      { option_group_options; marker }
    let parse xml =
      Some
        {
          option_group_options =
            (Util.of_option []
               (Util.option_bind (Xml.member "OptionGroupOptions" xml)
                  OptionGroupOptionsList.parse));
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Some
             (Query.Pair
                ("OptionGroupOptions.member",
                  (OptionGroupOptionsList.to_query v.option_group_options)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Some
             ("option_group_options",
               (OptionGroupOptionsList.to_json v.option_group_options))])
    let of_json j =
      {
        option_group_options =
          (OptionGroupOptionsList.of_json
             (Util.of_option_exn (Json.lookup j "option_group_options")));
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module DescribeDBLogFilesMessage =
  struct
    type t =
      {
      d_b_instance_identifier: String.t ;
      filename_contains: String.t option ;
      file_last_written: Long.t option ;
      file_size: Long.t option ;
      filters: FilterList.t ;
      max_records: Integer.t option ;
      marker: String.t option }
    let make ~d_b_instance_identifier  ?filename_contains  ?file_last_written
       ?file_size  ?(filters= [])  ?max_records  ?marker  () =
      {
        d_b_instance_identifier;
        filename_contains;
        file_last_written;
        file_size;
        filters;
        max_records;
        marker
      }
    let parse xml =
      Some
        {
          d_b_instance_identifier =
            (Xml.required "DBInstanceIdentifier"
               (Util.option_bind (Xml.member "DBInstanceIdentifier" xml)
                  String.parse));
          filename_contains =
            (Util.option_bind (Xml.member "FilenameContains" xml)
               String.parse);
          file_last_written =
            (Util.option_bind (Xml.member "FileLastWritten" xml) Long.parse);
          file_size =
            (Util.option_bind (Xml.member "FileSize" xml) Long.parse);
          filters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Filters" xml) FilterList.parse));
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Some
             (Query.Pair ("Filters.member", (FilterList.to_query v.filters)));
           Util.option_map v.file_size
             (fun f -> Query.Pair ("FileSize", (Long.to_query f)));
           Util.option_map v.file_last_written
             (fun f -> Query.Pair ("FileLastWritten", (Long.to_query f)));
           Util.option_map v.filename_contains
             (fun f -> Query.Pair ("FilenameContains", (String.to_query f)));
           Some
             (Query.Pair
                ("DBInstanceIdentifier",
                  (String.to_query v.d_b_instance_identifier)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Some ("filters", (FilterList.to_json v.filters));
           Util.option_map v.file_size
             (fun f -> ("file_size", (Long.to_json f)));
           Util.option_map v.file_last_written
             (fun f -> ("file_last_written", (Long.to_json f)));
           Util.option_map v.filename_contains
             (fun f -> ("filename_contains", (String.to_json f)));
           Some
             ("d_b_instance_identifier",
               (String.to_json v.d_b_instance_identifier))])
    let of_json j =
      {
        d_b_instance_identifier =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "d_b_instance_identifier")));
        filename_contains =
          (Util.option_map (Json.lookup j "filename_contains") String.of_json);
        file_last_written =
          (Util.option_map (Json.lookup j "file_last_written") Long.of_json);
        file_size =
          (Util.option_map (Json.lookup j "file_size") Long.of_json);
        filters =
          (FilterList.of_json (Util.of_option_exn (Json.lookup j "filters")));
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module DeleteDBClusterResult =
  struct
    type t = {
      d_b_cluster: DBCluster.t option }
    let make ?d_b_cluster  () = { d_b_cluster }
    let parse xml =
      Some
        {
          d_b_cluster =
            (Util.option_bind (Xml.member "DBCluster" xml) DBCluster.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.d_b_cluster
              (fun f -> Query.Pair ("DBCluster", (DBCluster.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.d_b_cluster
              (fun f -> ("d_b_cluster", (DBCluster.to_json f)))])
    let of_json j =
      {
        d_b_cluster =
          (Util.option_map (Json.lookup j "d_b_cluster") DBCluster.of_json)
      }
  end
module InvalidDBClusterSnapshotStateFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DescribeDBSecurityGroupsMessage =
  struct
    type t =
      {
      d_b_security_group_name: String.t option ;
      filters: FilterList.t ;
      max_records: Integer.t option ;
      marker: String.t option }
    let make ?d_b_security_group_name  ?(filters= [])  ?max_records  ?marker 
      () = { d_b_security_group_name; filters; max_records; marker }
    let parse xml =
      Some
        {
          d_b_security_group_name =
            (Util.option_bind (Xml.member "DBSecurityGroupName" xml)
               String.parse);
          filters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Filters" xml) FilterList.parse));
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Some
             (Query.Pair ("Filters.member", (FilterList.to_query v.filters)));
           Util.option_map v.d_b_security_group_name
             (fun f ->
                Query.Pair ("DBSecurityGroupName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Some ("filters", (FilterList.to_json v.filters));
           Util.option_map v.d_b_security_group_name
             (fun f -> ("d_b_security_group_name", (String.to_json f)))])
    let of_json j =
      {
        d_b_security_group_name =
          (Util.option_map (Json.lookup j "d_b_security_group_name")
             String.of_json);
        filters =
          (FilterList.of_json (Util.of_option_exn (Json.lookup j "filters")));
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module InsufficientStorageClusterCapacityFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module AuthorizationNotFoundFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DBClusterQuotaExceededFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CreateDBSnapshotResult =
  struct
    type t = {
      d_b_snapshot: DBSnapshot.t option }
    let make ?d_b_snapshot  () = { d_b_snapshot }
    let parse xml =
      Some
        {
          d_b_snapshot =
            (Util.option_bind (Xml.member "DBSnapshot" xml) DBSnapshot.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.d_b_snapshot
              (fun f -> Query.Pair ("DBSnapshot", (DBSnapshot.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.d_b_snapshot
              (fun f -> ("d_b_snapshot", (DBSnapshot.to_json f)))])
    let of_json j =
      {
        d_b_snapshot =
          (Util.option_map (Json.lookup j "d_b_snapshot") DBSnapshot.of_json)
      }
  end
module CreateDBSubnetGroupMessage =
  struct
    type t =
      {
      d_b_subnet_group_name: String.t ;
      d_b_subnet_group_description: String.t ;
      subnet_ids: SubnetIdentifierList.t ;
      tags: TagList.t }
    let make ~d_b_subnet_group_name  ~d_b_subnet_group_description 
      ~subnet_ids  ?(tags= [])  () =
      { d_b_subnet_group_name; d_b_subnet_group_description; subnet_ids; tags
      }
    let parse xml =
      Some
        {
          d_b_subnet_group_name =
            (Xml.required "DBSubnetGroupName"
               (Util.option_bind (Xml.member "DBSubnetGroupName" xml)
                  String.parse));
          d_b_subnet_group_description =
            (Xml.required "DBSubnetGroupDescription"
               (Util.option_bind (Xml.member "DBSubnetGroupDescription" xml)
                  String.parse));
          subnet_ids =
            (Xml.required "SubnetIds"
               (Util.option_bind (Xml.member "SubnetIds" xml)
                  SubnetIdentifierList.parse));
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) TagList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Tags.member", (TagList.to_query v.tags)));
           Some
             (Query.Pair
                ("SubnetIds.member",
                  (SubnetIdentifierList.to_query v.subnet_ids)));
           Some
             (Query.Pair
                ("DBSubnetGroupDescription",
                  (String.to_query v.d_b_subnet_group_description)));
           Some
             (Query.Pair
                ("DBSubnetGroupName",
                  (String.to_query v.d_b_subnet_group_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tags", (TagList.to_json v.tags));
           Some ("subnet_ids", (SubnetIdentifierList.to_json v.subnet_ids));
           Some
             ("d_b_subnet_group_description",
               (String.to_json v.d_b_subnet_group_description));
           Some
             ("d_b_subnet_group_name",
               (String.to_json v.d_b_subnet_group_name))])
    let of_json j =
      {
        d_b_subnet_group_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "d_b_subnet_group_name")));
        d_b_subnet_group_description =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "d_b_subnet_group_description")));
        subnet_ids =
          (SubnetIdentifierList.of_json
             (Util.of_option_exn (Json.lookup j "subnet_ids")));
        tags = (TagList.of_json (Util.of_option_exn (Json.lookup j "tags")))
      }
  end
module DeleteDBSnapshotMessage =
  struct
    type t = {
      d_b_snapshot_identifier: String.t }
    let make ~d_b_snapshot_identifier  () = { d_b_snapshot_identifier }
    let parse xml =
      Some
        {
          d_b_snapshot_identifier =
            (Xml.required "DBSnapshotIdentifier"
               (Util.option_bind (Xml.member "DBSnapshotIdentifier" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("DBSnapshotIdentifier",
                   (String.to_query v.d_b_snapshot_identifier)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("d_b_snapshot_identifier",
                (String.to_json v.d_b_snapshot_identifier))])
    let of_json j =
      {
        d_b_snapshot_identifier =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "d_b_snapshot_identifier")))
      }
  end
module CreateDBInstanceReadReplicaResult =
  struct
    type t = {
      d_b_instance: DBInstance.t option }
    let make ?d_b_instance  () = { d_b_instance }
    let parse xml =
      Some
        {
          d_b_instance =
            (Util.option_bind (Xml.member "DBInstance" xml) DBInstance.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.d_b_instance
              (fun f -> Query.Pair ("DBInstance", (DBInstance.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.d_b_instance
              (fun f -> ("d_b_instance", (DBInstance.to_json f)))])
    let of_json j =
      {
        d_b_instance =
          (Util.option_map (Json.lookup j "d_b_instance") DBInstance.of_json)
      }
  end
module DescribeDBSubnetGroupsMessage =
  struct
    type t =
      {
      d_b_subnet_group_name: String.t option ;
      filters: FilterList.t ;
      max_records: Integer.t option ;
      marker: String.t option }
    let make ?d_b_subnet_group_name  ?(filters= [])  ?max_records  ?marker 
      () = { d_b_subnet_group_name; filters; max_records; marker }
    let parse xml =
      Some
        {
          d_b_subnet_group_name =
            (Util.option_bind (Xml.member "DBSubnetGroupName" xml)
               String.parse);
          filters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Filters" xml) FilterList.parse));
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Some
             (Query.Pair ("Filters.member", (FilterList.to_query v.filters)));
           Util.option_map v.d_b_subnet_group_name
             (fun f -> Query.Pair ("DBSubnetGroupName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Some ("filters", (FilterList.to_json v.filters));
           Util.option_map v.d_b_subnet_group_name
             (fun f -> ("d_b_subnet_group_name", (String.to_json f)))])
    let of_json j =
      {
        d_b_subnet_group_name =
          (Util.option_map (Json.lookup j "d_b_subnet_group_name")
             String.of_json);
        filters =
          (FilterList.of_json (Util.of_option_exn (Json.lookup j "filters")));
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module ReservedDBInstancesOfferingNotFoundFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DeleteDBSubnetGroupMessage =
  struct
    type t = {
      d_b_subnet_group_name: String.t }
    let make ~d_b_subnet_group_name  () = { d_b_subnet_group_name }
    let parse xml =
      Some
        {
          d_b_subnet_group_name =
            (Xml.required "DBSubnetGroupName"
               (Util.option_bind (Xml.member "DBSubnetGroupName" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("DBSubnetGroupName",
                   (String.to_query v.d_b_subnet_group_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("d_b_subnet_group_name",
                (String.to_json v.d_b_subnet_group_name))])
    let of_json j =
      {
        d_b_subnet_group_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "d_b_subnet_group_name")))
      }
  end
module AddSourceIdentifierToSubscriptionResult =
  struct
    type t = {
      event_subscription: EventSubscription.t option }
    let make ?event_subscription  () = { event_subscription }
    let parse xml =
      Some
        {
          event_subscription =
            (Util.option_bind (Xml.member "EventSubscription" xml)
               EventSubscription.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.event_subscription
              (fun f ->
                 Query.Pair
                   ("EventSubscription", (EventSubscription.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.event_subscription
              (fun f -> ("event_subscription", (EventSubscription.to_json f)))])
    let of_json j =
      {
        event_subscription =
          (Util.option_map (Json.lookup j "event_subscription")
             EventSubscription.of_json)
      }
  end
module DescribeEventSubscriptionsMessage =
  struct
    type t =
      {
      subscription_name: String.t option ;
      filters: FilterList.t ;
      max_records: Integer.t option ;
      marker: String.t option }
    let make ?subscription_name  ?(filters= [])  ?max_records  ?marker  () =
      { subscription_name; filters; max_records; marker }
    let parse xml =
      Some
        {
          subscription_name =
            (Util.option_bind (Xml.member "SubscriptionName" xml)
               String.parse);
          filters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Filters" xml) FilterList.parse));
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Some
             (Query.Pair ("Filters.member", (FilterList.to_query v.filters)));
           Util.option_map v.subscription_name
             (fun f -> Query.Pair ("SubscriptionName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Some ("filters", (FilterList.to_json v.filters));
           Util.option_map v.subscription_name
             (fun f -> ("subscription_name", (String.to_json f)))])
    let of_json j =
      {
        subscription_name =
          (Util.option_map (Json.lookup j "subscription_name") String.of_json);
        filters =
          (FilterList.of_json (Util.of_option_exn (Json.lookup j "filters")));
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module DescribeAccountAttributesMessage =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DBInstanceNotFoundFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module TagListMessage =
  struct
    type t = {
      tag_list: TagList.t }
    let make ?(tag_list= [])  () = { tag_list }
    let parse xml =
      Some
        {
          tag_list =
            (Util.of_option []
               (Util.option_bind (Xml.member "TagList" xml) TagList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("TagList.member", (TagList.to_query v.tag_list)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tag_list", (TagList.to_json v.tag_list))])
    let of_json j =
      {
        tag_list =
          (TagList.of_json (Util.of_option_exn (Json.lookup j "tag_list")))
      }
  end
module InvalidDBSubnetGroupStateFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module RebootDBInstanceMessage =
  struct
    type t =
      {
      d_b_instance_identifier: String.t ;
      force_failover: Boolean.t option }
    let make ~d_b_instance_identifier  ?force_failover  () =
      { d_b_instance_identifier; force_failover }
    let parse xml =
      Some
        {
          d_b_instance_identifier =
            (Xml.required "DBInstanceIdentifier"
               (Util.option_bind (Xml.member "DBInstanceIdentifier" xml)
                  String.parse));
          force_failover =
            (Util.option_bind (Xml.member "ForceFailover" xml) Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.force_failover
              (fun f -> Query.Pair ("ForceFailover", (Boolean.to_query f)));
           Some
             (Query.Pair
                ("DBInstanceIdentifier",
                  (String.to_query v.d_b_instance_identifier)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.force_failover
              (fun f -> ("force_failover", (Boolean.to_json f)));
           Some
             ("d_b_instance_identifier",
               (String.to_json v.d_b_instance_identifier))])
    let of_json j =
      {
        d_b_instance_identifier =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "d_b_instance_identifier")));
        force_failover =
          (Util.option_map (Json.lookup j "force_failover") Boolean.of_json)
      }
  end
module AuthorizationAlreadyExistsFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DeleteDBClusterSnapshotResult =
  struct
    type t = {
      d_b_cluster_snapshot: DBClusterSnapshot.t option }
    let make ?d_b_cluster_snapshot  () = { d_b_cluster_snapshot }
    let parse xml =
      Some
        {
          d_b_cluster_snapshot =
            (Util.option_bind (Xml.member "DBClusterSnapshot" xml)
               DBClusterSnapshot.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.d_b_cluster_snapshot
              (fun f ->
                 Query.Pair
                   ("DBClusterSnapshot", (DBClusterSnapshot.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.d_b_cluster_snapshot
              (fun f ->
                 ("d_b_cluster_snapshot", (DBClusterSnapshot.to_json f)))])
    let of_json j =
      {
        d_b_cluster_snapshot =
          (Util.option_map (Json.lookup j "d_b_cluster_snapshot")
             DBClusterSnapshot.of_json)
      }
  end
module DBSnapshotAlreadyExistsFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ModifyDBClusterMessage =
  struct
    type t =
      {
      d_b_cluster_identifier: String.t option ;
      new_d_b_cluster_identifier: String.t option ;
      apply_immediately: Boolean.t option ;
      backup_retention_period: Integer.t option ;
      d_b_cluster_parameter_group_name: String.t option ;
      vpc_security_group_ids: VpcSecurityGroupIdList.t ;
      port: Integer.t option ;
      master_user_password: String.t option ;
      option_group_name: String.t option ;
      preferred_backup_window: String.t option ;
      preferred_maintenance_window: String.t option }
    let make ?d_b_cluster_identifier  ?new_d_b_cluster_identifier 
      ?apply_immediately  ?backup_retention_period 
      ?d_b_cluster_parameter_group_name  ?(vpc_security_group_ids= [])  ?port
       ?master_user_password  ?option_group_name  ?preferred_backup_window 
      ?preferred_maintenance_window  () =
      {
        d_b_cluster_identifier;
        new_d_b_cluster_identifier;
        apply_immediately;
        backup_retention_period;
        d_b_cluster_parameter_group_name;
        vpc_security_group_ids;
        port;
        master_user_password;
        option_group_name;
        preferred_backup_window;
        preferred_maintenance_window
      }
    let parse xml =
      Some
        {
          d_b_cluster_identifier =
            (Util.option_bind (Xml.member "DBClusterIdentifier" xml)
               String.parse);
          new_d_b_cluster_identifier =
            (Util.option_bind (Xml.member "NewDBClusterIdentifier" xml)
               String.parse);
          apply_immediately =
            (Util.option_bind (Xml.member "ApplyImmediately" xml)
               Boolean.parse);
          backup_retention_period =
            (Util.option_bind (Xml.member "BackupRetentionPeriod" xml)
               Integer.parse);
          d_b_cluster_parameter_group_name =
            (Util.option_bind (Xml.member "DBClusterParameterGroupName" xml)
               String.parse);
          vpc_security_group_ids =
            (Util.of_option []
               (Util.option_bind (Xml.member "VpcSecurityGroupIds" xml)
                  VpcSecurityGroupIdList.parse));
          port = (Util.option_bind (Xml.member "Port" xml) Integer.parse);
          master_user_password =
            (Util.option_bind (Xml.member "MasterUserPassword" xml)
               String.parse);
          option_group_name =
            (Util.option_bind (Xml.member "OptionGroupName" xml) String.parse);
          preferred_backup_window =
            (Util.option_bind (Xml.member "PreferredBackupWindow" xml)
               String.parse);
          preferred_maintenance_window =
            (Util.option_bind (Xml.member "PreferredMaintenanceWindow" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.preferred_maintenance_window
              (fun f ->
                 Query.Pair
                   ("PreferredMaintenanceWindow", (String.to_query f)));
           Util.option_map v.preferred_backup_window
             (fun f ->
                Query.Pair ("PreferredBackupWindow", (String.to_query f)));
           Util.option_map v.option_group_name
             (fun f -> Query.Pair ("OptionGroupName", (String.to_query f)));
           Util.option_map v.master_user_password
             (fun f -> Query.Pair ("MasterUserPassword", (String.to_query f)));
           Util.option_map v.port
             (fun f -> Query.Pair ("Port", (Integer.to_query f)));
           Some
             (Query.Pair
                ("VpcSecurityGroupIds.member",
                  (VpcSecurityGroupIdList.to_query v.vpc_security_group_ids)));
           Util.option_map v.d_b_cluster_parameter_group_name
             (fun f ->
                Query.Pair
                  ("DBClusterParameterGroupName", (String.to_query f)));
           Util.option_map v.backup_retention_period
             (fun f ->
                Query.Pair ("BackupRetentionPeriod", (Integer.to_query f)));
           Util.option_map v.apply_immediately
             (fun f -> Query.Pair ("ApplyImmediately", (Boolean.to_query f)));
           Util.option_map v.new_d_b_cluster_identifier
             (fun f ->
                Query.Pair ("NewDBClusterIdentifier", (String.to_query f)));
           Util.option_map v.d_b_cluster_identifier
             (fun f ->
                Query.Pair ("DBClusterIdentifier", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.preferred_maintenance_window
              (fun f -> ("preferred_maintenance_window", (String.to_json f)));
           Util.option_map v.preferred_backup_window
             (fun f -> ("preferred_backup_window", (String.to_json f)));
           Util.option_map v.option_group_name
             (fun f -> ("option_group_name", (String.to_json f)));
           Util.option_map v.master_user_password
             (fun f -> ("master_user_password", (String.to_json f)));
           Util.option_map v.port (fun f -> ("port", (Integer.to_json f)));
           Some
             ("vpc_security_group_ids",
               (VpcSecurityGroupIdList.to_json v.vpc_security_group_ids));
           Util.option_map v.d_b_cluster_parameter_group_name
             (fun f ->
                ("d_b_cluster_parameter_group_name", (String.to_json f)));
           Util.option_map v.backup_retention_period
             (fun f -> ("backup_retention_period", (Integer.to_json f)));
           Util.option_map v.apply_immediately
             (fun f -> ("apply_immediately", (Boolean.to_json f)));
           Util.option_map v.new_d_b_cluster_identifier
             (fun f -> ("new_d_b_cluster_identifier", (String.to_json f)));
           Util.option_map v.d_b_cluster_identifier
             (fun f -> ("d_b_cluster_identifier", (String.to_json f)))])
    let of_json j =
      {
        d_b_cluster_identifier =
          (Util.option_map (Json.lookup j "d_b_cluster_identifier")
             String.of_json);
        new_d_b_cluster_identifier =
          (Util.option_map (Json.lookup j "new_d_b_cluster_identifier")
             String.of_json);
        apply_immediately =
          (Util.option_map (Json.lookup j "apply_immediately")
             Boolean.of_json);
        backup_retention_period =
          (Util.option_map (Json.lookup j "backup_retention_period")
             Integer.of_json);
        d_b_cluster_parameter_group_name =
          (Util.option_map (Json.lookup j "d_b_cluster_parameter_group_name")
             String.of_json);
        vpc_security_group_ids =
          (VpcSecurityGroupIdList.of_json
             (Util.of_option_exn (Json.lookup j "vpc_security_group_ids")));
        port = (Util.option_map (Json.lookup j "port") Integer.of_json);
        master_user_password =
          (Util.option_map (Json.lookup j "master_user_password")
             String.of_json);
        option_group_name =
          (Util.option_map (Json.lookup j "option_group_name") String.of_json);
        preferred_backup_window =
          (Util.option_map (Json.lookup j "preferred_backup_window")
             String.of_json);
        preferred_maintenance_window =
          (Util.option_map (Json.lookup j "preferred_maintenance_window")
             String.of_json)
      }
  end
module CreateDBInstanceResult =
  struct
    type t = {
      d_b_instance: DBInstance.t option }
    let make ?d_b_instance  () = { d_b_instance }
    let parse xml =
      Some
        {
          d_b_instance =
            (Util.option_bind (Xml.member "DBInstance" xml) DBInstance.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.d_b_instance
              (fun f -> Query.Pair ("DBInstance", (DBInstance.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.d_b_instance
              (fun f -> ("d_b_instance", (DBInstance.to_json f)))])
    let of_json j =
      {
        d_b_instance =
          (Util.option_map (Json.lookup j "d_b_instance") DBInstance.of_json)
      }
  end
module DBClusterParameterGroupNameMessage =
  struct
    type t = {
      d_b_cluster_parameter_group_name: String.t option }
    let make ?d_b_cluster_parameter_group_name  () =
      { d_b_cluster_parameter_group_name }
    let parse xml =
      Some
        {
          d_b_cluster_parameter_group_name =
            (Util.option_bind (Xml.member "DBClusterParameterGroupName" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.d_b_cluster_parameter_group_name
              (fun f ->
                 Query.Pair
                   ("DBClusterParameterGroupName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.d_b_cluster_parameter_group_name
              (fun f ->
                 ("d_b_cluster_parameter_group_name", (String.to_json f)))])
    let of_json j =
      {
        d_b_cluster_parameter_group_name =
          (Util.option_map (Json.lookup j "d_b_cluster_parameter_group_name")
             String.of_json)
      }
  end
module DBClusterSnapshotAlreadyExistsFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DeleteDBClusterSnapshotMessage =
  struct
    type t = {
      d_b_cluster_snapshot_identifier: String.t }
    let make ~d_b_cluster_snapshot_identifier  () =
      { d_b_cluster_snapshot_identifier }
    let parse xml =
      Some
        {
          d_b_cluster_snapshot_identifier =
            (Xml.required "DBClusterSnapshotIdentifier"
               (Util.option_bind
                  (Xml.member "DBClusterSnapshotIdentifier" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("DBClusterSnapshotIdentifier",
                   (String.to_query v.d_b_cluster_snapshot_identifier)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("d_b_cluster_snapshot_identifier",
                (String.to_json v.d_b_cluster_snapshot_identifier))])
    let of_json j =
      {
        d_b_cluster_snapshot_identifier =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "d_b_cluster_snapshot_identifier")))
      }
  end
module DBSubnetGroupDoesNotCoverEnoughAZs =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DeleteDBClusterMessage =
  struct
    type t =
      {
      d_b_cluster_identifier: String.t option ;
      skip_final_snapshot: Boolean.t option ;
      final_d_b_snapshot_identifier: String.t option }
    let make ?d_b_cluster_identifier  ?skip_final_snapshot 
      ?final_d_b_snapshot_identifier  () =
      {
        d_b_cluster_identifier;
        skip_final_snapshot;
        final_d_b_snapshot_identifier
      }
    let parse xml =
      Some
        {
          d_b_cluster_identifier =
            (Util.option_bind (Xml.member "DBClusterIdentifier" xml)
               String.parse);
          skip_final_snapshot =
            (Util.option_bind (Xml.member "SkipFinalSnapshot" xml)
               Boolean.parse);
          final_d_b_snapshot_identifier =
            (Util.option_bind (Xml.member "FinalDBSnapshotIdentifier" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.final_d_b_snapshot_identifier
              (fun f ->
                 Query.Pair
                   ("FinalDBSnapshotIdentifier", (String.to_query f)));
           Util.option_map v.skip_final_snapshot
             (fun f -> Query.Pair ("SkipFinalSnapshot", (Boolean.to_query f)));
           Util.option_map v.d_b_cluster_identifier
             (fun f ->
                Query.Pair ("DBClusterIdentifier", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.final_d_b_snapshot_identifier
              (fun f -> ("final_d_b_snapshot_identifier", (String.to_json f)));
           Util.option_map v.skip_final_snapshot
             (fun f -> ("skip_final_snapshot", (Boolean.to_json f)));
           Util.option_map v.d_b_cluster_identifier
             (fun f -> ("d_b_cluster_identifier", (String.to_json f)))])
    let of_json j =
      {
        d_b_cluster_identifier =
          (Util.option_map (Json.lookup j "d_b_cluster_identifier")
             String.of_json);
        skip_final_snapshot =
          (Util.option_map (Json.lookup j "skip_final_snapshot")
             Boolean.of_json);
        final_d_b_snapshot_identifier =
          (Util.option_map (Json.lookup j "final_d_b_snapshot_identifier")
             String.of_json)
      }
  end
module CreateDBClusterResult =
  struct
    type t = {
      d_b_cluster: DBCluster.t option }
    let make ?d_b_cluster  () = { d_b_cluster }
    let parse xml =
      Some
        {
          d_b_cluster =
            (Util.option_bind (Xml.member "DBCluster" xml) DBCluster.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.d_b_cluster
              (fun f -> Query.Pair ("DBCluster", (DBCluster.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.d_b_cluster
              (fun f -> ("d_b_cluster", (DBCluster.to_json f)))])
    let of_json j =
      {
        d_b_cluster =
          (Util.option_map (Json.lookup j "d_b_cluster") DBCluster.of_json)
      }
  end
module DomainNotFoundFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DescribeEngineDefaultParametersResult =
  struct
    type t = {
      engine_defaults: EngineDefaults.t }
    let make ~engine_defaults  () = { engine_defaults }
    let parse xml =
      Some
        {
          engine_defaults =
            (Xml.required "EngineDefaults"
               (Util.option_bind (Xml.member "EngineDefaults" xml)
                  EngineDefaults.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("EngineDefaults",
                   (EngineDefaults.to_query v.engine_defaults)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("engine_defaults", (EngineDefaults.to_json v.engine_defaults))])
    let of_json j =
      {
        engine_defaults =
          (EngineDefaults.of_json
             (Util.of_option_exn (Json.lookup j "engine_defaults")))
      }
  end
module InvalidDBSecurityGroupStateFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module PointInTimeRestoreNotEnabledFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module EventSubscriptionsMessage =
  struct
    type t =
      {
      marker: String.t option ;
      event_subscriptions_list: EventSubscriptionsList.t }
    let make ?marker  ?(event_subscriptions_list= [])  () =
      { marker; event_subscriptions_list }
    let parse xml =
      Some
        {
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          event_subscriptions_list =
            (Util.of_option []
               (Util.option_bind (Xml.member "EventSubscriptionsList" xml)
                  EventSubscriptionsList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("EventSubscriptionsList.member",
                   (EventSubscriptionsList.to_query
                      v.event_subscriptions_list)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("event_subscriptions_list",
                (EventSubscriptionsList.to_json v.event_subscriptions_list));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)))])
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        event_subscriptions_list =
          (EventSubscriptionsList.of_json
             (Util.of_option_exn (Json.lookup j "event_subscriptions_list")))
      }
  end
module DescribeEngineDefaultClusterParametersMessage =
  struct
    type t =
      {
      d_b_parameter_group_family: String.t ;
      filters: FilterList.t ;
      max_records: Integer.t option ;
      marker: String.t option }
    let make ~d_b_parameter_group_family  ?(filters= [])  ?max_records 
      ?marker  () =
      { d_b_parameter_group_family; filters; max_records; marker }
    let parse xml =
      Some
        {
          d_b_parameter_group_family =
            (Xml.required "DBParameterGroupFamily"
               (Util.option_bind (Xml.member "DBParameterGroupFamily" xml)
                  String.parse));
          filters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Filters" xml) FilterList.parse));
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Some
             (Query.Pair ("Filters.member", (FilterList.to_query v.filters)));
           Some
             (Query.Pair
                ("DBParameterGroupFamily",
                  (String.to_query v.d_b_parameter_group_family)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Some ("filters", (FilterList.to_json v.filters));
           Some
             ("d_b_parameter_group_family",
               (String.to_json v.d_b_parameter_group_family))])
    let of_json j =
      {
        d_b_parameter_group_family =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "d_b_parameter_group_family")));
        filters =
          (FilterList.of_json (Util.of_option_exn (Json.lookup j "filters")));
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module AddSourceIdentifierToSubscriptionMessage =
  struct
    type t = {
      subscription_name: String.t ;
      source_identifier: String.t }
    let make ~subscription_name  ~source_identifier  () =
      { subscription_name; source_identifier }
    let parse xml =
      Some
        {
          subscription_name =
            (Xml.required "SubscriptionName"
               (Util.option_bind (Xml.member "SubscriptionName" xml)
                  String.parse));
          source_identifier =
            (Xml.required "SourceIdentifier"
               (Util.option_bind (Xml.member "SourceIdentifier" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("SourceIdentifier", (String.to_query v.source_identifier)));
           Some
             (Query.Pair
                ("SubscriptionName", (String.to_query v.subscription_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("source_identifier", (String.to_json v.source_identifier));
           Some ("subscription_name", (String.to_json v.subscription_name))])
    let of_json j =
      {
        subscription_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "subscription_name")));
        source_identifier =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "source_identifier")))
      }
  end
module DBClusterSnapshotNotFoundFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DBSubnetQuotaExceededFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module StorageTypeNotSupportedFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module InvalidSubnet =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ModifyOptionGroupMessage =
  struct
    type t =
      {
      option_group_name: String.t ;
      options_to_include: OptionConfigurationList.t ;
      options_to_remove: OptionNamesList.t ;
      apply_immediately: Boolean.t option }
    let make ~option_group_name  ?(options_to_include= []) 
      ?(options_to_remove= [])  ?apply_immediately  () =
      {
        option_group_name;
        options_to_include;
        options_to_remove;
        apply_immediately
      }
    let parse xml =
      Some
        {
          option_group_name =
            (Xml.required "OptionGroupName"
               (Util.option_bind (Xml.member "OptionGroupName" xml)
                  String.parse));
          options_to_include =
            (Util.of_option []
               (Util.option_bind (Xml.member "OptionsToInclude" xml)
                  OptionConfigurationList.parse));
          options_to_remove =
            (Util.of_option []
               (Util.option_bind (Xml.member "OptionsToRemove" xml)
                  OptionNamesList.parse));
          apply_immediately =
            (Util.option_bind (Xml.member "ApplyImmediately" xml)
               Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.apply_immediately
              (fun f -> Query.Pair ("ApplyImmediately", (Boolean.to_query f)));
           Some
             (Query.Pair
                ("OptionsToRemove.member",
                  (OptionNamesList.to_query v.options_to_remove)));
           Some
             (Query.Pair
                ("OptionsToInclude.member",
                  (OptionConfigurationList.to_query v.options_to_include)));
           Some
             (Query.Pair
                ("OptionGroupName", (String.to_query v.option_group_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.apply_immediately
              (fun f -> ("apply_immediately", (Boolean.to_json f)));
           Some
             ("options_to_remove",
               (OptionNamesList.to_json v.options_to_remove));
           Some
             ("options_to_include",
               (OptionConfigurationList.to_json v.options_to_include));
           Some ("option_group_name", (String.to_json v.option_group_name))])
    let of_json j =
      {
        option_group_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "option_group_name")));
        options_to_include =
          (OptionConfigurationList.of_json
             (Util.of_option_exn (Json.lookup j "options_to_include")));
        options_to_remove =
          (OptionNamesList.of_json
             (Util.of_option_exn (Json.lookup j "options_to_remove")));
        apply_immediately =
          (Util.option_map (Json.lookup j "apply_immediately")
             Boolean.of_json)
      }
  end
module ReservedDBInstanceMessage =
  struct
    type t =
      {
      marker: String.t option ;
      reserved_d_b_instances: ReservedDBInstanceList.t }
    let make ?marker  ?(reserved_d_b_instances= [])  () =
      { marker; reserved_d_b_instances }
    let parse xml =
      Some
        {
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          reserved_d_b_instances =
            (Util.of_option []
               (Util.option_bind (Xml.member "ReservedDBInstances" xml)
                  ReservedDBInstanceList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ReservedDBInstances.member",
                   (ReservedDBInstanceList.to_query v.reserved_d_b_instances)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("reserved_d_b_instances",
                (ReservedDBInstanceList.to_json v.reserved_d_b_instances));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)))])
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        reserved_d_b_instances =
          (ReservedDBInstanceList.of_json
             (Util.of_option_exn (Json.lookup j "reserved_d_b_instances")))
      }
  end
module DBParameterGroupDetails =
  struct
    type t = {
      parameters: ParametersList.t ;
      marker: String.t option }
    let make ?(parameters= [])  ?marker  () = { parameters; marker }
    let parse xml =
      Some
        {
          parameters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Parameters" xml)
                  ParametersList.parse));
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Some
             (Query.Pair
                ("Parameters.member", (ParametersList.to_query v.parameters)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Some ("parameters", (ParametersList.to_json v.parameters))])
    let of_json j =
      {
        parameters =
          (ParametersList.of_json
             (Util.of_option_exn (Json.lookup j "parameters")));
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module AddTagsToResourceMessage =
  struct
    type t = {
      resource_name: String.t ;
      tags: TagList.t }
    let make ~resource_name  ~tags  () = { resource_name; tags }
    let parse xml =
      Some
        {
          resource_name =
            (Xml.required "ResourceName"
               (Util.option_bind (Xml.member "ResourceName" xml) String.parse));
          tags =
            (Xml.required "Tags"
               (Util.option_bind (Xml.member "Tags" xml) TagList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Tags.member", (TagList.to_query v.tags)));
           Some
             (Query.Pair ("ResourceName", (String.to_query v.resource_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tags", (TagList.to_json v.tags));
           Some ("resource_name", (String.to_json v.resource_name))])
    let of_json j =
      {
        resource_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "resource_name")));
        tags = (TagList.of_json (Util.of_option_exn (Json.lookup j "tags")))
      }
  end
module ApplyPendingMaintenanceActionResult =
  struct
    type t =
      {
      resource_pending_maintenance_actions:
        ResourcePendingMaintenanceActions.t option }
    let make ?resource_pending_maintenance_actions  () =
      { resource_pending_maintenance_actions }
    let parse xml =
      Some
        {
          resource_pending_maintenance_actions =
            (Util.option_bind
               (Xml.member "ResourcePendingMaintenanceActions" xml)
               ResourcePendingMaintenanceActions.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.resource_pending_maintenance_actions
              (fun f ->
                 Query.Pair
                   ("ResourcePendingMaintenanceActions",
                     (ResourcePendingMaintenanceActions.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.resource_pending_maintenance_actions
              (fun f ->
                 ("resource_pending_maintenance_actions",
                   (ResourcePendingMaintenanceActions.to_json f)))])
    let of_json j =
      {
        resource_pending_maintenance_actions =
          (Util.option_map
             (Json.lookup j "resource_pending_maintenance_actions")
             ResourcePendingMaintenanceActions.of_json)
      }
  end
module InvalidOptionGroupStateFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ModifyDBSubnetGroupResult =
  struct
    type t = {
      d_b_subnet_group: DBSubnetGroup.t option }
    let make ?d_b_subnet_group  () = { d_b_subnet_group }
    let parse xml =
      Some
        {
          d_b_subnet_group =
            (Util.option_bind (Xml.member "DBSubnetGroup" xml)
               DBSubnetGroup.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.d_b_subnet_group
              (fun f ->
                 Query.Pair ("DBSubnetGroup", (DBSubnetGroup.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.d_b_subnet_group
              (fun f -> ("d_b_subnet_group", (DBSubnetGroup.to_json f)))])
    let of_json j =
      {
        d_b_subnet_group =
          (Util.option_map (Json.lookup j "d_b_subnet_group")
             DBSubnetGroup.of_json)
      }
  end
module InsufficientDomainCapacityFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module SNSNoAuthorizationFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CreateDBClusterSnapshotResult =
  struct
    type t = {
      d_b_cluster_snapshot: DBClusterSnapshot.t option }
    let make ?d_b_cluster_snapshot  () = { d_b_cluster_snapshot }
    let parse xml =
      Some
        {
          d_b_cluster_snapshot =
            (Util.option_bind (Xml.member "DBClusterSnapshot" xml)
               DBClusterSnapshot.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.d_b_cluster_snapshot
              (fun f ->
                 Query.Pair
                   ("DBClusterSnapshot", (DBClusterSnapshot.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.d_b_cluster_snapshot
              (fun f ->
                 ("d_b_cluster_snapshot", (DBClusterSnapshot.to_json f)))])
    let of_json j =
      {
        d_b_cluster_snapshot =
          (Util.option_map (Json.lookup j "d_b_cluster_snapshot")
             DBClusterSnapshot.of_json)
      }
  end
module CreateOptionGroupMessage =
  struct
    type t =
      {
      option_group_name: String.t ;
      engine_name: String.t ;
      major_engine_version: String.t ;
      option_group_description: String.t ;
      tags: TagList.t }
    let make ~option_group_name  ~engine_name  ~major_engine_version 
      ~option_group_description  ?(tags= [])  () =
      {
        option_group_name;
        engine_name;
        major_engine_version;
        option_group_description;
        tags
      }
    let parse xml =
      Some
        {
          option_group_name =
            (Xml.required "OptionGroupName"
               (Util.option_bind (Xml.member "OptionGroupName" xml)
                  String.parse));
          engine_name =
            (Xml.required "EngineName"
               (Util.option_bind (Xml.member "EngineName" xml) String.parse));
          major_engine_version =
            (Xml.required "MajorEngineVersion"
               (Util.option_bind (Xml.member "MajorEngineVersion" xml)
                  String.parse));
          option_group_description =
            (Xml.required "OptionGroupDescription"
               (Util.option_bind (Xml.member "OptionGroupDescription" xml)
                  String.parse));
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) TagList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Tags.member", (TagList.to_query v.tags)));
           Some
             (Query.Pair
                ("OptionGroupDescription",
                  (String.to_query v.option_group_description)));
           Some
             (Query.Pair
                ("MajorEngineVersion",
                  (String.to_query v.major_engine_version)));
           Some (Query.Pair ("EngineName", (String.to_query v.engine_name)));
           Some
             (Query.Pair
                ("OptionGroupName", (String.to_query v.option_group_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tags", (TagList.to_json v.tags));
           Some
             ("option_group_description",
               (String.to_json v.option_group_description));
           Some
             ("major_engine_version",
               (String.to_json v.major_engine_version));
           Some ("engine_name", (String.to_json v.engine_name));
           Some ("option_group_name", (String.to_json v.option_group_name))])
    let of_json j =
      {
        option_group_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "option_group_name")));
        engine_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "engine_name")));
        major_engine_version =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "major_engine_version")));
        option_group_description =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "option_group_description")));
        tags = (TagList.of_json (Util.of_option_exn (Json.lookup j "tags")))
      }
  end
module InvalidVPCNetworkStateFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module SubnetAlreadyInUse =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ProvisionedIopsNotAvailableInAZFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module PendingMaintenanceActionsMessage =
  struct
    type t =
      {
      pending_maintenance_actions: PendingMaintenanceActions.t ;
      marker: String.t option }
    let make ?(pending_maintenance_actions= [])  ?marker  () =
      { pending_maintenance_actions; marker }
    let parse xml =
      Some
        {
          pending_maintenance_actions =
            (Util.of_option []
               (Util.option_bind (Xml.member "PendingMaintenanceActions" xml)
                  PendingMaintenanceActions.parse));
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Some
             (Query.Pair
                ("PendingMaintenanceActions.member",
                  (PendingMaintenanceActions.to_query
                     v.pending_maintenance_actions)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Some
             ("pending_maintenance_actions",
               (PendingMaintenanceActions.to_json
                  v.pending_maintenance_actions))])
    let of_json j =
      {
        pending_maintenance_actions =
          (PendingMaintenanceActions.of_json
             (Util.of_option_exn
                (Json.lookup j "pending_maintenance_actions")));
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module RestoreDBInstanceToPointInTimeMessage =
  struct
    type t =
      {
      source_d_b_instance_identifier: String.t ;
      target_d_b_instance_identifier: String.t ;
      restore_time: DateTime.t option ;
      use_latest_restorable_time: Boolean.t option ;
      d_b_instance_class: String.t option ;
      port: Integer.t option ;
      availability_zone: String.t option ;
      d_b_subnet_group_name: String.t option ;
      multi_a_z: Boolean.t option ;
      publicly_accessible: Boolean.t option ;
      auto_minor_version_upgrade: Boolean.t option ;
      license_model: String.t option ;
      d_b_name: String.t option ;
      engine: String.t option ;
      iops: Integer.t option ;
      option_group_name: String.t option ;
      copy_tags_to_snapshot: Boolean.t option ;
      tags: TagList.t ;
      storage_type: String.t option ;
      tde_credential_arn: String.t option ;
      tde_credential_password: String.t option ;
      vpc_security_group_ids: VpcSecurityGroupIdList.t ;
      d_b_security_groups: DBSecurityGroupNameList.t ;
      domain: String.t option }
    let make ~source_d_b_instance_identifier  ~target_d_b_instance_identifier
       ?restore_time  ?use_latest_restorable_time  ?d_b_instance_class  ?port
       ?availability_zone  ?d_b_subnet_group_name  ?multi_a_z 
      ?publicly_accessible  ?auto_minor_version_upgrade  ?license_model 
      ?d_b_name  ?engine  ?iops  ?option_group_name  ?copy_tags_to_snapshot 
      ?(tags= [])  ?storage_type  ?tde_credential_arn 
      ?tde_credential_password  ?(vpc_security_group_ids= []) 
      ?(d_b_security_groups= [])  ?domain  () =
      {
        source_d_b_instance_identifier;
        target_d_b_instance_identifier;
        restore_time;
        use_latest_restorable_time;
        d_b_instance_class;
        port;
        availability_zone;
        d_b_subnet_group_name;
        multi_a_z;
        publicly_accessible;
        auto_minor_version_upgrade;
        license_model;
        d_b_name;
        engine;
        iops;
        option_group_name;
        copy_tags_to_snapshot;
        tags;
        storage_type;
        tde_credential_arn;
        tde_credential_password;
        vpc_security_group_ids;
        d_b_security_groups;
        domain
      }
    let parse xml =
      Some
        {
          source_d_b_instance_identifier =
            (Xml.required "SourceDBInstanceIdentifier"
               (Util.option_bind
                  (Xml.member "SourceDBInstanceIdentifier" xml) String.parse));
          target_d_b_instance_identifier =
            (Xml.required "TargetDBInstanceIdentifier"
               (Util.option_bind
                  (Xml.member "TargetDBInstanceIdentifier" xml) String.parse));
          restore_time =
            (Util.option_bind (Xml.member "RestoreTime" xml) DateTime.parse);
          use_latest_restorable_time =
            (Util.option_bind (Xml.member "UseLatestRestorableTime" xml)
               Boolean.parse);
          d_b_instance_class =
            (Util.option_bind (Xml.member "DBInstanceClass" xml) String.parse);
          port = (Util.option_bind (Xml.member "Port" xml) Integer.parse);
          availability_zone =
            (Util.option_bind (Xml.member "AvailabilityZone" xml)
               String.parse);
          d_b_subnet_group_name =
            (Util.option_bind (Xml.member "DBSubnetGroupName" xml)
               String.parse);
          multi_a_z =
            (Util.option_bind (Xml.member "MultiAZ" xml) Boolean.parse);
          publicly_accessible =
            (Util.option_bind (Xml.member "PubliclyAccessible" xml)
               Boolean.parse);
          auto_minor_version_upgrade =
            (Util.option_bind (Xml.member "AutoMinorVersionUpgrade" xml)
               Boolean.parse);
          license_model =
            (Util.option_bind (Xml.member "LicenseModel" xml) String.parse);
          d_b_name =
            (Util.option_bind (Xml.member "DBName" xml) String.parse);
          engine = (Util.option_bind (Xml.member "Engine" xml) String.parse);
          iops = (Util.option_bind (Xml.member "Iops" xml) Integer.parse);
          option_group_name =
            (Util.option_bind (Xml.member "OptionGroupName" xml) String.parse);
          copy_tags_to_snapshot =
            (Util.option_bind (Xml.member "CopyTagsToSnapshot" xml)
               Boolean.parse);
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) TagList.parse));
          storage_type =
            (Util.option_bind (Xml.member "StorageType" xml) String.parse);
          tde_credential_arn =
            (Util.option_bind (Xml.member "TdeCredentialArn" xml)
               String.parse);
          tde_credential_password =
            (Util.option_bind (Xml.member "TdeCredentialPassword" xml)
               String.parse);
          vpc_security_group_ids =
            (Util.of_option []
               (Util.option_bind (Xml.member "VpcSecurityGroupIds" xml)
                  VpcSecurityGroupIdList.parse));
          d_b_security_groups =
            (Util.of_option []
               (Util.option_bind (Xml.member "DBSecurityGroups" xml)
                  DBSecurityGroupNameList.parse));
          domain = (Util.option_bind (Xml.member "Domain" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.domain
              (fun f -> Query.Pair ("Domain", (String.to_query f)));
           Some
             (Query.Pair
                ("DBSecurityGroups.member",
                  (DBSecurityGroupNameList.to_query v.d_b_security_groups)));
           Some
             (Query.Pair
                ("VpcSecurityGroupIds.member",
                  (VpcSecurityGroupIdList.to_query v.vpc_security_group_ids)));
           Util.option_map v.tde_credential_password
             (fun f ->
                Query.Pair ("TdeCredentialPassword", (String.to_query f)));
           Util.option_map v.tde_credential_arn
             (fun f -> Query.Pair ("TdeCredentialArn", (String.to_query f)));
           Util.option_map v.storage_type
             (fun f -> Query.Pair ("StorageType", (String.to_query f)));
           Some (Query.Pair ("Tags.member", (TagList.to_query v.tags)));
           Util.option_map v.copy_tags_to_snapshot
             (fun f ->
                Query.Pair ("CopyTagsToSnapshot", (Boolean.to_query f)));
           Util.option_map v.option_group_name
             (fun f -> Query.Pair ("OptionGroupName", (String.to_query f)));
           Util.option_map v.iops
             (fun f -> Query.Pair ("Iops", (Integer.to_query f)));
           Util.option_map v.engine
             (fun f -> Query.Pair ("Engine", (String.to_query f)));
           Util.option_map v.d_b_name
             (fun f -> Query.Pair ("DBName", (String.to_query f)));
           Util.option_map v.license_model
             (fun f -> Query.Pair ("LicenseModel", (String.to_query f)));
           Util.option_map v.auto_minor_version_upgrade
             (fun f ->
                Query.Pair ("AutoMinorVersionUpgrade", (Boolean.to_query f)));
           Util.option_map v.publicly_accessible
             (fun f ->
                Query.Pair ("PubliclyAccessible", (Boolean.to_query f)));
           Util.option_map v.multi_a_z
             (fun f -> Query.Pair ("MultiAZ", (Boolean.to_query f)));
           Util.option_map v.d_b_subnet_group_name
             (fun f -> Query.Pair ("DBSubnetGroupName", (String.to_query f)));
           Util.option_map v.availability_zone
             (fun f -> Query.Pair ("AvailabilityZone", (String.to_query f)));
           Util.option_map v.port
             (fun f -> Query.Pair ("Port", (Integer.to_query f)));
           Util.option_map v.d_b_instance_class
             (fun f -> Query.Pair ("DBInstanceClass", (String.to_query f)));
           Util.option_map v.use_latest_restorable_time
             (fun f ->
                Query.Pair ("UseLatestRestorableTime", (Boolean.to_query f)));
           Util.option_map v.restore_time
             (fun f -> Query.Pair ("RestoreTime", (DateTime.to_query f)));
           Some
             (Query.Pair
                ("TargetDBInstanceIdentifier",
                  (String.to_query v.target_d_b_instance_identifier)));
           Some
             (Query.Pair
                ("SourceDBInstanceIdentifier",
                  (String.to_query v.source_d_b_instance_identifier)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.domain
              (fun f -> ("domain", (String.to_json f)));
           Some
             ("d_b_security_groups",
               (DBSecurityGroupNameList.to_json v.d_b_security_groups));
           Some
             ("vpc_security_group_ids",
               (VpcSecurityGroupIdList.to_json v.vpc_security_group_ids));
           Util.option_map v.tde_credential_password
             (fun f -> ("tde_credential_password", (String.to_json f)));
           Util.option_map v.tde_credential_arn
             (fun f -> ("tde_credential_arn", (String.to_json f)));
           Util.option_map v.storage_type
             (fun f -> ("storage_type", (String.to_json f)));
           Some ("tags", (TagList.to_json v.tags));
           Util.option_map v.copy_tags_to_snapshot
             (fun f -> ("copy_tags_to_snapshot", (Boolean.to_json f)));
           Util.option_map v.option_group_name
             (fun f -> ("option_group_name", (String.to_json f)));
           Util.option_map v.iops (fun f -> ("iops", (Integer.to_json f)));
           Util.option_map v.engine (fun f -> ("engine", (String.to_json f)));
           Util.option_map v.d_b_name
             (fun f -> ("d_b_name", (String.to_json f)));
           Util.option_map v.license_model
             (fun f -> ("license_model", (String.to_json f)));
           Util.option_map v.auto_minor_version_upgrade
             (fun f -> ("auto_minor_version_upgrade", (Boolean.to_json f)));
           Util.option_map v.publicly_accessible
             (fun f -> ("publicly_accessible", (Boolean.to_json f)));
           Util.option_map v.multi_a_z
             (fun f -> ("multi_a_z", (Boolean.to_json f)));
           Util.option_map v.d_b_subnet_group_name
             (fun f -> ("d_b_subnet_group_name", (String.to_json f)));
           Util.option_map v.availability_zone
             (fun f -> ("availability_zone", (String.to_json f)));
           Util.option_map v.port (fun f -> ("port", (Integer.to_json f)));
           Util.option_map v.d_b_instance_class
             (fun f -> ("d_b_instance_class", (String.to_json f)));
           Util.option_map v.use_latest_restorable_time
             (fun f -> ("use_latest_restorable_time", (Boolean.to_json f)));
           Util.option_map v.restore_time
             (fun f -> ("restore_time", (DateTime.to_json f)));
           Some
             ("target_d_b_instance_identifier",
               (String.to_json v.target_d_b_instance_identifier));
           Some
             ("source_d_b_instance_identifier",
               (String.to_json v.source_d_b_instance_identifier))])
    let of_json j =
      {
        source_d_b_instance_identifier =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "source_d_b_instance_identifier")));
        target_d_b_instance_identifier =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "target_d_b_instance_identifier")));
        restore_time =
          (Util.option_map (Json.lookup j "restore_time") DateTime.of_json);
        use_latest_restorable_time =
          (Util.option_map (Json.lookup j "use_latest_restorable_time")
             Boolean.of_json);
        d_b_instance_class =
          (Util.option_map (Json.lookup j "d_b_instance_class")
             String.of_json);
        port = (Util.option_map (Json.lookup j "port") Integer.of_json);
        availability_zone =
          (Util.option_map (Json.lookup j "availability_zone") String.of_json);
        d_b_subnet_group_name =
          (Util.option_map (Json.lookup j "d_b_subnet_group_name")
             String.of_json);
        multi_a_z =
          (Util.option_map (Json.lookup j "multi_a_z") Boolean.of_json);
        publicly_accessible =
          (Util.option_map (Json.lookup j "publicly_accessible")
             Boolean.of_json);
        auto_minor_version_upgrade =
          (Util.option_map (Json.lookup j "auto_minor_version_upgrade")
             Boolean.of_json);
        license_model =
          (Util.option_map (Json.lookup j "license_model") String.of_json);
        d_b_name =
          (Util.option_map (Json.lookup j "d_b_name") String.of_json);
        engine = (Util.option_map (Json.lookup j "engine") String.of_json);
        iops = (Util.option_map (Json.lookup j "iops") Integer.of_json);
        option_group_name =
          (Util.option_map (Json.lookup j "option_group_name") String.of_json);
        copy_tags_to_snapshot =
          (Util.option_map (Json.lookup j "copy_tags_to_snapshot")
             Boolean.of_json);
        tags = (TagList.of_json (Util.of_option_exn (Json.lookup j "tags")));
        storage_type =
          (Util.option_map (Json.lookup j "storage_type") String.of_json);
        tde_credential_arn =
          (Util.option_map (Json.lookup j "tde_credential_arn")
             String.of_json);
        tde_credential_password =
          (Util.option_map (Json.lookup j "tde_credential_password")
             String.of_json);
        vpc_security_group_ids =
          (VpcSecurityGroupIdList.of_json
             (Util.of_option_exn (Json.lookup j "vpc_security_group_ids")));
        d_b_security_groups =
          (DBSecurityGroupNameList.of_json
             (Util.of_option_exn (Json.lookup j "d_b_security_groups")));
        domain = (Util.option_map (Json.lookup j "domain") String.of_json)
      }
  end
module ModifyDBParameterGroupMessage =
  struct
    type t =
      {
      d_b_parameter_group_name: String.t ;
      parameters: ParametersList.t }
    let make ~d_b_parameter_group_name  ~parameters  () =
      { d_b_parameter_group_name; parameters }
    let parse xml =
      Some
        {
          d_b_parameter_group_name =
            (Xml.required "DBParameterGroupName"
               (Util.option_bind (Xml.member "DBParameterGroupName" xml)
                  String.parse));
          parameters =
            (Xml.required "Parameters"
               (Util.option_bind (Xml.member "Parameters" xml)
                  ParametersList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Parameters.member",
                   (ParametersList.to_query v.parameters)));
           Some
             (Query.Pair
                ("DBParameterGroupName",
                  (String.to_query v.d_b_parameter_group_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("parameters", (ParametersList.to_json v.parameters));
           Some
             ("d_b_parameter_group_name",
               (String.to_json v.d_b_parameter_group_name))])
    let of_json j =
      {
        d_b_parameter_group_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "d_b_parameter_group_name")));
        parameters =
          (ParametersList.of_json
             (Util.of_option_exn (Json.lookup j "parameters")))
      }
  end
module SubscriptionCategoryNotFoundFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DBSubnetGroupMessage =
  struct
    type t = {
      marker: String.t option ;
      d_b_subnet_groups: DBSubnetGroups.t }
    let make ?marker  ?(d_b_subnet_groups= [])  () =
      { marker; d_b_subnet_groups }
    let parse xml =
      Some
        {
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          d_b_subnet_groups =
            (Util.of_option []
               (Util.option_bind (Xml.member "DBSubnetGroups" xml)
                  DBSubnetGroups.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("DBSubnetGroups.member",
                   (DBSubnetGroups.to_query v.d_b_subnet_groups)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("d_b_subnet_groups",
                (DBSubnetGroups.to_json v.d_b_subnet_groups));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)))])
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        d_b_subnet_groups =
          (DBSubnetGroups.of_json
             (Util.of_option_exn (Json.lookup j "d_b_subnet_groups")))
      }
  end
module ResourceNotFoundFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CreateDBClusterSnapshotMessage =
  struct
    type t =
      {
      d_b_cluster_snapshot_identifier: String.t ;
      d_b_cluster_identifier: String.t ;
      tags: TagList.t }
    let make ~d_b_cluster_snapshot_identifier  ~d_b_cluster_identifier 
      ?(tags= [])  () =
      { d_b_cluster_snapshot_identifier; d_b_cluster_identifier; tags }
    let parse xml =
      Some
        {
          d_b_cluster_snapshot_identifier =
            (Xml.required "DBClusterSnapshotIdentifier"
               (Util.option_bind
                  (Xml.member "DBClusterSnapshotIdentifier" xml) String.parse));
          d_b_cluster_identifier =
            (Xml.required "DBClusterIdentifier"
               (Util.option_bind (Xml.member "DBClusterIdentifier" xml)
                  String.parse));
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) TagList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Tags.member", (TagList.to_query v.tags)));
           Some
             (Query.Pair
                ("DBClusterIdentifier",
                  (String.to_query v.d_b_cluster_identifier)));
           Some
             (Query.Pair
                ("DBClusterSnapshotIdentifier",
                  (String.to_query v.d_b_cluster_snapshot_identifier)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tags", (TagList.to_json v.tags));
           Some
             ("d_b_cluster_identifier",
               (String.to_json v.d_b_cluster_identifier));
           Some
             ("d_b_cluster_snapshot_identifier",
               (String.to_json v.d_b_cluster_snapshot_identifier))])
    let of_json j =
      {
        d_b_cluster_snapshot_identifier =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "d_b_cluster_snapshot_identifier")));
        d_b_cluster_identifier =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "d_b_cluster_identifier")));
        tags = (TagList.of_json (Util.of_option_exn (Json.lookup j "tags")))
      }
  end
module ReservedDBInstanceQuotaExceededFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CreateDBParameterGroupResult =
  struct
    type t = {
      d_b_parameter_group: DBParameterGroup.t option }
    let make ?d_b_parameter_group  () = { d_b_parameter_group }
    let parse xml =
      Some
        {
          d_b_parameter_group =
            (Util.option_bind (Xml.member "DBParameterGroup" xml)
               DBParameterGroup.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.d_b_parameter_group
              (fun f ->
                 Query.Pair
                   ("DBParameterGroup", (DBParameterGroup.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.d_b_parameter_group
              (fun f -> ("d_b_parameter_group", (DBParameterGroup.to_json f)))])
    let of_json j =
      {
        d_b_parameter_group =
          (Util.option_map (Json.lookup j "d_b_parameter_group")
             DBParameterGroup.of_json)
      }
  end
module InvalidDBSnapshotStateFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module InvalidDBInstanceStateFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DownloadDBLogFilePortionMessage =
  struct
    type t =
      {
      d_b_instance_identifier: String.t ;
      log_file_name: String.t ;
      marker: String.t option ;
      number_of_lines: Integer.t option }
    let make ~d_b_instance_identifier  ~log_file_name  ?marker 
      ?number_of_lines  () =
      { d_b_instance_identifier; log_file_name; marker; number_of_lines }
    let parse xml =
      Some
        {
          d_b_instance_identifier =
            (Xml.required "DBInstanceIdentifier"
               (Util.option_bind (Xml.member "DBInstanceIdentifier" xml)
                  String.parse));
          log_file_name =
            (Xml.required "LogFileName"
               (Util.option_bind (Xml.member "LogFileName" xml) String.parse));
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          number_of_lines =
            (Util.option_bind (Xml.member "NumberOfLines" xml) Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.number_of_lines
              (fun f -> Query.Pair ("NumberOfLines", (Integer.to_query f)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Some
             (Query.Pair ("LogFileName", (String.to_query v.log_file_name)));
           Some
             (Query.Pair
                ("DBInstanceIdentifier",
                  (String.to_query v.d_b_instance_identifier)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.number_of_lines
              (fun f -> ("number_of_lines", (Integer.to_json f)));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)));
           Some ("log_file_name", (String.to_json v.log_file_name));
           Some
             ("d_b_instance_identifier",
               (String.to_json v.d_b_instance_identifier))])
    let of_json j =
      {
        d_b_instance_identifier =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "d_b_instance_identifier")));
        log_file_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "log_file_name")));
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        number_of_lines =
          (Util.option_map (Json.lookup j "number_of_lines") Integer.of_json)
      }
  end
module AuthorizeDBSecurityGroupIngressMessage =
  struct
    type t =
      {
      d_b_security_group_name: String.t ;
      c_i_d_r_i_p: String.t option ;
      e_c2_security_group_name: String.t option ;
      e_c2_security_group_id: String.t option ;
      e_c2_security_group_owner_id: String.t option }
    let make ~d_b_security_group_name  ?c_i_d_r_i_p 
      ?e_c2_security_group_name  ?e_c2_security_group_id 
      ?e_c2_security_group_owner_id  () =
      {
        d_b_security_group_name;
        c_i_d_r_i_p;
        e_c2_security_group_name;
        e_c2_security_group_id;
        e_c2_security_group_owner_id
      }
    let parse xml =
      Some
        {
          d_b_security_group_name =
            (Xml.required "DBSecurityGroupName"
               (Util.option_bind (Xml.member "DBSecurityGroupName" xml)
                  String.parse));
          c_i_d_r_i_p =
            (Util.option_bind (Xml.member "CIDRIP" xml) String.parse);
          e_c2_security_group_name =
            (Util.option_bind (Xml.member "EC2SecurityGroupName" xml)
               String.parse);
          e_c2_security_group_id =
            (Util.option_bind (Xml.member "EC2SecurityGroupId" xml)
               String.parse);
          e_c2_security_group_owner_id =
            (Util.option_bind (Xml.member "EC2SecurityGroupOwnerId" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.e_c2_security_group_owner_id
              (fun f ->
                 Query.Pair ("EC2SecurityGroupOwnerId", (String.to_query f)));
           Util.option_map v.e_c2_security_group_id
             (fun f -> Query.Pair ("EC2SecurityGroupId", (String.to_query f)));
           Util.option_map v.e_c2_security_group_name
             (fun f ->
                Query.Pair ("EC2SecurityGroupName", (String.to_query f)));
           Util.option_map v.c_i_d_r_i_p
             (fun f -> Query.Pair ("CIDRIP", (String.to_query f)));
           Some
             (Query.Pair
                ("DBSecurityGroupName",
                  (String.to_query v.d_b_security_group_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.e_c2_security_group_owner_id
              (fun f -> ("e_c2_security_group_owner_id", (String.to_json f)));
           Util.option_map v.e_c2_security_group_id
             (fun f -> ("e_c2_security_group_id", (String.to_json f)));
           Util.option_map v.e_c2_security_group_name
             (fun f -> ("e_c2_security_group_name", (String.to_json f)));
           Util.option_map v.c_i_d_r_i_p
             (fun f -> ("c_i_d_r_i_p", (String.to_json f)));
           Some
             ("d_b_security_group_name",
               (String.to_json v.d_b_security_group_name))])
    let of_json j =
      {
        d_b_security_group_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "d_b_security_group_name")));
        c_i_d_r_i_p =
          (Util.option_map (Json.lookup j "c_i_d_r_i_p") String.of_json);
        e_c2_security_group_name =
          (Util.option_map (Json.lookup j "e_c2_security_group_name")
             String.of_json);
        e_c2_security_group_id =
          (Util.option_map (Json.lookup j "e_c2_security_group_id")
             String.of_json);
        e_c2_security_group_owner_id =
          (Util.option_map (Json.lookup j "e_c2_security_group_owner_id")
             String.of_json)
      }
  end
module DBEngineVersionMessage =
  struct
    type t =
      {
      marker: String.t option ;
      d_b_engine_versions: DBEngineVersionList.t }
    let make ?marker  ?(d_b_engine_versions= [])  () =
      { marker; d_b_engine_versions }
    let parse xml =
      Some
        {
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          d_b_engine_versions =
            (Util.of_option []
               (Util.option_bind (Xml.member "DBEngineVersions" xml)
                  DBEngineVersionList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("DBEngineVersions.member",
                   (DBEngineVersionList.to_query v.d_b_engine_versions)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("d_b_engine_versions",
                (DBEngineVersionList.to_json v.d_b_engine_versions));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)))])
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        d_b_engine_versions =
          (DBEngineVersionList.of_json
             (Util.of_option_exn (Json.lookup j "d_b_engine_versions")))
      }
  end
module CreateDBClusterParameterGroupResult =
  struct
    type t = {
      d_b_cluster_parameter_group: DBClusterParameterGroup.t option }
    let make ?d_b_cluster_parameter_group  () =
      { d_b_cluster_parameter_group }
    let parse xml =
      Some
        {
          d_b_cluster_parameter_group =
            (Util.option_bind (Xml.member "DBClusterParameterGroup" xml)
               DBClusterParameterGroup.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.d_b_cluster_parameter_group
              (fun f ->
                 Query.Pair
                   ("DBClusterParameterGroup",
                     (DBClusterParameterGroup.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.d_b_cluster_parameter_group
              (fun f ->
                 ("d_b_cluster_parameter_group",
                   (DBClusterParameterGroup.to_json f)))])
    let of_json j =
      {
        d_b_cluster_parameter_group =
          (Util.option_map (Json.lookup j "d_b_cluster_parameter_group")
             DBClusterParameterGroup.of_json)
      }
  end
module DBClusterParameterGroupNotFoundFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DBSnapshotMessage =
  struct
    type t = {
      marker: String.t option ;
      d_b_snapshots: DBSnapshotList.t }
    let make ?marker  ?(d_b_snapshots= [])  () = { marker; d_b_snapshots }
    let parse xml =
      Some
        {
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          d_b_snapshots =
            (Util.of_option []
               (Util.option_bind (Xml.member "DBSnapshots" xml)
                  DBSnapshotList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("DBSnapshots.member",
                   (DBSnapshotList.to_query v.d_b_snapshots)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("d_b_snapshots", (DBSnapshotList.to_json v.d_b_snapshots));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)))])
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        d_b_snapshots =
          (DBSnapshotList.of_json
             (Util.of_option_exn (Json.lookup j "d_b_snapshots")))
      }
  end
module DownloadDBLogFilePortionDetails =
  struct
    type t =
      {
      log_file_data: String.t option ;
      marker: String.t option ;
      additional_data_pending: Boolean.t option }
    let make ?log_file_data  ?marker  ?additional_data_pending  () =
      { log_file_data; marker; additional_data_pending }
    let parse xml =
      Some
        {
          log_file_data =
            (Util.option_bind (Xml.member "LogFileData" xml) String.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          additional_data_pending =
            (Util.option_bind (Xml.member "AdditionalDataPending" xml)
               Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.additional_data_pending
              (fun f ->
                 Query.Pair ("AdditionalDataPending", (Boolean.to_query f)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.log_file_data
             (fun f -> Query.Pair ("LogFileData", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.additional_data_pending
              (fun f -> ("additional_data_pending", (Boolean.to_json f)));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.log_file_data
             (fun f -> ("log_file_data", (String.to_json f)))])
    let of_json j =
      {
        log_file_data =
          (Util.option_map (Json.lookup j "log_file_data") String.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        additional_data_pending =
          (Util.option_map (Json.lookup j "additional_data_pending")
             Boolean.of_json)
      }
  end
module DBUpgradeDependencyFailureFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DBSecurityGroupNotSupportedFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module RestoreDBClusterToPointInTimeMessage =
  struct
    type t =
      {
      d_b_cluster_identifier: String.t ;
      source_d_b_cluster_identifier: String.t ;
      restore_to_time: DateTime.t option ;
      use_latest_restorable_time: Boolean.t option ;
      port: Integer.t option ;
      d_b_subnet_group_name: String.t option ;
      option_group_name: String.t option ;
      vpc_security_group_ids: VpcSecurityGroupIdList.t ;
      tags: TagList.t }
    let make ~d_b_cluster_identifier  ~source_d_b_cluster_identifier 
      ?restore_to_time  ?use_latest_restorable_time  ?port 
      ?d_b_subnet_group_name  ?option_group_name  ?(vpc_security_group_ids=
      [])  ?(tags= [])  () =
      {
        d_b_cluster_identifier;
        source_d_b_cluster_identifier;
        restore_to_time;
        use_latest_restorable_time;
        port;
        d_b_subnet_group_name;
        option_group_name;
        vpc_security_group_ids;
        tags
      }
    let parse xml =
      Some
        {
          d_b_cluster_identifier =
            (Xml.required "DBClusterIdentifier"
               (Util.option_bind (Xml.member "DBClusterIdentifier" xml)
                  String.parse));
          source_d_b_cluster_identifier =
            (Xml.required "SourceDBClusterIdentifier"
               (Util.option_bind (Xml.member "SourceDBClusterIdentifier" xml)
                  String.parse));
          restore_to_time =
            (Util.option_bind (Xml.member "RestoreToTime" xml) DateTime.parse);
          use_latest_restorable_time =
            (Util.option_bind (Xml.member "UseLatestRestorableTime" xml)
               Boolean.parse);
          port = (Util.option_bind (Xml.member "Port" xml) Integer.parse);
          d_b_subnet_group_name =
            (Util.option_bind (Xml.member "DBSubnetGroupName" xml)
               String.parse);
          option_group_name =
            (Util.option_bind (Xml.member "OptionGroupName" xml) String.parse);
          vpc_security_group_ids =
            (Util.of_option []
               (Util.option_bind (Xml.member "VpcSecurityGroupIds" xml)
                  VpcSecurityGroupIdList.parse));
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) TagList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Tags.member", (TagList.to_query v.tags)));
           Some
             (Query.Pair
                ("VpcSecurityGroupIds.member",
                  (VpcSecurityGroupIdList.to_query v.vpc_security_group_ids)));
           Util.option_map v.option_group_name
             (fun f -> Query.Pair ("OptionGroupName", (String.to_query f)));
           Util.option_map v.d_b_subnet_group_name
             (fun f -> Query.Pair ("DBSubnetGroupName", (String.to_query f)));
           Util.option_map v.port
             (fun f -> Query.Pair ("Port", (Integer.to_query f)));
           Util.option_map v.use_latest_restorable_time
             (fun f ->
                Query.Pair ("UseLatestRestorableTime", (Boolean.to_query f)));
           Util.option_map v.restore_to_time
             (fun f -> Query.Pair ("RestoreToTime", (DateTime.to_query f)));
           Some
             (Query.Pair
                ("SourceDBClusterIdentifier",
                  (String.to_query v.source_d_b_cluster_identifier)));
           Some
             (Query.Pair
                ("DBClusterIdentifier",
                  (String.to_query v.d_b_cluster_identifier)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tags", (TagList.to_json v.tags));
           Some
             ("vpc_security_group_ids",
               (VpcSecurityGroupIdList.to_json v.vpc_security_group_ids));
           Util.option_map v.option_group_name
             (fun f -> ("option_group_name", (String.to_json f)));
           Util.option_map v.d_b_subnet_group_name
             (fun f -> ("d_b_subnet_group_name", (String.to_json f)));
           Util.option_map v.port (fun f -> ("port", (Integer.to_json f)));
           Util.option_map v.use_latest_restorable_time
             (fun f -> ("use_latest_restorable_time", (Boolean.to_json f)));
           Util.option_map v.restore_to_time
             (fun f -> ("restore_to_time", (DateTime.to_json f)));
           Some
             ("source_d_b_cluster_identifier",
               (String.to_json v.source_d_b_cluster_identifier));
           Some
             ("d_b_cluster_identifier",
               (String.to_json v.d_b_cluster_identifier))])
    let of_json j =
      {
        d_b_cluster_identifier =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "d_b_cluster_identifier")));
        source_d_b_cluster_identifier =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "source_d_b_cluster_identifier")));
        restore_to_time =
          (Util.option_map (Json.lookup j "restore_to_time") DateTime.of_json);
        use_latest_restorable_time =
          (Util.option_map (Json.lookup j "use_latest_restorable_time")
             Boolean.of_json);
        port = (Util.option_map (Json.lookup j "port") Integer.of_json);
        d_b_subnet_group_name =
          (Util.option_map (Json.lookup j "d_b_subnet_group_name")
             String.of_json);
        option_group_name =
          (Util.option_map (Json.lookup j "option_group_name") String.of_json);
        vpc_security_group_ids =
          (VpcSecurityGroupIdList.of_json
             (Util.of_option_exn (Json.lookup j "vpc_security_group_ids")));
        tags = (TagList.of_json (Util.of_option_exn (Json.lookup j "tags")))
      }
  end
module CreateEventSubscriptionResult =
  struct
    type t = {
      event_subscription: EventSubscription.t option }
    let make ?event_subscription  () = { event_subscription }
    let parse xml =
      Some
        {
          event_subscription =
            (Util.option_bind (Xml.member "EventSubscription" xml)
               EventSubscription.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.event_subscription
              (fun f ->
                 Query.Pair
                   ("EventSubscription", (EventSubscription.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.event_subscription
              (fun f -> ("event_subscription", (EventSubscription.to_json f)))])
    let of_json j =
      {
        event_subscription =
          (Util.option_map (Json.lookup j "event_subscription")
             EventSubscription.of_json)
      }
  end
module DBLogFileNotFoundFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module EventCategoriesMessage =
  struct
    type t = {
      event_categories_map_list: EventCategoriesMapList.t }
    let make ?(event_categories_map_list= [])  () =
      { event_categories_map_list }
    let parse xml =
      Some
        {
          event_categories_map_list =
            (Util.of_option []
               (Util.option_bind (Xml.member "EventCategoriesMapList" xml)
                  EventCategoriesMapList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("EventCategoriesMapList.member",
                   (EventCategoriesMapList.to_query
                      v.event_categories_map_list)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("event_categories_map_list",
                (EventCategoriesMapList.to_json v.event_categories_map_list))])
    let of_json j =
      {
        event_categories_map_list =
          (EventCategoriesMapList.of_json
             (Util.of_option_exn (Json.lookup j "event_categories_map_list")))
      }
  end
module DescribeDBSnapshotsMessage =
  struct
    type t =
      {
      d_b_instance_identifier: String.t option ;
      d_b_snapshot_identifier: String.t option ;
      snapshot_type: String.t option ;
      filters: FilterList.t ;
      max_records: Integer.t option ;
      marker: String.t option }
    let make ?d_b_instance_identifier  ?d_b_snapshot_identifier 
      ?snapshot_type  ?(filters= [])  ?max_records  ?marker  () =
      {
        d_b_instance_identifier;
        d_b_snapshot_identifier;
        snapshot_type;
        filters;
        max_records;
        marker
      }
    let parse xml =
      Some
        {
          d_b_instance_identifier =
            (Util.option_bind (Xml.member "DBInstanceIdentifier" xml)
               String.parse);
          d_b_snapshot_identifier =
            (Util.option_bind (Xml.member "DBSnapshotIdentifier" xml)
               String.parse);
          snapshot_type =
            (Util.option_bind (Xml.member "SnapshotType" xml) String.parse);
          filters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Filters" xml) FilterList.parse));
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Some
             (Query.Pair ("Filters.member", (FilterList.to_query v.filters)));
           Util.option_map v.snapshot_type
             (fun f -> Query.Pair ("SnapshotType", (String.to_query f)));
           Util.option_map v.d_b_snapshot_identifier
             (fun f ->
                Query.Pair ("DBSnapshotIdentifier", (String.to_query f)));
           Util.option_map v.d_b_instance_identifier
             (fun f ->
                Query.Pair ("DBInstanceIdentifier", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Some ("filters", (FilterList.to_json v.filters));
           Util.option_map v.snapshot_type
             (fun f -> ("snapshot_type", (String.to_json f)));
           Util.option_map v.d_b_snapshot_identifier
             (fun f -> ("d_b_snapshot_identifier", (String.to_json f)));
           Util.option_map v.d_b_instance_identifier
             (fun f -> ("d_b_instance_identifier", (String.to_json f)))])
    let of_json j =
      {
        d_b_instance_identifier =
          (Util.option_map (Json.lookup j "d_b_instance_identifier")
             String.of_json);
        d_b_snapshot_identifier =
          (Util.option_map (Json.lookup j "d_b_snapshot_identifier")
             String.of_json);
        snapshot_type =
          (Util.option_map (Json.lookup j "snapshot_type") String.of_json);
        filters =
          (FilterList.of_json (Util.of_option_exn (Json.lookup j "filters")));
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module CopyDBClusterSnapshotResult =
  struct
    type t = {
      d_b_cluster_snapshot: DBClusterSnapshot.t option }
    let make ?d_b_cluster_snapshot  () = { d_b_cluster_snapshot }
    let parse xml =
      Some
        {
          d_b_cluster_snapshot =
            (Util.option_bind (Xml.member "DBClusterSnapshot" xml)
               DBClusterSnapshot.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.d_b_cluster_snapshot
              (fun f ->
                 Query.Pair
                   ("DBClusterSnapshot", (DBClusterSnapshot.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.d_b_cluster_snapshot
              (fun f ->
                 ("d_b_cluster_snapshot", (DBClusterSnapshot.to_json f)))])
    let of_json j =
      {
        d_b_cluster_snapshot =
          (Util.option_map (Json.lookup j "d_b_cluster_snapshot")
             DBClusterSnapshot.of_json)
      }
  end
module CreateEventSubscriptionMessage =
  struct
    type t =
      {
      subscription_name: String.t ;
      sns_topic_arn: String.t ;
      source_type: String.t option ;
      event_categories: EventCategoriesList.t ;
      source_ids: SourceIdsList.t ;
      enabled: Boolean.t option ;
      tags: TagList.t }
    let make ~subscription_name  ~sns_topic_arn  ?source_type 
      ?(event_categories= [])  ?(source_ids= [])  ?enabled  ?(tags= [])  () =
      {
        subscription_name;
        sns_topic_arn;
        source_type;
        event_categories;
        source_ids;
        enabled;
        tags
      }
    let parse xml =
      Some
        {
          subscription_name =
            (Xml.required "SubscriptionName"
               (Util.option_bind (Xml.member "SubscriptionName" xml)
                  String.parse));
          sns_topic_arn =
            (Xml.required "SnsTopicArn"
               (Util.option_bind (Xml.member "SnsTopicArn" xml) String.parse));
          source_type =
            (Util.option_bind (Xml.member "SourceType" xml) String.parse);
          event_categories =
            (Util.of_option []
               (Util.option_bind (Xml.member "EventCategories" xml)
                  EventCategoriesList.parse));
          source_ids =
            (Util.of_option []
               (Util.option_bind (Xml.member "SourceIds" xml)
                  SourceIdsList.parse));
          enabled =
            (Util.option_bind (Xml.member "Enabled" xml) Boolean.parse);
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) TagList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Tags.member", (TagList.to_query v.tags)));
           Util.option_map v.enabled
             (fun f -> Query.Pair ("Enabled", (Boolean.to_query f)));
           Some
             (Query.Pair
                ("SourceIds.member", (SourceIdsList.to_query v.source_ids)));
           Some
             (Query.Pair
                ("EventCategories.member",
                  (EventCategoriesList.to_query v.event_categories)));
           Util.option_map v.source_type
             (fun f -> Query.Pair ("SourceType", (String.to_query f)));
           Some
             (Query.Pair ("SnsTopicArn", (String.to_query v.sns_topic_arn)));
           Some
             (Query.Pair
                ("SubscriptionName", (String.to_query v.subscription_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tags", (TagList.to_json v.tags));
           Util.option_map v.enabled
             (fun f -> ("enabled", (Boolean.to_json f)));
           Some ("source_ids", (SourceIdsList.to_json v.source_ids));
           Some
             ("event_categories",
               (EventCategoriesList.to_json v.event_categories));
           Util.option_map v.source_type
             (fun f -> ("source_type", (String.to_json f)));
           Some ("sns_topic_arn", (String.to_json v.sns_topic_arn));
           Some ("subscription_name", (String.to_json v.subscription_name))])
    let of_json j =
      {
        subscription_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "subscription_name")));
        sns_topic_arn =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "sns_topic_arn")));
        source_type =
          (Util.option_map (Json.lookup j "source_type") String.of_json);
        event_categories =
          (EventCategoriesList.of_json
             (Util.of_option_exn (Json.lookup j "event_categories")));
        source_ids =
          (SourceIdsList.of_json
             (Util.of_option_exn (Json.lookup j "source_ids")));
        enabled = (Util.option_map (Json.lookup j "enabled") Boolean.of_json);
        tags = (TagList.of_json (Util.of_option_exn (Json.lookup j "tags")))
      }
  end
module CopyDBClusterSnapshotMessage =
  struct
    type t =
      {
      source_d_b_cluster_snapshot_identifier: String.t ;
      target_d_b_cluster_snapshot_identifier: String.t ;
      tags: TagList.t }
    let make ~source_d_b_cluster_snapshot_identifier 
      ~target_d_b_cluster_snapshot_identifier  ?(tags= [])  () =
      {
        source_d_b_cluster_snapshot_identifier;
        target_d_b_cluster_snapshot_identifier;
        tags
      }
    let parse xml =
      Some
        {
          source_d_b_cluster_snapshot_identifier =
            (Xml.required "SourceDBClusterSnapshotIdentifier"
               (Util.option_bind
                  (Xml.member "SourceDBClusterSnapshotIdentifier" xml)
                  String.parse));
          target_d_b_cluster_snapshot_identifier =
            (Xml.required "TargetDBClusterSnapshotIdentifier"
               (Util.option_bind
                  (Xml.member "TargetDBClusterSnapshotIdentifier" xml)
                  String.parse));
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) TagList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Tags.member", (TagList.to_query v.tags)));
           Some
             (Query.Pair
                ("TargetDBClusterSnapshotIdentifier",
                  (String.to_query v.target_d_b_cluster_snapshot_identifier)));
           Some
             (Query.Pair
                ("SourceDBClusterSnapshotIdentifier",
                  (String.to_query v.source_d_b_cluster_snapshot_identifier)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tags", (TagList.to_json v.tags));
           Some
             ("target_d_b_cluster_snapshot_identifier",
               (String.to_json v.target_d_b_cluster_snapshot_identifier));
           Some
             ("source_d_b_cluster_snapshot_identifier",
               (String.to_json v.source_d_b_cluster_snapshot_identifier))])
    let of_json j =
      {
        source_d_b_cluster_snapshot_identifier =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "source_d_b_cluster_snapshot_identifier")));
        target_d_b_cluster_snapshot_identifier =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "target_d_b_cluster_snapshot_identifier")));
        tags = (TagList.of_json (Util.of_option_exn (Json.lookup j "tags")))
      }
  end
module DescribeDBClustersMessage =
  struct
    type t =
      {
      d_b_cluster_identifier: String.t option ;
      filters: FilterList.t ;
      max_records: Integer.t option ;
      marker: String.t option }
    let make ?d_b_cluster_identifier  ?(filters= [])  ?max_records  ?marker 
      () = { d_b_cluster_identifier; filters; max_records; marker }
    let parse xml =
      Some
        {
          d_b_cluster_identifier =
            (Util.option_bind (Xml.member "DBClusterIdentifier" xml)
               String.parse);
          filters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Filters" xml) FilterList.parse));
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Some
             (Query.Pair ("Filters.member", (FilterList.to_query v.filters)));
           Util.option_map v.d_b_cluster_identifier
             (fun f ->
                Query.Pair ("DBClusterIdentifier", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Some ("filters", (FilterList.to_json v.filters));
           Util.option_map v.d_b_cluster_identifier
             (fun f -> ("d_b_cluster_identifier", (String.to_json f)))])
    let of_json j =
      {
        d_b_cluster_identifier =
          (Util.option_map (Json.lookup j "d_b_cluster_identifier")
             String.of_json);
        filters =
          (FilterList.of_json (Util.of_option_exn (Json.lookup j "filters")));
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module DBSecurityGroupAlreadyExistsFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DBSubnetGroupQuotaExceededFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DeleteDBParameterGroupMessage =
  struct
    type t = {
      d_b_parameter_group_name: String.t }
    let make ~d_b_parameter_group_name  () = { d_b_parameter_group_name }
    let parse xml =
      Some
        {
          d_b_parameter_group_name =
            (Xml.required "DBParameterGroupName"
               (Util.option_bind (Xml.member "DBParameterGroupName" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("DBParameterGroupName",
                   (String.to_query v.d_b_parameter_group_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("d_b_parameter_group_name",
                (String.to_json v.d_b_parameter_group_name))])
    let of_json j =
      {
        d_b_parameter_group_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "d_b_parameter_group_name")))
      }
  end
module CreateDBClusterMessage =
  struct
    type t =
      {
      availability_zones: AvailabilityZones.t ;
      backup_retention_period: Integer.t option ;
      character_set_name: String.t option ;
      database_name: String.t option ;
      d_b_cluster_identifier: String.t option ;
      d_b_cluster_parameter_group_name: String.t option ;
      vpc_security_group_ids: VpcSecurityGroupIdList.t ;
      d_b_subnet_group_name: String.t option ;
      engine: String.t option ;
      engine_version: String.t option ;
      port: Integer.t option ;
      master_username: String.t option ;
      master_user_password: String.t option ;
      option_group_name: String.t option ;
      preferred_backup_window: String.t option ;
      preferred_maintenance_window: String.t option ;
      tags: TagList.t }
    let make ?(availability_zones= [])  ?backup_retention_period 
      ?character_set_name  ?database_name  ?d_b_cluster_identifier 
      ?d_b_cluster_parameter_group_name  ?(vpc_security_group_ids= []) 
      ?d_b_subnet_group_name  ?engine  ?engine_version  ?port 
      ?master_username  ?master_user_password  ?option_group_name 
      ?preferred_backup_window  ?preferred_maintenance_window  ?(tags= []) 
      () =
      {
        availability_zones;
        backup_retention_period;
        character_set_name;
        database_name;
        d_b_cluster_identifier;
        d_b_cluster_parameter_group_name;
        vpc_security_group_ids;
        d_b_subnet_group_name;
        engine;
        engine_version;
        port;
        master_username;
        master_user_password;
        option_group_name;
        preferred_backup_window;
        preferred_maintenance_window;
        tags
      }
    let parse xml =
      Some
        {
          availability_zones =
            (Util.of_option []
               (Util.option_bind (Xml.member "AvailabilityZones" xml)
                  AvailabilityZones.parse));
          backup_retention_period =
            (Util.option_bind (Xml.member "BackupRetentionPeriod" xml)
               Integer.parse);
          character_set_name =
            (Util.option_bind (Xml.member "CharacterSetName" xml)
               String.parse);
          database_name =
            (Util.option_bind (Xml.member "DatabaseName" xml) String.parse);
          d_b_cluster_identifier =
            (Util.option_bind (Xml.member "DBClusterIdentifier" xml)
               String.parse);
          d_b_cluster_parameter_group_name =
            (Util.option_bind (Xml.member "DBClusterParameterGroupName" xml)
               String.parse);
          vpc_security_group_ids =
            (Util.of_option []
               (Util.option_bind (Xml.member "VpcSecurityGroupIds" xml)
                  VpcSecurityGroupIdList.parse));
          d_b_subnet_group_name =
            (Util.option_bind (Xml.member "DBSubnetGroupName" xml)
               String.parse);
          engine = (Util.option_bind (Xml.member "Engine" xml) String.parse);
          engine_version =
            (Util.option_bind (Xml.member "EngineVersion" xml) String.parse);
          port = (Util.option_bind (Xml.member "Port" xml) Integer.parse);
          master_username =
            (Util.option_bind (Xml.member "MasterUsername" xml) String.parse);
          master_user_password =
            (Util.option_bind (Xml.member "MasterUserPassword" xml)
               String.parse);
          option_group_name =
            (Util.option_bind (Xml.member "OptionGroupName" xml) String.parse);
          preferred_backup_window =
            (Util.option_bind (Xml.member "PreferredBackupWindow" xml)
               String.parse);
          preferred_maintenance_window =
            (Util.option_bind (Xml.member "PreferredMaintenanceWindow" xml)
               String.parse);
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) TagList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Tags.member", (TagList.to_query v.tags)));
           Util.option_map v.preferred_maintenance_window
             (fun f ->
                Query.Pair
                  ("PreferredMaintenanceWindow", (String.to_query f)));
           Util.option_map v.preferred_backup_window
             (fun f ->
                Query.Pair ("PreferredBackupWindow", (String.to_query f)));
           Util.option_map v.option_group_name
             (fun f -> Query.Pair ("OptionGroupName", (String.to_query f)));
           Util.option_map v.master_user_password
             (fun f -> Query.Pair ("MasterUserPassword", (String.to_query f)));
           Util.option_map v.master_username
             (fun f -> Query.Pair ("MasterUsername", (String.to_query f)));
           Util.option_map v.port
             (fun f -> Query.Pair ("Port", (Integer.to_query f)));
           Util.option_map v.engine_version
             (fun f -> Query.Pair ("EngineVersion", (String.to_query f)));
           Util.option_map v.engine
             (fun f -> Query.Pair ("Engine", (String.to_query f)));
           Util.option_map v.d_b_subnet_group_name
             (fun f -> Query.Pair ("DBSubnetGroupName", (String.to_query f)));
           Some
             (Query.Pair
                ("VpcSecurityGroupIds.member",
                  (VpcSecurityGroupIdList.to_query v.vpc_security_group_ids)));
           Util.option_map v.d_b_cluster_parameter_group_name
             (fun f ->
                Query.Pair
                  ("DBClusterParameterGroupName", (String.to_query f)));
           Util.option_map v.d_b_cluster_identifier
             (fun f ->
                Query.Pair ("DBClusterIdentifier", (String.to_query f)));
           Util.option_map v.database_name
             (fun f -> Query.Pair ("DatabaseName", (String.to_query f)));
           Util.option_map v.character_set_name
             (fun f -> Query.Pair ("CharacterSetName", (String.to_query f)));
           Util.option_map v.backup_retention_period
             (fun f ->
                Query.Pair ("BackupRetentionPeriod", (Integer.to_query f)));
           Some
             (Query.Pair
                ("AvailabilityZones.member",
                  (AvailabilityZones.to_query v.availability_zones)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tags", (TagList.to_json v.tags));
           Util.option_map v.preferred_maintenance_window
             (fun f -> ("preferred_maintenance_window", (String.to_json f)));
           Util.option_map v.preferred_backup_window
             (fun f -> ("preferred_backup_window", (String.to_json f)));
           Util.option_map v.option_group_name
             (fun f -> ("option_group_name", (String.to_json f)));
           Util.option_map v.master_user_password
             (fun f -> ("master_user_password", (String.to_json f)));
           Util.option_map v.master_username
             (fun f -> ("master_username", (String.to_json f)));
           Util.option_map v.port (fun f -> ("port", (Integer.to_json f)));
           Util.option_map v.engine_version
             (fun f -> ("engine_version", (String.to_json f)));
           Util.option_map v.engine (fun f -> ("engine", (String.to_json f)));
           Util.option_map v.d_b_subnet_group_name
             (fun f -> ("d_b_subnet_group_name", (String.to_json f)));
           Some
             ("vpc_security_group_ids",
               (VpcSecurityGroupIdList.to_json v.vpc_security_group_ids));
           Util.option_map v.d_b_cluster_parameter_group_name
             (fun f ->
                ("d_b_cluster_parameter_group_name", (String.to_json f)));
           Util.option_map v.d_b_cluster_identifier
             (fun f -> ("d_b_cluster_identifier", (String.to_json f)));
           Util.option_map v.database_name
             (fun f -> ("database_name", (String.to_json f)));
           Util.option_map v.character_set_name
             (fun f -> ("character_set_name", (String.to_json f)));
           Util.option_map v.backup_retention_period
             (fun f -> ("backup_retention_period", (Integer.to_json f)));
           Some
             ("availability_zones",
               (AvailabilityZones.to_json v.availability_zones))])
    let of_json j =
      {
        availability_zones =
          (AvailabilityZones.of_json
             (Util.of_option_exn (Json.lookup j "availability_zones")));
        backup_retention_period =
          (Util.option_map (Json.lookup j "backup_retention_period")
             Integer.of_json);
        character_set_name =
          (Util.option_map (Json.lookup j "character_set_name")
             String.of_json);
        database_name =
          (Util.option_map (Json.lookup j "database_name") String.of_json);
        d_b_cluster_identifier =
          (Util.option_map (Json.lookup j "d_b_cluster_identifier")
             String.of_json);
        d_b_cluster_parameter_group_name =
          (Util.option_map (Json.lookup j "d_b_cluster_parameter_group_name")
             String.of_json);
        vpc_security_group_ids =
          (VpcSecurityGroupIdList.of_json
             (Util.of_option_exn (Json.lookup j "vpc_security_group_ids")));
        d_b_subnet_group_name =
          (Util.option_map (Json.lookup j "d_b_subnet_group_name")
             String.of_json);
        engine = (Util.option_map (Json.lookup j "engine") String.of_json);
        engine_version =
          (Util.option_map (Json.lookup j "engine_version") String.of_json);
        port = (Util.option_map (Json.lookup j "port") Integer.of_json);
        master_username =
          (Util.option_map (Json.lookup j "master_username") String.of_json);
        master_user_password =
          (Util.option_map (Json.lookup j "master_user_password")
             String.of_json);
        option_group_name =
          (Util.option_map (Json.lookup j "option_group_name") String.of_json);
        preferred_backup_window =
          (Util.option_map (Json.lookup j "preferred_backup_window")
             String.of_json);
        preferred_maintenance_window =
          (Util.option_map (Json.lookup j "preferred_maintenance_window")
             String.of_json);
        tags = (TagList.of_json (Util.of_option_exn (Json.lookup j "tags")))
      }
  end
module ModifyDBClusterParameterGroupMessage =
  struct
    type t =
      {
      d_b_cluster_parameter_group_name: String.t ;
      parameters: ParametersList.t }
    let make ~d_b_cluster_parameter_group_name  ~parameters  () =
      { d_b_cluster_parameter_group_name; parameters }
    let parse xml =
      Some
        {
          d_b_cluster_parameter_group_name =
            (Xml.required "DBClusterParameterGroupName"
               (Util.option_bind
                  (Xml.member "DBClusterParameterGroupName" xml) String.parse));
          parameters =
            (Xml.required "Parameters"
               (Util.option_bind (Xml.member "Parameters" xml)
                  ParametersList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Parameters.member",
                   (ParametersList.to_query v.parameters)));
           Some
             (Query.Pair
                ("DBClusterParameterGroupName",
                  (String.to_query v.d_b_cluster_parameter_group_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("parameters", (ParametersList.to_json v.parameters));
           Some
             ("d_b_cluster_parameter_group_name",
               (String.to_json v.d_b_cluster_parameter_group_name))])
    let of_json j =
      {
        d_b_cluster_parameter_group_name =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "d_b_cluster_parameter_group_name")));
        parameters =
          (ParametersList.of_json
             (Util.of_option_exn (Json.lookup j "parameters")))
      }
  end
module ModifyOptionGroupResult =
  struct
    type t = {
      option_group: OptionGroup.t option }
    let make ?option_group  () = { option_group }
    let parse xml =
      Some
        {
          option_group =
            (Util.option_bind (Xml.member "OptionGroup" xml)
               OptionGroup.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.option_group
              (fun f -> Query.Pair ("OptionGroup", (OptionGroup.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.option_group
              (fun f -> ("option_group", (OptionGroup.to_json f)))])
    let of_json j =
      {
        option_group =
          (Util.option_map (Json.lookup j "option_group") OptionGroup.of_json)
      }
  end
module InvalidDBSubnetGroupFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ModifyDBInstanceMessage =
  struct
    type t =
      {
      d_b_instance_identifier: String.t ;
      allocated_storage: Integer.t option ;
      d_b_instance_class: String.t option ;
      d_b_security_groups: DBSecurityGroupNameList.t ;
      vpc_security_group_ids: VpcSecurityGroupIdList.t ;
      apply_immediately: Boolean.t option ;
      master_user_password: String.t option ;
      d_b_parameter_group_name: String.t option ;
      backup_retention_period: Integer.t option ;
      preferred_backup_window: String.t option ;
      preferred_maintenance_window: String.t option ;
      multi_a_z: Boolean.t option ;
      engine_version: String.t option ;
      allow_major_version_upgrade: Boolean.t option ;
      auto_minor_version_upgrade: Boolean.t option ;
      iops: Integer.t option ;
      option_group_name: String.t option ;
      new_d_b_instance_identifier: String.t option ;
      storage_type: String.t option ;
      tde_credential_arn: String.t option ;
      tde_credential_password: String.t option ;
      c_a_certificate_identifier: String.t option ;
      domain: String.t option ;
      copy_tags_to_snapshot: Boolean.t option }
    let make ~d_b_instance_identifier  ?allocated_storage 
      ?d_b_instance_class  ?(d_b_security_groups= []) 
      ?(vpc_security_group_ids= [])  ?apply_immediately 
      ?master_user_password  ?d_b_parameter_group_name 
      ?backup_retention_period  ?preferred_backup_window 
      ?preferred_maintenance_window  ?multi_a_z  ?engine_version 
      ?allow_major_version_upgrade  ?auto_minor_version_upgrade  ?iops 
      ?option_group_name  ?new_d_b_instance_identifier  ?storage_type 
      ?tde_credential_arn  ?tde_credential_password 
      ?c_a_certificate_identifier  ?domain  ?copy_tags_to_snapshot  () =
      {
        d_b_instance_identifier;
        allocated_storage;
        d_b_instance_class;
        d_b_security_groups;
        vpc_security_group_ids;
        apply_immediately;
        master_user_password;
        d_b_parameter_group_name;
        backup_retention_period;
        preferred_backup_window;
        preferred_maintenance_window;
        multi_a_z;
        engine_version;
        allow_major_version_upgrade;
        auto_minor_version_upgrade;
        iops;
        option_group_name;
        new_d_b_instance_identifier;
        storage_type;
        tde_credential_arn;
        tde_credential_password;
        c_a_certificate_identifier;
        domain;
        copy_tags_to_snapshot
      }
    let parse xml =
      Some
        {
          d_b_instance_identifier =
            (Xml.required "DBInstanceIdentifier"
               (Util.option_bind (Xml.member "DBInstanceIdentifier" xml)
                  String.parse));
          allocated_storage =
            (Util.option_bind (Xml.member "AllocatedStorage" xml)
               Integer.parse);
          d_b_instance_class =
            (Util.option_bind (Xml.member "DBInstanceClass" xml) String.parse);
          d_b_security_groups =
            (Util.of_option []
               (Util.option_bind (Xml.member "DBSecurityGroups" xml)
                  DBSecurityGroupNameList.parse));
          vpc_security_group_ids =
            (Util.of_option []
               (Util.option_bind (Xml.member "VpcSecurityGroupIds" xml)
                  VpcSecurityGroupIdList.parse));
          apply_immediately =
            (Util.option_bind (Xml.member "ApplyImmediately" xml)
               Boolean.parse);
          master_user_password =
            (Util.option_bind (Xml.member "MasterUserPassword" xml)
               String.parse);
          d_b_parameter_group_name =
            (Util.option_bind (Xml.member "DBParameterGroupName" xml)
               String.parse);
          backup_retention_period =
            (Util.option_bind (Xml.member "BackupRetentionPeriod" xml)
               Integer.parse);
          preferred_backup_window =
            (Util.option_bind (Xml.member "PreferredBackupWindow" xml)
               String.parse);
          preferred_maintenance_window =
            (Util.option_bind (Xml.member "PreferredMaintenanceWindow" xml)
               String.parse);
          multi_a_z =
            (Util.option_bind (Xml.member "MultiAZ" xml) Boolean.parse);
          engine_version =
            (Util.option_bind (Xml.member "EngineVersion" xml) String.parse);
          allow_major_version_upgrade =
            (Util.option_bind (Xml.member "AllowMajorVersionUpgrade" xml)
               Boolean.parse);
          auto_minor_version_upgrade =
            (Util.option_bind (Xml.member "AutoMinorVersionUpgrade" xml)
               Boolean.parse);
          iops = (Util.option_bind (Xml.member "Iops" xml) Integer.parse);
          option_group_name =
            (Util.option_bind (Xml.member "OptionGroupName" xml) String.parse);
          new_d_b_instance_identifier =
            (Util.option_bind (Xml.member "NewDBInstanceIdentifier" xml)
               String.parse);
          storage_type =
            (Util.option_bind (Xml.member "StorageType" xml) String.parse);
          tde_credential_arn =
            (Util.option_bind (Xml.member "TdeCredentialArn" xml)
               String.parse);
          tde_credential_password =
            (Util.option_bind (Xml.member "TdeCredentialPassword" xml)
               String.parse);
          c_a_certificate_identifier =
            (Util.option_bind (Xml.member "CACertificateIdentifier" xml)
               String.parse);
          domain = (Util.option_bind (Xml.member "Domain" xml) String.parse);
          copy_tags_to_snapshot =
            (Util.option_bind (Xml.member "CopyTagsToSnapshot" xml)
               Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.copy_tags_to_snapshot
              (fun f ->
                 Query.Pair ("CopyTagsToSnapshot", (Boolean.to_query f)));
           Util.option_map v.domain
             (fun f -> Query.Pair ("Domain", (String.to_query f)));
           Util.option_map v.c_a_certificate_identifier
             (fun f ->
                Query.Pair ("CACertificateIdentifier", (String.to_query f)));
           Util.option_map v.tde_credential_password
             (fun f ->
                Query.Pair ("TdeCredentialPassword", (String.to_query f)));
           Util.option_map v.tde_credential_arn
             (fun f -> Query.Pair ("TdeCredentialArn", (String.to_query f)));
           Util.option_map v.storage_type
             (fun f -> Query.Pair ("StorageType", (String.to_query f)));
           Util.option_map v.new_d_b_instance_identifier
             (fun f ->
                Query.Pair ("NewDBInstanceIdentifier", (String.to_query f)));
           Util.option_map v.option_group_name
             (fun f -> Query.Pair ("OptionGroupName", (String.to_query f)));
           Util.option_map v.iops
             (fun f -> Query.Pair ("Iops", (Integer.to_query f)));
           Util.option_map v.auto_minor_version_upgrade
             (fun f ->
                Query.Pair ("AutoMinorVersionUpgrade", (Boolean.to_query f)));
           Util.option_map v.allow_major_version_upgrade
             (fun f ->
                Query.Pair ("AllowMajorVersionUpgrade", (Boolean.to_query f)));
           Util.option_map v.engine_version
             (fun f -> Query.Pair ("EngineVersion", (String.to_query f)));
           Util.option_map v.multi_a_z
             (fun f -> Query.Pair ("MultiAZ", (Boolean.to_query f)));
           Util.option_map v.preferred_maintenance_window
             (fun f ->
                Query.Pair
                  ("PreferredMaintenanceWindow", (String.to_query f)));
           Util.option_map v.preferred_backup_window
             (fun f ->
                Query.Pair ("PreferredBackupWindow", (String.to_query f)));
           Util.option_map v.backup_retention_period
             (fun f ->
                Query.Pair ("BackupRetentionPeriod", (Integer.to_query f)));
           Util.option_map v.d_b_parameter_group_name
             (fun f ->
                Query.Pair ("DBParameterGroupName", (String.to_query f)));
           Util.option_map v.master_user_password
             (fun f -> Query.Pair ("MasterUserPassword", (String.to_query f)));
           Util.option_map v.apply_immediately
             (fun f -> Query.Pair ("ApplyImmediately", (Boolean.to_query f)));
           Some
             (Query.Pair
                ("VpcSecurityGroupIds.member",
                  (VpcSecurityGroupIdList.to_query v.vpc_security_group_ids)));
           Some
             (Query.Pair
                ("DBSecurityGroups.member",
                  (DBSecurityGroupNameList.to_query v.d_b_security_groups)));
           Util.option_map v.d_b_instance_class
             (fun f -> Query.Pair ("DBInstanceClass", (String.to_query f)));
           Util.option_map v.allocated_storage
             (fun f -> Query.Pair ("AllocatedStorage", (Integer.to_query f)));
           Some
             (Query.Pair
                ("DBInstanceIdentifier",
                  (String.to_query v.d_b_instance_identifier)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.copy_tags_to_snapshot
              (fun f -> ("copy_tags_to_snapshot", (Boolean.to_json f)));
           Util.option_map v.domain (fun f -> ("domain", (String.to_json f)));
           Util.option_map v.c_a_certificate_identifier
             (fun f -> ("c_a_certificate_identifier", (String.to_json f)));
           Util.option_map v.tde_credential_password
             (fun f -> ("tde_credential_password", (String.to_json f)));
           Util.option_map v.tde_credential_arn
             (fun f -> ("tde_credential_arn", (String.to_json f)));
           Util.option_map v.storage_type
             (fun f -> ("storage_type", (String.to_json f)));
           Util.option_map v.new_d_b_instance_identifier
             (fun f -> ("new_d_b_instance_identifier", (String.to_json f)));
           Util.option_map v.option_group_name
             (fun f -> ("option_group_name", (String.to_json f)));
           Util.option_map v.iops (fun f -> ("iops", (Integer.to_json f)));
           Util.option_map v.auto_minor_version_upgrade
             (fun f -> ("auto_minor_version_upgrade", (Boolean.to_json f)));
           Util.option_map v.allow_major_version_upgrade
             (fun f -> ("allow_major_version_upgrade", (Boolean.to_json f)));
           Util.option_map v.engine_version
             (fun f -> ("engine_version", (String.to_json f)));
           Util.option_map v.multi_a_z
             (fun f -> ("multi_a_z", (Boolean.to_json f)));
           Util.option_map v.preferred_maintenance_window
             (fun f -> ("preferred_maintenance_window", (String.to_json f)));
           Util.option_map v.preferred_backup_window
             (fun f -> ("preferred_backup_window", (String.to_json f)));
           Util.option_map v.backup_retention_period
             (fun f -> ("backup_retention_period", (Integer.to_json f)));
           Util.option_map v.d_b_parameter_group_name
             (fun f -> ("d_b_parameter_group_name", (String.to_json f)));
           Util.option_map v.master_user_password
             (fun f -> ("master_user_password", (String.to_json f)));
           Util.option_map v.apply_immediately
             (fun f -> ("apply_immediately", (Boolean.to_json f)));
           Some
             ("vpc_security_group_ids",
               (VpcSecurityGroupIdList.to_json v.vpc_security_group_ids));
           Some
             ("d_b_security_groups",
               (DBSecurityGroupNameList.to_json v.d_b_security_groups));
           Util.option_map v.d_b_instance_class
             (fun f -> ("d_b_instance_class", (String.to_json f)));
           Util.option_map v.allocated_storage
             (fun f -> ("allocated_storage", (Integer.to_json f)));
           Some
             ("d_b_instance_identifier",
               (String.to_json v.d_b_instance_identifier))])
    let of_json j =
      {
        d_b_instance_identifier =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "d_b_instance_identifier")));
        allocated_storage =
          (Util.option_map (Json.lookup j "allocated_storage")
             Integer.of_json);
        d_b_instance_class =
          (Util.option_map (Json.lookup j "d_b_instance_class")
             String.of_json);
        d_b_security_groups =
          (DBSecurityGroupNameList.of_json
             (Util.of_option_exn (Json.lookup j "d_b_security_groups")));
        vpc_security_group_ids =
          (VpcSecurityGroupIdList.of_json
             (Util.of_option_exn (Json.lookup j "vpc_security_group_ids")));
        apply_immediately =
          (Util.option_map (Json.lookup j "apply_immediately")
             Boolean.of_json);
        master_user_password =
          (Util.option_map (Json.lookup j "master_user_password")
             String.of_json);
        d_b_parameter_group_name =
          (Util.option_map (Json.lookup j "d_b_parameter_group_name")
             String.of_json);
        backup_retention_period =
          (Util.option_map (Json.lookup j "backup_retention_period")
             Integer.of_json);
        preferred_backup_window =
          (Util.option_map (Json.lookup j "preferred_backup_window")
             String.of_json);
        preferred_maintenance_window =
          (Util.option_map (Json.lookup j "preferred_maintenance_window")
             String.of_json);
        multi_a_z =
          (Util.option_map (Json.lookup j "multi_a_z") Boolean.of_json);
        engine_version =
          (Util.option_map (Json.lookup j "engine_version") String.of_json);
        allow_major_version_upgrade =
          (Util.option_map (Json.lookup j "allow_major_version_upgrade")
             Boolean.of_json);
        auto_minor_version_upgrade =
          (Util.option_map (Json.lookup j "auto_minor_version_upgrade")
             Boolean.of_json);
        iops = (Util.option_map (Json.lookup j "iops") Integer.of_json);
        option_group_name =
          (Util.option_map (Json.lookup j "option_group_name") String.of_json);
        new_d_b_instance_identifier =
          (Util.option_map (Json.lookup j "new_d_b_instance_identifier")
             String.of_json);
        storage_type =
          (Util.option_map (Json.lookup j "storage_type") String.of_json);
        tde_credential_arn =
          (Util.option_map (Json.lookup j "tde_credential_arn")
             String.of_json);
        tde_credential_password =
          (Util.option_map (Json.lookup j "tde_credential_password")
             String.of_json);
        c_a_certificate_identifier =
          (Util.option_map (Json.lookup j "c_a_certificate_identifier")
             String.of_json);
        domain = (Util.option_map (Json.lookup j "domain") String.of_json);
        copy_tags_to_snapshot =
          (Util.option_map (Json.lookup j "copy_tags_to_snapshot")
             Boolean.of_json)
      }
  end
module DescribeReservedDBInstancesOfferingsMessage =
  struct
    type t =
      {
      reserved_d_b_instances_offering_id: String.t option ;
      d_b_instance_class: String.t option ;
      duration: String.t option ;
      product_description: String.t option ;
      offering_type: String.t option ;
      multi_a_z: Boolean.t option ;
      filters: FilterList.t ;
      max_records: Integer.t option ;
      marker: String.t option }
    let make ?reserved_d_b_instances_offering_id  ?d_b_instance_class 
      ?duration  ?product_description  ?offering_type  ?multi_a_z  ?(filters=
      [])  ?max_records  ?marker  () =
      {
        reserved_d_b_instances_offering_id;
        d_b_instance_class;
        duration;
        product_description;
        offering_type;
        multi_a_z;
        filters;
        max_records;
        marker
      }
    let parse xml =
      Some
        {
          reserved_d_b_instances_offering_id =
            (Util.option_bind
               (Xml.member "ReservedDBInstancesOfferingId" xml) String.parse);
          d_b_instance_class =
            (Util.option_bind (Xml.member "DBInstanceClass" xml) String.parse);
          duration =
            (Util.option_bind (Xml.member "Duration" xml) String.parse);
          product_description =
            (Util.option_bind (Xml.member "ProductDescription" xml)
               String.parse);
          offering_type =
            (Util.option_bind (Xml.member "OfferingType" xml) String.parse);
          multi_a_z =
            (Util.option_bind (Xml.member "MultiAZ" xml) Boolean.parse);
          filters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Filters" xml) FilterList.parse));
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Some
             (Query.Pair ("Filters.member", (FilterList.to_query v.filters)));
           Util.option_map v.multi_a_z
             (fun f -> Query.Pair ("MultiAZ", (Boolean.to_query f)));
           Util.option_map v.offering_type
             (fun f -> Query.Pair ("OfferingType", (String.to_query f)));
           Util.option_map v.product_description
             (fun f -> Query.Pair ("ProductDescription", (String.to_query f)));
           Util.option_map v.duration
             (fun f -> Query.Pair ("Duration", (String.to_query f)));
           Util.option_map v.d_b_instance_class
             (fun f -> Query.Pair ("DBInstanceClass", (String.to_query f)));
           Util.option_map v.reserved_d_b_instances_offering_id
             (fun f ->
                Query.Pair
                  ("ReservedDBInstancesOfferingId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Some ("filters", (FilterList.to_json v.filters));
           Util.option_map v.multi_a_z
             (fun f -> ("multi_a_z", (Boolean.to_json f)));
           Util.option_map v.offering_type
             (fun f -> ("offering_type", (String.to_json f)));
           Util.option_map v.product_description
             (fun f -> ("product_description", (String.to_json f)));
           Util.option_map v.duration
             (fun f -> ("duration", (String.to_json f)));
           Util.option_map v.d_b_instance_class
             (fun f -> ("d_b_instance_class", (String.to_json f)));
           Util.option_map v.reserved_d_b_instances_offering_id
             (fun f ->
                ("reserved_d_b_instances_offering_id", (String.to_json f)))])
    let of_json j =
      {
        reserved_d_b_instances_offering_id =
          (Util.option_map
             (Json.lookup j "reserved_d_b_instances_offering_id")
             String.of_json);
        d_b_instance_class =
          (Util.option_map (Json.lookup j "d_b_instance_class")
             String.of_json);
        duration =
          (Util.option_map (Json.lookup j "duration") String.of_json);
        product_description =
          (Util.option_map (Json.lookup j "product_description")
             String.of_json);
        offering_type =
          (Util.option_map (Json.lookup j "offering_type") String.of_json);
        multi_a_z =
          (Util.option_map (Json.lookup j "multi_a_z") Boolean.of_json);
        filters =
          (FilterList.of_json (Util.of_option_exn (Json.lookup j "filters")));
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module OptionGroupNotFoundFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ResetDBClusterParameterGroupMessage =
  struct
    type t =
      {
      d_b_cluster_parameter_group_name: String.t ;
      reset_all_parameters: Boolean.t option ;
      parameters: ParametersList.t }
    let make ~d_b_cluster_parameter_group_name  ?reset_all_parameters 
      ?(parameters= [])  () =
      { d_b_cluster_parameter_group_name; reset_all_parameters; parameters }
    let parse xml =
      Some
        {
          d_b_cluster_parameter_group_name =
            (Xml.required "DBClusterParameterGroupName"
               (Util.option_bind
                  (Xml.member "DBClusterParameterGroupName" xml) String.parse));
          reset_all_parameters =
            (Util.option_bind (Xml.member "ResetAllParameters" xml)
               Boolean.parse);
          parameters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Parameters" xml)
                  ParametersList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Parameters.member",
                   (ParametersList.to_query v.parameters)));
           Util.option_map v.reset_all_parameters
             (fun f ->
                Query.Pair ("ResetAllParameters", (Boolean.to_query f)));
           Some
             (Query.Pair
                ("DBClusterParameterGroupName",
                  (String.to_query v.d_b_cluster_parameter_group_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("parameters", (ParametersList.to_json v.parameters));
           Util.option_map v.reset_all_parameters
             (fun f -> ("reset_all_parameters", (Boolean.to_json f)));
           Some
             ("d_b_cluster_parameter_group_name",
               (String.to_json v.d_b_cluster_parameter_group_name))])
    let of_json j =
      {
        d_b_cluster_parameter_group_name =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "d_b_cluster_parameter_group_name")));
        reset_all_parameters =
          (Util.option_map (Json.lookup j "reset_all_parameters")
             Boolean.of_json);
        parameters =
          (ParametersList.of_json
             (Util.of_option_exn (Json.lookup j "parameters")))
      }
  end
module RestoreDBClusterFromSnapshotResult =
  struct
    type t = {
      d_b_cluster: DBCluster.t option }
    let make ?d_b_cluster  () = { d_b_cluster }
    let parse xml =
      Some
        {
          d_b_cluster =
            (Util.option_bind (Xml.member "DBCluster" xml) DBCluster.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.d_b_cluster
              (fun f -> Query.Pair ("DBCluster", (DBCluster.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.d_b_cluster
              (fun f -> ("d_b_cluster", (DBCluster.to_json f)))])
    let of_json j =
      {
        d_b_cluster =
          (Util.option_map (Json.lookup j "d_b_cluster") DBCluster.of_json)
      }
  end
module RestoreDBInstanceFromDBSnapshotResult =
  struct
    type t = {
      d_b_instance: DBInstance.t option }
    let make ?d_b_instance  () = { d_b_instance }
    let parse xml =
      Some
        {
          d_b_instance =
            (Util.option_bind (Xml.member "DBInstance" xml) DBInstance.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.d_b_instance
              (fun f -> Query.Pair ("DBInstance", (DBInstance.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.d_b_instance
              (fun f -> ("d_b_instance", (DBInstance.to_json f)))])
    let of_json j =
      {
        d_b_instance =
          (Util.option_map (Json.lookup j "d_b_instance") DBInstance.of_json)
      }
  end
module RestoreDBClusterFromSnapshotMessage =
  struct
    type t =
      {
      availability_zones: AvailabilityZones.t ;
      d_b_cluster_identifier: String.t ;
      snapshot_identifier: String.t ;
      engine: String.t ;
      engine_version: String.t option ;
      port: Integer.t option ;
      d_b_subnet_group_name: String.t option ;
      database_name: String.t option ;
      option_group_name: String.t option ;
      vpc_security_group_ids: VpcSecurityGroupIdList.t ;
      tags: TagList.t }
    let make ?(availability_zones= [])  ~d_b_cluster_identifier 
      ~snapshot_identifier  ~engine  ?engine_version  ?port 
      ?d_b_subnet_group_name  ?database_name  ?option_group_name 
      ?(vpc_security_group_ids= [])  ?(tags= [])  () =
      {
        availability_zones;
        d_b_cluster_identifier;
        snapshot_identifier;
        engine;
        engine_version;
        port;
        d_b_subnet_group_name;
        database_name;
        option_group_name;
        vpc_security_group_ids;
        tags
      }
    let parse xml =
      Some
        {
          availability_zones =
            (Util.of_option []
               (Util.option_bind (Xml.member "AvailabilityZones" xml)
                  AvailabilityZones.parse));
          d_b_cluster_identifier =
            (Xml.required "DBClusterIdentifier"
               (Util.option_bind (Xml.member "DBClusterIdentifier" xml)
                  String.parse));
          snapshot_identifier =
            (Xml.required "SnapshotIdentifier"
               (Util.option_bind (Xml.member "SnapshotIdentifier" xml)
                  String.parse));
          engine =
            (Xml.required "Engine"
               (Util.option_bind (Xml.member "Engine" xml) String.parse));
          engine_version =
            (Util.option_bind (Xml.member "EngineVersion" xml) String.parse);
          port = (Util.option_bind (Xml.member "Port" xml) Integer.parse);
          d_b_subnet_group_name =
            (Util.option_bind (Xml.member "DBSubnetGroupName" xml)
               String.parse);
          database_name =
            (Util.option_bind (Xml.member "DatabaseName" xml) String.parse);
          option_group_name =
            (Util.option_bind (Xml.member "OptionGroupName" xml) String.parse);
          vpc_security_group_ids =
            (Util.of_option []
               (Util.option_bind (Xml.member "VpcSecurityGroupIds" xml)
                  VpcSecurityGroupIdList.parse));
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) TagList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Tags.member", (TagList.to_query v.tags)));
           Some
             (Query.Pair
                ("VpcSecurityGroupIds.member",
                  (VpcSecurityGroupIdList.to_query v.vpc_security_group_ids)));
           Util.option_map v.option_group_name
             (fun f -> Query.Pair ("OptionGroupName", (String.to_query f)));
           Util.option_map v.database_name
             (fun f -> Query.Pair ("DatabaseName", (String.to_query f)));
           Util.option_map v.d_b_subnet_group_name
             (fun f -> Query.Pair ("DBSubnetGroupName", (String.to_query f)));
           Util.option_map v.port
             (fun f -> Query.Pair ("Port", (Integer.to_query f)));
           Util.option_map v.engine_version
             (fun f -> Query.Pair ("EngineVersion", (String.to_query f)));
           Some (Query.Pair ("Engine", (String.to_query v.engine)));
           Some
             (Query.Pair
                ("SnapshotIdentifier",
                  (String.to_query v.snapshot_identifier)));
           Some
             (Query.Pair
                ("DBClusterIdentifier",
                  (String.to_query v.d_b_cluster_identifier)));
           Some
             (Query.Pair
                ("AvailabilityZones.member",
                  (AvailabilityZones.to_query v.availability_zones)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tags", (TagList.to_json v.tags));
           Some
             ("vpc_security_group_ids",
               (VpcSecurityGroupIdList.to_json v.vpc_security_group_ids));
           Util.option_map v.option_group_name
             (fun f -> ("option_group_name", (String.to_json f)));
           Util.option_map v.database_name
             (fun f -> ("database_name", (String.to_json f)));
           Util.option_map v.d_b_subnet_group_name
             (fun f -> ("d_b_subnet_group_name", (String.to_json f)));
           Util.option_map v.port (fun f -> ("port", (Integer.to_json f)));
           Util.option_map v.engine_version
             (fun f -> ("engine_version", (String.to_json f)));
           Some ("engine", (String.to_json v.engine));
           Some
             ("snapshot_identifier", (String.to_json v.snapshot_identifier));
           Some
             ("d_b_cluster_identifier",
               (String.to_json v.d_b_cluster_identifier));
           Some
             ("availability_zones",
               (AvailabilityZones.to_json v.availability_zones))])
    let of_json j =
      {
        availability_zones =
          (AvailabilityZones.of_json
             (Util.of_option_exn (Json.lookup j "availability_zones")));
        d_b_cluster_identifier =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "d_b_cluster_identifier")));
        snapshot_identifier =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "snapshot_identifier")));
        engine =
          (String.of_json (Util.of_option_exn (Json.lookup j "engine")));
        engine_version =
          (Util.option_map (Json.lookup j "engine_version") String.of_json);
        port = (Util.option_map (Json.lookup j "port") Integer.of_json);
        d_b_subnet_group_name =
          (Util.option_map (Json.lookup j "d_b_subnet_group_name")
             String.of_json);
        database_name =
          (Util.option_map (Json.lookup j "database_name") String.of_json);
        option_group_name =
          (Util.option_map (Json.lookup j "option_group_name") String.of_json);
        vpc_security_group_ids =
          (VpcSecurityGroupIdList.of_json
             (Util.of_option_exn (Json.lookup j "vpc_security_group_ids")));
        tags = (TagList.of_json (Util.of_option_exn (Json.lookup j "tags")))
      }
  end
module DBParameterGroupQuotaExceededFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DescribeDBInstancesMessage =
  struct
    type t =
      {
      d_b_instance_identifier: String.t option ;
      filters: FilterList.t ;
      max_records: Integer.t option ;
      marker: String.t option }
    let make ?d_b_instance_identifier  ?(filters= [])  ?max_records  ?marker 
      () = { d_b_instance_identifier; filters; max_records; marker }
    let parse xml =
      Some
        {
          d_b_instance_identifier =
            (Util.option_bind (Xml.member "DBInstanceIdentifier" xml)
               String.parse);
          filters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Filters" xml) FilterList.parse));
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Some
             (Query.Pair ("Filters.member", (FilterList.to_query v.filters)));
           Util.option_map v.d_b_instance_identifier
             (fun f ->
                Query.Pair ("DBInstanceIdentifier", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Some ("filters", (FilterList.to_json v.filters));
           Util.option_map v.d_b_instance_identifier
             (fun f -> ("d_b_instance_identifier", (String.to_json f)))])
    let of_json j =
      {
        d_b_instance_identifier =
          (Util.option_map (Json.lookup j "d_b_instance_identifier")
             String.of_json);
        filters =
          (FilterList.of_json (Util.of_option_exn (Json.lookup j "filters")));
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module ModifyDBInstanceResult =
  struct
    type t = {
      d_b_instance: DBInstance.t option }
    let make ?d_b_instance  () = { d_b_instance }
    let parse xml =
      Some
        {
          d_b_instance =
            (Util.option_bind (Xml.member "DBInstance" xml) DBInstance.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.d_b_instance
              (fun f -> Query.Pair ("DBInstance", (DBInstance.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.d_b_instance
              (fun f -> ("d_b_instance", (DBInstance.to_json f)))])
    let of_json j =
      {
        d_b_instance =
          (Util.option_map (Json.lookup j "d_b_instance") DBInstance.of_json)
      }
  end
module DescribeEngineDefaultClusterParametersResult =
  struct
    type t = {
      engine_defaults: EngineDefaults.t option }
    let make ?engine_defaults  () = { engine_defaults }
    let parse xml =
      Some
        {
          engine_defaults =
            (Util.option_bind (Xml.member "EngineDefaults" xml)
               EngineDefaults.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.engine_defaults
              (fun f ->
                 Query.Pair ("EngineDefaults", (EngineDefaults.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.engine_defaults
              (fun f -> ("engine_defaults", (EngineDefaults.to_json f)))])
    let of_json j =
      {
        engine_defaults =
          (Util.option_map (Json.lookup j "engine_defaults")
             EngineDefaults.of_json)
      }
  end
module CreateDBSnapshotMessage =
  struct
    type t =
      {
      d_b_snapshot_identifier: String.t ;
      d_b_instance_identifier: String.t ;
      tags: TagList.t }
    let make ~d_b_snapshot_identifier  ~d_b_instance_identifier  ?(tags= []) 
      () = { d_b_snapshot_identifier; d_b_instance_identifier; tags }
    let parse xml =
      Some
        {
          d_b_snapshot_identifier =
            (Xml.required "DBSnapshotIdentifier"
               (Util.option_bind (Xml.member "DBSnapshotIdentifier" xml)
                  String.parse));
          d_b_instance_identifier =
            (Xml.required "DBInstanceIdentifier"
               (Util.option_bind (Xml.member "DBInstanceIdentifier" xml)
                  String.parse));
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) TagList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Tags.member", (TagList.to_query v.tags)));
           Some
             (Query.Pair
                ("DBInstanceIdentifier",
                  (String.to_query v.d_b_instance_identifier)));
           Some
             (Query.Pair
                ("DBSnapshotIdentifier",
                  (String.to_query v.d_b_snapshot_identifier)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tags", (TagList.to_json v.tags));
           Some
             ("d_b_instance_identifier",
               (String.to_json v.d_b_instance_identifier));
           Some
             ("d_b_snapshot_identifier",
               (String.to_json v.d_b_snapshot_identifier))])
    let of_json j =
      {
        d_b_snapshot_identifier =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "d_b_snapshot_identifier")));
        d_b_instance_identifier =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "d_b_instance_identifier")));
        tags = (TagList.of_json (Util.of_option_exn (Json.lookup j "tags")))
      }
  end
module CopyDBParameterGroupMessage =
  struct
    type t =
      {
      source_d_b_parameter_group_identifier: String.t ;
      target_d_b_parameter_group_identifier: String.t ;
      target_d_b_parameter_group_description: String.t ;
      tags: TagList.t }
    let make ~source_d_b_parameter_group_identifier 
      ~target_d_b_parameter_group_identifier 
      ~target_d_b_parameter_group_description  ?(tags= [])  () =
      {
        source_d_b_parameter_group_identifier;
        target_d_b_parameter_group_identifier;
        target_d_b_parameter_group_description;
        tags
      }
    let parse xml =
      Some
        {
          source_d_b_parameter_group_identifier =
            (Xml.required "SourceDBParameterGroupIdentifier"
               (Util.option_bind
                  (Xml.member "SourceDBParameterGroupIdentifier" xml)
                  String.parse));
          target_d_b_parameter_group_identifier =
            (Xml.required "TargetDBParameterGroupIdentifier"
               (Util.option_bind
                  (Xml.member "TargetDBParameterGroupIdentifier" xml)
                  String.parse));
          target_d_b_parameter_group_description =
            (Xml.required "TargetDBParameterGroupDescription"
               (Util.option_bind
                  (Xml.member "TargetDBParameterGroupDescription" xml)
                  String.parse));
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) TagList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Tags.member", (TagList.to_query v.tags)));
           Some
             (Query.Pair
                ("TargetDBParameterGroupDescription",
                  (String.to_query v.target_d_b_parameter_group_description)));
           Some
             (Query.Pair
                ("TargetDBParameterGroupIdentifier",
                  (String.to_query v.target_d_b_parameter_group_identifier)));
           Some
             (Query.Pair
                ("SourceDBParameterGroupIdentifier",
                  (String.to_query v.source_d_b_parameter_group_identifier)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tags", (TagList.to_json v.tags));
           Some
             ("target_d_b_parameter_group_description",
               (String.to_json v.target_d_b_parameter_group_description));
           Some
             ("target_d_b_parameter_group_identifier",
               (String.to_json v.target_d_b_parameter_group_identifier));
           Some
             ("source_d_b_parameter_group_identifier",
               (String.to_json v.source_d_b_parameter_group_identifier))])
    let of_json j =
      {
        source_d_b_parameter_group_identifier =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "source_d_b_parameter_group_identifier")));
        target_d_b_parameter_group_identifier =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "target_d_b_parameter_group_identifier")));
        target_d_b_parameter_group_description =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "target_d_b_parameter_group_description")));
        tags = (TagList.of_json (Util.of_option_exn (Json.lookup j "tags")))
      }
  end
module DeleteEventSubscriptionResult =
  struct
    type t = {
      event_subscription: EventSubscription.t option }
    let make ?event_subscription  () = { event_subscription }
    let parse xml =
      Some
        {
          event_subscription =
            (Util.option_bind (Xml.member "EventSubscription" xml)
               EventSubscription.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.event_subscription
              (fun f ->
                 Query.Pair
                   ("EventSubscription", (EventSubscription.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.event_subscription
              (fun f -> ("event_subscription", (EventSubscription.to_json f)))])
    let of_json j =
      {
        event_subscription =
          (Util.option_map (Json.lookup j "event_subscription")
             EventSubscription.of_json)
      }
  end
module OptionGroupAlreadyExistsFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module InvalidEventSubscriptionStateFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DBInstanceMessage =
  struct
    type t = {
      marker: String.t option ;
      d_b_instances: DBInstanceList.t }
    let make ?marker  ?(d_b_instances= [])  () = { marker; d_b_instances }
    let parse xml =
      Some
        {
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          d_b_instances =
            (Util.of_option []
               (Util.option_bind (Xml.member "DBInstances" xml)
                  DBInstanceList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("DBInstances.member",
                   (DBInstanceList.to_query v.d_b_instances)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("d_b_instances", (DBInstanceList.to_json v.d_b_instances));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)))])
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        d_b_instances =
          (DBInstanceList.of_json
             (Util.of_option_exn (Json.lookup j "d_b_instances")))
      }
  end
module CopyDBSnapshotResult =
  struct
    type t = {
      d_b_snapshot: DBSnapshot.t option }
    let make ?d_b_snapshot  () = { d_b_snapshot }
    let parse xml =
      Some
        {
          d_b_snapshot =
            (Util.option_bind (Xml.member "DBSnapshot" xml) DBSnapshot.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.d_b_snapshot
              (fun f -> Query.Pair ("DBSnapshot", (DBSnapshot.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.d_b_snapshot
              (fun f -> ("d_b_snapshot", (DBSnapshot.to_json f)))])
    let of_json j =
      {
        d_b_snapshot =
          (Util.option_map (Json.lookup j "d_b_snapshot") DBSnapshot.of_json)
      }
  end
module InsufficientDBInstanceCapacityFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CreateOptionGroupResult =
  struct
    type t = {
      option_group: OptionGroup.t option }
    let make ?option_group  () = { option_group }
    let parse xml =
      Some
        {
          option_group =
            (Util.option_bind (Xml.member "OptionGroup" xml)
               OptionGroup.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.option_group
              (fun f -> Query.Pair ("OptionGroup", (OptionGroup.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.option_group
              (fun f -> ("option_group", (OptionGroup.to_json f)))])
    let of_json j =
      {
        option_group =
          (Util.option_map (Json.lookup j "option_group") OptionGroup.of_json)
      }
  end
module DescribeCertificatesMessage =
  struct
    type t =
      {
      certificate_identifier: String.t option ;
      filters: FilterList.t ;
      max_records: Integer.t option ;
      marker: String.t option }
    let make ?certificate_identifier  ?(filters= [])  ?max_records  ?marker 
      () = { certificate_identifier; filters; max_records; marker }
    let parse xml =
      Some
        {
          certificate_identifier =
            (Util.option_bind (Xml.member "CertificateIdentifier" xml)
               String.parse);
          filters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Filters" xml) FilterList.parse));
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Some
             (Query.Pair ("Filters.member", (FilterList.to_query v.filters)));
           Util.option_map v.certificate_identifier
             (fun f ->
                Query.Pair ("CertificateIdentifier", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Some ("filters", (FilterList.to_json v.filters));
           Util.option_map v.certificate_identifier
             (fun f -> ("certificate_identifier", (String.to_json f)))])
    let of_json j =
      {
        certificate_identifier =
          (Util.option_map (Json.lookup j "certificate_identifier")
             String.of_json);
        filters =
          (FilterList.of_json (Util.of_option_exn (Json.lookup j "filters")));
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module DBSubnetGroupNotAllowedFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CreateDBSecurityGroupResult =
  struct
    type t = {
      d_b_security_group: DBSecurityGroup.t option }
    let make ?d_b_security_group  () = { d_b_security_group }
    let parse xml =
      Some
        {
          d_b_security_group =
            (Util.option_bind (Xml.member "DBSecurityGroup" xml)
               DBSecurityGroup.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.d_b_security_group
              (fun f ->
                 Query.Pair ("DBSecurityGroup", (DBSecurityGroup.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.d_b_security_group
              (fun f -> ("d_b_security_group", (DBSecurityGroup.to_json f)))])
    let of_json j =
      {
        d_b_security_group =
          (Util.option_map (Json.lookup j "d_b_security_group")
             DBSecurityGroup.of_json)
      }
  end
module AccountAttributesMessage =
  struct
    type t = {
      account_quotas: AccountQuotaList.t }
    let make ?(account_quotas= [])  () = { account_quotas }
    let parse xml =
      Some
        {
          account_quotas =
            (Util.of_option []
               (Util.option_bind (Xml.member "AccountQuotas" xml)
                  AccountQuotaList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("AccountQuotas.member",
                   (AccountQuotaList.to_query v.account_quotas)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("account_quotas", (AccountQuotaList.to_json v.account_quotas))])
    let of_json j =
      {
        account_quotas =
          (AccountQuotaList.of_json
             (Util.of_option_exn (Json.lookup j "account_quotas")))
      }
  end
module DBSecurityGroupQuotaExceededFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module RestoreDBClusterToPointInTimeResult =
  struct
    type t = {
      d_b_cluster: DBCluster.t option }
    let make ?d_b_cluster  () = { d_b_cluster }
    let parse xml =
      Some
        {
          d_b_cluster =
            (Util.option_bind (Xml.member "DBCluster" xml) DBCluster.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.d_b_cluster
              (fun f -> Query.Pair ("DBCluster", (DBCluster.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.d_b_cluster
              (fun f -> ("d_b_cluster", (DBCluster.to_json f)))])
    let of_json j =
      {
        d_b_cluster =
          (Util.option_map (Json.lookup j "d_b_cluster") DBCluster.of_json)
      }
  end
module StorageQuotaExceededFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module InvalidDBSubnetStateFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module RebootDBInstanceResult =
  struct
    type t = {
      d_b_instance: DBInstance.t option }
    let make ?d_b_instance  () = { d_b_instance }
    let parse xml =
      Some
        {
          d_b_instance =
            (Util.option_bind (Xml.member "DBInstance" xml) DBInstance.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.d_b_instance
              (fun f -> Query.Pair ("DBInstance", (DBInstance.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.d_b_instance
              (fun f -> ("d_b_instance", (DBInstance.to_json f)))])
    let of_json j =
      {
        d_b_instance =
          (Util.option_map (Json.lookup j "d_b_instance") DBInstance.of_json)
      }
  end
module ResetDBParameterGroupMessage =
  struct
    type t =
      {
      d_b_parameter_group_name: String.t ;
      reset_all_parameters: Boolean.t option ;
      parameters: ParametersList.t }
    let make ~d_b_parameter_group_name  ?reset_all_parameters  ?(parameters=
      [])  () =
      { d_b_parameter_group_name; reset_all_parameters; parameters }
    let parse xml =
      Some
        {
          d_b_parameter_group_name =
            (Xml.required "DBParameterGroupName"
               (Util.option_bind (Xml.member "DBParameterGroupName" xml)
                  String.parse));
          reset_all_parameters =
            (Util.option_bind (Xml.member "ResetAllParameters" xml)
               Boolean.parse);
          parameters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Parameters" xml)
                  ParametersList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("Parameters.member",
                   (ParametersList.to_query v.parameters)));
           Util.option_map v.reset_all_parameters
             (fun f ->
                Query.Pair ("ResetAllParameters", (Boolean.to_query f)));
           Some
             (Query.Pair
                ("DBParameterGroupName",
                  (String.to_query v.d_b_parameter_group_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("parameters", (ParametersList.to_json v.parameters));
           Util.option_map v.reset_all_parameters
             (fun f -> ("reset_all_parameters", (Boolean.to_json f)));
           Some
             ("d_b_parameter_group_name",
               (String.to_json v.d_b_parameter_group_name))])
    let of_json j =
      {
        d_b_parameter_group_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "d_b_parameter_group_name")));
        reset_all_parameters =
          (Util.option_map (Json.lookup j "reset_all_parameters")
             Boolean.of_json);
        parameters =
          (ParametersList.of_json
             (Util.of_option_exn (Json.lookup j "parameters")))
      }
  end
module DescribeOptionGroupsMessage =
  struct
    type t =
      {
      option_group_name: String.t option ;
      filters: FilterList.t ;
      marker: String.t option ;
      max_records: Integer.t option ;
      engine_name: String.t option ;
      major_engine_version: String.t option }
    let make ?option_group_name  ?(filters= [])  ?marker  ?max_records 
      ?engine_name  ?major_engine_version  () =
      {
        option_group_name;
        filters;
        marker;
        max_records;
        engine_name;
        major_engine_version
      }
    let parse xml =
      Some
        {
          option_group_name =
            (Util.option_bind (Xml.member "OptionGroupName" xml) String.parse);
          filters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Filters" xml) FilterList.parse));
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          engine_name =
            (Util.option_bind (Xml.member "EngineName" xml) String.parse);
          major_engine_version =
            (Util.option_bind (Xml.member "MajorEngineVersion" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.major_engine_version
              (fun f ->
                 Query.Pair ("MajorEngineVersion", (String.to_query f)));
           Util.option_map v.engine_name
             (fun f -> Query.Pair ("EngineName", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Some
             (Query.Pair ("Filters.member", (FilterList.to_query v.filters)));
           Util.option_map v.option_group_name
             (fun f -> Query.Pair ("OptionGroupName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.major_engine_version
              (fun f -> ("major_engine_version", (String.to_json f)));
           Util.option_map v.engine_name
             (fun f -> ("engine_name", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)));
           Some ("filters", (FilterList.to_json v.filters));
           Util.option_map v.option_group_name
             (fun f -> ("option_group_name", (String.to_json f)))])
    let of_json j =
      {
        option_group_name =
          (Util.option_map (Json.lookup j "option_group_name") String.of_json);
        filters =
          (FilterList.of_json (Util.of_option_exn (Json.lookup j "filters")));
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        engine_name =
          (Util.option_map (Json.lookup j "engine_name") String.of_json);
        major_engine_version =
          (Util.option_map (Json.lookup j "major_engine_version")
             String.of_json)
      }
  end
module InvalidDBParameterGroupStateFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DeleteEventSubscriptionMessage =
  struct
    type t = {
      subscription_name: String.t }
    let make ~subscription_name  () = { subscription_name }
    let parse xml =
      Some
        {
          subscription_name =
            (Xml.required "SubscriptionName"
               (Util.option_bind (Xml.member "SubscriptionName" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("SubscriptionName", (String.to_query v.subscription_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("subscription_name", (String.to_json v.subscription_name))])
    let of_json j =
      {
        subscription_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "subscription_name")))
      }
  end
module DBParameterGroupNotFoundFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DeleteDBSecurityGroupMessage =
  struct
    type t = {
      d_b_security_group_name: String.t }
    let make ~d_b_security_group_name  () = { d_b_security_group_name }
    let parse xml =
      Some
        {
          d_b_security_group_name =
            (Xml.required "DBSecurityGroupName"
               (Util.option_bind (Xml.member "DBSecurityGroupName" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("DBSecurityGroupName",
                   (String.to_query v.d_b_security_group_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("d_b_security_group_name",
                (String.to_json v.d_b_security_group_name))])
    let of_json j =
      {
        d_b_security_group_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "d_b_security_group_name")))
      }
  end
module DBInstanceAlreadyExistsFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module RevokeDBSecurityGroupIngressMessage =
  struct
    type t =
      {
      d_b_security_group_name: String.t ;
      c_i_d_r_i_p: String.t option ;
      e_c2_security_group_name: String.t option ;
      e_c2_security_group_id: String.t option ;
      e_c2_security_group_owner_id: String.t option }
    let make ~d_b_security_group_name  ?c_i_d_r_i_p 
      ?e_c2_security_group_name  ?e_c2_security_group_id 
      ?e_c2_security_group_owner_id  () =
      {
        d_b_security_group_name;
        c_i_d_r_i_p;
        e_c2_security_group_name;
        e_c2_security_group_id;
        e_c2_security_group_owner_id
      }
    let parse xml =
      Some
        {
          d_b_security_group_name =
            (Xml.required "DBSecurityGroupName"
               (Util.option_bind (Xml.member "DBSecurityGroupName" xml)
                  String.parse));
          c_i_d_r_i_p =
            (Util.option_bind (Xml.member "CIDRIP" xml) String.parse);
          e_c2_security_group_name =
            (Util.option_bind (Xml.member "EC2SecurityGroupName" xml)
               String.parse);
          e_c2_security_group_id =
            (Util.option_bind (Xml.member "EC2SecurityGroupId" xml)
               String.parse);
          e_c2_security_group_owner_id =
            (Util.option_bind (Xml.member "EC2SecurityGroupOwnerId" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.e_c2_security_group_owner_id
              (fun f ->
                 Query.Pair ("EC2SecurityGroupOwnerId", (String.to_query f)));
           Util.option_map v.e_c2_security_group_id
             (fun f -> Query.Pair ("EC2SecurityGroupId", (String.to_query f)));
           Util.option_map v.e_c2_security_group_name
             (fun f ->
                Query.Pair ("EC2SecurityGroupName", (String.to_query f)));
           Util.option_map v.c_i_d_r_i_p
             (fun f -> Query.Pair ("CIDRIP", (String.to_query f)));
           Some
             (Query.Pair
                ("DBSecurityGroupName",
                  (String.to_query v.d_b_security_group_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.e_c2_security_group_owner_id
              (fun f -> ("e_c2_security_group_owner_id", (String.to_json f)));
           Util.option_map v.e_c2_security_group_id
             (fun f -> ("e_c2_security_group_id", (String.to_json f)));
           Util.option_map v.e_c2_security_group_name
             (fun f -> ("e_c2_security_group_name", (String.to_json f)));
           Util.option_map v.c_i_d_r_i_p
             (fun f -> ("c_i_d_r_i_p", (String.to_json f)));
           Some
             ("d_b_security_group_name",
               (String.to_json v.d_b_security_group_name))])
    let of_json j =
      {
        d_b_security_group_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "d_b_security_group_name")));
        c_i_d_r_i_p =
          (Util.option_map (Json.lookup j "c_i_d_r_i_p") String.of_json);
        e_c2_security_group_name =
          (Util.option_map (Json.lookup j "e_c2_security_group_name")
             String.of_json);
        e_c2_security_group_id =
          (Util.option_map (Json.lookup j "e_c2_security_group_id")
             String.of_json);
        e_c2_security_group_owner_id =
          (Util.option_map (Json.lookup j "e_c2_security_group_owner_id")
             String.of_json)
      }
  end
module SubscriptionNotFoundFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DBClusterParameterGroupsMessage =
  struct
    type t =
      {
      marker: String.t option ;
      d_b_cluster_parameter_groups: DBClusterParameterGroupList.t }
    let make ?marker  ?(d_b_cluster_parameter_groups= [])  () =
      { marker; d_b_cluster_parameter_groups }
    let parse xml =
      Some
        {
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          d_b_cluster_parameter_groups =
            (Util.of_option []
               (Util.option_bind (Xml.member "DBClusterParameterGroups" xml)
                  DBClusterParameterGroupList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("DBClusterParameterGroups.member",
                   (DBClusterParameterGroupList.to_query
                      v.d_b_cluster_parameter_groups)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("d_b_cluster_parameter_groups",
                (DBClusterParameterGroupList.to_json
                   v.d_b_cluster_parameter_groups));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)))])
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        d_b_cluster_parameter_groups =
          (DBClusterParameterGroupList.of_json
             (Util.of_option_exn
                (Json.lookup j "d_b_cluster_parameter_groups")))
      }
  end
module ReservedDBInstanceAlreadyExistsFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CopyDBSnapshotMessage =
  struct
    type t =
      {
      source_d_b_snapshot_identifier: String.t ;
      target_d_b_snapshot_identifier: String.t ;
      tags: TagList.t ;
      copy_tags: Boolean.t option }
    let make ~source_d_b_snapshot_identifier  ~target_d_b_snapshot_identifier
       ?(tags= [])  ?copy_tags  () =
      {
        source_d_b_snapshot_identifier;
        target_d_b_snapshot_identifier;
        tags;
        copy_tags
      }
    let parse xml =
      Some
        {
          source_d_b_snapshot_identifier =
            (Xml.required "SourceDBSnapshotIdentifier"
               (Util.option_bind
                  (Xml.member "SourceDBSnapshotIdentifier" xml) String.parse));
          target_d_b_snapshot_identifier =
            (Xml.required "TargetDBSnapshotIdentifier"
               (Util.option_bind
                  (Xml.member "TargetDBSnapshotIdentifier" xml) String.parse));
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) TagList.parse));
          copy_tags =
            (Util.option_bind (Xml.member "CopyTags" xml) Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.copy_tags
              (fun f -> Query.Pair ("CopyTags", (Boolean.to_query f)));
           Some (Query.Pair ("Tags.member", (TagList.to_query v.tags)));
           Some
             (Query.Pair
                ("TargetDBSnapshotIdentifier",
                  (String.to_query v.target_d_b_snapshot_identifier)));
           Some
             (Query.Pair
                ("SourceDBSnapshotIdentifier",
                  (String.to_query v.source_d_b_snapshot_identifier)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.copy_tags
              (fun f -> ("copy_tags", (Boolean.to_json f)));
           Some ("tags", (TagList.to_json v.tags));
           Some
             ("target_d_b_snapshot_identifier",
               (String.to_json v.target_d_b_snapshot_identifier));
           Some
             ("source_d_b_snapshot_identifier",
               (String.to_json v.source_d_b_snapshot_identifier))])
    let of_json j =
      {
        source_d_b_snapshot_identifier =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "source_d_b_snapshot_identifier")));
        target_d_b_snapshot_identifier =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "target_d_b_snapshot_identifier")));
        tags = (TagList.of_json (Util.of_option_exn (Json.lookup j "tags")));
        copy_tags =
          (Util.option_map (Json.lookup j "copy_tags") Boolean.of_json)
      }
  end
module RevokeDBSecurityGroupIngressResult =
  struct
    type t = {
      d_b_security_group: DBSecurityGroup.t option }
    let make ?d_b_security_group  () = { d_b_security_group }
    let parse xml =
      Some
        {
          d_b_security_group =
            (Util.option_bind (Xml.member "DBSecurityGroup" xml)
               DBSecurityGroup.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.d_b_security_group
              (fun f ->
                 Query.Pair ("DBSecurityGroup", (DBSecurityGroup.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.d_b_security_group
              (fun f -> ("d_b_security_group", (DBSecurityGroup.to_json f)))])
    let of_json j =
      {
        d_b_security_group =
          (Util.option_map (Json.lookup j "d_b_security_group")
             DBSecurityGroup.of_json)
      }
  end
module DescribeDBLogFilesResponse =
  struct
    type t =
      {
      describe_d_b_log_files: DescribeDBLogFilesList.t ;
      marker: String.t option }
    let make ?(describe_d_b_log_files= [])  ?marker  () =
      { describe_d_b_log_files; marker }
    let parse xml =
      Some
        {
          describe_d_b_log_files =
            (Util.of_option []
               (Util.option_bind (Xml.member "DescribeDBLogFiles" xml)
                  DescribeDBLogFilesList.parse));
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Some
             (Query.Pair
                ("DescribeDBLogFiles.member",
                  (DescribeDBLogFilesList.to_query v.describe_d_b_log_files)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Some
             ("describe_d_b_log_files",
               (DescribeDBLogFilesList.to_json v.describe_d_b_log_files))])
    let of_json j =
      {
        describe_d_b_log_files =
          (DescribeDBLogFilesList.of_json
             (Util.of_option_exn (Json.lookup j "describe_d_b_log_files")));
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module DescribeEventsMessage =
  struct
    type t =
      {
      source_identifier: String.t option ;
      source_type: SourceType.t option ;
      start_time: DateTime.t option ;
      end_time: DateTime.t option ;
      duration: Integer.t option ;
      event_categories: EventCategoriesList.t ;
      filters: FilterList.t ;
      max_records: Integer.t option ;
      marker: String.t option }
    let make ?source_identifier  ?source_type  ?start_time  ?end_time 
      ?duration  ?(event_categories= [])  ?(filters= [])  ?max_records 
      ?marker  () =
      {
        source_identifier;
        source_type;
        start_time;
        end_time;
        duration;
        event_categories;
        filters;
        max_records;
        marker
      }
    let parse xml =
      Some
        {
          source_identifier =
            (Util.option_bind (Xml.member "SourceIdentifier" xml)
               String.parse);
          source_type =
            (Util.option_bind (Xml.member "SourceType" xml) SourceType.parse);
          start_time =
            (Util.option_bind (Xml.member "StartTime" xml) DateTime.parse);
          end_time =
            (Util.option_bind (Xml.member "EndTime" xml) DateTime.parse);
          duration =
            (Util.option_bind (Xml.member "Duration" xml) Integer.parse);
          event_categories =
            (Util.of_option []
               (Util.option_bind (Xml.member "EventCategories" xml)
                  EventCategoriesList.parse));
          filters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Filters" xml) FilterList.parse));
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Some
             (Query.Pair ("Filters.member", (FilterList.to_query v.filters)));
           Some
             (Query.Pair
                ("EventCategories.member",
                  (EventCategoriesList.to_query v.event_categories)));
           Util.option_map v.duration
             (fun f -> Query.Pair ("Duration", (Integer.to_query f)));
           Util.option_map v.end_time
             (fun f -> Query.Pair ("EndTime", (DateTime.to_query f)));
           Util.option_map v.start_time
             (fun f -> Query.Pair ("StartTime", (DateTime.to_query f)));
           Util.option_map v.source_type
             (fun f -> Query.Pair ("SourceType", (SourceType.to_query f)));
           Util.option_map v.source_identifier
             (fun f -> Query.Pair ("SourceIdentifier", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Some ("filters", (FilterList.to_json v.filters));
           Some
             ("event_categories",
               (EventCategoriesList.to_json v.event_categories));
           Util.option_map v.duration
             (fun f -> ("duration", (Integer.to_json f)));
           Util.option_map v.end_time
             (fun f -> ("end_time", (DateTime.to_json f)));
           Util.option_map v.start_time
             (fun f -> ("start_time", (DateTime.to_json f)));
           Util.option_map v.source_type
             (fun f -> ("source_type", (SourceType.to_json f)));
           Util.option_map v.source_identifier
             (fun f -> ("source_identifier", (String.to_json f)))])
    let of_json j =
      {
        source_identifier =
          (Util.option_map (Json.lookup j "source_identifier") String.of_json);
        source_type =
          (Util.option_map (Json.lookup j "source_type") SourceType.of_json);
        start_time =
          (Util.option_map (Json.lookup j "start_time") DateTime.of_json);
        end_time =
          (Util.option_map (Json.lookup j "end_time") DateTime.of_json);
        duration =
          (Util.option_map (Json.lookup j "duration") Integer.of_json);
        event_categories =
          (EventCategoriesList.of_json
             (Util.of_option_exn (Json.lookup j "event_categories")));
        filters =
          (FilterList.of_json (Util.of_option_exn (Json.lookup j "filters")));
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module InvalidDBClusterStateFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module SNSTopicArnNotFoundFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CertificateNotFoundFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ReservedDBInstanceNotFoundFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module OptionGroupQuotaExceededFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DescribeDBClusterParametersMessage =
  struct
    type t =
      {
      d_b_cluster_parameter_group_name: String.t ;
      source: String.t option ;
      filters: FilterList.t ;
      max_records: Integer.t option ;
      marker: String.t option }
    let make ~d_b_cluster_parameter_group_name  ?source  ?(filters= []) 
      ?max_records  ?marker  () =
      {
        d_b_cluster_parameter_group_name;
        source;
        filters;
        max_records;
        marker
      }
    let parse xml =
      Some
        {
          d_b_cluster_parameter_group_name =
            (Xml.required "DBClusterParameterGroupName"
               (Util.option_bind
                  (Xml.member "DBClusterParameterGroupName" xml) String.parse));
          source = (Util.option_bind (Xml.member "Source" xml) String.parse);
          filters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Filters" xml) FilterList.parse));
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Some
             (Query.Pair ("Filters.member", (FilterList.to_query v.filters)));
           Util.option_map v.source
             (fun f -> Query.Pair ("Source", (String.to_query f)));
           Some
             (Query.Pair
                ("DBClusterParameterGroupName",
                  (String.to_query v.d_b_cluster_parameter_group_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Some ("filters", (FilterList.to_json v.filters));
           Util.option_map v.source (fun f -> ("source", (String.to_json f)));
           Some
             ("d_b_cluster_parameter_group_name",
               (String.to_json v.d_b_cluster_parameter_group_name))])
    let of_json j =
      {
        d_b_cluster_parameter_group_name =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "d_b_cluster_parameter_group_name")));
        source = (Util.option_map (Json.lookup j "source") String.of_json);
        filters =
          (FilterList.of_json (Util.of_option_exn (Json.lookup j "filters")));
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module SnapshotQuotaExceededFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DBClusterNotFoundFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DescribeReservedDBInstancesMessage =
  struct
    type t =
      {
      reserved_d_b_instance_id: String.t option ;
      reserved_d_b_instances_offering_id: String.t option ;
      d_b_instance_class: String.t option ;
      duration: String.t option ;
      product_description: String.t option ;
      offering_type: String.t option ;
      multi_a_z: Boolean.t option ;
      filters: FilterList.t ;
      max_records: Integer.t option ;
      marker: String.t option }
    let make ?reserved_d_b_instance_id  ?reserved_d_b_instances_offering_id 
      ?d_b_instance_class  ?duration  ?product_description  ?offering_type 
      ?multi_a_z  ?(filters= [])  ?max_records  ?marker  () =
      {
        reserved_d_b_instance_id;
        reserved_d_b_instances_offering_id;
        d_b_instance_class;
        duration;
        product_description;
        offering_type;
        multi_a_z;
        filters;
        max_records;
        marker
      }
    let parse xml =
      Some
        {
          reserved_d_b_instance_id =
            (Util.option_bind (Xml.member "ReservedDBInstanceId" xml)
               String.parse);
          reserved_d_b_instances_offering_id =
            (Util.option_bind
               (Xml.member "ReservedDBInstancesOfferingId" xml) String.parse);
          d_b_instance_class =
            (Util.option_bind (Xml.member "DBInstanceClass" xml) String.parse);
          duration =
            (Util.option_bind (Xml.member "Duration" xml) String.parse);
          product_description =
            (Util.option_bind (Xml.member "ProductDescription" xml)
               String.parse);
          offering_type =
            (Util.option_bind (Xml.member "OfferingType" xml) String.parse);
          multi_a_z =
            (Util.option_bind (Xml.member "MultiAZ" xml) Boolean.parse);
          filters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Filters" xml) FilterList.parse));
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Some
             (Query.Pair ("Filters.member", (FilterList.to_query v.filters)));
           Util.option_map v.multi_a_z
             (fun f -> Query.Pair ("MultiAZ", (Boolean.to_query f)));
           Util.option_map v.offering_type
             (fun f -> Query.Pair ("OfferingType", (String.to_query f)));
           Util.option_map v.product_description
             (fun f -> Query.Pair ("ProductDescription", (String.to_query f)));
           Util.option_map v.duration
             (fun f -> Query.Pair ("Duration", (String.to_query f)));
           Util.option_map v.d_b_instance_class
             (fun f -> Query.Pair ("DBInstanceClass", (String.to_query f)));
           Util.option_map v.reserved_d_b_instances_offering_id
             (fun f ->
                Query.Pair
                  ("ReservedDBInstancesOfferingId", (String.to_query f)));
           Util.option_map v.reserved_d_b_instance_id
             (fun f ->
                Query.Pair ("ReservedDBInstanceId", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Some ("filters", (FilterList.to_json v.filters));
           Util.option_map v.multi_a_z
             (fun f -> ("multi_a_z", (Boolean.to_json f)));
           Util.option_map v.offering_type
             (fun f -> ("offering_type", (String.to_json f)));
           Util.option_map v.product_description
             (fun f -> ("product_description", (String.to_json f)));
           Util.option_map v.duration
             (fun f -> ("duration", (String.to_json f)));
           Util.option_map v.d_b_instance_class
             (fun f -> ("d_b_instance_class", (String.to_json f)));
           Util.option_map v.reserved_d_b_instances_offering_id
             (fun f ->
                ("reserved_d_b_instances_offering_id", (String.to_json f)));
           Util.option_map v.reserved_d_b_instance_id
             (fun f -> ("reserved_d_b_instance_id", (String.to_json f)))])
    let of_json j =
      {
        reserved_d_b_instance_id =
          (Util.option_map (Json.lookup j "reserved_d_b_instance_id")
             String.of_json);
        reserved_d_b_instances_offering_id =
          (Util.option_map
             (Json.lookup j "reserved_d_b_instances_offering_id")
             String.of_json);
        d_b_instance_class =
          (Util.option_map (Json.lookup j "d_b_instance_class")
             String.of_json);
        duration =
          (Util.option_map (Json.lookup j "duration") String.of_json);
        product_description =
          (Util.option_map (Json.lookup j "product_description")
             String.of_json);
        offering_type =
          (Util.option_map (Json.lookup j "offering_type") String.of_json);
        multi_a_z =
          (Util.option_map (Json.lookup j "multi_a_z") Boolean.of_json);
        filters =
          (FilterList.of_json (Util.of_option_exn (Json.lookup j "filters")));
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module DescribePendingMaintenanceActionsMessage =
  struct
    type t =
      {
      resource_identifier: String.t option ;
      filters: FilterList.t ;
      marker: String.t option ;
      max_records: Integer.t option }
    let make ?resource_identifier  ?(filters= [])  ?marker  ?max_records  ()
      = { resource_identifier; filters; marker; max_records }
    let parse xml =
      Some
        {
          resource_identifier =
            (Util.option_bind (Xml.member "ResourceIdentifier" xml)
               String.parse);
          filters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Filters" xml) FilterList.parse));
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.max_records
              (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Some
             (Query.Pair ("Filters.member", (FilterList.to_query v.filters)));
           Util.option_map v.resource_identifier
             (fun f -> Query.Pair ("ResourceIdentifier", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.max_records
              (fun f -> ("max_records", (Integer.to_json f)));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)));
           Some ("filters", (FilterList.to_json v.filters));
           Util.option_map v.resource_identifier
             (fun f -> ("resource_identifier", (String.to_json f)))])
    let of_json j =
      {
        resource_identifier =
          (Util.option_map (Json.lookup j "resource_identifier")
             String.of_json);
        filters =
          (FilterList.of_json (Util.of_option_exn (Json.lookup j "filters")));
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json)
      }
  end
module DBClusterParameterGroupDetails =
  struct
    type t = {
      parameters: ParametersList.t ;
      marker: String.t option }
    let make ?(parameters= [])  ?marker  () = { parameters; marker }
    let parse xml =
      Some
        {
          parameters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Parameters" xml)
                  ParametersList.parse));
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Some
             (Query.Pair
                ("Parameters.member", (ParametersList.to_query v.parameters)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Some ("parameters", (ParametersList.to_json v.parameters))])
    let of_json j =
      {
        parameters =
          (ParametersList.of_json
             (Util.of_option_exn (Json.lookup j "parameters")));
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module DescribeDBClusterParameterGroupsMessage =
  struct
    type t =
      {
      d_b_cluster_parameter_group_name: String.t option ;
      filters: FilterList.t ;
      max_records: Integer.t option ;
      marker: String.t option }
    let make ?d_b_cluster_parameter_group_name  ?(filters= [])  ?max_records 
      ?marker  () =
      { d_b_cluster_parameter_group_name; filters; max_records; marker }
    let parse xml =
      Some
        {
          d_b_cluster_parameter_group_name =
            (Util.option_bind (Xml.member "DBClusterParameterGroupName" xml)
               String.parse);
          filters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Filters" xml) FilterList.parse));
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Some
             (Query.Pair ("Filters.member", (FilterList.to_query v.filters)));
           Util.option_map v.d_b_cluster_parameter_group_name
             (fun f ->
                Query.Pair
                  ("DBClusterParameterGroupName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Some ("filters", (FilterList.to_json v.filters));
           Util.option_map v.d_b_cluster_parameter_group_name
             (fun f ->
                ("d_b_cluster_parameter_group_name", (String.to_json f)))])
    let of_json j =
      {
        d_b_cluster_parameter_group_name =
          (Util.option_map (Json.lookup j "d_b_cluster_parameter_group_name")
             String.of_json);
        filters =
          (FilterList.of_json (Util.of_option_exn (Json.lookup j "filters")));
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module PromoteReadReplicaResult =
  struct
    type t = {
      d_b_instance: DBInstance.t option }
    let make ?d_b_instance  () = { d_b_instance }
    let parse xml =
      Some
        {
          d_b_instance =
            (Util.option_bind (Xml.member "DBInstance" xml) DBInstance.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.d_b_instance
              (fun f -> Query.Pair ("DBInstance", (DBInstance.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.d_b_instance
              (fun f -> ("d_b_instance", (DBInstance.to_json f)))])
    let of_json j =
      {
        d_b_instance =
          (Util.option_map (Json.lookup j "d_b_instance") DBInstance.of_json)
      }
  end
module CopyOptionGroupResult =
  struct
    type t = {
      option_group: OptionGroup.t option }
    let make ?option_group  () = { option_group }
    let parse xml =
      Some
        {
          option_group =
            (Util.option_bind (Xml.member "OptionGroup" xml)
               OptionGroup.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.option_group
              (fun f -> Query.Pair ("OptionGroup", (OptionGroup.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.option_group
              (fun f -> ("option_group", (OptionGroup.to_json f)))])
    let of_json j =
      {
        option_group =
          (Util.option_map (Json.lookup j "option_group") OptionGroup.of_json)
      }
  end
module CreateDBInstanceMessage =
  struct
    type t =
      {
      d_b_name: String.t option ;
      d_b_instance_identifier: String.t ;
      allocated_storage: Integer.t option ;
      d_b_instance_class: String.t ;
      engine: String.t ;
      master_username: String.t option ;
      master_user_password: String.t option ;
      d_b_security_groups: DBSecurityGroupNameList.t ;
      vpc_security_group_ids: VpcSecurityGroupIdList.t ;
      availability_zone: String.t option ;
      d_b_subnet_group_name: String.t option ;
      preferred_maintenance_window: String.t option ;
      d_b_parameter_group_name: String.t option ;
      backup_retention_period: Integer.t option ;
      preferred_backup_window: String.t option ;
      port: Integer.t option ;
      multi_a_z: Boolean.t option ;
      engine_version: String.t option ;
      auto_minor_version_upgrade: Boolean.t option ;
      license_model: String.t option ;
      iops: Integer.t option ;
      option_group_name: String.t option ;
      character_set_name: String.t option ;
      publicly_accessible: Boolean.t option ;
      tags: TagList.t ;
      d_b_cluster_identifier: String.t option ;
      storage_type: String.t option ;
      tde_credential_arn: String.t option ;
      tde_credential_password: String.t option ;
      storage_encrypted: Boolean.t option ;
      kms_key_id: String.t option ;
      domain: String.t option ;
      copy_tags_to_snapshot: Boolean.t option }
    let make ?d_b_name  ~d_b_instance_identifier  ?allocated_storage 
      ~d_b_instance_class  ~engine  ?master_username  ?master_user_password 
      ?(d_b_security_groups= [])  ?(vpc_security_group_ids= []) 
      ?availability_zone  ?d_b_subnet_group_name 
      ?preferred_maintenance_window  ?d_b_parameter_group_name 
      ?backup_retention_period  ?preferred_backup_window  ?port  ?multi_a_z 
      ?engine_version  ?auto_minor_version_upgrade  ?license_model  ?iops 
      ?option_group_name  ?character_set_name  ?publicly_accessible  ?(tags=
      [])  ?d_b_cluster_identifier  ?storage_type  ?tde_credential_arn 
      ?tde_credential_password  ?storage_encrypted  ?kms_key_id  ?domain 
      ?copy_tags_to_snapshot  () =
      {
        d_b_name;
        d_b_instance_identifier;
        allocated_storage;
        d_b_instance_class;
        engine;
        master_username;
        master_user_password;
        d_b_security_groups;
        vpc_security_group_ids;
        availability_zone;
        d_b_subnet_group_name;
        preferred_maintenance_window;
        d_b_parameter_group_name;
        backup_retention_period;
        preferred_backup_window;
        port;
        multi_a_z;
        engine_version;
        auto_minor_version_upgrade;
        license_model;
        iops;
        option_group_name;
        character_set_name;
        publicly_accessible;
        tags;
        d_b_cluster_identifier;
        storage_type;
        tde_credential_arn;
        tde_credential_password;
        storage_encrypted;
        kms_key_id;
        domain;
        copy_tags_to_snapshot
      }
    let parse xml =
      Some
        {
          d_b_name =
            (Util.option_bind (Xml.member "DBName" xml) String.parse);
          d_b_instance_identifier =
            (Xml.required "DBInstanceIdentifier"
               (Util.option_bind (Xml.member "DBInstanceIdentifier" xml)
                  String.parse));
          allocated_storage =
            (Util.option_bind (Xml.member "AllocatedStorage" xml)
               Integer.parse);
          d_b_instance_class =
            (Xml.required "DBInstanceClass"
               (Util.option_bind (Xml.member "DBInstanceClass" xml)
                  String.parse));
          engine =
            (Xml.required "Engine"
               (Util.option_bind (Xml.member "Engine" xml) String.parse));
          master_username =
            (Util.option_bind (Xml.member "MasterUsername" xml) String.parse);
          master_user_password =
            (Util.option_bind (Xml.member "MasterUserPassword" xml)
               String.parse);
          d_b_security_groups =
            (Util.of_option []
               (Util.option_bind (Xml.member "DBSecurityGroups" xml)
                  DBSecurityGroupNameList.parse));
          vpc_security_group_ids =
            (Util.of_option []
               (Util.option_bind (Xml.member "VpcSecurityGroupIds" xml)
                  VpcSecurityGroupIdList.parse));
          availability_zone =
            (Util.option_bind (Xml.member "AvailabilityZone" xml)
               String.parse);
          d_b_subnet_group_name =
            (Util.option_bind (Xml.member "DBSubnetGroupName" xml)
               String.parse);
          preferred_maintenance_window =
            (Util.option_bind (Xml.member "PreferredMaintenanceWindow" xml)
               String.parse);
          d_b_parameter_group_name =
            (Util.option_bind (Xml.member "DBParameterGroupName" xml)
               String.parse);
          backup_retention_period =
            (Util.option_bind (Xml.member "BackupRetentionPeriod" xml)
               Integer.parse);
          preferred_backup_window =
            (Util.option_bind (Xml.member "PreferredBackupWindow" xml)
               String.parse);
          port = (Util.option_bind (Xml.member "Port" xml) Integer.parse);
          multi_a_z =
            (Util.option_bind (Xml.member "MultiAZ" xml) Boolean.parse);
          engine_version =
            (Util.option_bind (Xml.member "EngineVersion" xml) String.parse);
          auto_minor_version_upgrade =
            (Util.option_bind (Xml.member "AutoMinorVersionUpgrade" xml)
               Boolean.parse);
          license_model =
            (Util.option_bind (Xml.member "LicenseModel" xml) String.parse);
          iops = (Util.option_bind (Xml.member "Iops" xml) Integer.parse);
          option_group_name =
            (Util.option_bind (Xml.member "OptionGroupName" xml) String.parse);
          character_set_name =
            (Util.option_bind (Xml.member "CharacterSetName" xml)
               String.parse);
          publicly_accessible =
            (Util.option_bind (Xml.member "PubliclyAccessible" xml)
               Boolean.parse);
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) TagList.parse));
          d_b_cluster_identifier =
            (Util.option_bind (Xml.member "DBClusterIdentifier" xml)
               String.parse);
          storage_type =
            (Util.option_bind (Xml.member "StorageType" xml) String.parse);
          tde_credential_arn =
            (Util.option_bind (Xml.member "TdeCredentialArn" xml)
               String.parse);
          tde_credential_password =
            (Util.option_bind (Xml.member "TdeCredentialPassword" xml)
               String.parse);
          storage_encrypted =
            (Util.option_bind (Xml.member "StorageEncrypted" xml)
               Boolean.parse);
          kms_key_id =
            (Util.option_bind (Xml.member "KmsKeyId" xml) String.parse);
          domain = (Util.option_bind (Xml.member "Domain" xml) String.parse);
          copy_tags_to_snapshot =
            (Util.option_bind (Xml.member "CopyTagsToSnapshot" xml)
               Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.copy_tags_to_snapshot
              (fun f ->
                 Query.Pair ("CopyTagsToSnapshot", (Boolean.to_query f)));
           Util.option_map v.domain
             (fun f -> Query.Pair ("Domain", (String.to_query f)));
           Util.option_map v.kms_key_id
             (fun f -> Query.Pair ("KmsKeyId", (String.to_query f)));
           Util.option_map v.storage_encrypted
             (fun f -> Query.Pair ("StorageEncrypted", (Boolean.to_query f)));
           Util.option_map v.tde_credential_password
             (fun f ->
                Query.Pair ("TdeCredentialPassword", (String.to_query f)));
           Util.option_map v.tde_credential_arn
             (fun f -> Query.Pair ("TdeCredentialArn", (String.to_query f)));
           Util.option_map v.storage_type
             (fun f -> Query.Pair ("StorageType", (String.to_query f)));
           Util.option_map v.d_b_cluster_identifier
             (fun f ->
                Query.Pair ("DBClusterIdentifier", (String.to_query f)));
           Some (Query.Pair ("Tags.member", (TagList.to_query v.tags)));
           Util.option_map v.publicly_accessible
             (fun f ->
                Query.Pair ("PubliclyAccessible", (Boolean.to_query f)));
           Util.option_map v.character_set_name
             (fun f -> Query.Pair ("CharacterSetName", (String.to_query f)));
           Util.option_map v.option_group_name
             (fun f -> Query.Pair ("OptionGroupName", (String.to_query f)));
           Util.option_map v.iops
             (fun f -> Query.Pair ("Iops", (Integer.to_query f)));
           Util.option_map v.license_model
             (fun f -> Query.Pair ("LicenseModel", (String.to_query f)));
           Util.option_map v.auto_minor_version_upgrade
             (fun f ->
                Query.Pair ("AutoMinorVersionUpgrade", (Boolean.to_query f)));
           Util.option_map v.engine_version
             (fun f -> Query.Pair ("EngineVersion", (String.to_query f)));
           Util.option_map v.multi_a_z
             (fun f -> Query.Pair ("MultiAZ", (Boolean.to_query f)));
           Util.option_map v.port
             (fun f -> Query.Pair ("Port", (Integer.to_query f)));
           Util.option_map v.preferred_backup_window
             (fun f ->
                Query.Pair ("PreferredBackupWindow", (String.to_query f)));
           Util.option_map v.backup_retention_period
             (fun f ->
                Query.Pair ("BackupRetentionPeriod", (Integer.to_query f)));
           Util.option_map v.d_b_parameter_group_name
             (fun f ->
                Query.Pair ("DBParameterGroupName", (String.to_query f)));
           Util.option_map v.preferred_maintenance_window
             (fun f ->
                Query.Pair
                  ("PreferredMaintenanceWindow", (String.to_query f)));
           Util.option_map v.d_b_subnet_group_name
             (fun f -> Query.Pair ("DBSubnetGroupName", (String.to_query f)));
           Util.option_map v.availability_zone
             (fun f -> Query.Pair ("AvailabilityZone", (String.to_query f)));
           Some
             (Query.Pair
                ("VpcSecurityGroupIds.member",
                  (VpcSecurityGroupIdList.to_query v.vpc_security_group_ids)));
           Some
             (Query.Pair
                ("DBSecurityGroups.member",
                  (DBSecurityGroupNameList.to_query v.d_b_security_groups)));
           Util.option_map v.master_user_password
             (fun f -> Query.Pair ("MasterUserPassword", (String.to_query f)));
           Util.option_map v.master_username
             (fun f -> Query.Pair ("MasterUsername", (String.to_query f)));
           Some (Query.Pair ("Engine", (String.to_query v.engine)));
           Some
             (Query.Pair
                ("DBInstanceClass", (String.to_query v.d_b_instance_class)));
           Util.option_map v.allocated_storage
             (fun f -> Query.Pair ("AllocatedStorage", (Integer.to_query f)));
           Some
             (Query.Pair
                ("DBInstanceIdentifier",
                  (String.to_query v.d_b_instance_identifier)));
           Util.option_map v.d_b_name
             (fun f -> Query.Pair ("DBName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.copy_tags_to_snapshot
              (fun f -> ("copy_tags_to_snapshot", (Boolean.to_json f)));
           Util.option_map v.domain (fun f -> ("domain", (String.to_json f)));
           Util.option_map v.kms_key_id
             (fun f -> ("kms_key_id", (String.to_json f)));
           Util.option_map v.storage_encrypted
             (fun f -> ("storage_encrypted", (Boolean.to_json f)));
           Util.option_map v.tde_credential_password
             (fun f -> ("tde_credential_password", (String.to_json f)));
           Util.option_map v.tde_credential_arn
             (fun f -> ("tde_credential_arn", (String.to_json f)));
           Util.option_map v.storage_type
             (fun f -> ("storage_type", (String.to_json f)));
           Util.option_map v.d_b_cluster_identifier
             (fun f -> ("d_b_cluster_identifier", (String.to_json f)));
           Some ("tags", (TagList.to_json v.tags));
           Util.option_map v.publicly_accessible
             (fun f -> ("publicly_accessible", (Boolean.to_json f)));
           Util.option_map v.character_set_name
             (fun f -> ("character_set_name", (String.to_json f)));
           Util.option_map v.option_group_name
             (fun f -> ("option_group_name", (String.to_json f)));
           Util.option_map v.iops (fun f -> ("iops", (Integer.to_json f)));
           Util.option_map v.license_model
             (fun f -> ("license_model", (String.to_json f)));
           Util.option_map v.auto_minor_version_upgrade
             (fun f -> ("auto_minor_version_upgrade", (Boolean.to_json f)));
           Util.option_map v.engine_version
             (fun f -> ("engine_version", (String.to_json f)));
           Util.option_map v.multi_a_z
             (fun f -> ("multi_a_z", (Boolean.to_json f)));
           Util.option_map v.port (fun f -> ("port", (Integer.to_json f)));
           Util.option_map v.preferred_backup_window
             (fun f -> ("preferred_backup_window", (String.to_json f)));
           Util.option_map v.backup_retention_period
             (fun f -> ("backup_retention_period", (Integer.to_json f)));
           Util.option_map v.d_b_parameter_group_name
             (fun f -> ("d_b_parameter_group_name", (String.to_json f)));
           Util.option_map v.preferred_maintenance_window
             (fun f -> ("preferred_maintenance_window", (String.to_json f)));
           Util.option_map v.d_b_subnet_group_name
             (fun f -> ("d_b_subnet_group_name", (String.to_json f)));
           Util.option_map v.availability_zone
             (fun f -> ("availability_zone", (String.to_json f)));
           Some
             ("vpc_security_group_ids",
               (VpcSecurityGroupIdList.to_json v.vpc_security_group_ids));
           Some
             ("d_b_security_groups",
               (DBSecurityGroupNameList.to_json v.d_b_security_groups));
           Util.option_map v.master_user_password
             (fun f -> ("master_user_password", (String.to_json f)));
           Util.option_map v.master_username
             (fun f -> ("master_username", (String.to_json f)));
           Some ("engine", (String.to_json v.engine));
           Some ("d_b_instance_class", (String.to_json v.d_b_instance_class));
           Util.option_map v.allocated_storage
             (fun f -> ("allocated_storage", (Integer.to_json f)));
           Some
             ("d_b_instance_identifier",
               (String.to_json v.d_b_instance_identifier));
           Util.option_map v.d_b_name
             (fun f -> ("d_b_name", (String.to_json f)))])
    let of_json j =
      {
        d_b_name =
          (Util.option_map (Json.lookup j "d_b_name") String.of_json);
        d_b_instance_identifier =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "d_b_instance_identifier")));
        allocated_storage =
          (Util.option_map (Json.lookup j "allocated_storage")
             Integer.of_json);
        d_b_instance_class =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "d_b_instance_class")));
        engine =
          (String.of_json (Util.of_option_exn (Json.lookup j "engine")));
        master_username =
          (Util.option_map (Json.lookup j "master_username") String.of_json);
        master_user_password =
          (Util.option_map (Json.lookup j "master_user_password")
             String.of_json);
        d_b_security_groups =
          (DBSecurityGroupNameList.of_json
             (Util.of_option_exn (Json.lookup j "d_b_security_groups")));
        vpc_security_group_ids =
          (VpcSecurityGroupIdList.of_json
             (Util.of_option_exn (Json.lookup j "vpc_security_group_ids")));
        availability_zone =
          (Util.option_map (Json.lookup j "availability_zone") String.of_json);
        d_b_subnet_group_name =
          (Util.option_map (Json.lookup j "d_b_subnet_group_name")
             String.of_json);
        preferred_maintenance_window =
          (Util.option_map (Json.lookup j "preferred_maintenance_window")
             String.of_json);
        d_b_parameter_group_name =
          (Util.option_map (Json.lookup j "d_b_parameter_group_name")
             String.of_json);
        backup_retention_period =
          (Util.option_map (Json.lookup j "backup_retention_period")
             Integer.of_json);
        preferred_backup_window =
          (Util.option_map (Json.lookup j "preferred_backup_window")
             String.of_json);
        port = (Util.option_map (Json.lookup j "port") Integer.of_json);
        multi_a_z =
          (Util.option_map (Json.lookup j "multi_a_z") Boolean.of_json);
        engine_version =
          (Util.option_map (Json.lookup j "engine_version") String.of_json);
        auto_minor_version_upgrade =
          (Util.option_map (Json.lookup j "auto_minor_version_upgrade")
             Boolean.of_json);
        license_model =
          (Util.option_map (Json.lookup j "license_model") String.of_json);
        iops = (Util.option_map (Json.lookup j "iops") Integer.of_json);
        option_group_name =
          (Util.option_map (Json.lookup j "option_group_name") String.of_json);
        character_set_name =
          (Util.option_map (Json.lookup j "character_set_name")
             String.of_json);
        publicly_accessible =
          (Util.option_map (Json.lookup j "publicly_accessible")
             Boolean.of_json);
        tags = (TagList.of_json (Util.of_option_exn (Json.lookup j "tags")));
        d_b_cluster_identifier =
          (Util.option_map (Json.lookup j "d_b_cluster_identifier")
             String.of_json);
        storage_type =
          (Util.option_map (Json.lookup j "storage_type") String.of_json);
        tde_credential_arn =
          (Util.option_map (Json.lookup j "tde_credential_arn")
             String.of_json);
        tde_credential_password =
          (Util.option_map (Json.lookup j "tde_credential_password")
             String.of_json);
        storage_encrypted =
          (Util.option_map (Json.lookup j "storage_encrypted")
             Boolean.of_json);
        kms_key_id =
          (Util.option_map (Json.lookup j "kms_key_id") String.of_json);
        domain = (Util.option_map (Json.lookup j "domain") String.of_json);
        copy_tags_to_snapshot =
          (Util.option_map (Json.lookup j "copy_tags_to_snapshot")
             Boolean.of_json)
      }
  end
module RemoveSourceIdentifierFromSubscriptionResult =
  struct
    type t = {
      event_subscription: EventSubscription.t option }
    let make ?event_subscription  () = { event_subscription }
    let parse xml =
      Some
        {
          event_subscription =
            (Util.option_bind (Xml.member "EventSubscription" xml)
               EventSubscription.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.event_subscription
              (fun f ->
                 Query.Pair
                   ("EventSubscription", (EventSubscription.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.event_subscription
              (fun f -> ("event_subscription", (EventSubscription.to_json f)))])
    let of_json j =
      {
        event_subscription =
          (Util.option_map (Json.lookup j "event_subscription")
             EventSubscription.of_json)
      }
  end
module RestoreDBInstanceFromDBSnapshotMessage =
  struct
    type t =
      {
      d_b_instance_identifier: String.t ;
      d_b_snapshot_identifier: String.t ;
      d_b_instance_class: String.t option ;
      port: Integer.t option ;
      availability_zone: String.t option ;
      d_b_subnet_group_name: String.t option ;
      multi_a_z: Boolean.t option ;
      publicly_accessible: Boolean.t option ;
      auto_minor_version_upgrade: Boolean.t option ;
      license_model: String.t option ;
      d_b_name: String.t option ;
      engine: String.t option ;
      iops: Integer.t option ;
      option_group_name: String.t option ;
      tags: TagList.t ;
      storage_type: String.t option ;
      tde_credential_arn: String.t option ;
      tde_credential_password: String.t option ;
      vpc_security_group_ids: VpcSecurityGroupIdList.t ;
      d_b_security_groups: DBSecurityGroupNameList.t ;
      domain: String.t option ;
      copy_tags_to_snapshot: Boolean.t option }
    let make ~d_b_instance_identifier  ~d_b_snapshot_identifier 
      ?d_b_instance_class  ?port  ?availability_zone  ?d_b_subnet_group_name 
      ?multi_a_z  ?publicly_accessible  ?auto_minor_version_upgrade 
      ?license_model  ?d_b_name  ?engine  ?iops  ?option_group_name  ?(tags=
      [])  ?storage_type  ?tde_credential_arn  ?tde_credential_password 
      ?(vpc_security_group_ids= [])  ?(d_b_security_groups= [])  ?domain 
      ?copy_tags_to_snapshot  () =
      {
        d_b_instance_identifier;
        d_b_snapshot_identifier;
        d_b_instance_class;
        port;
        availability_zone;
        d_b_subnet_group_name;
        multi_a_z;
        publicly_accessible;
        auto_minor_version_upgrade;
        license_model;
        d_b_name;
        engine;
        iops;
        option_group_name;
        tags;
        storage_type;
        tde_credential_arn;
        tde_credential_password;
        vpc_security_group_ids;
        d_b_security_groups;
        domain;
        copy_tags_to_snapshot
      }
    let parse xml =
      Some
        {
          d_b_instance_identifier =
            (Xml.required "DBInstanceIdentifier"
               (Util.option_bind (Xml.member "DBInstanceIdentifier" xml)
                  String.parse));
          d_b_snapshot_identifier =
            (Xml.required "DBSnapshotIdentifier"
               (Util.option_bind (Xml.member "DBSnapshotIdentifier" xml)
                  String.parse));
          d_b_instance_class =
            (Util.option_bind (Xml.member "DBInstanceClass" xml) String.parse);
          port = (Util.option_bind (Xml.member "Port" xml) Integer.parse);
          availability_zone =
            (Util.option_bind (Xml.member "AvailabilityZone" xml)
               String.parse);
          d_b_subnet_group_name =
            (Util.option_bind (Xml.member "DBSubnetGroupName" xml)
               String.parse);
          multi_a_z =
            (Util.option_bind (Xml.member "MultiAZ" xml) Boolean.parse);
          publicly_accessible =
            (Util.option_bind (Xml.member "PubliclyAccessible" xml)
               Boolean.parse);
          auto_minor_version_upgrade =
            (Util.option_bind (Xml.member "AutoMinorVersionUpgrade" xml)
               Boolean.parse);
          license_model =
            (Util.option_bind (Xml.member "LicenseModel" xml) String.parse);
          d_b_name =
            (Util.option_bind (Xml.member "DBName" xml) String.parse);
          engine = (Util.option_bind (Xml.member "Engine" xml) String.parse);
          iops = (Util.option_bind (Xml.member "Iops" xml) Integer.parse);
          option_group_name =
            (Util.option_bind (Xml.member "OptionGroupName" xml) String.parse);
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) TagList.parse));
          storage_type =
            (Util.option_bind (Xml.member "StorageType" xml) String.parse);
          tde_credential_arn =
            (Util.option_bind (Xml.member "TdeCredentialArn" xml)
               String.parse);
          tde_credential_password =
            (Util.option_bind (Xml.member "TdeCredentialPassword" xml)
               String.parse);
          vpc_security_group_ids =
            (Util.of_option []
               (Util.option_bind (Xml.member "VpcSecurityGroupIds" xml)
                  VpcSecurityGroupIdList.parse));
          d_b_security_groups =
            (Util.of_option []
               (Util.option_bind (Xml.member "DBSecurityGroups" xml)
                  DBSecurityGroupNameList.parse));
          domain = (Util.option_bind (Xml.member "Domain" xml) String.parse);
          copy_tags_to_snapshot =
            (Util.option_bind (Xml.member "CopyTagsToSnapshot" xml)
               Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.copy_tags_to_snapshot
              (fun f ->
                 Query.Pair ("CopyTagsToSnapshot", (Boolean.to_query f)));
           Util.option_map v.domain
             (fun f -> Query.Pair ("Domain", (String.to_query f)));
           Some
             (Query.Pair
                ("DBSecurityGroups.member",
                  (DBSecurityGroupNameList.to_query v.d_b_security_groups)));
           Some
             (Query.Pair
                ("VpcSecurityGroupIds.member",
                  (VpcSecurityGroupIdList.to_query v.vpc_security_group_ids)));
           Util.option_map v.tde_credential_password
             (fun f ->
                Query.Pair ("TdeCredentialPassword", (String.to_query f)));
           Util.option_map v.tde_credential_arn
             (fun f -> Query.Pair ("TdeCredentialArn", (String.to_query f)));
           Util.option_map v.storage_type
             (fun f -> Query.Pair ("StorageType", (String.to_query f)));
           Some (Query.Pair ("Tags.member", (TagList.to_query v.tags)));
           Util.option_map v.option_group_name
             (fun f -> Query.Pair ("OptionGroupName", (String.to_query f)));
           Util.option_map v.iops
             (fun f -> Query.Pair ("Iops", (Integer.to_query f)));
           Util.option_map v.engine
             (fun f -> Query.Pair ("Engine", (String.to_query f)));
           Util.option_map v.d_b_name
             (fun f -> Query.Pair ("DBName", (String.to_query f)));
           Util.option_map v.license_model
             (fun f -> Query.Pair ("LicenseModel", (String.to_query f)));
           Util.option_map v.auto_minor_version_upgrade
             (fun f ->
                Query.Pair ("AutoMinorVersionUpgrade", (Boolean.to_query f)));
           Util.option_map v.publicly_accessible
             (fun f ->
                Query.Pair ("PubliclyAccessible", (Boolean.to_query f)));
           Util.option_map v.multi_a_z
             (fun f -> Query.Pair ("MultiAZ", (Boolean.to_query f)));
           Util.option_map v.d_b_subnet_group_name
             (fun f -> Query.Pair ("DBSubnetGroupName", (String.to_query f)));
           Util.option_map v.availability_zone
             (fun f -> Query.Pair ("AvailabilityZone", (String.to_query f)));
           Util.option_map v.port
             (fun f -> Query.Pair ("Port", (Integer.to_query f)));
           Util.option_map v.d_b_instance_class
             (fun f -> Query.Pair ("DBInstanceClass", (String.to_query f)));
           Some
             (Query.Pair
                ("DBSnapshotIdentifier",
                  (String.to_query v.d_b_snapshot_identifier)));
           Some
             (Query.Pair
                ("DBInstanceIdentifier",
                  (String.to_query v.d_b_instance_identifier)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.copy_tags_to_snapshot
              (fun f -> ("copy_tags_to_snapshot", (Boolean.to_json f)));
           Util.option_map v.domain (fun f -> ("domain", (String.to_json f)));
           Some
             ("d_b_security_groups",
               (DBSecurityGroupNameList.to_json v.d_b_security_groups));
           Some
             ("vpc_security_group_ids",
               (VpcSecurityGroupIdList.to_json v.vpc_security_group_ids));
           Util.option_map v.tde_credential_password
             (fun f -> ("tde_credential_password", (String.to_json f)));
           Util.option_map v.tde_credential_arn
             (fun f -> ("tde_credential_arn", (String.to_json f)));
           Util.option_map v.storage_type
             (fun f -> ("storage_type", (String.to_json f)));
           Some ("tags", (TagList.to_json v.tags));
           Util.option_map v.option_group_name
             (fun f -> ("option_group_name", (String.to_json f)));
           Util.option_map v.iops (fun f -> ("iops", (Integer.to_json f)));
           Util.option_map v.engine (fun f -> ("engine", (String.to_json f)));
           Util.option_map v.d_b_name
             (fun f -> ("d_b_name", (String.to_json f)));
           Util.option_map v.license_model
             (fun f -> ("license_model", (String.to_json f)));
           Util.option_map v.auto_minor_version_upgrade
             (fun f -> ("auto_minor_version_upgrade", (Boolean.to_json f)));
           Util.option_map v.publicly_accessible
             (fun f -> ("publicly_accessible", (Boolean.to_json f)));
           Util.option_map v.multi_a_z
             (fun f -> ("multi_a_z", (Boolean.to_json f)));
           Util.option_map v.d_b_subnet_group_name
             (fun f -> ("d_b_subnet_group_name", (String.to_json f)));
           Util.option_map v.availability_zone
             (fun f -> ("availability_zone", (String.to_json f)));
           Util.option_map v.port (fun f -> ("port", (Integer.to_json f)));
           Util.option_map v.d_b_instance_class
             (fun f -> ("d_b_instance_class", (String.to_json f)));
           Some
             ("d_b_snapshot_identifier",
               (String.to_json v.d_b_snapshot_identifier));
           Some
             ("d_b_instance_identifier",
               (String.to_json v.d_b_instance_identifier))])
    let of_json j =
      {
        d_b_instance_identifier =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "d_b_instance_identifier")));
        d_b_snapshot_identifier =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "d_b_snapshot_identifier")));
        d_b_instance_class =
          (Util.option_map (Json.lookup j "d_b_instance_class")
             String.of_json);
        port = (Util.option_map (Json.lookup j "port") Integer.of_json);
        availability_zone =
          (Util.option_map (Json.lookup j "availability_zone") String.of_json);
        d_b_subnet_group_name =
          (Util.option_map (Json.lookup j "d_b_subnet_group_name")
             String.of_json);
        multi_a_z =
          (Util.option_map (Json.lookup j "multi_a_z") Boolean.of_json);
        publicly_accessible =
          (Util.option_map (Json.lookup j "publicly_accessible")
             Boolean.of_json);
        auto_minor_version_upgrade =
          (Util.option_map (Json.lookup j "auto_minor_version_upgrade")
             Boolean.of_json);
        license_model =
          (Util.option_map (Json.lookup j "license_model") String.of_json);
        d_b_name =
          (Util.option_map (Json.lookup j "d_b_name") String.of_json);
        engine = (Util.option_map (Json.lookup j "engine") String.of_json);
        iops = (Util.option_map (Json.lookup j "iops") Integer.of_json);
        option_group_name =
          (Util.option_map (Json.lookup j "option_group_name") String.of_json);
        tags = (TagList.of_json (Util.of_option_exn (Json.lookup j "tags")));
        storage_type =
          (Util.option_map (Json.lookup j "storage_type") String.of_json);
        tde_credential_arn =
          (Util.option_map (Json.lookup j "tde_credential_arn")
             String.of_json);
        tde_credential_password =
          (Util.option_map (Json.lookup j "tde_credential_password")
             String.of_json);
        vpc_security_group_ids =
          (VpcSecurityGroupIdList.of_json
             (Util.of_option_exn (Json.lookup j "vpc_security_group_ids")));
        d_b_security_groups =
          (DBSecurityGroupNameList.of_json
             (Util.of_option_exn (Json.lookup j "d_b_security_groups")));
        domain = (Util.option_map (Json.lookup j "domain") String.of_json);
        copy_tags_to_snapshot =
          (Util.option_map (Json.lookup j "copy_tags_to_snapshot")
             Boolean.of_json)
      }
  end
module DBSnapshotNotFoundFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CreateDBSecurityGroupMessage =
  struct
    type t =
      {
      d_b_security_group_name: String.t ;
      d_b_security_group_description: String.t ;
      tags: TagList.t }
    let make ~d_b_security_group_name  ~d_b_security_group_description 
      ?(tags= [])  () =
      { d_b_security_group_name; d_b_security_group_description; tags }
    let parse xml =
      Some
        {
          d_b_security_group_name =
            (Xml.required "DBSecurityGroupName"
               (Util.option_bind (Xml.member "DBSecurityGroupName" xml)
                  String.parse));
          d_b_security_group_description =
            (Xml.required "DBSecurityGroupDescription"
               (Util.option_bind
                  (Xml.member "DBSecurityGroupDescription" xml) String.parse));
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) TagList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Tags.member", (TagList.to_query v.tags)));
           Some
             (Query.Pair
                ("DBSecurityGroupDescription",
                  (String.to_query v.d_b_security_group_description)));
           Some
             (Query.Pair
                ("DBSecurityGroupName",
                  (String.to_query v.d_b_security_group_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tags", (TagList.to_json v.tags));
           Some
             ("d_b_security_group_description",
               (String.to_json v.d_b_security_group_description));
           Some
             ("d_b_security_group_name",
               (String.to_json v.d_b_security_group_name))])
    let of_json j =
      {
        d_b_security_group_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "d_b_security_group_name")));
        d_b_security_group_description =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "d_b_security_group_description")));
        tags = (TagList.of_json (Util.of_option_exn (Json.lookup j "tags")))
      }
  end
module DBClusterAlreadyExistsFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module InvalidRestoreFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module FailoverDBClusterMessage =
  struct
    type t = {
      d_b_cluster_identifier: String.t option }
    let make ?d_b_cluster_identifier  () = { d_b_cluster_identifier }
    let parse xml =
      Some
        {
          d_b_cluster_identifier =
            (Util.option_bind (Xml.member "DBClusterIdentifier" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.d_b_cluster_identifier
              (fun f ->
                 Query.Pair ("DBClusterIdentifier", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.d_b_cluster_identifier
              (fun f -> ("d_b_cluster_identifier", (String.to_json f)))])
    let of_json j =
      {
        d_b_cluster_identifier =
          (Util.option_map (Json.lookup j "d_b_cluster_identifier")
             String.of_json)
      }
  end
module PurchaseReservedDBInstancesOfferingResult =
  struct
    type t = {
      reserved_d_b_instance: ReservedDBInstance.t option }
    let make ?reserved_d_b_instance  () = { reserved_d_b_instance }
    let parse xml =
      Some
        {
          reserved_d_b_instance =
            (Util.option_bind (Xml.member "ReservedDBInstance" xml)
               ReservedDBInstance.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.reserved_d_b_instance
              (fun f ->
                 Query.Pair
                   ("ReservedDBInstance", (ReservedDBInstance.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.reserved_d_b_instance
              (fun f ->
                 ("reserved_d_b_instance", (ReservedDBInstance.to_json f)))])
    let of_json j =
      {
        reserved_d_b_instance =
          (Util.option_map (Json.lookup j "reserved_d_b_instance")
             ReservedDBInstance.of_json)
      }
  end
module RestoreDBInstanceToPointInTimeResult =
  struct
    type t = {
      d_b_instance: DBInstance.t option }
    let make ?d_b_instance  () = { d_b_instance }
    let parse xml =
      Some
        {
          d_b_instance =
            (Util.option_bind (Xml.member "DBInstance" xml) DBInstance.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.d_b_instance
              (fun f -> Query.Pair ("DBInstance", (DBInstance.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.d_b_instance
              (fun f -> ("d_b_instance", (DBInstance.to_json f)))])
    let of_json j =
      {
        d_b_instance =
          (Util.option_map (Json.lookup j "d_b_instance") DBInstance.of_json)
      }
  end
module ApplyPendingMaintenanceActionMessage =
  struct
    type t =
      {
      resource_identifier: String.t ;
      apply_action: String.t ;
      opt_in_type: String.t }
    let make ~resource_identifier  ~apply_action  ~opt_in_type  () =
      { resource_identifier; apply_action; opt_in_type }
    let parse xml =
      Some
        {
          resource_identifier =
            (Xml.required "ResourceIdentifier"
               (Util.option_bind (Xml.member "ResourceIdentifier" xml)
                  String.parse));
          apply_action =
            (Xml.required "ApplyAction"
               (Util.option_bind (Xml.member "ApplyAction" xml) String.parse));
          opt_in_type =
            (Xml.required "OptInType"
               (Util.option_bind (Xml.member "OptInType" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("OptInType", (String.to_query v.opt_in_type)));
           Some
             (Query.Pair ("ApplyAction", (String.to_query v.apply_action)));
           Some
             (Query.Pair
                ("ResourceIdentifier",
                  (String.to_query v.resource_identifier)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("opt_in_type", (String.to_json v.opt_in_type));
           Some ("apply_action", (String.to_json v.apply_action));
           Some
             ("resource_identifier", (String.to_json v.resource_identifier))])
    let of_json j =
      {
        resource_identifier =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "resource_identifier")));
        apply_action =
          (String.of_json (Util.of_option_exn (Json.lookup j "apply_action")));
        opt_in_type =
          (String.of_json (Util.of_option_exn (Json.lookup j "opt_in_type")))
      }
  end
module CreateDBClusterParameterGroupMessage =
  struct
    type t =
      {
      d_b_cluster_parameter_group_name: String.t ;
      d_b_parameter_group_family: String.t ;
      description: String.t ;
      tags: TagList.t }
    let make ~d_b_cluster_parameter_group_name  ~d_b_parameter_group_family 
      ~description  ?(tags= [])  () =
      {
        d_b_cluster_parameter_group_name;
        d_b_parameter_group_family;
        description;
        tags
      }
    let parse xml =
      Some
        {
          d_b_cluster_parameter_group_name =
            (Xml.required "DBClusterParameterGroupName"
               (Util.option_bind
                  (Xml.member "DBClusterParameterGroupName" xml) String.parse));
          d_b_parameter_group_family =
            (Xml.required "DBParameterGroupFamily"
               (Util.option_bind (Xml.member "DBParameterGroupFamily" xml)
                  String.parse));
          description =
            (Xml.required "Description"
               (Util.option_bind (Xml.member "Description" xml) String.parse));
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) TagList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Tags.member", (TagList.to_query v.tags)));
           Some (Query.Pair ("Description", (String.to_query v.description)));
           Some
             (Query.Pair
                ("DBParameterGroupFamily",
                  (String.to_query v.d_b_parameter_group_family)));
           Some
             (Query.Pair
                ("DBClusterParameterGroupName",
                  (String.to_query v.d_b_cluster_parameter_group_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tags", (TagList.to_json v.tags));
           Some ("description", (String.to_json v.description));
           Some
             ("d_b_parameter_group_family",
               (String.to_json v.d_b_parameter_group_family));
           Some
             ("d_b_cluster_parameter_group_name",
               (String.to_json v.d_b_cluster_parameter_group_name))])
    let of_json j =
      {
        d_b_cluster_parameter_group_name =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "d_b_cluster_parameter_group_name")));
        d_b_parameter_group_family =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "d_b_parameter_group_family")));
        description =
          (String.of_json (Util.of_option_exn (Json.lookup j "description")));
        tags = (TagList.of_json (Util.of_option_exn (Json.lookup j "tags")))
      }
  end
module DeleteOptionGroupMessage =
  struct
    type t = {
      option_group_name: String.t }
    let make ~option_group_name  () = { option_group_name }
    let parse xml =
      Some
        {
          option_group_name =
            (Xml.required "OptionGroupName"
               (Util.option_bind (Xml.member "OptionGroupName" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("OptionGroupName", (String.to_query v.option_group_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("option_group_name", (String.to_json v.option_group_name))])
    let of_json j =
      {
        option_group_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "option_group_name")))
      }
  end
module DescribeDBEngineVersionsMessage =
  struct
    type t =
      {
      engine: String.t option ;
      engine_version: String.t option ;
      d_b_parameter_group_family: String.t option ;
      filters: FilterList.t ;
      max_records: Integer.t option ;
      marker: String.t option ;
      default_only: Boolean.t option ;
      list_supported_character_sets: Boolean.t option }
    let make ?engine  ?engine_version  ?d_b_parameter_group_family 
      ?(filters= [])  ?max_records  ?marker  ?default_only 
      ?list_supported_character_sets  () =
      {
        engine;
        engine_version;
        d_b_parameter_group_family;
        filters;
        max_records;
        marker;
        default_only;
        list_supported_character_sets
      }
    let parse xml =
      Some
        {
          engine = (Util.option_bind (Xml.member "Engine" xml) String.parse);
          engine_version =
            (Util.option_bind (Xml.member "EngineVersion" xml) String.parse);
          d_b_parameter_group_family =
            (Util.option_bind (Xml.member "DBParameterGroupFamily" xml)
               String.parse);
          filters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Filters" xml) FilterList.parse));
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          default_only =
            (Util.option_bind (Xml.member "DefaultOnly" xml) Boolean.parse);
          list_supported_character_sets =
            (Util.option_bind (Xml.member "ListSupportedCharacterSets" xml)
               Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.list_supported_character_sets
              (fun f ->
                 Query.Pair
                   ("ListSupportedCharacterSets", (Boolean.to_query f)));
           Util.option_map v.default_only
             (fun f -> Query.Pair ("DefaultOnly", (Boolean.to_query f)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Some
             (Query.Pair ("Filters.member", (FilterList.to_query v.filters)));
           Util.option_map v.d_b_parameter_group_family
             (fun f ->
                Query.Pair ("DBParameterGroupFamily", (String.to_query f)));
           Util.option_map v.engine_version
             (fun f -> Query.Pair ("EngineVersion", (String.to_query f)));
           Util.option_map v.engine
             (fun f -> Query.Pair ("Engine", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.list_supported_character_sets
              (fun f ->
                 ("list_supported_character_sets", (Boolean.to_json f)));
           Util.option_map v.default_only
             (fun f -> ("default_only", (Boolean.to_json f)));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Some ("filters", (FilterList.to_json v.filters));
           Util.option_map v.d_b_parameter_group_family
             (fun f -> ("d_b_parameter_group_family", (String.to_json f)));
           Util.option_map v.engine_version
             (fun f -> ("engine_version", (String.to_json f)));
           Util.option_map v.engine (fun f -> ("engine", (String.to_json f)))])
    let of_json j =
      {
        engine = (Util.option_map (Json.lookup j "engine") String.of_json);
        engine_version =
          (Util.option_map (Json.lookup j "engine_version") String.of_json);
        d_b_parameter_group_family =
          (Util.option_map (Json.lookup j "d_b_parameter_group_family")
             String.of_json);
        filters =
          (FilterList.of_json (Util.of_option_exn (Json.lookup j "filters")));
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        default_only =
          (Util.option_map (Json.lookup j "default_only") Boolean.of_json);
        list_supported_character_sets =
          (Util.option_map (Json.lookup j "list_supported_character_sets")
             Boolean.of_json)
      }
  end
module DBParameterGroupsMessage =
  struct
    type t =
      {
      marker: String.t option ;
      d_b_parameter_groups: DBParameterGroupList.t }
    let make ?marker  ?(d_b_parameter_groups= [])  () =
      { marker; d_b_parameter_groups }
    let parse xml =
      Some
        {
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          d_b_parameter_groups =
            (Util.of_option []
               (Util.option_bind (Xml.member "DBParameterGroups" xml)
                  DBParameterGroupList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("DBParameterGroups.member",
                   (DBParameterGroupList.to_query v.d_b_parameter_groups)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("d_b_parameter_groups",
                (DBParameterGroupList.to_json v.d_b_parameter_groups));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)))])
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        d_b_parameter_groups =
          (DBParameterGroupList.of_json
             (Util.of_option_exn (Json.lookup j "d_b_parameter_groups")))
      }
  end
module CopyDBParameterGroupResult =
  struct
    type t = {
      d_b_parameter_group: DBParameterGroup.t option }
    let make ?d_b_parameter_group  () = { d_b_parameter_group }
    let parse xml =
      Some
        {
          d_b_parameter_group =
            (Util.option_bind (Xml.member "DBParameterGroup" xml)
               DBParameterGroup.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.d_b_parameter_group
              (fun f ->
                 Query.Pair
                   ("DBParameterGroup", (DBParameterGroup.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.d_b_parameter_group
              (fun f -> ("d_b_parameter_group", (DBParameterGroup.to_json f)))])
    let of_json j =
      {
        d_b_parameter_group =
          (Util.option_map (Json.lookup j "d_b_parameter_group")
             DBParameterGroup.of_json)
      }
  end
module DescribeOptionGroupOptionsMessage =
  struct
    type t =
      {
      engine_name: String.t ;
      major_engine_version: String.t option ;
      filters: FilterList.t ;
      max_records: Integer.t option ;
      marker: String.t option }
    let make ~engine_name  ?major_engine_version  ?(filters= []) 
      ?max_records  ?marker  () =
      { engine_name; major_engine_version; filters; max_records; marker }
    let parse xml =
      Some
        {
          engine_name =
            (Xml.required "EngineName"
               (Util.option_bind (Xml.member "EngineName" xml) String.parse));
          major_engine_version =
            (Util.option_bind (Xml.member "MajorEngineVersion" xml)
               String.parse);
          filters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Filters" xml) FilterList.parse));
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Some
             (Query.Pair ("Filters.member", (FilterList.to_query v.filters)));
           Util.option_map v.major_engine_version
             (fun f -> Query.Pair ("MajorEngineVersion", (String.to_query f)));
           Some (Query.Pair ("EngineName", (String.to_query v.engine_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Some ("filters", (FilterList.to_json v.filters));
           Util.option_map v.major_engine_version
             (fun f -> ("major_engine_version", (String.to_json f)));
           Some ("engine_name", (String.to_json v.engine_name))])
    let of_json j =
      {
        engine_name =
          (String.of_json (Util.of_option_exn (Json.lookup j "engine_name")));
        major_engine_version =
          (Util.option_map (Json.lookup j "major_engine_version")
             String.of_json);
        filters =
          (FilterList.of_json (Util.of_option_exn (Json.lookup j "filters")));
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module EventsMessage =
  struct
    type t = {
      marker: String.t option ;
      events: EventList.t }
    let make ?marker  ?(events= [])  () = { marker; events }
    let parse xml =
      Some
        {
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          events =
            (Util.of_option []
               (Util.option_bind (Xml.member "Events" xml) EventList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("Events.member", (EventList.to_query v.events)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("events", (EventList.to_json v.events));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)))])
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        events =
          (EventList.of_json (Util.of_option_exn (Json.lookup j "events")))
      }
  end
module SubscriptionAlreadyExistFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ModifyDBSubnetGroupMessage =
  struct
    type t =
      {
      d_b_subnet_group_name: String.t ;
      d_b_subnet_group_description: String.t option ;
      subnet_ids: SubnetIdentifierList.t }
    let make ~d_b_subnet_group_name  ?d_b_subnet_group_description 
      ~subnet_ids  () =
      { d_b_subnet_group_name; d_b_subnet_group_description; subnet_ids }
    let parse xml =
      Some
        {
          d_b_subnet_group_name =
            (Xml.required "DBSubnetGroupName"
               (Util.option_bind (Xml.member "DBSubnetGroupName" xml)
                  String.parse));
          d_b_subnet_group_description =
            (Util.option_bind (Xml.member "DBSubnetGroupDescription" xml)
               String.parse);
          subnet_ids =
            (Xml.required "SubnetIds"
               (Util.option_bind (Xml.member "SubnetIds" xml)
                  SubnetIdentifierList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("SubnetIds.member",
                   (SubnetIdentifierList.to_query v.subnet_ids)));
           Util.option_map v.d_b_subnet_group_description
             (fun f ->
                Query.Pair ("DBSubnetGroupDescription", (String.to_query f)));
           Some
             (Query.Pair
                ("DBSubnetGroupName",
                  (String.to_query v.d_b_subnet_group_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("subnet_ids", (SubnetIdentifierList.to_json v.subnet_ids));
           Util.option_map v.d_b_subnet_group_description
             (fun f -> ("d_b_subnet_group_description", (String.to_json f)));
           Some
             ("d_b_subnet_group_name",
               (String.to_json v.d_b_subnet_group_name))])
    let of_json j =
      {
        d_b_subnet_group_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "d_b_subnet_group_name")));
        d_b_subnet_group_description =
          (Util.option_map (Json.lookup j "d_b_subnet_group_description")
             String.of_json);
        subnet_ids =
          (SubnetIdentifierList.of_json
             (Util.of_option_exn (Json.lookup j "subnet_ids")))
      }
  end
module CertificateMessage =
  struct
    type t = {
      certificates: CertificateList.t ;
      marker: String.t option }
    let make ?(certificates= [])  ?marker  () = { certificates; marker }
    let parse xml =
      Some
        {
          certificates =
            (Util.of_option []
               (Util.option_bind (Xml.member "Certificates" xml)
                  CertificateList.parse));
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Some
             (Query.Pair
                ("Certificates.member",
                  (CertificateList.to_query v.certificates)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Some ("certificates", (CertificateList.to_json v.certificates))])
    let of_json j =
      {
        certificates =
          (CertificateList.of_json
             (Util.of_option_exn (Json.lookup j "certificates")));
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module CreateDBInstanceReadReplicaMessage =
  struct
    type t =
      {
      d_b_instance_identifier: String.t ;
      source_d_b_instance_identifier: String.t ;
      d_b_instance_class: String.t option ;
      availability_zone: String.t option ;
      port: Integer.t option ;
      auto_minor_version_upgrade: Boolean.t option ;
      iops: Integer.t option ;
      option_group_name: String.t option ;
      publicly_accessible: Boolean.t option ;
      tags: TagList.t ;
      d_b_subnet_group_name: String.t option ;
      storage_type: String.t option ;
      copy_tags_to_snapshot: Boolean.t option }
    let make ~d_b_instance_identifier  ~source_d_b_instance_identifier 
      ?d_b_instance_class  ?availability_zone  ?port 
      ?auto_minor_version_upgrade  ?iops  ?option_group_name 
      ?publicly_accessible  ?(tags= [])  ?d_b_subnet_group_name 
      ?storage_type  ?copy_tags_to_snapshot  () =
      {
        d_b_instance_identifier;
        source_d_b_instance_identifier;
        d_b_instance_class;
        availability_zone;
        port;
        auto_minor_version_upgrade;
        iops;
        option_group_name;
        publicly_accessible;
        tags;
        d_b_subnet_group_name;
        storage_type;
        copy_tags_to_snapshot
      }
    let parse xml =
      Some
        {
          d_b_instance_identifier =
            (Xml.required "DBInstanceIdentifier"
               (Util.option_bind (Xml.member "DBInstanceIdentifier" xml)
                  String.parse));
          source_d_b_instance_identifier =
            (Xml.required "SourceDBInstanceIdentifier"
               (Util.option_bind
                  (Xml.member "SourceDBInstanceIdentifier" xml) String.parse));
          d_b_instance_class =
            (Util.option_bind (Xml.member "DBInstanceClass" xml) String.parse);
          availability_zone =
            (Util.option_bind (Xml.member "AvailabilityZone" xml)
               String.parse);
          port = (Util.option_bind (Xml.member "Port" xml) Integer.parse);
          auto_minor_version_upgrade =
            (Util.option_bind (Xml.member "AutoMinorVersionUpgrade" xml)
               Boolean.parse);
          iops = (Util.option_bind (Xml.member "Iops" xml) Integer.parse);
          option_group_name =
            (Util.option_bind (Xml.member "OptionGroupName" xml) String.parse);
          publicly_accessible =
            (Util.option_bind (Xml.member "PubliclyAccessible" xml)
               Boolean.parse);
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) TagList.parse));
          d_b_subnet_group_name =
            (Util.option_bind (Xml.member "DBSubnetGroupName" xml)
               String.parse);
          storage_type =
            (Util.option_bind (Xml.member "StorageType" xml) String.parse);
          copy_tags_to_snapshot =
            (Util.option_bind (Xml.member "CopyTagsToSnapshot" xml)
               Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.copy_tags_to_snapshot
              (fun f ->
                 Query.Pair ("CopyTagsToSnapshot", (Boolean.to_query f)));
           Util.option_map v.storage_type
             (fun f -> Query.Pair ("StorageType", (String.to_query f)));
           Util.option_map v.d_b_subnet_group_name
             (fun f -> Query.Pair ("DBSubnetGroupName", (String.to_query f)));
           Some (Query.Pair ("Tags.member", (TagList.to_query v.tags)));
           Util.option_map v.publicly_accessible
             (fun f ->
                Query.Pair ("PubliclyAccessible", (Boolean.to_query f)));
           Util.option_map v.option_group_name
             (fun f -> Query.Pair ("OptionGroupName", (String.to_query f)));
           Util.option_map v.iops
             (fun f -> Query.Pair ("Iops", (Integer.to_query f)));
           Util.option_map v.auto_minor_version_upgrade
             (fun f ->
                Query.Pair ("AutoMinorVersionUpgrade", (Boolean.to_query f)));
           Util.option_map v.port
             (fun f -> Query.Pair ("Port", (Integer.to_query f)));
           Util.option_map v.availability_zone
             (fun f -> Query.Pair ("AvailabilityZone", (String.to_query f)));
           Util.option_map v.d_b_instance_class
             (fun f -> Query.Pair ("DBInstanceClass", (String.to_query f)));
           Some
             (Query.Pair
                ("SourceDBInstanceIdentifier",
                  (String.to_query v.source_d_b_instance_identifier)));
           Some
             (Query.Pair
                ("DBInstanceIdentifier",
                  (String.to_query v.d_b_instance_identifier)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.copy_tags_to_snapshot
              (fun f -> ("copy_tags_to_snapshot", (Boolean.to_json f)));
           Util.option_map v.storage_type
             (fun f -> ("storage_type", (String.to_json f)));
           Util.option_map v.d_b_subnet_group_name
             (fun f -> ("d_b_subnet_group_name", (String.to_json f)));
           Some ("tags", (TagList.to_json v.tags));
           Util.option_map v.publicly_accessible
             (fun f -> ("publicly_accessible", (Boolean.to_json f)));
           Util.option_map v.option_group_name
             (fun f -> ("option_group_name", (String.to_json f)));
           Util.option_map v.iops (fun f -> ("iops", (Integer.to_json f)));
           Util.option_map v.auto_minor_version_upgrade
             (fun f -> ("auto_minor_version_upgrade", (Boolean.to_json f)));
           Util.option_map v.port (fun f -> ("port", (Integer.to_json f)));
           Util.option_map v.availability_zone
             (fun f -> ("availability_zone", (String.to_json f)));
           Util.option_map v.d_b_instance_class
             (fun f -> ("d_b_instance_class", (String.to_json f)));
           Some
             ("source_d_b_instance_identifier",
               (String.to_json v.source_d_b_instance_identifier));
           Some
             ("d_b_instance_identifier",
               (String.to_json v.d_b_instance_identifier))])
    let of_json j =
      {
        d_b_instance_identifier =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "d_b_instance_identifier")));
        source_d_b_instance_identifier =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "source_d_b_instance_identifier")));
        d_b_instance_class =
          (Util.option_map (Json.lookup j "d_b_instance_class")
             String.of_json);
        availability_zone =
          (Util.option_map (Json.lookup j "availability_zone") String.of_json);
        port = (Util.option_map (Json.lookup j "port") Integer.of_json);
        auto_minor_version_upgrade =
          (Util.option_map (Json.lookup j "auto_minor_version_upgrade")
             Boolean.of_json);
        iops = (Util.option_map (Json.lookup j "iops") Integer.of_json);
        option_group_name =
          (Util.option_map (Json.lookup j "option_group_name") String.of_json);
        publicly_accessible =
          (Util.option_map (Json.lookup j "publicly_accessible")
             Boolean.of_json);
        tags = (TagList.of_json (Util.of_option_exn (Json.lookup j "tags")));
        d_b_subnet_group_name =
          (Util.option_map (Json.lookup j "d_b_subnet_group_name")
             String.of_json);
        storage_type =
          (Util.option_map (Json.lookup j "storage_type") String.of_json);
        copy_tags_to_snapshot =
          (Util.option_map (Json.lookup j "copy_tags_to_snapshot")
             Boolean.of_json)
      }
  end
module AuthorizeDBSecurityGroupIngressResult =
  struct
    type t = {
      d_b_security_group: DBSecurityGroup.t option }
    let make ?d_b_security_group  () = { d_b_security_group }
    let parse xml =
      Some
        {
          d_b_security_group =
            (Util.option_bind (Xml.member "DBSecurityGroup" xml)
               DBSecurityGroup.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.d_b_security_group
              (fun f ->
                 Query.Pair ("DBSecurityGroup", (DBSecurityGroup.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.d_b_security_group
              (fun f -> ("d_b_security_group", (DBSecurityGroup.to_json f)))])
    let of_json j =
      {
        d_b_security_group =
          (Util.option_map (Json.lookup j "d_b_security_group")
             DBSecurityGroup.of_json)
      }
  end
module SourceNotFoundFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module OptionGroups =
  struct
    type t =
      {
      option_groups_list: OptionGroupsList.t ;
      marker: String.t option }
    let make ?(option_groups_list= [])  ?marker  () =
      { option_groups_list; marker }
    let parse xml =
      Some
        {
          option_groups_list =
            (Util.of_option []
               (Util.option_bind (Xml.member "OptionGroupsList" xml)
                  OptionGroupsList.parse));
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Some
             (Query.Pair
                ("OptionGroupsList.member",
                  (OptionGroupsList.to_query v.option_groups_list)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Some
             ("option_groups_list",
               (OptionGroupsList.to_json v.option_groups_list))])
    let of_json j =
      {
        option_groups_list =
          (OptionGroupsList.of_json
             (Util.of_option_exn (Json.lookup j "option_groups_list")));
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module DBSecurityGroupNotFoundFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DeleteDBSnapshotResult =
  struct
    type t = {
      d_b_snapshot: DBSnapshot.t option }
    let make ?d_b_snapshot  () = { d_b_snapshot }
    let parse xml =
      Some
        {
          d_b_snapshot =
            (Util.option_bind (Xml.member "DBSnapshot" xml) DBSnapshot.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.d_b_snapshot
              (fun f -> Query.Pair ("DBSnapshot", (DBSnapshot.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.d_b_snapshot
              (fun f -> ("d_b_snapshot", (DBSnapshot.to_json f)))])
    let of_json j =
      {
        d_b_snapshot =
          (Util.option_map (Json.lookup j "d_b_snapshot") DBSnapshot.of_json)
      }
  end
module DescribeDBParameterGroupsMessage =
  struct
    type t =
      {
      d_b_parameter_group_name: String.t option ;
      filters: FilterList.t ;
      max_records: Integer.t option ;
      marker: String.t option }
    let make ?d_b_parameter_group_name  ?(filters= [])  ?max_records  ?marker
       () = { d_b_parameter_group_name; filters; max_records; marker }
    let parse xml =
      Some
        {
          d_b_parameter_group_name =
            (Util.option_bind (Xml.member "DBParameterGroupName" xml)
               String.parse);
          filters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Filters" xml) FilterList.parse));
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Some
             (Query.Pair ("Filters.member", (FilterList.to_query v.filters)));
           Util.option_map v.d_b_parameter_group_name
             (fun f ->
                Query.Pair ("DBParameterGroupName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Some ("filters", (FilterList.to_json v.filters));
           Util.option_map v.d_b_parameter_group_name
             (fun f -> ("d_b_parameter_group_name", (String.to_json f)))])
    let of_json j =
      {
        d_b_parameter_group_name =
          (Util.option_map (Json.lookup j "d_b_parameter_group_name")
             String.of_json);
        filters =
          (FilterList.of_json (Util.of_option_exn (Json.lookup j "filters")));
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module InsufficientDBClusterCapacityFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module SNSInvalidTopicFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DBSubnetGroupAlreadyExistsFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DeleteDBClusterParameterGroupMessage =
  struct
    type t = {
      d_b_cluster_parameter_group_name: String.t }
    let make ~d_b_cluster_parameter_group_name  () =
      { d_b_cluster_parameter_group_name }
    let parse xml =
      Some
        {
          d_b_cluster_parameter_group_name =
            (Xml.required "DBClusterParameterGroupName"
               (Util.option_bind
                  (Xml.member "DBClusterParameterGroupName" xml) String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("DBClusterParameterGroupName",
                   (String.to_query v.d_b_cluster_parameter_group_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("d_b_cluster_parameter_group_name",
                (String.to_json v.d_b_cluster_parameter_group_name))])
    let of_json j =
      {
        d_b_cluster_parameter_group_name =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "d_b_cluster_parameter_group_name")))
      }
  end
module OrderableDBInstanceOptionsMessage =
  struct
    type t =
      {
      orderable_d_b_instance_options: OrderableDBInstanceOptionsList.t ;
      marker: String.t option }
    let make ?(orderable_d_b_instance_options= [])  ?marker  () =
      { orderable_d_b_instance_options; marker }
    let parse xml =
      Some
        {
          orderable_d_b_instance_options =
            (Util.of_option []
               (Util.option_bind
                  (Xml.member "OrderableDBInstanceOptions" xml)
                  OrderableDBInstanceOptionsList.parse));
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Some
             (Query.Pair
                ("OrderableDBInstanceOptions.member",
                  (OrderableDBInstanceOptionsList.to_query
                     v.orderable_d_b_instance_options)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Some
             ("orderable_d_b_instance_options",
               (OrderableDBInstanceOptionsList.to_json
                  v.orderable_d_b_instance_options))])
    let of_json j =
      {
        orderable_d_b_instance_options =
          (OrderableDBInstanceOptionsList.of_json
             (Util.of_option_exn
                (Json.lookup j "orderable_d_b_instance_options")));
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module DeleteDBInstanceResult =
  struct
    type t = {
      d_b_instance: DBInstance.t option }
    let make ?d_b_instance  () = { d_b_instance }
    let parse xml =
      Some
        {
          d_b_instance =
            (Util.option_bind (Xml.member "DBInstance" xml) DBInstance.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.d_b_instance
              (fun f -> Query.Pair ("DBInstance", (DBInstance.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.d_b_instance
              (fun f -> ("d_b_instance", (DBInstance.to_json f)))])
    let of_json j =
      {
        d_b_instance =
          (Util.option_map (Json.lookup j "d_b_instance") DBInstance.of_json)
      }
  end
module DescribeDBClusterSnapshotsMessage =
  struct
    type t =
      {
      d_b_cluster_identifier: String.t option ;
      d_b_cluster_snapshot_identifier: String.t option ;
      snapshot_type: String.t option ;
      filters: FilterList.t ;
      max_records: Integer.t option ;
      marker: String.t option }
    let make ?d_b_cluster_identifier  ?d_b_cluster_snapshot_identifier 
      ?snapshot_type  ?(filters= [])  ?max_records  ?marker  () =
      {
        d_b_cluster_identifier;
        d_b_cluster_snapshot_identifier;
        snapshot_type;
        filters;
        max_records;
        marker
      }
    let parse xml =
      Some
        {
          d_b_cluster_identifier =
            (Util.option_bind (Xml.member "DBClusterIdentifier" xml)
               String.parse);
          d_b_cluster_snapshot_identifier =
            (Util.option_bind (Xml.member "DBClusterSnapshotIdentifier" xml)
               String.parse);
          snapshot_type =
            (Util.option_bind (Xml.member "SnapshotType" xml) String.parse);
          filters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Filters" xml) FilterList.parse));
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Some
             (Query.Pair ("Filters.member", (FilterList.to_query v.filters)));
           Util.option_map v.snapshot_type
             (fun f -> Query.Pair ("SnapshotType", (String.to_query f)));
           Util.option_map v.d_b_cluster_snapshot_identifier
             (fun f ->
                Query.Pair
                  ("DBClusterSnapshotIdentifier", (String.to_query f)));
           Util.option_map v.d_b_cluster_identifier
             (fun f ->
                Query.Pair ("DBClusterIdentifier", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Some ("filters", (FilterList.to_json v.filters));
           Util.option_map v.snapshot_type
             (fun f -> ("snapshot_type", (String.to_json f)));
           Util.option_map v.d_b_cluster_snapshot_identifier
             (fun f ->
                ("d_b_cluster_snapshot_identifier", (String.to_json f)));
           Util.option_map v.d_b_cluster_identifier
             (fun f -> ("d_b_cluster_identifier", (String.to_json f)))])
    let of_json j =
      {
        d_b_cluster_identifier =
          (Util.option_map (Json.lookup j "d_b_cluster_identifier")
             String.of_json);
        d_b_cluster_snapshot_identifier =
          (Util.option_map (Json.lookup j "d_b_cluster_snapshot_identifier")
             String.of_json);
        snapshot_type =
          (Util.option_map (Json.lookup j "snapshot_type") String.of_json);
        filters =
          (FilterList.of_json (Util.of_option_exn (Json.lookup j "filters")));
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module ModifyEventSubscriptionMessage =
  struct
    type t =
      {
      subscription_name: String.t ;
      sns_topic_arn: String.t option ;
      source_type: String.t option ;
      event_categories: EventCategoriesList.t ;
      enabled: Boolean.t option }
    let make ~subscription_name  ?sns_topic_arn  ?source_type 
      ?(event_categories= [])  ?enabled  () =
      {
        subscription_name;
        sns_topic_arn;
        source_type;
        event_categories;
        enabled
      }
    let parse xml =
      Some
        {
          subscription_name =
            (Xml.required "SubscriptionName"
               (Util.option_bind (Xml.member "SubscriptionName" xml)
                  String.parse));
          sns_topic_arn =
            (Util.option_bind (Xml.member "SnsTopicArn" xml) String.parse);
          source_type =
            (Util.option_bind (Xml.member "SourceType" xml) String.parse);
          event_categories =
            (Util.of_option []
               (Util.option_bind (Xml.member "EventCategories" xml)
                  EventCategoriesList.parse));
          enabled =
            (Util.option_bind (Xml.member "Enabled" xml) Boolean.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.enabled
              (fun f -> Query.Pair ("Enabled", (Boolean.to_query f)));
           Some
             (Query.Pair
                ("EventCategories.member",
                  (EventCategoriesList.to_query v.event_categories)));
           Util.option_map v.source_type
             (fun f -> Query.Pair ("SourceType", (String.to_query f)));
           Util.option_map v.sns_topic_arn
             (fun f -> Query.Pair ("SnsTopicArn", (String.to_query f)));
           Some
             (Query.Pair
                ("SubscriptionName", (String.to_query v.subscription_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.enabled
              (fun f -> ("enabled", (Boolean.to_json f)));
           Some
             ("event_categories",
               (EventCategoriesList.to_json v.event_categories));
           Util.option_map v.source_type
             (fun f -> ("source_type", (String.to_json f)));
           Util.option_map v.sns_topic_arn
             (fun f -> ("sns_topic_arn", (String.to_json f)));
           Some ("subscription_name", (String.to_json v.subscription_name))])
    let of_json j =
      {
        subscription_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "subscription_name")));
        sns_topic_arn =
          (Util.option_map (Json.lookup j "sns_topic_arn") String.of_json);
        source_type =
          (Util.option_map (Json.lookup j "source_type") String.of_json);
        event_categories =
          (EventCategoriesList.of_json
             (Util.of_option_exn (Json.lookup j "event_categories")));
        enabled = (Util.option_map (Json.lookup j "enabled") Boolean.of_json)
      }
  end
module RemoveTagsFromResourceMessage =
  struct
    type t = {
      resource_name: String.t ;
      tag_keys: KeyList.t }
    let make ~resource_name  ~tag_keys  () = { resource_name; tag_keys }
    let parse xml =
      Some
        {
          resource_name =
            (Xml.required "ResourceName"
               (Util.option_bind (Xml.member "ResourceName" xml) String.parse));
          tag_keys =
            (Xml.required "TagKeys"
               (Util.option_bind (Xml.member "TagKeys" xml) KeyList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("TagKeys.member", (KeyList.to_query v.tag_keys)));
           Some
             (Query.Pair ("ResourceName", (String.to_query v.resource_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tag_keys", (KeyList.to_json v.tag_keys));
           Some ("resource_name", (String.to_json v.resource_name))])
    let of_json j =
      {
        resource_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "resource_name")));
        tag_keys =
          (KeyList.of_json (Util.of_option_exn (Json.lookup j "tag_keys")))
      }
  end
module PurchaseReservedDBInstancesOfferingMessage =
  struct
    type t =
      {
      reserved_d_b_instances_offering_id: String.t ;
      reserved_d_b_instance_id: String.t option ;
      d_b_instance_count: Integer.t option ;
      tags: TagList.t }
    let make ~reserved_d_b_instances_offering_id  ?reserved_d_b_instance_id 
      ?d_b_instance_count  ?(tags= [])  () =
      {
        reserved_d_b_instances_offering_id;
        reserved_d_b_instance_id;
        d_b_instance_count;
        tags
      }
    let parse xml =
      Some
        {
          reserved_d_b_instances_offering_id =
            (Xml.required "ReservedDBInstancesOfferingId"
               (Util.option_bind
                  (Xml.member "ReservedDBInstancesOfferingId" xml)
                  String.parse));
          reserved_d_b_instance_id =
            (Util.option_bind (Xml.member "ReservedDBInstanceId" xml)
               String.parse);
          d_b_instance_count =
            (Util.option_bind (Xml.member "DBInstanceCount" xml)
               Integer.parse);
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) TagList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Tags.member", (TagList.to_query v.tags)));
           Util.option_map v.d_b_instance_count
             (fun f -> Query.Pair ("DBInstanceCount", (Integer.to_query f)));
           Util.option_map v.reserved_d_b_instance_id
             (fun f ->
                Query.Pair ("ReservedDBInstanceId", (String.to_query f)));
           Some
             (Query.Pair
                ("ReservedDBInstancesOfferingId",
                  (String.to_query v.reserved_d_b_instances_offering_id)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tags", (TagList.to_json v.tags));
           Util.option_map v.d_b_instance_count
             (fun f -> ("d_b_instance_count", (Integer.to_json f)));
           Util.option_map v.reserved_d_b_instance_id
             (fun f -> ("reserved_d_b_instance_id", (String.to_json f)));
           Some
             ("reserved_d_b_instances_offering_id",
               (String.to_json v.reserved_d_b_instances_offering_id))])
    let of_json j =
      {
        reserved_d_b_instances_offering_id =
          (String.of_json
             (Util.of_option_exn
                (Json.lookup j "reserved_d_b_instances_offering_id")));
        reserved_d_b_instance_id =
          (Util.option_map (Json.lookup j "reserved_d_b_instance_id")
             String.of_json);
        d_b_instance_count =
          (Util.option_map (Json.lookup j "d_b_instance_count")
             Integer.of_json);
        tags = (TagList.of_json (Util.of_option_exn (Json.lookup j "tags")))
      }
  end
module KMSKeyNotAccessibleFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DescribeEventCategoriesMessage =
  struct
    type t = {
      source_type: String.t option ;
      filters: FilterList.t }
    let make ?source_type  ?(filters= [])  () = { source_type; filters }
    let parse xml =
      Some
        {
          source_type =
            (Util.option_bind (Xml.member "SourceType" xml) String.parse);
          filters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Filters" xml) FilterList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("Filters.member", (FilterList.to_query v.filters)));
           Util.option_map v.source_type
             (fun f -> Query.Pair ("SourceType", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("filters", (FilterList.to_json v.filters));
           Util.option_map v.source_type
             (fun f -> ("source_type", (String.to_json f)))])
    let of_json j =
      {
        source_type =
          (Util.option_map (Json.lookup j "source_type") String.of_json);
        filters =
          (FilterList.of_json (Util.of_option_exn (Json.lookup j "filters")))
      }
  end
module CreateDBParameterGroupMessage =
  struct
    type t =
      {
      d_b_parameter_group_name: String.t ;
      d_b_parameter_group_family: String.t ;
      description: String.t ;
      tags: TagList.t }
    let make ~d_b_parameter_group_name  ~d_b_parameter_group_family 
      ~description  ?(tags= [])  () =
      {
        d_b_parameter_group_name;
        d_b_parameter_group_family;
        description;
        tags
      }
    let parse xml =
      Some
        {
          d_b_parameter_group_name =
            (Xml.required "DBParameterGroupName"
               (Util.option_bind (Xml.member "DBParameterGroupName" xml)
                  String.parse));
          d_b_parameter_group_family =
            (Xml.required "DBParameterGroupFamily"
               (Util.option_bind (Xml.member "DBParameterGroupFamily" xml)
                  String.parse));
          description =
            (Xml.required "Description"
               (Util.option_bind (Xml.member "Description" xml) String.parse));
          tags =
            (Util.of_option []
               (Util.option_bind (Xml.member "Tags" xml) TagList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some (Query.Pair ("Tags.member", (TagList.to_query v.tags)));
           Some (Query.Pair ("Description", (String.to_query v.description)));
           Some
             (Query.Pair
                ("DBParameterGroupFamily",
                  (String.to_query v.d_b_parameter_group_family)));
           Some
             (Query.Pair
                ("DBParameterGroupName",
                  (String.to_query v.d_b_parameter_group_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("tags", (TagList.to_json v.tags));
           Some ("description", (String.to_json v.description));
           Some
             ("d_b_parameter_group_family",
               (String.to_json v.d_b_parameter_group_family));
           Some
             ("d_b_parameter_group_name",
               (String.to_json v.d_b_parameter_group_name))])
    let of_json j =
      {
        d_b_parameter_group_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "d_b_parameter_group_name")));
        d_b_parameter_group_family =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "d_b_parameter_group_family")));
        description =
          (String.of_json (Util.of_option_exn (Json.lookup j "description")));
        tags = (TagList.of_json (Util.of_option_exn (Json.lookup j "tags")))
      }
  end
module AuthorizationQuotaExceededFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DescribeDBParametersMessage =
  struct
    type t =
      {
      d_b_parameter_group_name: String.t ;
      source: String.t option ;
      filters: FilterList.t ;
      max_records: Integer.t option ;
      marker: String.t option }
    let make ~d_b_parameter_group_name  ?source  ?(filters= [])  ?max_records
       ?marker  () =
      { d_b_parameter_group_name; source; filters; max_records; marker }
    let parse xml =
      Some
        {
          d_b_parameter_group_name =
            (Xml.required "DBParameterGroupName"
               (Util.option_bind (Xml.member "DBParameterGroupName" xml)
                  String.parse));
          source = (Util.option_bind (Xml.member "Source" xml) String.parse);
          filters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Filters" xml) FilterList.parse));
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Some
             (Query.Pair ("Filters.member", (FilterList.to_query v.filters)));
           Util.option_map v.source
             (fun f -> Query.Pair ("Source", (String.to_query f)));
           Some
             (Query.Pair
                ("DBParameterGroupName",
                  (String.to_query v.d_b_parameter_group_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Some ("filters", (FilterList.to_json v.filters));
           Util.option_map v.source (fun f -> ("source", (String.to_json f)));
           Some
             ("d_b_parameter_group_name",
               (String.to_json v.d_b_parameter_group_name))])
    let of_json j =
      {
        d_b_parameter_group_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "d_b_parameter_group_name")));
        source = (Util.option_map (Json.lookup j "source") String.of_json);
        filters =
          (FilterList.of_json (Util.of_option_exn (Json.lookup j "filters")));
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module DBParameterGroupNameMessage =
  struct
    type t = {
      d_b_parameter_group_name: String.t option }
    let make ?d_b_parameter_group_name  () = { d_b_parameter_group_name }
    let parse xml =
      Some
        {
          d_b_parameter_group_name =
            (Util.option_bind (Xml.member "DBParameterGroupName" xml)
               String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.d_b_parameter_group_name
              (fun f ->
                 Query.Pair ("DBParameterGroupName", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.d_b_parameter_group_name
              (fun f -> ("d_b_parameter_group_name", (String.to_json f)))])
    let of_json j =
      {
        d_b_parameter_group_name =
          (Util.option_map (Json.lookup j "d_b_parameter_group_name")
             String.of_json)
      }
  end
module DBSubnetGroupNotFoundFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module DescribeOrderableDBInstanceOptionsMessage =
  struct
    type t =
      {
      engine: String.t ;
      engine_version: String.t option ;
      d_b_instance_class: String.t option ;
      license_model: String.t option ;
      vpc: Boolean.t option ;
      filters: FilterList.t ;
      max_records: Integer.t option ;
      marker: String.t option }
    let make ~engine  ?engine_version  ?d_b_instance_class  ?license_model 
      ?vpc  ?(filters= [])  ?max_records  ?marker  () =
      {
        engine;
        engine_version;
        d_b_instance_class;
        license_model;
        vpc;
        filters;
        max_records;
        marker
      }
    let parse xml =
      Some
        {
          engine =
            (Xml.required "Engine"
               (Util.option_bind (Xml.member "Engine" xml) String.parse));
          engine_version =
            (Util.option_bind (Xml.member "EngineVersion" xml) String.parse);
          d_b_instance_class =
            (Util.option_bind (Xml.member "DBInstanceClass" xml) String.parse);
          license_model =
            (Util.option_bind (Xml.member "LicenseModel" xml) String.parse);
          vpc = (Util.option_bind (Xml.member "Vpc" xml) Boolean.parse);
          filters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Filters" xml) FilterList.parse));
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Some
             (Query.Pair ("Filters.member", (FilterList.to_query v.filters)));
           Util.option_map v.vpc
             (fun f -> Query.Pair ("Vpc", (Boolean.to_query f)));
           Util.option_map v.license_model
             (fun f -> Query.Pair ("LicenseModel", (String.to_query f)));
           Util.option_map v.d_b_instance_class
             (fun f -> Query.Pair ("DBInstanceClass", (String.to_query f)));
           Util.option_map v.engine_version
             (fun f -> Query.Pair ("EngineVersion", (String.to_query f)));
           Some (Query.Pair ("Engine", (String.to_query v.engine)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Some ("filters", (FilterList.to_json v.filters));
           Util.option_map v.vpc (fun f -> ("vpc", (Boolean.to_json f)));
           Util.option_map v.license_model
             (fun f -> ("license_model", (String.to_json f)));
           Util.option_map v.d_b_instance_class
             (fun f -> ("d_b_instance_class", (String.to_json f)));
           Util.option_map v.engine_version
             (fun f -> ("engine_version", (String.to_json f)));
           Some ("engine", (String.to_json v.engine))])
    let of_json j =
      {
        engine =
          (String.of_json (Util.of_option_exn (Json.lookup j "engine")));
        engine_version =
          (Util.option_map (Json.lookup j "engine_version") String.of_json);
        d_b_instance_class =
          (Util.option_map (Json.lookup j "d_b_instance_class")
             String.of_json);
        license_model =
          (Util.option_map (Json.lookup j "license_model") String.of_json);
        vpc = (Util.option_map (Json.lookup j "vpc") Boolean.of_json);
        filters =
          (FilterList.of_json (Util.of_option_exn (Json.lookup j "filters")));
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module DBClusterSnapshotMessage =
  struct
    type t =
      {
      marker: String.t option ;
      d_b_cluster_snapshots: DBClusterSnapshotList.t }
    let make ?marker  ?(d_b_cluster_snapshots= [])  () =
      { marker; d_b_cluster_snapshots }
    let parse xml =
      Some
        {
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          d_b_cluster_snapshots =
            (Util.of_option []
               (Util.option_bind (Xml.member "DBClusterSnapshots" xml)
                  DBClusterSnapshotList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("DBClusterSnapshots.member",
                   (DBClusterSnapshotList.to_query v.d_b_cluster_snapshots)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("d_b_cluster_snapshots",
                (DBClusterSnapshotList.to_json v.d_b_cluster_snapshots));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)))])
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        d_b_cluster_snapshots =
          (DBClusterSnapshotList.of_json
             (Util.of_option_exn (Json.lookup j "d_b_cluster_snapshots")))
      }
  end
module EventSubscriptionQuotaExceededFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module ListTagsForResourceMessage =
  struct
    type t = {
      resource_name: String.t ;
      filters: FilterList.t }
    let make ~resource_name  ?(filters= [])  () = { resource_name; filters }
    let parse xml =
      Some
        {
          resource_name =
            (Xml.required "ResourceName"
               (Util.option_bind (Xml.member "ResourceName" xml) String.parse));
          filters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Filters" xml) FilterList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair ("Filters.member", (FilterList.to_query v.filters)));
           Some
             (Query.Pair ("ResourceName", (String.to_query v.resource_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("filters", (FilterList.to_json v.filters));
           Some ("resource_name", (String.to_json v.resource_name))])
    let of_json j =
      {
        resource_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "resource_name")));
        filters =
          (FilterList.of_json (Util.of_option_exn (Json.lookup j "filters")))
      }
  end
module DescribeEngineDefaultParametersMessage =
  struct
    type t =
      {
      d_b_parameter_group_family: String.t ;
      filters: FilterList.t ;
      max_records: Integer.t option ;
      marker: String.t option }
    let make ~d_b_parameter_group_family  ?(filters= [])  ?max_records 
      ?marker  () =
      { d_b_parameter_group_family; filters; max_records; marker }
    let parse xml =
      Some
        {
          d_b_parameter_group_family =
            (Xml.required "DBParameterGroupFamily"
               (Util.option_bind (Xml.member "DBParameterGroupFamily" xml)
                  String.parse));
          filters =
            (Util.of_option []
               (Util.option_bind (Xml.member "Filters" xml) FilterList.parse));
          max_records =
            (Util.option_bind (Xml.member "MaxRecords" xml) Integer.parse);
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> Query.Pair ("Marker", (String.to_query f)));
           Util.option_map v.max_records
             (fun f -> Query.Pair ("MaxRecords", (Integer.to_query f)));
           Some
             (Query.Pair ("Filters.member", (FilterList.to_query v.filters)));
           Some
             (Query.Pair
                ("DBParameterGroupFamily",
                  (String.to_query v.d_b_parameter_group_family)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.marker
              (fun f -> ("marker", (String.to_json f)));
           Util.option_map v.max_records
             (fun f -> ("max_records", (Integer.to_json f)));
           Some ("filters", (FilterList.to_json v.filters));
           Some
             ("d_b_parameter_group_family",
               (String.to_json v.d_b_parameter_group_family))])
    let of_json j =
      {
        d_b_parameter_group_family =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "d_b_parameter_group_family")));
        filters =
          (FilterList.of_json (Util.of_option_exn (Json.lookup j "filters")));
        max_records =
          (Util.option_map (Json.lookup j "max_records") Integer.of_json);
        marker = (Util.option_map (Json.lookup j "marker") String.of_json)
      }
  end
module ReservedDBInstancesOfferingMessage =
  struct
    type t =
      {
      marker: String.t option ;
      reserved_d_b_instances_offerings: ReservedDBInstancesOfferingList.t }
    let make ?marker  ?(reserved_d_b_instances_offerings= [])  () =
      { marker; reserved_d_b_instances_offerings }
    let parse xml =
      Some
        {
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          reserved_d_b_instances_offerings =
            (Util.of_option []
               (Util.option_bind
                  (Xml.member "ReservedDBInstancesOfferings" xml)
                  ReservedDBInstancesOfferingList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("ReservedDBInstancesOfferings.member",
                   (ReservedDBInstancesOfferingList.to_query
                      v.reserved_d_b_instances_offerings)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("reserved_d_b_instances_offerings",
                (ReservedDBInstancesOfferingList.to_json
                   v.reserved_d_b_instances_offerings));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)))])
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        reserved_d_b_instances_offerings =
          (ReservedDBInstancesOfferingList.of_json
             (Util.of_option_exn
                (Json.lookup j "reserved_d_b_instances_offerings")))
      }
  end
module DBClusterMessage =
  struct
    type t = {
      marker: String.t option ;
      d_b_clusters: DBClusterList.t }
    let make ?marker  ?(d_b_clusters= [])  () = { marker; d_b_clusters }
    let parse xml =
      Some
        {
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          d_b_clusters =
            (Util.of_option []
               (Util.option_bind (Xml.member "DBClusters" xml)
                  DBClusterList.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("DBClusters.member",
                   (DBClusterList.to_query v.d_b_clusters)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("d_b_clusters", (DBClusterList.to_json v.d_b_clusters));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)))])
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        d_b_clusters =
          (DBClusterList.of_json
             (Util.of_option_exn (Json.lookup j "d_b_clusters")))
      }
  end
module InstanceQuotaExceededFault =
  struct
    type t = unit
    let make () = ()
    let parse xml = Some ()
    let to_query v = Query.List (Util.list_filter_opt [])
    let to_json v = `Assoc (Util.list_filter_opt [])
    let of_json j = ()
  end
module CreateDBSubnetGroupResult =
  struct
    type t = {
      d_b_subnet_group: DBSubnetGroup.t option }
    let make ?d_b_subnet_group  () = { d_b_subnet_group }
    let parse xml =
      Some
        {
          d_b_subnet_group =
            (Util.option_bind (Xml.member "DBSubnetGroup" xml)
               DBSubnetGroup.parse)
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Util.option_map v.d_b_subnet_group
              (fun f ->
                 Query.Pair ("DBSubnetGroup", (DBSubnetGroup.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Util.option_map v.d_b_subnet_group
              (fun f -> ("d_b_subnet_group", (DBSubnetGroup.to_json f)))])
    let of_json j =
      {
        d_b_subnet_group =
          (Util.option_map (Json.lookup j "d_b_subnet_group")
             DBSubnetGroup.of_json)
      }
  end
module RemoveSourceIdentifierFromSubscriptionMessage =
  struct
    type t = {
      subscription_name: String.t ;
      source_identifier: String.t }
    let make ~subscription_name  ~source_identifier  () =
      { subscription_name; source_identifier }
    let parse xml =
      Some
        {
          subscription_name =
            (Xml.required "SubscriptionName"
               (Util.option_bind (Xml.member "SubscriptionName" xml)
                  String.parse));
          source_identifier =
            (Xml.required "SourceIdentifier"
               (Util.option_bind (Xml.member "SourceIdentifier" xml)
                  String.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("SourceIdentifier", (String.to_query v.source_identifier)));
           Some
             (Query.Pair
                ("SubscriptionName", (String.to_query v.subscription_name)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some ("source_identifier", (String.to_json v.source_identifier));
           Some ("subscription_name", (String.to_json v.subscription_name))])
    let of_json j =
      {
        subscription_name =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "subscription_name")));
        source_identifier =
          (String.of_json
             (Util.of_option_exn (Json.lookup j "source_identifier")))
      }
  end
module DBSecurityGroupMessage =
  struct
    type t =
      {
      marker: String.t option ;
      d_b_security_groups: DBSecurityGroups.t }
    let make ?marker  ?(d_b_security_groups= [])  () =
      { marker; d_b_security_groups }
    let parse xml =
      Some
        {
          marker = (Util.option_bind (Xml.member "Marker" xml) String.parse);
          d_b_security_groups =
            (Util.of_option []
               (Util.option_bind (Xml.member "DBSecurityGroups" xml)
                  DBSecurityGroups.parse))
        }
    let to_query v =
      Query.List
        (Util.list_filter_opt
           [Some
              (Query.Pair
                 ("DBSecurityGroups.member",
                   (DBSecurityGroups.to_query v.d_b_security_groups)));
           Util.option_map v.marker
             (fun f -> Query.Pair ("Marker", (String.to_query f)))])
    let to_json v =
      `Assoc
        (Util.list_filter_opt
           [Some
              ("d_b_security_groups",
                (DBSecurityGroups.to_json v.d_b_security_groups));
           Util.option_map v.marker (fun f -> ("marker", (String.to_json f)))])
    let of_json j =
      {
        marker = (Util.option_map (Json.lookup j "marker") String.of_json);
        d_b_security_groups =
          (DBSecurityGroups.of_json
             (Util.of_option_exn (Json.lookup j "d_b_security_groups")))
      }
  end