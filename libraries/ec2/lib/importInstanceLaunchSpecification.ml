open Aws.BaseTypes

type t =
  { architecture : ArchitectureValues.t option
  ; group_names : SecurityGroupStringList.t
  ; group_ids : SecurityGroupIdStringList.t
  ; additional_info : String.t option
  ; user_data : UserData.t option
  ; instance_type : InstanceType.t option
  ; placement : Placement.t option
  ; monitoring : Boolean.t option
  ; subnet_id : String.t option
  ; instance_initiated_shutdown_behavior : ShutdownBehavior.t option
  ; private_ip_address : String.t option
  }

let make
    ?architecture
    ?(group_names = [])
    ?(group_ids = [])
    ?additional_info
    ?user_data
    ?instance_type
    ?placement
    ?monitoring
    ?subnet_id
    ?instance_initiated_shutdown_behavior
    ?private_ip_address
    () =
  { architecture
  ; group_names
  ; group_ids
  ; additional_info
  ; user_data
  ; instance_type
  ; placement
  ; monitoring
  ; subnet_id
  ; instance_initiated_shutdown_behavior
  ; private_ip_address
  }

let parse xml =
  Some
    { architecture =
        Aws.Util.option_bind (Aws.Xml.member "architecture" xml) ArchitectureValues.parse
    ; group_names =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "GroupName" xml)
             SecurityGroupStringList.parse)
    ; group_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "GroupId" xml)
             SecurityGroupIdStringList.parse)
    ; additional_info =
        Aws.Util.option_bind (Aws.Xml.member "additionalInfo" xml) String.parse
    ; user_data = Aws.Util.option_bind (Aws.Xml.member "userData" xml) UserData.parse
    ; instance_type =
        Aws.Util.option_bind (Aws.Xml.member "instanceType" xml) InstanceType.parse
    ; placement = Aws.Util.option_bind (Aws.Xml.member "placement" xml) Placement.parse
    ; monitoring = Aws.Util.option_bind (Aws.Xml.member "monitoring" xml) Boolean.parse
    ; subnet_id = Aws.Util.option_bind (Aws.Xml.member "subnetId" xml) String.parse
    ; instance_initiated_shutdown_behavior =
        Aws.Util.option_bind
          (Aws.Xml.member "instanceInitiatedShutdownBehavior" xml)
          ShutdownBehavior.parse
    ; private_ip_address =
        Aws.Util.option_bind (Aws.Xml.member "privateIpAddress" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.private_ip_address (fun f ->
             Aws.Query.Pair ("PrivateIpAddress", String.to_query f))
       ; Aws.Util.option_map v.instance_initiated_shutdown_behavior (fun f ->
             Aws.Query.Pair
               ("InstanceInitiatedShutdownBehavior", ShutdownBehavior.to_query f))
       ; Aws.Util.option_map v.subnet_id (fun f ->
             Aws.Query.Pair ("SubnetId", String.to_query f))
       ; Aws.Util.option_map v.monitoring (fun f ->
             Aws.Query.Pair ("Monitoring", Boolean.to_query f))
       ; Aws.Util.option_map v.placement (fun f ->
             Aws.Query.Pair ("Placement", Placement.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", InstanceType.to_query f))
       ; Aws.Util.option_map v.user_data (fun f ->
             Aws.Query.Pair ("UserData", UserData.to_query f))
       ; Aws.Util.option_map v.additional_info (fun f ->
             Aws.Query.Pair ("AdditionalInfo", String.to_query f))
       ; Some (Aws.Query.Pair ("GroupId", SecurityGroupIdStringList.to_query v.group_ids))
       ; Some
           (Aws.Query.Pair ("GroupName", SecurityGroupStringList.to_query v.group_names))
       ; Aws.Util.option_map v.architecture (fun f ->
             Aws.Query.Pair ("Architecture", ArchitectureValues.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.private_ip_address (fun f ->
             "privateIpAddress", String.to_json f)
       ; Aws.Util.option_map v.instance_initiated_shutdown_behavior (fun f ->
             "instanceInitiatedShutdownBehavior", ShutdownBehavior.to_json f)
       ; Aws.Util.option_map v.subnet_id (fun f -> "subnetId", String.to_json f)
       ; Aws.Util.option_map v.monitoring (fun f -> "monitoring", Boolean.to_json f)
       ; Aws.Util.option_map v.placement (fun f -> "placement", Placement.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f ->
             "instanceType", InstanceType.to_json f)
       ; Aws.Util.option_map v.user_data (fun f -> "userData", UserData.to_json f)
       ; Aws.Util.option_map v.additional_info (fun f ->
             "additionalInfo", String.to_json f)
       ; Some ("GroupId", SecurityGroupIdStringList.to_json v.group_ids)
       ; Some ("GroupName", SecurityGroupStringList.to_json v.group_names)
       ; Aws.Util.option_map v.architecture (fun f ->
             "architecture", ArchitectureValues.to_json f)
       ])

let of_json j =
  { architecture =
      Aws.Util.option_map (Aws.Json.lookup j "architecture") ArchitectureValues.of_json
  ; group_names =
      SecurityGroupStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "GroupName"))
  ; group_ids =
      SecurityGroupIdStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "GroupId"))
  ; additional_info =
      Aws.Util.option_map (Aws.Json.lookup j "additionalInfo") String.of_json
  ; user_data = Aws.Util.option_map (Aws.Json.lookup j "userData") UserData.of_json
  ; instance_type =
      Aws.Util.option_map (Aws.Json.lookup j "instanceType") InstanceType.of_json
  ; placement = Aws.Util.option_map (Aws.Json.lookup j "placement") Placement.of_json
  ; monitoring = Aws.Util.option_map (Aws.Json.lookup j "monitoring") Boolean.of_json
  ; subnet_id = Aws.Util.option_map (Aws.Json.lookup j "subnetId") String.of_json
  ; instance_initiated_shutdown_behavior =
      Aws.Util.option_map
        (Aws.Json.lookup j "instanceInitiatedShutdownBehavior")
        ShutdownBehavior.of_json
  ; private_ip_address =
      Aws.Util.option_map (Aws.Json.lookup j "privateIpAddress") String.of_json
  }
