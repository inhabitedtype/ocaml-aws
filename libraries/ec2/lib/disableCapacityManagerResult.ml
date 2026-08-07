open Aws.BaseTypes

type t =
  { capacity_manager_status : CapacityManagerStatus.t option
  ; organizations_access : Boolean.t option
  }

let make ?capacity_manager_status ?organizations_access () =
  { capacity_manager_status; organizations_access }

let parse xml =
  Some
    { capacity_manager_status =
        Aws.Util.option_bind
          (Aws.Xml.member "capacityManagerStatus" xml)
          CapacityManagerStatus.parse
    ; organizations_access =
        Aws.Util.option_bind (Aws.Xml.member "organizationsAccess" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.organizations_access (fun f ->
             Aws.Query.Pair ("OrganizationsAccess", Boolean.to_query f))
       ; Aws.Util.option_map v.capacity_manager_status (fun f ->
             Aws.Query.Pair ("CapacityManagerStatus", CapacityManagerStatus.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.organizations_access (fun f ->
             "organizationsAccess", Boolean.to_json f)
       ; Aws.Util.option_map v.capacity_manager_status (fun f ->
             "capacityManagerStatus", CapacityManagerStatus.to_json f)
       ])

let of_json j =
  { capacity_manager_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "capacityManagerStatus")
        CapacityManagerStatus.of_json
  ; organizations_access =
      Aws.Util.option_map (Aws.Json.lookup j "organizationsAccess") Boolean.of_json
  }
