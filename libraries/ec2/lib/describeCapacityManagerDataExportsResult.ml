open Aws.BaseTypes

type t =
  { capacity_manager_data_exports : CapacityManagerDataExportResponseSet.t
  ; next_token : String.t option
  }

let make ?(capacity_manager_data_exports = []) ?next_token () =
  { capacity_manager_data_exports; next_token }

let parse xml =
  Some
    { capacity_manager_data_exports =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "capacityManagerDataExportSet" xml)
             CapacityManagerDataExportResponseSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "CapacityManagerDataExportSet"
              , CapacityManagerDataExportResponseSet.to_query
                  v.capacity_manager_data_exports ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "capacityManagerDataExportSet"
           , CapacityManagerDataExportResponseSet.to_json v.capacity_manager_data_exports
           )
       ])

let of_json j =
  { capacity_manager_data_exports =
      CapacityManagerDataExportResponseSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "capacityManagerDataExportSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
