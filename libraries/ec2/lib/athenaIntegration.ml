open Aws.BaseTypes

type t =
  { integration_result_s3_destination_arn : String.t
  ; partition_load_frequency : PartitionLoadFrequency.t
  ; partition_start_date : DateTime.t option
  ; partition_end_date : DateTime.t option
  }

let make
    ~integration_result_s3_destination_arn
    ~partition_load_frequency
    ?partition_start_date
    ?partition_end_date
    () =
  { integration_result_s3_destination_arn
  ; partition_load_frequency
  ; partition_start_date
  ; partition_end_date
  }

let parse xml =
  Some
    { integration_result_s3_destination_arn =
        Aws.Xml.required
          "IntegrationResultS3DestinationArn"
          (Aws.Util.option_bind
             (Aws.Xml.member "IntegrationResultS3DestinationArn" xml)
             String.parse)
    ; partition_load_frequency =
        Aws.Xml.required
          "PartitionLoadFrequency"
          (Aws.Util.option_bind
             (Aws.Xml.member "PartitionLoadFrequency" xml)
             PartitionLoadFrequency.parse)
    ; partition_start_date =
        Aws.Util.option_bind (Aws.Xml.member "PartitionStartDate" xml) DateTime.parse
    ; partition_end_date =
        Aws.Util.option_bind (Aws.Xml.member "PartitionEndDate" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.partition_end_date (fun f ->
             Aws.Query.Pair ("PartitionEndDate", DateTime.to_query f))
       ; Aws.Util.option_map v.partition_start_date (fun f ->
             Aws.Query.Pair ("PartitionStartDate", DateTime.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "PartitionLoadFrequency"
              , PartitionLoadFrequency.to_query v.partition_load_frequency ))
       ; Some
           (Aws.Query.Pair
              ( "IntegrationResultS3DestinationArn"
              , String.to_query v.integration_result_s3_destination_arn ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.partition_end_date (fun f ->
             "PartitionEndDate", DateTime.to_json f)
       ; Aws.Util.option_map v.partition_start_date (fun f ->
             "PartitionStartDate", DateTime.to_json f)
       ; Some
           ( "PartitionLoadFrequency"
           , PartitionLoadFrequency.to_json v.partition_load_frequency )
       ; Some
           ( "IntegrationResultS3DestinationArn"
           , String.to_json v.integration_result_s3_destination_arn )
       ])

let of_json j =
  { integration_result_s3_destination_arn =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "IntegrationResultS3DestinationArn"))
  ; partition_load_frequency =
      PartitionLoadFrequency.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "PartitionLoadFrequency"))
  ; partition_start_date =
      Aws.Util.option_map (Aws.Json.lookup j "PartitionStartDate") DateTime.of_json
  ; partition_end_date =
      Aws.Util.option_map (Aws.Json.lookup j "PartitionEndDate") DateTime.of_json
  }
