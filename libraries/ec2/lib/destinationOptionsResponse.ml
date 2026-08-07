open Aws.BaseTypes

type t =
  { file_format : DestinationFileFormat.t option
  ; hive_compatible_partitions : Boolean.t option
  ; per_hour_partition : Boolean.t option
  }

let make ?file_format ?hive_compatible_partitions ?per_hour_partition () =
  { file_format; hive_compatible_partitions; per_hour_partition }

let parse xml =
  Some
    { file_format =
        Aws.Util.option_bind (Aws.Xml.member "fileFormat" xml) DestinationFileFormat.parse
    ; hive_compatible_partitions =
        Aws.Util.option_bind (Aws.Xml.member "hiveCompatiblePartitions" xml) Boolean.parse
    ; per_hour_partition =
        Aws.Util.option_bind (Aws.Xml.member "perHourPartition" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.per_hour_partition (fun f ->
             Aws.Query.Pair ("PerHourPartition", Boolean.to_query f))
       ; Aws.Util.option_map v.hive_compatible_partitions (fun f ->
             Aws.Query.Pair ("HiveCompatiblePartitions", Boolean.to_query f))
       ; Aws.Util.option_map v.file_format (fun f ->
             Aws.Query.Pair ("FileFormat", DestinationFileFormat.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.per_hour_partition (fun f ->
             "perHourPartition", Boolean.to_json f)
       ; Aws.Util.option_map v.hive_compatible_partitions (fun f ->
             "hiveCompatiblePartitions", Boolean.to_json f)
       ; Aws.Util.option_map v.file_format (fun f ->
             "fileFormat", DestinationFileFormat.to_json f)
       ])

let of_json j =
  { file_format =
      Aws.Util.option_map (Aws.Json.lookup j "fileFormat") DestinationFileFormat.of_json
  ; hive_compatible_partitions =
      Aws.Util.option_map (Aws.Json.lookup j "hiveCompatiblePartitions") Boolean.of_json
  ; per_hour_partition =
      Aws.Util.option_map (Aws.Json.lookup j "perHourPartition") Boolean.of_json
  }
