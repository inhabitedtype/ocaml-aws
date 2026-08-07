open Aws.BaseTypes

type t =
  { fpgas : FpgaDeviceInfoList.t
  ; total_fpga_memory_in_mi_b : Integer.t option
  }

let make ?(fpgas = []) ?total_fpga_memory_in_mi_b () =
  { fpgas; total_fpga_memory_in_mi_b }

let parse xml =
  Some
    { fpgas =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "fpgas" xml) FpgaDeviceInfoList.parse)
    ; total_fpga_memory_in_mi_b =
        Aws.Util.option_bind (Aws.Xml.member "totalFpgaMemoryInMiB" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.total_fpga_memory_in_mi_b (fun f ->
             Aws.Query.Pair ("TotalFpgaMemoryInMiB", Integer.to_query f))
       ; Some (Aws.Query.Pair ("Fpgas", FpgaDeviceInfoList.to_query v.fpgas))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.total_fpga_memory_in_mi_b (fun f ->
             "totalFpgaMemoryInMiB", Integer.to_json f)
       ; Some ("fpgas", FpgaDeviceInfoList.to_json v.fpgas)
       ])

let of_json j =
  { fpgas =
      FpgaDeviceInfoList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "fpgas"))
  ; total_fpga_memory_in_mi_b =
      Aws.Util.option_map (Aws.Json.lookup j "totalFpgaMemoryInMiB") Integer.of_json
  }
