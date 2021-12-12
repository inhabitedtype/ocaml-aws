open Aws_kms_test

module T = TestSuite (struct
  type 'a m = 'a Lwt.t

  let run_request = Aws_lwt.Runtime.run_request

  let un_m = Lwt_main.run
end)
