open Aws_kms_test

module T = TestSuite (struct
  type 'a m = 'a Async.Deferred.t

  let run_request = Aws_async.Runtime.run_request

  let un_m v = Async.Thread_safe.block_on_async_exn (fun () -> v)
end)
