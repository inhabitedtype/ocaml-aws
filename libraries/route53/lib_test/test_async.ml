open Aws_route53_test

module T = TestSuite (struct
  type 'a m = 'a Async.Deferred.t

  let access_key = Unix.getenv "AWS_ACCESS_KEY"

  let secret_key = Unix.getenv "AWS_SECRET_KEY"

  let run_request = Aws_async.Runtime.run_request ~access_key ~secret_key

  let un_m v = Async.Thread_safe.block_on_async_exn (fun () -> v)
end)
