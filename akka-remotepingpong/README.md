Akka-remotingpingpong
====

sbt "run-main sample.remote.pingpong.Starter pong"

sbt "run-main sample.remote.pingpong.Starter ping localhost 10 10000 1000000"

sbt "run-main sample.remote.pingpong.Starter ping localhost 100 10000 1000000"

sbt "run-main sample.remote.pingpong.Starter ping localhost 1000 10000 1000000"

sbt "run-main sample.remote.pingpong.Starter ping localhost 10000 100000 1000000"

sbt "run-main sample.remote.pingpong.Starter ping localhost 100000 200000 1000000"

Rscript src/main/r/stat.r

Analyze plots in stat directory