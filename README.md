Akka based Craft server
=======================

This is a reimplementation of the python server distributed with the [fogleman/Craft](https://github.com/fogleman/Craft) project. Specifically it is built to run with the modified client in the [nsg/Craft](https://github.com/nsg/Craft) project, which is implementing the new binary protocol optimized for server generated worlds.

## Building
Get [SBT](http://www.scala-sbt.org/download.html)!

```sbt test``` to run unit tests.

## Running

```sbt run``` starts the server and binds port 4080.
