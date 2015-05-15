Konstructs Server
=================

This is a Infiniminer/Minecraft inspired game server. It has four overall goals:

- Scalability
- Huge worlds
- Extendability
- Server driven game logic

## Scalability - actors

The server is implemented using [Akka](http://akka.io/) actors and the new IO subsystem ported from the [Spray](http://spray.io/) project. We try to keep everything nicely isolated in or behind an actor to make the game easy to scale up (run on multiple cores/CPUs). We hope that this in the long run also will help with scaling out (running the same world on several servers).

## Huge worlds - separated game logic and world

At the core we try to have very simple block format that is shared with the client. We store all blocks compressed using [DEFALTE](http://en.wikipedia.org/wiki/DEFLATE), also when in memory or being sent to the client. This means that we can keep a huge amount of blocks in memory which helps fight IO latency as well as keeping disk IO to a minimum. Actually, one of the unimplemented features is block unloading. So far we never needed it!

Since we can manage a huge amount of blocks, it makes sense to not limit world interaction to where the player is. Therefore we have completely separated all world logic from the world itself. Therefore automatic world interaction is not dependent on the part of the world being loaded, but is always done for all of the world via plugins.

## Extendability - plugins are actors

Since the server is at is core just a set of actors exchanging messages every plugin is created equal. Message interfaces are provided to interact with the block world as well as with players. Persistence are provide by actors capable of loading and storing JSON data on behalf of other actors. It is therefore easy to create a plugin that keeps out of the way of other plugins.

## Server driven game logic - true multiplayer game

To keep the game extremely extensible, while still keeping the client simple, all game logic is implemented server side. This means that for the player there is no need to download different plugins or versions of the client to play different worlds. Plugins mainly interact with the block world and/or using one of the simple features implemented in the client (like the inventory). We are also planning on adding some kind of crafting support.

## Building
Get [SBT](http://www.scala-sbt.org/download.html)!

```sbt test``` to run unit tests.

## Running

```sbt run``` starts the server and binds port 4080.
