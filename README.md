# Mongo.ml

**Mongo.ml** is an OCaml driver for MongoDB.

## OlegHQ `mongo-eio` fork scope

This fork is packaged with Dune for OCaml 5 and currently supports the
synchronous compatibility modules plus additive Eio wrappers used by Poster.
The supported command path uses MongoDB `OP_MSG` for normal CRUD, index, admin,
cursor, authentication, and pool smoke coverage.

The historical `lwt/` sources are kept in the repository for reference only.
They are not included in the Dune package, are not covered by the current
`OP_MSG`/auth/topology tests, and should not be treated as production-supported
unless they are explicitly modernized.

## Production notes for this fork

Use the modern `Mongo_config`, `Mongo_connection`, `Mongo_pool`,
`Mongo_topology`, and `Mongo_eio.direct_client` path for new code. The legacy
collection-bound `Mongo.t` API remains for compatibility, but new behavior is
verified on the OP_MSG command/pool/direct-client path.

Monitor callbacks should update both topology and pool state. A typical
callback folds the `Mongo_server_description.t` through
`Mongo_topology.update_server` and then passes the same description to
`Mongo_pool.update_server_description`; the pool hook clears stale pooled
connections when a monitor reports an `Unknown` server with error details.

Replica-set discovery, seed fallback, server selection, failover reconnect,
timeouts, SCRAM authentication, TLS server authentication, retryable command
execution, cursor `getMore`/`killCursors`, and pool lifecycle behavior have
unit/e2e coverage in this fork. Remaining production gaps are full background
SDAM ownership of per-discovered-server pools, long-lived multi-node routing
after topology changes, fuller CMAP event parity, speculative/reauthentication
auth flows, and advanced TLS options such as client certificates and revocation
checking.

## Verification

Run the local driver matrix before relying on this fork:

```sh
opam exec -- dune runtest --root vendor/mongo-eio
vendor/mongo-eio/scripts/verify-driver.sh
```

By default the script runs standalone driver/admin/pool e2e tests against
`POSTER_MONGO_HOST`/`POSTER_MONGO_PORT`, defaulting to `oracle-vm:27017`, then
starts local `mongo:7` containers for TLS, single-node replica-set, three-node
failover, retry failpoint, and SCRAM auth smoke tests. Hosted CI sets
`RUN_STANDALONE_CONTAINER=1` so the same standalone e2e path runs against a
local `mongo:7` container instead of the developer `oracle-vm` host.

It supplies a series of APIs which can be used to communicate with MongoDB, i.e., **Insert**, **Update**, **Delete** and **Query / Find**.

Here is the [Mongo.ml API docs](http://massd.github.io/mongo/doc/).

### Prerequisite

This driver uses **unix** and external [Bson.ml](http://massd.github.io/bson/) modules.

Here is the [Bson.ml API doc](http://massd.github.io/bson/doc/Bson.html).

***

### How to use it

**Mongo** and **MongoAdmin** are the two modules for high level usage.

**Mongo** is a MongoDB client for general purpose. It can be used to operate normal bson documents on MongoDB.

**MongoAdmin** is a special MongoDB client for accessing admin level of MongoDB commands, such as _list databases_, etc. Please refer to [MongoDB commands](http://docs.mongodb.org/manual/reference/command/).

The usages of these two modules are similar:

1. **Mongo.create** a Mongo with ip, port, db\_name, and collection\_name (MongoAdmin does not need db\_name or collection\_name)
2. Depending on the request type, create the Bson document using **Bson.ml**
3. **Mongo.insert** / **Mongo.update** / **Mongo.delete** / **Mongo.find** / **Mongo.get_more** / **Mongo.kill_cursors**
4. Only **Mongo.find** and **Mongo.get_more** will wait for a **MongoReply**. Others will finish immediately.
5. **Mongo.destroy** the Mongo to release the resources.

***

### Sample usage

Please refer to **test/test_mongo.ml** for a taste of usage.

	ocamlbuild -use-ocamlfind -I src test/test_mongo.native
	./test_mongo.native

***

### Extend the driver

Comparing to MongoDB's official drivers, **this OCaml driver is not that complete.**

This driver can be used only for **essential** operations on MongoDB, particularly with all default options/configurations.

I am slowly extend this driver and **experienced OCaml/MongoDB developers are welcomed to join**.

***

### The source code

**MongoOperation** defines all operations allowed by MongoDB.

**MongoHeader** defines the header that is used in MongoDB messages. It includes encoding / decoding the MongoDB messages. When constructing a **MongoRequest**, encoding is used; when constructing a **MongoReply** from the message sent by MongoDB, decoding is used.

**MongoRequest** create the message bytes (string) for all requests. The output string can be used to **MongoSend** to send to MongoDB socket. Every function inside has full parameters according to [MongoDB wire protocol](http://docs.mongodb.org/meta-driver/latest/legacy/mongodb-wire-protocol/).

**MongoSend** takes a **unix file_descr** and a **string** and send the string to the file_descr. It uses **unix**.

**MongoReply** is the type that contains the reply MongoDB.

**Mongo** and **MongoAdmin** are the client-faced interfaces. They are the first places to be extended.

***

### Misc

The current version is **0.67.2**.

### OPAM
Since version 0.67.0, bson.ml and mongo.ml are package in opam.

To install with opam: `opam install mongo`
