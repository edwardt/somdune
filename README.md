# Somdune: HTTP router, load balancer, and magic maker

Somdune is a simple, stable, reliable HTTP proxy or router. When it receives an HTTP query, it uses
its plugins to decide where it should go. Once the route is decided, Somdune becomes a truly
transparent TCP relay or tunnel to the destination web server.

## Building

Somdune builds with Rebar, which is included in the project. Once you have Erlang installed, run:

    ./rebar compile

## Usage

Somdune requires one or more plugins to decide on the routing policy. Every incoming request is parsed
by Somdune and then passed to the plugin. Plugins can instruct Somdune to do several things:

* Drop the query completely
* Return an arbitrary web page (or any other web response)
* Proxy the request as-is to a different web service (the most common operation)
* Proxy a modified version of the request to a different web service

Please see the `example/` subdirectory for the plugin reference implementation, **Webdirs**. Webdirs
"swallows" various sites (Twitter, the CouchOne home page, and a CouchDB server) inside itself. For
example, when you query `/twitter/Whatever`, you will get a response from `twitter.com/Whatever`;
when you query `/jhs/_all_dbs`, you will hit `jhs.couchone.com/_all_dbs`, etc.
