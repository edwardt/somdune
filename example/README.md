# Webdirs: Web sites in directories!

This is an example Somdune application. It starts a Somdune proxy with a simple plugin:
Subdirectories under the web site are proxied to various other web sites.

* `/google` goes to www.google.com
* `/jhs` goes to my CouchDB server, jhs.couchone.com
* `/couchone` goes to the CouchOne web site, www.couchone.com

Note that cookies and other things will not make these sites terribly useful; however,
it is just an example of how to proxy to various web services based on any aspect of
the incoming request.

## Building

Webdirs builds with Rebar, which is included in the Somdune project. After building Somdune itself,
simply build Webdirs the same way.

    cd example
    ../rebar compile

## Running

Make sure both Somdune and Webdirs was compiled with `rebar compile`.

Simply run the `webdirs` module (with the correct Erlang library paths of course). That module
will start the actual OTP application for you.

    cd example
    erl -pa ../ebin -pa ./ebin -heart -s webdirs
