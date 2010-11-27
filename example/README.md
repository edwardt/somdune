# Webdirs: Web sites in directories!

This is an example Somdune application. It starts a Somdune proxy with a simple plugin:
Subdirectories under the web site are proxied to various other web sites.

* `/couchone` goes to the CouchOne web site, www.couchone.com
* `/jhs` goes to my CouchDB server, jhs.couchone.com
* `/twitter` goes to www.twitter.com

Note that cookies and other problems will not make these sites terribly useful; however,
it should be clear what is going on and how Somdune works.

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

Next connect to http://localhost:8888/ and follow the links!
