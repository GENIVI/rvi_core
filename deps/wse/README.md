wse - Websockets for Erlang
===========================

WSE is a simple, standalone, websocket server which execute
request in the browser instead of in the web server. There is a bit
of boot strapping that may need help from a web server. But a simple
web page and a browser is all that it takes to get running.

To get control of part of a web page (or all of it) use the following 
HTML snippet. Also make sure you have ej.js and wse.js in the same directory.

    <html><head>
    <title>wse demo page</title>
    <script src='ej.js'></script>
    <script src='wse.js'></script>
    <script>
        window.onload = function() {
          if (Wse.open("ws://localhost:1234/websession"))
             Wse.start('wse_demo', 'run', ["myid"]);
        };
    </script></head>
    <body>
      <div id="myid"></div>
    </body></html>

Next thing is to start erlang wse server (default to port 1234):

    $ erl
    > wse_server:start().

Then have the erlang module wse_demo in the path somewhere:

    -module(wse_demo).
    -export([run/2]).

    run(Ws, Where) ->
        ElemNode = wse:createElement(Ws, "p"),
        TextNode = wse:createTextNode(Ws, "Hello world"),
        wse:appendChild(Ws, ElemNode, TextNode),
        wse:appendChild(Ws, wse:id(Where), ElemNode).

The browser will call wse_demo:run (via the websocket) with the web socket proxy process as a the first argument and the "myid" as the second argument. From thereon the web page can be manipulated at will.

# register pages

One nice trick to interact with pages from command line is to
register the page it self

    <html><head>
    <title>Page A</title>
    <script src='ej.js'></script>
    <script src='wse.js'></script>
    <script>
      window.onload = function() {
      if (Wse.open("ws://localhost:1234/websession")) {
        Wse.register('page_a');
      };
    </script>
    </head><body>
    <p id="x">Hello A World</p>
    </body></html>

Now this page should be registered in erlang and can be tested. Start with
changing the text in the paragraf on the page.

    > {ok,X} = wse:getElementById(page_a, "x").
    > {ok,Text} = wse:firstChild(page_a, X).
    > wse:set(page_a, Text, "nodeValue", "Hej A").

Or we could send some ehtml (erlang style html) defining a button to it

    > wse:send(page_a, "x", {button,[{id,"y"}],["Press Me"]}).

Or why not send a whole table to it

    > wse:send(page_a, "x", {table,[],[{tr,[],[{td,[],["A"]},{td,[],["B"]}]},{tr,[],[{td,[],["C"]},{td,[],["D"]}]}]}).

To have events sent to us when pressing a button, using send, we can do

    > {ok,E} = wse:create_event(page_a).
    > wse:send(page_a, "x", {button,[{id,"y"},{onclick,"Wse.notify("++integer_to_list(E)++",'click');"}],["Press Me"]}).

Click on the button on page A a couple of times

    > flush().
    Shell got {notify,2,[],"click"}
    Shell got {notify,2,[],"click"}
    Shell got {notify,2,[],"click"}
