:- use_module(library(http/thread_httpd)).
:- use_module(library(arouter)).

:- route_get(hello/Name, handle_hello(Name)).

handle_hello(Name):-
    format('Content-Type: text/plain; charset=UTF-8~n~n'),
    format('Hello ~w', [Name]).

:- http_server(route, [port(8008)]).
