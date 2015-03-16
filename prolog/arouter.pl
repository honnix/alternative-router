:- module(arouter, [
    route/1,                 % +Request
    blueprint/2,             % +Name, +Prefix
    route_with_fallbacks/2,  % +Fallbacks, +Request
    route_get/2,             % +Route, :Goal
    route_post/2,            % +Route, :Goal
    route_put/2,             % +Route, :Goal
    route_del/2,             % +Route, :Goal
    routes/3,                % +Route, +Methods, :Goal
    route_get_b/3,           % +Blueprint, +Route, :Goal
    route_post_b/3,          % +Blueprint, +Route, :Goal
    route_put_b/3,           % +Blueprint, +Route, :Goal
    route_del_b/3,           % +Blueprint, +Route, :Goal
    routes_b/4,              % +Blueprint, +Route, +Methods, :Goal
    route_get/3,             % +Route, :BeforeGoal, :Goal
    route_post/3,            % +Route, :BeforeGoal, :Goal
    route_put/3,             % +Route, :BeforeGoal, :Goal
    route_del/3,             % +Route, :BeforeGoal, :Goal
    routes/4,                % +Route, +Methods, :BeforeGoal, :Goal
    route_get_b/4,           % +Blueprint, +Route, :BeforeGoal, :Goal
    route_post_b/4,          % +Blueprint, +Route, :BeforeGoal, :Goal
    route_put_b/4,           % +Blueprint, +Route, :BeforeGoal, :Goal
    route_del_b/4,           % +Blueprint, +Route, :BeforeGoal, :Goal
    routes_b/5,              % +Blueprint, +Route, +Methods, :BeforeGoal, :Goal
    new_route/3,             % +Method, +Route, :Goal
    new_route/4,             % +Method, +Route, :BeforeGoal, :Goal
    new_route_b/4,           % +Blueprint, +Method, +Route, :Goal
    new_route_b/5,           % +Blueprint, +Method, +Route, :BeforeGoal, :Goal
    route_remove/2,          % +Method, +Route
    route_remove_b/3,        % +Blueprint, +Method, +Route
    route/4,                 % ?Method, ?Route, ?Before, ?Goal
    path_to_route/2,         % +Path, -Route
    request/1                % -Request
]).

/** <module> Alternative HTTP routing

HTTP routing with path expressions.
*/

:- use_module(library(debug)).
:- use_module(library(error)).

%! route(?Method, ?Route, ?Before, ?Goal) is nondet.
%
% Retrieves currently registered routes.

:- dynamic(route/4).

%! blueprint_rec(?Name, ?Prefix) is nondet.
%
% Retrieves currently registered blueprints.

:- dynamic(blueprint_rec/2).

:- meta_predicate(route_get(+, 0)).
:- meta_predicate(route_post(+, 0)).
:- meta_predicate(route_put(+, 0)).
:- meta_predicate(route_del(+, 0)).
:- meta_predicate(routes(+, +, 0)).
:- meta_predicate(route_get_b(+, +, 0)).
:- meta_predicate(route_post_b(+, +, 0)).
:- meta_predicate(route_put_b(+, +, 0)).
:- meta_predicate(route_del_b(+, +, 0)).
:- meta_predicate(routes_b(+, +, +, 0)).
:- meta_predicate(route_get(+, 1, 0)).
:- meta_predicate(route_post(+, 1, 0)).
:- meta_predicate(route_put(+, 1, 0)).
:- meta_predicate(route_del(+, 1, 0)).
:- meta_predicate(routes(+, +, 1, 0)).
:- meta_predicate(route_get_b(+, +, 1, 0)).
:- meta_predicate(route_post_b(+, +, 1, 0)).
:- meta_predicate(route_put_b(+, +, 1, 0)).
:- meta_predicate(route_del_b(+, +, 1, 0)).
:- meta_predicate(routes_b(+, +, +, 1, 0)).
:- meta_predicate(new_route(+, +, 0)).
:- meta_predicate(new_route_b(+, +, +, 0)).
:- meta_predicate(new_route(+, +, 1, 0)).
:- meta_predicate(new_route_b(+, +, +, 1, 0)).
:- meta_predicate(blueprint(+, +)).

:- thread_local
    request/1.

%! blueprint(+Name, +Prefix) is det.
%
% Registers a new blueprint for a set
% of handlers.

blueprint(Name, Prefix) :-
    sub_atom(Prefix, 0, _, 1, Prefix1),
    asserta(blueprint_rec(Name, Prefix1)).

%! route_get(+Route, :Goal) is det.
%
% Registers a new GET route handler.

route_get(Route, Goal):-
    new_route(get, Route, Goal).

%! route_put(+Route, :Goal) is det.
%
% Registers a new PUT route handler.

route_put(Route, Goal):-
    new_route(put, Route, Goal).

%! route_del(+Route, :Goal) is det.
%
% Registers a new DELETE route handler.

route_del(Route, Goal):-
    new_route(delete, Route, Goal).

%! route_post(+Route, :Goal) is det.
%
% Registers a new POST route handler.

route_post(Route, Goal):-
    new_route(post, Route, Goal).

%! routes(+Route, +Methods, :Goal) is det.
%
% Registers new route handlers for all specified
% Methods.

routes(Route, Methods, Goal) :-
    foreach(member(X, Methods), new_route(X, Route, Goal)).

%! route_get_b(+Blueprint, +Route, :Goal) is det.
%
% Registers a new GET route handler.

route_get_b(Blueprint, Route, Goal):-
    new_route_b(Blueprint, get, Route, Goal).

%! route_put_b(+Blueprint, +Route, :Goal) is det.
%
% Registers a new PUT route handler.

route_put_b(Blueprint, Route, Goal):-
    new_route_b(Blueprint, put, Route, Goal).

%! route_del_b(+Blueprint, +Route, :Goal) is det.
%
% Registers a new DELETE route handler.

route_del_b(Blueprint, Route, Goal):-
    new_route_b(Blueprint, delete, Route, Goal).

%! route_post_b(+Blueprint, +Route, :Goal) is det.
%
% Registers a new POST route handler.

route_post_b(Blueprint, Route, Goal):-
    new_route_b(Blueprint, post, Route, Goal).

%! routes_b(+Blueprint, +Route, +Methods, :Goal) is det.
%
% Registers new route handlers for all specified
% Methods.

routes_b(Blueprint, Route, Methods, Goal) :-
    foreach(member(X, Methods), new_route_b(Blueprint, X, Route, Goal)).

%! route_get(+Route, :Before, :Goal) is det.
%
% Registers a new GET route handler.
% Accepts Before goal.

route_get(Route, Before, Goal):-
    new_route(get, Route, Before, Goal).

%! route_put(+Route, :Before, :Goal) is det.
%
% Registers a new PUT route handler.
% Accepts Before goal.

route_put(Route, Before, Goal):-
    new_route(put, Route, Before, Goal).
    
%! route_del(+Route, :Before, :Goal) is det.
%
% Registers a new DELETE route handler.
% Accepts Before goal.

route_del(Route, Before, Goal):-
    new_route(delete, Route, Before, Goal).

%! route_post(+Route, :Before, :Goal) is det.
%
% Registers a new POST route handler.
% Accepts Before goal.

route_post(Route, Before, Goal):-
    new_route(post, Route, Before, Goal).    

%! routes(+Route, +Methods, :Before, :Goal) is det.
%
% Registers route handlers for all specified
% Methods.
% Accepts Before goal.

routes(Route, Methods, Before, Goal):-
    foreach(member(X, Methods), new_route(X, Route, Before, Goal)).

%! route_get_b(+Blueprint, +Route, :Before, :Goal) is det.
%
% Registers a new GET route handler.
% Accepts Before goal.

route_get_b(Blueprint, Route, Before, Goal):-
    new_route_b(Blueprint, get, Route, Before, Goal).

%! route_put_b(+Blueprint, +Route, :Before, :Goal) is det.
%
% Registers a new PUT route handler.
% Accepts Before goal.

route_put_b(Blueprint, Route, Before, Goal):-
    new_route_b(Blueprint, put, Route, Before, Goal).

%! route_del_b(+Blueprint, +Route, :Before, :Goal) is det.
%
% Registers a new DELETE route handler.
% Accepts Before goal.

route_del_b(Blueprint, Route, Before, Goal):-
    new_route_b(Blueprint, delete, Route, Before, Goal).

%! route_post_b(+Blueprint, +Route, :Before, :Goal) is det.
%
% Registers a new POST route handler.
% Accepts Before goal.

route_post_b(Blueprint, Route, Before, Goal):-
    new_route_b(Blueprint, post, Route, Before, Goal).

%! routes_b(+Blueprint, +Route, +Methods, :Before, :Goal) is det.
%
% Registers route handlers for all specified
% Methods.
% Accepts Before goal.

routes_b(Blueprint, Route, Methods, Before, Goal):-
    foreach(member(X, Methods), new_route(Blueprint, X, Route, Before, Goal)).

%! new_route(+Method, +Route, :Before, :Goal) is det.
%
% Registers a new method-specific route handler.
% Does nothing when the route already exists
% for the method.

new_route(Method, Route, Before, Goal):-
    must_be(atom, Method),
    check_route(Route),
    (   existing_route(Method, Route, Ref)
    ->  erase(Ref)
    ;   true),
    asserta(route(Method, Route, goal(Before), Goal)).

%! new_route_b(+Blueprint, +Method, +Route, :Before, :Goal) is det.
%
% Registers a new method-specific route handler.
% Does nothing when the route already exists
% for the method.

new_route_b(Blueprint, Method, Route, Before, Goal):-
    must_be(atom, Method),
    blueprint_rec(Blueprint, Prefix),
    path_to_route(Prefix, Route1),
    concat_route(Route1, Route, FullRoute),
    check_route(FullRoute),
    (   existing_route(Method, FullRoute, Ref)
    ->  erase(Ref)
    ;   true),
    asserta(route(Method, FullRoute, goal(Before), Goal)).

%! new_route(+Method, +Route, :Goal) is det.
%
% Registers a new method-specific route handler.
% Does nothing when the route already exists
% for the method.

new_route(Method, Route, Goal):-
    must_be(atom, Method),
    check_route(Route),
    (   existing_route(Method, Route, Ref)
    ->  erase(Ref)
    ;   true),
    asserta(route(Method, Route, none, Goal)).

%! new_route_b(+Blueprint, +Method, +Route, :Goal) is det.
%
% Registers a new method-specific route handler.
% Does nothing when the route already exists
% for the method.

new_route_b(Blueprint, Method, Route, Goal):-
    must_be(atom, Method),
    blueprint_rec(Blueprint, Prefix),
    path_to_route(Prefix, Route1),
    concat_route(Route1, Route, FullRoute),
    check_route(FullRoute),
    (   existing_route(Method, FullRoute, Ref)
    ->  erase(Ref)
    ;   true),
    asserta(route(Method, FullRoute, none, Goal)).

check_route(Atom):-
    atomic(Atom), !.

check_route(Var):-
    var(Var), !.

check_route(/(Left, Right)):-
    check_route(Left),
    check_route(Right), !.

check_route(Route):-
    throw(error(invalid_route(Route))).

% Matches route to path.
% This similar to route-route match
% but a route variable can match atomic
% value in the path.

route_path_match(Route, /):- !,
    nonvar(Route),
    Route = '/'.

route_path_match(Route, Atomic):-
    atomic(Atomic), !,
    Route = Atomic.

route_path_match(Route, /(LeftPath, RightPath)):-
    nonvar(Route), !,
    Route = /(LeftRoute, RightRoute),
    route_path_match(LeftRoute, LeftPath),
    route_path_match(RightRoute, RightPath).

% Matches two routes to detect
% "same" routes. Does not bind
% variables between them

route_route_match(Root1, Root2):-
    nonvar(Root1),
    nonvar(Root2),
    Root1 = '/',
    Root2 = '/', !.

route_route_match(Atomic1, Atomic2):-
    atomic(Atomic1),
    atomic(Atomic2),
    Atomic1 \= '/',
    Atomic1 = Atomic2, !.

route_route_match(Var1, Var2):-
    var(Var1),
    var(Var2), !.

route_route_match(Route1, Route2):-
    nonvar(Route1),
    nonvar(Route2),
    Route1 = /(Left1, Right1),
    Route2 = /(Left2, Right2),
    route_route_match(Left1, Left2),
    route_route_match(Right1, Right2).

% Finds clause references of all
% matching routes.

existing_route(Method, Route, Ref):-
    clause(route(Method, RouteTest, _, _), _, Ref),
    route_route_match(Route, RouteTest).

% Same as above but finds all matching routes.

existing_routes(Method, Route, Refs):-
    findall(Ref, existing_route(Method, Route, Ref), Refs).

%! route_remove(+Method, +Route) is det.
%
% Removes the given route. When either Method
% or Route or both are not set or are partially
% instantiated then all matching routes are removed.
% Method can be left unbound.

route_remove(Method, Route):-
    check_route(Route),
    existing_routes(Method, Route, Refs),
    remove_refs(Refs).

%! route_remove_b(+Blueprint, +Method, +Route) is det.
%
% Removes the given route. When either Method
% or Route or both are not set or are partially
% instantiated then all matching routes are removed.
% Method can be left unbound.

route_remove_b(Blueprint, Method, Route):-
    blueprint_rec(Blueprint, Prefix),
    path_to_route(Prefix, Route1),
    concat_route(Route1, Route, FullRoute),
    check_route(FullRoute),
    existing_routes(Method, FullRoute, Refs),
    remove_refs(Refs).

remove_refs([Ref|Refs]):-
    erase(Ref),
    remove_refs(Refs).

remove_refs([]).

%! route(+Request) is semidet.
%
% Routes the request into an handler.
% Fails when no handler is found.
% Request must contain method(Method)
% and path(Path).
% Throws handler_failed(Method, Path) when
% handler was found but it failed during
% execution.
    
route(Request):-
    memberchk(method(Method), Request),
    memberchk(path(Path), Request),
    path_to_route(Path, Route),
    debug(arouter, 'dispatch: ~p ~p', [Method, Route]),
    setup_call_cleanup(
        asserta(request(Request)),
        dispatch(Method, Route),
        retract(request(Request))
    ).

%! route_with_fallbacks(+Fallbacks, +Request) is semidet.
%
% Routes the request into an handler.
% Fails when no handler is found.
% Request must contain method(Method)
% and path(Path).
% Throws handler_failed(Method, Path) when
% handler was found but it failed during
% execution.
% If no handlers could be found, fallbacks
% will be called until one succeeds or the
% whole predict fails.

route_with_fallbacks(Fallbacks, Request):-
    (   route(Request)
    ->  true
    ;   setup_call_cleanup(
            asserta(request(Request)),
            try_fallbacks(Fallbacks),
            retract(request(Request))
        )
    ).

try_fallbacks([]):- false.
try_fallbacks([H|T]):-
    (   run_handler(H)
    ->  true
    ;   try_fallbacks(T)
    ).

%! dispatch(+Method, +Route) is semidet.
%
% Attempts to dispatch the request.
% Fails when no matching handler is found.
% Throws handler_failed(Method, Path) when
% handler was found but it failed during
% execution.

dispatch(Method, Path):-
    route(Method, Route, Before, Goal),
    route_path_match(Route, Path), !,
    (   run_handler(Before, Goal)
    ->  true
    ;   throw(error(handler_failed(Method, Path)))).

:- meta_predicate(run_handler(+, 0)).

run_handler(Before, Goal):- !,
    (   Before = goal(BeforeGoal)
    ->  call(BeforeGoal, arouter:run_handler(Goal))
    ;   run_handler(Goal)).

:- meta_predicate(run_handler(0)).

run_handler(Handler):-
    call(Handler).

%! concat_route(+Route1, +Route2, -Route) is det.
%
% concat a/b/c and d/e to a/b/c/d/e.
%

concat_route(R1, R2, Route) :-
    \+ compound(R2), !,
    Route = R1/R2.

concat_route(R1, /(R21, R22), Route) :-
    concat_route(R1/R21, R22, Route).

%! path_to_route(+Path, -Route) is det.
%
% Turns path atom like '/path/to/something' into
% a Prolog term path/to/something.
    
path_to_route(Path, Route):-    
    atom_codes(Path, Codes),
    phrase(path_tokens([/|Tokens]), Codes),
    path_to_route_term(Tokens, Route), !.
    
path_to_route_term([], /).
    
path_to_route_term([First|Rest], Term):-
    path_to_route_term(Rest, First, Term).
    
path_to_route_term([/,A|Rest], Acc, Term):-
    path_to_route_term(Rest, /(Acc, A), Term).

path_to_route_term([A], Acc, Route):-
    (   A = (/)
    ->  Route = /(Acc, '')
    ;   Route = /(Acc, A)).

path_to_route_term([], Acc, Acc).

path_tokens([Token|Tokens]) -->
    path_token(Token),
    path_tokens(Tokens).
    
path_tokens([]) --> [].

path_token(/) --> "/", !.

path_token(Atom) -->
    path_char(Char), !,
    path_char_token(Chars),
    { atom_chars(Atom, [Char|Chars]) }.

path_char_token([Char|Chars]) -->
    path_char(Char), !,
    path_char_token(Chars).
    
path_char_token([]) --> [].

path_char(Char) --> [Char], { Char \= 0'/ }.
