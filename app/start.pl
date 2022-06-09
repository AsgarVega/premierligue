/*
 * Ejemplo sacado de https://www.monolune.com/articles/web-programming-in-swi-prolog/
 */


/* 
    :- use_module(library(http/thread_httpd)).
    :- use_module(library(http/http_dispatch)).
    :- use_module(library(http/http_json)).

    % URL handlers.
    :- http_handler('/', handle_request, []).

    % Calculates a + b.
    solve(_{a:X, b:Y}, _{answer:N}) :-
        number(X),
        number(Y),
        N is X + Y.

    handle_request(Request) :-
        http_read_json_dict(Request, Query),
        solve(Query, Solution),
        reply_json_dict(Solution).

    server(Port) :-
        http_server(http_dispatch, [port(Port)]).

    :- initialization(server(8000)). 
 */

/*
    library(http/thread_httpd)
        This library is responsible for accepting and
        managing connections.2
    library(http/http_dyn_workers)
        This library dynamically adds and removes 
        workers based on the workload of the server.
    library(http/http_wrapper)
        This library takes a connection, parses the 
        HTTP request header and runs a goal that pro-
        duces a CGI document based on the parsed request.
        It watches for exceptions and turns these into
        (error) status pages. The status page generation
        may be hooked to provide custom pages.
    library(http/http_dispatch)
        This library associates the path of the HTTP 
        request with a handler that services this 
        particular path. It also manages timeouts and 
        may pass the execution of a request to a dedi-
        cated thread with specified resource limits 
        using http_spawn/2. The module supports pluga-
        ble request rewrite handlers that may be used 
        to implement identification, authorization, 
        input argument processing, etc.
    library(http/http_parameters)
        This library parses HTTP request parameters, 
        both dealing with GET and POST style parameter
        passing.
    library(http/html_write)
        This library translates a Prolog term into an 
        HTML document using Prolog grammar rules (DCG).
        It provides a modular infrastructure to build 
        pages that are guaranteed to be valid HTML. The
        HTTP server libraries provide several alternatives 
        for generating HTML ranging from simple printing 
        to current_output to XML-based templates (PWP).
    library(http/http_json)
        This library parses a POSTed HTTP document into
        a Prolog dict and formulates an HTTP JSON reply
        from a Prolog dict and is typically used to 
        implement REST services.

    https://www.swi-prolog.org/pldoc/man?section=httpserver
*/
%importamos las reglas del sistema del Sistema experto
:- include(reglas).

% librerias
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert)).
% ----------------------------------------------------------------------------------------------
% ----------------Server------------------------------------------------------------------------
% ----------------------------------------------------------------------------------------------

:- json_object
      point(x:integer, y:integer).

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

:- initialization(server(8000)). 

% URL handlers.
:- http_handler('/', handle_request, []). % escuchamos en /
:- http_handler('/partido', to_do, []).
:- http_handler('/tabla', to_do, []).
:- http_handler('/pronostico/partido', to_do, []).
:- http_handler('/pronostico/tabla', to_do, []).
:- http_handler('/equipos',response_es_un_equipo,[]).

% metodo que responde un to do para demstrar que no esta implementado aun
to_do(Request) :-
    atomics_to_string(Request,Text),
    string_concat('ToDo',Text,R),
    reply_json_dict(R).

handle_request(Request) :-
    reply_html_page(title('Titulo :3'),
        [ h1('Bienvenido'),
            p('Aquí se mostraran los posibles cosos de api rest')
        ]).
response_es_un_equipo(Request) :-
    es_un_equipo(R).
    reply_html_page(title('Lista de equipos'),
        [h1('lista de equipos'),
            p(R)
        ]).

