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

% librerias
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/html_write)).

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

:- initialization(server(8000)). 

% URL handlers.
:- http_handler('/', handle_request, []). % escuchamos en /
:- http_handler('/partido', to_do, []).
:- http_handler('/tabla', to_do, []).
:- http_handler('/pronostico/partido', to_do, []).
:- http_handler('/pronostico/tabla', to_do, []).

% metodo que responde un to do para demstrar que no esta implementado aun
to_do(Request) :-
    reply_json_dict('To do').

handle_request(Request) :-
    reply_html_page(title('Titulo :3'),
        [ h1('Bienvenido'),
            p('Aquí se mostraran los posibles cosos de api rest')
        ]).