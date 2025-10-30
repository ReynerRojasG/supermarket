:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).

:- use_module(products).
:- use_module(api).

start :-
    load_products,
    (   getenv('PORT', PortAtom)
    ->  atom_number(PortAtom, Port)
    ;   Port = 8080
    ),
    format('===> Puerto detectado: ~w~n', [Port]),
    http_server(http_dispatch, [port(Port), bind_address('0.0.0.0')]),
    format('===> Servidor escuchando en todas las interfaces (~w)~n', [Port]),
    writeln('Productos cargados correctamente.'),
    thread_get_message(_).   


% consult('C:/Users/Reyner/Documents/UNA_2025/II_SEMESTRE/PARADIGMAS/Proyecto_prolog/supermarket.pl').
% consult('supermarket.pl').

% ========================
% COMO USAR EL PROYECTO
% 1. Desde swi-prolog ejecutar
% consult('C:/Users/Reyner/Documents/UNA_2025/II_SEMESTRE/PARADIGMAS/Proyecto_prolog/supermarket.pl').


% 3. O con extensión ejecutar: "swipl"
% consult('supermarket.pl').

% ========================
