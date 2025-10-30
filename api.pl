:- module(api, [start_server/1]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(products).  % Importa los hechos

:- http_handler(root(products), products_handler, []).
:- http_handler(root(category), category_handler, []).
:- http_handler(root(update), update_stock, []).

start_server(Port) :-
    http_server(http_dispatch, [port(Port)]),
    writeln('Servidor iniciado en el puerto ':Port).

% ========================
% Endpoint de productos
% ========================

% Búsqueda insensible, encuentra BANANO o Banano o banano
% Endpoint "GET": /products?name=Banano
products_handler(Request) :-
    http_parameters(Request, [name(QueryAtom, [atom])]),
    downcase_atom(QueryAtom, QueryLower),
    findall(
        json{code:Code, name:Name, category:Category, price:Price, stock:Stock, unit:Unit, description:Description},
        (
            product(Code, Name, Category, Price, Stock, Unit, Description),
            atom_string(Name, NameStr),
            string_lower(NameStr, NameLower),
            sub_string(NameLower, _, _, _, QueryLower)
        ),
        Results
    ),
    reply_json(Results).

% Endpoint "UPDATE" para stock:
%http://localhost:8080/update?code=P0001&quantity=50

update_stock(Request) :-
    http_parameters(Request, [
        code(Code, [atom]),
        quantity(Quantity, [integer])
    ]),
    (   product(Code, Name, Category, Price, Stock, Unit, Description)
    ->  (   Stock >= Quantity
        ->  NewStock is Stock - Quantity,
            retract(product(Code, Name, Category, Price, Stock, Unit, Description)),
            assertz(product(Code, Name, Category, Price, NewStock, Unit, Description)),
            update_csv(Code, NewStock),
            reply_json(json{status: "Stock actualizado", code: Code, new_stock: NewStock})
        ;   reply_json(json{error: "Stock insuficiente"}, [status(400)])
        )
    ;   reply_json(json{error: "Producto no encontrado"}, [status(404)])
    ).

% Endpoint "GET" para categoría:
%% Endpoint: /category?type=frutas
category_handler(Request) :-
    http_parameters(Request, [type(TypeStr, [atom])]),
    atom_string(Type, TypeStr),
    findall(
        json{name:Name, stock:Stock},
        product(_, Name, Type, _, Stock, _, _),
        Results
    ),
    reply_json(Results).
