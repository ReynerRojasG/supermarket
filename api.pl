:- module(api, [start_server/1]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).
:- set_setting(http:cors, [*]).
:- use_module(products).  % Importa los hechos

:- http_handler(root(products), products_handler, []).
:- http_handler(root(category), category_handler, []).
:- http_handler(root(update), update_stock, []).

:- use_module(library(csv)).       
:- use_module(routes).              
:- use_module(orders).

:- http_handler(root(order/route), order_route_handler, []).
:- http_handler(root(orders), orders_post_handler, []).
:- http_handler(root(orders/list), orders_get_handler, []).
:- http_handler(root(orders/update), update_order_status_handler, []).
:- http_handler(root(orders/create), orders_create_handler, []).

% ========================
% Iniciar servidor
% ========================
start_server(Port) :-
    http_server(http_dispatch, [port(Port)]),
    format('Servidor iniciado en el puerto ~w~n', [Port]).

% ========================
% Endpoint: /products?name=Banano
% Descripción: Busca productos por nombre (insensible a mayúsculas)
% ========================
products_handler(Request) :-
    cors_enable(Request, [methods([get])]),
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

% ========================
% Endpoint: /update?code=P0001&quantity=5
% Descripción: Descuenta stock de un producto
% ========================
update_stock(Request) :-
    cors_enable(Request, [methods([get, post])]),
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
            reply_json(json{status: "Stock actualizado", code: Code, new_stock:NewStock})
        ;   reply_json(json{error: "Stock insuficiente"}, [status(400)])
        )
    ;   reply_json(json{error: "Producto no encontrado"}, [status(404)])
    ).

% ========================
% Endpoint: /category?type=frutas
% Descripción: Lista productos por categoría
% ========================
category_handler(Request) :-
    cors_enable(Request, [methods([get])]),
    http_parameters(Request, [type(TypeStr, [atom])]),
    atom_string(Type, TypeStr),
    findall(
        json{name:Name, stock:Stock},
        product(_, Name, Type, _, Stock, _, _),
        Results
    ),
    reply_json(Results).

normalize_place(Input, OutAtom) :-
    (   atom(Input)   -> atom_string(Input, S0)
    ;   string(Input) -> S0 = Input
    ;   term_string(Input, S0)
    ),
    normalize_space(string(S1), S0),  
    string_upper(S1, S2),           
    atom_string(OutAtom, S2).

% ========================
% Endpoint: /order/route?order=ORD0001
% Descripción: Calcula ruta desde origen hasta destino de la orden
% ========================
order_route_handler(Request) :-
    cors_enable(Request, [methods([get])]),
    http_parameters(Request, [order(OrderCode, [atom])]),
    (   delivery_place_from_csv(OrderCode, Place0)
    ->  normalize_place(Place0, Place), 
        default_origin(Origin),
        (   route_path(Origin, Place, Path)
        ->  length(Path, Hops),
            reply_json(json{
                order: OrderCode,
                origin: Origin,
                delivery_place: Place,
                steps: Path,      
                hops: Hops
            })
        ;   reply_json(json{
                order: OrderCode,
                error: "No route found from origin to delivery place",
                origin: Origin,
                delivery_place: Place
            }, [status(404)])
        )
    ;   reply_json(json{error: "Order not found", order: OrderCode}, [status(404)])
    ).

% ========================
% Helper: Extrae lugar de entrega desde orders.csv
% ========================
delivery_place_from_csv(OrderCode, Place) :-
    exists_file('orders.csv'),
    csv_read_file('orders.csv', Rows, [functor(row), arity(8)]),
    (   Rows = [row('Date','Code','Product','Quantity','DeliveryPlace','ReceiverName','Status','OrderCode')|Data]
    ->  true
    ;   Data = Rows
    ),
    member(Row, Data),
    arg(8, Row, OrderCode),  
    arg(5, Row, Place).       

% ========================
% Endpoint: /orders/create?...
% Descripción: Crea una orden vía GET
% ========================
orders_create_handler(Request) :-
    cors_enable(Request, [methods([get])]),
    http_parameters(Request, [
        code(Code, []),
        name(Name, []),
        quantity(Quantity, [integer]),
        deliveryPlace(DeliveryPlace, []),
        receiverName(ReceiverName, []),
        date(Date, []),
        status(Status, [optional('Pendiente')])
    ]),
    next_order_code(OrderCode),
    log_order(Date, Code, Name, Quantity, DeliveryPlace, ReceiverName, Status, OrderCode),
    reply_json(json{
        status:"OK",
        orderCreated:OrderCode
    }).

% ========================
% Endpoint: /orders/list
% Descripción: Lista todas las órdenes registradas
% ========================
orders_get_handler(Request) :-
    cors_enable(Request, [methods([get])]),
    (   exists_file('orders.csv')
    ->  csv_read_file('orders.csv', Rows, [functor(row), arity(8)]),
        Rows = [_H|Data],
        findall(
            json{date:D, code:C, product:P, quantity:Q, deliveryPlace:DP, receiverName:RN, status:S, orderCode:OC},
            (member(row(D,C,P,Q,DP,RN,S,OC), Data)),
            JsonList
        ),
        reply_json(JsonList)
    ;   reply_json([], [status(200)])
    ).

% ========================
% Endpoint: /orders/update?orderCode=ORD0001&status=Entregado
% Descripción: Actualiza el estado de una orden
% ========================
update_order_status_handler(Request) :-
    cors_enable(Request, [methods([get, post])]),
    http_parameters(Request, [
        orderCode(OrderCodeAtom, [atom]),
        status(StatusAtom, [atom])
    ]),
    
    atom_string(OrderCode, OrderCodeAtom),
    atom_string(NewStatus, StatusAtom),

    csv_read_file('orders.csv', Rows, [functor(row), arity(8)]),
    Rows = [Header|Rest],

    (
        member(row(D,C,P,Q,DP,RN,_Old,OrderCode), Rest)
    ->
        maplist(
            replace_order(OrderCode, NewStatus),
            Rest,
            NewRest
        ),

        open('orders.csv', write, S, [encoding(utf8)]),
        csv_write_stream(S, [Header|NewRest], []),
        close(S),

        reply_json(json{
            status:"actualizado",
            order:OrderCode,
            newStatus:NewStatus
        })
    ;
        reply_json(json{error:"Orden no encontrada"}, [status(404)])
    ).
% ========================
% Helper: Reemplaza estado de orden específica
% ========================
replace_order(OrderCode, NewStatus, row(D,C,P,Q,DP,RN,_Old,OrderCode),
                                row(D,C,P,Q,DP,RN,NewStatus,OrderCode)) :- !.
replace_order(_, _, Row, Row).