:- module(orders, [log_order/8]).
:- use_module(library(csv)).

% =========================
% Generar código incremental leyendo el último del CSV
% =========================
next_order_code(Code) :-
    (   exists_file('orders.csv')
    ->  % Leer archivo y obtener la última línea
        csv_read_file('orders.csv', Rows, [functor(row), arity(8)]),
        (   last(Rows, LastRow),
            arg(8, LastRow, LastCode),
            atom_concat('ORD', NumAtom, LastCode),
            catch(atom_number(NumAtom, Num), _, Num = 0),
            NextNum is Num + 1
        ->  true
        ;   NextNum = 1
        )
    ;   NextNum = 1
    ),
   
    format(atom(Code), 'ORD~`0t~d~4+', [NextNum]).

% =========================
% Registrar una orden 
% =========================
log_order(Date, ProductCode, ProductName, Quantity, Place, ReceiverName, Status, OrderCode) :-
    next_order_code(OrderCode),
    (   \+ exists_file('orders.csv')
    ->  open('orders.csv', write, Stream, [encoding(utf8)]),
        csv_write_stream(Stream, [row('Date','Code','Product','Quantity','DeliveryPlace','ReceiverName','Status','OrderCode')], []),
        close(Stream)
    ;   true
    ),
    open('orders.csv', append, Stream, [encoding(utf8)]),
    csv_write_stream(Stream,
        [row(Date, ProductCode, ProductName, Quantity, Place, ReceiverName, Status, OrderCode)], []),
    close(Stream).
