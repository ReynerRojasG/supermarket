:- module(productos, [load_products/0, product/7, update_csv/2]).
:- use_module(library(csv)).
:- dynamic product/7.

load_products :-
    csv_read_file('products.csv', Rows, [functor(row), arity(7)]),
    Rows = [_ | DataRows],
    maplist(row_to_fact, DataRows).

row_to_fact(row(Code, Name, CategoryStr, Price, Stock, Unit, Description)) :-
    atom_string(Category, CategoryStr),
    assertz(product(Code, Name, Category, Price, Stock, Unit, Description)).

update_csv(Code, NewStock) :-
    csv_read_file('products.csv', Rows, [functor(row), arity(7)]),
    Rows = [Header | DataRows],
    maplist(row_to_list, DataRows, RowLists),
    maplist(update_row_stock(Code, NewStock), RowLists, UpdatedRows),
    open('products.csv', write, Stream),
    csv_write_stream(Stream, [Header | UpdatedRows], []),
    close(Stream).

row_to_list(Row, List) :-
    Row =.. [_|List].

update_row_stock(Code, NewStock, [Code, Name, Category, Price, _, Unit, Description], 
                               row(Code, Name, Category, Price, NewStock, Unit, Description)) :- !.
update_row_stock(_, _, [C, N, Cat, P, S, U, D], row(C, N, Cat, P, S, U, D)).
