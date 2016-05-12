-module(lc_tests).
-compile([export_all]).

% List Comprehension Syntax
% [ Expr || Qual1,...Qualn ]

% Qualifier syntax
% Generator: Val <- Vals
% Filter: Expr which must evaluate to true or false

% -------------------- Auxiliary Functions and Records
-record(test_record, {num1, num2}).

random(List) ->
    lists:nth(random:uniform(length(List)), List).

random_bin(Length) ->
    list_to_binary([random:uniform(255) || _ <- lists:seq(1, Length)]).

rng(0, _N) -> [];
rng(Length, N) ->
    [random:uniform(N) | rng(Length - 1, N)].

random_test_records(0, _N) -> [];
random_test_records(Length, N) ->
    [#test_record{num1=random:uniform(N), num2=random:uniform(N)} |
      random_test_records(Length - 1, N)].

% To run the tests, wrap each function inside a new process, then send the input as a message
% This granularity helps to inspect process heap usage

stats() -> [memory, heap_size, total_heap_size, stack_size, garbage_collection].

test(Name, Fn, Args) when is_list(Args) ->
    Me = self(),
    spawn(
      fun() ->
              Before = process_info(self(), stats()),
              Res = apply(Fn, Args),
              %garbage_collect(),
              After = process_info(self(), stats()),
              Me ! {Before, After, Res}
      end),
    receive
        {B, A, R} -> {Name, {B, A}, R}
    end;
test(Name, Fn, Arg) ->
    test(Name, Fn, [Arg]).

run_tests([], Acc) ->
    Acc;
run_tests([{Name, Fn, Args}|Tail], Acc) ->
    Res = test(Name, Fn, Args),
    run_tests(Tail, [Res|Acc]).

run_tests(Lst) when is_list(Lst) ->
    run_tests(Lst, []).

int_arg() ->
    50.
list_arg() ->
    rng(50, 50).
record_arg() ->
    random_test_records(50, 50).

all_tests() ->
    IntArg = 500,
    LstArg = rng(500, 50),
    RecArg = random_test_records(500, 50),
    [{"ss", fun ss/1, [IntArg]},
     {"ses", fun ses/1, [IntArg]},
     {"pen", fun pen/1, [IntArg]},
     {"pen2", fun pen2/1, [IntArg]},
     {"single_filter", fun single_filter/1, [LstArg]},
     {"record_extract", fun record_extract/1, [RecArg]},
     {"function_param", fun function_param_lc/2, [param, LstArg]},
     {"sorted_gen", fun sorted_gen/1, [RecArg]},
     {"make_n", fun make_n/2, [{param1, param2}, IntArg]},
     {"intermediary_list", fun intermediary_list/1, [IntArg]},
     {"intermediary_list2", fun intermediary_list2/1, [IntArg]},
     {"intermediary_list3", fun intermediary_list3/1, [IntArg]},
     {"nested_lc_gen", fun nested_lc_gen/1, [LstArg]}
    ].

% -------------------- Basic Cases

% Sum of squares: Produces 1 intermediary list
ss(N) when is_integer(N) ->
   lists:sum([X * X || X <- lists:seq(1,N) ]).

% Sum of even squares: Produces 1 intermediary list
ses(N) when is_integer(N) ->
    lists:sum([X * X || X <- lists:seq(1,N),
                        X rem 2 =:= 0 ]).

% (even,odd) pairs: Produces 2 intermediary lists
pen(N) when is_integer(N) ->
    [[X,Y] || X <- lists:seq(1,N),
              X rem 2 =:= 0,
              Y <- lists:seq(1,N),
              Y rem 2 =:= 1].

% (even,odd) pairs that don't add up to multiples of 5
pen2(N) when is_integer(N) ->
    [[X,Y] || X <- lists:seq(1,N),
              X rem 2 =:= 0,
              Y <- lists:seq(1,N),
              Y rem 2 =:= 1,
              (X + Y) rem 5 /= 0].

% -------------------- Tests from rtb-boolean

% Single filter expr
single_filter(N) when is_list(N) ->
    [Entry || Entry <- N,
              is_integer(Entry)].

% Extract record value from record

record_extract(N) when is_list(N) ->
    [Entry#test_record.num1 || Entry <- N].

% [P1,V], V <- P2
function_param_lc(P1, P2) when is_list(P2) ->
    [[P1,V] || V <- P2].

% Using a sorted list as a generator
sorted_gen(L) when is_list(L) ->
    [Entry#test_record.num1 ||
        Entry <- lists:sort(fun(X,Y) ->
                                    X#test_record.num1 < Y#test_record.num1
                            end, L)].

% -------------------- Tests from rtb-gateway

% _ <- Generator
% Allocates a list to the heap just to repeat a structure N times
make_n(V,N) when is_integer(N) ->
    [V || _ <- lists:seq(1,N)].
    
% Bound list as generator. Can be inlined
intermediary_list(Len) when is_integer(Len) ->
    Inter = random_test_records(Len, 50),
    [Entry#test_record.num2 || Entry <- Inter].

intermediary_list2(Len) when is_integer(Len) ->
    Inter1 = random_test_records(Len, 50),
    Inter2 = [Entry#test_record.num2 || Entry <- Inter1],
    [X*X || X <- Inter2].

intermediary_list3(Len) when is_integer(Len) ->
    Inter1 = random_test_records(Len, 50),
    Inter2 = [{random:uniform(50), Entry} || Entry <- Inter1],
    Inter3 = [{random:uniform(100), Entry} || Entry <- Inter2],
    [A*B*C*D || {A, {B, #test_record{num1 = C, num2 = D}}} <- Inter3].

% Nested LCs
% TODO: [[ || ] || ]

% [ || [ || ]]
nested_lc_gen(Lst) when is_list(Lst) ->
    [ #test_record{num1 = Entry, num2 = Entry * Entry} ||
        Entry <- [ Val || Val <- Lst,
                          Val rem 2 =:= 1]].

% TODO: Single element list as generator
