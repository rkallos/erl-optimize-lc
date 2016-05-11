-module(lc_tests).
-compile([export_all, 'P']).

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

test(Fn, Args) when is_list(Args) ->
    Pid = spawn(
            fun() ->
                    receive
                        {Parent, Args} ->
                            begin
                                Before = process_info(self(), stats()),
                                apply(Fn, Args),
                                %garbage_collect(),
                                After = process_info(self(), stats()),
                                Parent ! {self(), Before, After}
                            end
                    end
            end),
    Pid ! {self(), Args},
    receive
        {Cid, B, A} -> io:format("~p~n", [{Cid, B, A}])
    end;
test(Fn, Arg) ->
    test(Fn, [Arg]).

% -------------------- Basic Cases

% Sum of squares: Produces 1 intermediary list
ss(N) ->
   lists:sum([X * X || X <- lists:seq(1,N) ]).

% Sum of even squares: Produces 1 intermediary list
ses(N) ->
    lists:sum([X * X || X <- lists:seq(1,N),
                        X rem 2 =:= 0 ]).

% (even,odd) pairs: Produces 2 intermediary lists
pen(N) ->
    [[X,Y] || X <- lists:seq(1,N),
              X rem 2 =:= 0,
              Y <- lists:seq(1,N),
              Y rem 2 =:= 1].

% (even,odd) pairs that don't add up to multiples of 5
pen2(N) ->
    [[X,Y] || X <- lists:seq(1,N),
              X rem 2 =:= 0,
              Y <- lists:seq(1,N),
              Y rem 2 =:= 1,
              (X + Y) rem 5 /= 0].

% -------------------- Tests from rtb-boolean

% Single filter expr
single_filter(N) ->
    [Entry || Entry <- N,
              is_integer(Entry)].

% Extract record value from record

record_extract(N) ->
    [Entry#test_record.num1 || Entry <- N].

% [P1,V], V <- P2
function_param_lc(P1, P2) ->
    [[P1,V] || V <- P2].

% Using a sorted list as a generator
sorted_gen(L) ->
    [Entry#test_record.num1 ||
        Entry <- lists:sort(fun(X,Y) ->
                                    X#test_record.num1 < Y#test_record.num1
                            end, L)].

% -------------------- Tests from rtb-gateway

% _ <- Generator
% Allocates a list to the heap just to repeat a structure N times
make_n(V,N) ->
    [V || _ <- lists:seq(1,N)].
    
% Bound list as generator. Can be inlined
intermediary_list(L) ->
    Inter = random_test_records(L, 50),
    [Entry#test_record.num2 || Entry <- Inter].

intermediary_list2(L) ->
    Inter1 = random_test_records(L, 50),
    Inter2 = [Entry#test_record.num2 || Entry <- Inter1],
    [X*X || X <- Inter2].

intermediary_list3(L) ->
    Inter1 = random_test_records(L, 50),
    Inter2 = [{random:uniform(50), Entry} || Entry <- Inter1],
    Inter3 = [{random:uniform(100), Entry} || Entry <- Inter2],
    [A*B*C*D || {A, {B, #test_record{num1 = C, num2 = D}}} <- Inter3].

% Nested LCs
% TODO: [[ || ] || ]

% [ || [ || ]]
nested_lc_gen(L) ->
    [ #test_record{num1 = Entry, num2 = Entry * Entry} ||
        Entry <- [ Val || Val <- L,
                          Val rem 2 =:= 1]].

% TODO: Single element list as generator
