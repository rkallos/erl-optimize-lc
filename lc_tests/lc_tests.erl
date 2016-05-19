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

% Produce a lazy sequence of [M..N] of the form [val|thunk]
% Functions that consume lists in a recursive fashion should be able to consume this properly...?
lazy_seq(M,N) when M > N -> [];
lazy_seq(M,N) -> [M | fun() -> lazy_seq(M+1,N) end].

lazy_foldr(F, Acc, [Hd|Tl]) ->
    F(Hd, lazy_foldr(F, Acc, apply(Tl,[])));
lazy_foldr(_F, Acc, []) -> Acc.

% Produce a lazy sequence of [M..N] via contination passing
cps_cons(Head, Tail, K) ->
    K([Head|Tail]).

lazy_cp_seq(M,N) ->
    lazy_cp_seq(M,N,fun(Pair) -> Pair end).
lazy_cp_seq(M,N,K) ->
    if N < M -> [];
       true -> [M | fun() -> lazy_cp_seq(M+1, N, fun(Lst) -> cps_cons(M,Lst,K) end) end]
    end.

% To run the tests, wrap each function inside a new process, then send the input as a message
% This granularity helps to inspect process heap usage

stats() -> [memory, heap_size, total_heap_size, stack_size, garbage_collection].

test(Name, Fn, Args) when is_list(Args) ->
    Me = self(),

    Pid = spawn(
      fun() ->
              Before = process_info(self(), stats()),
              Res = apply(Fn, Args),
              %garbage_collect(),
              After = process_info(self(), stats()),
              Me ! {Before, After, Res}
      end),
    Pid ! Me,
    receive
        {B, A, R} -> {Name, Pid, {B, A}, R}
    end;
test(Name, Fn, Arg) ->
    test(Name, Fn, [Arg]).

run_tests([], Pid, Acc) ->
    timer:sleep(100),
    Pid ! self(),
    receive
        Map -> [{Name,maps:get(Proc, Map, 0),{Before,After},Result} ||
                   {Name,Proc,{Before,After},Result} <- Acc]
    end;
run_tests([{Name, Fn, Args}|Tail], Pid, Acc) ->
    Res = test(Name, Fn, Args),
    run_tests(Tail, Pid, [Res|Acc]).

run_tests(Lst) when is_list(Lst) ->
    Pid = spawn(fun () -> handle_gc_evts() end),
    timer:sleep(100),
    run_tests(Lst, Pid, []).

handle_gc_evts() ->
    erlang:trace(new, true, [garbage_collection]),
    handle_gc_evts(maps:new()).
handle_gc_evts(Map) ->
    receive
        {trace,Pid,Type,Details} -> handle_gc_evts(handle_gc(Map, Pid, Type, Details));
        Pid -> begin
                 erlang:trace(new, false, [garbage_collection]),
                 Pid ! Map
               end
    end.

handle_gc(Map, Pid, Type, Details) ->
    HeapSize = proplists:get_value(heap_size,Details),
    case maps:get(Pid, Map, undefined) of
        undefined -> maps:put(Pid, HeapSize, Map);
        Frees -> case Type of
                      gc_start -> maps:put(Pid, Frees+HeapSize, Map);
                      gc_end -> maps:put(Pid, Frees-HeapSize, Map)
                  end
        %% undefined -> maps:put(Pid, [{Type,proplists:get_value(heap_size,Details)}], Map);
        %% Lst -> maps:put(Pid, [{Type,proplists:get_value(heap_size,Details)}|Lst], Map)
    end.


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
     {"pen2_dumb", fun pen2_dumb/1, [IntArg]},
     {"pen2_foldr", fun pen2_foldr/1, [IntArg]},
     {"pen2_foldl", fun pen2_foldl/1, [IntArg]},
     {"pen2_listsfuns", fun pen2_listsfuns/1, [IntArg]},
     {"pen2_mr", fun pen2_mr/1, [IntArg]},
     {"pen2_lazy", fun pen2_lazy/1, [IntArg]},
     {"pen2_lazy_cps", fun pen2_lazy_cps/1, [IntArg]},
     {"single_filter", fun single_filter/1, [LstArg]},
     {"record_extract", fun record_extract/1, [RecArg]},
     {"function_param", fun function_param_lc/2, [param, LstArg]},
     {"sorted_gen", fun sorted_gen/1, [RecArg]},
     {"make_n", fun make_n/2, [{param1, param2}, IntArg]},
     {"intermediary_list", fun intermediary_list/1, [IntArg]},
     {"intermediary_list2", fun intermediary_list2/1, [IntArg]},
     {"intermediary_list3", fun intermediary_list3/1, [IntArg]},
     {"nested_lc_gen", fun nested_lc_gen/1, [LstArg]},
     {"unused", fun unused/0, []}
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

% Equivalent to pen2, but doesn't generate any intermediate list
pen2_dumb(N) ->
    pen2_dumb_helper(N, 1, 1, []).
pen2_dumb_helper(N, X, _Y, L) when X > N ->
    L;
pen2_dumb_helper(N, X, Y, L) when Y > N ->
    pen2_dumb_helper(N, X+1, 0, L);
pen2_dumb_helper(N, X, Y, L) when X rem 2 =:= 0 andalso Y rem 2 =:= 1 andalso (X+Y) rem 5 /= 0 ->
    [[X,Y]|pen2_dumb_helper(N, X, Y+1, L)];
pen2_dumb_helper(N, X, Y, L) ->
    pen2_dumb_helper(N, X, Y+1, L).

% Equivalent to pen2, but uses foldr instead of a list comprehension
pen2_foldl(N) when is_integer(N) ->
    Gen = lists:seq(1,N),
    lists:reverse(lists:foldl(
      fun(X, Acc1) ->
              if X rem 2 =:= 0 ->
                      lists:foldl(
                        fun(Y, Acc2) ->
                                if Y rem 2 =:= 1 andalso (X + Y) rem 5 /= 0 -> [[X,Y]|Acc2];
                                   true -> Acc2
                                end
                        end,
                        [], Gen) ++ Acc1;
                 true -> Acc1
              end
      end, [], Gen)).

pen2_foldr(N) when is_integer(N) ->
    Gen = lists:seq(1,N),
    lists:foldr(
      fun(X, Acc1) ->
              if X rem 2 =:= 0 ->
                      lists:foldr(
                        fun(Y, Acc2) ->
                                if Y rem 2 =:= 1 andalso (X + Y) rem 5 /= 0 -> [[X,Y]|Acc2];
                                   true -> Acc2
                                end
                        end,
                        [], Gen) ++ Acc1;
                 true -> Acc1
              end
      end, [], Gen).

pen2_listsfuns(N) when is_integer(N) ->
    Gen = lists:seq(1,N),
    lists:filter(fun([X,Y]) -> (X + Y) rem 5 /= 0 end,
                 [[X,Y] || X <- lists:filter(fun(X) -> X rem 2 =:= 0 end, Gen),
                           Y <- lists:filter(fun(Y) -> Y rem 2 =:= 1 end, Gen)]).

% Uses mutually recursive funs to generate final product
pen2_mr(N) when is_integer(N) ->
    Gen = lists:seq(1,N),
    RedYs = fun Red_ys(AX = [X|_Xs], [Y|Ys], MR) ->
                    if Y rem 2 =:= 1 andalso (X + Y) rem 5 /= 0 ->
                            [[X,Y]|Red_ys(AX, Ys, MR)];
                       true -> Red_ys(AX, Ys, MR)
                    end;
                Red_ys([_X|Xs], [], MR) -> MR(Xs) end,
    RedXs = fun Red_xs(AX = [X|Xs]) ->
                    if X rem 2 =:= 0 -> RedYs(AX, Gen, Red_xs);
                       true -> Red_xs(Xs)
                    end;
                Red_xs([]) -> []
            end,
    RedXs(Gen).

% Uses lazy_seq instead of lists:seq()
pen2_lazy(N) when is_integer(N) ->
    Gen = lazy_seq(1,N),
    RedYs = fun Red_ys(AX = [X|_Xs], [Y|Ys], MR) ->
                    if Y rem 2 =:= 1 andalso (X + Y) rem 5 /= 0 ->
                            [[X,Y]|Red_ys(AX, Ys(), MR)];
                       true -> Red_ys(AX, Ys(), MR) end;
                Red_ys([_X|Xs], [], MR) -> MR(Xs()) end,
    RedXs = fun Red_xs(AX = [X|Xs]) ->
                    if X rem 2 =:= 0 -> RedYs(AX, Gen, Red_xs);
                       true -> Red_xs(Xs())
                    end;
                Red_xs([]) -> [] end,
    RedXs(Gen).

% pen2 using continuation passing
pen2_lazy_cps(N) when is_integer(N) ->
    Gen = lazy_cp_seq(1,N),
    RedYs = fun Red_ys(AX = [X|_Xs], [Y|Ys], MR) ->
                    if Y rem 2 =:= 1 andalso (X + Y) rem 5 /= 0 ->
                            [[X,Y]|Red_ys(AX, Ys(), MR)];
                       true -> Red_ys(AX, Ys(), MR) end;
                Red_ys([_X|Xs], [], MR) -> MR(Xs()) end,
    RedXs = fun Red_xs(AX = [X|Xs]) ->
                    if X rem 2 =:= 0 -> RedYs(AX, Gen, Red_xs);
                       true -> Red_xs(Xs())
                    end;
                Red_xs([]) -> [] end,
    RedXs(Gen).


% Unused LC result
unused() ->
    _ = [[X, timer:sleep(1)] || X <- lists:seq(1, 1000)],
    ok.

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
