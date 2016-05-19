-file("lc_tests_opt.erl", 1).

random(List) ->
    lists:nth(random:uniform(length(List)), List).

random_bin(Length) ->
    list_to_binary([ 
                    random:uniform(255) ||
                        _ <- lists:seq(1, Length)
                   ]).

rng(0, _N) ->
    [];
rng(Length, N) ->
    [random:uniform(N)|rng(Length - 1, N)].

random_test_records(0, _N) ->
    [];
random_test_records(Length, N) ->
    [{test_record,random:uniform(N),random:uniform(N)}|
     random_test_records(Length - 1, N)].

lazy_seq(M, N) when M > N ->
    [];
lazy_seq(M, N) ->
    [M|
     % fun-info: {0,0,'-lazy_seq/2-fun-0-'}
     fun() ->
            lazy_seq(M + 1, N)
     end].

lazy_foldr(F, Acc, [Hd|Tl]) ->
    F(Hd, lazy_foldr(F, Acc, apply(Tl, [])));
lazy_foldr(_F, Acc, []) ->
    Acc.

cps_cons(Head, Tail, K) ->
    K([Head|Tail]).

lazy_cp_seq(M, N) ->
    lazy_cp_seq(M,
                N,
                % fun-info: {0,0,'-lazy_cp_seq/2-fun-0-'}
                fun(Pair) ->
                       Pair
                end).

lazy_cp_seq(M, N, K) ->
    if
        N < M ->
            [];
        true ->
            [M|
             % fun-info: {0,0,'-lazy_cp_seq/3-fun-1-'}
             fun() ->
                    lazy_cp_seq(M + 1,
                                N,
                                % fun-info: {0,0,'-lazy_cp_seq/3-fun-0-'}
                                fun(Lst) ->
                                       cps_cons(M, Lst, K)
                                end)
             end]
    end.

stats() ->
    [memory,heap_size,total_heap_size,stack_size,garbage_collection].

test(Name, Fn, Args) when is_list(Args) ->
    Me = self(),
    Pid =
        spawn(% fun-info: {0,0,'-test/3-fun-0-'}
              fun() ->
                     Before = process_info(self(), stats()),
                     Res = apply(Fn, Args),
                     After = process_info(self(), stats()),
                     Me ! {Before,After,Res}
              end),
    Pid ! Me,
    receive
        {B,A,R} ->
            {Name,Pid,{B,A},R}
    end;
test(Name, Fn, Arg) ->
    test(Name, Fn, [Arg]).

run_tests([], Pid, Acc) ->
    timer:sleep(100),
    Pid ! self(),
    receive
        Map ->
            [ 
             {Name,maps:get(Proc, Map, 0),{Before,After},Result} ||
                 {Name,Proc,{Before,After},Result} <- Acc
            ]
    end;
run_tests([{Name,Fn,Args}|Tail], Pid, Acc) ->
    Res = test(Name, Fn, Args),
    run_tests(Tail, Pid, [Res|Acc]).

run_tests(Lst) when is_list(Lst) ->
    Pid =
        spawn(% fun-info: {0,0,'-run_tests/1-fun-0-'}
              fun() ->
                     handle_gc_evts()
              end),
    timer:sleep(100),
    run_tests(Lst, Pid, []).

handle_gc_evts() ->
    erlang:trace(new, true, [garbage_collection]),
    handle_gc_evts(maps:new()).

handle_gc_evts(Map) ->
    receive
        {trace,Pid,Type,Details} ->
            handle_gc_evts(handle_gc(Map, Pid, Type, Details));
        Pid ->
            begin
                erlang:trace(new, false, [garbage_collection]),
                Pid ! Map
            end
    end.

handle_gc(Map, Pid, Type, Details) ->
    HeapSize = proplists:get_value(heap_size, Details),
    case maps:get(Pid, Map, undefined) of
        undefined ->
            maps:put(Pid, HeapSize, Map);
        Frees ->
            case Type of
                gc_start ->
                    maps:put(Pid, Frees + HeapSize, Map);
                gc_end ->
                    maps:put(Pid, Frees - HeapSize, Map)
            end
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
    [{"ss",
      % fun-info: {0,0,'-all_tests/0-fun-0-'}
      fun ss/1,
      [IntArg]},
     {"ses",
      % fun-info: {0,0,'-all_tests/0-fun-1-'}
      fun ses/1,
      [IntArg]},
     {"pen",
      % fun-info: {0,0,'-all_tests/0-fun-2-'}
      fun pen/1,
      [IntArg]},
     {"pen2",
      % fun-info: {0,0,'-all_tests/0-fun-3-'}
      fun pen2/1,
      [IntArg]},
     {"pen2_dumb",
      % fun-info: {0,0,'-all_tests/0-fun-4-'}
      fun pen2_dumb/1,
      [IntArg]},
     {"pen2_dumb_opt",
      % fun-info: {0,0,'-all_tests/0-fun-5-'}
      fun pen2_dumb_opt/1,
      [IntArg]},
     {"pen2_foldr",
      % fun-info: {0,0,'-all_tests/0-fun-6-'}
      fun pen2_foldr/1,
      [IntArg]},
     {"pen2_foldl",
      % fun-info: {0,0,'-all_tests/0-fun-7-'}
      fun pen2_foldl/1,
      [IntArg]},
     {"pen2_listsfuns",
      % fun-info: {0,0,'-all_tests/0-fun-8-'}
      fun pen2_listsfuns/1,
      [IntArg]},
     {"pen2_mr",
      % fun-info: {0,0,'-all_tests/0-fun-9-'}
      fun pen2_mr/1,
      [IntArg]},
     {"pen2_lazy",
      % fun-info: {0,0,'-all_tests/0-fun-10-'}
      fun pen2_lazy/1,
      [IntArg]},
     {"pen2_lazy_cps",
      % fun-info: {0,0,'-all_tests/0-fun-11-'}
      fun pen2_lazy_cps/1,
      [IntArg]},
     {"single_filter",
      % fun-info: {0,0,'-all_tests/0-fun-12-'}
      fun single_filter/1,
      [LstArg]},
     {"record_extract",
      % fun-info: {0,0,'-all_tests/0-fun-13-'}
      fun record_extract/1,
      [RecArg]},
     {"function_param",
      % fun-info: {0,0,'-all_tests/0-fun-14-'}
      fun function_param_lc/2,
      [param,LstArg]},
     {"sorted_gen",
      % fun-info: {0,0,'-all_tests/0-fun-15-'}
      fun sorted_gen/1,
      [RecArg]},
     {"make_n",
      % fun-info: {0,0,'-all_tests/0-fun-16-'}
      fun make_n/2,
      [{param1,param2},IntArg]},
     {"intermediary_list",
      % fun-info: {0,0,'-all_tests/0-fun-17-'}
      fun intermediary_list/1,
      [IntArg]},
     {"intermediary_list2",
      % fun-info: {0,0,'-all_tests/0-fun-18-'}
      fun intermediary_list2/1,
      [IntArg]},
     {"intermediary_list3",
      % fun-info: {0,0,'-all_tests/0-fun-19-'}
      fun intermediary_list3/1,
      [IntArg]},
     {"nested_lc_gen",
      % fun-info: {0,0,'-all_tests/0-fun-20-'}
      fun nested_lc_gen/1,
      [LstArg]},
     {"nested_lc_gen_opt",
      % fun-info: {0,0,'-all_tests/0-fun-21-'}
      fun nested_lc_gen_opt/1,
      [LstArg]},
     {"unused",
      % fun-info: {0,0,'-all_tests/0-fun-22-'}
      fun unused/0,
      []}].

ss(N) when is_integer(N) ->
    lists:sum([ 
               X * X ||
                   X <- lists:seq(1, N)
              ]).

ses(N) when is_integer(N) ->
    lists:sum([ 
               X * X ||
                   X <- lists:seq(1, N),
                   X rem 2 =:= 0
              ]).

pen(N) when is_integer(N) ->
    [ 
     [X,Y] ||
         X <- lists:seq(1, N),
         X rem 2 =:= 0,
         Y <- lists:seq(1, N),
         Y rem 2 =:= 1
    ].

pen2(N) when is_integer(N) ->
    [ 
     [X,Y] ||
         X <- lists:seq(1, N),
         X rem 2 =:= 0,
         Y <- lists:seq(1, N),
         Y rem 2 =:= 1,
         (X + Y) rem 5 /= 0
    ].

pen2_dumb(N) ->
    pen2_dumb_helper(N, 1, 1, []).

pen2_dumb_helper(N, X, _Y, L) when X > N ->
    L;
pen2_dumb_helper(N, X, Y, L) when Y > N ->
    pen2_dumb_helper(N, X + 1, 0, L);
pen2_dumb_helper(N, X, Y, L)
    when
        X rem 2 =:= 0
        andalso
        Y rem 2 =:= 1
        andalso
        (X + Y) rem 5 /= 0 ->
    [[X,Y]|pen2_dumb_helper(N, X, Y + 1, L)];
pen2_dumb_helper(N, X, Y, L) ->
    pen2_dumb_helper(N, X, Y + 1, L).

pen2_dumb_opt(N) ->
    Lst = lists:reverse(lists:seq(1, N)),
    pen2_dumb_opt_helper(Lst, Lst, Lst, Lst, []).

pen2_dumb_opt_helper([], _, _, _, L) ->
    L;
pen2_dumb_opt_helper([_X|Xs], [], XGen, YGen, L) ->
    pen2_dumb_opt_helper(Xs, YGen, XGen, YGen, L);
pen2_dumb_opt_helper([X|Xs], _, XGen, YGen, L) when X rem 2 /= 0 ->
    pen2_dumb_opt_helper(Xs, YGen, XGen, YGen, L);
pen2_dumb_opt_helper(AX = [X|_Xs], [Y|Ys], XGen, YGen, L)
    when
        Y rem 2 =:= 1
        andalso
        (X + Y) rem 5 /= 0 ->
    pen2_dumb_opt_helper(AX, Ys, XGen, YGen, [[X,Y]|L]);
pen2_dumb_opt_helper(AX, [_Y|Ys], XGen, YGen, L) ->
    pen2_dumb_opt_helper(AX, Ys, XGen, YGen, L).

pen2_foldl(N) when is_integer(N) ->
    Gen = lists:seq(1, N),
    lists:reverse(lists:foldl(% fun-info: {0,0,'-pen2_foldl/1-fun-1-'}
                              fun(X, Acc1) ->
                                     if
                                         X rem 2 =:= 0 ->
                                             lists:foldl(% fun-info: {0,0,'-pen2_foldl/1-fun-0-'}
                                                         fun(Y, Acc2) ->
                                                                if
                                                                    Y
                                                                    rem
                                                                    2
                                                                    =:=
                                                                    1
                                                                    andalso
                                                                    (X
                                                                     +
                                                                     Y)
                                                                    rem
                                                                    5
                                                                    /=
                                                                    0 ->
                                                                        [[X,
                                                                          Y]|
                                                                         Acc2];
                                                                    true ->
                                                                        Acc2
                                                                end
                                                         end,
                                                         [],
                                                         Gen)
                                             ++
                                             Acc1;
                                         true ->
                                             Acc1
                                     end
                              end,
                              [],
                              Gen)).

pen2_foldr(N) when is_integer(N) ->
    Gen = lists:seq(1, N),
    lists:foldr(% fun-info: {0,0,'-pen2_foldr/1-fun-1-'}
                fun(X, Acc1) ->
                       if
                           X rem 2 =:= 0 ->
                               lists:foldr(% fun-info: {0,0,'-pen2_foldr/1-fun-0-'}
                                           fun(Y, Acc2) ->
                                                  if
                                                      Y rem 2 =:= 1
                                                      andalso
                                                      (X + Y) rem 5 /= 0 ->
                                                          [[X,Y]|Acc2];
                                                      true ->
                                                          Acc2
                                                  end
                                           end,
                                           [],
                                           Gen)
                               ++
                               Acc1;
                           true ->
                               Acc1
                       end
                end,
                [],
                Gen).

pen2_listsfuns(N) when is_integer(N) ->
    Gen = lists:seq(1, N),
    lists:filter(% fun-info: {0,0,'-pen2_listsfuns/1-fun-0-'}
                 fun([X,Y]) ->
                        (X + Y) rem 5 /= 0
                 end,
                 [ 
                  [X,Y] ||
                      X <-
                          lists:filter(% fun-info: {0,0,'-pen2_listsfuns/1-fun-1-'}
                                       fun(X) ->
                                              X rem 2 =:= 0
                                       end,
                                       Gen),
                      Y <-
                          lists:filter(% fun-info: {0,0,'-pen2_listsfuns/1-fun-2-'}
                                       fun(Y) ->
                                              Y rem 2 =:= 1
                                       end,
                                       Gen)
                 ]).

pen2_mr(N) when is_integer(N) ->
    Gen = lists:seq(1, N),
    RedYs =
        % fun-info: {0,0,'-pen2_mr/1-Red_ys-0-'}
        fun Red_ys(AX = [X|_Xs], [Y|Ys], MR) ->
                if
                    Y rem 2 =:= 1
                    andalso
                    (X + Y) rem 5 /= 0 ->
                        [[X,Y]|Red_ys(AX, Ys, MR)];
                    true ->
                        Red_ys(AX, Ys, MR)
                end;
            Red_ys([_X|Xs], [], MR) ->
                MR(Xs)
        end,
    RedXs =
        % fun-info: {0,0,'-pen2_mr/1-Red_xs-1-'}
        fun Red_xs(AX = [X|Xs]) ->
                if
                    X rem 2 =:= 0 ->
                        RedYs(AX, Gen, Red_xs);
                    true ->
                        Red_xs(Xs)
                end;
            Red_xs([]) ->
                []
        end,
    RedXs(Gen).

pen2_lazy(N) when is_integer(N) ->
    Gen = lazy_seq(1, N),
    RedYs =
        % fun-info: {0,0,'-pen2_lazy/1-Red_ys-0-'}
        fun Red_ys(AX = [X|_Xs], [Y|Ys], MR) ->
                if
                    Y rem 2 =:= 1
                    andalso
                    (X + Y) rem 5 /= 0 ->
                        [[X,Y]|Red_ys(AX, Ys(), MR)];
                    true ->
                        Red_ys(AX, Ys(), MR)
                end;
            Red_ys([_X|Xs], [], MR) ->
                MR(Xs())
        end,
    RedXs =
        % fun-info: {0,0,'-pen2_lazy/1-Red_xs-1-'}
        fun Red_xs(AX = [X|Xs]) ->
                if
                    X rem 2 =:= 0 ->
                        RedYs(AX, Gen, Red_xs);
                    true ->
                        Red_xs(Xs())
                end;
            Red_xs([]) ->
                []
        end,
    RedXs(Gen).

pen2_lazy_cps(N) when is_integer(N) ->
    Gen = lazy_cp_seq(1, N),
    RedYs =
        % fun-info: {0,0,'-pen2_lazy_cps/1-Red_ys-0-'}
        fun Red_ys(AX = [X|_Xs], [Y|Ys], MR) ->
                if
                    Y rem 2 =:= 1
                    andalso
                    (X + Y) rem 5 /= 0 ->
                        [[X,Y]|Red_ys(AX, Ys(), MR)];
                    true ->
                        Red_ys(AX, Ys(), MR)
                end;
            Red_ys([_X|Xs], [], MR) ->
                MR(Xs())
        end,
    RedXs =
        % fun-info: {0,0,'-pen2_lazy_cps/1-Red_xs-1-'}
        fun Red_xs(AX = [X|Xs]) ->
                if
                    X rem 2 =:= 0 ->
                        RedYs(AX, Gen, Red_xs);
                    true ->
                        Red_xs(Xs())
                end;
            Red_xs([]) ->
                []
        end,
    RedXs(Gen).

unused() ->
    _ = [ 
         [X,timer:sleep(1)] ||
             X <- lists:seq(1, 1000)
        ],
    ok.

single_filter(N) when is_list(N) ->
    [ 
     Entry ||
         Entry <- N,
         is_integer(Entry)
    ].

record_extract(N) when is_list(N) ->
    [ 
     case Entry of
         {test_record,rec0,_} ->
             rec0;
         _ ->
             error({badrecord,test_record})
     end ||
         Entry <- N
    ].

function_param_lc(P1, P2) when is_list(P2) ->
    [ 
     [P1,V] ||
         V <- P2
    ].

sorted_gen(L) when is_list(L) ->
    [ 
     case Entry of
         {test_record,rec3,_} ->
             rec3;
         _ ->
             error({badrecord,test_record})
     end ||
         Entry <-
             lists:sort(% fun-info: {0,0,'-sorted_gen/1-fun-0-'}
                        fun(X, Y) ->
                               case X of
                                   {test_record,rec1,_} ->
                                       rec1;
                                   _ ->
                                       error({badrecord,test_record})
                               end
                               <
                               case Y of
                                   {test_record,rec2,_} ->
                                       rec2;
                                   _ ->
                                       error({badrecord,test_record})
                               end
                        end,
                        L)
    ].

make_n(V, N) when is_integer(N) ->
    [ 
     V ||
         _ <- lists:seq(1, N)
    ].

intermediary_list(Len) when is_integer(Len) ->
    Inter = random_test_records(Len, 50),
    [ 
     case Entry of
         {test_record,_,rec4} ->
             rec4;
         _ ->
             error({badrecord,test_record})
     end ||
         Entry <- Inter
    ].

intermediary_list2(Len) when is_integer(Len) ->
    Inter1 = random_test_records(Len, 50),
    Inter2 =
        [ 
         case Entry of
             {test_record,_,rec5} ->
                 rec5;
             _ ->
                 error({badrecord,test_record})
         end ||
             Entry <- Inter1
        ],
    [ 
     X * X ||
         X <- Inter2
    ].

intermediary_list3(Len) when is_integer(Len) ->
    Inter1 = random_test_records(Len, 50),
    Inter2 =
        [ 
         {random:uniform(50),Entry} ||
             Entry <- Inter1
        ],
    Inter3 =
        [ 
         {random:uniform(100),Entry} ||
             Entry <- Inter2
        ],
    [ 
     A * B * C * D ||
         {A,{B,{test_record,C,D}}} <- Inter3
    ].

nested_lc_gen(Lst) when is_list(Lst) ->
    [ 
     {test_record,Entry,Entry * Entry} ||
         Entry <-
             [ 
              Val ||
                  Val <- Lst,
                  Val rem 2 =:= 1
             ]
    ].

nested_lc_gen_opt(Lst) ->
    Entries =
        lists:reverse([ 
                       Val ||
                           Val <- Lst,
                           Val rem 2 =:= 1
                      ]),
    nested_lc_gen_opt_helper(Entries, []).

nested_lc_gen_opt_helper([Entry|Entries], Res) ->
    nested_lc_gen_opt_helper(Entries,
                             [{test_record,Entry,Entry * Entry}|Res]);
nested_lc_gen_opt_helper([], Res) ->
    Res.

module_info() ->
    erlang:get_module_info(lc_tests_opt).

module_info(X) ->
    erlang:get_module_info(lc_tests_opt, X).

