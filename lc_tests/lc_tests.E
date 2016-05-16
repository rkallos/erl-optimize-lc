-file("lc_tests.erl", 1).

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

stats() ->
    [memory,heap_size,total_heap_size,stack_size,garbage_collection].

test(Name, Fn, Args) when is_list(Args) ->
    Me = self(),
    spawn(% fun-info: {0,0,'-test/3-fun-0-'}
          fun() ->
                 Before = process_info(self(), stats()),
                 Res = apply(Fn, Args),
                 After = process_info(self(), stats()),
                 Me ! {Before,After,Res}
          end),
    receive
        {B,A,R} ->
            {Name,{B,A},R}
    end;
test(Name, Fn, Arg) ->
    test(Name, Fn, [Arg]).

run_tests([], Acc) ->
    Acc;
run_tests([{Name,Fn,Args}|Tail], Acc) ->
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
     {"pen2_foldr",
      % fun-info: {0,0,'-all_tests/0-fun-4-'}
      fun pen2_foldr/1,
      [IntArg]},
     {"pen2_foldl",
      % fun-info: {0,0,'-all_tests/0-fun-5-'}
      fun pen2_foldl/1,
      [IntArg]},
     {"pen2_listsfuns",
      % fun-info: {0,0,'-all_tests/0-fun-6-'}
      fun pen2_listsfuns/1,
      [IntArg]},
     {"pen2_mr",
      % fun-info: {0,0,'-all_tests/0-fun-7-'}
      fun pen2_mr/1,
      [IntArg]},
     {"pen2_lazier",
      % fun-info: {0,0,'-all_tests/0-fun-8-'}
      fun pen2_lazier/1,
      [IntArg]},
     {"single_filter",
      % fun-info: {0,0,'-all_tests/0-fun-9-'}
      fun single_filter/1,
      [LstArg]},
     {"record_extract",
      % fun-info: {0,0,'-all_tests/0-fun-10-'}
      fun record_extract/1,
      [RecArg]},
     {"function_param",
      % fun-info: {0,0,'-all_tests/0-fun-11-'}
      fun function_param_lc/2,
      [param,LstArg]},
     {"sorted_gen",
      % fun-info: {0,0,'-all_tests/0-fun-12-'}
      fun sorted_gen/1,
      [RecArg]},
     {"make_n",
      % fun-info: {0,0,'-all_tests/0-fun-13-'}
      fun make_n/2,
      [{param1,param2},IntArg]},
     {"intermediary_list",
      % fun-info: {0,0,'-all_tests/0-fun-14-'}
      fun intermediary_list/1,
      [IntArg]},
     {"intermediary_list2",
      % fun-info: {0,0,'-all_tests/0-fun-15-'}
      fun intermediary_list2/1,
      [IntArg]},
     {"intermediary_list3",
      % fun-info: {0,0,'-all_tests/0-fun-16-'}
      fun intermediary_list3/1,
      [IntArg]},
     {"nested_lc_gen",
      % fun-info: {0,0,'-all_tests/0-fun-17-'}
      fun nested_lc_gen/1,
      [LstArg]},
     {"unused",
      % fun-info: {0,0,'-all_tests/0-fun-18-'}
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

pen2_lazier(N) when is_integer(N) ->
    Gen =
        % fun-info: {0,0,'-pen2_lazier/1-fun-0-'}
        fun() ->
               lazy_seq(1, N)
        end,
    RedYs =
        % fun-info: {0,0,'-pen2_lazier/1-Red_ys-1-'}
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
        % fun-info: {0,0,'-pen2_lazier/1-Red_xs-2-'}
        fun Red_xs(AX = [X|Xs]) ->
                if
                    X rem 2 =:= 0 ->
                        RedYs(AX, Gen(), Red_xs);
                    true ->
                        Red_xs(Xs())
                end;
            Red_xs([]) ->
                []
        end,
    RedXs(Gen()).

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

module_info() ->
    erlang:get_module_info(lc_tests).

module_info(X) ->
    erlang:get_module_info(lc_tests, X).

