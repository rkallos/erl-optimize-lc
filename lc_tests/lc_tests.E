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
     {"single_filter",
      % fun-info: {0,0,'-all_tests/0-fun-4-'}
      fun single_filter/1,
      [LstArg]},
     {"record_extract",
      % fun-info: {0,0,'-all_tests/0-fun-5-'}
      fun record_extract/1,
      [RecArg]},
     {"function_param",
      % fun-info: {0,0,'-all_tests/0-fun-6-'}
      fun function_param_lc/2,
      [param,LstArg]},
     {"sorted_gen",
      % fun-info: {0,0,'-all_tests/0-fun-7-'}
      fun sorted_gen/1,
      [RecArg]},
     {"make_n",
      % fun-info: {0,0,'-all_tests/0-fun-8-'}
      fun make_n/2,
      [{param1,param2},IntArg]},
     {"intermediary_list",
      % fun-info: {0,0,'-all_tests/0-fun-9-'}
      fun intermediary_list/1,
      [IntArg]},
     {"intermediary_list2",
      % fun-info: {0,0,'-all_tests/0-fun-10-'}
      fun intermediary_list2/1,
      [IntArg]},
     {"intermediary_list3",
      % fun-info: {0,0,'-all_tests/0-fun-11-'}
      fun intermediary_list3/1,
      [IntArg]},
     {"nested_lc_gen",
      % fun-info: {0,0,'-all_tests/0-fun-12-'}
      fun nested_lc_gen/1,
      [LstArg]}].

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

