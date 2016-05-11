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

stats() ->
    [memory,heap_size,total_heap_size,stack_size,garbage_collection].

test(Fn, Args) when is_list(Args) ->
    Pid =
        spawn(% fun-info: {0,0,'-test/2-fun-0-'}
              fun() ->
                     receive
                         {Parent,Args} ->
                             begin
                                 Before = process_info(self(), stats()),
                                 apply(Fn, Args),
                                 After = process_info(self(), stats()),
                                 Parent ! {self(),Before,After}
                             end
                     end
              end),
    Pid ! {self(),Args},
    receive
        {Cid,B,A} ->
            io:format("~p~n", [{Cid,B,A}])
    end;
test(Fn, Arg) ->
    test(Fn, [Arg]).

ss(N) ->
    lists:sum([ 
               X * X ||
                   X <- lists:seq(1, N)
              ]).

ses(N) ->
    lists:sum([ 
               X * X ||
                   X <- lists:seq(1, N),
                   X rem 2 =:= 0
              ]).

pen(N) ->
    [ 
     [X,Y] ||
         X <- lists:seq(1, N),
         X rem 2 =:= 0,
         Y <- lists:seq(1, N),
         Y rem 2 =:= 1
    ].

pen2(N) ->
    [ 
     [X,Y] ||
         X <- lists:seq(1, N),
         X rem 2 =:= 0,
         Y <- lists:seq(1, N),
         Y rem 2 =:= 1,
         (X + Y) rem 5 /= 0
    ].

single_filter(N) ->
    [ 
     Entry ||
         Entry <- N,
         is_integer(Entry)
    ].

record_extract(N) ->
    [ 
     case Entry of
         {test_record,rec0,_} ->
             rec0;
         _ ->
             error({badrecord,test_record})
     end ||
         Entry <- N
    ].

function_param_lc(P1, P2) ->
    [ 
     [P1,V] ||
         V <- P2
    ].

sorted_gen(L) ->
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

make_n(V, N) ->
    [ 
     V ||
         _ <- lists:seq(1, N)
    ].

intermediary_list(L) ->
    Inter = random_test_records(L, 50),
    [ 
     case Entry of
         {test_record,_,rec4} ->
             rec4;
         _ ->
             error({badrecord,test_record})
     end ||
         Entry <- Inter
    ].

intermediary_list2(L) ->
    Inter1 = random_test_records(L, 50),
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

intermediary_list3(L) ->
    Inter1 = random_test_records(L, 50),
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

nested_lc_gen(L) ->
    [ 
     {test_record,Entry,Entry * Entry} ||
         Entry <-
             [ 
              Val ||
                  Val <- L,
                  Val rem 2 =:= 1
             ]
    ].

module_info() ->
    erlang:get_module_info(lc_tests_opt).

module_info(X) ->
    erlang:get_module_info(lc_tests_opt, X).

