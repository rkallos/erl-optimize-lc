{module, lc_tests_opt}.  %% version = 0

{exports, [{all_tests,0},
           {function_param_lc,2},
           {handle_gc,4},
           {handle_gc_evts,0},
           {handle_gc_evts,1},
           {int_arg,0},
           {intermediary_list,1},
           {intermediary_list2,1},
           {intermediary_list3,1},
           {lazy_foldr,3},
           {lazy_seq,2},
           {list_arg,0},
           {make_n,2},
           {module_info,0},
           {module_info,1},
           {nested_lc_gen,1},
           {pen,1},
           {pen2,1},
           {pen2_foldl,1},
           {pen2_foldr,1},
           {pen2_lazier,1},
           {pen2_listsfuns,1},
           {pen2_mr,1},
           {random,1},
           {random_bin,1},
           {random_test_records,2},
           {record_arg,0},
           {record_extract,1},
           {rng,2},
           {run_tests,1},
           {run_tests,3},
           {ses,1},
           {single_filter,1},
           {sorted_gen,1},
           {ss,1},
           {stats,0},
           {test,3},
           {unused,0}]}.

{attributes, []}.

{labels, 292}.


{function, random, 1, 2}.
  {label,1}.
    {line,[{location,"lc_tests_opt.erl",14}]}.
    {func_info,{atom,lc_tests_opt},{atom,random},1}.
  {label,2}.
    {allocate_zero,1,1}.
    {line,[{location,"lc_tests_opt.erl",15}]}.
    {gc_bif,length,{f,0},1,[{x,0}],{x,1}}.
    {move,{x,0},{y,0}}.
    {move,{x,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",15}]}.
    {call_ext,1,{extfunc,random,uniform,1}}.
    {move,{y,0},{x,1}}.
    {line,[{location,"lc_tests_opt.erl",15}]}.
    {call_ext_last,2,{extfunc,lists,nth,2},1}.


{function, random_bin, 1, 4}.
  {label,3}.
    {line,[{location,"lc_tests_opt.erl",17}]}.
    {func_info,{atom,lc_tests_opt},{atom,random_bin},1}.
  {label,4}.
    {allocate,0,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",18}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {line,[{location,"lc_tests_opt.erl",18}]}.
    {call,1,{f,290}}.
    {line,[{location,"lc_tests_opt.erl",18}]}.
    {call_ext_last,1,{extfunc,erlang,list_to_binary,1},0}.


{function, rng, 2, 6}.
  {label,5}.
    {line,[{location,"lc_tests_opt.erl",20}]}.
    {func_info,{atom,lc_tests_opt},{atom,rng},2}.
  {label,6}.
    {test,is_eq_exact,{f,7},[{x,0},{integer,0}]}.
    {move,nil,{x,0}}.
    return.
  {label,7}.
    {allocate,2,2}.
    {move,{x,0},{y,0}}.
    {move,{x,1},{x,0}}.
    {move,{x,0},{y,1}}.
    {line,[{location,"lc_tests_opt.erl",22}]}.
    {call_ext,1,{extfunc,random,uniform,1}}.
    {line,[{location,"lc_tests_opt.erl",22}]}.
    {gc_bif,'-',{f,0},1,[{y,0},{integer,1}],{x,1}}.
    {move,{x,0},{x,2}}.
    {move,{x,1},{x,0}}.
    {move,{y,1},{x,1}}.
    {move,{x,2},{y,1}}.
    {trim,1,1}.
    {line,[{location,"lc_tests_opt.erl",22}]}.
    {call,2,{f,6}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.


{function, random_test_records, 2, 9}.
  {label,8}.
    {line,[{location,"lc_tests_opt.erl",24}]}.
    {func_info,{atom,lc_tests_opt},{atom,random_test_records},2}.
  {label,9}.
    {test,is_eq_exact,{f,10},[{x,0},{integer,0}]}.
    {move,nil,{x,0}}.
    return.
  {label,10}.
    {allocate,3,2}.
    {init,{y,0}}.
    {move,{x,0},{y,1}}.
    {move,{x,1},{x,0}}.
    {move,{x,0},{y,2}}.
    {line,[{location,"lc_tests_opt.erl",26}]}.
    {call_ext,1,{extfunc,random,uniform,1}}.
    {move,{x,0},{y,0}}.
    {move,{y,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",26}]}.
    {call_ext,1,{extfunc,random,uniform,1}}.
    {line,[{location,"lc_tests_opt.erl",27}]}.
    {gc_bif,'-',{f,0},1,[{y,1},{integer,1}],{x,1}}.
    {move,{x,0},{x,2}}.
    {move,{x,1},{x,0}}.
    {move,{y,2},{x,1}}.
    {move,{x,2},{y,2}}.
    {kill,{y,1}}.
    {line,[{location,"lc_tests_opt.erl",27}]}.
    {call,2,{f,9}}.
    {test_heap,6,1}.
    {put_tuple,3,{x,1}}.
    {put,{atom,test_record}}.
    {put,{y,0}}.
    {put,{y,2}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,3}.
    return.


{function, lazy_seq, 2, 12}.
  {label,11}.
    {line,[{location,"lc_tests_opt.erl",31}]}.
    {func_info,{atom,lc_tests_opt},{atom,lazy_seq},2}.
  {label,12}.
    {test,is_lt,{f,13},[{x,1},{x,0}]}.
    {move,nil,{x,0}}.
    return.
  {label,13}.
    {allocate,1,2}.
    {move,{x,1},{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{x,2},{x,0}}.
    {move,{x,1},{y,0}}.
    {make_fun2,{f,288},0,0,2}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.


{function, lazy_foldr, 3, 15}.
  {label,14}.
    {line,[{location,"lc_tests_opt.erl",34}]}.
    {func_info,{atom,lc_tests_opt},{atom,lazy_foldr},3}.
  {label,15}.
    {test,is_nonempty_list,{f,16},[{x,2}]}.
    {allocate,3,3}.
    {get_list,{x,2},{y,2},{x,4}}.
    {move,{x,0},{y,1}}.
    {move,{x,1},{y,0}}.
    {move,nil,{x,1}}.
    {move,{x,4},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",35}]}.
    {call_ext,2,{extfunc,erlang,apply,2}}.
    {move,{y,0},{x,1}}.
    {move,{x,0},{x,2}}.
    {trim,1,2}.
    {move,{y,0},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",35}]}.
    {call,3,{f,15}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,2}}.
    {move,{y,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",35}]}.
    {call_fun,2}.
    {deallocate,2}.
    return.
  {label,16}.
    {test,is_nil,{f,14},[{x,2}]}.
    {move,{x,1},{x,0}}.
    return.


{function, stats, 0, 18}.
  {label,17}.
    {line,[{location,"lc_tests_opt.erl",42}]}.
    {func_info,{atom,lc_tests_opt},{atom,stats},0}.
  {label,18}.
    {move,{literal,[memory,heap_size,total_heap_size,stack_size,
                    garbage_collection]},
          {x,0}}.
    return.


{function, test, 3, 20}.
  {label,19}.
    {line,[{location,"lc_tests_opt.erl",44}]}.
    {func_info,{atom,lc_tests_opt},{atom,test},3}.
  {label,20}.
    {test,is_list,{f,24},[{x,2}]}.
    {allocate_zero,2,3}.
    {bif,self,{f,0},[],{x,3}}.
    {move,{x,0},{y,1}}.
    {move,{x,1},{x,5}}.
    {move,{x,2},{x,1}}.
    {move,{x,5},{x,2}}.
    {move,{x,3},{x,0}}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,286},0,0,3}.
    {line,[{location,"lc_tests_opt.erl",47}]}.
    {call_ext,1,{extfunc,erlang,spawn,1}}.
    {move,{y,0},{x,1}}.
    {move,{x,0},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",55}]}.
    send.
    {line,[{location,"lc_tests_opt.erl",56}]}.
  {label,21}.
    {loop_rec,{f,23},{x,0}}.
    {test,is_tuple,{f,22},[{x,0}]}.
    {test,test_arity,{f,22},[{x,0},3]}.
    {get_tuple_element,{x,0},0,{x,1}}.
    {get_tuple_element,{x,0},1,{x,2}}.
    {get_tuple_element,{x,0},2,{x,3}}.
    remove_message.
    {test_heap,8,4}.
    {put_tuple,2,{x,4}}.
    {put,{x,1}}.
    {put,{x,2}}.
    {put_tuple,4,{x,0}}.
    {put,{y,1}}.
    {put,{y,0}}.
    {put,{x,4}}.
    {put,{x,3}}.
    {deallocate,2}.
    return.
  {label,22}.
    {loop_rec_end,{f,21}}.
  {label,23}.
    {wait,{f,21}}.
  {label,24}.
    {test_heap,2,3}.
    {put_list,{x,2},nil,{x,2}}.
    {call_only,3,{f,20}}.


{function, run_tests, 3, 26}.
  {label,25}.
    {line,[{location,"lc_tests_opt.erl",62}]}.
    {func_info,{atom,lc_tests_opt},{atom,run_tests},3}.
  {label,26}.
    {test,is_nonempty_list,{f,27},[{x,0}]}.
    {get_list,{x,0},{x,3},{x,4}}.
    {test,is_tuple,{f,25},[{x,3}]}.
    {test,test_arity,{f,25},[{x,3},3]}.
    {allocate,3,5}.
    {get_tuple_element,{x,3},0,{x,0}}.
    {get_tuple_element,{x,3},1,{x,6}}.
    {get_tuple_element,{x,3},2,{x,7}}.
    {move,{x,1},{y,1}}.
    {move,{x,2},{y,0}}.
    {move,{x,6},{x,1}}.
    {move,{x,7},{x,2}}.
    {move,{x,4},{y,2}}.
    {line,[{location,"lc_tests_opt.erl",70}]}.
    {call,3,{f,20}}.
    {test_heap,2,1}.
    {put_list,{x,0},{y,0},{x,2}}.
    {move,{y,1},{x,1}}.
    {move,{y,2},{x,0}}.
    {call_last,3,{f,26},3}.
  {label,27}.
    {test,is_nil,{f,25},[{x,0}]}.
    {allocate,2,3}.
    {move,{integer,100},{x,0}}.
    {move,{x,1},{y,0}}.
    {move,{x,2},{y,1}}.
    {line,[{location,"lc_tests_opt.erl",63}]}.
    {call_ext,1,{extfunc,timer,sleep,1}}.
    {bif,self,{f,0},[],{x,1}}.
    {move,{y,0},{x,0}}.
    {kill,{y,0}}.
    {line,[{location,"lc_tests_opt.erl",64}]}.
    send.
    {line,[{location,"lc_tests_opt.erl",65}]}.
  {label,28}.
    {loop_rec,{f,29},{x,0}}.
    remove_message.
    {move,{x,0},{x,1}}.
    {move,{y,1},{x,0}}.
    {call_last,2,{f,281},2}.
  {label,29}.
    {wait,{f,28}}.


{function, run_tests, 1, 31}.
  {label,30}.
    {line,[{location,"lc_tests_opt.erl",73}]}.
    {func_info,{atom,lc_tests_opt},{atom,run_tests},1}.
  {label,31}.
    {test,is_list,{f,30},[{x,0}]}.
    {allocate_zero,2,1}.
    {move,{x,0},{y,1}}.
    {make_fun2,{f,279},0,0,0}.
    {line,[{location,"lc_tests_opt.erl",74}]}.
    {call_ext,1,{extfunc,erlang,spawn,1}}.
    {move,{x,0},{y,0}}.
    {move,{integer,100},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",75}]}.
    {call_ext,1,{extfunc,timer,sleep,1}}.
    {move,{y,0},{x,1}}.
    {move,nil,{x,2}}.
    {move,{y,1},{x,0}}.
    {call_last,3,{f,26},2}.


{function, handle_gc_evts, 0, 33}.
  {label,32}.
    {line,[{location,"lc_tests_opt.erl",78}]}.
    {func_info,{atom,lc_tests_opt},{atom,handle_gc_evts},0}.
  {label,33}.
    {allocate,0,0}.
    {move,{atom,true},{x,1}}.
    {move,{literal,[garbage_collection]},{x,2}}.
    {move,{atom,new},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",79}]}.
    {call_ext,3,{extfunc,erlang,trace,3}}.
    {line,[{location,"lc_tests_opt.erl",80}]}.
    {call_ext,0,{extfunc,maps,new,0}}.
    {call_last,1,{f,35},0}.


{function, handle_gc_evts, 1, 35}.
  {label,34}.
    {line,[{location,"lc_tests_opt.erl",81}]}.
    {func_info,{atom,lc_tests_opt},{atom,handle_gc_evts},1}.
  {label,35}.
    {allocate_zero,2,1}.
    {move,{x,0},{y,1}}.
    {line,[{location,"lc_tests_opt.erl",82}]}.
  {label,36}.
    {loop_rec,{f,38},{x,0}}.
    {test,is_tuple,{f,37},[{x,0}]}.
    {test,test_arity,{f,37},[{x,0},4]}.
    {get_tuple_element,{x,0},0,{x,1}}.
    {get_tuple_element,{x,0},1,{x,2}}.
    {get_tuple_element,{x,0},2,{x,3}}.
    {get_tuple_element,{x,0},3,{x,4}}.
    {test,is_eq_exact,{f,37},[{x,1},{atom,trace}]}.
    remove_message.
    {move,{x,2},{x,1}}.
    {move,{x,3},{x,2}}.
    {move,{x,4},{x,3}}.
    {move,{y,1},{x,0}}.
    {trim,2,0}.
    {line,[{location,"lc_tests_opt.erl",83}]}.
    {call,4,{f,40}}.
    {call_last,1,{f,35},0}.
  {label,37}.
    remove_message.
    {move,{x,0},{y,0}}.
    {move,{atom,false},{x,1}}.
    {move,{literal,[garbage_collection]},{x,2}}.
    {move,{atom,new},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",85}]}.
    {call_ext,3,{extfunc,erlang,trace,3}}.
    {move,{y,1},{x,1}}.
    {move,{y,0},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",86}]}.
    send.
    {deallocate,2}.
    return.
  {label,38}.
    {wait,{f,36}}.


{function, handle_gc, 4, 40}.
  {label,39}.
    {line,[{location,"lc_tests_opt.erl",90}]}.
    {func_info,{atom,lc_tests_opt},{atom,handle_gc},4}.
  {label,40}.
    {allocate,4,4}.
    {init,{y,0}}.
    {move,{x,0},{y,3}}.
    {move,{x,1},{y,2}}.
    {move,{x,3},{x,1}}.
    {move,{atom,heap_size},{x,0}}.
    {move,{x,2},{y,1}}.
    {line,[{location,"lc_tests_opt.erl",91}]}.
    {call_ext,2,{extfunc,proplists,get_value,2}}.
    {move,{x,0},{y,0}}.
    {move,{y,3},{x,1}}.
    {move,{atom,undefined},{x,2}}.
    {move,{y,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",92}]}.
    {call_ext,3,{extfunc,maps,get,3}}.
    {test,is_eq_exact,{f,41},[{x,0},{atom,undefined}]}.
    {move,{y,0},{x,1}}.
    {move,{y,3},{x,2}}.
    {move,{y,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",93}]}.
    {call_ext_last,3,{extfunc,maps,put,3},4}.
  {label,41}.
    {test,is_atom,{f,44},[{y,1}]}.
    {select_val,{y,1},
                {f,44},
                {list,[{atom,gc_end},{f,42},{atom,gc_start},{f,43}]}}.
  {label,42}.
    {line,[{location,"lc_tests_opt.erl",96}]}.
    {gc_bif,'-',{f,0},1,[{x,0},{y,0}],{x,1}}.
    {move,{y,3},{x,2}}.
    {move,{y,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",96}]}.
    {call_ext_last,3,{extfunc,maps,put,3},4}.
  {label,43}.
    {line,[{location,"lc_tests_opt.erl",95}]}.
    {gc_bif,'+',{f,0},1,[{x,0},{y,0}],{x,1}}.
    {move,{y,3},{x,2}}.
    {move,{y,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",95}]}.
    {call_ext_last,3,{extfunc,maps,put,3},4}.
  {label,44}.
    {line,[{location,"lc_tests_opt.erl",94}]}.
    {case_end,{y,1}}.


{function, int_arg, 0, 46}.
  {label,45}.
    {line,[{location,"lc_tests_opt.erl",103}]}.
    {func_info,{atom,lc_tests_opt},{atom,int_arg},0}.
  {label,46}.
    {move,{integer,50},{x,0}}.
    return.


{function, list_arg, 0, 48}.
  {label,47}.
    {line,[{location,"lc_tests_opt.erl",105}]}.
    {func_info,{atom,lc_tests_opt},{atom,list_arg},0}.
  {label,48}.
    {move,{integer,50},{x,1}}.
    {move,{integer,50},{x,0}}.
    {call_only,2,{f,6}}.


{function, record_arg, 0, 50}.
  {label,49}.
    {line,[{location,"lc_tests_opt.erl",107}]}.
    {func_info,{atom,lc_tests_opt},{atom,record_arg},0}.
  {label,50}.
    {move,{integer,50},{x,1}}.
    {move,{integer,50},{x,0}}.
    {call_only,2,{f,9}}.


{function, all_tests, 0, 52}.
  {label,51}.
    {line,[{location,"lc_tests_opt.erl",110}]}.
    {func_info,{atom,lc_tests_opt},{atom,all_tests},0}.
  {label,52}.
    {allocate_zero,23,0}.
    {move,{integer,50},{x,1}}.
    {move,{integer,500},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",112}]}.
    {call,2,{f,6}}.
    {move,{x,0},{y,22}}.
    {move,{integer,50},{x,1}}.
    {move,{integer,500},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",113}]}.
    {call,2,{f,9}}.
    {move,{x,0},{y,21}}.
    {make_fun2,{f,277},0,0,0}.
    {move,{x,0},{y,20}}.
    {make_fun2,{f,275},0,0,0}.
    {move,{x,0},{y,19}}.
    {make_fun2,{f,273},0,0,0}.
    {move,{x,0},{y,18}}.
    {make_fun2,{f,271},0,0,0}.
    {move,{x,0},{y,17}}.
    {make_fun2,{f,269},0,0,0}.
    {move,{x,0},{y,16}}.
    {make_fun2,{f,267},0,0,0}.
    {move,{x,0},{y,15}}.
    {make_fun2,{f,265},0,0,0}.
    {move,{x,0},{y,14}}.
    {make_fun2,{f,263},0,0,0}.
    {move,{x,0},{y,13}}.
    {make_fun2,{f,261},0,0,0}.
    {move,{x,0},{y,12}}.
    {make_fun2,{f,259},0,0,0}.
    {test_heap,2,1}.
    {put_list,{y,22},nil,{y,10}}.
    {move,{x,0},{y,11}}.
    {make_fun2,{f,257},0,0,0}.
    {test_heap,2,1}.
    {put_list,{y,21},nil,{y,8}}.
    {move,{x,0},{y,9}}.
    {make_fun2,{f,255},0,0,0}.
    {test_heap,4,1}.
    {put_list,{y,22},nil,{x,1}}.
    {put_list,{atom,param},{x,1},{y,6}}.
    {move,{x,0},{y,7}}.
    {make_fun2,{f,253},0,0,0}.
    {test_heap,2,1}.
    {put_list,{y,21},nil,{y,5}}.
    {move,{x,0},{y,21}}.
    {make_fun2,{f,251},0,0,0}.
    {move,{x,0},{y,4}}.
    {make_fun2,{f,249},0,0,0}.
    {move,{x,0},{y,3}}.
    {make_fun2,{f,247},0,0,0}.
    {move,{x,0},{y,2}}.
    {make_fun2,{f,245},0,0,0}.
    {move,{x,0},{y,1}}.
    {make_fun2,{f,243},0,0,0}.
    {test_heap,2,1}.
    {put_list,{y,22},nil,{y,0}}.
    {move,{x,0},{y,22}}.
    {make_fun2,{f,241},0,0,0}.
    {test_heap,114,1}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"unused"}}.
    {put,{x,0}}.
    {put,nil}.
    {put_list,{x,1},nil,{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"nested_lc_gen"}}.
    {put,{y,22}}.
    {put,{y,0}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"intermediary_list3"}}.
    {put,{y,1}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"intermediary_list2"}}.
    {put,{y,2}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"intermediary_list"}}.
    {put,{y,3}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"make_n"}}.
    {put,{y,4}}.
    {put,{literal,[{param1,param2},500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"sorted_gen"}}.
    {put,{y,21}}.
    {put,{y,5}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"function_param"}}.
    {put,{y,7}}.
    {put,{y,6}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"record_extract"}}.
    {put,{y,9}}.
    {put,{y,8}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"single_filter"}}.
    {put,{y,11}}.
    {put,{y,10}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"pen2_lazier"}}.
    {put,{y,12}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"pen2_mr"}}.
    {put,{y,13}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"pen2_listsfuns"}}.
    {put,{y,14}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"pen2_foldl"}}.
    {put,{y,15}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"pen2_foldr"}}.
    {put,{y,16}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"pen2"}}.
    {put,{y,17}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"pen"}}.
    {put,{y,18}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"ses"}}.
    {put,{y,19}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,1}}.
    {put_tuple,3,{x,2}}.
    {put,{literal,"ss"}}.
    {put,{y,20}}.
    {put,{literal,[500]}}.
    {put_list,{x,2},{x,1},{x,0}}.
    {deallocate,23}.
    return.


{function, ss, 1, 54}.
  {label,53}.
    {line,[{location,"lc_tests_opt.erl",138}]}.
    {func_info,{atom,lc_tests_opt},{atom,ss},1}.
  {label,54}.
    {test,is_integer,{f,53},[{x,0}]}.
    {allocate,0,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",139}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {line,[{location,"lc_tests_opt.erl",139}]}.
    {call,1,{f,238}}.
    {line,[{location,"lc_tests_opt.erl",139}]}.
    {call_ext_last,1,{extfunc,lists,sum,1},0}.


{function, ses, 1, 56}.
  {label,55}.
    {line,[{location,"lc_tests_opt.erl",142}]}.
    {func_info,{atom,lc_tests_opt},{atom,ses},1}.
  {label,56}.
    {test,is_integer,{f,55},[{x,0}]}.
    {allocate,0,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",143}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {line,[{location,"lc_tests_opt.erl",143}]}.
    {call,1,{f,234}}.
    {line,[{location,"lc_tests_opt.erl",143}]}.
    {call_ext_last,1,{extfunc,lists,sum,1},0}.


{function, pen, 1, 58}.
  {label,57}.
    {line,[{location,"lc_tests_opt.erl",147}]}.
    {func_info,{atom,lc_tests_opt},{atom,pen},1}.
  {label,58}.
    {test,is_integer,{f,57},[{x,0}]}.
    {allocate,1,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",148}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,0},{x,1}}.
    {call_last,2,{f,224},1}.


{function, pen2, 1, 60}.
  {label,59}.
    {line,[{location,"lc_tests_opt.erl",154}]}.
    {func_info,{atom,lc_tests_opt},{atom,pen2},1}.
  {label,60}.
    {test,is_integer,{f,59},[{x,0}]}.
    {allocate,1,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",155}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,0},{x,1}}.
    {call_last,2,{f,214},1}.


{function, pen2_foldl, 1, 62}.
  {label,61}.
    {line,[{location,"lc_tests_opt.erl",162}]}.
    {func_info,{atom,lc_tests_opt},{atom,pen2_foldl},1}.
  {label,62}.
    {test,is_integer,{f,61},[{x,0}]}.
    {allocate_zero,1,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",163}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,208},0,0,1}.
    {move,{y,0},{x,2}}.
    {move,nil,{x,1}}.
    {trim,1,0}.
    {line,[{location,"lc_tests_opt.erl",164}]}.
    {call_ext,3,{extfunc,lists,foldl,3}}.
    {line,[{location,"lc_tests_opt.erl",164}]}.
    {call_ext_last,1,{extfunc,lists,reverse,1},0}.


{function, pen2_foldr, 1, 64}.
  {label,63}.
    {line,[{location,"lc_tests_opt.erl",178}]}.
    {func_info,{atom,lc_tests_opt},{atom,pen2_foldr},1}.
  {label,64}.
    {test,is_integer,{f,63},[{x,0}]}.
    {allocate_zero,1,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",179}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,202},0,0,1}.
    {move,{y,0},{x,2}}.
    {move,nil,{x,1}}.
    {line,[{location,"lc_tests_opt.erl",180}]}.
    {call_ext_last,3,{extfunc,lists,foldr,3},1}.


{function, pen2_listsfuns, 1, 66}.
  {label,65}.
    {line,[{location,"lc_tests_opt.erl",194}]}.
    {func_info,{atom,lc_tests_opt},{atom,pen2_listsfuns},1}.
  {label,66}.
    {test,is_integer,{f,65},[{x,0}]}.
    {allocate_zero,2,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",195}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{x,0},{y,1}}.
    {make_fun2,{f,200},0,0,0}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,188},0,0,0}.
    {move,{y,1},{x,1}}.
    {line,[{location,"lc_tests_opt.erl",197}]}.
    {call_ext,2,{extfunc,lists,filter,2}}.
    {move,{y,1},{x,1}}.
    {kill,{y,1}}.
    {line,[{location,"lc_tests_opt.erl",197}]}.
    {call,2,{f,190}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",196}]}.
    {call_ext_last,2,{extfunc,lists,filter,2},2}.


{function, pen2_mr, 1, 68}.
  {label,67}.
    {line,[{location,"lc_tests_opt.erl",201}]}.
    {func_info,{atom,lc_tests_opt},{atom,pen2_mr},1}.
  {label,68}.
    {test,is_integer,{f,67},[{x,0}]}.
    {allocate_zero,1,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",202}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,179},0,0,0}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {make_fun2,{f,170},0,0,2}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",215}]}.
    {call_fun,1}.
    {deallocate,1}.
    return.


{function, pen2_lazier, 1, 70}.
  {label,69}.
    {line,[{location,"lc_tests_opt.erl",218}]}.
    {func_info,{atom,lc_tests_opt},{atom,pen2_lazier},1}.
  {label,70}.
    {test,is_integer,{f,69},[{x,0}]}.
    {allocate_zero,1,1}.
    {make_fun2,{f,168},0,0,1}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,159},0,0,0}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {make_fun2,{f,150},0,0,2}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",230}]}.
    {call_fun,0}.
    {move,{y,0},{x,1}}.
    {line,[{location,"lc_tests_opt.erl",230}]}.
    {call_fun,1}.
    {deallocate,1}.
    return.


{function, unused, 0, 72}.
  {label,71}.
    {line,[{location,"lc_tests_opt.erl",234}]}.
    {func_info,{atom,lc_tests_opt},{atom,unused},0}.
  {label,72}.
    {allocate,0,0}.
    {move,{integer,1000},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",235}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {line,[{location,"lc_tests_opt.erl",235}]}.
    {call,1,{f,147}}.
    {move,{atom,ok},{x,0}}.
    {deallocate,0}.
    return.


{function, single_filter, 1, 74}.
  {label,73}.
    {line,[{location,"lc_tests_opt.erl",241}]}.
    {func_info,{atom,lc_tests_opt},{atom,single_filter},1}.
  {label,74}.
    {test,is_list,{f,73},[{x,0}]}.
    {call_only,1,{f,143}}.


{function, record_extract, 1, 76}.
  {label,75}.
    {line,[{location,"lc_tests_opt.erl",247}]}.
    {func_info,{atom,lc_tests_opt},{atom,record_extract},1}.
  {label,76}.
    {test,is_list,{f,75},[{x,0}]}.
    {call_only,1,{f,139}}.


{function, function_param_lc, 2, 78}.
  {label,77}.
    {line,[{location,"lc_tests_opt.erl",251}]}.
    {func_info,{atom,lc_tests_opt},{atom,function_param_lc},2}.
  {label,78}.
    {test,is_list,{f,77},[{x,1}]}.
    {move,{x,1},{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{x,2},{x,0}}.
    {call_only,2,{f,135}}.


{function, sorted_gen, 1, 80}.
  {label,79}.
    {line,[{location,"lc_tests_opt.erl",255}]}.
    {func_info,{atom,lc_tests_opt},{atom,sorted_gen},1}.
  {label,80}.
    {test,is_list,{f,79},[{x,0}]}.
    {allocate,1,1}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,128},0,0,0}.
    {move,{y,0},{x,1}}.
    {trim,1,0}.
    {line,[{location,"lc_tests_opt.erl",257}]}.
    {call_ext,2,{extfunc,lists,sort,2}}.
    {call_last,1,{f,131},0}.


{function, make_n, 2, 82}.
  {label,81}.
    {line,[{location,"lc_tests_opt.erl",265}]}.
    {func_info,{atom,lc_tests_opt},{atom,make_n},2}.
  {label,82}.
    {test,is_integer,{f,81},[{x,1}]}.
    {allocate,1,2}.
    {move,{x,0},{y,0}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",266}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,0},{x,1}}.
    {call_last,2,{f,124},1}.


{function, intermediary_list, 1, 84}.
  {label,83}.
    {line,[{location,"lc_tests_opt.erl",269}]}.
    {func_info,{atom,lc_tests_opt},{atom,intermediary_list},1}.
  {label,84}.
    {test,is_integer,{f,83},[{x,0}]}.
    {allocate,0,1}.
    {move,{integer,50},{x,1}}.
    {line,[{location,"lc_tests_opt.erl",270}]}.
    {call,2,{f,9}}.
    {call_last,1,{f,120},0}.


{function, intermediary_list2, 1, 86}.
  {label,85}.
    {line,[{location,"lc_tests_opt.erl",273}]}.
    {func_info,{atom,lc_tests_opt},{atom,intermediary_list2},1}.
  {label,86}.
    {test,is_integer,{f,85},[{x,0}]}.
    {allocate,0,1}.
    {move,{integer,50},{x,1}}.
    {line,[{location,"lc_tests_opt.erl",274}]}.
    {call,2,{f,9}}.
    {line,[{location,"lc_tests_opt.erl",275}]}.
    {call,1,{f,116}}.
    {call_last,1,{f,113},0}.


{function, intermediary_list3, 1, 88}.
  {label,87}.
    {line,[{location,"lc_tests_opt.erl",278}]}.
    {func_info,{atom,lc_tests_opt},{atom,intermediary_list3},1}.
  {label,88}.
    {test,is_integer,{f,87},[{x,0}]}.
    {allocate,0,1}.
    {move,{integer,50},{x,1}}.
    {line,[{location,"lc_tests_opt.erl",279}]}.
    {call,2,{f,9}}.
    {line,[{location,"lc_tests_opt.erl",280}]}.
    {call,1,{f,110}}.
    {line,[{location,"lc_tests_opt.erl",281}]}.
    {call,1,{f,107}}.
    {call_last,1,{f,103},0}.


{function, nested_lc_gen, 1, 90}.
  {label,89}.
    {line,[{location,"lc_tests_opt.erl",288}]}.
    {func_info,{atom,lc_tests_opt},{atom,nested_lc_gen},1}.
  {label,90}.
    {test,is_list,{f,89},[{x,0}]}.
    {allocate,0,1}.
    {line,[{location,"lc_tests_opt.erl",290}]}.
    {call,1,{f,96}}.
    {call_last,1,{f,100},0}.


{function, module_info, 0, 92}.
  {label,91}.
    {line,[]}.
    {func_info,{atom,lc_tests_opt},{atom,module_info},0}.
  {label,92}.
    {move,{atom,lc_tests_opt},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 94}.
  {label,93}.
    {line,[]}.
    {func_info,{atom,lc_tests_opt},{atom,module_info},1}.
  {label,94}.
    {move,{x,0},{x,1}}.
    {move,{atom,lc_tests_opt},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.


{function, '-nested_lc_gen/1-lc$^0/1-1-', 1, 96}.
  {label,95}.
    {line,[{location,"lc_tests_opt.erl",290}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-nested_lc_gen/1-lc$^0/1-1-'},1}.
  {label,96}.
    {test,is_nonempty_list,{f,98},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {gc_bif,'rem',{f,97},3,[{x,1},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,97},[{x,3},{integer,1}]}.
    {allocate,1,3}.
    {move,{x,2},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",290}]}.
    {call,1,{f,96}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,97}.
    {move,{x,2},{x,0}}.
    {call_only,1,{f,96}}.
  {label,98}.
    {test,is_nil,{f,95},[{x,0}]}.
    return.


{function, '-nested_lc_gen/1-lc$^1/1-0-', 1, 100}.
  {label,99}.
    {line,[{location,"lc_tests_opt.erl",289}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-nested_lc_gen/1-lc$^1/1-0-'},1}.
  {label,100}.
    {test,is_nonempty_list,{f,101},[{x,0}]}.
    {allocate_zero,2,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {line,[{location,"lc_tests_opt.erl",289}]}.
    {gc_bif,'*',{f,0},3,[{x,1},{x,1}],{y,0}}.
    {move,{x,2},{x,0}}.
    {move,{x,1},{y,1}}.
    {line,[{location,"lc_tests_opt.erl",290}]}.
    {call,1,{f,100}}.
    {test_heap,6,1}.
    {put_tuple,3,{x,1}}.
    {put,{atom,test_record}}.
    {put,{y,1}}.
    {put,{y,0}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,101}.
    {test,is_nil,{f,99},[{x,0}]}.
    return.


{function, '-intermediary_list3/1-lc$^2/1-2-', 1, 103}.
  {label,102}.
    {line,[{location,"lc_tests_opt.erl",282}]}.
    {func_info,{atom,lc_tests_opt},
               {atom,'-intermediary_list3/1-lc$^2/1-2-'},
               1}.
  {label,103}.
    {test,is_nonempty_list,{f,105},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_tuple,{f,104},[{x,1}]}.
    {test,test_arity,{f,104},[{x,1},2]}.
    {get_tuple_element,{x,1},0,{x,3}}.
    {get_tuple_element,{x,1},1,{x,4}}.
    {test,is_tuple,{f,104},[{x,4}]}.
    {test,test_arity,{f,104},[{x,4},2]}.
    {get_tuple_element,{x,4},0,{x,5}}.
    {get_tuple_element,{x,4},1,{x,6}}.
    {test,is_tuple,{f,104},[{x,6}]}.
    {test,test_arity,{f,104},[{x,6},3]}.
    {get_tuple_element,{x,6},0,{x,7}}.
    {get_tuple_element,{x,6},1,{x,8}}.
    {get_tuple_element,{x,6},2,{x,9}}.
    {test,is_eq_exact,{f,104},[{x,7},{atom,test_record}]}.
    {allocate_zero,1,10}.
    {line,[{location,"lc_tests_opt.erl",282}]}.
    {gc_bif,'*',{f,0},10,[{x,3},{x,5}],{x,0}}.
    {line,[{location,"lc_tests_opt.erl",282}]}.
    {gc_bif,'*',{f,0},10,[{x,0},{x,8}],{x,0}}.
    {line,[{location,"lc_tests_opt.erl",282}]}.
    {gc_bif,'*',{f,0},10,[{x,0},{x,9}],{y,0}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",282}]}.
    {call,1,{f,103}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,104}.
    {move,{x,2},{x,0}}.
    {call_only,1,{f,103}}.
  {label,105}.
    {test,is_nil,{f,102},[{x,0}]}.
    return.


{function, '-intermediary_list3/1-lc$^1/1-1-', 1, 107}.
  {label,106}.
    {line,[{location,"lc_tests_opt.erl",281}]}.
    {func_info,{atom,lc_tests_opt},
               {atom,'-intermediary_list3/1-lc$^1/1-1-'},
               1}.
  {label,107}.
    {test,is_nonempty_list,{f,108},[{x,0}]}.
    {allocate,2,1}.
    {get_list,{x,0},{y,1},{y,0}}.
    {move,{integer,100},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",281}]}.
    {call_ext,1,{extfunc,random,uniform,1}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",281}]}.
    {call,1,{f,107}}.
    {test_heap,5,1}.
    {put_tuple,2,{x,1}}.
    {put,{y,0}}.
    {put,{y,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,108}.
    {test,is_nil,{f,106},[{x,0}]}.
    return.


{function, '-intermediary_list3/1-lc$^0/1-0-', 1, 110}.
  {label,109}.
    {line,[{location,"lc_tests_opt.erl",280}]}.
    {func_info,{atom,lc_tests_opt},
               {atom,'-intermediary_list3/1-lc$^0/1-0-'},
               1}.
  {label,110}.
    {test,is_nonempty_list,{f,111},[{x,0}]}.
    {allocate,2,1}.
    {get_list,{x,0},{y,1},{y,0}}.
    {move,{integer,50},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",280}]}.
    {call_ext,1,{extfunc,random,uniform,1}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",280}]}.
    {call,1,{f,110}}.
    {test_heap,5,1}.
    {put_tuple,2,{x,1}}.
    {put,{y,0}}.
    {put,{y,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,111}.
    {test,is_nil,{f,109},[{x,0}]}.
    return.


{function, '-intermediary_list2/1-lc$^1/1-1-', 1, 113}.
  {label,112}.
    {line,[{location,"lc_tests_opt.erl",276}]}.
    {func_info,{atom,lc_tests_opt},
               {atom,'-intermediary_list2/1-lc$^1/1-1-'},
               1}.
  {label,113}.
    {test,is_nonempty_list,{f,114},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {line,[{location,"lc_tests_opt.erl",276}]}.
    {gc_bif,'*',{f,0},3,[{x,1},{x,1}],{y,0}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",276}]}.
    {call,1,{f,113}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,114}.
    {test,is_nil,{f,112},[{x,0}]}.
    return.


{function, '-intermediary_list2/1-lc$^0/1-0-', 1, 116}.
  {label,115}.
    {line,[{location,"lc_tests_opt.erl",275}]}.
    {func_info,{atom,lc_tests_opt},
               {atom,'-intermediary_list2/1-lc$^0/1-0-'},
               1}.
  {label,116}.
    {test,is_nonempty_list,{f,117},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_tuple,{f,118},[{x,1}]}.
    {test,test_arity,{f,118},[{x,1},3]}.
    {get_tuple_element,{x,1},0,{x,0}}.
    {get_tuple_element,{x,1},2,{x,3}}.
    {test,is_eq_exact,{f,118},[{x,0},{atom,test_record}]}.
    {move,{x,2},{x,0}}.
    {move,{x,3},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",275}]}.
    {call,1,{f,116}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,117}.
    {test,is_nil,{f,115},[{x,0}]}.
    return.
  {label,118}.
    {move,{literal,{badrecord,test_record}},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",275}]}.
    {call_ext,1,{extfunc,erlang,error,1}}.


{function, '-intermediary_list/1-lc$^0/1-0-', 1, 120}.
  {label,119}.
    {line,[{location,"lc_tests_opt.erl",271}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-intermediary_list/1-lc$^0/1-0-'},1}.
  {label,120}.
    {test,is_nonempty_list,{f,121},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_tuple,{f,122},[{x,1}]}.
    {test,test_arity,{f,122},[{x,1},3]}.
    {get_tuple_element,{x,1},0,{x,0}}.
    {get_tuple_element,{x,1},2,{x,3}}.
    {test,is_eq_exact,{f,122},[{x,0},{atom,test_record}]}.
    {move,{x,2},{x,0}}.
    {move,{x,3},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",271}]}.
    {call,1,{f,120}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,121}.
    {test,is_nil,{f,119},[{x,0}]}.
    return.
  {label,122}.
    {move,{literal,{badrecord,test_record}},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",271}]}.
    {call_ext,1,{extfunc,erlang,error,1}}.


{function, '-make_n/2-lc$^0/1-0-', 2, 124}.
  {label,123}.
    {line,[{location,"lc_tests_opt.erl",266}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-make_n/2-lc$^0/1-0-'},2}.
  {label,124}.
    {test,is_nonempty_list,{f,125},[{x,0}]}.
    {allocate,1,2}.
    {get_list,{x,0},{x,2},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",266}]}.
    {call,2,{f,124}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,125}.
    {test,is_nil,{f,126},[{x,0}]}.
    return.
  {label,126}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",266}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-sorted_gen/1-fun-0-', 2, 128}.
  {label,127}.
    {line,[{location,"lc_tests_opt.erl",257}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-sorted_gen/1-fun-0-'},2}.
  {label,128}.
    {test,is_tuple,{f,129},[{x,0}]}.
    {test,test_arity,{f,129},[{x,0},3]}.
    {get_tuple_element,{x,0},0,{x,2}}.
    {get_tuple_element,{x,0},1,{x,3}}.
    {test,is_eq_exact,{f,129},[{x,2},{atom,test_record}]}.
    {test,is_tuple,{f,129},[{x,1}]}.
    {test,test_arity,{f,129},[{x,1},3]}.
    {get_tuple_element,{x,1},0,{x,0}}.
    {get_tuple_element,{x,1},1,{x,2}}.
    {test,is_eq_exact,{f,129},[{x,0},{atom,test_record}]}.
    {bif,'<',{f,0},[{x,3},{x,2}],{x,0}}.
    return.
  {label,129}.
    {move,{literal,{badrecord,test_record}},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",258}]}.
    {call_ext,1,{extfunc,erlang,error,1}}.


{function, '-sorted_gen/1-lc$^0/1-0-', 1, 131}.
  {label,130}.
    {line,[{location,"lc_tests_opt.erl",256}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-sorted_gen/1-lc$^0/1-0-'},1}.
  {label,131}.
    {test,is_nonempty_list,{f,132},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_tuple,{f,133},[{x,1}]}.
    {test,test_arity,{f,133},[{x,1},3]}.
    {get_tuple_element,{x,1},0,{x,0}}.
    {get_tuple_element,{x,1},1,{x,3}}.
    {test,is_eq_exact,{f,133},[{x,0},{atom,test_record}]}.
    {move,{x,2},{x,0}}.
    {move,{x,3},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",257}]}.
    {call,1,{f,131}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,132}.
    {test,is_nil,{f,130},[{x,0}]}.
    return.
  {label,133}.
    {move,{literal,{badrecord,test_record}},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",256}]}.
    {call_ext,1,{extfunc,erlang,error,1}}.


{function, '-function_param_lc/2-lc$^0/1-0-', 2, 135}.
  {label,134}.
    {line,[{location,"lc_tests_opt.erl",252}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-function_param_lc/2-lc$^0/1-0-'},2}.
  {label,135}.
    {test,is_nonempty_list,{f,136},[{x,0}]}.
    {allocate,2,2}.
    {get_list,{x,0},{y,0},{x,0}}.
    {move,{x,1},{y,1}}.
    {line,[{location,"lc_tests_opt.erl",252}]}.
    {call,2,{f,135}}.
    {test_heap,6,1}.
    {put_list,{y,0},nil,{x,1}}.
    {put_list,{y,1},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,136}.
    {test,is_nil,{f,137},[{x,0}]}.
    return.
  {label,137}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",252}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-record_extract/1-lc$^0/1-0-', 1, 139}.
  {label,138}.
    {line,[{location,"lc_tests_opt.erl",248}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-record_extract/1-lc$^0/1-0-'},1}.
  {label,139}.
    {test,is_nonempty_list,{f,140},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_tuple,{f,141},[{x,1}]}.
    {test,test_arity,{f,141},[{x,1},3]}.
    {get_tuple_element,{x,1},0,{x,0}}.
    {get_tuple_element,{x,1},1,{x,3}}.
    {test,is_eq_exact,{f,141},[{x,0},{atom,test_record}]}.
    {move,{x,2},{x,0}}.
    {move,{x,3},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",248}]}.
    {call,1,{f,139}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,140}.
    {test,is_nil,{f,138},[{x,0}]}.
    return.
  {label,141}.
    {move,{literal,{badrecord,test_record}},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",248}]}.
    {call_ext,1,{extfunc,erlang,error,1}}.


{function, '-single_filter/1-lc$^0/1-0-', 1, 143}.
  {label,142}.
    {line,[{location,"lc_tests_opt.erl",242}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-single_filter/1-lc$^0/1-0-'},1}.
  {label,143}.
    {test,is_nonempty_list,{f,145},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_integer,{f,144},[{x,1}]}.
    {allocate,1,3}.
    {move,{x,2},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",242}]}.
    {call,1,{f,143}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,144}.
    {move,{x,2},{x,0}}.
    {call_only,1,{f,143}}.
  {label,145}.
    {test,is_nil,{f,142},[{x,0}]}.
    return.


{function, '-unused/0-lc$^0/1-0-', 1, 147}.
  {label,146}.
    {line,[{location,"lc_tests_opt.erl",235}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-unused/0-lc$^0/1-0-'},1}.
  {label,147}.
    {test,is_nonempty_list,{f,148},[{x,0}]}.
    {allocate,1,1}.
    {get_list,{x,0},{x,1},{y,0}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",235}]}.
    {call_ext,1,{extfunc,timer,sleep,1}}.
    {move,{y,0},{x,0}}.
    {call_last,1,{f,147},1}.
  {label,148}.
    {test,is_nil,{f,146},[{x,0}]}.
    return.


{function, '-pen2_lazier/1-anonymous-5-', 3, 150}.
  {label,149}.
    {line,[]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_lazier/1-anonymous-5-'},3}.
  {label,150}.
    {call_only,3,{f,152}}.


{function, '-pen2_lazier/1-Red_xs/1-1-', 3, 152}.
  {label,151}.
    {line,[{location,"lc_tests_opt.erl",225}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_lazier/1-Red_xs/1-1-'},3}.
  {label,152}.
    {allocate,3,3}.
    {move,{x,0},{y,2}}.
    {move,{x,1},{x,0}}.
    {move,{x,2},{x,1}}.
    {move,{x,0},{y,0}}.
    {move,{x,1},{y,1}}.
    {make_fun2,{f,157},0,0,2}.
    {test,is_nonempty_list,{f,154},[{y,2}]}.
    {get_list,{y,2},{x,1},{x,2}}.
    {gc_bif,'rem',{f,153},3,[{x,1},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,153},[{x,3},{integer,0}]}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",226}]}.
    {call_fun,0}.
    {move,{y,0},{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{y,1},{x,3}}.
    {move,{y,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",226}]}.
    {call_fun,3}.
    {deallocate,3}.
    return.
  {label,153}.
    {move,{x,0},{y,2}}.
    {trim,2,1}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",227}]}.
    {call_fun,0}.
    {move,{y,0},{x,1}}.
    {line,[{location,"lc_tests_opt.erl",227}]}.
    {call_fun,1}.
    {deallocate,1}.
    return.
  {label,154}.
    {test,is_nil,{f,155},[{y,2}]}.
    {move,nil,{x,0}}.
    {deallocate,3}.
    return.
  {label,155}.
    {test_heap,2,0}.
    {put_tuple,1,{x,1}}.
    {put,{y,2}}.
    {line,[{location,"lc_tests_opt.erl",225}]}.
    {case_end,{x,1}}.


{function, '-pen2_lazier/1-anonymous-4-', 3, 157}.
  {label,156}.
    {line,[]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_lazier/1-anonymous-4-'},3}.
  {label,157}.
    {call_only,3,{f,152}}.


{function, '-pen2_lazier/1-anonymous-3-', 3, 159}.
  {label,158}.
    {line,[]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_lazier/1-anonymous-3-'},3}.
  {label,159}.
    {call_only,3,{f,161}}.


{function, '-pen2_lazier/1-Red_ys/3-0-', 3, 161}.
  {label,160}.
    {line,[{location,"lc_tests_opt.erl",220}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_lazier/1-Red_ys/3-0-'},3}.
  {label,161}.
    {allocate,5,3}.
    {init,{y,0}}.
    {init,{y,1}}.
    {move,{x,2},{y,2}}.
    {move,{x,1},{y,3}}.
    {move,{x,0},{y,4}}.
    {make_fun2,{f,166},0,0,0}.
    {test,is_nonempty_list,{f,164},[{y,4}]}.
    {get_list,{y,4},{x,1},{x,2}}.
    {test,is_nonempty_list,{f,163},[{y,3}]}.
    {get_list,{y,3},{x,3},{x,4}}.
    {gc_bif,'rem',{f,162},5,[{x,3},{integer,2}],{x,2}}.
    {test,is_eq_exact,{f,162},[{x,2},{integer,1}]}.
    {gc_bif,'+',{f,162},5,[{x,1},{x,3}],{x,2}}.
    {gc_bif,'rem',{f,162},5,[{x,2},{integer,5}],{x,2}}.
    {bif,'=/=',{f,162},[{x,2},{integer,0}],{x,2}}.
    {move,{x,2},{x,5}}.
    {test,is_eq_exact,{f,162},[{x,5},{atom,true}]}.
    {move,{x,0},{y,0}}.
    {move,{x,4},{x,0}}.
    {move,{x,3},{y,1}}.
    {move,{x,1},{y,3}}.
    {line,[{location,"lc_tests_opt.erl",222}]}.
    {call_fun,0}.
    {move,{y,2},{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,3}}.
    {move,{y,4},{x,0}}.
    {move,{y,1},{y,4}}.
    {trim,3,2}.
    {line,[{location,"lc_tests_opt.erl",222}]}.
    {call_fun,3}.
    {test_heap,6,1}.
    {put_list,{y,1},nil,{x,1}}.
    {put_list,{y,0},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,162}.
    {move,{x,0},{y,3}}.
    {move,{x,4},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",223}]}.
    {call_fun,0}.
    {move,{y,2},{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{y,3},{x,3}}.
    {move,{y,4},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",223}]}.
    {call_fun,3}.
    {deallocate,5}.
    return.
  {label,163}.
    {test,is_nil,{f,164},[{y,3}]}.
    {kill,{y,3}}.
    {kill,{y,4}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",224}]}.
    {call_fun,0}.
    {move,{y,2},{x,1}}.
    {line,[{location,"lc_tests_opt.erl",224}]}.
    {call_fun,1}.
    {deallocate,5}.
    return.
  {label,164}.
    {test_heap,4,0}.
    {put_tuple,3,{x,1}}.
    {put,{y,4}}.
    {put,{y,3}}.
    {put,{y,2}}.
    {line,[{location,"lc_tests_opt.erl",220}]}.
    {case_end,{x,1}}.


{function, '-pen2_lazier/1-anonymous-2-', 3, 166}.
  {label,165}.
    {line,[]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_lazier/1-anonymous-2-'},3}.
  {label,166}.
    {call_only,3,{f,161}}.


{function, '-pen2_lazier/1-fun-0-', 1, 168}.
  {label,167}.
    {line,[{location,"lc_tests_opt.erl",219}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_lazier/1-fun-0-'},1}.
  {label,168}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {call_only,2,{f,12}}.


{function, '-pen2_mr/1-anonymous-5-', 3, 170}.
  {label,169}.
    {line,[]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_mr/1-anonymous-5-'},3}.
  {label,170}.
    {call_only,3,{f,172}}.


{function, '-pen2_mr/1-Red_xs/1-1-', 3, 172}.
  {label,171}.
    {line,[{location,"lc_tests_opt.erl",209}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_mr/1-Red_xs/1-1-'},3}.
  {label,172}.
    {allocate,3,3}.
    {move,{x,0},{y,2}}.
    {move,{x,1},{x,0}}.
    {move,{x,2},{x,1}}.
    {move,{x,0},{y,0}}.
    {move,{x,1},{y,1}}.
    {make_fun2,{f,177},0,0,2}.
    {test,is_nonempty_list,{f,174},[{y,2}]}.
    {get_list,{y,2},{x,1},{x,2}}.
    {gc_bif,'rem',{f,173},3,[{x,1},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,173},[{x,3},{integer,0}]}.
    {move,{x,0},{x,2}}.
    {move,{y,0},{x,1}}.
    {move,{y,1},{x,3}}.
    {move,{y,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",210}]}.
    {call_fun,3}.
    {deallocate,3}.
    return.
  {label,173}.
    {move,{x,0},{x,1}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",211}]}.
    {call_fun,1}.
    {deallocate,3}.
    return.
  {label,174}.
    {test,is_nil,{f,175},[{y,2}]}.
    {move,nil,{x,0}}.
    {deallocate,3}.
    return.
  {label,175}.
    {test_heap,2,0}.
    {put_tuple,1,{x,1}}.
    {put,{y,2}}.
    {line,[{location,"lc_tests_opt.erl",209}]}.
    {case_end,{x,1}}.


{function, '-pen2_mr/1-anonymous-4-', 3, 177}.
  {label,176}.
    {line,[]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_mr/1-anonymous-4-'},3}.
  {label,177}.
    {call_only,3,{f,172}}.


{function, '-pen2_mr/1-anonymous-3-', 3, 179}.
  {label,178}.
    {line,[]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_mr/1-anonymous-3-'},3}.
  {label,179}.
    {call_only,3,{f,181}}.


{function, '-pen2_mr/1-Red_ys/3-0-', 3, 181}.
  {label,180}.
    {line,[{location,"lc_tests_opt.erl",203}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_mr/1-Red_ys/3-0-'},3}.
  {label,181}.
    {allocate,3,3}.
    {move,{x,1},{y,0}}.
    {move,{x,0},{y,1}}.
    {move,{x,2},{y,2}}.
    {make_fun2,{f,186},0,0,0}.
    {test,is_nonempty_list,{f,184},[{y,1}]}.
    {get_list,{y,1},{x,1},{x,2}}.
    {test,is_nonempty_list,{f,183},[{y,0}]}.
    {get_list,{y,0},{x,3},{x,4}}.
    {gc_bif,'rem',{f,182},5,[{x,3},{integer,2}],{x,2}}.
    {test,is_eq_exact,{f,182},[{x,2},{integer,1}]}.
    {gc_bif,'+',{f,182},5,[{x,1},{x,3}],{x,2}}.
    {gc_bif,'rem',{f,182},5,[{x,2},{integer,5}],{x,2}}.
    {bif,'=/=',{f,182},[{x,2},{integer,0}],{x,2}}.
    {move,{x,2},{x,5}}.
    {test,is_eq_exact,{f,182},[{x,5},{atom,true}]}.
    {move,{x,3},{x,6}}.
    {move,{x,1},{x,5}}.
    {move,{y,2},{x,2}}.
    {move,{x,4},{x,1}}.
    {move,{x,0},{x,3}}.
    {move,{y,1},{x,0}}.
    {move,{x,6},{y,1}}.
    {move,{x,5},{y,2}}.
    {trim,1,2}.
    {line,[{location,"lc_tests_opt.erl",205}]}.
    {call_fun,3}.
    {test_heap,6,1}.
    {put_list,{y,0},nil,{x,1}}.
    {put_list,{y,1},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,182}.
    {move,{y,2},{x,2}}.
    {move,{x,4},{x,1}}.
    {move,{x,0},{x,3}}.
    {move,{y,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",206}]}.
    {call_fun,3}.
    {deallocate,3}.
    return.
  {label,183}.
    {test,is_nil,{f,184},[{y,0}]}.
    {move,{y,2},{x,1}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",208}]}.
    {call_fun,1}.
    {deallocate,3}.
    return.
  {label,184}.
    {test_heap,4,0}.
    {put_tuple,3,{x,1}}.
    {put,{y,1}}.
    {put,{y,0}}.
    {put,{y,2}}.
    {line,[{location,"lc_tests_opt.erl",203}]}.
    {case_end,{x,1}}.


{function, '-pen2_mr/1-anonymous-2-', 3, 186}.
  {label,185}.
    {line,[]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_mr/1-anonymous-2-'},3}.
  {label,186}.
    {call_only,3,{f,181}}.


{function, '-pen2_listsfuns/1-fun-1-', 1, 188}.
  {label,187}.
    {line,[{location,"lc_tests_opt.erl",197}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_listsfuns/1-fun-1-'},1}.
  {label,188}.
    {line,[{location,"lc_tests_opt.erl",197}]}.
    {gc_bif,'rem',{f,0},1,[{x,0},{integer,2}],{x,0}}.
    {bif,'=:=',{f,0},[{x,0},{integer,0}],{x,0}}.
    return.


{function, '-pen2_listsfuns/1-lc$^0/1-0-', 2, 190}.
  {label,189}.
    {line,[{location,"lc_tests_opt.erl",197}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_listsfuns/1-lc$^0/1-0-'},2}.
  {label,190}.
    {test,is_nonempty_list,{f,191},[{x,0}]}.
    {allocate,3,2}.
    {get_list,{x,0},{y,1},{y,2}}.
    {move,{x,1},{y,0}}.
    {make_fun2,{f,194},0,0,0}.
    {move,{y,0},{x,1}}.
    {line,[{location,"lc_tests_opt.erl",198}]}.
    {call_ext,2,{extfunc,lists,filter,2}}.
    {move,{y,1},{x,2}}.
    {move,{y,2},{x,3}}.
    {move,{y,0},{x,1}}.
    {call_last,4,{f,196},3}.
  {label,191}.
    {test,is_nil,{f,192},[{x,0}]}.
    return.
  {label,192}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",197}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pen2_listsfuns/1-fun-2-', 1, 194}.
  {label,193}.
    {line,[{location,"lc_tests_opt.erl",198}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_listsfuns/1-fun-2-'},1}.
  {label,194}.
    {line,[{location,"lc_tests_opt.erl",198}]}.
    {gc_bif,'rem',{f,0},1,[{x,0},{integer,2}],{x,0}}.
    {bif,'=:=',{f,0},[{x,0},{integer,1}],{x,0}}.
    return.


{function, '-pen2_listsfuns/1-lc$^1/1-1-', 4, 196}.
  {label,195}.
    {line,[{location,"lc_tests_opt.erl",197}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_listsfuns/1-lc$^1/1-1-'},4}.
  {label,196}.
    {test,is_nonempty_list,{f,197},[{x,0}]}.
    {allocate,2,4}.
    {get_list,{x,0},{y,0},{x,0}}.
    {move,{x,2},{y,1}}.
    {line,[{location,"lc_tests_opt.erl",198}]}.
    {call,4,{f,196}}.
    {test_heap,6,1}.
    {put_list,{y,0},nil,{x,1}}.
    {put_list,{y,1},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,197}.
    {test,is_nil,{f,198},[{x,0}]}.
    {move,{x,3},{x,0}}.
    {call_only,2,{f,190}}.
  {label,198}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",197}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pen2_listsfuns/1-fun-0-', 1, 200}.
  {label,199}.
    {line,[{location,"lc_tests_opt.erl",196}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_listsfuns/1-fun-0-'},1}.
  {label,200}.
    {test,is_nonempty_list,{f,199},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_nonempty_list,{f,199},[{x,2}]}.
    {get_list,{x,2},{x,3},{x,4}}.
    {test,is_nil,{f,199},[{x,4}]}.
    {line,[{location,"lc_tests_opt.erl",196}]}.
    {gc_bif,'+',{f,0},4,[{x,1},{x,3}],{x,0}}.
    {line,[{location,"lc_tests_opt.erl",196}]}.
    {gc_bif,'rem',{f,0},1,[{x,0},{integer,5}],{x,0}}.
    {bif,'=/=',{f,0},[{x,0},{integer,0}],{x,0}}.
    return.


{function, '-pen2_foldr/1-fun-1-', 3, 202}.
  {label,201}.
    {line,[{location,"lc_tests_opt.erl",181}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_foldr/1-fun-1-'},3}.
  {label,202}.
    {gc_bif,'rem',{f,203},3,[{x,0},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,203},[{x,3},{integer,0}]}.
    {allocate,2,3}.
    {move,{x,2},{y,0}}.
    {move,{x,1},{y,1}}.
    {make_fun2,{f,205},0,0,1}.
    {move,{y,0},{x,2}}.
    {move,nil,{x,1}}.
    {trim,1,1}.
    {line,[{location,"lc_tests_opt.erl",183}]}.
    {call_ext,3,{extfunc,lists,foldr,3}}.
    {move,{y,0},{x,1}}.
    {line,[{location,"lc_tests_opt.erl",189}]}.
    {call_ext_last,2,{extfunc,erlang,'++',2},1}.
  {label,203}.
    {move,{x,1},{x,0}}.
    return.


{function, '-pen2_foldr/1-fun-0-', 3, 205}.
  {label,204}.
    {line,[{location,"lc_tests_opt.erl",184}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_foldr/1-fun-0-'},3}.
  {label,205}.
    {gc_bif,'rem',{f,206},3,[{x,0},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,206},[{x,3},{integer,1}]}.
    {gc_bif,'+',{f,206},3,[{x,2},{x,0}],{x,3}}.
    {gc_bif,'rem',{f,206},4,[{x,3},{integer,5}],{x,3}}.
    {test,is_ne_exact,{f,206},[{x,3},{integer,0}]}.
    {test_heap,6,3}.
    {put_list,{x,0},nil,{x,0}}.
    {put_list,{x,2},{x,0},{x,2}}.
    {put_list,{x,2},{x,1},{x,0}}.
    return.
  {label,206}.
    {move,{x,1},{x,0}}.
    return.


{function, '-pen2_foldl/1-fun-1-', 3, 208}.
  {label,207}.
    {line,[{location,"lc_tests_opt.erl",165}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_foldl/1-fun-1-'},3}.
  {label,208}.
    {gc_bif,'rem',{f,209},3,[{x,0},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,209},[{x,3},{integer,0}]}.
    {allocate,2,3}.
    {move,{x,2},{y,0}}.
    {move,{x,1},{y,1}}.
    {make_fun2,{f,211},0,0,1}.
    {move,{y,0},{x,2}}.
    {move,nil,{x,1}}.
    {trim,1,1}.
    {line,[{location,"lc_tests_opt.erl",167}]}.
    {call_ext,3,{extfunc,lists,foldl,3}}.
    {move,{y,0},{x,1}}.
    {line,[{location,"lc_tests_opt.erl",173}]}.
    {call_ext_last,2,{extfunc,erlang,'++',2},1}.
  {label,209}.
    {move,{x,1},{x,0}}.
    return.


{function, '-pen2_foldl/1-fun-0-', 3, 211}.
  {label,210}.
    {line,[{location,"lc_tests_opt.erl",168}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_foldl/1-fun-0-'},3}.
  {label,211}.
    {gc_bif,'rem',{f,212},3,[{x,0},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,212},[{x,3},{integer,1}]}.
    {gc_bif,'+',{f,212},3,[{x,2},{x,0}],{x,3}}.
    {gc_bif,'rem',{f,212},4,[{x,3},{integer,5}],{x,3}}.
    {test,is_ne_exact,{f,212},[{x,3},{integer,0}]}.
    {test_heap,6,3}.
    {put_list,{x,0},nil,{x,0}}.
    {put_list,{x,2},{x,0},{x,2}}.
    {put_list,{x,2},{x,1},{x,0}}.
    return.
  {label,212}.
    {move,{x,1},{x,0}}.
    return.


{function, '-pen2/1-lc$^0/1-0-', 2, 214}.
  {label,213}.
    {line,[{location,"lc_tests_opt.erl",155}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2/1-lc$^0/1-0-'},2}.
  {label,214}.
    {test,is_nonempty_list,{f,216},[{x,0}]}.
    {get_list,{x,0},{x,2},{x,3}}.
    {gc_bif,'rem',{f,215},4,[{x,2},{integer,2}],{x,4}}.
    {test,is_eq_exact,{f,215},[{x,4},{integer,0}]}.
    {allocate,3,4}.
    {move,{integer,1},{x,0}}.
    {move,{x,1},{y,0}}.
    {move,{x,3},{y,1}}.
    {move,{x,2},{y,2}}.
    {line,[{location,"lc_tests_opt.erl",157}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,1},{x,2}}.
    {move,{y,2},{x,3}}.
    {move,{y,0},{x,1}}.
    {call_last,4,{f,219},3}.
  {label,215}.
    {move,{x,3},{x,0}}.
    {call_only,2,{f,214}}.
  {label,216}.
    {test,is_nil,{f,217},[{x,0}]}.
    return.
  {label,217}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",155}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pen2/1-lc$^1/1-1-', 4, 219}.
  {label,218}.
    {line,[{location,"lc_tests_opt.erl",155}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2/1-lc$^1/1-1-'},4}.
  {label,219}.
    {test,is_nonempty_list,{f,221},[{x,0}]}.
    {get_list,{x,0},{x,4},{x,5}}.
    {gc_bif,'rem',{f,220},6,[{x,4},{integer,2}],{x,6}}.
    {gc_bif,'+',{f,220},7,[{x,3},{x,4}],{x,7}}.
    {gc_bif,'rem',{f,220},8,[{x,7},{integer,5}],{x,8}}.
    {test,is_eq_exact,{f,220},[{x,6},{integer,1}]}.
    {test,is_ne_exact,{f,220},[{x,8},{integer,0}]}.
    {allocate,2,6}.
    {move,{x,5},{x,0}}.
    {move,{x,4},{y,0}}.
    {move,{x,3},{y,1}}.
    {line,[{location,"lc_tests_opt.erl",157}]}.
    {call,4,{f,219}}.
    {test_heap,6,1}.
    {put_list,{y,0},nil,{x,1}}.
    {put_list,{y,1},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,220}.
    {move,{x,5},{x,0}}.
    {call_only,4,{f,219}}.
  {label,221}.
    {test,is_nil,{f,222},[{x,0}]}.
    {move,{x,2},{x,0}}.
    {call_only,2,{f,214}}.
  {label,222}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",155}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pen/1-lc$^0/1-0-', 2, 224}.
  {label,223}.
    {line,[{location,"lc_tests_opt.erl",148}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen/1-lc$^0/1-0-'},2}.
  {label,224}.
    {test,is_nonempty_list,{f,226},[{x,0}]}.
    {get_list,{x,0},{x,2},{x,3}}.
    {gc_bif,'rem',{f,225},4,[{x,2},{integer,2}],{x,4}}.
    {test,is_eq_exact,{f,225},[{x,4},{integer,0}]}.
    {allocate,3,4}.
    {move,{integer,1},{x,0}}.
    {move,{x,1},{y,0}}.
    {move,{x,3},{y,1}}.
    {move,{x,2},{y,2}}.
    {line,[{location,"lc_tests_opt.erl",150}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,1},{x,2}}.
    {move,{y,2},{x,3}}.
    {move,{y,0},{x,1}}.
    {call_last,4,{f,229},3}.
  {label,225}.
    {move,{x,3},{x,0}}.
    {call_only,2,{f,224}}.
  {label,226}.
    {test,is_nil,{f,227},[{x,0}]}.
    return.
  {label,227}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",148}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pen/1-lc$^1/1-1-', 4, 229}.
  {label,228}.
    {line,[{location,"lc_tests_opt.erl",148}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen/1-lc$^1/1-1-'},4}.
  {label,229}.
    {test,is_nonempty_list,{f,231},[{x,0}]}.
    {get_list,{x,0},{x,4},{x,5}}.
    {gc_bif,'rem',{f,230},6,[{x,4},{integer,2}],{x,6}}.
    {test,is_eq_exact,{f,230},[{x,6},{integer,1}]}.
    {allocate,2,6}.
    {move,{x,5},{x,0}}.
    {move,{x,4},{y,0}}.
    {move,{x,3},{y,1}}.
    {line,[{location,"lc_tests_opt.erl",150}]}.
    {call,4,{f,229}}.
    {test_heap,6,1}.
    {put_list,{y,0},nil,{x,1}}.
    {put_list,{y,1},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,230}.
    {move,{x,5},{x,0}}.
    {call_only,4,{f,229}}.
  {label,231}.
    {test,is_nil,{f,232},[{x,0}]}.
    {move,{x,2},{x,0}}.
    {call_only,2,{f,224}}.
  {label,232}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",148}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-ses/1-lc$^0/1-0-', 1, 234}.
  {label,233}.
    {line,[{location,"lc_tests_opt.erl",143}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-ses/1-lc$^0/1-0-'},1}.
  {label,234}.
    {test,is_nonempty_list,{f,236},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {gc_bif,'rem',{f,235},3,[{x,1},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,235},[{x,3},{integer,0}]}.
    {allocate_zero,1,3}.
    {line,[{location,"lc_tests_opt.erl",143}]}.
    {gc_bif,'*',{f,0},3,[{x,1},{x,1}],{y,0}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",143}]}.
    {call,1,{f,234}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,235}.
    {move,{x,2},{x,0}}.
    {call_only,1,{f,234}}.
  {label,236}.
    {test,is_nil,{f,233},[{x,0}]}.
    return.


{function, '-ss/1-lc$^0/1-0-', 1, 238}.
  {label,237}.
    {line,[{location,"lc_tests_opt.erl",139}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-ss/1-lc$^0/1-0-'},1}.
  {label,238}.
    {test,is_nonempty_list,{f,239},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {line,[{location,"lc_tests_opt.erl",139}]}.
    {gc_bif,'*',{f,0},3,[{x,1},{x,1}],{y,0}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",139}]}.
    {call,1,{f,238}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,239}.
    {test,is_nil,{f,237},[{x,0}]}.
    return.


{function, '-all_tests/0-fun-18-', 0, 241}.
  {label,240}.
    {line,[{location,"lc_tests_opt.erl",132}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-18-'},0}.
  {label,241}.
    {call_only,0,{f,72}}.


{function, '-all_tests/0-fun-17-', 1, 243}.
  {label,242}.
    {line,[{location,"lc_tests_opt.erl",131}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-17-'},1}.
  {label,243}.
    {call_only,1,{f,90}}.


{function, '-all_tests/0-fun-16-', 1, 245}.
  {label,244}.
    {line,[{location,"lc_tests_opt.erl",130}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-16-'},1}.
  {label,245}.
    {call_only,1,{f,88}}.


{function, '-all_tests/0-fun-15-', 1, 247}.
  {label,246}.
    {line,[{location,"lc_tests_opt.erl",129}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-15-'},1}.
  {label,247}.
    {call_only,1,{f,86}}.


{function, '-all_tests/0-fun-14-', 1, 249}.
  {label,248}.
    {line,[{location,"lc_tests_opt.erl",128}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-14-'},1}.
  {label,249}.
    {call_only,1,{f,84}}.


{function, '-all_tests/0-fun-13-', 2, 251}.
  {label,250}.
    {line,[{location,"lc_tests_opt.erl",127}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-13-'},2}.
  {label,251}.
    {call_only,2,{f,82}}.


{function, '-all_tests/0-fun-12-', 1, 253}.
  {label,252}.
    {line,[{location,"lc_tests_opt.erl",126}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-12-'},1}.
  {label,253}.
    {call_only,1,{f,80}}.


{function, '-all_tests/0-fun-11-', 2, 255}.
  {label,254}.
    {line,[{location,"lc_tests_opt.erl",125}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-11-'},2}.
  {label,255}.
    {call_only,2,{f,78}}.


{function, '-all_tests/0-fun-10-', 1, 257}.
  {label,256}.
    {line,[{location,"lc_tests_opt.erl",124}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-10-'},1}.
  {label,257}.
    {call_only,1,{f,76}}.


{function, '-all_tests/0-fun-9-', 1, 259}.
  {label,258}.
    {line,[{location,"lc_tests_opt.erl",123}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-9-'},1}.
  {label,259}.
    {call_only,1,{f,74}}.


{function, '-all_tests/0-fun-8-', 1, 261}.
  {label,260}.
    {line,[{location,"lc_tests_opt.erl",122}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-8-'},1}.
  {label,261}.
    {call_only,1,{f,70}}.


{function, '-all_tests/0-fun-7-', 1, 263}.
  {label,262}.
    {line,[{location,"lc_tests_opt.erl",121}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-7-'},1}.
  {label,263}.
    {call_only,1,{f,68}}.


{function, '-all_tests/0-fun-6-', 1, 265}.
  {label,264}.
    {line,[{location,"lc_tests_opt.erl",120}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-6-'},1}.
  {label,265}.
    {call_only,1,{f,66}}.


{function, '-all_tests/0-fun-5-', 1, 267}.
  {label,266}.
    {line,[{location,"lc_tests_opt.erl",119}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-5-'},1}.
  {label,267}.
    {call_only,1,{f,62}}.


{function, '-all_tests/0-fun-4-', 1, 269}.
  {label,268}.
    {line,[{location,"lc_tests_opt.erl",118}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-4-'},1}.
  {label,269}.
    {call_only,1,{f,64}}.


{function, '-all_tests/0-fun-3-', 1, 271}.
  {label,270}.
    {line,[{location,"lc_tests_opt.erl",117}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-3-'},1}.
  {label,271}.
    {call_only,1,{f,60}}.


{function, '-all_tests/0-fun-2-', 1, 273}.
  {label,272}.
    {line,[{location,"lc_tests_opt.erl",116}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-2-'},1}.
  {label,273}.
    {call_only,1,{f,58}}.


{function, '-all_tests/0-fun-1-', 1, 275}.
  {label,274}.
    {line,[{location,"lc_tests_opt.erl",115}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-1-'},1}.
  {label,275}.
    {call_only,1,{f,56}}.


{function, '-all_tests/0-fun-0-', 1, 277}.
  {label,276}.
    {line,[{location,"lc_tests_opt.erl",114}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-0-'},1}.
  {label,277}.
    {call_only,1,{f,54}}.


{function, '-run_tests/1-fun-0-', 0, 279}.
  {label,278}.
    {line,[{location,"lc_tests_opt.erl",74}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-run_tests/1-fun-0-'},0}.
  {label,279}.
    {call_only,0,{f,33}}.


{function, '-run_tests/3-lc$^0/1-0-', 2, 281}.
  {label,280}.
    {line,[{location,"lc_tests_opt.erl",66}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-run_tests/3-lc$^0/1-0-'},2}.
  {label,281}.
    {test,is_nonempty_list,{f,283},[{x,0}]}.
    {get_list,{x,0},{x,2},{x,3}}.
    {test,is_tuple,{f,282},[{x,2}]}.
    {test,test_arity,{f,282},[{x,2},4]}.
    {get_tuple_element,{x,2},0,{x,4}}.
    {get_tuple_element,{x,2},1,{x,5}}.
    {get_tuple_element,{x,2},2,{x,6}}.
    {get_tuple_element,{x,2},3,{x,7}}.
    {test,is_tuple,{f,282},[{x,6}]}.
    {test,test_arity,{f,282},[{x,6},2]}.
    {allocate,6,8}.
    {get_tuple_element,{x,6},0,{y,3}}.
    {get_tuple_element,{x,6},1,{y,2}}.
    {move,{integer,0},{x,2}}.
    {move,{x,5},{x,0}}.
    {move,{x,1},{y,0}}.
    {move,{x,3},{y,1}}.
    {move,{x,7},{y,4}}.
    {move,{x,4},{y,5}}.
    {line,[{location,"lc_tests_opt.erl",66}]}.
    {call_ext,3,{extfunc,maps,get,3}}.
    {move,{x,0},{x,2}}.
    {move,{y,0},{x,1}}.
    {move,{y,1},{x,0}}.
    {move,{x,2},{y,1}}.
    {trim,1,5}.
    {line,[{location,"lc_tests_opt.erl",67}]}.
    {call,2,{f,281}}.
    {test_heap,10,1}.
    {put_tuple,2,{x,1}}.
    {put,{y,2}}.
    {put,{y,1}}.
    {put_tuple,4,{x,2}}.
    {put,{y,4}}.
    {put,{y,0}}.
    {put,{x,1}}.
    {put,{y,3}}.
    {put_list,{x,2},{x,0},{x,0}}.
    {deallocate,5}.
    return.
  {label,282}.
    {move,{x,3},{x,0}}.
    {call_only,2,{f,281}}.
  {label,283}.
    {test,is_nil,{f,284},[{x,0}]}.
    return.
  {label,284}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",66}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-test/3-fun-0-', 3, 286}.
  {label,285}.
    {line,[{location,"lc_tests_opt.erl",48}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-test/3-fun-0-'},3}.
  {label,286}.
    {allocate_zero,4,3}.
    {bif,self,{f,0},[],{y,0}}.
    {move,{x,1},{y,1}}.
    {move,{x,2},{y,2}}.
    {move,{x,0},{y,3}}.
    {line,[{location,"lc_tests_opt.erl",49}]}.
    {call,0,{f,18}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {kill,{y,0}}.
    {line,[{location,"lc_tests_opt.erl",49}]}.
    {call_ext,2,{extfunc,erlang,process_info,2}}.
    {move,{x,0},{x,2}}.
    {move,{y,1},{x,1}}.
    {move,{y,2},{x,0}}.
    {move,{x,2},{y,2}}.
    {kill,{y,1}}.
    {line,[{location,"lc_tests_opt.erl",50}]}.
    {call_ext,2,{extfunc,erlang,apply,2}}.
    {bif,self,{f,0},[],{y,0}}.
    {move,{x,0},{y,1}}.
    {line,[{location,"lc_tests_opt.erl",52}]}.
    {call,0,{f,18}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {trim,1,3}.
    {line,[{location,"lc_tests_opt.erl",52}]}.
    {call_ext,2,{extfunc,erlang,process_info,2}}.
    {test_heap,4,1}.
    {put_tuple,3,{x,1}}.
    {put,{y,1}}.
    {put,{x,0}}.
    {put,{y,0}}.
    {move,{y,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",53}]}.
    send.
    {deallocate,3}.
    return.


{function, '-lazy_seq/2-fun-0-', 2, 288}.
  {label,287}.
    {line,[{location,"lc_tests_opt.erl",32}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-lazy_seq/2-fun-0-'},2}.
  {label,288}.
    {line,[{location,"lc_tests_opt.erl",32}]}.
    {gc_bif,'+',{f,0},2,[{x,1},{integer,1}],{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{x,2},{x,0}}.
    {call_only,2,{f,12}}.


{function, '-random_bin/1-lc$^0/1-0-', 1, 290}.
  {label,289}.
    {line,[{location,"lc_tests_opt.erl",18}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-random_bin/1-lc$^0/1-0-'},1}.
  {label,290}.
    {test,is_nonempty_list,{f,291},[{x,0}]}.
    {allocate,1,1}.
    {get_list,{x,0},{x,1},{y,0}}.
    {move,{integer,255},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",18}]}.
    {call_ext,1,{extfunc,random,uniform,1}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",18}]}.
    {call,1,{f,290}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,291}.
    {test,is_nil,{f,289},[{x,0}]}.
    return.
