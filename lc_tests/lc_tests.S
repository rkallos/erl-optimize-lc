{module, lc_tests}.  %% version = 0

{exports, [{all_tests,0},
           {cps_cons,3},
           {function_param_lc,2},
           {handle_gc,4},
           {handle_gc_evts,0},
           {handle_gc_evts,1},
           {int_arg,0},
           {intermediary_list,1},
           {intermediary_list2,1},
           {intermediary_list3,1},
           {lazy_cp_seq,2},
           {lazy_cp_seq,3},
           {lazy_foldr,3},
           {lazy_seq,2},
           {list_arg,0},
           {make_n,2},
           {module_info,0},
           {module_info,1},
           {nested_lc_gen,1},
           {nested_lc_gen_opt,1},
           {nested_lc_gen_opt_helper,2},
           {pen,1},
           {pen2,1},
           {pen2_dumb,1},
           {pen2_dumb_helper,4},
           {pen2_dumb_opt,1},
           {pen2_dumb_opt_helper,5},
           {pen2_foldl,1},
           {pen2_foldr,1},
           {pen2_lazy,1},
           {pen2_lazy_cps,1},
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

{labels, 355}.


{function, random, 1, 2}.
  {label,1}.
    {line,[{location,"lc_tests.erl",14}]}.
    {func_info,{atom,lc_tests},{atom,random},1}.
  {label,2}.
    {allocate_zero,1,1}.
    {line,[{location,"lc_tests.erl",15}]}.
    {gc_bif,length,{f,0},1,[{x,0}],{x,1}}.
    {move,{x,0},{y,0}}.
    {move,{x,1},{x,0}}.
    {line,[{location,"lc_tests.erl",15}]}.
    {call_ext,1,{extfunc,random,uniform,1}}.
    {move,{y,0},{x,1}}.
    {line,[{location,"lc_tests.erl",15}]}.
    {call_ext_last,2,{extfunc,lists,nth,2},1}.


{function, random_bin, 1, 4}.
  {label,3}.
    {line,[{location,"lc_tests.erl",17}]}.
    {func_info,{atom,lc_tests},{atom,random_bin},1}.
  {label,4}.
    {allocate,0,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests.erl",18}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {line,[{location,"lc_tests.erl",18}]}.
    {call,1,{f,353}}.
    {line,[{location,"lc_tests.erl",18}]}.
    {call_ext_last,1,{extfunc,erlang,list_to_binary,1},0}.


{function, rng, 2, 6}.
  {label,5}.
    {line,[{location,"lc_tests.erl",20}]}.
    {func_info,{atom,lc_tests},{atom,rng},2}.
  {label,6}.
    {test,is_eq_exact,{f,7},[{x,0},{integer,0}]}.
    {move,nil,{x,0}}.
    return.
  {label,7}.
    {allocate,2,2}.
    {move,{x,0},{y,0}}.
    {move,{x,1},{x,0}}.
    {move,{x,0},{y,1}}.
    {line,[{location,"lc_tests.erl",22}]}.
    {call_ext,1,{extfunc,random,uniform,1}}.
    {line,[{location,"lc_tests.erl",22}]}.
    {gc_bif,'-',{f,0},1,[{y,0},{integer,1}],{x,1}}.
    {move,{x,0},{x,2}}.
    {move,{x,1},{x,0}}.
    {move,{y,1},{x,1}}.
    {move,{x,2},{y,1}}.
    {trim,1,1}.
    {line,[{location,"lc_tests.erl",22}]}.
    {call,2,{f,6}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.


{function, random_test_records, 2, 9}.
  {label,8}.
    {line,[{location,"lc_tests.erl",24}]}.
    {func_info,{atom,lc_tests},{atom,random_test_records},2}.
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
    {line,[{location,"lc_tests.erl",26}]}.
    {call_ext,1,{extfunc,random,uniform,1}}.
    {move,{x,0},{y,0}}.
    {move,{y,2},{x,0}}.
    {line,[{location,"lc_tests.erl",26}]}.
    {call_ext,1,{extfunc,random,uniform,1}}.
    {line,[{location,"lc_tests.erl",27}]}.
    {gc_bif,'-',{f,0},1,[{y,1},{integer,1}],{x,1}}.
    {move,{x,0},{x,2}}.
    {move,{x,1},{x,0}}.
    {move,{y,2},{x,1}}.
    {move,{x,2},{y,2}}.
    {kill,{y,1}}.
    {line,[{location,"lc_tests.erl",27}]}.
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
    {line,[{location,"lc_tests.erl",31}]}.
    {func_info,{atom,lc_tests},{atom,lazy_seq},2}.
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
    {make_fun2,{f,351},0,0,2}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.


{function, lazy_foldr, 3, 15}.
  {label,14}.
    {line,[{location,"lc_tests.erl",34}]}.
    {func_info,{atom,lc_tests},{atom,lazy_foldr},3}.
  {label,15}.
    {test,is_nonempty_list,{f,16},[{x,2}]}.
    {allocate,3,3}.
    {get_list,{x,2},{y,2},{x,4}}.
    {move,{x,0},{y,1}}.
    {move,{x,1},{y,0}}.
    {move,nil,{x,1}}.
    {move,{x,4},{x,0}}.
    {line,[{location,"lc_tests.erl",35}]}.
    {call_ext,2,{extfunc,erlang,apply,2}}.
    {move,{y,0},{x,1}}.
    {move,{x,0},{x,2}}.
    {trim,1,2}.
    {move,{y,0},{x,0}}.
    {line,[{location,"lc_tests.erl",35}]}.
    {call,3,{f,15}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,2}}.
    {move,{y,1},{x,0}}.
    {line,[{location,"lc_tests.erl",35}]}.
    {call_fun,2}.
    {deallocate,2}.
    return.
  {label,16}.
    {test,is_nil,{f,14},[{x,2}]}.
    {move,{x,1},{x,0}}.
    return.


{function, cps_cons, 3, 18}.
  {label,17}.
    {line,[{location,"lc_tests.erl",39}]}.
    {func_info,{atom,lc_tests},{atom,cps_cons},3}.
  {label,18}.
    {allocate_heap,0,2,3}.
    {put_list,{x,0},{x,1},{x,0}}.
    {move,{x,2},{x,1}}.
    {line,[{location,"lc_tests.erl",40}]}.
    {call_fun,1}.
    {deallocate,0}.
    return.


{function, lazy_cp_seq, 2, 20}.
  {label,19}.
    {line,[{location,"lc_tests.erl",42}]}.
    {func_info,{atom,lc_tests},{atom,lazy_cp_seq},2}.
  {label,20}.
    {allocate,2,2}.
    {move,{x,1},{y,0}}.
    {move,{x,0},{y,1}}.
    {make_fun2,{f,349},0,0,0}.
    {move,{y,0},{x,1}}.
    {move,{x,0},{x,2}}.
    {move,{y,1},{x,0}}.
    {call_last,3,{f,22},2}.


{function, lazy_cp_seq, 3, 22}.
  {label,21}.
    {line,[{location,"lc_tests.erl",44}]}.
    {func_info,{atom,lc_tests},{atom,lazy_cp_seq},3}.
  {label,22}.
    {test,is_lt,{f,23},[{x,1},{x,0}]}.
    {move,nil,{x,0}}.
    return.
  {label,23}.
    {allocate,1,3}.
    {move,{x,2},{x,3}}.
    {move,{x,0},{x,2}}.
    {move,{x,3},{x,0}}.
    {move,{x,2},{y,0}}.
    {make_fun2,{f,345},0,0,3}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.


{function, stats, 0, 25}.
  {label,24}.
    {line,[{location,"lc_tests.erl",52}]}.
    {func_info,{atom,lc_tests},{atom,stats},0}.
  {label,25}.
    {move,{literal,[memory,heap_size,total_heap_size,stack_size,
                    garbage_collection]},
          {x,0}}.
    return.


{function, test, 3, 27}.
  {label,26}.
    {line,[{location,"lc_tests.erl",54}]}.
    {func_info,{atom,lc_tests},{atom,test},3}.
  {label,27}.
    {test,is_list,{f,31},[{x,2}]}.
    {allocate_zero,2,3}.
    {bif,self,{f,0},[],{x,3}}.
    {move,{x,0},{y,1}}.
    {move,{x,1},{x,5}}.
    {move,{x,2},{x,1}}.
    {move,{x,5},{x,2}}.
    {move,{x,3},{x,0}}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,343},0,0,3}.
    {line,[{location,"lc_tests.erl",57}]}.
    {call_ext,1,{extfunc,erlang,spawn,1}}.
    {move,{y,0},{x,1}}.
    {move,{x,0},{y,0}}.
    {line,[{location,"lc_tests.erl",65}]}.
    send.
    {line,[{location,"lc_tests.erl",66}]}.
  {label,28}.
    {loop_rec,{f,30},{x,0}}.
    {test,is_tuple,{f,29},[{x,0}]}.
    {test,test_arity,{f,29},[{x,0},3]}.
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
  {label,29}.
    {loop_rec_end,{f,28}}.
  {label,30}.
    {wait,{f,28}}.
  {label,31}.
    {test_heap,2,3}.
    {put_list,{x,2},nil,{x,2}}.
    {call_only,3,{f,27}}.


{function, run_tests, 3, 33}.
  {label,32}.
    {line,[{location,"lc_tests.erl",72}]}.
    {func_info,{atom,lc_tests},{atom,run_tests},3}.
  {label,33}.
    {test,is_nonempty_list,{f,34},[{x,0}]}.
    {get_list,{x,0},{x,3},{x,4}}.
    {test,is_tuple,{f,32},[{x,3}]}.
    {test,test_arity,{f,32},[{x,3},3]}.
    {allocate,3,5}.
    {get_tuple_element,{x,3},0,{x,0}}.
    {get_tuple_element,{x,3},1,{x,6}}.
    {get_tuple_element,{x,3},2,{x,7}}.
    {move,{x,1},{y,1}}.
    {move,{x,2},{y,0}}.
    {move,{x,6},{x,1}}.
    {move,{x,7},{x,2}}.
    {move,{x,4},{y,2}}.
    {line,[{location,"lc_tests.erl",80}]}.
    {call,3,{f,27}}.
    {test_heap,2,1}.
    {put_list,{x,0},{y,0},{x,2}}.
    {move,{y,1},{x,1}}.
    {move,{y,2},{x,0}}.
    {call_last,3,{f,33},3}.
  {label,34}.
    {test,is_nil,{f,32},[{x,0}]}.
    {allocate,2,3}.
    {move,{integer,100},{x,0}}.
    {move,{x,1},{y,0}}.
    {move,{x,2},{y,1}}.
    {line,[{location,"lc_tests.erl",73}]}.
    {call_ext,1,{extfunc,timer,sleep,1}}.
    {bif,self,{f,0},[],{x,1}}.
    {move,{y,0},{x,0}}.
    {kill,{y,0}}.
    {line,[{location,"lc_tests.erl",74}]}.
    send.
    {line,[{location,"lc_tests.erl",75}]}.
  {label,35}.
    {loop_rec,{f,36},{x,0}}.
    remove_message.
    {move,{x,0},{x,1}}.
    {move,{y,1},{x,0}}.
    {call_last,2,{f,338},2}.
  {label,36}.
    {wait,{f,35}}.


{function, run_tests, 1, 38}.
  {label,37}.
    {line,[{location,"lc_tests.erl",83}]}.
    {func_info,{atom,lc_tests},{atom,run_tests},1}.
  {label,38}.
    {test,is_list,{f,37},[{x,0}]}.
    {allocate_zero,2,1}.
    {move,{x,0},{y,1}}.
    {make_fun2,{f,336},0,0,0}.
    {line,[{location,"lc_tests.erl",84}]}.
    {call_ext,1,{extfunc,erlang,spawn,1}}.
    {move,{x,0},{y,0}}.
    {move,{integer,100},{x,0}}.
    {line,[{location,"lc_tests.erl",85}]}.
    {call_ext,1,{extfunc,timer,sleep,1}}.
    {move,{y,0},{x,1}}.
    {move,nil,{x,2}}.
    {move,{y,1},{x,0}}.
    {call_last,3,{f,33},2}.


{function, handle_gc_evts, 0, 40}.
  {label,39}.
    {line,[{location,"lc_tests.erl",88}]}.
    {func_info,{atom,lc_tests},{atom,handle_gc_evts},0}.
  {label,40}.
    {allocate,0,0}.
    {move,{atom,true},{x,1}}.
    {move,{literal,[garbage_collection]},{x,2}}.
    {move,{atom,new},{x,0}}.
    {line,[{location,"lc_tests.erl",89}]}.
    {call_ext,3,{extfunc,erlang,trace,3}}.
    {line,[{location,"lc_tests.erl",90}]}.
    {call_ext,0,{extfunc,maps,new,0}}.
    {call_last,1,{f,42},0}.


{function, handle_gc_evts, 1, 42}.
  {label,41}.
    {line,[{location,"lc_tests.erl",91}]}.
    {func_info,{atom,lc_tests},{atom,handle_gc_evts},1}.
  {label,42}.
    {allocate_zero,2,1}.
    {move,{x,0},{y,1}}.
    {line,[{location,"lc_tests.erl",92}]}.
  {label,43}.
    {loop_rec,{f,45},{x,0}}.
    {test,is_tuple,{f,44},[{x,0}]}.
    {test,test_arity,{f,44},[{x,0},4]}.
    {get_tuple_element,{x,0},0,{x,1}}.
    {get_tuple_element,{x,0},1,{x,2}}.
    {get_tuple_element,{x,0},2,{x,3}}.
    {get_tuple_element,{x,0},3,{x,4}}.
    {test,is_eq_exact,{f,44},[{x,1},{atom,trace}]}.
    remove_message.
    {move,{x,2},{x,1}}.
    {move,{x,3},{x,2}}.
    {move,{x,4},{x,3}}.
    {move,{y,1},{x,0}}.
    {trim,2,0}.
    {line,[{location,"lc_tests.erl",93}]}.
    {call,4,{f,47}}.
    {call_last,1,{f,42},0}.
  {label,44}.
    remove_message.
    {move,{x,0},{y,0}}.
    {move,{atom,false},{x,1}}.
    {move,{literal,[garbage_collection]},{x,2}}.
    {move,{atom,new},{x,0}}.
    {line,[{location,"lc_tests.erl",95}]}.
    {call_ext,3,{extfunc,erlang,trace,3}}.
    {move,{y,1},{x,1}}.
    {move,{y,0},{x,0}}.
    {line,[{location,"lc_tests.erl",96}]}.
    send.
    {deallocate,2}.
    return.
  {label,45}.
    {wait,{f,43}}.


{function, handle_gc, 4, 47}.
  {label,46}.
    {line,[{location,"lc_tests.erl",100}]}.
    {func_info,{atom,lc_tests},{atom,handle_gc},4}.
  {label,47}.
    {allocate,4,4}.
    {init,{y,0}}.
    {move,{x,0},{y,3}}.
    {move,{x,1},{y,2}}.
    {move,{x,3},{x,1}}.
    {move,{atom,heap_size},{x,0}}.
    {move,{x,2},{y,1}}.
    {line,[{location,"lc_tests.erl",101}]}.
    {call_ext,2,{extfunc,proplists,get_value,2}}.
    {move,{x,0},{y,0}}.
    {move,{y,3},{x,1}}.
    {move,{atom,undefined},{x,2}}.
    {move,{y,2},{x,0}}.
    {line,[{location,"lc_tests.erl",102}]}.
    {call_ext,3,{extfunc,maps,get,3}}.
    {test,is_eq_exact,{f,48},[{x,0},{atom,undefined}]}.
    {move,{y,0},{x,1}}.
    {move,{y,3},{x,2}}.
    {move,{y,2},{x,0}}.
    {line,[{location,"lc_tests.erl",103}]}.
    {call_ext_last,3,{extfunc,maps,put,3},4}.
  {label,48}.
    {test,is_atom,{f,51},[{y,1}]}.
    {select_val,{y,1},
                {f,51},
                {list,[{atom,gc_end},{f,49},{atom,gc_start},{f,50}]}}.
  {label,49}.
    {line,[{location,"lc_tests.erl",106}]}.
    {gc_bif,'-',{f,0},1,[{x,0},{y,0}],{x,1}}.
    {move,{y,3},{x,2}}.
    {move,{y,2},{x,0}}.
    {line,[{location,"lc_tests.erl",106}]}.
    {call_ext_last,3,{extfunc,maps,put,3},4}.
  {label,50}.
    {line,[{location,"lc_tests.erl",105}]}.
    {gc_bif,'+',{f,0},1,[{x,0},{y,0}],{x,1}}.
    {move,{y,3},{x,2}}.
    {move,{y,2},{x,0}}.
    {line,[{location,"lc_tests.erl",105}]}.
    {call_ext_last,3,{extfunc,maps,put,3},4}.
  {label,51}.
    {line,[{location,"lc_tests.erl",104}]}.
    {case_end,{y,1}}.


{function, int_arg, 0, 53}.
  {label,52}.
    {line,[{location,"lc_tests.erl",113}]}.
    {func_info,{atom,lc_tests},{atom,int_arg},0}.
  {label,53}.
    {move,{integer,50},{x,0}}.
    return.


{function, list_arg, 0, 55}.
  {label,54}.
    {line,[{location,"lc_tests.erl",115}]}.
    {func_info,{atom,lc_tests},{atom,list_arg},0}.
  {label,55}.
    {move,{integer,50},{x,1}}.
    {move,{integer,50},{x,0}}.
    {call_only,2,{f,6}}.


{function, record_arg, 0, 57}.
  {label,56}.
    {line,[{location,"lc_tests.erl",117}]}.
    {func_info,{atom,lc_tests},{atom,record_arg},0}.
  {label,57}.
    {move,{integer,50},{x,1}}.
    {move,{integer,50},{x,0}}.
    {call_only,2,{f,9}}.


{function, all_tests, 0, 59}.
  {label,58}.
    {line,[{location,"lc_tests.erl",120}]}.
    {func_info,{atom,lc_tests},{atom,all_tests},0}.
  {label,59}.
    {allocate_zero,28,0}.
    {move,{integer,50},{x,1}}.
    {move,{integer,500},{x,0}}.
    {line,[{location,"lc_tests.erl",122}]}.
    {call,2,{f,6}}.
    {move,{x,0},{y,27}}.
    {move,{integer,50},{x,1}}.
    {move,{integer,500},{x,0}}.
    {line,[{location,"lc_tests.erl",123}]}.
    {call,2,{f,9}}.
    {move,{x,0},{y,26}}.
    {make_fun2,{f,334},0,0,0}.
    {move,{x,0},{y,25}}.
    {make_fun2,{f,332},0,0,0}.
    {move,{x,0},{y,24}}.
    {make_fun2,{f,330},0,0,0}.
    {move,{x,0},{y,23}}.
    {make_fun2,{f,328},0,0,0}.
    {move,{x,0},{y,22}}.
    {make_fun2,{f,326},0,0,0}.
    {move,{x,0},{y,21}}.
    {make_fun2,{f,324},0,0,0}.
    {move,{x,0},{y,20}}.
    {make_fun2,{f,322},0,0,0}.
    {move,{x,0},{y,19}}.
    {make_fun2,{f,320},0,0,0}.
    {move,{x,0},{y,18}}.
    {make_fun2,{f,318},0,0,0}.
    {move,{x,0},{y,17}}.
    {make_fun2,{f,316},0,0,0}.
    {move,{x,0},{y,16}}.
    {make_fun2,{f,314},0,0,0}.
    {move,{x,0},{y,15}}.
    {make_fun2,{f,312},0,0,0}.
    {move,{x,0},{y,14}}.
    {make_fun2,{f,310},0,0,0}.
    {test_heap,2,1}.
    {put_list,{y,27},nil,{y,12}}.
    {move,{x,0},{y,13}}.
    {make_fun2,{f,308},0,0,0}.
    {test_heap,2,1}.
    {put_list,{y,26},nil,{y,10}}.
    {move,{x,0},{y,11}}.
    {make_fun2,{f,306},0,0,0}.
    {test_heap,4,1}.
    {put_list,{y,27},nil,{x,1}}.
    {put_list,{atom,param},{x,1},{y,8}}.
    {move,{x,0},{y,9}}.
    {make_fun2,{f,304},0,0,0}.
    {test_heap,2,1}.
    {put_list,{y,26},nil,{y,7}}.
    {move,{x,0},{y,26}}.
    {make_fun2,{f,302},0,0,0}.
    {move,{x,0},{y,6}}.
    {make_fun2,{f,300},0,0,0}.
    {move,{x,0},{y,5}}.
    {make_fun2,{f,298},0,0,0}.
    {move,{x,0},{y,4}}.
    {make_fun2,{f,296},0,0,0}.
    {move,{x,0},{y,3}}.
    {make_fun2,{f,294},0,0,0}.
    {test_heap,2,1}.
    {put_list,{y,27},nil,{y,1}}.
    {move,{x,0},{y,2}}.
    {make_fun2,{f,292},0,0,0}.
    {test_heap,2,1}.
    {put_list,{y,27},nil,{y,0}}.
    {move,{x,0},{y,27}}.
    {make_fun2,{f,290},0,0,0}.
    {test_heap,138,1}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"unused"}}.
    {put,{x,0}}.
    {put,nil}.
    {put_list,{x,1},nil,{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"nested_lc_gen_opt"}}.
    {put,{y,27}}.
    {put,{y,0}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"nested_lc_gen"}}.
    {put,{y,2}}.
    {put,{y,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"intermediary_list3"}}.
    {put,{y,3}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"intermediary_list2"}}.
    {put,{y,4}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"intermediary_list"}}.
    {put,{y,5}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"make_n"}}.
    {put,{y,6}}.
    {put,{literal,[{param1,param2},500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"sorted_gen"}}.
    {put,{y,26}}.
    {put,{y,7}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"function_param"}}.
    {put,{y,9}}.
    {put,{y,8}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"record_extract"}}.
    {put,{y,11}}.
    {put,{y,10}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"single_filter"}}.
    {put,{y,13}}.
    {put,{y,12}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"pen2_lazy_cps"}}.
    {put,{y,14}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"pen2_lazy"}}.
    {put,{y,15}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"pen2_mr"}}.
    {put,{y,16}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"pen2_listsfuns"}}.
    {put,{y,17}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"pen2_foldl"}}.
    {put,{y,18}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"pen2_foldr"}}.
    {put,{y,19}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"pen2_dumb_opt"}}.
    {put,{y,20}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"pen2_dumb"}}.
    {put,{y,21}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"pen2"}}.
    {put,{y,22}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"pen"}}.
    {put,{y,23}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"ses"}}.
    {put,{y,24}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,1}}.
    {put_tuple,3,{x,2}}.
    {put,{literal,"ss"}}.
    {put,{y,25}}.
    {put,{literal,[500]}}.
    {put_list,{x,2},{x,1},{x,0}}.
    {deallocate,28}.
    return.


{function, ss, 1, 61}.
  {label,60}.
    {line,[{location,"lc_tests.erl",152}]}.
    {func_info,{atom,lc_tests},{atom,ss},1}.
  {label,61}.
    {test,is_integer,{f,60},[{x,0}]}.
    {allocate,0,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests.erl",153}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {line,[{location,"lc_tests.erl",153}]}.
    {call,1,{f,287}}.
    {line,[{location,"lc_tests.erl",153}]}.
    {call_ext_last,1,{extfunc,lists,sum,1},0}.


{function, ses, 1, 63}.
  {label,62}.
    {line,[{location,"lc_tests.erl",156}]}.
    {func_info,{atom,lc_tests},{atom,ses},1}.
  {label,63}.
    {test,is_integer,{f,62},[{x,0}]}.
    {allocate,0,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests.erl",157}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {line,[{location,"lc_tests.erl",157}]}.
    {call,1,{f,283}}.
    {line,[{location,"lc_tests.erl",157}]}.
    {call_ext_last,1,{extfunc,lists,sum,1},0}.


{function, pen, 1, 65}.
  {label,64}.
    {line,[{location,"lc_tests.erl",161}]}.
    {func_info,{atom,lc_tests},{atom,pen},1}.
  {label,65}.
    {test,is_integer,{f,64},[{x,0}]}.
    {allocate,1,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests.erl",162}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,0},{x,1}}.
    {call_last,2,{f,273},1}.


{function, pen2, 1, 67}.
  {label,66}.
    {line,[{location,"lc_tests.erl",168}]}.
    {func_info,{atom,lc_tests},{atom,pen2},1}.
  {label,67}.
    {test,is_integer,{f,66},[{x,0}]}.
    {allocate,1,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests.erl",169}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,0},{x,1}}.
    {call_last,2,{f,263},1}.


{function, pen2_dumb, 1, 69}.
  {label,68}.
    {line,[{location,"lc_tests.erl",176}]}.
    {func_info,{atom,lc_tests},{atom,pen2_dumb},1}.
  {label,69}.
    {move,{integer,1},{x,2}}.
    {move,nil,{x,3}}.
    {move,{integer,1},{x,1}}.
    {call_only,4,{f,71}}.


{function, pen2_dumb_helper, 4, 71}.
  {label,70}.
    {line,[{location,"lc_tests.erl",178}]}.
    {func_info,{atom,lc_tests},{atom,pen2_dumb_helper},4}.
  {label,71}.
    {test,is_lt,{f,72},[{x,0},{x,1}]}.
    {move,{x,3},{x,0}}.
    return.
  {label,72}.
    {test,is_lt,{f,73},[{x,0},{x,2}]}.
    {line,[{location,"lc_tests.erl",181}]}.
    {gc_bif,'+',{f,0},4,[{x,1},{integer,1}],{x,1}}.
    {move,{integer,0},{x,2}}.
    {call_only,4,{f,71}}.
  {label,73}.
    {gc_bif,'rem',{f,74},4,[{x,1},{integer,2}],{x,4}}.
    {test,is_eq_exact,{f,74},[{x,4},{integer,0}]}.
    {gc_bif,'rem',{f,74},4,[{x,2},{integer,2}],{x,4}}.
    {test,is_eq_exact,{f,74},[{x,4},{integer,1}]}.
    {gc_bif,'+',{f,74},4,[{x,1},{x,2}],{x,4}}.
    {gc_bif,'rem',{f,74},5,[{x,4},{integer,5}],{x,4}}.
    {test,is_ne_exact,{f,74},[{x,4},{integer,0}]}.
    {allocate_zero,2,4}.
    {line,[{location,"lc_tests.erl",183}]}.
    {gc_bif,'+',{f,0},4,[{x,2},{integer,1}],{x,4}}.
    {move,{x,2},{y,0}}.
    {move,{x,4},{x,2}}.
    {move,{x,1},{y,1}}.
    {line,[{location,"lc_tests.erl",183}]}.
    {call,4,{f,71}}.
    {test_heap,6,1}.
    {put_list,{y,0},nil,{x,1}}.
    {put_list,{y,1},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,74}.
    {line,[{location,"lc_tests.erl",185}]}.
    {gc_bif,'+',{f,0},4,[{x,2},{integer,1}],{x,2}}.
    {call_only,4,{f,71}}.


{function, pen2_dumb_opt, 1, 76}.
  {label,75}.
    {line,[{location,"lc_tests.erl",189}]}.
    {func_info,{atom,lc_tests},{atom,pen2_dumb_opt},1}.
  {label,76}.
    {allocate,0,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests.erl",190}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {line,[{location,"lc_tests.erl",190}]}.
    {call_ext,1,{extfunc,lists,reverse,1}}.
    {move,{x,0},{x,3}}.
    {move,{x,0},{x,2}}.
    {move,nil,{x,4}}.
    {move,{x,0},{x,1}}.
    {call_last,5,{f,78},0}.


{function, pen2_dumb_opt_helper, 5, 78}.
  {label,77}.
    {line,[{location,"lc_tests.erl",192}]}.
    {func_info,{atom,lc_tests},{atom,pen2_dumb_opt_helper},5}.
  {label,78}.
    {test,is_nonempty_list,{f,81},[{x,0}]}.
    {get_list,{x,0},{x,5},{x,6}}.
    {test,is_nil,{f,79},[{x,1}]}.
    {move,{x,3},{x,1}}.
    {move,{x,6},{x,0}}.
    {call_only,5,{f,78}}.
  {label,79}.
    {gc_bif,'rem',{f,80},7,[{x,5},{integer,2}],{x,7}}.
    {test,is_ne_exact,{f,80},[{x,7},{integer,0}]}.
    {move,{x,3},{x,1}}.
    {move,{x,6},{x,0}}.
    {call_only,5,{f,78}}.
  {label,80}.
    {test,is_nonempty_list,{f,77},[{x,1}]}.
    {get_list,{x,1},{x,7},{x,8}}.
    {gc_bif,'rem',{f,82},9,[{x,7},{integer,2}],{x,9}}.
    {test,is_eq_exact,{f,82},[{x,9},{integer,1}]}.
    {gc_bif,'+',{f,82},9,[{x,5},{x,7}],{x,9}}.
    {gc_bif,'rem',{f,82},10,[{x,9},{integer,5}],{x,9}}.
    {test,is_ne_exact,{f,82},[{x,9},{integer,0}]}.
    {test_heap,6,9}.
    {put_list,{x,7},nil,{x,6}}.
    {put_list,{x,5},{x,6},{x,5}}.
    {put_list,{x,5},{x,4},{x,4}}.
    {move,{x,8},{x,1}}.
    {call_only,5,{f,78}}.
  {label,81}.
    {test,is_nil,{f,82},[{x,0}]}.
    {move,{x,4},{x,0}}.
    return.
  {label,82}.
    {test,is_nonempty_list,{f,77},[{x,1}]}.
    {get_list,{x,1},{x,5},{x,1}}.
    {call_only,5,{f,78}}.


{function, pen2_foldl, 1, 84}.
  {label,83}.
    {line,[{location,"lc_tests.erl",205}]}.
    {func_info,{atom,lc_tests},{atom,pen2_foldl},1}.
  {label,84}.
    {test,is_integer,{f,83},[{x,0}]}.
    {allocate_zero,1,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests.erl",206}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,257},0,0,1}.
    {move,{y,0},{x,2}}.
    {move,nil,{x,1}}.
    {trim,1,0}.
    {line,[{location,"lc_tests.erl",207}]}.
    {call_ext,3,{extfunc,lists,foldl,3}}.
    {line,[{location,"lc_tests.erl",207}]}.
    {call_ext_last,1,{extfunc,lists,reverse,1},0}.


{function, pen2_foldr, 1, 86}.
  {label,85}.
    {line,[{location,"lc_tests.erl",221}]}.
    {func_info,{atom,lc_tests},{atom,pen2_foldr},1}.
  {label,86}.
    {test,is_integer,{f,85},[{x,0}]}.
    {allocate_zero,1,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests.erl",222}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,251},0,0,1}.
    {move,{y,0},{x,2}}.
    {move,nil,{x,1}}.
    {line,[{location,"lc_tests.erl",223}]}.
    {call_ext_last,3,{extfunc,lists,foldr,3},1}.


{function, pen2_listsfuns, 1, 88}.
  {label,87}.
    {line,[{location,"lc_tests.erl",237}]}.
    {func_info,{atom,lc_tests},{atom,pen2_listsfuns},1}.
  {label,88}.
    {test,is_integer,{f,87},[{x,0}]}.
    {allocate_zero,2,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests.erl",238}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{x,0},{y,1}}.
    {make_fun2,{f,249},0,0,0}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,237},0,0,0}.
    {move,{y,1},{x,1}}.
    {line,[{location,"lc_tests.erl",240}]}.
    {call_ext,2,{extfunc,lists,filter,2}}.
    {move,{y,1},{x,1}}.
    {kill,{y,1}}.
    {line,[{location,"lc_tests.erl",240}]}.
    {call,2,{f,239}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {line,[{location,"lc_tests.erl",239}]}.
    {call_ext_last,2,{extfunc,lists,filter,2},2}.


{function, pen2_mr, 1, 90}.
  {label,89}.
    {line,[{location,"lc_tests.erl",244}]}.
    {func_info,{atom,lc_tests},{atom,pen2_mr},1}.
  {label,90}.
    {test,is_integer,{f,89},[{x,0}]}.
    {allocate_zero,1,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests.erl",245}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,228},0,0,0}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {make_fun2,{f,219},0,0,2}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {line,[{location,"lc_tests.erl",258}]}.
    {call_fun,1}.
    {deallocate,1}.
    return.


{function, pen2_lazy, 1, 92}.
  {label,91}.
    {line,[{location,"lc_tests.erl",261}]}.
    {func_info,{atom,lc_tests},{atom,pen2_lazy},1}.
  {label,92}.
    {test,is_integer,{f,91},[{x,0}]}.
    {allocate_zero,1,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests.erl",262}]}.
    {call,2,{f,12}}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,210},0,0,0}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {make_fun2,{f,201},0,0,2}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {line,[{location,"lc_tests.erl",273}]}.
    {call_fun,1}.
    {deallocate,1}.
    return.


{function, pen2_lazy_cps, 1, 94}.
  {label,93}.
    {line,[{location,"lc_tests.erl",276}]}.
    {func_info,{atom,lc_tests},{atom,pen2_lazy_cps},1}.
  {label,94}.
    {test,is_integer,{f,93},[{x,0}]}.
    {allocate_zero,1,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests.erl",277}]}.
    {call,2,{f,20}}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,192},0,0,0}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {make_fun2,{f,183},0,0,2}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {line,[{location,"lc_tests.erl",288}]}.
    {call_fun,1}.
    {deallocate,1}.
    return.


{function, unused, 0, 96}.
  {label,95}.
    {line,[{location,"lc_tests.erl",292}]}.
    {func_info,{atom,lc_tests},{atom,unused},0}.
  {label,96}.
    {allocate,0,0}.
    {move,{integer,1000},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests.erl",293}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {line,[{location,"lc_tests.erl",293}]}.
    {call,1,{f,180}}.
    {move,{atom,ok},{x,0}}.
    {deallocate,0}.
    return.


{function, single_filter, 1, 98}.
  {label,97}.
    {line,[{location,"lc_tests.erl",299}]}.
    {func_info,{atom,lc_tests},{atom,single_filter},1}.
  {label,98}.
    {test,is_list,{f,97},[{x,0}]}.
    {call_only,1,{f,176}}.


{function, record_extract, 1, 100}.
  {label,99}.
    {line,[{location,"lc_tests.erl",305}]}.
    {func_info,{atom,lc_tests},{atom,record_extract},1}.
  {label,100}.
    {test,is_list,{f,99},[{x,0}]}.
    {call_only,1,{f,172}}.


{function, function_param_lc, 2, 102}.
  {label,101}.
    {line,[{location,"lc_tests.erl",309}]}.
    {func_info,{atom,lc_tests},{atom,function_param_lc},2}.
  {label,102}.
    {test,is_list,{f,101},[{x,1}]}.
    {move,{x,1},{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{x,2},{x,0}}.
    {call_only,2,{f,168}}.


{function, sorted_gen, 1, 104}.
  {label,103}.
    {line,[{location,"lc_tests.erl",313}]}.
    {func_info,{atom,lc_tests},{atom,sorted_gen},1}.
  {label,104}.
    {test,is_list,{f,103},[{x,0}]}.
    {allocate,1,1}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,161},0,0,0}.
    {move,{y,0},{x,1}}.
    {trim,1,0}.
    {line,[{location,"lc_tests.erl",315}]}.
    {call_ext,2,{extfunc,lists,sort,2}}.
    {call_last,1,{f,164},0}.


{function, make_n, 2, 106}.
  {label,105}.
    {line,[{location,"lc_tests.erl",323}]}.
    {func_info,{atom,lc_tests},{atom,make_n},2}.
  {label,106}.
    {test,is_integer,{f,105},[{x,1}]}.
    {allocate,1,2}.
    {move,{x,0},{y,0}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests.erl",324}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,0},{x,1}}.
    {call_last,2,{f,157},1}.


{function, intermediary_list, 1, 108}.
  {label,107}.
    {line,[{location,"lc_tests.erl",327}]}.
    {func_info,{atom,lc_tests},{atom,intermediary_list},1}.
  {label,108}.
    {test,is_integer,{f,107},[{x,0}]}.
    {allocate,0,1}.
    {move,{integer,50},{x,1}}.
    {line,[{location,"lc_tests.erl",328}]}.
    {call,2,{f,9}}.
    {call_last,1,{f,153},0}.


{function, intermediary_list2, 1, 110}.
  {label,109}.
    {line,[{location,"lc_tests.erl",331}]}.
    {func_info,{atom,lc_tests},{atom,intermediary_list2},1}.
  {label,110}.
    {test,is_integer,{f,109},[{x,0}]}.
    {allocate,0,1}.
    {move,{integer,50},{x,1}}.
    {line,[{location,"lc_tests.erl",332}]}.
    {call,2,{f,9}}.
    {line,[{location,"lc_tests.erl",333}]}.
    {call,1,{f,149}}.
    {call_last,1,{f,146},0}.


{function, intermediary_list3, 1, 112}.
  {label,111}.
    {line,[{location,"lc_tests.erl",336}]}.
    {func_info,{atom,lc_tests},{atom,intermediary_list3},1}.
  {label,112}.
    {test,is_integer,{f,111},[{x,0}]}.
    {allocate,0,1}.
    {move,{integer,50},{x,1}}.
    {line,[{location,"lc_tests.erl",337}]}.
    {call,2,{f,9}}.
    {line,[{location,"lc_tests.erl",338}]}.
    {call,1,{f,143}}.
    {line,[{location,"lc_tests.erl",339}]}.
    {call,1,{f,140}}.
    {call_last,1,{f,136},0}.


{function, nested_lc_gen, 1, 114}.
  {label,113}.
    {line,[{location,"lc_tests.erl",346}]}.
    {func_info,{atom,lc_tests},{atom,nested_lc_gen},1}.
  {label,114}.
    {test,is_list,{f,113},[{x,0}]}.
    {allocate,0,1}.
    {line,[{location,"lc_tests.erl",348}]}.
    {call,1,{f,129}}.
    {call_last,1,{f,133},0}.


{function, nested_lc_gen_opt, 1, 116}.
  {label,115}.
    {line,[{location,"lc_tests.erl",351}]}.
    {func_info,{atom,lc_tests},{atom,nested_lc_gen_opt},1}.
  {label,116}.
    {allocate,0,1}.
    {line,[{location,"lc_tests.erl",352}]}.
    {call,1,{f,125}}.
    {line,[{location,"lc_tests.erl",352}]}.
    {call_ext,1,{extfunc,lists,reverse,1}}.
    {move,nil,{x,1}}.
    {call_last,2,{f,118},0}.


{function, nested_lc_gen_opt_helper, 2, 118}.
  {label,117}.
    {line,[{location,"lc_tests.erl",354}]}.
    {func_info,{atom,lc_tests},{atom,nested_lc_gen_opt_helper},2}.
  {label,118}.
    {test,is_nonempty_list,{f,119},[{x,0}]}.
    {get_list,{x,0},{x,2},{x,3}}.
    {line,[{location,"lc_tests.erl",355}]}.
    {gc_bif,'*',{f,0},4,[{x,2},{x,2}],{x,0}}.
    {test_heap,6,4}.
    {put_tuple,3,{x,4}}.
    {put,{atom,test_record}}.
    {put,{x,2}}.
    {put,{x,0}}.
    {put_list,{x,4},{x,1},{x,1}}.
    {move,{x,3},{x,0}}.
    {call_only,2,{f,118}}.
  {label,119}.
    {test,is_nil,{f,117},[{x,0}]}.
    {move,{x,1},{x,0}}.
    return.


{function, module_info, 0, 121}.
  {label,120}.
    {line,[]}.
    {func_info,{atom,lc_tests},{atom,module_info},0}.
  {label,121}.
    {move,{atom,lc_tests},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 123}.
  {label,122}.
    {line,[]}.
    {func_info,{atom,lc_tests},{atom,module_info},1}.
  {label,123}.
    {move,{x,0},{x,1}}.
    {move,{atom,lc_tests},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.


{function, '-nested_lc_gen_opt/1-lc$^0/1-0-', 1, 125}.
  {label,124}.
    {line,[{location,"lc_tests.erl",352}]}.
    {func_info,{atom,lc_tests},{atom,'-nested_lc_gen_opt/1-lc$^0/1-0-'},1}.
  {label,125}.
    {test,is_nonempty_list,{f,127},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {gc_bif,'rem',{f,126},3,[{x,1},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,126},[{x,3},{integer,1}]}.
    {allocate,1,3}.
    {move,{x,2},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests.erl",352}]}.
    {call,1,{f,125}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,126}.
    {move,{x,2},{x,0}}.
    {call_only,1,{f,125}}.
  {label,127}.
    {test,is_nil,{f,124},[{x,0}]}.
    return.


{function, '-nested_lc_gen/1-lc$^0/1-1-', 1, 129}.
  {label,128}.
    {line,[{location,"lc_tests.erl",348}]}.
    {func_info,{atom,lc_tests},{atom,'-nested_lc_gen/1-lc$^0/1-1-'},1}.
  {label,129}.
    {test,is_nonempty_list,{f,131},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {gc_bif,'rem',{f,130},3,[{x,1},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,130},[{x,3},{integer,1}]}.
    {allocate,1,3}.
    {move,{x,2},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests.erl",348}]}.
    {call,1,{f,129}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,130}.
    {move,{x,2},{x,0}}.
    {call_only,1,{f,129}}.
  {label,131}.
    {test,is_nil,{f,128},[{x,0}]}.
    return.


{function, '-nested_lc_gen/1-lc$^1/1-0-', 1, 133}.
  {label,132}.
    {line,[{location,"lc_tests.erl",347}]}.
    {func_info,{atom,lc_tests},{atom,'-nested_lc_gen/1-lc$^1/1-0-'},1}.
  {label,133}.
    {test,is_nonempty_list,{f,134},[{x,0}]}.
    {allocate_zero,2,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {line,[{location,"lc_tests.erl",347}]}.
    {gc_bif,'*',{f,0},3,[{x,1},{x,1}],{y,0}}.
    {move,{x,2},{x,0}}.
    {move,{x,1},{y,1}}.
    {line,[{location,"lc_tests.erl",348}]}.
    {call,1,{f,133}}.
    {test_heap,6,1}.
    {put_tuple,3,{x,1}}.
    {put,{atom,test_record}}.
    {put,{y,1}}.
    {put,{y,0}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,134}.
    {test,is_nil,{f,132},[{x,0}]}.
    return.


{function, '-intermediary_list3/1-lc$^2/1-2-', 1, 136}.
  {label,135}.
    {line,[{location,"lc_tests.erl",340}]}.
    {func_info,{atom,lc_tests},{atom,'-intermediary_list3/1-lc$^2/1-2-'},1}.
  {label,136}.
    {test,is_nonempty_list,{f,138},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_tuple,{f,137},[{x,1}]}.
    {test,test_arity,{f,137},[{x,1},2]}.
    {get_tuple_element,{x,1},0,{x,3}}.
    {get_tuple_element,{x,1},1,{x,4}}.
    {test,is_tuple,{f,137},[{x,4}]}.
    {test,test_arity,{f,137},[{x,4},2]}.
    {get_tuple_element,{x,4},0,{x,5}}.
    {get_tuple_element,{x,4},1,{x,6}}.
    {test,is_tuple,{f,137},[{x,6}]}.
    {test,test_arity,{f,137},[{x,6},3]}.
    {get_tuple_element,{x,6},0,{x,7}}.
    {get_tuple_element,{x,6},1,{x,8}}.
    {get_tuple_element,{x,6},2,{x,9}}.
    {test,is_eq_exact,{f,137},[{x,7},{atom,test_record}]}.
    {allocate_zero,1,10}.
    {line,[{location,"lc_tests.erl",340}]}.
    {gc_bif,'*',{f,0},10,[{x,3},{x,5}],{x,0}}.
    {line,[{location,"lc_tests.erl",340}]}.
    {gc_bif,'*',{f,0},10,[{x,0},{x,8}],{x,0}}.
    {line,[{location,"lc_tests.erl",340}]}.
    {gc_bif,'*',{f,0},10,[{x,0},{x,9}],{y,0}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests.erl",340}]}.
    {call,1,{f,136}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,137}.
    {move,{x,2},{x,0}}.
    {call_only,1,{f,136}}.
  {label,138}.
    {test,is_nil,{f,135},[{x,0}]}.
    return.


{function, '-intermediary_list3/1-lc$^1/1-1-', 1, 140}.
  {label,139}.
    {line,[{location,"lc_tests.erl",339}]}.
    {func_info,{atom,lc_tests},{atom,'-intermediary_list3/1-lc$^1/1-1-'},1}.
  {label,140}.
    {test,is_nonempty_list,{f,141},[{x,0}]}.
    {allocate,2,1}.
    {get_list,{x,0},{y,1},{y,0}}.
    {move,{integer,100},{x,0}}.
    {line,[{location,"lc_tests.erl",339}]}.
    {call_ext,1,{extfunc,random,uniform,1}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests.erl",339}]}.
    {call,1,{f,140}}.
    {test_heap,5,1}.
    {put_tuple,2,{x,1}}.
    {put,{y,0}}.
    {put,{y,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,141}.
    {test,is_nil,{f,139},[{x,0}]}.
    return.


{function, '-intermediary_list3/1-lc$^0/1-0-', 1, 143}.
  {label,142}.
    {line,[{location,"lc_tests.erl",338}]}.
    {func_info,{atom,lc_tests},{atom,'-intermediary_list3/1-lc$^0/1-0-'},1}.
  {label,143}.
    {test,is_nonempty_list,{f,144},[{x,0}]}.
    {allocate,2,1}.
    {get_list,{x,0},{y,1},{y,0}}.
    {move,{integer,50},{x,0}}.
    {line,[{location,"lc_tests.erl",338}]}.
    {call_ext,1,{extfunc,random,uniform,1}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests.erl",338}]}.
    {call,1,{f,143}}.
    {test_heap,5,1}.
    {put_tuple,2,{x,1}}.
    {put,{y,0}}.
    {put,{y,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,144}.
    {test,is_nil,{f,142},[{x,0}]}.
    return.


{function, '-intermediary_list2/1-lc$^1/1-1-', 1, 146}.
  {label,145}.
    {line,[{location,"lc_tests.erl",334}]}.
    {func_info,{atom,lc_tests},{atom,'-intermediary_list2/1-lc$^1/1-1-'},1}.
  {label,146}.
    {test,is_nonempty_list,{f,147},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {line,[{location,"lc_tests.erl",334}]}.
    {gc_bif,'*',{f,0},3,[{x,1},{x,1}],{y,0}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests.erl",334}]}.
    {call,1,{f,146}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,147}.
    {test,is_nil,{f,145},[{x,0}]}.
    return.


{function, '-intermediary_list2/1-lc$^0/1-0-', 1, 149}.
  {label,148}.
    {line,[{location,"lc_tests.erl",333}]}.
    {func_info,{atom,lc_tests},{atom,'-intermediary_list2/1-lc$^0/1-0-'},1}.
  {label,149}.
    {test,is_nonempty_list,{f,150},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_tuple,{f,151},[{x,1}]}.
    {test,test_arity,{f,151},[{x,1},3]}.
    {get_tuple_element,{x,1},0,{x,0}}.
    {get_tuple_element,{x,1},2,{x,3}}.
    {test,is_eq_exact,{f,151},[{x,0},{atom,test_record}]}.
    {move,{x,2},{x,0}}.
    {move,{x,3},{y,0}}.
    {line,[{location,"lc_tests.erl",333}]}.
    {call,1,{f,149}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,150}.
    {test,is_nil,{f,148},[{x,0}]}.
    return.
  {label,151}.
    {move,{literal,{badrecord,test_record}},{x,0}}.
    {line,[{location,"lc_tests.erl",333}]}.
    {call_ext,1,{extfunc,erlang,error,1}}.


{function, '-intermediary_list/1-lc$^0/1-0-', 1, 153}.
  {label,152}.
    {line,[{location,"lc_tests.erl",329}]}.
    {func_info,{atom,lc_tests},{atom,'-intermediary_list/1-lc$^0/1-0-'},1}.
  {label,153}.
    {test,is_nonempty_list,{f,154},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_tuple,{f,155},[{x,1}]}.
    {test,test_arity,{f,155},[{x,1},3]}.
    {get_tuple_element,{x,1},0,{x,0}}.
    {get_tuple_element,{x,1},2,{x,3}}.
    {test,is_eq_exact,{f,155},[{x,0},{atom,test_record}]}.
    {move,{x,2},{x,0}}.
    {move,{x,3},{y,0}}.
    {line,[{location,"lc_tests.erl",329}]}.
    {call,1,{f,153}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,154}.
    {test,is_nil,{f,152},[{x,0}]}.
    return.
  {label,155}.
    {move,{literal,{badrecord,test_record}},{x,0}}.
    {line,[{location,"lc_tests.erl",329}]}.
    {call_ext,1,{extfunc,erlang,error,1}}.


{function, '-make_n/2-lc$^0/1-0-', 2, 157}.
  {label,156}.
    {line,[{location,"lc_tests.erl",324}]}.
    {func_info,{atom,lc_tests},{atom,'-make_n/2-lc$^0/1-0-'},2}.
  {label,157}.
    {test,is_nonempty_list,{f,158},[{x,0}]}.
    {allocate,1,2}.
    {get_list,{x,0},{x,2},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests.erl",324}]}.
    {call,2,{f,157}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,158}.
    {test,is_nil,{f,159},[{x,0}]}.
    return.
  {label,159}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests.erl",324}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-sorted_gen/1-fun-0-', 2, 161}.
  {label,160}.
    {line,[{location,"lc_tests.erl",315}]}.
    {func_info,{atom,lc_tests},{atom,'-sorted_gen/1-fun-0-'},2}.
  {label,161}.
    {test,is_tuple,{f,162},[{x,0}]}.
    {test,test_arity,{f,162},[{x,0},3]}.
    {get_tuple_element,{x,0},0,{x,2}}.
    {get_tuple_element,{x,0},1,{x,3}}.
    {test,is_eq_exact,{f,162},[{x,2},{atom,test_record}]}.
    {test,is_tuple,{f,162},[{x,1}]}.
    {test,test_arity,{f,162},[{x,1},3]}.
    {get_tuple_element,{x,1},0,{x,0}}.
    {get_tuple_element,{x,1},1,{x,2}}.
    {test,is_eq_exact,{f,162},[{x,0},{atom,test_record}]}.
    {bif,'<',{f,0},[{x,3},{x,2}],{x,0}}.
    return.
  {label,162}.
    {move,{literal,{badrecord,test_record}},{x,0}}.
    {line,[{location,"lc_tests.erl",316}]}.
    {call_ext,1,{extfunc,erlang,error,1}}.


{function, '-sorted_gen/1-lc$^0/1-0-', 1, 164}.
  {label,163}.
    {line,[{location,"lc_tests.erl",314}]}.
    {func_info,{atom,lc_tests},{atom,'-sorted_gen/1-lc$^0/1-0-'},1}.
  {label,164}.
    {test,is_nonempty_list,{f,165},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_tuple,{f,166},[{x,1}]}.
    {test,test_arity,{f,166},[{x,1},3]}.
    {get_tuple_element,{x,1},0,{x,0}}.
    {get_tuple_element,{x,1},1,{x,3}}.
    {test,is_eq_exact,{f,166},[{x,0},{atom,test_record}]}.
    {move,{x,2},{x,0}}.
    {move,{x,3},{y,0}}.
    {line,[{location,"lc_tests.erl",315}]}.
    {call,1,{f,164}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,165}.
    {test,is_nil,{f,163},[{x,0}]}.
    return.
  {label,166}.
    {move,{literal,{badrecord,test_record}},{x,0}}.
    {line,[{location,"lc_tests.erl",314}]}.
    {call_ext,1,{extfunc,erlang,error,1}}.


{function, '-function_param_lc/2-lc$^0/1-0-', 2, 168}.
  {label,167}.
    {line,[{location,"lc_tests.erl",310}]}.
    {func_info,{atom,lc_tests},{atom,'-function_param_lc/2-lc$^0/1-0-'},2}.
  {label,168}.
    {test,is_nonempty_list,{f,169},[{x,0}]}.
    {allocate,2,2}.
    {get_list,{x,0},{y,0},{x,0}}.
    {move,{x,1},{y,1}}.
    {line,[{location,"lc_tests.erl",310}]}.
    {call,2,{f,168}}.
    {test_heap,6,1}.
    {put_list,{y,0},nil,{x,1}}.
    {put_list,{y,1},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,169}.
    {test,is_nil,{f,170},[{x,0}]}.
    return.
  {label,170}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests.erl",310}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-record_extract/1-lc$^0/1-0-', 1, 172}.
  {label,171}.
    {line,[{location,"lc_tests.erl",306}]}.
    {func_info,{atom,lc_tests},{atom,'-record_extract/1-lc$^0/1-0-'},1}.
  {label,172}.
    {test,is_nonempty_list,{f,173},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_tuple,{f,174},[{x,1}]}.
    {test,test_arity,{f,174},[{x,1},3]}.
    {get_tuple_element,{x,1},0,{x,0}}.
    {get_tuple_element,{x,1},1,{x,3}}.
    {test,is_eq_exact,{f,174},[{x,0},{atom,test_record}]}.
    {move,{x,2},{x,0}}.
    {move,{x,3},{y,0}}.
    {line,[{location,"lc_tests.erl",306}]}.
    {call,1,{f,172}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,173}.
    {test,is_nil,{f,171},[{x,0}]}.
    return.
  {label,174}.
    {move,{literal,{badrecord,test_record}},{x,0}}.
    {line,[{location,"lc_tests.erl",306}]}.
    {call_ext,1,{extfunc,erlang,error,1}}.


{function, '-single_filter/1-lc$^0/1-0-', 1, 176}.
  {label,175}.
    {line,[{location,"lc_tests.erl",300}]}.
    {func_info,{atom,lc_tests},{atom,'-single_filter/1-lc$^0/1-0-'},1}.
  {label,176}.
    {test,is_nonempty_list,{f,178},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_integer,{f,177},[{x,1}]}.
    {allocate,1,3}.
    {move,{x,2},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests.erl",300}]}.
    {call,1,{f,176}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,177}.
    {move,{x,2},{x,0}}.
    {call_only,1,{f,176}}.
  {label,178}.
    {test,is_nil,{f,175},[{x,0}]}.
    return.


{function, '-unused/0-lc$^0/1-0-', 1, 180}.
  {label,179}.
    {line,[{location,"lc_tests.erl",293}]}.
    {func_info,{atom,lc_tests},{atom,'-unused/0-lc$^0/1-0-'},1}.
  {label,180}.
    {test,is_nonempty_list,{f,181},[{x,0}]}.
    {allocate,1,1}.
    {get_list,{x,0},{x,1},{y,0}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests.erl",293}]}.
    {call_ext,1,{extfunc,timer,sleep,1}}.
    {move,{y,0},{x,0}}.
    {call_last,1,{f,180},1}.
  {label,181}.
    {test,is_nil,{f,179},[{x,0}]}.
    return.


{function, '-pen2_lazy_cps/1-anonymous-5-', 3, 183}.
  {label,182}.
    {line,[]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_lazy_cps/1-anonymous-5-'},3}.
  {label,183}.
    {call_only,3,{f,185}}.


{function, '-pen2_lazy_cps/1-Red_xs/1-1-', 3, 185}.
  {label,184}.
    {line,[{location,"lc_tests.erl",283}]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_lazy_cps/1-Red_xs/1-1-'},3}.
  {label,185}.
    {allocate,3,3}.
    {move,{x,0},{y,2}}.
    {move,{x,1},{x,0}}.
    {move,{x,2},{x,1}}.
    {move,{x,0},{y,0}}.
    {move,{x,1},{y,1}}.
    {make_fun2,{f,190},0,0,2}.
    {test,is_nonempty_list,{f,187},[{y,2}]}.
    {get_list,{y,2},{x,1},{x,2}}.
    {gc_bif,'rem',{f,186},3,[{x,1},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,186},[{x,3},{integer,0}]}.
    {move,{x,0},{x,2}}.
    {move,{y,0},{x,1}}.
    {move,{y,1},{x,3}}.
    {move,{y,2},{x,0}}.
    {line,[{location,"lc_tests.erl",284}]}.
    {call_fun,3}.
    {deallocate,3}.
    return.
  {label,186}.
    {move,{x,0},{y,2}}.
    {trim,2,1}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests.erl",285}]}.
    {call_fun,0}.
    {move,{y,0},{x,1}}.
    {line,[{location,"lc_tests.erl",285}]}.
    {call_fun,1}.
    {deallocate,1}.
    return.
  {label,187}.
    {test,is_nil,{f,188},[{y,2}]}.
    {move,nil,{x,0}}.
    {deallocate,3}.
    return.
  {label,188}.
    {test_heap,2,0}.
    {put_tuple,1,{x,1}}.
    {put,{y,2}}.
    {line,[{location,"lc_tests.erl",283}]}.
    {case_end,{x,1}}.


{function, '-pen2_lazy_cps/1-anonymous-4-', 3, 190}.
  {label,189}.
    {line,[]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_lazy_cps/1-anonymous-4-'},3}.
  {label,190}.
    {call_only,3,{f,185}}.


{function, '-pen2_lazy_cps/1-anonymous-3-', 3, 192}.
  {label,191}.
    {line,[]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_lazy_cps/1-anonymous-3-'},3}.
  {label,192}.
    {call_only,3,{f,194}}.


{function, '-pen2_lazy_cps/1-Red_ys/3-0-', 3, 194}.
  {label,193}.
    {line,[{location,"lc_tests.erl",278}]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_lazy_cps/1-Red_ys/3-0-'},3}.
  {label,194}.
    {allocate,5,3}.
    {init,{y,0}}.
    {init,{y,1}}.
    {move,{x,2},{y,2}}.
    {move,{x,1},{y,3}}.
    {move,{x,0},{y,4}}.
    {make_fun2,{f,199},0,0,0}.
    {test,is_nonempty_list,{f,197},[{y,4}]}.
    {get_list,{y,4},{x,1},{x,2}}.
    {test,is_nonempty_list,{f,196},[{y,3}]}.
    {get_list,{y,3},{x,3},{x,4}}.
    {gc_bif,'rem',{f,195},5,[{x,3},{integer,2}],{x,2}}.
    {test,is_eq_exact,{f,195},[{x,2},{integer,1}]}.
    {gc_bif,'+',{f,195},5,[{x,1},{x,3}],{x,2}}.
    {gc_bif,'rem',{f,195},5,[{x,2},{integer,5}],{x,2}}.
    {bif,'=/=',{f,195},[{x,2},{integer,0}],{x,2}}.
    {move,{x,2},{x,5}}.
    {test,is_eq_exact,{f,195},[{x,5},{atom,true}]}.
    {move,{x,0},{y,0}}.
    {move,{x,4},{x,0}}.
    {move,{x,3},{y,1}}.
    {move,{x,1},{y,3}}.
    {line,[{location,"lc_tests.erl",280}]}.
    {call_fun,0}.
    {move,{y,2},{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,3}}.
    {move,{y,4},{x,0}}.
    {move,{y,1},{y,4}}.
    {trim,3,2}.
    {line,[{location,"lc_tests.erl",280}]}.
    {call_fun,3}.
    {test_heap,6,1}.
    {put_list,{y,1},nil,{x,1}}.
    {put_list,{y,0},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,195}.
    {move,{x,0},{y,3}}.
    {move,{x,4},{x,0}}.
    {line,[{location,"lc_tests.erl",281}]}.
    {call_fun,0}.
    {move,{y,2},{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{y,3},{x,3}}.
    {move,{y,4},{x,0}}.
    {line,[{location,"lc_tests.erl",281}]}.
    {call_fun,3}.
    {deallocate,5}.
    return.
  {label,196}.
    {test,is_nil,{f,197},[{y,3}]}.
    {kill,{y,3}}.
    {kill,{y,4}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests.erl",282}]}.
    {call_fun,0}.
    {move,{y,2},{x,1}}.
    {line,[{location,"lc_tests.erl",282}]}.
    {call_fun,1}.
    {deallocate,5}.
    return.
  {label,197}.
    {test_heap,4,0}.
    {put_tuple,3,{x,1}}.
    {put,{y,4}}.
    {put,{y,3}}.
    {put,{y,2}}.
    {line,[{location,"lc_tests.erl",278}]}.
    {case_end,{x,1}}.


{function, '-pen2_lazy_cps/1-anonymous-2-', 3, 199}.
  {label,198}.
    {line,[]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_lazy_cps/1-anonymous-2-'},3}.
  {label,199}.
    {call_only,3,{f,194}}.


{function, '-pen2_lazy/1-anonymous-5-', 3, 201}.
  {label,200}.
    {line,[]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_lazy/1-anonymous-5-'},3}.
  {label,201}.
    {call_only,3,{f,203}}.


{function, '-pen2_lazy/1-Red_xs/1-1-', 3, 203}.
  {label,202}.
    {line,[{location,"lc_tests.erl",268}]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_lazy/1-Red_xs/1-1-'},3}.
  {label,203}.
    {allocate,3,3}.
    {move,{x,0},{y,2}}.
    {move,{x,1},{x,0}}.
    {move,{x,2},{x,1}}.
    {move,{x,0},{y,0}}.
    {move,{x,1},{y,1}}.
    {make_fun2,{f,208},0,0,2}.
    {test,is_nonempty_list,{f,205},[{y,2}]}.
    {get_list,{y,2},{x,1},{x,2}}.
    {gc_bif,'rem',{f,204},3,[{x,1},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,204},[{x,3},{integer,0}]}.
    {move,{x,0},{x,2}}.
    {move,{y,0},{x,1}}.
    {move,{y,1},{x,3}}.
    {move,{y,2},{x,0}}.
    {line,[{location,"lc_tests.erl",269}]}.
    {call_fun,3}.
    {deallocate,3}.
    return.
  {label,204}.
    {move,{x,0},{y,2}}.
    {trim,2,1}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests.erl",270}]}.
    {call_fun,0}.
    {move,{y,0},{x,1}}.
    {line,[{location,"lc_tests.erl",270}]}.
    {call_fun,1}.
    {deallocate,1}.
    return.
  {label,205}.
    {test,is_nil,{f,206},[{y,2}]}.
    {move,nil,{x,0}}.
    {deallocate,3}.
    return.
  {label,206}.
    {test_heap,2,0}.
    {put_tuple,1,{x,1}}.
    {put,{y,2}}.
    {line,[{location,"lc_tests.erl",268}]}.
    {case_end,{x,1}}.


{function, '-pen2_lazy/1-anonymous-4-', 3, 208}.
  {label,207}.
    {line,[]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_lazy/1-anonymous-4-'},3}.
  {label,208}.
    {call_only,3,{f,203}}.


{function, '-pen2_lazy/1-anonymous-3-', 3, 210}.
  {label,209}.
    {line,[]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_lazy/1-anonymous-3-'},3}.
  {label,210}.
    {call_only,3,{f,212}}.


{function, '-pen2_lazy/1-Red_ys/3-0-', 3, 212}.
  {label,211}.
    {line,[{location,"lc_tests.erl",263}]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_lazy/1-Red_ys/3-0-'},3}.
  {label,212}.
    {allocate,5,3}.
    {init,{y,0}}.
    {init,{y,1}}.
    {move,{x,2},{y,2}}.
    {move,{x,1},{y,3}}.
    {move,{x,0},{y,4}}.
    {make_fun2,{f,217},0,0,0}.
    {test,is_nonempty_list,{f,215},[{y,4}]}.
    {get_list,{y,4},{x,1},{x,2}}.
    {test,is_nonempty_list,{f,214},[{y,3}]}.
    {get_list,{y,3},{x,3},{x,4}}.
    {gc_bif,'rem',{f,213},5,[{x,3},{integer,2}],{x,2}}.
    {test,is_eq_exact,{f,213},[{x,2},{integer,1}]}.
    {gc_bif,'+',{f,213},5,[{x,1},{x,3}],{x,2}}.
    {gc_bif,'rem',{f,213},5,[{x,2},{integer,5}],{x,2}}.
    {bif,'=/=',{f,213},[{x,2},{integer,0}],{x,2}}.
    {move,{x,2},{x,5}}.
    {test,is_eq_exact,{f,213},[{x,5},{atom,true}]}.
    {move,{x,0},{y,0}}.
    {move,{x,4},{x,0}}.
    {move,{x,3},{y,1}}.
    {move,{x,1},{y,3}}.
    {line,[{location,"lc_tests.erl",265}]}.
    {call_fun,0}.
    {move,{y,2},{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,3}}.
    {move,{y,4},{x,0}}.
    {move,{y,1},{y,4}}.
    {trim,3,2}.
    {line,[{location,"lc_tests.erl",265}]}.
    {call_fun,3}.
    {test_heap,6,1}.
    {put_list,{y,1},nil,{x,1}}.
    {put_list,{y,0},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,213}.
    {move,{x,0},{y,3}}.
    {move,{x,4},{x,0}}.
    {line,[{location,"lc_tests.erl",266}]}.
    {call_fun,0}.
    {move,{y,2},{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{y,3},{x,3}}.
    {move,{y,4},{x,0}}.
    {line,[{location,"lc_tests.erl",266}]}.
    {call_fun,3}.
    {deallocate,5}.
    return.
  {label,214}.
    {test,is_nil,{f,215},[{y,3}]}.
    {kill,{y,3}}.
    {kill,{y,4}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests.erl",267}]}.
    {call_fun,0}.
    {move,{y,2},{x,1}}.
    {line,[{location,"lc_tests.erl",267}]}.
    {call_fun,1}.
    {deallocate,5}.
    return.
  {label,215}.
    {test_heap,4,0}.
    {put_tuple,3,{x,1}}.
    {put,{y,4}}.
    {put,{y,3}}.
    {put,{y,2}}.
    {line,[{location,"lc_tests.erl",263}]}.
    {case_end,{x,1}}.


{function, '-pen2_lazy/1-anonymous-2-', 3, 217}.
  {label,216}.
    {line,[]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_lazy/1-anonymous-2-'},3}.
  {label,217}.
    {call_only,3,{f,212}}.


{function, '-pen2_mr/1-anonymous-5-', 3, 219}.
  {label,218}.
    {line,[]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_mr/1-anonymous-5-'},3}.
  {label,219}.
    {call_only,3,{f,221}}.


{function, '-pen2_mr/1-Red_xs/1-1-', 3, 221}.
  {label,220}.
    {line,[{location,"lc_tests.erl",252}]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_mr/1-Red_xs/1-1-'},3}.
  {label,221}.
    {allocate,3,3}.
    {move,{x,0},{y,2}}.
    {move,{x,1},{x,0}}.
    {move,{x,2},{x,1}}.
    {move,{x,0},{y,0}}.
    {move,{x,1},{y,1}}.
    {make_fun2,{f,226},0,0,2}.
    {test,is_nonempty_list,{f,223},[{y,2}]}.
    {get_list,{y,2},{x,1},{x,2}}.
    {gc_bif,'rem',{f,222},3,[{x,1},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,222},[{x,3},{integer,0}]}.
    {move,{x,0},{x,2}}.
    {move,{y,0},{x,1}}.
    {move,{y,1},{x,3}}.
    {move,{y,2},{x,0}}.
    {line,[{location,"lc_tests.erl",253}]}.
    {call_fun,3}.
    {deallocate,3}.
    return.
  {label,222}.
    {move,{x,0},{x,1}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests.erl",254}]}.
    {call_fun,1}.
    {deallocate,3}.
    return.
  {label,223}.
    {test,is_nil,{f,224},[{y,2}]}.
    {move,nil,{x,0}}.
    {deallocate,3}.
    return.
  {label,224}.
    {test_heap,2,0}.
    {put_tuple,1,{x,1}}.
    {put,{y,2}}.
    {line,[{location,"lc_tests.erl",252}]}.
    {case_end,{x,1}}.


{function, '-pen2_mr/1-anonymous-4-', 3, 226}.
  {label,225}.
    {line,[]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_mr/1-anonymous-4-'},3}.
  {label,226}.
    {call_only,3,{f,221}}.


{function, '-pen2_mr/1-anonymous-3-', 3, 228}.
  {label,227}.
    {line,[]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_mr/1-anonymous-3-'},3}.
  {label,228}.
    {call_only,3,{f,230}}.


{function, '-pen2_mr/1-Red_ys/3-0-', 3, 230}.
  {label,229}.
    {line,[{location,"lc_tests.erl",246}]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_mr/1-Red_ys/3-0-'},3}.
  {label,230}.
    {allocate,3,3}.
    {move,{x,1},{y,0}}.
    {move,{x,0},{y,1}}.
    {move,{x,2},{y,2}}.
    {make_fun2,{f,235},0,0,0}.
    {test,is_nonempty_list,{f,233},[{y,1}]}.
    {get_list,{y,1},{x,1},{x,2}}.
    {test,is_nonempty_list,{f,232},[{y,0}]}.
    {get_list,{y,0},{x,3},{x,4}}.
    {gc_bif,'rem',{f,231},5,[{x,3},{integer,2}],{x,2}}.
    {test,is_eq_exact,{f,231},[{x,2},{integer,1}]}.
    {gc_bif,'+',{f,231},5,[{x,1},{x,3}],{x,2}}.
    {gc_bif,'rem',{f,231},5,[{x,2},{integer,5}],{x,2}}.
    {bif,'=/=',{f,231},[{x,2},{integer,0}],{x,2}}.
    {move,{x,2},{x,5}}.
    {test,is_eq_exact,{f,231},[{x,5},{atom,true}]}.
    {move,{x,3},{x,6}}.
    {move,{x,1},{x,5}}.
    {move,{y,2},{x,2}}.
    {move,{x,4},{x,1}}.
    {move,{x,0},{x,3}}.
    {move,{y,1},{x,0}}.
    {move,{x,6},{y,1}}.
    {move,{x,5},{y,2}}.
    {trim,1,2}.
    {line,[{location,"lc_tests.erl",248}]}.
    {call_fun,3}.
    {test_heap,6,1}.
    {put_list,{y,0},nil,{x,1}}.
    {put_list,{y,1},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,231}.
    {move,{y,2},{x,2}}.
    {move,{x,4},{x,1}}.
    {move,{x,0},{x,3}}.
    {move,{y,1},{x,0}}.
    {line,[{location,"lc_tests.erl",249}]}.
    {call_fun,3}.
    {deallocate,3}.
    return.
  {label,232}.
    {test,is_nil,{f,233},[{y,0}]}.
    {move,{y,2},{x,1}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests.erl",251}]}.
    {call_fun,1}.
    {deallocate,3}.
    return.
  {label,233}.
    {test_heap,4,0}.
    {put_tuple,3,{x,1}}.
    {put,{y,1}}.
    {put,{y,0}}.
    {put,{y,2}}.
    {line,[{location,"lc_tests.erl",246}]}.
    {case_end,{x,1}}.


{function, '-pen2_mr/1-anonymous-2-', 3, 235}.
  {label,234}.
    {line,[]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_mr/1-anonymous-2-'},3}.
  {label,235}.
    {call_only,3,{f,230}}.


{function, '-pen2_listsfuns/1-fun-1-', 1, 237}.
  {label,236}.
    {line,[{location,"lc_tests.erl",240}]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_listsfuns/1-fun-1-'},1}.
  {label,237}.
    {line,[{location,"lc_tests.erl",240}]}.
    {gc_bif,'rem',{f,0},1,[{x,0},{integer,2}],{x,0}}.
    {bif,'=:=',{f,0},[{x,0},{integer,0}],{x,0}}.
    return.


{function, '-pen2_listsfuns/1-lc$^0/1-0-', 2, 239}.
  {label,238}.
    {line,[{location,"lc_tests.erl",240}]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_listsfuns/1-lc$^0/1-0-'},2}.
  {label,239}.
    {test,is_nonempty_list,{f,240},[{x,0}]}.
    {allocate,3,2}.
    {get_list,{x,0},{y,1},{y,2}}.
    {move,{x,1},{y,0}}.
    {make_fun2,{f,243},0,0,0}.
    {move,{y,0},{x,1}}.
    {line,[{location,"lc_tests.erl",241}]}.
    {call_ext,2,{extfunc,lists,filter,2}}.
    {move,{y,1},{x,2}}.
    {move,{y,2},{x,3}}.
    {move,{y,0},{x,1}}.
    {call_last,4,{f,245},3}.
  {label,240}.
    {test,is_nil,{f,241},[{x,0}]}.
    return.
  {label,241}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests.erl",240}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pen2_listsfuns/1-fun-2-', 1, 243}.
  {label,242}.
    {line,[{location,"lc_tests.erl",241}]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_listsfuns/1-fun-2-'},1}.
  {label,243}.
    {line,[{location,"lc_tests.erl",241}]}.
    {gc_bif,'rem',{f,0},1,[{x,0},{integer,2}],{x,0}}.
    {bif,'=:=',{f,0},[{x,0},{integer,1}],{x,0}}.
    return.


{function, '-pen2_listsfuns/1-lc$^1/1-1-', 4, 245}.
  {label,244}.
    {line,[{location,"lc_tests.erl",240}]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_listsfuns/1-lc$^1/1-1-'},4}.
  {label,245}.
    {test,is_nonempty_list,{f,246},[{x,0}]}.
    {allocate,2,4}.
    {get_list,{x,0},{y,0},{x,0}}.
    {move,{x,2},{y,1}}.
    {line,[{location,"lc_tests.erl",241}]}.
    {call,4,{f,245}}.
    {test_heap,6,1}.
    {put_list,{y,0},nil,{x,1}}.
    {put_list,{y,1},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,246}.
    {test,is_nil,{f,247},[{x,0}]}.
    {move,{x,3},{x,0}}.
    {call_only,2,{f,239}}.
  {label,247}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests.erl",240}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pen2_listsfuns/1-fun-0-', 1, 249}.
  {label,248}.
    {line,[{location,"lc_tests.erl",239}]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_listsfuns/1-fun-0-'},1}.
  {label,249}.
    {test,is_nonempty_list,{f,248},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_nonempty_list,{f,248},[{x,2}]}.
    {get_list,{x,2},{x,3},{x,4}}.
    {test,is_nil,{f,248},[{x,4}]}.
    {line,[{location,"lc_tests.erl",239}]}.
    {gc_bif,'+',{f,0},4,[{x,1},{x,3}],{x,0}}.
    {line,[{location,"lc_tests.erl",239}]}.
    {gc_bif,'rem',{f,0},1,[{x,0},{integer,5}],{x,0}}.
    {bif,'=/=',{f,0},[{x,0},{integer,0}],{x,0}}.
    return.


{function, '-pen2_foldr/1-fun-1-', 3, 251}.
  {label,250}.
    {line,[{location,"lc_tests.erl",224}]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_foldr/1-fun-1-'},3}.
  {label,251}.
    {gc_bif,'rem',{f,252},3,[{x,0},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,252},[{x,3},{integer,0}]}.
    {allocate,2,3}.
    {move,{x,2},{y,0}}.
    {move,{x,1},{y,1}}.
    {make_fun2,{f,254},0,0,1}.
    {move,{y,0},{x,2}}.
    {move,nil,{x,1}}.
    {trim,1,1}.
    {line,[{location,"lc_tests.erl",226}]}.
    {call_ext,3,{extfunc,lists,foldr,3}}.
    {move,{y,0},{x,1}}.
    {line,[{location,"lc_tests.erl",232}]}.
    {call_ext_last,2,{extfunc,erlang,'++',2},1}.
  {label,252}.
    {move,{x,1},{x,0}}.
    return.


{function, '-pen2_foldr/1-fun-0-', 3, 254}.
  {label,253}.
    {line,[{location,"lc_tests.erl",227}]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_foldr/1-fun-0-'},3}.
  {label,254}.
    {gc_bif,'rem',{f,255},3,[{x,0},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,255},[{x,3},{integer,1}]}.
    {gc_bif,'+',{f,255},3,[{x,2},{x,0}],{x,3}}.
    {gc_bif,'rem',{f,255},4,[{x,3},{integer,5}],{x,3}}.
    {test,is_ne_exact,{f,255},[{x,3},{integer,0}]}.
    {test_heap,6,3}.
    {put_list,{x,0},nil,{x,0}}.
    {put_list,{x,2},{x,0},{x,2}}.
    {put_list,{x,2},{x,1},{x,0}}.
    return.
  {label,255}.
    {move,{x,1},{x,0}}.
    return.


{function, '-pen2_foldl/1-fun-1-', 3, 257}.
  {label,256}.
    {line,[{location,"lc_tests.erl",208}]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_foldl/1-fun-1-'},3}.
  {label,257}.
    {gc_bif,'rem',{f,258},3,[{x,0},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,258},[{x,3},{integer,0}]}.
    {allocate,2,3}.
    {move,{x,2},{y,0}}.
    {move,{x,1},{y,1}}.
    {make_fun2,{f,260},0,0,1}.
    {move,{y,0},{x,2}}.
    {move,nil,{x,1}}.
    {trim,1,1}.
    {line,[{location,"lc_tests.erl",210}]}.
    {call_ext,3,{extfunc,lists,foldl,3}}.
    {move,{y,0},{x,1}}.
    {line,[{location,"lc_tests.erl",216}]}.
    {call_ext_last,2,{extfunc,erlang,'++',2},1}.
  {label,258}.
    {move,{x,1},{x,0}}.
    return.


{function, '-pen2_foldl/1-fun-0-', 3, 260}.
  {label,259}.
    {line,[{location,"lc_tests.erl",211}]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_foldl/1-fun-0-'},3}.
  {label,260}.
    {gc_bif,'rem',{f,261},3,[{x,0},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,261},[{x,3},{integer,1}]}.
    {gc_bif,'+',{f,261},3,[{x,2},{x,0}],{x,3}}.
    {gc_bif,'rem',{f,261},4,[{x,3},{integer,5}],{x,3}}.
    {test,is_ne_exact,{f,261},[{x,3},{integer,0}]}.
    {test_heap,6,3}.
    {put_list,{x,0},nil,{x,0}}.
    {put_list,{x,2},{x,0},{x,2}}.
    {put_list,{x,2},{x,1},{x,0}}.
    return.
  {label,261}.
    {move,{x,1},{x,0}}.
    return.


{function, '-pen2/1-lc$^0/1-0-', 2, 263}.
  {label,262}.
    {line,[{location,"lc_tests.erl",169}]}.
    {func_info,{atom,lc_tests},{atom,'-pen2/1-lc$^0/1-0-'},2}.
  {label,263}.
    {test,is_nonempty_list,{f,265},[{x,0}]}.
    {get_list,{x,0},{x,2},{x,3}}.
    {gc_bif,'rem',{f,264},4,[{x,2},{integer,2}],{x,4}}.
    {test,is_eq_exact,{f,264},[{x,4},{integer,0}]}.
    {allocate,3,4}.
    {move,{integer,1},{x,0}}.
    {move,{x,1},{y,0}}.
    {move,{x,3},{y,1}}.
    {move,{x,2},{y,2}}.
    {line,[{location,"lc_tests.erl",171}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,1},{x,2}}.
    {move,{y,2},{x,3}}.
    {move,{y,0},{x,1}}.
    {call_last,4,{f,268},3}.
  {label,264}.
    {move,{x,3},{x,0}}.
    {call_only,2,{f,263}}.
  {label,265}.
    {test,is_nil,{f,266},[{x,0}]}.
    return.
  {label,266}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests.erl",169}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pen2/1-lc$^1/1-1-', 4, 268}.
  {label,267}.
    {line,[{location,"lc_tests.erl",169}]}.
    {func_info,{atom,lc_tests},{atom,'-pen2/1-lc$^1/1-1-'},4}.
  {label,268}.
    {test,is_nonempty_list,{f,270},[{x,0}]}.
    {get_list,{x,0},{x,4},{x,5}}.
    {gc_bif,'rem',{f,269},6,[{x,4},{integer,2}],{x,6}}.
    {gc_bif,'+',{f,269},7,[{x,3},{x,4}],{x,7}}.
    {gc_bif,'rem',{f,269},8,[{x,7},{integer,5}],{x,8}}.
    {test,is_eq_exact,{f,269},[{x,6},{integer,1}]}.
    {test,is_ne_exact,{f,269},[{x,8},{integer,0}]}.
    {allocate,2,6}.
    {move,{x,5},{x,0}}.
    {move,{x,4},{y,0}}.
    {move,{x,3},{y,1}}.
    {line,[{location,"lc_tests.erl",171}]}.
    {call,4,{f,268}}.
    {test_heap,6,1}.
    {put_list,{y,0},nil,{x,1}}.
    {put_list,{y,1},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,269}.
    {move,{x,5},{x,0}}.
    {call_only,4,{f,268}}.
  {label,270}.
    {test,is_nil,{f,271},[{x,0}]}.
    {move,{x,2},{x,0}}.
    {call_only,2,{f,263}}.
  {label,271}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests.erl",169}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pen/1-lc$^0/1-0-', 2, 273}.
  {label,272}.
    {line,[{location,"lc_tests.erl",162}]}.
    {func_info,{atom,lc_tests},{atom,'-pen/1-lc$^0/1-0-'},2}.
  {label,273}.
    {test,is_nonempty_list,{f,275},[{x,0}]}.
    {get_list,{x,0},{x,2},{x,3}}.
    {gc_bif,'rem',{f,274},4,[{x,2},{integer,2}],{x,4}}.
    {test,is_eq_exact,{f,274},[{x,4},{integer,0}]}.
    {allocate,3,4}.
    {move,{integer,1},{x,0}}.
    {move,{x,1},{y,0}}.
    {move,{x,3},{y,1}}.
    {move,{x,2},{y,2}}.
    {line,[{location,"lc_tests.erl",164}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,1},{x,2}}.
    {move,{y,2},{x,3}}.
    {move,{y,0},{x,1}}.
    {call_last,4,{f,278},3}.
  {label,274}.
    {move,{x,3},{x,0}}.
    {call_only,2,{f,273}}.
  {label,275}.
    {test,is_nil,{f,276},[{x,0}]}.
    return.
  {label,276}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests.erl",162}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pen/1-lc$^1/1-1-', 4, 278}.
  {label,277}.
    {line,[{location,"lc_tests.erl",162}]}.
    {func_info,{atom,lc_tests},{atom,'-pen/1-lc$^1/1-1-'},4}.
  {label,278}.
    {test,is_nonempty_list,{f,280},[{x,0}]}.
    {get_list,{x,0},{x,4},{x,5}}.
    {gc_bif,'rem',{f,279},6,[{x,4},{integer,2}],{x,6}}.
    {test,is_eq_exact,{f,279},[{x,6},{integer,1}]}.
    {allocate,2,6}.
    {move,{x,5},{x,0}}.
    {move,{x,4},{y,0}}.
    {move,{x,3},{y,1}}.
    {line,[{location,"lc_tests.erl",164}]}.
    {call,4,{f,278}}.
    {test_heap,6,1}.
    {put_list,{y,0},nil,{x,1}}.
    {put_list,{y,1},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,279}.
    {move,{x,5},{x,0}}.
    {call_only,4,{f,278}}.
  {label,280}.
    {test,is_nil,{f,281},[{x,0}]}.
    {move,{x,2},{x,0}}.
    {call_only,2,{f,273}}.
  {label,281}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests.erl",162}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-ses/1-lc$^0/1-0-', 1, 283}.
  {label,282}.
    {line,[{location,"lc_tests.erl",157}]}.
    {func_info,{atom,lc_tests},{atom,'-ses/1-lc$^0/1-0-'},1}.
  {label,283}.
    {test,is_nonempty_list,{f,285},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {gc_bif,'rem',{f,284},3,[{x,1},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,284},[{x,3},{integer,0}]}.
    {allocate_zero,1,3}.
    {line,[{location,"lc_tests.erl",157}]}.
    {gc_bif,'*',{f,0},3,[{x,1},{x,1}],{y,0}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests.erl",157}]}.
    {call,1,{f,283}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,284}.
    {move,{x,2},{x,0}}.
    {call_only,1,{f,283}}.
  {label,285}.
    {test,is_nil,{f,282},[{x,0}]}.
    return.


{function, '-ss/1-lc$^0/1-0-', 1, 287}.
  {label,286}.
    {line,[{location,"lc_tests.erl",153}]}.
    {func_info,{atom,lc_tests},{atom,'-ss/1-lc$^0/1-0-'},1}.
  {label,287}.
    {test,is_nonempty_list,{f,288},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {line,[{location,"lc_tests.erl",153}]}.
    {gc_bif,'*',{f,0},3,[{x,1},{x,1}],{y,0}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests.erl",153}]}.
    {call,1,{f,287}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,288}.
    {test,is_nil,{f,286},[{x,0}]}.
    return.


{function, '-all_tests/0-fun-22-', 0, 290}.
  {label,289}.
    {line,[{location,"lc_tests.erl",146}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-22-'},0}.
  {label,290}.
    {call_only,0,{f,96}}.


{function, '-all_tests/0-fun-21-', 1, 292}.
  {label,291}.
    {line,[{location,"lc_tests.erl",145}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-21-'},1}.
  {label,292}.
    {call_only,1,{f,116}}.


{function, '-all_tests/0-fun-20-', 1, 294}.
  {label,293}.
    {line,[{location,"lc_tests.erl",144}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-20-'},1}.
  {label,294}.
    {call_only,1,{f,114}}.


{function, '-all_tests/0-fun-19-', 1, 296}.
  {label,295}.
    {line,[{location,"lc_tests.erl",143}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-19-'},1}.
  {label,296}.
    {call_only,1,{f,112}}.


{function, '-all_tests/0-fun-18-', 1, 298}.
  {label,297}.
    {line,[{location,"lc_tests.erl",142}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-18-'},1}.
  {label,298}.
    {call_only,1,{f,110}}.


{function, '-all_tests/0-fun-17-', 1, 300}.
  {label,299}.
    {line,[{location,"lc_tests.erl",141}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-17-'},1}.
  {label,300}.
    {call_only,1,{f,108}}.


{function, '-all_tests/0-fun-16-', 2, 302}.
  {label,301}.
    {line,[{location,"lc_tests.erl",140}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-16-'},2}.
  {label,302}.
    {call_only,2,{f,106}}.


{function, '-all_tests/0-fun-15-', 1, 304}.
  {label,303}.
    {line,[{location,"lc_tests.erl",139}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-15-'},1}.
  {label,304}.
    {call_only,1,{f,104}}.


{function, '-all_tests/0-fun-14-', 2, 306}.
  {label,305}.
    {line,[{location,"lc_tests.erl",138}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-14-'},2}.
  {label,306}.
    {call_only,2,{f,102}}.


{function, '-all_tests/0-fun-13-', 1, 308}.
  {label,307}.
    {line,[{location,"lc_tests.erl",137}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-13-'},1}.
  {label,308}.
    {call_only,1,{f,100}}.


{function, '-all_tests/0-fun-12-', 1, 310}.
  {label,309}.
    {line,[{location,"lc_tests.erl",136}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-12-'},1}.
  {label,310}.
    {call_only,1,{f,98}}.


{function, '-all_tests/0-fun-11-', 1, 312}.
  {label,311}.
    {line,[{location,"lc_tests.erl",135}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-11-'},1}.
  {label,312}.
    {call_only,1,{f,94}}.


{function, '-all_tests/0-fun-10-', 1, 314}.
  {label,313}.
    {line,[{location,"lc_tests.erl",134}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-10-'},1}.
  {label,314}.
    {call_only,1,{f,92}}.


{function, '-all_tests/0-fun-9-', 1, 316}.
  {label,315}.
    {line,[{location,"lc_tests.erl",133}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-9-'},1}.
  {label,316}.
    {call_only,1,{f,90}}.


{function, '-all_tests/0-fun-8-', 1, 318}.
  {label,317}.
    {line,[{location,"lc_tests.erl",132}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-8-'},1}.
  {label,318}.
    {call_only,1,{f,88}}.


{function, '-all_tests/0-fun-7-', 1, 320}.
  {label,319}.
    {line,[{location,"lc_tests.erl",131}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-7-'},1}.
  {label,320}.
    {call_only,1,{f,84}}.


{function, '-all_tests/0-fun-6-', 1, 322}.
  {label,321}.
    {line,[{location,"lc_tests.erl",130}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-6-'},1}.
  {label,322}.
    {call_only,1,{f,86}}.


{function, '-all_tests/0-fun-5-', 1, 324}.
  {label,323}.
    {line,[{location,"lc_tests.erl",129}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-5-'},1}.
  {label,324}.
    {call_only,1,{f,76}}.


{function, '-all_tests/0-fun-4-', 1, 326}.
  {label,325}.
    {line,[{location,"lc_tests.erl",128}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-4-'},1}.
  {label,326}.
    {call_only,1,{f,69}}.


{function, '-all_tests/0-fun-3-', 1, 328}.
  {label,327}.
    {line,[{location,"lc_tests.erl",127}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-3-'},1}.
  {label,328}.
    {call_only,1,{f,67}}.


{function, '-all_tests/0-fun-2-', 1, 330}.
  {label,329}.
    {line,[{location,"lc_tests.erl",126}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-2-'},1}.
  {label,330}.
    {call_only,1,{f,65}}.


{function, '-all_tests/0-fun-1-', 1, 332}.
  {label,331}.
    {line,[{location,"lc_tests.erl",125}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-1-'},1}.
  {label,332}.
    {call_only,1,{f,63}}.


{function, '-all_tests/0-fun-0-', 1, 334}.
  {label,333}.
    {line,[{location,"lc_tests.erl",124}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-0-'},1}.
  {label,334}.
    {call_only,1,{f,61}}.


{function, '-run_tests/1-fun-0-', 0, 336}.
  {label,335}.
    {line,[{location,"lc_tests.erl",84}]}.
    {func_info,{atom,lc_tests},{atom,'-run_tests/1-fun-0-'},0}.
  {label,336}.
    {call_only,0,{f,40}}.


{function, '-run_tests/3-lc$^0/1-0-', 2, 338}.
  {label,337}.
    {line,[{location,"lc_tests.erl",76}]}.
    {func_info,{atom,lc_tests},{atom,'-run_tests/3-lc$^0/1-0-'},2}.
  {label,338}.
    {test,is_nonempty_list,{f,340},[{x,0}]}.
    {get_list,{x,0},{x,2},{x,3}}.
    {test,is_tuple,{f,339},[{x,2}]}.
    {test,test_arity,{f,339},[{x,2},4]}.
    {get_tuple_element,{x,2},0,{x,4}}.
    {get_tuple_element,{x,2},1,{x,5}}.
    {get_tuple_element,{x,2},2,{x,6}}.
    {get_tuple_element,{x,2},3,{x,7}}.
    {test,is_tuple,{f,339},[{x,6}]}.
    {test,test_arity,{f,339},[{x,6},2]}.
    {allocate,6,8}.
    {get_tuple_element,{x,6},0,{y,3}}.
    {get_tuple_element,{x,6},1,{y,2}}.
    {move,{integer,0},{x,2}}.
    {move,{x,5},{x,0}}.
    {move,{x,1},{y,0}}.
    {move,{x,3},{y,1}}.
    {move,{x,7},{y,4}}.
    {move,{x,4},{y,5}}.
    {line,[{location,"lc_tests.erl",76}]}.
    {call_ext,3,{extfunc,maps,get,3}}.
    {move,{x,0},{x,2}}.
    {move,{y,0},{x,1}}.
    {move,{y,1},{x,0}}.
    {move,{x,2},{y,1}}.
    {trim,1,5}.
    {line,[{location,"lc_tests.erl",77}]}.
    {call,2,{f,338}}.
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
  {label,339}.
    {move,{x,3},{x,0}}.
    {call_only,2,{f,338}}.
  {label,340}.
    {test,is_nil,{f,341},[{x,0}]}.
    return.
  {label,341}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests.erl",76}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-test/3-fun-0-', 3, 343}.
  {label,342}.
    {line,[{location,"lc_tests.erl",58}]}.
    {func_info,{atom,lc_tests},{atom,'-test/3-fun-0-'},3}.
  {label,343}.
    {allocate_zero,4,3}.
    {bif,self,{f,0},[],{y,0}}.
    {move,{x,1},{y,1}}.
    {move,{x,2},{y,2}}.
    {move,{x,0},{y,3}}.
    {line,[{location,"lc_tests.erl",59}]}.
    {call,0,{f,25}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {kill,{y,0}}.
    {line,[{location,"lc_tests.erl",59}]}.
    {call_ext,2,{extfunc,erlang,process_info,2}}.
    {move,{x,0},{x,2}}.
    {move,{y,1},{x,1}}.
    {move,{y,2},{x,0}}.
    {move,{x,2},{y,2}}.
    {kill,{y,1}}.
    {line,[{location,"lc_tests.erl",60}]}.
    {call_ext,2,{extfunc,erlang,apply,2}}.
    {bif,self,{f,0},[],{y,0}}.
    {move,{x,0},{y,1}}.
    {line,[{location,"lc_tests.erl",62}]}.
    {call,0,{f,25}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {trim,1,3}.
    {line,[{location,"lc_tests.erl",62}]}.
    {call_ext,2,{extfunc,erlang,process_info,2}}.
    {test_heap,4,1}.
    {put_tuple,3,{x,1}}.
    {put,{y,1}}.
    {put,{x,0}}.
    {put,{y,0}}.
    {move,{y,2},{x,0}}.
    {line,[{location,"lc_tests.erl",63}]}.
    send.
    {deallocate,3}.
    return.


{function, '-lazy_cp_seq/3-fun-1-', 3, 345}.
  {label,344}.
    {line,[{location,"lc_tests.erl",46}]}.
    {func_info,{atom,lc_tests},{atom,'-lazy_cp_seq/3-fun-1-'},3}.
  {label,345}.
    {allocate_zero,2,3}.
    {line,[{location,"lc_tests.erl",46}]}.
    {gc_bif,'+',{f,0},3,[{x,2},{integer,1}],{y,1}}.
    {move,{x,1},{y,0}}.
    {move,{x,2},{x,1}}.
    {make_fun2,{f,347},0,0,2}.
    {move,{y,0},{x,1}}.
    {move,{x,0},{x,2}}.
    {move,{y,1},{x,0}}.
    {call_last,3,{f,22},2}.


{function, '-lazy_cp_seq/3-fun-0-', 3, 347}.
  {label,346}.
    {line,[{location,"lc_tests.erl",46}]}.
    {func_info,{atom,lc_tests},{atom,'-lazy_cp_seq/3-fun-0-'},3}.
  {label,347}.
    {move,{x,2},{x,3}}.
    {move,{x,1},{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{x,3},{x,0}}.
    {call_only,3,{f,18}}.


{function, '-lazy_cp_seq/2-fun-0-', 1, 349}.
  {label,348}.
    {line,[{location,"lc_tests.erl",43}]}.
    {func_info,{atom,lc_tests},{atom,'-lazy_cp_seq/2-fun-0-'},1}.
  {label,349}.
    return.


{function, '-lazy_seq/2-fun-0-', 2, 351}.
  {label,350}.
    {line,[{location,"lc_tests.erl",32}]}.
    {func_info,{atom,lc_tests},{atom,'-lazy_seq/2-fun-0-'},2}.
  {label,351}.
    {line,[{location,"lc_tests.erl",32}]}.
    {gc_bif,'+',{f,0},2,[{x,1},{integer,1}],{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{x,2},{x,0}}.
    {call_only,2,{f,12}}.


{function, '-random_bin/1-lc$^0/1-0-', 1, 353}.
  {label,352}.
    {line,[{location,"lc_tests.erl",18}]}.
    {func_info,{atom,lc_tests},{atom,'-random_bin/1-lc$^0/1-0-'},1}.
  {label,353}.
    {test,is_nonempty_list,{f,354},[{x,0}]}.
    {allocate,1,1}.
    {get_list,{x,0},{x,1},{y,0}}.
    {move,{integer,255},{x,0}}.
    {line,[{location,"lc_tests.erl",18}]}.
    {call_ext,1,{extfunc,random,uniform,1}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests.erl",18}]}.
    {call,1,{f,353}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,354}.
    {test,is_nil,{f,352},[{x,0}]}.
    return.
