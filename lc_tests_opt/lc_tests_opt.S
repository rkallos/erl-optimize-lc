{module, lc_tests_opt}.  %% version = 0

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
           {pyth,1},
           {pyth1,1},
           {pyth2,0},
           {pyth3,1},
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

{labels, 422}.


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
    {call,1,{f,420}}.
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
    {make_fun2,{f,418},0,0,2}.
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


{function, cps_cons, 3, 18}.
  {label,17}.
    {line,[{location,"lc_tests_opt.erl",39}]}.
    {func_info,{atom,lc_tests_opt},{atom,cps_cons},3}.
  {label,18}.
    {allocate_heap,0,2,3}.
    {put_list,{x,0},{x,1},{x,0}}.
    {move,{x,2},{x,1}}.
    {line,[{location,"lc_tests_opt.erl",40}]}.
    {call_fun,1}.
    {deallocate,0}.
    return.


{function, lazy_cp_seq, 2, 20}.
  {label,19}.
    {line,[{location,"lc_tests_opt.erl",42}]}.
    {func_info,{atom,lc_tests_opt},{atom,lazy_cp_seq},2}.
  {label,20}.
    {allocate,2,2}.
    {move,{x,1},{y,0}}.
    {move,{x,0},{y,1}}.
    {make_fun2,{f,416},0,0,0}.
    {move,{y,0},{x,1}}.
    {move,{x,0},{x,2}}.
    {move,{y,1},{x,0}}.
    {call_last,3,{f,22},2}.


{function, lazy_cp_seq, 3, 22}.
  {label,21}.
    {line,[{location,"lc_tests_opt.erl",44}]}.
    {func_info,{atom,lc_tests_opt},{atom,lazy_cp_seq},3}.
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
    {make_fun2,{f,412},0,0,3}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.


{function, stats, 0, 25}.
  {label,24}.
    {line,[{location,"lc_tests_opt.erl",52}]}.
    {func_info,{atom,lc_tests_opt},{atom,stats},0}.
  {label,25}.
    {move,{literal,[memory,heap_size,total_heap_size,stack_size,
                    garbage_collection]},
          {x,0}}.
    return.


{function, test, 3, 27}.
  {label,26}.
    {line,[{location,"lc_tests_opt.erl",54}]}.
    {func_info,{atom,lc_tests_opt},{atom,test},3}.
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
    {make_fun2,{f,410},0,0,3}.
    {line,[{location,"lc_tests_opt.erl",57}]}.
    {call_ext,1,{extfunc,erlang,spawn,1}}.
    {move,{y,0},{x,1}}.
    {move,{x,0},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",65}]}.
    send.
    {line,[{location,"lc_tests_opt.erl",66}]}.
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
    {line,[{location,"lc_tests_opt.erl",72}]}.
    {func_info,{atom,lc_tests_opt},{atom,run_tests},3}.
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
    {line,[{location,"lc_tests_opt.erl",80}]}.
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
    {line,[{location,"lc_tests_opt.erl",73}]}.
    {call_ext,1,{extfunc,timer,sleep,1}}.
    {bif,self,{f,0},[],{x,1}}.
    {move,{y,0},{x,0}}.
    {kill,{y,0}}.
    {line,[{location,"lc_tests_opt.erl",74}]}.
    send.
    {line,[{location,"lc_tests_opt.erl",75}]}.
  {label,35}.
    {loop_rec,{f,36},{x,0}}.
    remove_message.
    {move,{x,0},{x,1}}.
    {move,{y,1},{x,0}}.
    {call_last,2,{f,405},2}.
  {label,36}.
    {wait,{f,35}}.


{function, run_tests, 1, 38}.
  {label,37}.
    {line,[{location,"lc_tests_opt.erl",83}]}.
    {func_info,{atom,lc_tests_opt},{atom,run_tests},1}.
  {label,38}.
    {test,is_list,{f,37},[{x,0}]}.
    {allocate_zero,2,1}.
    {move,{x,0},{y,1}}.
    {make_fun2,{f,403},0,0,0}.
    {line,[{location,"lc_tests_opt.erl",84}]}.
    {call_ext,1,{extfunc,erlang,spawn,1}}.
    {move,{x,0},{y,0}}.
    {move,{integer,100},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",85}]}.
    {call_ext,1,{extfunc,timer,sleep,1}}.
    {move,{y,0},{x,1}}.
    {move,nil,{x,2}}.
    {move,{y,1},{x,0}}.
    {call_last,3,{f,33},2}.


{function, handle_gc_evts, 0, 40}.
  {label,39}.
    {line,[{location,"lc_tests_opt.erl",88}]}.
    {func_info,{atom,lc_tests_opt},{atom,handle_gc_evts},0}.
  {label,40}.
    {allocate,0,0}.
    {move,{atom,true},{x,1}}.
    {move,{literal,[garbage_collection]},{x,2}}.
    {move,{atom,new},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",89}]}.
    {call_ext,3,{extfunc,erlang,trace,3}}.
    {line,[{location,"lc_tests_opt.erl",90}]}.
    {call_ext,0,{extfunc,maps,new,0}}.
    {call_last,1,{f,42},0}.


{function, handle_gc_evts, 1, 42}.
  {label,41}.
    {line,[{location,"lc_tests_opt.erl",91}]}.
    {func_info,{atom,lc_tests_opt},{atom,handle_gc_evts},1}.
  {label,42}.
    {allocate_zero,2,1}.
    {move,{x,0},{y,1}}.
    {line,[{location,"lc_tests_opt.erl",92}]}.
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
    {line,[{location,"lc_tests_opt.erl",93}]}.
    {call,4,{f,47}}.
    {call_last,1,{f,42},0}.
  {label,44}.
    remove_message.
    {move,{x,0},{y,0}}.
    {move,{atom,false},{x,1}}.
    {move,{literal,[garbage_collection]},{x,2}}.
    {move,{atom,new},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",95}]}.
    {call_ext,3,{extfunc,erlang,trace,3}}.
    {move,{y,1},{x,1}}.
    {move,{y,0},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",96}]}.
    send.
    {deallocate,2}.
    return.
  {label,45}.
    {wait,{f,43}}.


{function, handle_gc, 4, 47}.
  {label,46}.
    {line,[{location,"lc_tests_opt.erl",100}]}.
    {func_info,{atom,lc_tests_opt},{atom,handle_gc},4}.
  {label,47}.
    {allocate,4,4}.
    {init,{y,0}}.
    {move,{x,0},{y,3}}.
    {move,{x,1},{y,2}}.
    {move,{x,3},{x,1}}.
    {move,{atom,heap_size},{x,0}}.
    {move,{x,2},{y,1}}.
    {line,[{location,"lc_tests_opt.erl",101}]}.
    {call_ext,2,{extfunc,proplists,get_value,2}}.
    {move,{x,0},{y,0}}.
    {move,{y,3},{x,1}}.
    {move,{atom,undefined},{x,2}}.
    {move,{y,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",102}]}.
    {call_ext,3,{extfunc,maps,get,3}}.
    {test,is_eq_exact,{f,48},[{x,0},{atom,undefined}]}.
    {move,{y,0},{x,1}}.
    {move,{y,3},{x,2}}.
    {move,{y,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",103}]}.
    {call_ext_last,3,{extfunc,maps,put,3},4}.
  {label,48}.
    {test,is_atom,{f,51},[{y,1}]}.
    {select_val,{y,1},
                {f,51},
                {list,[{atom,gc_end},{f,49},{atom,gc_start},{f,50}]}}.
  {label,49}.
    {line,[{location,"lc_tests_opt.erl",106}]}.
    {gc_bif,'-',{f,0},1,[{x,0},{y,0}],{x,1}}.
    {move,{y,3},{x,2}}.
    {move,{y,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",106}]}.
    {call_ext_last,3,{extfunc,maps,put,3},4}.
  {label,50}.
    {line,[{location,"lc_tests_opt.erl",105}]}.
    {gc_bif,'+',{f,0},1,[{x,0},{y,0}],{x,1}}.
    {move,{y,3},{x,2}}.
    {move,{y,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",105}]}.
    {call_ext_last,3,{extfunc,maps,put,3},4}.
  {label,51}.
    {line,[{location,"lc_tests_opt.erl",104}]}.
    {case_end,{y,1}}.


{function, int_arg, 0, 53}.
  {label,52}.
    {line,[{location,"lc_tests_opt.erl",113}]}.
    {func_info,{atom,lc_tests_opt},{atom,int_arg},0}.
  {label,53}.
    {move,{integer,50},{x,0}}.
    return.


{function, list_arg, 0, 55}.
  {label,54}.
    {line,[{location,"lc_tests_opt.erl",115}]}.
    {func_info,{atom,lc_tests_opt},{atom,list_arg},0}.
  {label,55}.
    {move,{integer,50},{x,1}}.
    {move,{integer,50},{x,0}}.
    {call_only,2,{f,6}}.


{function, record_arg, 0, 57}.
  {label,56}.
    {line,[{location,"lc_tests_opt.erl",117}]}.
    {func_info,{atom,lc_tests_opt},{atom,record_arg},0}.
  {label,57}.
    {move,{integer,50},{x,1}}.
    {move,{integer,50},{x,0}}.
    {call_only,2,{f,9}}.


{function, all_tests, 0, 59}.
  {label,58}.
    {line,[{location,"lc_tests_opt.erl",120}]}.
    {func_info,{atom,lc_tests_opt},{atom,all_tests},0}.
  {label,59}.
    {allocate_zero,32,0}.
    {move,{integer,50},{x,1}}.
    {move,{integer,500},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",122}]}.
    {call,2,{f,6}}.
    {move,{x,0},{y,31}}.
    {move,{integer,50},{x,1}}.
    {move,{integer,500},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",123}]}.
    {call,2,{f,9}}.
    {move,{x,0},{y,30}}.
    {make_fun2,{f,401},0,0,0}.
    {move,{x,0},{y,29}}.
    {make_fun2,{f,399},0,0,0}.
    {move,{x,0},{y,28}}.
    {make_fun2,{f,397},0,0,0}.
    {move,{x,0},{y,27}}.
    {make_fun2,{f,395},0,0,0}.
    {move,{x,0},{y,26}}.
    {make_fun2,{f,393},0,0,0}.
    {move,{x,0},{y,25}}.
    {make_fun2,{f,391},0,0,0}.
    {move,{x,0},{y,24}}.
    {make_fun2,{f,389},0,0,0}.
    {move,{x,0},{y,23}}.
    {make_fun2,{f,387},0,0,0}.
    {move,{x,0},{y,22}}.
    {make_fun2,{f,385},0,0,0}.
    {move,{x,0},{y,21}}.
    {make_fun2,{f,383},0,0,0}.
    {move,{x,0},{y,20}}.
    {make_fun2,{f,381},0,0,0}.
    {move,{x,0},{y,19}}.
    {make_fun2,{f,379},0,0,0}.
    {move,{x,0},{y,18}}.
    {make_fun2,{f,377},0,0,0}.
    {move,{x,0},{y,17}}.
    {make_fun2,{f,375},0,0,0}.
    {move,{x,0},{y,16}}.
    {make_fun2,{f,373},0,0,0}.
    {move,{x,0},{y,15}}.
    {make_fun2,{f,371},0,0,0}.
    {move,{x,0},{y,14}}.
    {make_fun2,{f,369},0,0,0}.
    {test_heap,2,1}.
    {put_list,{y,31},nil,{y,12}}.
    {move,{x,0},{y,13}}.
    {make_fun2,{f,367},0,0,0}.
    {test_heap,2,1}.
    {put_list,{y,30},nil,{y,10}}.
    {move,{x,0},{y,11}}.
    {make_fun2,{f,365},0,0,0}.
    {test_heap,4,1}.
    {put_list,{y,31},nil,{x,1}}.
    {put_list,{atom,param},{x,1},{y,8}}.
    {move,{x,0},{y,9}}.
    {make_fun2,{f,363},0,0,0}.
    {test_heap,2,1}.
    {put_list,{y,30},nil,{y,7}}.
    {move,{x,0},{y,30}}.
    {make_fun2,{f,361},0,0,0}.
    {move,{x,0},{y,6}}.
    {make_fun2,{f,359},0,0,0}.
    {move,{x,0},{y,5}}.
    {make_fun2,{f,357},0,0,0}.
    {move,{x,0},{y,4}}.
    {make_fun2,{f,355},0,0,0}.
    {move,{x,0},{y,3}}.
    {make_fun2,{f,353},0,0,0}.
    {test_heap,2,1}.
    {put_list,{y,31},nil,{y,1}}.
    {move,{x,0},{y,2}}.
    {make_fun2,{f,351},0,0,0}.
    {test_heap,2,1}.
    {put_list,{y,31},nil,{y,0}}.
    {move,{x,0},{y,31}}.
    {make_fun2,{f,349},0,0,0}.
    {test_heap,162,1}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"unused"}}.
    {put,{x,0}}.
    {put,nil}.
    {put_list,{x,1},nil,{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"nested_lc_gen_opt"}}.
    {put,{y,31}}.
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
    {put,{y,30}}.
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
    {put,{literal,"pyth3"}}.
    {put,{y,24}}.
    {put,{literal,"}"}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"pyth2"}}.
    {put,{y,25}}.
    {put,nil}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"pyth1"}}.
    {put,{y,26}}.
    {put,{literal,"}"}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"pyth"}}.
    {put,{y,27}}.
    {put,{literal,"}"}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"ses"}}.
    {put,{y,28}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,1}}.
    {put_tuple,3,{x,2}}.
    {put,{literal,"ss"}}.
    {put,{y,29}}.
    {put,{literal,[500]}}.
    {put_list,{x,2},{x,1},{x,0}}.
    {deallocate,32}.
    return.


{function, ss, 1, 61}.
  {label,60}.
    {line,[{location,"lc_tests_opt.erl",156}]}.
    {func_info,{atom,lc_tests_opt},{atom,ss},1}.
  {label,61}.
    {test,is_integer,{f,60},[{x,0}]}.
    {allocate,0,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",157}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {line,[{location,"lc_tests_opt.erl",157}]}.
    {call,1,{f,346}}.
    {line,[{location,"lc_tests_opt.erl",157}]}.
    {call_ext_last,1,{extfunc,lists,sum,1},0}.


{function, ses, 1, 63}.
  {label,62}.
    {line,[{location,"lc_tests_opt.erl",160}]}.
    {func_info,{atom,lc_tests_opt},{atom,ses},1}.
  {label,63}.
    {test,is_integer,{f,62},[{x,0}]}.
    {allocate,0,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",161}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {line,[{location,"lc_tests_opt.erl",161}]}.
    {call,1,{f,342}}.
    {line,[{location,"lc_tests_opt.erl",161}]}.
    {call_ext_last,1,{extfunc,lists,sum,1},0}.


{function, pen, 1, 65}.
  {label,64}.
    {line,[{location,"lc_tests_opt.erl",165}]}.
    {func_info,{atom,lc_tests_opt},{atom,pen},1}.
  {label,65}.
    {test,is_integer,{f,64},[{x,0}]}.
    {allocate,1,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",166}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,0},{x,1}}.
    {call_last,2,{f,332},1}.


{function, pen2, 1, 67}.
  {label,66}.
    {line,[{location,"lc_tests_opt.erl",172}]}.
    {func_info,{atom,lc_tests_opt},{atom,pen2},1}.
  {label,67}.
    {test,is_integer,{f,66},[{x,0}]}.
    {allocate,1,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",173}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,0},{x,1}}.
    {call_last,2,{f,322},1}.


{function, pen2_dumb, 1, 69}.
  {label,68}.
    {line,[{location,"lc_tests_opt.erl",180}]}.
    {func_info,{atom,lc_tests_opt},{atom,pen2_dumb},1}.
  {label,69}.
    {move,{integer,1},{x,2}}.
    {move,nil,{x,3}}.
    {move,{integer,1},{x,1}}.
    {call_only,4,{f,71}}.


{function, pen2_dumb_helper, 4, 71}.
  {label,70}.
    {line,[{location,"lc_tests_opt.erl",182}]}.
    {func_info,{atom,lc_tests_opt},{atom,pen2_dumb_helper},4}.
  {label,71}.
    {test,is_lt,{f,72},[{x,0},{x,1}]}.
    {move,{x,3},{x,0}}.
    return.
  {label,72}.
    {test,is_lt,{f,73},[{x,0},{x,2}]}.
    {line,[{location,"lc_tests_opt.erl",185}]}.
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
    {line,[{location,"lc_tests_opt.erl",187}]}.
    {gc_bif,'+',{f,0},4,[{x,2},{integer,1}],{x,4}}.
    {move,{x,2},{y,0}}.
    {move,{x,4},{x,2}}.
    {move,{x,1},{y,1}}.
    {line,[{location,"lc_tests_opt.erl",187}]}.
    {call,4,{f,71}}.
    {test_heap,6,1}.
    {put_list,{y,0},nil,{x,1}}.
    {put_list,{y,1},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,74}.
    {line,[{location,"lc_tests_opt.erl",189}]}.
    {gc_bif,'+',{f,0},4,[{x,2},{integer,1}],{x,2}}.
    {call_only,4,{f,71}}.


{function, pen2_dumb_opt, 1, 76}.
  {label,75}.
    {line,[{location,"lc_tests_opt.erl",193}]}.
    {func_info,{atom,lc_tests_opt},{atom,pen2_dumb_opt},1}.
  {label,76}.
    {allocate,0,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",194}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {line,[{location,"lc_tests_opt.erl",194}]}.
    {call_ext,1,{extfunc,lists,reverse,1}}.
    {move,{x,0},{x,3}}.
    {move,{x,0},{x,2}}.
    {move,nil,{x,4}}.
    {move,{x,0},{x,1}}.
    {call_last,5,{f,78},0}.


{function, pen2_dumb_opt_helper, 5, 78}.
  {label,77}.
    {line,[{location,"lc_tests_opt.erl",196}]}.
    {func_info,{atom,lc_tests_opt},{atom,pen2_dumb_opt_helper},5}.
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
    {line,[{location,"lc_tests_opt.erl",209}]}.
    {func_info,{atom,lc_tests_opt},{atom,pen2_foldl},1}.
  {label,84}.
    {test,is_integer,{f,83},[{x,0}]}.
    {allocate_zero,1,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",210}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,316},0,0,1}.
    {move,{y,0},{x,2}}.
    {move,nil,{x,1}}.
    {trim,1,0}.
    {line,[{location,"lc_tests_opt.erl",211}]}.
    {call_ext,3,{extfunc,lists,foldl,3}}.
    {line,[{location,"lc_tests_opt.erl",211}]}.
    {call_ext_last,1,{extfunc,lists,reverse,1},0}.


{function, pen2_foldr, 1, 86}.
  {label,85}.
    {line,[{location,"lc_tests_opt.erl",225}]}.
    {func_info,{atom,lc_tests_opt},{atom,pen2_foldr},1}.
  {label,86}.
    {test,is_integer,{f,85},[{x,0}]}.
    {allocate_zero,1,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",226}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,310},0,0,1}.
    {move,{y,0},{x,2}}.
    {move,nil,{x,1}}.
    {line,[{location,"lc_tests_opt.erl",227}]}.
    {call_ext_last,3,{extfunc,lists,foldr,3},1}.


{function, pen2_listsfuns, 1, 88}.
  {label,87}.
    {line,[{location,"lc_tests_opt.erl",241}]}.
    {func_info,{atom,lc_tests_opt},{atom,pen2_listsfuns},1}.
  {label,88}.
    {test,is_integer,{f,87},[{x,0}]}.
    {allocate_zero,2,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",242}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{x,0},{y,1}}.
    {make_fun2,{f,308},0,0,0}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,296},0,0,0}.
    {move,{y,1},{x,1}}.
    {line,[{location,"lc_tests_opt.erl",244}]}.
    {call_ext,2,{extfunc,lists,filter,2}}.
    {move,{y,1},{x,1}}.
    {kill,{y,1}}.
    {line,[{location,"lc_tests_opt.erl",244}]}.
    {call,2,{f,298}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",243}]}.
    {call_ext_last,2,{extfunc,lists,filter,2},2}.


{function, pen2_mr, 1, 90}.
  {label,89}.
    {line,[{location,"lc_tests_opt.erl",248}]}.
    {func_info,{atom,lc_tests_opt},{atom,pen2_mr},1}.
  {label,90}.
    {test,is_integer,{f,89},[{x,0}]}.
    {allocate_zero,1,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",249}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,287},0,0,0}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {make_fun2,{f,278},0,0,2}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",262}]}.
    {call_fun,1}.
    {deallocate,1}.
    return.


{function, pen2_lazy, 1, 92}.
  {label,91}.
    {line,[{location,"lc_tests_opt.erl",265}]}.
    {func_info,{atom,lc_tests_opt},{atom,pen2_lazy},1}.
  {label,92}.
    {test,is_integer,{f,91},[{x,0}]}.
    {allocate_zero,1,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",266}]}.
    {call,2,{f,12}}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,269},0,0,0}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {make_fun2,{f,260},0,0,2}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",277}]}.
    {call_fun,1}.
    {deallocate,1}.
    return.


{function, pen2_lazy_cps, 1, 94}.
  {label,93}.
    {line,[{location,"lc_tests_opt.erl",280}]}.
    {func_info,{atom,lc_tests_opt},{atom,pen2_lazy_cps},1}.
  {label,94}.
    {test,is_integer,{f,93},[{x,0}]}.
    {allocate_zero,1,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",281}]}.
    {call,2,{f,20}}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,251},0,0,0}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {make_fun2,{f,242},0,0,2}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",292}]}.
    {call_fun,1}.
    {deallocate,1}.
    return.


{function, unused, 0, 96}.
  {label,95}.
    {line,[{location,"lc_tests_opt.erl",296}]}.
    {func_info,{atom,lc_tests_opt},{atom,unused},0}.
  {label,96}.
    {allocate,0,0}.
    {move,{integer,1000},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",297}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {line,[{location,"lc_tests_opt.erl",297}]}.
    {call,1,{f,239}}.
    {move,{atom,ok},{x,0}}.
    {deallocate,0}.
    return.


{function, pyth, 1, 98}.
  {label,97}.
    {line,[{location,"lc_tests_opt.erl",303}]}.
    {func_info,{atom,lc_tests_opt},{atom,pyth},1}.
  {label,98}.
    {allocate,1,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",304}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,0},{x,1}}.
    {call_last,2,{f,226},1}.


{function, pyth1, 1, 100}.
  {label,99}.
    {line,[{location,"lc_tests_opt.erl",317}]}.
    {func_info,{atom,lc_tests_opt},{atom,pyth1},1}.
  {label,100}.
    {allocate_zero,1,1}.
    {line,[{location,"lc_tests_opt.erl",318}]}.
    {gc_bif,'-',{f,0},1,[{x,0},{integer,2}],{x,1}}.
    {move,{x,0},{y,0}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",318}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,0},{x,1}}.
    {call_last,2,{f,213},1}.


{function, pyth2, 0, 102}.
  {label,101}.
    {line,[{location,"lc_tests_opt.erl",328}]}.
    {func_info,{atom,lc_tests_opt},{atom,pyth2},0}.
  {label,102}.
    {allocate,0,0}.
    {move,{integer,125},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",329}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {call_last,1,{f,201},0}.


{function, pyth3, 1, 104}.
  {label,103}.
    {line,[{location,"lc_tests_opt.erl",338}]}.
    {func_info,{atom,lc_tests_opt},{atom,pyth3},1}.
  {label,104}.
    {allocate,1,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",339}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,0},{x,2}}.
    {move,{x,0},{x,1}}.
    {call_last,3,{f,188},1}.


{function, single_filter, 1, 106}.
  {label,105}.
    {line,[{location,"lc_tests_opt.erl",353}]}.
    {func_info,{atom,lc_tests_opt},{atom,single_filter},1}.
  {label,106}.
    {test,is_list,{f,105},[{x,0}]}.
    {call_only,1,{f,184}}.


{function, record_extract, 1, 108}.
  {label,107}.
    {line,[{location,"lc_tests_opt.erl",359}]}.
    {func_info,{atom,lc_tests_opt},{atom,record_extract},1}.
  {label,108}.
    {test,is_list,{f,107},[{x,0}]}.
    {call_only,1,{f,180}}.


{function, function_param_lc, 2, 110}.
  {label,109}.
    {line,[{location,"lc_tests_opt.erl",363}]}.
    {func_info,{atom,lc_tests_opt},{atom,function_param_lc},2}.
  {label,110}.
    {test,is_list,{f,109},[{x,1}]}.
    {move,{x,1},{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{x,2},{x,0}}.
    {call_only,2,{f,176}}.


{function, sorted_gen, 1, 112}.
  {label,111}.
    {line,[{location,"lc_tests_opt.erl",367}]}.
    {func_info,{atom,lc_tests_opt},{atom,sorted_gen},1}.
  {label,112}.
    {test,is_list,{f,111},[{x,0}]}.
    {allocate,1,1}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,169},0,0,0}.
    {move,{y,0},{x,1}}.
    {trim,1,0}.
    {line,[{location,"lc_tests_opt.erl",369}]}.
    {call_ext,2,{extfunc,lists,sort,2}}.
    {call_last,1,{f,172},0}.


{function, make_n, 2, 114}.
  {label,113}.
    {line,[{location,"lc_tests_opt.erl",377}]}.
    {func_info,{atom,lc_tests_opt},{atom,make_n},2}.
  {label,114}.
    {test,is_integer,{f,113},[{x,1}]}.
    {allocate,1,2}.
    {move,{x,0},{y,0}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",378}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,0},{x,1}}.
    {call_last,2,{f,165},1}.


{function, intermediary_list, 1, 116}.
  {label,115}.
    {line,[{location,"lc_tests_opt.erl",381}]}.
    {func_info,{atom,lc_tests_opt},{atom,intermediary_list},1}.
  {label,116}.
    {test,is_integer,{f,115},[{x,0}]}.
    {allocate,0,1}.
    {move,{integer,50},{x,1}}.
    {line,[{location,"lc_tests_opt.erl",382}]}.
    {call,2,{f,9}}.
    {call_last,1,{f,161},0}.


{function, intermediary_list2, 1, 118}.
  {label,117}.
    {line,[{location,"lc_tests_opt.erl",385}]}.
    {func_info,{atom,lc_tests_opt},{atom,intermediary_list2},1}.
  {label,118}.
    {test,is_integer,{f,117},[{x,0}]}.
    {allocate,0,1}.
    {move,{integer,50},{x,1}}.
    {line,[{location,"lc_tests_opt.erl",386}]}.
    {call,2,{f,9}}.
    {line,[{location,"lc_tests_opt.erl",387}]}.
    {call,1,{f,157}}.
    {call_last,1,{f,154},0}.


{function, intermediary_list3, 1, 120}.
  {label,119}.
    {line,[{location,"lc_tests_opt.erl",390}]}.
    {func_info,{atom,lc_tests_opt},{atom,intermediary_list3},1}.
  {label,120}.
    {test,is_integer,{f,119},[{x,0}]}.
    {allocate,0,1}.
    {move,{integer,50},{x,1}}.
    {line,[{location,"lc_tests_opt.erl",391}]}.
    {call,2,{f,9}}.
    {line,[{location,"lc_tests_opt.erl",392}]}.
    {call,1,{f,151}}.
    {line,[{location,"lc_tests_opt.erl",393}]}.
    {call,1,{f,148}}.
    {call_last,1,{f,144},0}.


{function, nested_lc_gen, 1, 122}.
  {label,121}.
    {line,[{location,"lc_tests_opt.erl",400}]}.
    {func_info,{atom,lc_tests_opt},{atom,nested_lc_gen},1}.
  {label,122}.
    {test,is_list,{f,121},[{x,0}]}.
    {allocate,0,1}.
    {line,[{location,"lc_tests_opt.erl",402}]}.
    {call,1,{f,137}}.
    {call_last,1,{f,141},0}.


{function, nested_lc_gen_opt, 1, 124}.
  {label,123}.
    {line,[{location,"lc_tests_opt.erl",405}]}.
    {func_info,{atom,lc_tests_opt},{atom,nested_lc_gen_opt},1}.
  {label,124}.
    {allocate,0,1}.
    {line,[{location,"lc_tests_opt.erl",406}]}.
    {call,1,{f,133}}.
    {line,[{location,"lc_tests_opt.erl",406}]}.
    {call_ext,1,{extfunc,lists,reverse,1}}.
    {move,nil,{x,1}}.
    {call_last,2,{f,126},0}.


{function, nested_lc_gen_opt_helper, 2, 126}.
  {label,125}.
    {line,[{location,"lc_tests_opt.erl",408}]}.
    {func_info,{atom,lc_tests_opt},{atom,nested_lc_gen_opt_helper},2}.
  {label,126}.
    {test,is_nonempty_list,{f,127},[{x,0}]}.
    {get_list,{x,0},{x,2},{x,3}}.
    {line,[{location,"lc_tests_opt.erl",409}]}.
    {gc_bif,'*',{f,0},4,[{x,2},{x,2}],{x,0}}.
    {test_heap,6,4}.
    {put_tuple,3,{x,4}}.
    {put,{atom,test_record}}.
    {put,{x,2}}.
    {put,{x,0}}.
    {put_list,{x,4},{x,1},{x,1}}.
    {move,{x,3},{x,0}}.
    {call_only,2,{f,126}}.
  {label,127}.
    {test,is_nil,{f,125},[{x,0}]}.
    {move,{x,1},{x,0}}.
    return.


{function, module_info, 0, 129}.
  {label,128}.
    {line,[]}.
    {func_info,{atom,lc_tests_opt},{atom,module_info},0}.
  {label,129}.
    {move,{atom,lc_tests_opt},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 131}.
  {label,130}.
    {line,[]}.
    {func_info,{atom,lc_tests_opt},{atom,module_info},1}.
  {label,131}.
    {move,{x,0},{x,1}}.
    {move,{atom,lc_tests_opt},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.


{function, '-nested_lc_gen_opt/1-lc$^0/1-0-', 1, 133}.
  {label,132}.
    {line,[{location,"lc_tests_opt.erl",406}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-nested_lc_gen_opt/1-lc$^0/1-0-'},1}.
  {label,133}.
    {test,is_nonempty_list,{f,135},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {gc_bif,'rem',{f,134},3,[{x,1},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,134},[{x,3},{integer,1}]}.
    {allocate,1,3}.
    {move,{x,2},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",406}]}.
    {call,1,{f,133}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,134}.
    {move,{x,2},{x,0}}.
    {call_only,1,{f,133}}.
  {label,135}.
    {test,is_nil,{f,132},[{x,0}]}.
    return.


{function, '-nested_lc_gen/1-lc$^0/1-1-', 1, 137}.
  {label,136}.
    {line,[{location,"lc_tests_opt.erl",402}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-nested_lc_gen/1-lc$^0/1-1-'},1}.
  {label,137}.
    {test,is_nonempty_list,{f,139},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {gc_bif,'rem',{f,138},3,[{x,1},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,138},[{x,3},{integer,1}]}.
    {allocate,1,3}.
    {move,{x,2},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",402}]}.
    {call,1,{f,137}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,138}.
    {move,{x,2},{x,0}}.
    {call_only,1,{f,137}}.
  {label,139}.
    {test,is_nil,{f,136},[{x,0}]}.
    return.


{function, '-nested_lc_gen/1-lc$^1/1-0-', 1, 141}.
  {label,140}.
    {line,[{location,"lc_tests_opt.erl",401}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-nested_lc_gen/1-lc$^1/1-0-'},1}.
  {label,141}.
    {test,is_nonempty_list,{f,142},[{x,0}]}.
    {allocate_zero,2,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {line,[{location,"lc_tests_opt.erl",401}]}.
    {gc_bif,'*',{f,0},3,[{x,1},{x,1}],{y,0}}.
    {move,{x,2},{x,0}}.
    {move,{x,1},{y,1}}.
    {line,[{location,"lc_tests_opt.erl",402}]}.
    {call,1,{f,141}}.
    {test_heap,6,1}.
    {put_tuple,3,{x,1}}.
    {put,{atom,test_record}}.
    {put,{y,1}}.
    {put,{y,0}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,142}.
    {test,is_nil,{f,140},[{x,0}]}.
    return.


{function, '-intermediary_list3/1-lc$^2/1-2-', 1, 144}.
  {label,143}.
    {line,[{location,"lc_tests_opt.erl",394}]}.
    {func_info,{atom,lc_tests_opt},
               {atom,'-intermediary_list3/1-lc$^2/1-2-'},
               1}.
  {label,144}.
    {test,is_nonempty_list,{f,146},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_tuple,{f,145},[{x,1}]}.
    {test,test_arity,{f,145},[{x,1},2]}.
    {get_tuple_element,{x,1},0,{x,3}}.
    {get_tuple_element,{x,1},1,{x,4}}.
    {test,is_tuple,{f,145},[{x,4}]}.
    {test,test_arity,{f,145},[{x,4},2]}.
    {get_tuple_element,{x,4},0,{x,5}}.
    {get_tuple_element,{x,4},1,{x,6}}.
    {test,is_tuple,{f,145},[{x,6}]}.
    {test,test_arity,{f,145},[{x,6},3]}.
    {get_tuple_element,{x,6},0,{x,7}}.
    {get_tuple_element,{x,6},1,{x,8}}.
    {get_tuple_element,{x,6},2,{x,9}}.
    {test,is_eq_exact,{f,145},[{x,7},{atom,test_record}]}.
    {allocate_zero,1,10}.
    {line,[{location,"lc_tests_opt.erl",394}]}.
    {gc_bif,'*',{f,0},10,[{x,3},{x,5}],{x,0}}.
    {line,[{location,"lc_tests_opt.erl",394}]}.
    {gc_bif,'*',{f,0},10,[{x,0},{x,8}],{x,0}}.
    {line,[{location,"lc_tests_opt.erl",394}]}.
    {gc_bif,'*',{f,0},10,[{x,0},{x,9}],{y,0}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",394}]}.
    {call,1,{f,144}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,145}.
    {move,{x,2},{x,0}}.
    {call_only,1,{f,144}}.
  {label,146}.
    {test,is_nil,{f,143},[{x,0}]}.
    return.


{function, '-intermediary_list3/1-lc$^1/1-1-', 1, 148}.
  {label,147}.
    {line,[{location,"lc_tests_opt.erl",393}]}.
    {func_info,{atom,lc_tests_opt},
               {atom,'-intermediary_list3/1-lc$^1/1-1-'},
               1}.
  {label,148}.
    {test,is_nonempty_list,{f,149},[{x,0}]}.
    {allocate,2,1}.
    {get_list,{x,0},{y,1},{y,0}}.
    {move,{integer,100},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",393}]}.
    {call_ext,1,{extfunc,random,uniform,1}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",393}]}.
    {call,1,{f,148}}.
    {test_heap,5,1}.
    {put_tuple,2,{x,1}}.
    {put,{y,0}}.
    {put,{y,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,149}.
    {test,is_nil,{f,147},[{x,0}]}.
    return.


{function, '-intermediary_list3/1-lc$^0/1-0-', 1, 151}.
  {label,150}.
    {line,[{location,"lc_tests_opt.erl",392}]}.
    {func_info,{atom,lc_tests_opt},
               {atom,'-intermediary_list3/1-lc$^0/1-0-'},
               1}.
  {label,151}.
    {test,is_nonempty_list,{f,152},[{x,0}]}.
    {allocate,2,1}.
    {get_list,{x,0},{y,1},{y,0}}.
    {move,{integer,50},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",392}]}.
    {call_ext,1,{extfunc,random,uniform,1}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",392}]}.
    {call,1,{f,151}}.
    {test_heap,5,1}.
    {put_tuple,2,{x,1}}.
    {put,{y,0}}.
    {put,{y,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,152}.
    {test,is_nil,{f,150},[{x,0}]}.
    return.


{function, '-intermediary_list2/1-lc$^1/1-1-', 1, 154}.
  {label,153}.
    {line,[{location,"lc_tests_opt.erl",388}]}.
    {func_info,{atom,lc_tests_opt},
               {atom,'-intermediary_list2/1-lc$^1/1-1-'},
               1}.
  {label,154}.
    {test,is_nonempty_list,{f,155},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {line,[{location,"lc_tests_opt.erl",388}]}.
    {gc_bif,'*',{f,0},3,[{x,1},{x,1}],{y,0}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",388}]}.
    {call,1,{f,154}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,155}.
    {test,is_nil,{f,153},[{x,0}]}.
    return.


{function, '-intermediary_list2/1-lc$^0/1-0-', 1, 157}.
  {label,156}.
    {line,[{location,"lc_tests_opt.erl",387}]}.
    {func_info,{atom,lc_tests_opt},
               {atom,'-intermediary_list2/1-lc$^0/1-0-'},
               1}.
  {label,157}.
    {test,is_nonempty_list,{f,158},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_tuple,{f,159},[{x,1}]}.
    {test,test_arity,{f,159},[{x,1},3]}.
    {get_tuple_element,{x,1},0,{x,0}}.
    {get_tuple_element,{x,1},2,{x,3}}.
    {test,is_eq_exact,{f,159},[{x,0},{atom,test_record}]}.
    {move,{x,2},{x,0}}.
    {move,{x,3},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",387}]}.
    {call,1,{f,157}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,158}.
    {test,is_nil,{f,156},[{x,0}]}.
    return.
  {label,159}.
    {move,{literal,{badrecord,test_record}},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",387}]}.
    {call_ext,1,{extfunc,erlang,error,1}}.


{function, '-intermediary_list/1-lc$^0/1-0-', 1, 161}.
  {label,160}.
    {line,[{location,"lc_tests_opt.erl",383}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-intermediary_list/1-lc$^0/1-0-'},1}.
  {label,161}.
    {test,is_nonempty_list,{f,162},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_tuple,{f,163},[{x,1}]}.
    {test,test_arity,{f,163},[{x,1},3]}.
    {get_tuple_element,{x,1},0,{x,0}}.
    {get_tuple_element,{x,1},2,{x,3}}.
    {test,is_eq_exact,{f,163},[{x,0},{atom,test_record}]}.
    {move,{x,2},{x,0}}.
    {move,{x,3},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",383}]}.
    {call,1,{f,161}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,162}.
    {test,is_nil,{f,160},[{x,0}]}.
    return.
  {label,163}.
    {move,{literal,{badrecord,test_record}},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",383}]}.
    {call_ext,1,{extfunc,erlang,error,1}}.


{function, '-make_n/2-lc$^0/1-0-', 2, 165}.
  {label,164}.
    {line,[{location,"lc_tests_opt.erl",378}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-make_n/2-lc$^0/1-0-'},2}.
  {label,165}.
    {test,is_nonempty_list,{f,166},[{x,0}]}.
    {allocate,1,2}.
    {get_list,{x,0},{x,2},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",378}]}.
    {call,2,{f,165}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,166}.
    {test,is_nil,{f,167},[{x,0}]}.
    return.
  {label,167}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",378}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-sorted_gen/1-fun-0-', 2, 169}.
  {label,168}.
    {line,[{location,"lc_tests_opt.erl",369}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-sorted_gen/1-fun-0-'},2}.
  {label,169}.
    {test,is_tuple,{f,170},[{x,0}]}.
    {test,test_arity,{f,170},[{x,0},3]}.
    {get_tuple_element,{x,0},0,{x,2}}.
    {get_tuple_element,{x,0},1,{x,3}}.
    {test,is_eq_exact,{f,170},[{x,2},{atom,test_record}]}.
    {test,is_tuple,{f,170},[{x,1}]}.
    {test,test_arity,{f,170},[{x,1},3]}.
    {get_tuple_element,{x,1},0,{x,0}}.
    {get_tuple_element,{x,1},1,{x,2}}.
    {test,is_eq_exact,{f,170},[{x,0},{atom,test_record}]}.
    {bif,'<',{f,0},[{x,3},{x,2}],{x,0}}.
    return.
  {label,170}.
    {move,{literal,{badrecord,test_record}},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",370}]}.
    {call_ext,1,{extfunc,erlang,error,1}}.


{function, '-sorted_gen/1-lc$^0/1-0-', 1, 172}.
  {label,171}.
    {line,[{location,"lc_tests_opt.erl",368}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-sorted_gen/1-lc$^0/1-0-'},1}.
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
    {line,[{location,"lc_tests_opt.erl",369}]}.
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
    {line,[{location,"lc_tests_opt.erl",368}]}.
    {call_ext,1,{extfunc,erlang,error,1}}.


{function, '-function_param_lc/2-lc$^0/1-0-', 2, 176}.
  {label,175}.
    {line,[{location,"lc_tests_opt.erl",364}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-function_param_lc/2-lc$^0/1-0-'},2}.
  {label,176}.
    {test,is_nonempty_list,{f,177},[{x,0}]}.
    {allocate,2,2}.
    {get_list,{x,0},{y,0},{x,0}}.
    {move,{x,1},{y,1}}.
    {line,[{location,"lc_tests_opt.erl",364}]}.
    {call,2,{f,176}}.
    {test_heap,6,1}.
    {put_list,{y,0},nil,{x,1}}.
    {put_list,{y,1},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,177}.
    {test,is_nil,{f,178},[{x,0}]}.
    return.
  {label,178}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",364}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-record_extract/1-lc$^0/1-0-', 1, 180}.
  {label,179}.
    {line,[{location,"lc_tests_opt.erl",360}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-record_extract/1-lc$^0/1-0-'},1}.
  {label,180}.
    {test,is_nonempty_list,{f,181},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_tuple,{f,182},[{x,1}]}.
    {test,test_arity,{f,182},[{x,1},3]}.
    {get_tuple_element,{x,1},0,{x,0}}.
    {get_tuple_element,{x,1},1,{x,3}}.
    {test,is_eq_exact,{f,182},[{x,0},{atom,test_record}]}.
    {move,{x,2},{x,0}}.
    {move,{x,3},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",360}]}.
    {call,1,{f,180}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,181}.
    {test,is_nil,{f,179},[{x,0}]}.
    return.
  {label,182}.
    {move,{literal,{badrecord,test_record}},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",360}]}.
    {call_ext,1,{extfunc,erlang,error,1}}.


{function, '-single_filter/1-lc$^0/1-0-', 1, 184}.
  {label,183}.
    {line,[{location,"lc_tests_opt.erl",354}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-single_filter/1-lc$^0/1-0-'},1}.
  {label,184}.
    {test,is_nonempty_list,{f,186},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_integer,{f,185},[{x,1}]}.
    {allocate,1,3}.
    {move,{x,2},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",354}]}.
    {call,1,{f,184}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,185}.
    {move,{x,2},{x,0}}.
    {call_only,1,{f,184}}.
  {label,186}.
    {test,is_nil,{f,183},[{x,0}]}.
    return.


{function, '-pyth3/1-lc$^0/1-0-', 3, 188}.
  {label,187}.
    {line,[{location,"lc_tests_opt.erl",340}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pyth3/1-lc$^0/1-0-'},3}.
  {label,188}.
    {test,is_nonempty_list,{f,189},[{x,0}]}.
    {get_list,{x,0},{x,4},{x,3}}.
    {move,{x,1},{x,0}}.
    {call_only,5,{f,192}}.
  {label,189}.
    {test,is_nil,{f,190},[{x,0}]}.
    return.
  {label,190}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",340}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pyth3/1-lc$^1/1-1-', 5, 192}.
  {label,191}.
    {line,[{location,"lc_tests_opt.erl",340}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pyth3/1-lc$^1/1-1-'},5}.
  {label,192}.
    {test,is_nonempty_list,{f,193},[{x,0}]}.
    {get_list,{x,0},{x,6},{x,5}}.
    {move,{x,1},{x,0}}.
    {call_only,7,{f,196}}.
  {label,193}.
    {test,is_nil,{f,194},[{x,0}]}.
    {move,{x,3},{x,0}}.
    {call_only,3,{f,188}}.
  {label,194}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",340}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pyth3/1-lc$^2/1-2-', 7, 196}.
  {label,195}.
    {line,[{location,"lc_tests_opt.erl",340}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pyth3/1-lc$^2/1-2-'},7}.
  {label,196}.
    {test,is_nonempty_list,{f,198},[{x,0}]}.
    {get_list,{x,0},{x,7},{x,8}}.
    {gc_bif,'+',{f,197},9,[{x,4},{x,6}],{x,9}}.
    {gc_bif,'+',{f,197},10,[{x,9},{x,7}],{x,10}}.
    {gc_bif,'*',{f,197},11,[{x,4},{x,4}],{x,11}}.
    {gc_bif,'*',{f,197},12,[{x,6},{x,6}],{x,12}}.
    {gc_bif,'+',{f,197},13,[{x,11},{x,12}],{x,13}}.
    {gc_bif,'*',{f,197},14,[{x,7},{x,7}],{x,14}}.
    {test,is_ge,{f,197},[{x,6},{x,4}]}.
    {test,is_ge,{f,197},[{x,7},{x,6}]}.
    {test,is_ge,{f,197},[{x,2},{x,10}]}.
    {test,is_eq_exact,{f,197},[{x,13},{x,14}]}.
    {allocate,3,9}.
    {move,{x,8},{x,0}}.
    {move,{x,7},{y,0}}.
    {move,{x,4},{y,1}}.
    {move,{x,6},{y,2}}.
    {line,[{location,"lc_tests_opt.erl",342}]}.
    {call,7,{f,196}}.
    {test_heap,6,1}.
    {put_tuple,3,{x,1}}.
    {put,{y,1}}.
    {put,{y,2}}.
    {put,{y,0}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,3}.
    return.
  {label,197}.
    {move,{x,8},{x,0}}.
    {call_only,7,{f,196}}.
  {label,198}.
    {test,is_nil,{f,199},[{x,0}]}.
    {move,{x,5},{x,0}}.
    {call_only,5,{f,192}}.
  {label,199}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",340}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pyth2/0-lc$^0/1-0-', 1, 201}.
  {label,200}.
    {line,[{location,"lc_tests_opt.erl",329}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pyth2/0-lc$^0/1-0-'},1}.
  {label,201}.
    {test,is_nonempty_list,{f,202},[{x,0}]}.
    {allocate,2,1}.
    {get_list,{x,0},{y,1},{y,0}}.
    {move,{integer,125},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",330}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,1},{x,2}}.
    {move,{y,0},{x,1}}.
    {call_last,3,{f,204},2}.
  {label,202}.
    {test,is_nil,{f,200},[{x,0}]}.
    return.


{function, '-pyth2/0-lc$^1/1-1-', 3, 204}.
  {label,203}.
    {line,[{location,"lc_tests_opt.erl",329}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pyth2/0-lc$^1/1-1-'},3}.
  {label,204}.
    {test,is_nonempty_list,{f,205},[{x,0}]}.
    {allocate,4,3}.
    {get_list,{x,0},{y,3},{y,2}}.
    {move,{x,1},{y,0}}.
    {move,{integer,125},{x,1}}.
    {move,{integer,1},{x,0}}.
    {move,{x,2},{y,1}}.
    {line,[{location,"lc_tests_opt.erl",331}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,2},{x,3}}.
    {move,{y,1},{x,2}}.
    {move,{y,3},{x,4}}.
    {move,{y,0},{x,1}}.
    {call_last,5,{f,208},4}.
  {label,205}.
    {test,is_nil,{f,206},[{x,0}]}.
    {move,{x,1},{x,0}}.
    {call_only,1,{f,201}}.
  {label,206}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",329}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pyth2/0-lc$^2/1-2-', 5, 208}.
  {label,207}.
    {line,[{location,"lc_tests_opt.erl",329}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pyth2/0-lc$^2/1-2-'},5}.
  {label,208}.
    {test,is_nonempty_list,{f,210},[{x,0}]}.
    {get_list,{x,0},{x,5},{x,6}}.
    {gc_bif,'+',{f,209},7,[{x,2},{x,4}],{x,7}}.
    {gc_bif,'+',{f,209},8,[{x,7},{x,5}],{x,8}}.
    {gc_bif,'*',{f,209},9,[{x,2},{x,2}],{x,9}}.
    {gc_bif,'*',{f,209},10,[{x,4},{x,4}],{x,10}}.
    {gc_bif,'+',{f,209},11,[{x,9},{x,10}],{x,11}}.
    {gc_bif,'*',{f,209},12,[{x,5},{x,5}],{x,12}}.
    {test,is_ge,{f,209},[{x,4},{x,2}]}.
    {test,is_ge,{f,209},[{x,5},{x,4}]}.
    {test,is_ge,{f,209},[{integer,125},{x,8}]}.
    {test,is_eq_exact,{f,209},[{x,11},{x,12}]}.
    {allocate,3,7}.
    {move,{x,6},{x,0}}.
    {move,{x,5},{y,0}}.
    {move,{x,2},{y,1}}.
    {move,{x,4},{y,2}}.
    {line,[{location,"lc_tests_opt.erl",331}]}.
    {call,5,{f,208}}.
    {test_heap,6,1}.
    {put_tuple,3,{x,1}}.
    {put,{y,1}}.
    {put,{y,2}}.
    {put,{y,0}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,3}.
    return.
  {label,209}.
    {move,{x,6},{x,0}}.
    {call_only,5,{f,208}}.
  {label,210}.
    {test,is_nil,{f,211},[{x,0}]}.
    {move,{x,3},{x,0}}.
    {call_only,3,{f,204}}.
  {label,211}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",329}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pyth1/1-lc$^0/1-0-', 2, 213}.
  {label,212}.
    {line,[{location,"lc_tests_opt.erl",318}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pyth1/1-lc$^0/1-0-'},2}.
  {label,213}.
    {test,is_nonempty_list,{f,214},[{x,0}]}.
    {allocate_zero,3,2}.
    {get_list,{x,0},{x,2},{x,3}}.
    {line,[{location,"lc_tests_opt.erl",319}]}.
    {gc_bif,'+',{f,0},4,[{x,2},{integer,1}],{x,0}}.
    {line,[{location,"lc_tests_opt.erl",319}]}.
    {gc_bif,'-',{f,0},4,[{x,1},{integer,1}],{x,4}}.
    {move,{x,1},{y,0}}.
    {move,{x,4},{x,1}}.
    {move,{x,3},{y,1}}.
    {move,{x,2},{y,2}}.
    {line,[{location,"lc_tests_opt.erl",319}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,1},{x,2}}.
    {move,{y,2},{x,3}}.
    {move,{y,0},{x,1}}.
    {call_last,4,{f,217},3}.
  {label,214}.
    {test,is_nil,{f,215},[{x,0}]}.
    return.
  {label,215}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",318}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pyth1/1-lc$^1/1-1-', 4, 217}.
  {label,216}.
    {line,[{location,"lc_tests_opt.erl",318}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pyth1/1-lc$^1/1-1-'},4}.
  {label,217}.
    {test,is_nonempty_list,{f,218},[{x,0}]}.
    {allocate_zero,5,4}.
    {get_list,{x,0},{x,4},{x,5}}.
    {line,[{location,"lc_tests_opt.erl",320}]}.
    {gc_bif,'+',{f,0},6,[{x,4},{integer,1}],{x,0}}.
    {move,{x,1},{y,0}}.
    {move,{x,2},{y,1}}.
    {move,{x,3},{y,2}}.
    {move,{x,5},{y,3}}.
    {move,{x,4},{y,4}}.
    {line,[{location,"lc_tests_opt.erl",320}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,2},{x,3}}.
    {move,{y,3},{x,4}}.
    {move,{y,1},{x,2}}.
    {move,{y,4},{x,5}}.
    {move,{y,0},{x,1}}.
    {call_last,6,{f,221},5}.
  {label,218}.
    {test,is_nil,{f,219},[{x,0}]}.
    {move,{x,2},{x,0}}.
    {call_only,2,{f,213}}.
  {label,219}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",318}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pyth1/1-lc$^2/1-2-', 6, 221}.
  {label,220}.
    {line,[{location,"lc_tests_opt.erl",318}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pyth1/1-lc$^2/1-2-'},6}.
  {label,221}.
    {test,is_nonempty_list,{f,223},[{x,0}]}.
    {get_list,{x,0},{x,6},{x,7}}.
    {gc_bif,'+',{f,222},8,[{x,3},{x,5}],{x,8}}.
    {gc_bif,'+',{f,222},9,[{x,8},{x,6}],{x,9}}.
    {gc_bif,'*',{f,222},10,[{x,3},{x,3}],{x,10}}.
    {gc_bif,'*',{f,222},11,[{x,5},{x,5}],{x,11}}.
    {gc_bif,'+',{f,222},12,[{x,10},{x,11}],{x,12}}.
    {gc_bif,'*',{f,222},13,[{x,6},{x,6}],{x,13}}.
    {test,is_ge,{f,222},[{x,5},{x,3}]}.
    {test,is_ge,{f,222},[{x,6},{x,5}]}.
    {test,is_ge,{f,222},[{x,1},{x,9}]}.
    {test,is_eq_exact,{f,222},[{x,12},{x,13}]}.
    {allocate,3,8}.
    {move,{x,7},{x,0}}.
    {move,{x,6},{y,0}}.
    {move,{x,3},{y,1}}.
    {move,{x,5},{y,2}}.
    {line,[{location,"lc_tests_opt.erl",320}]}.
    {call,6,{f,221}}.
    {test_heap,6,1}.
    {put_tuple,3,{x,1}}.
    {put,{y,1}}.
    {put,{y,2}}.
    {put,{y,0}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,3}.
    return.
  {label,222}.
    {move,{x,7},{x,0}}.
    {call_only,6,{f,221}}.
  {label,223}.
    {test,is_nil,{f,224},[{x,0}]}.
    {move,{x,4},{x,0}}.
    {call_only,4,{f,217}}.
  {label,224}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",318}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pyth/1-lc$^0/1-0-', 2, 226}.
  {label,225}.
    {line,[{location,"lc_tests_opt.erl",304}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pyth/1-lc$^0/1-0-'},2}.
  {label,226}.
    {test,is_nonempty_list,{f,227},[{x,0}]}.
    {allocate,3,2}.
    {get_list,{x,0},{y,2},{y,1}}.
    {move,{integer,1},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",305}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,1},{x,2}}.
    {move,{y,2},{x,3}}.
    {move,{y,0},{x,1}}.
    {call_last,4,{f,230},3}.
  {label,227}.
    {test,is_nil,{f,228},[{x,0}]}.
    return.
  {label,228}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",304}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pyth/1-lc$^1/1-1-', 4, 230}.
  {label,229}.
    {line,[{location,"lc_tests_opt.erl",304}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pyth/1-lc$^1/1-1-'},4}.
  {label,230}.
    {test,is_nonempty_list,{f,231},[{x,0}]}.
    {allocate,5,4}.
    {get_list,{x,0},{y,4},{y,3}}.
    {move,{integer,1},{x,0}}.
    {move,{x,1},{y,0}}.
    {move,{x,2},{y,1}}.
    {move,{x,3},{y,2}}.
    {line,[{location,"lc_tests_opt.erl",306}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,2},{x,3}}.
    {move,{y,3},{x,4}}.
    {move,{y,1},{x,2}}.
    {move,{y,4},{x,5}}.
    {move,{y,0},{x,1}}.
    {call_last,6,{f,234},5}.
  {label,231}.
    {test,is_nil,{f,232},[{x,0}]}.
    {move,{x,2},{x,0}}.
    {call_only,2,{f,226}}.
  {label,232}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",304}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pyth/1-lc$^2/1-2-', 6, 234}.
  {label,233}.
    {line,[{location,"lc_tests_opt.erl",304}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pyth/1-lc$^2/1-2-'},6}.
  {label,234}.
    {test,is_nonempty_list,{f,236},[{x,0}]}.
    {get_list,{x,0},{x,6},{x,7}}.
    {gc_bif,'+',{f,235},8,[{x,3},{x,5}],{x,8}}.
    {gc_bif,'+',{f,235},9,[{x,8},{x,6}],{x,9}}.
    {gc_bif,'*',{f,235},10,[{x,3},{x,3}],{x,10}}.
    {gc_bif,'*',{f,235},11,[{x,5},{x,5}],{x,11}}.
    {gc_bif,'+',{f,235},12,[{x,10},{x,11}],{x,12}}.
    {gc_bif,'*',{f,235},13,[{x,6},{x,6}],{x,13}}.
    {test,is_ge,{f,235},[{x,5},{x,3}]}.
    {test,is_ge,{f,235},[{x,6},{x,5}]}.
    {test,is_ge,{f,235},[{x,1},{x,9}]}.
    {test,is_eq_exact,{f,235},[{x,12},{x,13}]}.
    {allocate,3,8}.
    {move,{x,7},{x,0}}.
    {move,{x,6},{y,0}}.
    {move,{x,3},{y,1}}.
    {move,{x,5},{y,2}}.
    {line,[{location,"lc_tests_opt.erl",306}]}.
    {call,6,{f,234}}.
    {test_heap,6,1}.
    {put_tuple,3,{x,1}}.
    {put,{y,1}}.
    {put,{y,2}}.
    {put,{y,0}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,3}.
    return.
  {label,235}.
    {move,{x,7},{x,0}}.
    {call_only,6,{f,234}}.
  {label,236}.
    {test,is_nil,{f,237},[{x,0}]}.
    {move,{x,4},{x,0}}.
    {call_only,4,{f,230}}.
  {label,237}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",304}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-unused/0-lc$^0/1-0-', 1, 239}.
  {label,238}.
    {line,[{location,"lc_tests_opt.erl",297}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-unused/0-lc$^0/1-0-'},1}.
  {label,239}.
    {test,is_nonempty_list,{f,240},[{x,0}]}.
    {allocate,1,1}.
    {get_list,{x,0},{x,1},{y,0}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",297}]}.
    {call_ext,1,{extfunc,timer,sleep,1}}.
    {move,{y,0},{x,0}}.
    {call_last,1,{f,239},1}.
  {label,240}.
    {test,is_nil,{f,238},[{x,0}]}.
    return.


{function, '-pen2_lazy_cps/1-anonymous-5-', 3, 242}.
  {label,241}.
    {line,[]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_lazy_cps/1-anonymous-5-'},3}.
  {label,242}.
    {call_only,3,{f,244}}.


{function, '-pen2_lazy_cps/1-Red_xs/1-1-', 3, 244}.
  {label,243}.
    {line,[{location,"lc_tests_opt.erl",287}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_lazy_cps/1-Red_xs/1-1-'},3}.
  {label,244}.
    {allocate,3,3}.
    {move,{x,0},{y,2}}.
    {move,{x,1},{x,0}}.
    {move,{x,2},{x,1}}.
    {move,{x,0},{y,0}}.
    {move,{x,1},{y,1}}.
    {make_fun2,{f,249},0,0,2}.
    {test,is_nonempty_list,{f,246},[{y,2}]}.
    {get_list,{y,2},{x,1},{x,2}}.
    {gc_bif,'rem',{f,245},3,[{x,1},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,245},[{x,3},{integer,0}]}.
    {move,{x,0},{x,2}}.
    {move,{y,0},{x,1}}.
    {move,{y,1},{x,3}}.
    {move,{y,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",288}]}.
    {call_fun,3}.
    {deallocate,3}.
    return.
  {label,245}.
    {move,{x,0},{y,2}}.
    {trim,2,1}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",289}]}.
    {call_fun,0}.
    {move,{y,0},{x,1}}.
    {line,[{location,"lc_tests_opt.erl",289}]}.
    {call_fun,1}.
    {deallocate,1}.
    return.
  {label,246}.
    {test,is_nil,{f,247},[{y,2}]}.
    {move,nil,{x,0}}.
    {deallocate,3}.
    return.
  {label,247}.
    {test_heap,2,0}.
    {put_tuple,1,{x,1}}.
    {put,{y,2}}.
    {line,[{location,"lc_tests_opt.erl",287}]}.
    {case_end,{x,1}}.


{function, '-pen2_lazy_cps/1-anonymous-4-', 3, 249}.
  {label,248}.
    {line,[]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_lazy_cps/1-anonymous-4-'},3}.
  {label,249}.
    {call_only,3,{f,244}}.


{function, '-pen2_lazy_cps/1-anonymous-3-', 3, 251}.
  {label,250}.
    {line,[]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_lazy_cps/1-anonymous-3-'},3}.
  {label,251}.
    {call_only,3,{f,253}}.


{function, '-pen2_lazy_cps/1-Red_ys/3-0-', 3, 253}.
  {label,252}.
    {line,[{location,"lc_tests_opt.erl",282}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_lazy_cps/1-Red_ys/3-0-'},3}.
  {label,253}.
    {allocate,5,3}.
    {init,{y,0}}.
    {init,{y,1}}.
    {move,{x,2},{y,2}}.
    {move,{x,1},{y,3}}.
    {move,{x,0},{y,4}}.
    {make_fun2,{f,258},0,0,0}.
    {test,is_nonempty_list,{f,256},[{y,4}]}.
    {get_list,{y,4},{x,1},{x,2}}.
    {test,is_nonempty_list,{f,255},[{y,3}]}.
    {get_list,{y,3},{x,3},{x,4}}.
    {gc_bif,'rem',{f,254},5,[{x,3},{integer,2}],{x,2}}.
    {test,is_eq_exact,{f,254},[{x,2},{integer,1}]}.
    {gc_bif,'+',{f,254},5,[{x,1},{x,3}],{x,2}}.
    {gc_bif,'rem',{f,254},5,[{x,2},{integer,5}],{x,2}}.
    {bif,'=/=',{f,254},[{x,2},{integer,0}],{x,2}}.
    {move,{x,2},{x,5}}.
    {test,is_eq_exact,{f,254},[{x,5},{atom,true}]}.
    {move,{x,0},{y,0}}.
    {move,{x,4},{x,0}}.
    {move,{x,3},{y,1}}.
    {move,{x,1},{y,3}}.
    {line,[{location,"lc_tests_opt.erl",284}]}.
    {call_fun,0}.
    {move,{y,2},{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,3}}.
    {move,{y,4},{x,0}}.
    {move,{y,1},{y,4}}.
    {trim,3,2}.
    {line,[{location,"lc_tests_opt.erl",284}]}.
    {call_fun,3}.
    {test_heap,6,1}.
    {put_list,{y,1},nil,{x,1}}.
    {put_list,{y,0},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,254}.
    {move,{x,0},{y,3}}.
    {move,{x,4},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",285}]}.
    {call_fun,0}.
    {move,{y,2},{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{y,3},{x,3}}.
    {move,{y,4},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",285}]}.
    {call_fun,3}.
    {deallocate,5}.
    return.
  {label,255}.
    {test,is_nil,{f,256},[{y,3}]}.
    {kill,{y,3}}.
    {kill,{y,4}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",286}]}.
    {call_fun,0}.
    {move,{y,2},{x,1}}.
    {line,[{location,"lc_tests_opt.erl",286}]}.
    {call_fun,1}.
    {deallocate,5}.
    return.
  {label,256}.
    {test_heap,4,0}.
    {put_tuple,3,{x,1}}.
    {put,{y,4}}.
    {put,{y,3}}.
    {put,{y,2}}.
    {line,[{location,"lc_tests_opt.erl",282}]}.
    {case_end,{x,1}}.


{function, '-pen2_lazy_cps/1-anonymous-2-', 3, 258}.
  {label,257}.
    {line,[]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_lazy_cps/1-anonymous-2-'},3}.
  {label,258}.
    {call_only,3,{f,253}}.


{function, '-pen2_lazy/1-anonymous-5-', 3, 260}.
  {label,259}.
    {line,[]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_lazy/1-anonymous-5-'},3}.
  {label,260}.
    {call_only,3,{f,262}}.


{function, '-pen2_lazy/1-Red_xs/1-1-', 3, 262}.
  {label,261}.
    {line,[{location,"lc_tests_opt.erl",272}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_lazy/1-Red_xs/1-1-'},3}.
  {label,262}.
    {allocate,3,3}.
    {move,{x,0},{y,2}}.
    {move,{x,1},{x,0}}.
    {move,{x,2},{x,1}}.
    {move,{x,0},{y,0}}.
    {move,{x,1},{y,1}}.
    {make_fun2,{f,267},0,0,2}.
    {test,is_nonempty_list,{f,264},[{y,2}]}.
    {get_list,{y,2},{x,1},{x,2}}.
    {gc_bif,'rem',{f,263},3,[{x,1},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,263},[{x,3},{integer,0}]}.
    {move,{x,0},{x,2}}.
    {move,{y,0},{x,1}}.
    {move,{y,1},{x,3}}.
    {move,{y,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",273}]}.
    {call_fun,3}.
    {deallocate,3}.
    return.
  {label,263}.
    {move,{x,0},{y,2}}.
    {trim,2,1}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",274}]}.
    {call_fun,0}.
    {move,{y,0},{x,1}}.
    {line,[{location,"lc_tests_opt.erl",274}]}.
    {call_fun,1}.
    {deallocate,1}.
    return.
  {label,264}.
    {test,is_nil,{f,265},[{y,2}]}.
    {move,nil,{x,0}}.
    {deallocate,3}.
    return.
  {label,265}.
    {test_heap,2,0}.
    {put_tuple,1,{x,1}}.
    {put,{y,2}}.
    {line,[{location,"lc_tests_opt.erl",272}]}.
    {case_end,{x,1}}.


{function, '-pen2_lazy/1-anonymous-4-', 3, 267}.
  {label,266}.
    {line,[]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_lazy/1-anonymous-4-'},3}.
  {label,267}.
    {call_only,3,{f,262}}.


{function, '-pen2_lazy/1-anonymous-3-', 3, 269}.
  {label,268}.
    {line,[]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_lazy/1-anonymous-3-'},3}.
  {label,269}.
    {call_only,3,{f,271}}.


{function, '-pen2_lazy/1-Red_ys/3-0-', 3, 271}.
  {label,270}.
    {line,[{location,"lc_tests_opt.erl",267}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_lazy/1-Red_ys/3-0-'},3}.
  {label,271}.
    {allocate,5,3}.
    {init,{y,0}}.
    {init,{y,1}}.
    {move,{x,2},{y,2}}.
    {move,{x,1},{y,3}}.
    {move,{x,0},{y,4}}.
    {make_fun2,{f,276},0,0,0}.
    {test,is_nonempty_list,{f,274},[{y,4}]}.
    {get_list,{y,4},{x,1},{x,2}}.
    {test,is_nonempty_list,{f,273},[{y,3}]}.
    {get_list,{y,3},{x,3},{x,4}}.
    {gc_bif,'rem',{f,272},5,[{x,3},{integer,2}],{x,2}}.
    {test,is_eq_exact,{f,272},[{x,2},{integer,1}]}.
    {gc_bif,'+',{f,272},5,[{x,1},{x,3}],{x,2}}.
    {gc_bif,'rem',{f,272},5,[{x,2},{integer,5}],{x,2}}.
    {bif,'=/=',{f,272},[{x,2},{integer,0}],{x,2}}.
    {move,{x,2},{x,5}}.
    {test,is_eq_exact,{f,272},[{x,5},{atom,true}]}.
    {move,{x,0},{y,0}}.
    {move,{x,4},{x,0}}.
    {move,{x,3},{y,1}}.
    {move,{x,1},{y,3}}.
    {line,[{location,"lc_tests_opt.erl",269}]}.
    {call_fun,0}.
    {move,{y,2},{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,3}}.
    {move,{y,4},{x,0}}.
    {move,{y,1},{y,4}}.
    {trim,3,2}.
    {line,[{location,"lc_tests_opt.erl",269}]}.
    {call_fun,3}.
    {test_heap,6,1}.
    {put_list,{y,1},nil,{x,1}}.
    {put_list,{y,0},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,272}.
    {move,{x,0},{y,3}}.
    {move,{x,4},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",270}]}.
    {call_fun,0}.
    {move,{y,2},{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{y,3},{x,3}}.
    {move,{y,4},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",270}]}.
    {call_fun,3}.
    {deallocate,5}.
    return.
  {label,273}.
    {test,is_nil,{f,274},[{y,3}]}.
    {kill,{y,3}}.
    {kill,{y,4}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",271}]}.
    {call_fun,0}.
    {move,{y,2},{x,1}}.
    {line,[{location,"lc_tests_opt.erl",271}]}.
    {call_fun,1}.
    {deallocate,5}.
    return.
  {label,274}.
    {test_heap,4,0}.
    {put_tuple,3,{x,1}}.
    {put,{y,4}}.
    {put,{y,3}}.
    {put,{y,2}}.
    {line,[{location,"lc_tests_opt.erl",267}]}.
    {case_end,{x,1}}.


{function, '-pen2_lazy/1-anonymous-2-', 3, 276}.
  {label,275}.
    {line,[]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_lazy/1-anonymous-2-'},3}.
  {label,276}.
    {call_only,3,{f,271}}.


{function, '-pen2_mr/1-anonymous-5-', 3, 278}.
  {label,277}.
    {line,[]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_mr/1-anonymous-5-'},3}.
  {label,278}.
    {call_only,3,{f,280}}.


{function, '-pen2_mr/1-Red_xs/1-1-', 3, 280}.
  {label,279}.
    {line,[{location,"lc_tests_opt.erl",256}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_mr/1-Red_xs/1-1-'},3}.
  {label,280}.
    {allocate,3,3}.
    {move,{x,0},{y,2}}.
    {move,{x,1},{x,0}}.
    {move,{x,2},{x,1}}.
    {move,{x,0},{y,0}}.
    {move,{x,1},{y,1}}.
    {make_fun2,{f,285},0,0,2}.
    {test,is_nonempty_list,{f,282},[{y,2}]}.
    {get_list,{y,2},{x,1},{x,2}}.
    {gc_bif,'rem',{f,281},3,[{x,1},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,281},[{x,3},{integer,0}]}.
    {move,{x,0},{x,2}}.
    {move,{y,0},{x,1}}.
    {move,{y,1},{x,3}}.
    {move,{y,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",257}]}.
    {call_fun,3}.
    {deallocate,3}.
    return.
  {label,281}.
    {move,{x,0},{x,1}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",258}]}.
    {call_fun,1}.
    {deallocate,3}.
    return.
  {label,282}.
    {test,is_nil,{f,283},[{y,2}]}.
    {move,nil,{x,0}}.
    {deallocate,3}.
    return.
  {label,283}.
    {test_heap,2,0}.
    {put_tuple,1,{x,1}}.
    {put,{y,2}}.
    {line,[{location,"lc_tests_opt.erl",256}]}.
    {case_end,{x,1}}.


{function, '-pen2_mr/1-anonymous-4-', 3, 285}.
  {label,284}.
    {line,[]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_mr/1-anonymous-4-'},3}.
  {label,285}.
    {call_only,3,{f,280}}.


{function, '-pen2_mr/1-anonymous-3-', 3, 287}.
  {label,286}.
    {line,[]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_mr/1-anonymous-3-'},3}.
  {label,287}.
    {call_only,3,{f,289}}.


{function, '-pen2_mr/1-Red_ys/3-0-', 3, 289}.
  {label,288}.
    {line,[{location,"lc_tests_opt.erl",250}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_mr/1-Red_ys/3-0-'},3}.
  {label,289}.
    {allocate,3,3}.
    {move,{x,1},{y,0}}.
    {move,{x,0},{y,1}}.
    {move,{x,2},{y,2}}.
    {make_fun2,{f,294},0,0,0}.
    {test,is_nonempty_list,{f,292},[{y,1}]}.
    {get_list,{y,1},{x,1},{x,2}}.
    {test,is_nonempty_list,{f,291},[{y,0}]}.
    {get_list,{y,0},{x,3},{x,4}}.
    {gc_bif,'rem',{f,290},5,[{x,3},{integer,2}],{x,2}}.
    {test,is_eq_exact,{f,290},[{x,2},{integer,1}]}.
    {gc_bif,'+',{f,290},5,[{x,1},{x,3}],{x,2}}.
    {gc_bif,'rem',{f,290},5,[{x,2},{integer,5}],{x,2}}.
    {bif,'=/=',{f,290},[{x,2},{integer,0}],{x,2}}.
    {move,{x,2},{x,5}}.
    {test,is_eq_exact,{f,290},[{x,5},{atom,true}]}.
    {move,{x,3},{x,6}}.
    {move,{x,1},{x,5}}.
    {move,{y,2},{x,2}}.
    {move,{x,4},{x,1}}.
    {move,{x,0},{x,3}}.
    {move,{y,1},{x,0}}.
    {move,{x,6},{y,1}}.
    {move,{x,5},{y,2}}.
    {trim,1,2}.
    {line,[{location,"lc_tests_opt.erl",252}]}.
    {call_fun,3}.
    {test_heap,6,1}.
    {put_list,{y,0},nil,{x,1}}.
    {put_list,{y,1},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,290}.
    {move,{y,2},{x,2}}.
    {move,{x,4},{x,1}}.
    {move,{x,0},{x,3}}.
    {move,{y,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",253}]}.
    {call_fun,3}.
    {deallocate,3}.
    return.
  {label,291}.
    {test,is_nil,{f,292},[{y,0}]}.
    {move,{y,2},{x,1}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",255}]}.
    {call_fun,1}.
    {deallocate,3}.
    return.
  {label,292}.
    {test_heap,4,0}.
    {put_tuple,3,{x,1}}.
    {put,{y,1}}.
    {put,{y,0}}.
    {put,{y,2}}.
    {line,[{location,"lc_tests_opt.erl",250}]}.
    {case_end,{x,1}}.


{function, '-pen2_mr/1-anonymous-2-', 3, 294}.
  {label,293}.
    {line,[]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_mr/1-anonymous-2-'},3}.
  {label,294}.
    {call_only,3,{f,289}}.


{function, '-pen2_listsfuns/1-fun-1-', 1, 296}.
  {label,295}.
    {line,[{location,"lc_tests_opt.erl",244}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_listsfuns/1-fun-1-'},1}.
  {label,296}.
    {line,[{location,"lc_tests_opt.erl",244}]}.
    {gc_bif,'rem',{f,0},1,[{x,0},{integer,2}],{x,0}}.
    {bif,'=:=',{f,0},[{x,0},{integer,0}],{x,0}}.
    return.


{function, '-pen2_listsfuns/1-lc$^0/1-0-', 2, 298}.
  {label,297}.
    {line,[{location,"lc_tests_opt.erl",244}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_listsfuns/1-lc$^0/1-0-'},2}.
  {label,298}.
    {test,is_nonempty_list,{f,299},[{x,0}]}.
    {allocate,3,2}.
    {get_list,{x,0},{y,1},{y,2}}.
    {move,{x,1},{y,0}}.
    {make_fun2,{f,302},0,0,0}.
    {move,{y,0},{x,1}}.
    {line,[{location,"lc_tests_opt.erl",245}]}.
    {call_ext,2,{extfunc,lists,filter,2}}.
    {move,{y,1},{x,2}}.
    {move,{y,2},{x,3}}.
    {move,{y,0},{x,1}}.
    {call_last,4,{f,304},3}.
  {label,299}.
    {test,is_nil,{f,300},[{x,0}]}.
    return.
  {label,300}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",244}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pen2_listsfuns/1-fun-2-', 1, 302}.
  {label,301}.
    {line,[{location,"lc_tests_opt.erl",245}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_listsfuns/1-fun-2-'},1}.
  {label,302}.
    {line,[{location,"lc_tests_opt.erl",245}]}.
    {gc_bif,'rem',{f,0},1,[{x,0},{integer,2}],{x,0}}.
    {bif,'=:=',{f,0},[{x,0},{integer,1}],{x,0}}.
    return.


{function, '-pen2_listsfuns/1-lc$^1/1-1-', 4, 304}.
  {label,303}.
    {line,[{location,"lc_tests_opt.erl",244}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_listsfuns/1-lc$^1/1-1-'},4}.
  {label,304}.
    {test,is_nonempty_list,{f,305},[{x,0}]}.
    {allocate,2,4}.
    {get_list,{x,0},{y,0},{x,0}}.
    {move,{x,2},{y,1}}.
    {line,[{location,"lc_tests_opt.erl",245}]}.
    {call,4,{f,304}}.
    {test_heap,6,1}.
    {put_list,{y,0},nil,{x,1}}.
    {put_list,{y,1},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,305}.
    {test,is_nil,{f,306},[{x,0}]}.
    {move,{x,3},{x,0}}.
    {call_only,2,{f,298}}.
  {label,306}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",244}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pen2_listsfuns/1-fun-0-', 1, 308}.
  {label,307}.
    {line,[{location,"lc_tests_opt.erl",243}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_listsfuns/1-fun-0-'},1}.
  {label,308}.
    {test,is_nonempty_list,{f,307},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_nonempty_list,{f,307},[{x,2}]}.
    {get_list,{x,2},{x,3},{x,4}}.
    {test,is_nil,{f,307},[{x,4}]}.
    {line,[{location,"lc_tests_opt.erl",243}]}.
    {gc_bif,'+',{f,0},4,[{x,1},{x,3}],{x,0}}.
    {line,[{location,"lc_tests_opt.erl",243}]}.
    {gc_bif,'rem',{f,0},1,[{x,0},{integer,5}],{x,0}}.
    {bif,'=/=',{f,0},[{x,0},{integer,0}],{x,0}}.
    return.


{function, '-pen2_foldr/1-fun-1-', 3, 310}.
  {label,309}.
    {line,[{location,"lc_tests_opt.erl",228}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_foldr/1-fun-1-'},3}.
  {label,310}.
    {gc_bif,'rem',{f,311},3,[{x,0},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,311},[{x,3},{integer,0}]}.
    {allocate,2,3}.
    {move,{x,2},{y,0}}.
    {move,{x,1},{y,1}}.
    {make_fun2,{f,313},0,0,1}.
    {move,{y,0},{x,2}}.
    {move,nil,{x,1}}.
    {trim,1,1}.
    {line,[{location,"lc_tests_opt.erl",230}]}.
    {call_ext,3,{extfunc,lists,foldr,3}}.
    {move,{y,0},{x,1}}.
    {line,[{location,"lc_tests_opt.erl",236}]}.
    {call_ext_last,2,{extfunc,erlang,'++',2},1}.
  {label,311}.
    {move,{x,1},{x,0}}.
    return.


{function, '-pen2_foldr/1-fun-0-', 3, 313}.
  {label,312}.
    {line,[{location,"lc_tests_opt.erl",231}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_foldr/1-fun-0-'},3}.
  {label,313}.
    {gc_bif,'rem',{f,314},3,[{x,0},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,314},[{x,3},{integer,1}]}.
    {gc_bif,'+',{f,314},3,[{x,2},{x,0}],{x,3}}.
    {gc_bif,'rem',{f,314},4,[{x,3},{integer,5}],{x,3}}.
    {test,is_ne_exact,{f,314},[{x,3},{integer,0}]}.
    {test_heap,6,3}.
    {put_list,{x,0},nil,{x,0}}.
    {put_list,{x,2},{x,0},{x,2}}.
    {put_list,{x,2},{x,1},{x,0}}.
    return.
  {label,314}.
    {move,{x,1},{x,0}}.
    return.


{function, '-pen2_foldl/1-fun-1-', 3, 316}.
  {label,315}.
    {line,[{location,"lc_tests_opt.erl",212}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_foldl/1-fun-1-'},3}.
  {label,316}.
    {gc_bif,'rem',{f,317},3,[{x,0},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,317},[{x,3},{integer,0}]}.
    {allocate,2,3}.
    {move,{x,2},{y,0}}.
    {move,{x,1},{y,1}}.
    {make_fun2,{f,319},0,0,1}.
    {move,{y,0},{x,2}}.
    {move,nil,{x,1}}.
    {trim,1,1}.
    {line,[{location,"lc_tests_opt.erl",214}]}.
    {call_ext,3,{extfunc,lists,foldl,3}}.
    {move,{y,0},{x,1}}.
    {line,[{location,"lc_tests_opt.erl",220}]}.
    {call_ext_last,2,{extfunc,erlang,'++',2},1}.
  {label,317}.
    {move,{x,1},{x,0}}.
    return.


{function, '-pen2_foldl/1-fun-0-', 3, 319}.
  {label,318}.
    {line,[{location,"lc_tests_opt.erl",215}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_foldl/1-fun-0-'},3}.
  {label,319}.
    {gc_bif,'rem',{f,320},3,[{x,0},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,320},[{x,3},{integer,1}]}.
    {gc_bif,'+',{f,320},3,[{x,2},{x,0}],{x,3}}.
    {gc_bif,'rem',{f,320},4,[{x,3},{integer,5}],{x,3}}.
    {test,is_ne_exact,{f,320},[{x,3},{integer,0}]}.
    {test_heap,6,3}.
    {put_list,{x,0},nil,{x,0}}.
    {put_list,{x,2},{x,0},{x,2}}.
    {put_list,{x,2},{x,1},{x,0}}.
    return.
  {label,320}.
    {move,{x,1},{x,0}}.
    return.


{function, '-pen2/1-lc$^0/1-0-', 2, 322}.
  {label,321}.
    {line,[{location,"lc_tests_opt.erl",173}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2/1-lc$^0/1-0-'},2}.
  {label,322}.
    {test,is_nonempty_list,{f,324},[{x,0}]}.
    {get_list,{x,0},{x,2},{x,3}}.
    {gc_bif,'rem',{f,323},4,[{x,2},{integer,2}],{x,4}}.
    {test,is_eq_exact,{f,323},[{x,4},{integer,0}]}.
    {allocate,3,4}.
    {move,{integer,1},{x,0}}.
    {move,{x,1},{y,0}}.
    {move,{x,3},{y,1}}.
    {move,{x,2},{y,2}}.
    {line,[{location,"lc_tests_opt.erl",175}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,1},{x,2}}.
    {move,{y,2},{x,3}}.
    {move,{y,0},{x,1}}.
    {call_last,4,{f,327},3}.
  {label,323}.
    {move,{x,3},{x,0}}.
    {call_only,2,{f,322}}.
  {label,324}.
    {test,is_nil,{f,325},[{x,0}]}.
    return.
  {label,325}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",173}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pen2/1-lc$^1/1-1-', 4, 327}.
  {label,326}.
    {line,[{location,"lc_tests_opt.erl",173}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2/1-lc$^1/1-1-'},4}.
  {label,327}.
    {test,is_nonempty_list,{f,329},[{x,0}]}.
    {get_list,{x,0},{x,4},{x,5}}.
    {gc_bif,'rem',{f,328},6,[{x,4},{integer,2}],{x,6}}.
    {gc_bif,'+',{f,328},7,[{x,3},{x,4}],{x,7}}.
    {gc_bif,'rem',{f,328},8,[{x,7},{integer,5}],{x,8}}.
    {test,is_eq_exact,{f,328},[{x,6},{integer,1}]}.
    {test,is_ne_exact,{f,328},[{x,8},{integer,0}]}.
    {allocate,2,6}.
    {move,{x,5},{x,0}}.
    {move,{x,4},{y,0}}.
    {move,{x,3},{y,1}}.
    {line,[{location,"lc_tests_opt.erl",175}]}.
    {call,4,{f,327}}.
    {test_heap,6,1}.
    {put_list,{y,0},nil,{x,1}}.
    {put_list,{y,1},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,328}.
    {move,{x,5},{x,0}}.
    {call_only,4,{f,327}}.
  {label,329}.
    {test,is_nil,{f,330},[{x,0}]}.
    {move,{x,2},{x,0}}.
    {call_only,2,{f,322}}.
  {label,330}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",173}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pen/1-lc$^0/1-0-', 2, 332}.
  {label,331}.
    {line,[{location,"lc_tests_opt.erl",166}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen/1-lc$^0/1-0-'},2}.
  {label,332}.
    {test,is_nonempty_list,{f,334},[{x,0}]}.
    {get_list,{x,0},{x,2},{x,3}}.
    {gc_bif,'rem',{f,333},4,[{x,2},{integer,2}],{x,4}}.
    {test,is_eq_exact,{f,333},[{x,4},{integer,0}]}.
    {allocate,3,4}.
    {move,{integer,1},{x,0}}.
    {move,{x,1},{y,0}}.
    {move,{x,3},{y,1}}.
    {move,{x,2},{y,2}}.
    {line,[{location,"lc_tests_opt.erl",168}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,1},{x,2}}.
    {move,{y,2},{x,3}}.
    {move,{y,0},{x,1}}.
    {call_last,4,{f,337},3}.
  {label,333}.
    {move,{x,3},{x,0}}.
    {call_only,2,{f,332}}.
  {label,334}.
    {test,is_nil,{f,335},[{x,0}]}.
    return.
  {label,335}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",166}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pen/1-lc$^1/1-1-', 4, 337}.
  {label,336}.
    {line,[{location,"lc_tests_opt.erl",166}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen/1-lc$^1/1-1-'},4}.
  {label,337}.
    {test,is_nonempty_list,{f,339},[{x,0}]}.
    {get_list,{x,0},{x,4},{x,5}}.
    {gc_bif,'rem',{f,338},6,[{x,4},{integer,2}],{x,6}}.
    {test,is_eq_exact,{f,338},[{x,6},{integer,1}]}.
    {allocate,2,6}.
    {move,{x,5},{x,0}}.
    {move,{x,4},{y,0}}.
    {move,{x,3},{y,1}}.
    {line,[{location,"lc_tests_opt.erl",168}]}.
    {call,4,{f,337}}.
    {test_heap,6,1}.
    {put_list,{y,0},nil,{x,1}}.
    {put_list,{y,1},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,338}.
    {move,{x,5},{x,0}}.
    {call_only,4,{f,337}}.
  {label,339}.
    {test,is_nil,{f,340},[{x,0}]}.
    {move,{x,2},{x,0}}.
    {call_only,2,{f,332}}.
  {label,340}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",166}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-ses/1-lc$^0/1-0-', 1, 342}.
  {label,341}.
    {line,[{location,"lc_tests_opt.erl",161}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-ses/1-lc$^0/1-0-'},1}.
  {label,342}.
    {test,is_nonempty_list,{f,344},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {gc_bif,'rem',{f,343},3,[{x,1},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,343},[{x,3},{integer,0}]}.
    {allocate_zero,1,3}.
    {line,[{location,"lc_tests_opt.erl",161}]}.
    {gc_bif,'*',{f,0},3,[{x,1},{x,1}],{y,0}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",161}]}.
    {call,1,{f,342}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,343}.
    {move,{x,2},{x,0}}.
    {call_only,1,{f,342}}.
  {label,344}.
    {test,is_nil,{f,341},[{x,0}]}.
    return.


{function, '-ss/1-lc$^0/1-0-', 1, 346}.
  {label,345}.
    {line,[{location,"lc_tests_opt.erl",157}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-ss/1-lc$^0/1-0-'},1}.
  {label,346}.
    {test,is_nonempty_list,{f,347},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {line,[{location,"lc_tests_opt.erl",157}]}.
    {gc_bif,'*',{f,0},3,[{x,1},{x,1}],{y,0}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",157}]}.
    {call,1,{f,346}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,347}.
    {test,is_nil,{f,345},[{x,0}]}.
    return.


{function, '-all_tests/0-fun-26-', 0, 349}.
  {label,348}.
    {line,[{location,"lc_tests_opt.erl",150}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-26-'},0}.
  {label,349}.
    {call_only,0,{f,96}}.


{function, '-all_tests/0-fun-25-', 1, 351}.
  {label,350}.
    {line,[{location,"lc_tests_opt.erl",149}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-25-'},1}.
  {label,351}.
    {call_only,1,{f,124}}.


{function, '-all_tests/0-fun-24-', 1, 353}.
  {label,352}.
    {line,[{location,"lc_tests_opt.erl",148}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-24-'},1}.
  {label,353}.
    {call_only,1,{f,122}}.


{function, '-all_tests/0-fun-23-', 1, 355}.
  {label,354}.
    {line,[{location,"lc_tests_opt.erl",147}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-23-'},1}.
  {label,355}.
    {call_only,1,{f,120}}.


{function, '-all_tests/0-fun-22-', 1, 357}.
  {label,356}.
    {line,[{location,"lc_tests_opt.erl",146}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-22-'},1}.
  {label,357}.
    {call_only,1,{f,118}}.


{function, '-all_tests/0-fun-21-', 1, 359}.
  {label,358}.
    {line,[{location,"lc_tests_opt.erl",145}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-21-'},1}.
  {label,359}.
    {call_only,1,{f,116}}.


{function, '-all_tests/0-fun-20-', 2, 361}.
  {label,360}.
    {line,[{location,"lc_tests_opt.erl",144}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-20-'},2}.
  {label,361}.
    {call_only,2,{f,114}}.


{function, '-all_tests/0-fun-19-', 1, 363}.
  {label,362}.
    {line,[{location,"lc_tests_opt.erl",143}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-19-'},1}.
  {label,363}.
    {call_only,1,{f,112}}.


{function, '-all_tests/0-fun-18-', 2, 365}.
  {label,364}.
    {line,[{location,"lc_tests_opt.erl",142}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-18-'},2}.
  {label,365}.
    {call_only,2,{f,110}}.


{function, '-all_tests/0-fun-17-', 1, 367}.
  {label,366}.
    {line,[{location,"lc_tests_opt.erl",141}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-17-'},1}.
  {label,367}.
    {call_only,1,{f,108}}.


{function, '-all_tests/0-fun-16-', 1, 369}.
  {label,368}.
    {line,[{location,"lc_tests_opt.erl",140}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-16-'},1}.
  {label,369}.
    {call_only,1,{f,106}}.


{function, '-all_tests/0-fun-15-', 1, 371}.
  {label,370}.
    {line,[{location,"lc_tests_opt.erl",139}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-15-'},1}.
  {label,371}.
    {call_only,1,{f,94}}.


{function, '-all_tests/0-fun-14-', 1, 373}.
  {label,372}.
    {line,[{location,"lc_tests_opt.erl",138}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-14-'},1}.
  {label,373}.
    {call_only,1,{f,92}}.


{function, '-all_tests/0-fun-13-', 1, 375}.
  {label,374}.
    {line,[{location,"lc_tests_opt.erl",137}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-13-'},1}.
  {label,375}.
    {call_only,1,{f,90}}.


{function, '-all_tests/0-fun-12-', 1, 377}.
  {label,376}.
    {line,[{location,"lc_tests_opt.erl",136}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-12-'},1}.
  {label,377}.
    {call_only,1,{f,88}}.


{function, '-all_tests/0-fun-11-', 1, 379}.
  {label,378}.
    {line,[{location,"lc_tests_opt.erl",135}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-11-'},1}.
  {label,379}.
    {call_only,1,{f,84}}.


{function, '-all_tests/0-fun-10-', 1, 381}.
  {label,380}.
    {line,[{location,"lc_tests_opt.erl",134}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-10-'},1}.
  {label,381}.
    {call_only,1,{f,86}}.


{function, '-all_tests/0-fun-9-', 1, 383}.
  {label,382}.
    {line,[{location,"lc_tests_opt.erl",133}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-9-'},1}.
  {label,383}.
    {call_only,1,{f,76}}.


{function, '-all_tests/0-fun-8-', 1, 385}.
  {label,384}.
    {line,[{location,"lc_tests_opt.erl",132}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-8-'},1}.
  {label,385}.
    {call_only,1,{f,69}}.


{function, '-all_tests/0-fun-7-', 1, 387}.
  {label,386}.
    {line,[{location,"lc_tests_opt.erl",131}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-7-'},1}.
  {label,387}.
    {call_only,1,{f,67}}.


{function, '-all_tests/0-fun-6-', 1, 389}.
  {label,388}.
    {line,[{location,"lc_tests_opt.erl",130}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-6-'},1}.
  {label,389}.
    {call_only,1,{f,65}}.


{function, '-all_tests/0-fun-5-', 1, 391}.
  {label,390}.
    {line,[{location,"lc_tests_opt.erl",129}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-5-'},1}.
  {label,391}.
    {call_only,1,{f,104}}.


{function, '-all_tests/0-fun-4-', 0, 393}.
  {label,392}.
    {line,[{location,"lc_tests_opt.erl",128}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-4-'},0}.
  {label,393}.
    {call_only,0,{f,102}}.


{function, '-all_tests/0-fun-3-', 1, 395}.
  {label,394}.
    {line,[{location,"lc_tests_opt.erl",127}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-3-'},1}.
  {label,395}.
    {call_only,1,{f,100}}.


{function, '-all_tests/0-fun-2-', 1, 397}.
  {label,396}.
    {line,[{location,"lc_tests_opt.erl",126}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-2-'},1}.
  {label,397}.
    {call_only,1,{f,98}}.


{function, '-all_tests/0-fun-1-', 1, 399}.
  {label,398}.
    {line,[{location,"lc_tests_opt.erl",125}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-1-'},1}.
  {label,399}.
    {call_only,1,{f,63}}.


{function, '-all_tests/0-fun-0-', 1, 401}.
  {label,400}.
    {line,[{location,"lc_tests_opt.erl",124}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-0-'},1}.
  {label,401}.
    {call_only,1,{f,61}}.


{function, '-run_tests/1-fun-0-', 0, 403}.
  {label,402}.
    {line,[{location,"lc_tests_opt.erl",84}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-run_tests/1-fun-0-'},0}.
  {label,403}.
    {call_only,0,{f,40}}.


{function, '-run_tests/3-lc$^0/1-0-', 2, 405}.
  {label,404}.
    {line,[{location,"lc_tests_opt.erl",76}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-run_tests/3-lc$^0/1-0-'},2}.
  {label,405}.
    {test,is_nonempty_list,{f,407},[{x,0}]}.
    {get_list,{x,0},{x,2},{x,3}}.
    {test,is_tuple,{f,406},[{x,2}]}.
    {test,test_arity,{f,406},[{x,2},4]}.
    {get_tuple_element,{x,2},0,{x,4}}.
    {get_tuple_element,{x,2},1,{x,5}}.
    {get_tuple_element,{x,2},2,{x,6}}.
    {get_tuple_element,{x,2},3,{x,7}}.
    {test,is_tuple,{f,406},[{x,6}]}.
    {test,test_arity,{f,406},[{x,6},2]}.
    {allocate,6,8}.
    {get_tuple_element,{x,6},0,{y,3}}.
    {get_tuple_element,{x,6},1,{y,2}}.
    {move,{integer,0},{x,2}}.
    {move,{x,5},{x,0}}.
    {move,{x,1},{y,0}}.
    {move,{x,3},{y,1}}.
    {move,{x,7},{y,4}}.
    {move,{x,4},{y,5}}.
    {line,[{location,"lc_tests_opt.erl",76}]}.
    {call_ext,3,{extfunc,maps,get,3}}.
    {move,{x,0},{x,2}}.
    {move,{y,0},{x,1}}.
    {move,{y,1},{x,0}}.
    {move,{x,2},{y,1}}.
    {trim,1,5}.
    {line,[{location,"lc_tests_opt.erl",77}]}.
    {call,2,{f,405}}.
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
  {label,406}.
    {move,{x,3},{x,0}}.
    {call_only,2,{f,405}}.
  {label,407}.
    {test,is_nil,{f,408},[{x,0}]}.
    return.
  {label,408}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",76}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-test/3-fun-0-', 3, 410}.
  {label,409}.
    {line,[{location,"lc_tests_opt.erl",58}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-test/3-fun-0-'},3}.
  {label,410}.
    {allocate_zero,4,3}.
    {bif,self,{f,0},[],{y,0}}.
    {move,{x,1},{y,1}}.
    {move,{x,2},{y,2}}.
    {move,{x,0},{y,3}}.
    {line,[{location,"lc_tests_opt.erl",59}]}.
    {call,0,{f,25}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {kill,{y,0}}.
    {line,[{location,"lc_tests_opt.erl",59}]}.
    {call_ext,2,{extfunc,erlang,process_info,2}}.
    {move,{x,0},{x,2}}.
    {move,{y,1},{x,1}}.
    {move,{y,2},{x,0}}.
    {move,{x,2},{y,2}}.
    {kill,{y,1}}.
    {line,[{location,"lc_tests_opt.erl",60}]}.
    {call_ext,2,{extfunc,erlang,apply,2}}.
    {bif,self,{f,0},[],{y,0}}.
    {move,{x,0},{y,1}}.
    {line,[{location,"lc_tests_opt.erl",62}]}.
    {call,0,{f,25}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {trim,1,3}.
    {line,[{location,"lc_tests_opt.erl",62}]}.
    {call_ext,2,{extfunc,erlang,process_info,2}}.
    {test_heap,4,1}.
    {put_tuple,3,{x,1}}.
    {put,{y,1}}.
    {put,{x,0}}.
    {put,{y,0}}.
    {move,{y,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",63}]}.
    send.
    {deallocate,3}.
    return.


{function, '-lazy_cp_seq/3-fun-1-', 3, 412}.
  {label,411}.
    {line,[{location,"lc_tests_opt.erl",46}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-lazy_cp_seq/3-fun-1-'},3}.
  {label,412}.
    {allocate_zero,2,3}.
    {line,[{location,"lc_tests_opt.erl",46}]}.
    {gc_bif,'+',{f,0},3,[{x,2},{integer,1}],{y,1}}.
    {move,{x,1},{y,0}}.
    {move,{x,2},{x,1}}.
    {make_fun2,{f,414},0,0,2}.
    {move,{y,0},{x,1}}.
    {move,{x,0},{x,2}}.
    {move,{y,1},{x,0}}.
    {call_last,3,{f,22},2}.


{function, '-lazy_cp_seq/3-fun-0-', 3, 414}.
  {label,413}.
    {line,[{location,"lc_tests_opt.erl",46}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-lazy_cp_seq/3-fun-0-'},3}.
  {label,414}.
    {move,{x,2},{x,3}}.
    {move,{x,1},{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{x,3},{x,0}}.
    {call_only,3,{f,18}}.


{function, '-lazy_cp_seq/2-fun-0-', 1, 416}.
  {label,415}.
    {line,[{location,"lc_tests_opt.erl",43}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-lazy_cp_seq/2-fun-0-'},1}.
  {label,416}.
    return.


{function, '-lazy_seq/2-fun-0-', 2, 418}.
  {label,417}.
    {line,[{location,"lc_tests_opt.erl",32}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-lazy_seq/2-fun-0-'},2}.
  {label,418}.
    {line,[{location,"lc_tests_opt.erl",32}]}.
    {gc_bif,'+',{f,0},2,[{x,1},{integer,1}],{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{x,2},{x,0}}.
    {call_only,2,{f,12}}.


{function, '-random_bin/1-lc$^0/1-0-', 1, 420}.
  {label,419}.
    {line,[{location,"lc_tests_opt.erl",18}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-random_bin/1-lc$^0/1-0-'},1}.
  {label,420}.
    {test,is_nonempty_list,{f,421},[{x,0}]}.
    {allocate,1,1}.
    {get_list,{x,0},{x,1},{y,0}}.
    {move,{integer,255},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",18}]}.
    {call_ext,1,{extfunc,random,uniform,1}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",18}]}.
    {call,1,{f,420}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,421}.
    {test,is_nil,{f,419},[{x,0}]}.
    return.
