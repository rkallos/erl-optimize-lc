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
           {pen,1},
           {pen2,1},
           {pen2_dumb,1},
           {pen2_dumb_helper,4},
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

{labels, 334}.


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
    {call,1,{f,332}}.
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
    {make_fun2,{f,330},0,0,2}.
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
    {make_fun2,{f,328},0,0,0}.
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
    {make_fun2,{f,324},0,0,3}.
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
    {make_fun2,{f,322},0,0,3}.
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
    {call_last,2,{f,317},2}.
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
    {make_fun2,{f,315},0,0,0}.
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
    {allocate_zero,25,0}.
    {move,{integer,50},{x,1}}.
    {move,{integer,500},{x,0}}.
    {line,[{location,"lc_tests.erl",122}]}.
    {call,2,{f,6}}.
    {move,{x,0},{y,24}}.
    {move,{integer,50},{x,1}}.
    {move,{integer,500},{x,0}}.
    {line,[{location,"lc_tests.erl",123}]}.
    {call,2,{f,9}}.
    {move,{x,0},{y,23}}.
    {make_fun2,{f,313},0,0,0}.
    {move,{x,0},{y,22}}.
    {make_fun2,{f,311},0,0,0}.
    {move,{x,0},{y,21}}.
    {make_fun2,{f,309},0,0,0}.
    {move,{x,0},{y,20}}.
    {make_fun2,{f,307},0,0,0}.
    {move,{x,0},{y,19}}.
    {make_fun2,{f,305},0,0,0}.
    {move,{x,0},{y,18}}.
    {make_fun2,{f,303},0,0,0}.
    {move,{x,0},{y,17}}.
    {make_fun2,{f,301},0,0,0}.
    {move,{x,0},{y,16}}.
    {make_fun2,{f,299},0,0,0}.
    {move,{x,0},{y,15}}.
    {make_fun2,{f,297},0,0,0}.
    {move,{x,0},{y,14}}.
    {make_fun2,{f,295},0,0,0}.
    {move,{x,0},{y,13}}.
    {make_fun2,{f,293},0,0,0}.
    {move,{x,0},{y,12}}.
    {make_fun2,{f,291},0,0,0}.
    {test_heap,2,1}.
    {put_list,{y,24},nil,{y,10}}.
    {move,{x,0},{y,11}}.
    {make_fun2,{f,289},0,0,0}.
    {test_heap,2,1}.
    {put_list,{y,23},nil,{y,8}}.
    {move,{x,0},{y,9}}.
    {make_fun2,{f,287},0,0,0}.
    {test_heap,4,1}.
    {put_list,{y,24},nil,{x,1}}.
    {put_list,{atom,param},{x,1},{y,6}}.
    {move,{x,0},{y,7}}.
    {make_fun2,{f,285},0,0,0}.
    {test_heap,2,1}.
    {put_list,{y,23},nil,{y,5}}.
    {move,{x,0},{y,23}}.
    {make_fun2,{f,283},0,0,0}.
    {move,{x,0},{y,4}}.
    {make_fun2,{f,281},0,0,0}.
    {move,{x,0},{y,3}}.
    {make_fun2,{f,279},0,0,0}.
    {move,{x,0},{y,2}}.
    {make_fun2,{f,277},0,0,0}.
    {move,{x,0},{y,1}}.
    {make_fun2,{f,275},0,0,0}.
    {test_heap,2,1}.
    {put_list,{y,24},nil,{y,0}}.
    {move,{x,0},{y,24}}.
    {make_fun2,{f,273},0,0,0}.
    {test_heap,126,1}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"unused"}}.
    {put,{x,0}}.
    {put,nil}.
    {put_list,{x,1},nil,{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"nested_lc_gen"}}.
    {put,{y,24}}.
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
    {put,{y,23}}.
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
    {put,{literal,"pen2_lazy_cps"}}.
    {put,{y,12}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"pen2_lazy"}}.
    {put,{y,13}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"pen2_mr"}}.
    {put,{y,14}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"pen2_listsfuns"}}.
    {put,{y,15}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"pen2_foldl"}}.
    {put,{y,16}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"pen2_foldr"}}.
    {put,{y,17}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"pen2_dumb"}}.
    {put,{y,18}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"pen2"}}.
    {put,{y,19}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"pen"}}.
    {put,{y,20}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"ses"}}.
    {put,{y,21}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,1}}.
    {put_tuple,3,{x,2}}.
    {put,{literal,"ss"}}.
    {put,{y,22}}.
    {put,{literal,[500]}}.
    {put_list,{x,2},{x,1},{x,0}}.
    {deallocate,25}.
    return.


{function, ss, 1, 61}.
  {label,60}.
    {line,[{location,"lc_tests.erl",150}]}.
    {func_info,{atom,lc_tests},{atom,ss},1}.
  {label,61}.
    {test,is_integer,{f,60},[{x,0}]}.
    {allocate,0,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests.erl",151}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {line,[{location,"lc_tests.erl",151}]}.
    {call,1,{f,270}}.
    {line,[{location,"lc_tests.erl",151}]}.
    {call_ext_last,1,{extfunc,lists,sum,1},0}.


{function, ses, 1, 63}.
  {label,62}.
    {line,[{location,"lc_tests.erl",154}]}.
    {func_info,{atom,lc_tests},{atom,ses},1}.
  {label,63}.
    {test,is_integer,{f,62},[{x,0}]}.
    {allocate,0,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests.erl",155}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {line,[{location,"lc_tests.erl",155}]}.
    {call,1,{f,266}}.
    {line,[{location,"lc_tests.erl",155}]}.
    {call_ext_last,1,{extfunc,lists,sum,1},0}.


{function, pen, 1, 65}.
  {label,64}.
    {line,[{location,"lc_tests.erl",159}]}.
    {func_info,{atom,lc_tests},{atom,pen},1}.
  {label,65}.
    {test,is_integer,{f,64},[{x,0}]}.
    {allocate,1,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests.erl",160}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,0},{x,1}}.
    {call_last,2,{f,256},1}.


{function, pen2, 1, 67}.
  {label,66}.
    {line,[{location,"lc_tests.erl",166}]}.
    {func_info,{atom,lc_tests},{atom,pen2},1}.
  {label,67}.
    {test,is_integer,{f,66},[{x,0}]}.
    {allocate,1,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests.erl",167}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,0},{x,1}}.
    {call_last,2,{f,246},1}.


{function, pen2_dumb, 1, 69}.
  {label,68}.
    {line,[{location,"lc_tests.erl",174}]}.
    {func_info,{atom,lc_tests},{atom,pen2_dumb},1}.
  {label,69}.
    {move,{integer,1},{x,2}}.
    {move,nil,{x,3}}.
    {move,{integer,1},{x,1}}.
    {call_only,4,{f,71}}.


{function, pen2_dumb_helper, 4, 71}.
  {label,70}.
    {line,[{location,"lc_tests.erl",176}]}.
    {func_info,{atom,lc_tests},{atom,pen2_dumb_helper},4}.
  {label,71}.
    {test,is_lt,{f,72},[{x,0},{x,1}]}.
    {move,{x,3},{x,0}}.
    return.
  {label,72}.
    {test,is_lt,{f,73},[{x,0},{x,2}]}.
    {line,[{location,"lc_tests.erl",179}]}.
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
    {line,[{location,"lc_tests.erl",181}]}.
    {gc_bif,'+',{f,0},4,[{x,2},{integer,1}],{x,4}}.
    {move,{x,2},{y,0}}.
    {move,{x,4},{x,2}}.
    {move,{x,1},{y,1}}.
    {line,[{location,"lc_tests.erl",181}]}.
    {call,4,{f,71}}.
    {test_heap,6,1}.
    {put_list,{y,0},nil,{x,1}}.
    {put_list,{y,1},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,74}.
    {line,[{location,"lc_tests.erl",183}]}.
    {gc_bif,'+',{f,0},4,[{x,2},{integer,1}],{x,2}}.
    {call_only,4,{f,71}}.


{function, pen2_foldl, 1, 76}.
  {label,75}.
    {line,[{location,"lc_tests.erl",186}]}.
    {func_info,{atom,lc_tests},{atom,pen2_foldl},1}.
  {label,76}.
    {test,is_integer,{f,75},[{x,0}]}.
    {allocate_zero,1,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests.erl",187}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,240},0,0,1}.
    {move,{y,0},{x,2}}.
    {move,nil,{x,1}}.
    {trim,1,0}.
    {line,[{location,"lc_tests.erl",188}]}.
    {call_ext,3,{extfunc,lists,foldl,3}}.
    {line,[{location,"lc_tests.erl",188}]}.
    {call_ext_last,1,{extfunc,lists,reverse,1},0}.


{function, pen2_foldr, 1, 78}.
  {label,77}.
    {line,[{location,"lc_tests.erl",202}]}.
    {func_info,{atom,lc_tests},{atom,pen2_foldr},1}.
  {label,78}.
    {test,is_integer,{f,77},[{x,0}]}.
    {allocate_zero,1,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests.erl",203}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,234},0,0,1}.
    {move,{y,0},{x,2}}.
    {move,nil,{x,1}}.
    {line,[{location,"lc_tests.erl",204}]}.
    {call_ext_last,3,{extfunc,lists,foldr,3},1}.


{function, pen2_listsfuns, 1, 80}.
  {label,79}.
    {line,[{location,"lc_tests.erl",218}]}.
    {func_info,{atom,lc_tests},{atom,pen2_listsfuns},1}.
  {label,80}.
    {test,is_integer,{f,79},[{x,0}]}.
    {allocate_zero,2,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests.erl",219}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{x,0},{y,1}}.
    {make_fun2,{f,232},0,0,0}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,220},0,0,0}.
    {move,{y,1},{x,1}}.
    {line,[{location,"lc_tests.erl",221}]}.
    {call_ext,2,{extfunc,lists,filter,2}}.
    {move,{y,1},{x,1}}.
    {kill,{y,1}}.
    {line,[{location,"lc_tests.erl",221}]}.
    {call,2,{f,222}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {line,[{location,"lc_tests.erl",220}]}.
    {call_ext_last,2,{extfunc,lists,filter,2},2}.


{function, pen2_mr, 1, 82}.
  {label,81}.
    {line,[{location,"lc_tests.erl",225}]}.
    {func_info,{atom,lc_tests},{atom,pen2_mr},1}.
  {label,82}.
    {test,is_integer,{f,81},[{x,0}]}.
    {allocate_zero,1,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests.erl",226}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,211},0,0,0}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {make_fun2,{f,202},0,0,2}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {line,[{location,"lc_tests.erl",239}]}.
    {call_fun,1}.
    {deallocate,1}.
    return.


{function, pen2_lazy, 1, 84}.
  {label,83}.
    {line,[{location,"lc_tests.erl",242}]}.
    {func_info,{atom,lc_tests},{atom,pen2_lazy},1}.
  {label,84}.
    {test,is_integer,{f,83},[{x,0}]}.
    {allocate_zero,1,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests.erl",243}]}.
    {call,2,{f,12}}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,193},0,0,0}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {make_fun2,{f,184},0,0,2}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {line,[{location,"lc_tests.erl",254}]}.
    {call_fun,1}.
    {deallocate,1}.
    return.


{function, pen2_lazy_cps, 1, 86}.
  {label,85}.
    {line,[{location,"lc_tests.erl",257}]}.
    {func_info,{atom,lc_tests},{atom,pen2_lazy_cps},1}.
  {label,86}.
    {test,is_integer,{f,85},[{x,0}]}.
    {allocate_zero,1,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests.erl",258}]}.
    {call,2,{f,20}}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,175},0,0,0}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {make_fun2,{f,166},0,0,2}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {line,[{location,"lc_tests.erl",269}]}.
    {call_fun,1}.
    {deallocate,1}.
    return.


{function, unused, 0, 88}.
  {label,87}.
    {line,[{location,"lc_tests.erl",273}]}.
    {func_info,{atom,lc_tests},{atom,unused},0}.
  {label,88}.
    {allocate,0,0}.
    {move,{integer,1000},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests.erl",274}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {line,[{location,"lc_tests.erl",274}]}.
    {call,1,{f,163}}.
    {move,{atom,ok},{x,0}}.
    {deallocate,0}.
    return.


{function, single_filter, 1, 90}.
  {label,89}.
    {line,[{location,"lc_tests.erl",280}]}.
    {func_info,{atom,lc_tests},{atom,single_filter},1}.
  {label,90}.
    {test,is_list,{f,89},[{x,0}]}.
    {call_only,1,{f,159}}.


{function, record_extract, 1, 92}.
  {label,91}.
    {line,[{location,"lc_tests.erl",286}]}.
    {func_info,{atom,lc_tests},{atom,record_extract},1}.
  {label,92}.
    {test,is_list,{f,91},[{x,0}]}.
    {call_only,1,{f,155}}.


{function, function_param_lc, 2, 94}.
  {label,93}.
    {line,[{location,"lc_tests.erl",290}]}.
    {func_info,{atom,lc_tests},{atom,function_param_lc},2}.
  {label,94}.
    {test,is_list,{f,93},[{x,1}]}.
    {move,{x,1},{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{x,2},{x,0}}.
    {call_only,2,{f,151}}.


{function, sorted_gen, 1, 96}.
  {label,95}.
    {line,[{location,"lc_tests.erl",294}]}.
    {func_info,{atom,lc_tests},{atom,sorted_gen},1}.
  {label,96}.
    {test,is_list,{f,95},[{x,0}]}.
    {allocate,1,1}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,144},0,0,0}.
    {move,{y,0},{x,1}}.
    {trim,1,0}.
    {line,[{location,"lc_tests.erl",296}]}.
    {call_ext,2,{extfunc,lists,sort,2}}.
    {call_last,1,{f,147},0}.


{function, make_n, 2, 98}.
  {label,97}.
    {line,[{location,"lc_tests.erl",304}]}.
    {func_info,{atom,lc_tests},{atom,make_n},2}.
  {label,98}.
    {test,is_integer,{f,97},[{x,1}]}.
    {allocate,1,2}.
    {move,{x,0},{y,0}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests.erl",305}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,0},{x,1}}.
    {call_last,2,{f,140},1}.


{function, intermediary_list, 1, 100}.
  {label,99}.
    {line,[{location,"lc_tests.erl",308}]}.
    {func_info,{atom,lc_tests},{atom,intermediary_list},1}.
  {label,100}.
    {test,is_integer,{f,99},[{x,0}]}.
    {allocate,0,1}.
    {move,{integer,50},{x,1}}.
    {line,[{location,"lc_tests.erl",309}]}.
    {call,2,{f,9}}.
    {call_last,1,{f,136},0}.


{function, intermediary_list2, 1, 102}.
  {label,101}.
    {line,[{location,"lc_tests.erl",312}]}.
    {func_info,{atom,lc_tests},{atom,intermediary_list2},1}.
  {label,102}.
    {test,is_integer,{f,101},[{x,0}]}.
    {allocate,0,1}.
    {move,{integer,50},{x,1}}.
    {line,[{location,"lc_tests.erl",313}]}.
    {call,2,{f,9}}.
    {line,[{location,"lc_tests.erl",314}]}.
    {call,1,{f,132}}.
    {call_last,1,{f,129},0}.


{function, intermediary_list3, 1, 104}.
  {label,103}.
    {line,[{location,"lc_tests.erl",317}]}.
    {func_info,{atom,lc_tests},{atom,intermediary_list3},1}.
  {label,104}.
    {test,is_integer,{f,103},[{x,0}]}.
    {allocate,0,1}.
    {move,{integer,50},{x,1}}.
    {line,[{location,"lc_tests.erl",318}]}.
    {call,2,{f,9}}.
    {line,[{location,"lc_tests.erl",319}]}.
    {call,1,{f,126}}.
    {line,[{location,"lc_tests.erl",320}]}.
    {call,1,{f,123}}.
    {call_last,1,{f,119},0}.


{function, nested_lc_gen, 1, 106}.
  {label,105}.
    {line,[{location,"lc_tests.erl",327}]}.
    {func_info,{atom,lc_tests},{atom,nested_lc_gen},1}.
  {label,106}.
    {test,is_list,{f,105},[{x,0}]}.
    {allocate,0,1}.
    {line,[{location,"lc_tests.erl",329}]}.
    {call,1,{f,112}}.
    {call_last,1,{f,116},0}.


{function, module_info, 0, 108}.
  {label,107}.
    {line,[]}.
    {func_info,{atom,lc_tests},{atom,module_info},0}.
  {label,108}.
    {move,{atom,lc_tests},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 110}.
  {label,109}.
    {line,[]}.
    {func_info,{atom,lc_tests},{atom,module_info},1}.
  {label,110}.
    {move,{x,0},{x,1}}.
    {move,{atom,lc_tests},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.


{function, '-nested_lc_gen/1-lc$^0/1-1-', 1, 112}.
  {label,111}.
    {line,[{location,"lc_tests.erl",329}]}.
    {func_info,{atom,lc_tests},{atom,'-nested_lc_gen/1-lc$^0/1-1-'},1}.
  {label,112}.
    {test,is_nonempty_list,{f,114},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {gc_bif,'rem',{f,113},3,[{x,1},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,113},[{x,3},{integer,1}]}.
    {allocate,1,3}.
    {move,{x,2},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests.erl",329}]}.
    {call,1,{f,112}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,113}.
    {move,{x,2},{x,0}}.
    {call_only,1,{f,112}}.
  {label,114}.
    {test,is_nil,{f,111},[{x,0}]}.
    return.


{function, '-nested_lc_gen/1-lc$^1/1-0-', 1, 116}.
  {label,115}.
    {line,[{location,"lc_tests.erl",328}]}.
    {func_info,{atom,lc_tests},{atom,'-nested_lc_gen/1-lc$^1/1-0-'},1}.
  {label,116}.
    {test,is_nonempty_list,{f,117},[{x,0}]}.
    {allocate_zero,2,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {line,[{location,"lc_tests.erl",328}]}.
    {gc_bif,'*',{f,0},3,[{x,1},{x,1}],{y,0}}.
    {move,{x,2},{x,0}}.
    {move,{x,1},{y,1}}.
    {line,[{location,"lc_tests.erl",329}]}.
    {call,1,{f,116}}.
    {test_heap,6,1}.
    {put_tuple,3,{x,1}}.
    {put,{atom,test_record}}.
    {put,{y,1}}.
    {put,{y,0}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,117}.
    {test,is_nil,{f,115},[{x,0}]}.
    return.


{function, '-intermediary_list3/1-lc$^2/1-2-', 1, 119}.
  {label,118}.
    {line,[{location,"lc_tests.erl",321}]}.
    {func_info,{atom,lc_tests},{atom,'-intermediary_list3/1-lc$^2/1-2-'},1}.
  {label,119}.
    {test,is_nonempty_list,{f,121},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_tuple,{f,120},[{x,1}]}.
    {test,test_arity,{f,120},[{x,1},2]}.
    {get_tuple_element,{x,1},0,{x,3}}.
    {get_tuple_element,{x,1},1,{x,4}}.
    {test,is_tuple,{f,120},[{x,4}]}.
    {test,test_arity,{f,120},[{x,4},2]}.
    {get_tuple_element,{x,4},0,{x,5}}.
    {get_tuple_element,{x,4},1,{x,6}}.
    {test,is_tuple,{f,120},[{x,6}]}.
    {test,test_arity,{f,120},[{x,6},3]}.
    {get_tuple_element,{x,6},0,{x,7}}.
    {get_tuple_element,{x,6},1,{x,8}}.
    {get_tuple_element,{x,6},2,{x,9}}.
    {test,is_eq_exact,{f,120},[{x,7},{atom,test_record}]}.
    {allocate_zero,1,10}.
    {line,[{location,"lc_tests.erl",321}]}.
    {gc_bif,'*',{f,0},10,[{x,3},{x,5}],{x,0}}.
    {line,[{location,"lc_tests.erl",321}]}.
    {gc_bif,'*',{f,0},10,[{x,0},{x,8}],{x,0}}.
    {line,[{location,"lc_tests.erl",321}]}.
    {gc_bif,'*',{f,0},10,[{x,0},{x,9}],{y,0}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests.erl",321}]}.
    {call,1,{f,119}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,120}.
    {move,{x,2},{x,0}}.
    {call_only,1,{f,119}}.
  {label,121}.
    {test,is_nil,{f,118},[{x,0}]}.
    return.


{function, '-intermediary_list3/1-lc$^1/1-1-', 1, 123}.
  {label,122}.
    {line,[{location,"lc_tests.erl",320}]}.
    {func_info,{atom,lc_tests},{atom,'-intermediary_list3/1-lc$^1/1-1-'},1}.
  {label,123}.
    {test,is_nonempty_list,{f,124},[{x,0}]}.
    {allocate,2,1}.
    {get_list,{x,0},{y,1},{y,0}}.
    {move,{integer,100},{x,0}}.
    {line,[{location,"lc_tests.erl",320}]}.
    {call_ext,1,{extfunc,random,uniform,1}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests.erl",320}]}.
    {call,1,{f,123}}.
    {test_heap,5,1}.
    {put_tuple,2,{x,1}}.
    {put,{y,0}}.
    {put,{y,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,124}.
    {test,is_nil,{f,122},[{x,0}]}.
    return.


{function, '-intermediary_list3/1-lc$^0/1-0-', 1, 126}.
  {label,125}.
    {line,[{location,"lc_tests.erl",319}]}.
    {func_info,{atom,lc_tests},{atom,'-intermediary_list3/1-lc$^0/1-0-'},1}.
  {label,126}.
    {test,is_nonempty_list,{f,127},[{x,0}]}.
    {allocate,2,1}.
    {get_list,{x,0},{y,1},{y,0}}.
    {move,{integer,50},{x,0}}.
    {line,[{location,"lc_tests.erl",319}]}.
    {call_ext,1,{extfunc,random,uniform,1}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests.erl",319}]}.
    {call,1,{f,126}}.
    {test_heap,5,1}.
    {put_tuple,2,{x,1}}.
    {put,{y,0}}.
    {put,{y,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,127}.
    {test,is_nil,{f,125},[{x,0}]}.
    return.


{function, '-intermediary_list2/1-lc$^1/1-1-', 1, 129}.
  {label,128}.
    {line,[{location,"lc_tests.erl",315}]}.
    {func_info,{atom,lc_tests},{atom,'-intermediary_list2/1-lc$^1/1-1-'},1}.
  {label,129}.
    {test,is_nonempty_list,{f,130},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {line,[{location,"lc_tests.erl",315}]}.
    {gc_bif,'*',{f,0},3,[{x,1},{x,1}],{y,0}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests.erl",315}]}.
    {call,1,{f,129}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,130}.
    {test,is_nil,{f,128},[{x,0}]}.
    return.


{function, '-intermediary_list2/1-lc$^0/1-0-', 1, 132}.
  {label,131}.
    {line,[{location,"lc_tests.erl",314}]}.
    {func_info,{atom,lc_tests},{atom,'-intermediary_list2/1-lc$^0/1-0-'},1}.
  {label,132}.
    {test,is_nonempty_list,{f,133},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_tuple,{f,134},[{x,1}]}.
    {test,test_arity,{f,134},[{x,1},3]}.
    {get_tuple_element,{x,1},0,{x,0}}.
    {get_tuple_element,{x,1},2,{x,3}}.
    {test,is_eq_exact,{f,134},[{x,0},{atom,test_record}]}.
    {move,{x,2},{x,0}}.
    {move,{x,3},{y,0}}.
    {line,[{location,"lc_tests.erl",314}]}.
    {call,1,{f,132}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,133}.
    {test,is_nil,{f,131},[{x,0}]}.
    return.
  {label,134}.
    {move,{literal,{badrecord,test_record}},{x,0}}.
    {line,[{location,"lc_tests.erl",314}]}.
    {call_ext,1,{extfunc,erlang,error,1}}.


{function, '-intermediary_list/1-lc$^0/1-0-', 1, 136}.
  {label,135}.
    {line,[{location,"lc_tests.erl",310}]}.
    {func_info,{atom,lc_tests},{atom,'-intermediary_list/1-lc$^0/1-0-'},1}.
  {label,136}.
    {test,is_nonempty_list,{f,137},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_tuple,{f,138},[{x,1}]}.
    {test,test_arity,{f,138},[{x,1},3]}.
    {get_tuple_element,{x,1},0,{x,0}}.
    {get_tuple_element,{x,1},2,{x,3}}.
    {test,is_eq_exact,{f,138},[{x,0},{atom,test_record}]}.
    {move,{x,2},{x,0}}.
    {move,{x,3},{y,0}}.
    {line,[{location,"lc_tests.erl",310}]}.
    {call,1,{f,136}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,137}.
    {test,is_nil,{f,135},[{x,0}]}.
    return.
  {label,138}.
    {move,{literal,{badrecord,test_record}},{x,0}}.
    {line,[{location,"lc_tests.erl",310}]}.
    {call_ext,1,{extfunc,erlang,error,1}}.


{function, '-make_n/2-lc$^0/1-0-', 2, 140}.
  {label,139}.
    {line,[{location,"lc_tests.erl",305}]}.
    {func_info,{atom,lc_tests},{atom,'-make_n/2-lc$^0/1-0-'},2}.
  {label,140}.
    {test,is_nonempty_list,{f,141},[{x,0}]}.
    {allocate,1,2}.
    {get_list,{x,0},{x,2},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests.erl",305}]}.
    {call,2,{f,140}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,141}.
    {test,is_nil,{f,142},[{x,0}]}.
    return.
  {label,142}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests.erl",305}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-sorted_gen/1-fun-0-', 2, 144}.
  {label,143}.
    {line,[{location,"lc_tests.erl",296}]}.
    {func_info,{atom,lc_tests},{atom,'-sorted_gen/1-fun-0-'},2}.
  {label,144}.
    {test,is_tuple,{f,145},[{x,0}]}.
    {test,test_arity,{f,145},[{x,0},3]}.
    {get_tuple_element,{x,0},0,{x,2}}.
    {get_tuple_element,{x,0},1,{x,3}}.
    {test,is_eq_exact,{f,145},[{x,2},{atom,test_record}]}.
    {test,is_tuple,{f,145},[{x,1}]}.
    {test,test_arity,{f,145},[{x,1},3]}.
    {get_tuple_element,{x,1},0,{x,0}}.
    {get_tuple_element,{x,1},1,{x,2}}.
    {test,is_eq_exact,{f,145},[{x,0},{atom,test_record}]}.
    {bif,'<',{f,0},[{x,3},{x,2}],{x,0}}.
    return.
  {label,145}.
    {move,{literal,{badrecord,test_record}},{x,0}}.
    {line,[{location,"lc_tests.erl",297}]}.
    {call_ext,1,{extfunc,erlang,error,1}}.


{function, '-sorted_gen/1-lc$^0/1-0-', 1, 147}.
  {label,146}.
    {line,[{location,"lc_tests.erl",295}]}.
    {func_info,{atom,lc_tests},{atom,'-sorted_gen/1-lc$^0/1-0-'},1}.
  {label,147}.
    {test,is_nonempty_list,{f,148},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_tuple,{f,149},[{x,1}]}.
    {test,test_arity,{f,149},[{x,1},3]}.
    {get_tuple_element,{x,1},0,{x,0}}.
    {get_tuple_element,{x,1},1,{x,3}}.
    {test,is_eq_exact,{f,149},[{x,0},{atom,test_record}]}.
    {move,{x,2},{x,0}}.
    {move,{x,3},{y,0}}.
    {line,[{location,"lc_tests.erl",296}]}.
    {call,1,{f,147}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,148}.
    {test,is_nil,{f,146},[{x,0}]}.
    return.
  {label,149}.
    {move,{literal,{badrecord,test_record}},{x,0}}.
    {line,[{location,"lc_tests.erl",295}]}.
    {call_ext,1,{extfunc,erlang,error,1}}.


{function, '-function_param_lc/2-lc$^0/1-0-', 2, 151}.
  {label,150}.
    {line,[{location,"lc_tests.erl",291}]}.
    {func_info,{atom,lc_tests},{atom,'-function_param_lc/2-lc$^0/1-0-'},2}.
  {label,151}.
    {test,is_nonempty_list,{f,152},[{x,0}]}.
    {allocate,2,2}.
    {get_list,{x,0},{y,0},{x,0}}.
    {move,{x,1},{y,1}}.
    {line,[{location,"lc_tests.erl",291}]}.
    {call,2,{f,151}}.
    {test_heap,6,1}.
    {put_list,{y,0},nil,{x,1}}.
    {put_list,{y,1},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,152}.
    {test,is_nil,{f,153},[{x,0}]}.
    return.
  {label,153}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests.erl",291}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-record_extract/1-lc$^0/1-0-', 1, 155}.
  {label,154}.
    {line,[{location,"lc_tests.erl",287}]}.
    {func_info,{atom,lc_tests},{atom,'-record_extract/1-lc$^0/1-0-'},1}.
  {label,155}.
    {test,is_nonempty_list,{f,156},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_tuple,{f,157},[{x,1}]}.
    {test,test_arity,{f,157},[{x,1},3]}.
    {get_tuple_element,{x,1},0,{x,0}}.
    {get_tuple_element,{x,1},1,{x,3}}.
    {test,is_eq_exact,{f,157},[{x,0},{atom,test_record}]}.
    {move,{x,2},{x,0}}.
    {move,{x,3},{y,0}}.
    {line,[{location,"lc_tests.erl",287}]}.
    {call,1,{f,155}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,156}.
    {test,is_nil,{f,154},[{x,0}]}.
    return.
  {label,157}.
    {move,{literal,{badrecord,test_record}},{x,0}}.
    {line,[{location,"lc_tests.erl",287}]}.
    {call_ext,1,{extfunc,erlang,error,1}}.


{function, '-single_filter/1-lc$^0/1-0-', 1, 159}.
  {label,158}.
    {line,[{location,"lc_tests.erl",281}]}.
    {func_info,{atom,lc_tests},{atom,'-single_filter/1-lc$^0/1-0-'},1}.
  {label,159}.
    {test,is_nonempty_list,{f,161},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_integer,{f,160},[{x,1}]}.
    {allocate,1,3}.
    {move,{x,2},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests.erl",281}]}.
    {call,1,{f,159}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,160}.
    {move,{x,2},{x,0}}.
    {call_only,1,{f,159}}.
  {label,161}.
    {test,is_nil,{f,158},[{x,0}]}.
    return.


{function, '-unused/0-lc$^0/1-0-', 1, 163}.
  {label,162}.
    {line,[{location,"lc_tests.erl",274}]}.
    {func_info,{atom,lc_tests},{atom,'-unused/0-lc$^0/1-0-'},1}.
  {label,163}.
    {test,is_nonempty_list,{f,164},[{x,0}]}.
    {allocate,1,1}.
    {get_list,{x,0},{x,1},{y,0}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests.erl",274}]}.
    {call_ext,1,{extfunc,timer,sleep,1}}.
    {move,{y,0},{x,0}}.
    {call_last,1,{f,163},1}.
  {label,164}.
    {test,is_nil,{f,162},[{x,0}]}.
    return.


{function, '-pen2_lazy_cps/1-anonymous-5-', 3, 166}.
  {label,165}.
    {line,[]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_lazy_cps/1-anonymous-5-'},3}.
  {label,166}.
    {call_only,3,{f,168}}.


{function, '-pen2_lazy_cps/1-Red_xs/1-1-', 3, 168}.
  {label,167}.
    {line,[{location,"lc_tests.erl",264}]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_lazy_cps/1-Red_xs/1-1-'},3}.
  {label,168}.
    {allocate,3,3}.
    {move,{x,0},{y,2}}.
    {move,{x,1},{x,0}}.
    {move,{x,2},{x,1}}.
    {move,{x,0},{y,0}}.
    {move,{x,1},{y,1}}.
    {make_fun2,{f,173},0,0,2}.
    {test,is_nonempty_list,{f,170},[{y,2}]}.
    {get_list,{y,2},{x,1},{x,2}}.
    {gc_bif,'rem',{f,169},3,[{x,1},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,169},[{x,3},{integer,0}]}.
    {move,{x,0},{x,2}}.
    {move,{y,0},{x,1}}.
    {move,{y,1},{x,3}}.
    {move,{y,2},{x,0}}.
    {line,[{location,"lc_tests.erl",265}]}.
    {call_fun,3}.
    {deallocate,3}.
    return.
  {label,169}.
    {move,{x,0},{y,2}}.
    {trim,2,1}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests.erl",266}]}.
    {call_fun,0}.
    {move,{y,0},{x,1}}.
    {line,[{location,"lc_tests.erl",266}]}.
    {call_fun,1}.
    {deallocate,1}.
    return.
  {label,170}.
    {test,is_nil,{f,171},[{y,2}]}.
    {move,nil,{x,0}}.
    {deallocate,3}.
    return.
  {label,171}.
    {test_heap,2,0}.
    {put_tuple,1,{x,1}}.
    {put,{y,2}}.
    {line,[{location,"lc_tests.erl",264}]}.
    {case_end,{x,1}}.


{function, '-pen2_lazy_cps/1-anonymous-4-', 3, 173}.
  {label,172}.
    {line,[]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_lazy_cps/1-anonymous-4-'},3}.
  {label,173}.
    {call_only,3,{f,168}}.


{function, '-pen2_lazy_cps/1-anonymous-3-', 3, 175}.
  {label,174}.
    {line,[]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_lazy_cps/1-anonymous-3-'},3}.
  {label,175}.
    {call_only,3,{f,177}}.


{function, '-pen2_lazy_cps/1-Red_ys/3-0-', 3, 177}.
  {label,176}.
    {line,[{location,"lc_tests.erl",259}]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_lazy_cps/1-Red_ys/3-0-'},3}.
  {label,177}.
    {allocate,5,3}.
    {init,{y,0}}.
    {init,{y,1}}.
    {move,{x,2},{y,2}}.
    {move,{x,1},{y,3}}.
    {move,{x,0},{y,4}}.
    {make_fun2,{f,182},0,0,0}.
    {test,is_nonempty_list,{f,180},[{y,4}]}.
    {get_list,{y,4},{x,1},{x,2}}.
    {test,is_nonempty_list,{f,179},[{y,3}]}.
    {get_list,{y,3},{x,3},{x,4}}.
    {gc_bif,'rem',{f,178},5,[{x,3},{integer,2}],{x,2}}.
    {test,is_eq_exact,{f,178},[{x,2},{integer,1}]}.
    {gc_bif,'+',{f,178},5,[{x,1},{x,3}],{x,2}}.
    {gc_bif,'rem',{f,178},5,[{x,2},{integer,5}],{x,2}}.
    {bif,'=/=',{f,178},[{x,2},{integer,0}],{x,2}}.
    {move,{x,2},{x,5}}.
    {test,is_eq_exact,{f,178},[{x,5},{atom,true}]}.
    {move,{x,0},{y,0}}.
    {move,{x,4},{x,0}}.
    {move,{x,3},{y,1}}.
    {move,{x,1},{y,3}}.
    {line,[{location,"lc_tests.erl",261}]}.
    {call_fun,0}.
    {move,{y,2},{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,3}}.
    {move,{y,4},{x,0}}.
    {move,{y,1},{y,4}}.
    {trim,3,2}.
    {line,[{location,"lc_tests.erl",261}]}.
    {call_fun,3}.
    {test_heap,6,1}.
    {put_list,{y,1},nil,{x,1}}.
    {put_list,{y,0},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,178}.
    {move,{x,0},{y,3}}.
    {move,{x,4},{x,0}}.
    {line,[{location,"lc_tests.erl",262}]}.
    {call_fun,0}.
    {move,{y,2},{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{y,3},{x,3}}.
    {move,{y,4},{x,0}}.
    {line,[{location,"lc_tests.erl",262}]}.
    {call_fun,3}.
    {deallocate,5}.
    return.
  {label,179}.
    {test,is_nil,{f,180},[{y,3}]}.
    {kill,{y,3}}.
    {kill,{y,4}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests.erl",263}]}.
    {call_fun,0}.
    {move,{y,2},{x,1}}.
    {line,[{location,"lc_tests.erl",263}]}.
    {call_fun,1}.
    {deallocate,5}.
    return.
  {label,180}.
    {test_heap,4,0}.
    {put_tuple,3,{x,1}}.
    {put,{y,4}}.
    {put,{y,3}}.
    {put,{y,2}}.
    {line,[{location,"lc_tests.erl",259}]}.
    {case_end,{x,1}}.


{function, '-pen2_lazy_cps/1-anonymous-2-', 3, 182}.
  {label,181}.
    {line,[]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_lazy_cps/1-anonymous-2-'},3}.
  {label,182}.
    {call_only,3,{f,177}}.


{function, '-pen2_lazy/1-anonymous-5-', 3, 184}.
  {label,183}.
    {line,[]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_lazy/1-anonymous-5-'},3}.
  {label,184}.
    {call_only,3,{f,186}}.


{function, '-pen2_lazy/1-Red_xs/1-1-', 3, 186}.
  {label,185}.
    {line,[{location,"lc_tests.erl",249}]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_lazy/1-Red_xs/1-1-'},3}.
  {label,186}.
    {allocate,3,3}.
    {move,{x,0},{y,2}}.
    {move,{x,1},{x,0}}.
    {move,{x,2},{x,1}}.
    {move,{x,0},{y,0}}.
    {move,{x,1},{y,1}}.
    {make_fun2,{f,191},0,0,2}.
    {test,is_nonempty_list,{f,188},[{y,2}]}.
    {get_list,{y,2},{x,1},{x,2}}.
    {gc_bif,'rem',{f,187},3,[{x,1},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,187},[{x,3},{integer,0}]}.
    {move,{x,0},{x,2}}.
    {move,{y,0},{x,1}}.
    {move,{y,1},{x,3}}.
    {move,{y,2},{x,0}}.
    {line,[{location,"lc_tests.erl",250}]}.
    {call_fun,3}.
    {deallocate,3}.
    return.
  {label,187}.
    {move,{x,0},{y,2}}.
    {trim,2,1}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests.erl",251}]}.
    {call_fun,0}.
    {move,{y,0},{x,1}}.
    {line,[{location,"lc_tests.erl",251}]}.
    {call_fun,1}.
    {deallocate,1}.
    return.
  {label,188}.
    {test,is_nil,{f,189},[{y,2}]}.
    {move,nil,{x,0}}.
    {deallocate,3}.
    return.
  {label,189}.
    {test_heap,2,0}.
    {put_tuple,1,{x,1}}.
    {put,{y,2}}.
    {line,[{location,"lc_tests.erl",249}]}.
    {case_end,{x,1}}.


{function, '-pen2_lazy/1-anonymous-4-', 3, 191}.
  {label,190}.
    {line,[]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_lazy/1-anonymous-4-'},3}.
  {label,191}.
    {call_only,3,{f,186}}.


{function, '-pen2_lazy/1-anonymous-3-', 3, 193}.
  {label,192}.
    {line,[]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_lazy/1-anonymous-3-'},3}.
  {label,193}.
    {call_only,3,{f,195}}.


{function, '-pen2_lazy/1-Red_ys/3-0-', 3, 195}.
  {label,194}.
    {line,[{location,"lc_tests.erl",244}]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_lazy/1-Red_ys/3-0-'},3}.
  {label,195}.
    {allocate,5,3}.
    {init,{y,0}}.
    {init,{y,1}}.
    {move,{x,2},{y,2}}.
    {move,{x,1},{y,3}}.
    {move,{x,0},{y,4}}.
    {make_fun2,{f,200},0,0,0}.
    {test,is_nonempty_list,{f,198},[{y,4}]}.
    {get_list,{y,4},{x,1},{x,2}}.
    {test,is_nonempty_list,{f,197},[{y,3}]}.
    {get_list,{y,3},{x,3},{x,4}}.
    {gc_bif,'rem',{f,196},5,[{x,3},{integer,2}],{x,2}}.
    {test,is_eq_exact,{f,196},[{x,2},{integer,1}]}.
    {gc_bif,'+',{f,196},5,[{x,1},{x,3}],{x,2}}.
    {gc_bif,'rem',{f,196},5,[{x,2},{integer,5}],{x,2}}.
    {bif,'=/=',{f,196},[{x,2},{integer,0}],{x,2}}.
    {move,{x,2},{x,5}}.
    {test,is_eq_exact,{f,196},[{x,5},{atom,true}]}.
    {move,{x,0},{y,0}}.
    {move,{x,4},{x,0}}.
    {move,{x,3},{y,1}}.
    {move,{x,1},{y,3}}.
    {line,[{location,"lc_tests.erl",246}]}.
    {call_fun,0}.
    {move,{y,2},{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,3}}.
    {move,{y,4},{x,0}}.
    {move,{y,1},{y,4}}.
    {trim,3,2}.
    {line,[{location,"lc_tests.erl",246}]}.
    {call_fun,3}.
    {test_heap,6,1}.
    {put_list,{y,1},nil,{x,1}}.
    {put_list,{y,0},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,196}.
    {move,{x,0},{y,3}}.
    {move,{x,4},{x,0}}.
    {line,[{location,"lc_tests.erl",247}]}.
    {call_fun,0}.
    {move,{y,2},{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{y,3},{x,3}}.
    {move,{y,4},{x,0}}.
    {line,[{location,"lc_tests.erl",247}]}.
    {call_fun,3}.
    {deallocate,5}.
    return.
  {label,197}.
    {test,is_nil,{f,198},[{y,3}]}.
    {kill,{y,3}}.
    {kill,{y,4}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests.erl",248}]}.
    {call_fun,0}.
    {move,{y,2},{x,1}}.
    {line,[{location,"lc_tests.erl",248}]}.
    {call_fun,1}.
    {deallocate,5}.
    return.
  {label,198}.
    {test_heap,4,0}.
    {put_tuple,3,{x,1}}.
    {put,{y,4}}.
    {put,{y,3}}.
    {put,{y,2}}.
    {line,[{location,"lc_tests.erl",244}]}.
    {case_end,{x,1}}.


{function, '-pen2_lazy/1-anonymous-2-', 3, 200}.
  {label,199}.
    {line,[]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_lazy/1-anonymous-2-'},3}.
  {label,200}.
    {call_only,3,{f,195}}.


{function, '-pen2_mr/1-anonymous-5-', 3, 202}.
  {label,201}.
    {line,[]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_mr/1-anonymous-5-'},3}.
  {label,202}.
    {call_only,3,{f,204}}.


{function, '-pen2_mr/1-Red_xs/1-1-', 3, 204}.
  {label,203}.
    {line,[{location,"lc_tests.erl",233}]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_mr/1-Red_xs/1-1-'},3}.
  {label,204}.
    {allocate,3,3}.
    {move,{x,0},{y,2}}.
    {move,{x,1},{x,0}}.
    {move,{x,2},{x,1}}.
    {move,{x,0},{y,0}}.
    {move,{x,1},{y,1}}.
    {make_fun2,{f,209},0,0,2}.
    {test,is_nonempty_list,{f,206},[{y,2}]}.
    {get_list,{y,2},{x,1},{x,2}}.
    {gc_bif,'rem',{f,205},3,[{x,1},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,205},[{x,3},{integer,0}]}.
    {move,{x,0},{x,2}}.
    {move,{y,0},{x,1}}.
    {move,{y,1},{x,3}}.
    {move,{y,2},{x,0}}.
    {line,[{location,"lc_tests.erl",234}]}.
    {call_fun,3}.
    {deallocate,3}.
    return.
  {label,205}.
    {move,{x,0},{x,1}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests.erl",235}]}.
    {call_fun,1}.
    {deallocate,3}.
    return.
  {label,206}.
    {test,is_nil,{f,207},[{y,2}]}.
    {move,nil,{x,0}}.
    {deallocate,3}.
    return.
  {label,207}.
    {test_heap,2,0}.
    {put_tuple,1,{x,1}}.
    {put,{y,2}}.
    {line,[{location,"lc_tests.erl",233}]}.
    {case_end,{x,1}}.


{function, '-pen2_mr/1-anonymous-4-', 3, 209}.
  {label,208}.
    {line,[]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_mr/1-anonymous-4-'},3}.
  {label,209}.
    {call_only,3,{f,204}}.


{function, '-pen2_mr/1-anonymous-3-', 3, 211}.
  {label,210}.
    {line,[]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_mr/1-anonymous-3-'},3}.
  {label,211}.
    {call_only,3,{f,213}}.


{function, '-pen2_mr/1-Red_ys/3-0-', 3, 213}.
  {label,212}.
    {line,[{location,"lc_tests.erl",227}]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_mr/1-Red_ys/3-0-'},3}.
  {label,213}.
    {allocate,3,3}.
    {move,{x,1},{y,0}}.
    {move,{x,0},{y,1}}.
    {move,{x,2},{y,2}}.
    {make_fun2,{f,218},0,0,0}.
    {test,is_nonempty_list,{f,216},[{y,1}]}.
    {get_list,{y,1},{x,1},{x,2}}.
    {test,is_nonempty_list,{f,215},[{y,0}]}.
    {get_list,{y,0},{x,3},{x,4}}.
    {gc_bif,'rem',{f,214},5,[{x,3},{integer,2}],{x,2}}.
    {test,is_eq_exact,{f,214},[{x,2},{integer,1}]}.
    {gc_bif,'+',{f,214},5,[{x,1},{x,3}],{x,2}}.
    {gc_bif,'rem',{f,214},5,[{x,2},{integer,5}],{x,2}}.
    {bif,'=/=',{f,214},[{x,2},{integer,0}],{x,2}}.
    {move,{x,2},{x,5}}.
    {test,is_eq_exact,{f,214},[{x,5},{atom,true}]}.
    {move,{x,3},{x,6}}.
    {move,{x,1},{x,5}}.
    {move,{y,2},{x,2}}.
    {move,{x,4},{x,1}}.
    {move,{x,0},{x,3}}.
    {move,{y,1},{x,0}}.
    {move,{x,6},{y,1}}.
    {move,{x,5},{y,2}}.
    {trim,1,2}.
    {line,[{location,"lc_tests.erl",229}]}.
    {call_fun,3}.
    {test_heap,6,1}.
    {put_list,{y,0},nil,{x,1}}.
    {put_list,{y,1},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,214}.
    {move,{y,2},{x,2}}.
    {move,{x,4},{x,1}}.
    {move,{x,0},{x,3}}.
    {move,{y,1},{x,0}}.
    {line,[{location,"lc_tests.erl",230}]}.
    {call_fun,3}.
    {deallocate,3}.
    return.
  {label,215}.
    {test,is_nil,{f,216},[{y,0}]}.
    {move,{y,2},{x,1}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests.erl",232}]}.
    {call_fun,1}.
    {deallocate,3}.
    return.
  {label,216}.
    {test_heap,4,0}.
    {put_tuple,3,{x,1}}.
    {put,{y,1}}.
    {put,{y,0}}.
    {put,{y,2}}.
    {line,[{location,"lc_tests.erl",227}]}.
    {case_end,{x,1}}.


{function, '-pen2_mr/1-anonymous-2-', 3, 218}.
  {label,217}.
    {line,[]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_mr/1-anonymous-2-'},3}.
  {label,218}.
    {call_only,3,{f,213}}.


{function, '-pen2_listsfuns/1-fun-1-', 1, 220}.
  {label,219}.
    {line,[{location,"lc_tests.erl",221}]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_listsfuns/1-fun-1-'},1}.
  {label,220}.
    {line,[{location,"lc_tests.erl",221}]}.
    {gc_bif,'rem',{f,0},1,[{x,0},{integer,2}],{x,0}}.
    {bif,'=:=',{f,0},[{x,0},{integer,0}],{x,0}}.
    return.


{function, '-pen2_listsfuns/1-lc$^0/1-0-', 2, 222}.
  {label,221}.
    {line,[{location,"lc_tests.erl",221}]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_listsfuns/1-lc$^0/1-0-'},2}.
  {label,222}.
    {test,is_nonempty_list,{f,223},[{x,0}]}.
    {allocate,3,2}.
    {get_list,{x,0},{y,1},{y,2}}.
    {move,{x,1},{y,0}}.
    {make_fun2,{f,226},0,0,0}.
    {move,{y,0},{x,1}}.
    {line,[{location,"lc_tests.erl",222}]}.
    {call_ext,2,{extfunc,lists,filter,2}}.
    {move,{y,1},{x,2}}.
    {move,{y,2},{x,3}}.
    {move,{y,0},{x,1}}.
    {call_last,4,{f,228},3}.
  {label,223}.
    {test,is_nil,{f,224},[{x,0}]}.
    return.
  {label,224}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests.erl",221}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pen2_listsfuns/1-fun-2-', 1, 226}.
  {label,225}.
    {line,[{location,"lc_tests.erl",222}]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_listsfuns/1-fun-2-'},1}.
  {label,226}.
    {line,[{location,"lc_tests.erl",222}]}.
    {gc_bif,'rem',{f,0},1,[{x,0},{integer,2}],{x,0}}.
    {bif,'=:=',{f,0},[{x,0},{integer,1}],{x,0}}.
    return.


{function, '-pen2_listsfuns/1-lc$^1/1-1-', 4, 228}.
  {label,227}.
    {line,[{location,"lc_tests.erl",221}]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_listsfuns/1-lc$^1/1-1-'},4}.
  {label,228}.
    {test,is_nonempty_list,{f,229},[{x,0}]}.
    {allocate,2,4}.
    {get_list,{x,0},{y,0},{x,0}}.
    {move,{x,2},{y,1}}.
    {line,[{location,"lc_tests.erl",222}]}.
    {call,4,{f,228}}.
    {test_heap,6,1}.
    {put_list,{y,0},nil,{x,1}}.
    {put_list,{y,1},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,229}.
    {test,is_nil,{f,230},[{x,0}]}.
    {move,{x,3},{x,0}}.
    {call_only,2,{f,222}}.
  {label,230}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests.erl",221}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pen2_listsfuns/1-fun-0-', 1, 232}.
  {label,231}.
    {line,[{location,"lc_tests.erl",220}]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_listsfuns/1-fun-0-'},1}.
  {label,232}.
    {test,is_nonempty_list,{f,231},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_nonempty_list,{f,231},[{x,2}]}.
    {get_list,{x,2},{x,3},{x,4}}.
    {test,is_nil,{f,231},[{x,4}]}.
    {line,[{location,"lc_tests.erl",220}]}.
    {gc_bif,'+',{f,0},4,[{x,1},{x,3}],{x,0}}.
    {line,[{location,"lc_tests.erl",220}]}.
    {gc_bif,'rem',{f,0},1,[{x,0},{integer,5}],{x,0}}.
    {bif,'=/=',{f,0},[{x,0},{integer,0}],{x,0}}.
    return.


{function, '-pen2_foldr/1-fun-1-', 3, 234}.
  {label,233}.
    {line,[{location,"lc_tests.erl",205}]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_foldr/1-fun-1-'},3}.
  {label,234}.
    {gc_bif,'rem',{f,235},3,[{x,0},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,235},[{x,3},{integer,0}]}.
    {allocate,2,3}.
    {move,{x,2},{y,0}}.
    {move,{x,1},{y,1}}.
    {make_fun2,{f,237},0,0,1}.
    {move,{y,0},{x,2}}.
    {move,nil,{x,1}}.
    {trim,1,1}.
    {line,[{location,"lc_tests.erl",207}]}.
    {call_ext,3,{extfunc,lists,foldr,3}}.
    {move,{y,0},{x,1}}.
    {line,[{location,"lc_tests.erl",213}]}.
    {call_ext_last,2,{extfunc,erlang,'++',2},1}.
  {label,235}.
    {move,{x,1},{x,0}}.
    return.


{function, '-pen2_foldr/1-fun-0-', 3, 237}.
  {label,236}.
    {line,[{location,"lc_tests.erl",208}]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_foldr/1-fun-0-'},3}.
  {label,237}.
    {gc_bif,'rem',{f,238},3,[{x,0},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,238},[{x,3},{integer,1}]}.
    {gc_bif,'+',{f,238},3,[{x,2},{x,0}],{x,3}}.
    {gc_bif,'rem',{f,238},4,[{x,3},{integer,5}],{x,3}}.
    {test,is_ne_exact,{f,238},[{x,3},{integer,0}]}.
    {test_heap,6,3}.
    {put_list,{x,0},nil,{x,0}}.
    {put_list,{x,2},{x,0},{x,2}}.
    {put_list,{x,2},{x,1},{x,0}}.
    return.
  {label,238}.
    {move,{x,1},{x,0}}.
    return.


{function, '-pen2_foldl/1-fun-1-', 3, 240}.
  {label,239}.
    {line,[{location,"lc_tests.erl",189}]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_foldl/1-fun-1-'},3}.
  {label,240}.
    {gc_bif,'rem',{f,241},3,[{x,0},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,241},[{x,3},{integer,0}]}.
    {allocate,2,3}.
    {move,{x,2},{y,0}}.
    {move,{x,1},{y,1}}.
    {make_fun2,{f,243},0,0,1}.
    {move,{y,0},{x,2}}.
    {move,nil,{x,1}}.
    {trim,1,1}.
    {line,[{location,"lc_tests.erl",191}]}.
    {call_ext,3,{extfunc,lists,foldl,3}}.
    {move,{y,0},{x,1}}.
    {line,[{location,"lc_tests.erl",197}]}.
    {call_ext_last,2,{extfunc,erlang,'++',2},1}.
  {label,241}.
    {move,{x,1},{x,0}}.
    return.


{function, '-pen2_foldl/1-fun-0-', 3, 243}.
  {label,242}.
    {line,[{location,"lc_tests.erl",192}]}.
    {func_info,{atom,lc_tests},{atom,'-pen2_foldl/1-fun-0-'},3}.
  {label,243}.
    {gc_bif,'rem',{f,244},3,[{x,0},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,244},[{x,3},{integer,1}]}.
    {gc_bif,'+',{f,244},3,[{x,2},{x,0}],{x,3}}.
    {gc_bif,'rem',{f,244},4,[{x,3},{integer,5}],{x,3}}.
    {test,is_ne_exact,{f,244},[{x,3},{integer,0}]}.
    {test_heap,6,3}.
    {put_list,{x,0},nil,{x,0}}.
    {put_list,{x,2},{x,0},{x,2}}.
    {put_list,{x,2},{x,1},{x,0}}.
    return.
  {label,244}.
    {move,{x,1},{x,0}}.
    return.


{function, '-pen2/1-lc$^0/1-0-', 2, 246}.
  {label,245}.
    {line,[{location,"lc_tests.erl",167}]}.
    {func_info,{atom,lc_tests},{atom,'-pen2/1-lc$^0/1-0-'},2}.
  {label,246}.
    {test,is_nonempty_list,{f,248},[{x,0}]}.
    {get_list,{x,0},{x,2},{x,3}}.
    {gc_bif,'rem',{f,247},4,[{x,2},{integer,2}],{x,4}}.
    {test,is_eq_exact,{f,247},[{x,4},{integer,0}]}.
    {allocate,3,4}.
    {move,{integer,1},{x,0}}.
    {move,{x,1},{y,0}}.
    {move,{x,3},{y,1}}.
    {move,{x,2},{y,2}}.
    {line,[{location,"lc_tests.erl",169}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,1},{x,2}}.
    {move,{y,2},{x,3}}.
    {move,{y,0},{x,1}}.
    {call_last,4,{f,251},3}.
  {label,247}.
    {move,{x,3},{x,0}}.
    {call_only,2,{f,246}}.
  {label,248}.
    {test,is_nil,{f,249},[{x,0}]}.
    return.
  {label,249}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests.erl",167}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pen2/1-lc$^1/1-1-', 4, 251}.
  {label,250}.
    {line,[{location,"lc_tests.erl",167}]}.
    {func_info,{atom,lc_tests},{atom,'-pen2/1-lc$^1/1-1-'},4}.
  {label,251}.
    {test,is_nonempty_list,{f,253},[{x,0}]}.
    {get_list,{x,0},{x,4},{x,5}}.
    {gc_bif,'rem',{f,252},6,[{x,4},{integer,2}],{x,6}}.
    {gc_bif,'+',{f,252},7,[{x,3},{x,4}],{x,7}}.
    {gc_bif,'rem',{f,252},8,[{x,7},{integer,5}],{x,8}}.
    {test,is_eq_exact,{f,252},[{x,6},{integer,1}]}.
    {test,is_ne_exact,{f,252},[{x,8},{integer,0}]}.
    {allocate,2,6}.
    {move,{x,5},{x,0}}.
    {move,{x,4},{y,0}}.
    {move,{x,3},{y,1}}.
    {line,[{location,"lc_tests.erl",169}]}.
    {call,4,{f,251}}.
    {test_heap,6,1}.
    {put_list,{y,0},nil,{x,1}}.
    {put_list,{y,1},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,252}.
    {move,{x,5},{x,0}}.
    {call_only,4,{f,251}}.
  {label,253}.
    {test,is_nil,{f,254},[{x,0}]}.
    {move,{x,2},{x,0}}.
    {call_only,2,{f,246}}.
  {label,254}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests.erl",167}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pen/1-lc$^0/1-0-', 2, 256}.
  {label,255}.
    {line,[{location,"lc_tests.erl",160}]}.
    {func_info,{atom,lc_tests},{atom,'-pen/1-lc$^0/1-0-'},2}.
  {label,256}.
    {test,is_nonempty_list,{f,258},[{x,0}]}.
    {get_list,{x,0},{x,2},{x,3}}.
    {gc_bif,'rem',{f,257},4,[{x,2},{integer,2}],{x,4}}.
    {test,is_eq_exact,{f,257},[{x,4},{integer,0}]}.
    {allocate,3,4}.
    {move,{integer,1},{x,0}}.
    {move,{x,1},{y,0}}.
    {move,{x,3},{y,1}}.
    {move,{x,2},{y,2}}.
    {line,[{location,"lc_tests.erl",162}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,1},{x,2}}.
    {move,{y,2},{x,3}}.
    {move,{y,0},{x,1}}.
    {call_last,4,{f,261},3}.
  {label,257}.
    {move,{x,3},{x,0}}.
    {call_only,2,{f,256}}.
  {label,258}.
    {test,is_nil,{f,259},[{x,0}]}.
    return.
  {label,259}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests.erl",160}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pen/1-lc$^1/1-1-', 4, 261}.
  {label,260}.
    {line,[{location,"lc_tests.erl",160}]}.
    {func_info,{atom,lc_tests},{atom,'-pen/1-lc$^1/1-1-'},4}.
  {label,261}.
    {test,is_nonempty_list,{f,263},[{x,0}]}.
    {get_list,{x,0},{x,4},{x,5}}.
    {gc_bif,'rem',{f,262},6,[{x,4},{integer,2}],{x,6}}.
    {test,is_eq_exact,{f,262},[{x,6},{integer,1}]}.
    {allocate,2,6}.
    {move,{x,5},{x,0}}.
    {move,{x,4},{y,0}}.
    {move,{x,3},{y,1}}.
    {line,[{location,"lc_tests.erl",162}]}.
    {call,4,{f,261}}.
    {test_heap,6,1}.
    {put_list,{y,0},nil,{x,1}}.
    {put_list,{y,1},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,262}.
    {move,{x,5},{x,0}}.
    {call_only,4,{f,261}}.
  {label,263}.
    {test,is_nil,{f,264},[{x,0}]}.
    {move,{x,2},{x,0}}.
    {call_only,2,{f,256}}.
  {label,264}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests.erl",160}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-ses/1-lc$^0/1-0-', 1, 266}.
  {label,265}.
    {line,[{location,"lc_tests.erl",155}]}.
    {func_info,{atom,lc_tests},{atom,'-ses/1-lc$^0/1-0-'},1}.
  {label,266}.
    {test,is_nonempty_list,{f,268},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {gc_bif,'rem',{f,267},3,[{x,1},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,267},[{x,3},{integer,0}]}.
    {allocate_zero,1,3}.
    {line,[{location,"lc_tests.erl",155}]}.
    {gc_bif,'*',{f,0},3,[{x,1},{x,1}],{y,0}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests.erl",155}]}.
    {call,1,{f,266}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,267}.
    {move,{x,2},{x,0}}.
    {call_only,1,{f,266}}.
  {label,268}.
    {test,is_nil,{f,265},[{x,0}]}.
    return.


{function, '-ss/1-lc$^0/1-0-', 1, 270}.
  {label,269}.
    {line,[{location,"lc_tests.erl",151}]}.
    {func_info,{atom,lc_tests},{atom,'-ss/1-lc$^0/1-0-'},1}.
  {label,270}.
    {test,is_nonempty_list,{f,271},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {line,[{location,"lc_tests.erl",151}]}.
    {gc_bif,'*',{f,0},3,[{x,1},{x,1}],{y,0}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests.erl",151}]}.
    {call,1,{f,270}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,271}.
    {test,is_nil,{f,269},[{x,0}]}.
    return.


{function, '-all_tests/0-fun-20-', 0, 273}.
  {label,272}.
    {line,[{location,"lc_tests.erl",144}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-20-'},0}.
  {label,273}.
    {call_only,0,{f,88}}.


{function, '-all_tests/0-fun-19-', 1, 275}.
  {label,274}.
    {line,[{location,"lc_tests.erl",143}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-19-'},1}.
  {label,275}.
    {call_only,1,{f,106}}.


{function, '-all_tests/0-fun-18-', 1, 277}.
  {label,276}.
    {line,[{location,"lc_tests.erl",142}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-18-'},1}.
  {label,277}.
    {call_only,1,{f,104}}.


{function, '-all_tests/0-fun-17-', 1, 279}.
  {label,278}.
    {line,[{location,"lc_tests.erl",141}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-17-'},1}.
  {label,279}.
    {call_only,1,{f,102}}.


{function, '-all_tests/0-fun-16-', 1, 281}.
  {label,280}.
    {line,[{location,"lc_tests.erl",140}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-16-'},1}.
  {label,281}.
    {call_only,1,{f,100}}.


{function, '-all_tests/0-fun-15-', 2, 283}.
  {label,282}.
    {line,[{location,"lc_tests.erl",139}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-15-'},2}.
  {label,283}.
    {call_only,2,{f,98}}.


{function, '-all_tests/0-fun-14-', 1, 285}.
  {label,284}.
    {line,[{location,"lc_tests.erl",138}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-14-'},1}.
  {label,285}.
    {call_only,1,{f,96}}.


{function, '-all_tests/0-fun-13-', 2, 287}.
  {label,286}.
    {line,[{location,"lc_tests.erl",137}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-13-'},2}.
  {label,287}.
    {call_only,2,{f,94}}.


{function, '-all_tests/0-fun-12-', 1, 289}.
  {label,288}.
    {line,[{location,"lc_tests.erl",136}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-12-'},1}.
  {label,289}.
    {call_only,1,{f,92}}.


{function, '-all_tests/0-fun-11-', 1, 291}.
  {label,290}.
    {line,[{location,"lc_tests.erl",135}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-11-'},1}.
  {label,291}.
    {call_only,1,{f,90}}.


{function, '-all_tests/0-fun-10-', 1, 293}.
  {label,292}.
    {line,[{location,"lc_tests.erl",134}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-10-'},1}.
  {label,293}.
    {call_only,1,{f,86}}.


{function, '-all_tests/0-fun-9-', 1, 295}.
  {label,294}.
    {line,[{location,"lc_tests.erl",133}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-9-'},1}.
  {label,295}.
    {call_only,1,{f,84}}.


{function, '-all_tests/0-fun-8-', 1, 297}.
  {label,296}.
    {line,[{location,"lc_tests.erl",132}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-8-'},1}.
  {label,297}.
    {call_only,1,{f,82}}.


{function, '-all_tests/0-fun-7-', 1, 299}.
  {label,298}.
    {line,[{location,"lc_tests.erl",131}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-7-'},1}.
  {label,299}.
    {call_only,1,{f,80}}.


{function, '-all_tests/0-fun-6-', 1, 301}.
  {label,300}.
    {line,[{location,"lc_tests.erl",130}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-6-'},1}.
  {label,301}.
    {call_only,1,{f,76}}.


{function, '-all_tests/0-fun-5-', 1, 303}.
  {label,302}.
    {line,[{location,"lc_tests.erl",129}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-5-'},1}.
  {label,303}.
    {call_only,1,{f,78}}.


{function, '-all_tests/0-fun-4-', 1, 305}.
  {label,304}.
    {line,[{location,"lc_tests.erl",128}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-4-'},1}.
  {label,305}.
    {call_only,1,{f,69}}.


{function, '-all_tests/0-fun-3-', 1, 307}.
  {label,306}.
    {line,[{location,"lc_tests.erl",127}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-3-'},1}.
  {label,307}.
    {call_only,1,{f,67}}.


{function, '-all_tests/0-fun-2-', 1, 309}.
  {label,308}.
    {line,[{location,"lc_tests.erl",126}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-2-'},1}.
  {label,309}.
    {call_only,1,{f,65}}.


{function, '-all_tests/0-fun-1-', 1, 311}.
  {label,310}.
    {line,[{location,"lc_tests.erl",125}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-1-'},1}.
  {label,311}.
    {call_only,1,{f,63}}.


{function, '-all_tests/0-fun-0-', 1, 313}.
  {label,312}.
    {line,[{location,"lc_tests.erl",124}]}.
    {func_info,{atom,lc_tests},{atom,'-all_tests/0-fun-0-'},1}.
  {label,313}.
    {call_only,1,{f,61}}.


{function, '-run_tests/1-fun-0-', 0, 315}.
  {label,314}.
    {line,[{location,"lc_tests.erl",84}]}.
    {func_info,{atom,lc_tests},{atom,'-run_tests/1-fun-0-'},0}.
  {label,315}.
    {call_only,0,{f,40}}.


{function, '-run_tests/3-lc$^0/1-0-', 2, 317}.
  {label,316}.
    {line,[{location,"lc_tests.erl",76}]}.
    {func_info,{atom,lc_tests},{atom,'-run_tests/3-lc$^0/1-0-'},2}.
  {label,317}.
    {test,is_nonempty_list,{f,319},[{x,0}]}.
    {get_list,{x,0},{x,2},{x,3}}.
    {test,is_tuple,{f,318},[{x,2}]}.
    {test,test_arity,{f,318},[{x,2},4]}.
    {get_tuple_element,{x,2},0,{x,4}}.
    {get_tuple_element,{x,2},1,{x,5}}.
    {get_tuple_element,{x,2},2,{x,6}}.
    {get_tuple_element,{x,2},3,{x,7}}.
    {test,is_tuple,{f,318},[{x,6}]}.
    {test,test_arity,{f,318},[{x,6},2]}.
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
    {call,2,{f,317}}.
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
  {label,318}.
    {move,{x,3},{x,0}}.
    {call_only,2,{f,317}}.
  {label,319}.
    {test,is_nil,{f,320},[{x,0}]}.
    return.
  {label,320}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests.erl",76}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-test/3-fun-0-', 3, 322}.
  {label,321}.
    {line,[{location,"lc_tests.erl",58}]}.
    {func_info,{atom,lc_tests},{atom,'-test/3-fun-0-'},3}.
  {label,322}.
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


{function, '-lazy_cp_seq/3-fun-1-', 3, 324}.
  {label,323}.
    {line,[{location,"lc_tests.erl",46}]}.
    {func_info,{atom,lc_tests},{atom,'-lazy_cp_seq/3-fun-1-'},3}.
  {label,324}.
    {allocate_zero,2,3}.
    {line,[{location,"lc_tests.erl",46}]}.
    {gc_bif,'+',{f,0},3,[{x,2},{integer,1}],{y,1}}.
    {move,{x,1},{y,0}}.
    {move,{x,2},{x,1}}.
    {make_fun2,{f,326},0,0,2}.
    {move,{y,0},{x,1}}.
    {move,{x,0},{x,2}}.
    {move,{y,1},{x,0}}.
    {call_last,3,{f,22},2}.


{function, '-lazy_cp_seq/3-fun-0-', 3, 326}.
  {label,325}.
    {line,[{location,"lc_tests.erl",46}]}.
    {func_info,{atom,lc_tests},{atom,'-lazy_cp_seq/3-fun-0-'},3}.
  {label,326}.
    {move,{x,2},{x,3}}.
    {move,{x,1},{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{x,3},{x,0}}.
    {call_only,3,{f,18}}.


{function, '-lazy_cp_seq/2-fun-0-', 1, 328}.
  {label,327}.
    {line,[{location,"lc_tests.erl",43}]}.
    {func_info,{atom,lc_tests},{atom,'-lazy_cp_seq/2-fun-0-'},1}.
  {label,328}.
    return.


{function, '-lazy_seq/2-fun-0-', 2, 330}.
  {label,329}.
    {line,[{location,"lc_tests.erl",32}]}.
    {func_info,{atom,lc_tests},{atom,'-lazy_seq/2-fun-0-'},2}.
  {label,330}.
    {line,[{location,"lc_tests.erl",32}]}.
    {gc_bif,'+',{f,0},2,[{x,1},{integer,1}],{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{x,2},{x,0}}.
    {call_only,2,{f,12}}.


{function, '-random_bin/1-lc$^0/1-0-', 1, 332}.
  {label,331}.
    {line,[{location,"lc_tests.erl",18}]}.
    {func_info,{atom,lc_tests},{atom,'-random_bin/1-lc$^0/1-0-'},1}.
  {label,332}.
    {test,is_nonempty_list,{f,333},[{x,0}]}.
    {allocate,1,1}.
    {get_list,{x,0},{x,1},{y,0}}.
    {move,{integer,255},{x,0}}.
    {line,[{location,"lc_tests.erl",18}]}.
    {call_ext,1,{extfunc,random,uniform,1}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests.erl",18}]}.
    {call,1,{f,332}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,333}.
    {test,is_nil,{f,331},[{x,0}]}.
    return.
