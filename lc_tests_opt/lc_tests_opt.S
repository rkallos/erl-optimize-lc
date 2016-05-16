{module, lc_tests_opt}.  %% version = 0

{exports, [{all_tests,0},
           {function_param_lc,2},
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
           {run_tests,2},
           {ses,1},
           {single_filter,1},
           {sorted_gen,1},
           {ss,1},
           {stats,0},
           {test,3},
           {unused,0}]}.

{attributes, []}.

{labels, 270}.


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
    {call,1,{f,268}}.
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
    {make_fun2,{f,266},0,0,2}.
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
    {allocate_zero,1,3}.
    {bif,self,{f,0},[],{x,3}}.
    {move,{x,0},{y,0}}.
    {move,{x,1},{x,5}}.
    {move,{x,2},{x,1}}.
    {move,{x,5},{x,2}}.
    {move,{x,3},{x,0}}.
    {make_fun2,{f,264},0,0,3}.
    {line,[{location,"lc_tests_opt.erl",46}]}.
    {call_ext,1,{extfunc,erlang,spawn,1}}.
    {line,[{location,"lc_tests_opt.erl",54}]}.
  {label,21}.
    {loop_rec,{f,23},{x,0}}.
    {test,is_tuple,{f,22},[{x,0}]}.
    {test,test_arity,{f,22},[{x,0},3]}.
    {get_tuple_element,{x,0},0,{x,1}}.
    {get_tuple_element,{x,0},1,{x,2}}.
    {get_tuple_element,{x,0},2,{x,3}}.
    remove_message.
    {test_heap,7,4}.
    {put_tuple,2,{x,4}}.
    {put,{x,1}}.
    {put,{x,2}}.
    {put_tuple,3,{x,0}}.
    {put,{y,0}}.
    {put,{x,4}}.
    {put,{x,3}}.
    {deallocate,1}.
    return.
  {label,22}.
    {loop_rec_end,{f,21}}.
  {label,23}.
    {wait,{f,21}}.
  {label,24}.
    {test_heap,2,3}.
    {put_list,{x,2},nil,{x,2}}.
    {call_only,3,{f,20}}.


{function, run_tests, 2, 26}.
  {label,25}.
    {line,[{location,"lc_tests_opt.erl",60}]}.
    {func_info,{atom,lc_tests_opt},{atom,run_tests},2}.
  {label,26}.
    {test,is_nonempty_list,{f,27},[{x,0}]}.
    {get_list,{x,0},{x,2},{x,3}}.
    {test,is_tuple,{f,25},[{x,2}]}.
    {test,test_arity,{f,25},[{x,2},3]}.
    {allocate,2,4}.
    {get_tuple_element,{x,2},0,{x,0}}.
    {get_tuple_element,{x,2},1,{x,5}}.
    {get_tuple_element,{x,2},2,{x,2}}.
    {move,{x,1},{y,0}}.
    {move,{x,5},{x,1}}.
    {move,{x,3},{y,1}}.
    {line,[{location,"lc_tests_opt.erl",63}]}.
    {call,3,{f,20}}.
    {test_heap,2,1}.
    {put_list,{x,0},{y,0},{x,1}}.
    {move,{y,1},{x,0}}.
    {call_last,2,{f,26},2}.
  {label,27}.
    {test,is_nil,{f,25},[{x,0}]}.
    {move,{x,1},{x,0}}.
    return.


{function, run_tests, 1, 29}.
  {label,28}.
    {line,[{location,"lc_tests_opt.erl",66}]}.
    {func_info,{atom,lc_tests_opt},{atom,run_tests},1}.
  {label,29}.
    {test,is_list,{f,28},[{x,0}]}.
    {move,nil,{x,1}}.
    {call_only,2,{f,26}}.


{function, int_arg, 0, 31}.
  {label,30}.
    {line,[{location,"lc_tests_opt.erl",69}]}.
    {func_info,{atom,lc_tests_opt},{atom,int_arg},0}.
  {label,31}.
    {move,{integer,50},{x,0}}.
    return.


{function, list_arg, 0, 33}.
  {label,32}.
    {line,[{location,"lc_tests_opt.erl",71}]}.
    {func_info,{atom,lc_tests_opt},{atom,list_arg},0}.
  {label,33}.
    {move,{integer,50},{x,1}}.
    {move,{integer,50},{x,0}}.
    {call_only,2,{f,6}}.


{function, record_arg, 0, 35}.
  {label,34}.
    {line,[{location,"lc_tests_opt.erl",73}]}.
    {func_info,{atom,lc_tests_opt},{atom,record_arg},0}.
  {label,35}.
    {move,{integer,50},{x,1}}.
    {move,{integer,50},{x,0}}.
    {call_only,2,{f,9}}.


{function, all_tests, 0, 37}.
  {label,36}.
    {line,[{location,"lc_tests_opt.erl",76}]}.
    {func_info,{atom,lc_tests_opt},{atom,all_tests},0}.
  {label,37}.
    {allocate_zero,23,0}.
    {move,{integer,50},{x,1}}.
    {move,{integer,500},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",78}]}.
    {call,2,{f,6}}.
    {move,{x,0},{y,22}}.
    {move,{integer,50},{x,1}}.
    {move,{integer,500},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",79}]}.
    {call,2,{f,9}}.
    {move,{x,0},{y,21}}.
    {make_fun2,{f,262},0,0,0}.
    {move,{x,0},{y,20}}.
    {make_fun2,{f,260},0,0,0}.
    {move,{x,0},{y,19}}.
    {make_fun2,{f,258},0,0,0}.
    {move,{x,0},{y,18}}.
    {make_fun2,{f,256},0,0,0}.
    {move,{x,0},{y,17}}.
    {make_fun2,{f,254},0,0,0}.
    {move,{x,0},{y,16}}.
    {make_fun2,{f,252},0,0,0}.
    {move,{x,0},{y,15}}.
    {make_fun2,{f,250},0,0,0}.
    {move,{x,0},{y,14}}.
    {make_fun2,{f,248},0,0,0}.
    {move,{x,0},{y,13}}.
    {make_fun2,{f,246},0,0,0}.
    {move,{x,0},{y,12}}.
    {make_fun2,{f,244},0,0,0}.
    {test_heap,2,1}.
    {put_list,{y,22},nil,{y,10}}.
    {move,{x,0},{y,11}}.
    {make_fun2,{f,242},0,0,0}.
    {test_heap,2,1}.
    {put_list,{y,21},nil,{y,8}}.
    {move,{x,0},{y,9}}.
    {make_fun2,{f,240},0,0,0}.
    {test_heap,4,1}.
    {put_list,{y,22},nil,{x,1}}.
    {put_list,{atom,param},{x,1},{y,6}}.
    {move,{x,0},{y,7}}.
    {make_fun2,{f,238},0,0,0}.
    {test_heap,2,1}.
    {put_list,{y,21},nil,{y,5}}.
    {move,{x,0},{y,21}}.
    {make_fun2,{f,236},0,0,0}.
    {move,{x,0},{y,4}}.
    {make_fun2,{f,234},0,0,0}.
    {move,{x,0},{y,3}}.
    {make_fun2,{f,232},0,0,0}.
    {move,{x,0},{y,2}}.
    {make_fun2,{f,230},0,0,0}.
    {move,{x,0},{y,1}}.
    {make_fun2,{f,228},0,0,0}.
    {test_heap,2,1}.
    {put_list,{y,22},nil,{y,0}}.
    {move,{x,0},{y,22}}.
    {make_fun2,{f,226},0,0,0}.
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


{function, ss, 1, 39}.
  {label,38}.
    {line,[{location,"lc_tests_opt.erl",104}]}.
    {func_info,{atom,lc_tests_opt},{atom,ss},1}.
  {label,39}.
    {test,is_integer,{f,38},[{x,0}]}.
    {allocate,0,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",105}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {line,[{location,"lc_tests_opt.erl",105}]}.
    {call,1,{f,223}}.
    {line,[{location,"lc_tests_opt.erl",105}]}.
    {call_ext_last,1,{extfunc,lists,sum,1},0}.


{function, ses, 1, 41}.
  {label,40}.
    {line,[{location,"lc_tests_opt.erl",108}]}.
    {func_info,{atom,lc_tests_opt},{atom,ses},1}.
  {label,41}.
    {test,is_integer,{f,40},[{x,0}]}.
    {allocate,0,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",109}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {line,[{location,"lc_tests_opt.erl",109}]}.
    {call,1,{f,219}}.
    {line,[{location,"lc_tests_opt.erl",109}]}.
    {call_ext_last,1,{extfunc,lists,sum,1},0}.


{function, pen, 1, 43}.
  {label,42}.
    {line,[{location,"lc_tests_opt.erl",113}]}.
    {func_info,{atom,lc_tests_opt},{atom,pen},1}.
  {label,43}.
    {test,is_integer,{f,42},[{x,0}]}.
    {allocate,1,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",114}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,0},{x,1}}.
    {call_last,2,{f,209},1}.


{function, pen2, 1, 45}.
  {label,44}.
    {line,[{location,"lc_tests_opt.erl",120}]}.
    {func_info,{atom,lc_tests_opt},{atom,pen2},1}.
  {label,45}.
    {test,is_integer,{f,44},[{x,0}]}.
    {allocate,1,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",121}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,0},{x,1}}.
    {call_last,2,{f,199},1}.


{function, pen2_foldl, 1, 47}.
  {label,46}.
    {line,[{location,"lc_tests_opt.erl",128}]}.
    {func_info,{atom,lc_tests_opt},{atom,pen2_foldl},1}.
  {label,47}.
    {test,is_integer,{f,46},[{x,0}]}.
    {allocate_zero,1,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",129}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,193},0,0,1}.
    {move,{y,0},{x,2}}.
    {move,nil,{x,1}}.
    {trim,1,0}.
    {line,[{location,"lc_tests_opt.erl",130}]}.
    {call_ext,3,{extfunc,lists,foldl,3}}.
    {line,[{location,"lc_tests_opt.erl",130}]}.
    {call_ext_last,1,{extfunc,lists,reverse,1},0}.


{function, pen2_foldr, 1, 49}.
  {label,48}.
    {line,[{location,"lc_tests_opt.erl",144}]}.
    {func_info,{atom,lc_tests_opt},{atom,pen2_foldr},1}.
  {label,49}.
    {test,is_integer,{f,48},[{x,0}]}.
    {allocate_zero,1,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",145}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,187},0,0,1}.
    {move,{y,0},{x,2}}.
    {move,nil,{x,1}}.
    {line,[{location,"lc_tests_opt.erl",146}]}.
    {call_ext_last,3,{extfunc,lists,foldr,3},1}.


{function, pen2_listsfuns, 1, 51}.
  {label,50}.
    {line,[{location,"lc_tests_opt.erl",160}]}.
    {func_info,{atom,lc_tests_opt},{atom,pen2_listsfuns},1}.
  {label,51}.
    {test,is_integer,{f,50},[{x,0}]}.
    {allocate_zero,2,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",161}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{x,0},{y,1}}.
    {make_fun2,{f,185},0,0,0}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,173},0,0,0}.
    {move,{y,1},{x,1}}.
    {line,[{location,"lc_tests_opt.erl",163}]}.
    {call_ext,2,{extfunc,lists,filter,2}}.
    {move,{y,1},{x,1}}.
    {kill,{y,1}}.
    {line,[{location,"lc_tests_opt.erl",163}]}.
    {call,2,{f,175}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",162}]}.
    {call_ext_last,2,{extfunc,lists,filter,2},2}.


{function, pen2_mr, 1, 53}.
  {label,52}.
    {line,[{location,"lc_tests_opt.erl",167}]}.
    {func_info,{atom,lc_tests_opt},{atom,pen2_mr},1}.
  {label,53}.
    {test,is_integer,{f,52},[{x,0}]}.
    {allocate_zero,1,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",168}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,164},0,0,0}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {make_fun2,{f,155},0,0,2}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",181}]}.
    {call_fun,1}.
    {deallocate,1}.
    return.


{function, pen2_lazier, 1, 55}.
  {label,54}.
    {line,[{location,"lc_tests_opt.erl",184}]}.
    {func_info,{atom,lc_tests_opt},{atom,pen2_lazier},1}.
  {label,55}.
    {test,is_integer,{f,54},[{x,0}]}.
    {allocate_zero,1,1}.
    {make_fun2,{f,153},0,0,1}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,144},0,0,0}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {make_fun2,{f,135},0,0,2}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",196}]}.
    {call_fun,0}.
    {move,{y,0},{x,1}}.
    {line,[{location,"lc_tests_opt.erl",196}]}.
    {call_fun,1}.
    {deallocate,1}.
    return.


{function, unused, 0, 57}.
  {label,56}.
    {line,[{location,"lc_tests_opt.erl",200}]}.
    {func_info,{atom,lc_tests_opt},{atom,unused},0}.
  {label,57}.
    {allocate,0,0}.
    {move,{integer,1000},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",201}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {line,[{location,"lc_tests_opt.erl",201}]}.
    {call,1,{f,132}}.
    {move,{atom,ok},{x,0}}.
    {deallocate,0}.
    return.


{function, single_filter, 1, 59}.
  {label,58}.
    {line,[{location,"lc_tests_opt.erl",207}]}.
    {func_info,{atom,lc_tests_opt},{atom,single_filter},1}.
  {label,59}.
    {test,is_list,{f,58},[{x,0}]}.
    {call_only,1,{f,128}}.


{function, record_extract, 1, 61}.
  {label,60}.
    {line,[{location,"lc_tests_opt.erl",213}]}.
    {func_info,{atom,lc_tests_opt},{atom,record_extract},1}.
  {label,61}.
    {test,is_list,{f,60},[{x,0}]}.
    {call_only,1,{f,124}}.


{function, function_param_lc, 2, 63}.
  {label,62}.
    {line,[{location,"lc_tests_opt.erl",217}]}.
    {func_info,{atom,lc_tests_opt},{atom,function_param_lc},2}.
  {label,63}.
    {test,is_list,{f,62},[{x,1}]}.
    {move,{x,1},{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{x,2},{x,0}}.
    {call_only,2,{f,120}}.


{function, sorted_gen, 1, 65}.
  {label,64}.
    {line,[{location,"lc_tests_opt.erl",221}]}.
    {func_info,{atom,lc_tests_opt},{atom,sorted_gen},1}.
  {label,65}.
    {test,is_list,{f,64},[{x,0}]}.
    {allocate,1,1}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,113},0,0,0}.
    {move,{y,0},{x,1}}.
    {trim,1,0}.
    {line,[{location,"lc_tests_opt.erl",223}]}.
    {call_ext,2,{extfunc,lists,sort,2}}.
    {call_last,1,{f,116},0}.


{function, make_n, 2, 67}.
  {label,66}.
    {line,[{location,"lc_tests_opt.erl",231}]}.
    {func_info,{atom,lc_tests_opt},{atom,make_n},2}.
  {label,67}.
    {test,is_integer,{f,66},[{x,1}]}.
    {allocate,1,2}.
    {move,{x,0},{y,0}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",232}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,0},{x,1}}.
    {call_last,2,{f,109},1}.


{function, intermediary_list, 1, 69}.
  {label,68}.
    {line,[{location,"lc_tests_opt.erl",235}]}.
    {func_info,{atom,lc_tests_opt},{atom,intermediary_list},1}.
  {label,69}.
    {test,is_integer,{f,68},[{x,0}]}.
    {allocate,0,1}.
    {move,{integer,50},{x,1}}.
    {line,[{location,"lc_tests_opt.erl",236}]}.
    {call,2,{f,9}}.
    {call_last,1,{f,105},0}.


{function, intermediary_list2, 1, 71}.
  {label,70}.
    {line,[{location,"lc_tests_opt.erl",239}]}.
    {func_info,{atom,lc_tests_opt},{atom,intermediary_list2},1}.
  {label,71}.
    {test,is_integer,{f,70},[{x,0}]}.
    {allocate,0,1}.
    {move,{integer,50},{x,1}}.
    {line,[{location,"lc_tests_opt.erl",240}]}.
    {call,2,{f,9}}.
    {line,[{location,"lc_tests_opt.erl",241}]}.
    {call,1,{f,101}}.
    {call_last,1,{f,98},0}.


{function, intermediary_list3, 1, 73}.
  {label,72}.
    {line,[{location,"lc_tests_opt.erl",244}]}.
    {func_info,{atom,lc_tests_opt},{atom,intermediary_list3},1}.
  {label,73}.
    {test,is_integer,{f,72},[{x,0}]}.
    {allocate,0,1}.
    {move,{integer,50},{x,1}}.
    {line,[{location,"lc_tests_opt.erl",245}]}.
    {call,2,{f,9}}.
    {line,[{location,"lc_tests_opt.erl",246}]}.
    {call,1,{f,95}}.
    {line,[{location,"lc_tests_opt.erl",247}]}.
    {call,1,{f,92}}.
    {call_last,1,{f,88},0}.


{function, nested_lc_gen, 1, 75}.
  {label,74}.
    {line,[{location,"lc_tests_opt.erl",254}]}.
    {func_info,{atom,lc_tests_opt},{atom,nested_lc_gen},1}.
  {label,75}.
    {test,is_list,{f,74},[{x,0}]}.
    {allocate,0,1}.
    {line,[{location,"lc_tests_opt.erl",256}]}.
    {call,1,{f,81}}.
    {call_last,1,{f,85},0}.


{function, module_info, 0, 77}.
  {label,76}.
    {line,[]}.
    {func_info,{atom,lc_tests_opt},{atom,module_info},0}.
  {label,77}.
    {move,{atom,lc_tests_opt},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 79}.
  {label,78}.
    {line,[]}.
    {func_info,{atom,lc_tests_opt},{atom,module_info},1}.
  {label,79}.
    {move,{x,0},{x,1}}.
    {move,{atom,lc_tests_opt},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.


{function, '-nested_lc_gen/1-lc$^0/1-1-', 1, 81}.
  {label,80}.
    {line,[{location,"lc_tests_opt.erl",256}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-nested_lc_gen/1-lc$^0/1-1-'},1}.
  {label,81}.
    {test,is_nonempty_list,{f,83},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {gc_bif,'rem',{f,82},3,[{x,1},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,82},[{x,3},{integer,1}]}.
    {allocate,1,3}.
    {move,{x,2},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",256}]}.
    {call,1,{f,81}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,82}.
    {move,{x,2},{x,0}}.
    {call_only,1,{f,81}}.
  {label,83}.
    {test,is_nil,{f,80},[{x,0}]}.
    return.


{function, '-nested_lc_gen/1-lc$^1/1-0-', 1, 85}.
  {label,84}.
    {line,[{location,"lc_tests_opt.erl",255}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-nested_lc_gen/1-lc$^1/1-0-'},1}.
  {label,85}.
    {test,is_nonempty_list,{f,86},[{x,0}]}.
    {allocate_zero,2,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {line,[{location,"lc_tests_opt.erl",255}]}.
    {gc_bif,'*',{f,0},3,[{x,1},{x,1}],{y,0}}.
    {move,{x,2},{x,0}}.
    {move,{x,1},{y,1}}.
    {line,[{location,"lc_tests_opt.erl",256}]}.
    {call,1,{f,85}}.
    {test_heap,6,1}.
    {put_tuple,3,{x,1}}.
    {put,{atom,test_record}}.
    {put,{y,1}}.
    {put,{y,0}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,86}.
    {test,is_nil,{f,84},[{x,0}]}.
    return.


{function, '-intermediary_list3/1-lc$^2/1-2-', 1, 88}.
  {label,87}.
    {line,[{location,"lc_tests_opt.erl",248}]}.
    {func_info,{atom,lc_tests_opt},
               {atom,'-intermediary_list3/1-lc$^2/1-2-'},
               1}.
  {label,88}.
    {test,is_nonempty_list,{f,90},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_tuple,{f,89},[{x,1}]}.
    {test,test_arity,{f,89},[{x,1},2]}.
    {get_tuple_element,{x,1},0,{x,3}}.
    {get_tuple_element,{x,1},1,{x,4}}.
    {test,is_tuple,{f,89},[{x,4}]}.
    {test,test_arity,{f,89},[{x,4},2]}.
    {get_tuple_element,{x,4},0,{x,5}}.
    {get_tuple_element,{x,4},1,{x,6}}.
    {test,is_tuple,{f,89},[{x,6}]}.
    {test,test_arity,{f,89},[{x,6},3]}.
    {get_tuple_element,{x,6},0,{x,7}}.
    {get_tuple_element,{x,6},1,{x,8}}.
    {get_tuple_element,{x,6},2,{x,9}}.
    {test,is_eq_exact,{f,89},[{x,7},{atom,test_record}]}.
    {allocate_zero,1,10}.
    {line,[{location,"lc_tests_opt.erl",248}]}.
    {gc_bif,'*',{f,0},10,[{x,3},{x,5}],{x,0}}.
    {line,[{location,"lc_tests_opt.erl",248}]}.
    {gc_bif,'*',{f,0},10,[{x,0},{x,8}],{x,0}}.
    {line,[{location,"lc_tests_opt.erl",248}]}.
    {gc_bif,'*',{f,0},10,[{x,0},{x,9}],{y,0}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",248}]}.
    {call,1,{f,88}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,89}.
    {move,{x,2},{x,0}}.
    {call_only,1,{f,88}}.
  {label,90}.
    {test,is_nil,{f,87},[{x,0}]}.
    return.


{function, '-intermediary_list3/1-lc$^1/1-1-', 1, 92}.
  {label,91}.
    {line,[{location,"lc_tests_opt.erl",247}]}.
    {func_info,{atom,lc_tests_opt},
               {atom,'-intermediary_list3/1-lc$^1/1-1-'},
               1}.
  {label,92}.
    {test,is_nonempty_list,{f,93},[{x,0}]}.
    {allocate,2,1}.
    {get_list,{x,0},{y,1},{y,0}}.
    {move,{integer,100},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",247}]}.
    {call_ext,1,{extfunc,random,uniform,1}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",247}]}.
    {call,1,{f,92}}.
    {test_heap,5,1}.
    {put_tuple,2,{x,1}}.
    {put,{y,0}}.
    {put,{y,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,93}.
    {test,is_nil,{f,91},[{x,0}]}.
    return.


{function, '-intermediary_list3/1-lc$^0/1-0-', 1, 95}.
  {label,94}.
    {line,[{location,"lc_tests_opt.erl",246}]}.
    {func_info,{atom,lc_tests_opt},
               {atom,'-intermediary_list3/1-lc$^0/1-0-'},
               1}.
  {label,95}.
    {test,is_nonempty_list,{f,96},[{x,0}]}.
    {allocate,2,1}.
    {get_list,{x,0},{y,1},{y,0}}.
    {move,{integer,50},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",246}]}.
    {call_ext,1,{extfunc,random,uniform,1}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",246}]}.
    {call,1,{f,95}}.
    {test_heap,5,1}.
    {put_tuple,2,{x,1}}.
    {put,{y,0}}.
    {put,{y,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,96}.
    {test,is_nil,{f,94},[{x,0}]}.
    return.


{function, '-intermediary_list2/1-lc$^1/1-1-', 1, 98}.
  {label,97}.
    {line,[{location,"lc_tests_opt.erl",242}]}.
    {func_info,{atom,lc_tests_opt},
               {atom,'-intermediary_list2/1-lc$^1/1-1-'},
               1}.
  {label,98}.
    {test,is_nonempty_list,{f,99},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {line,[{location,"lc_tests_opt.erl",242}]}.
    {gc_bif,'*',{f,0},3,[{x,1},{x,1}],{y,0}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",242}]}.
    {call,1,{f,98}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,99}.
    {test,is_nil,{f,97},[{x,0}]}.
    return.


{function, '-intermediary_list2/1-lc$^0/1-0-', 1, 101}.
  {label,100}.
    {line,[{location,"lc_tests_opt.erl",241}]}.
    {func_info,{atom,lc_tests_opt},
               {atom,'-intermediary_list2/1-lc$^0/1-0-'},
               1}.
  {label,101}.
    {test,is_nonempty_list,{f,102},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_tuple,{f,103},[{x,1}]}.
    {test,test_arity,{f,103},[{x,1},3]}.
    {get_tuple_element,{x,1},0,{x,0}}.
    {get_tuple_element,{x,1},2,{x,3}}.
    {test,is_eq_exact,{f,103},[{x,0},{atom,test_record}]}.
    {move,{x,2},{x,0}}.
    {move,{x,3},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",241}]}.
    {call,1,{f,101}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,102}.
    {test,is_nil,{f,100},[{x,0}]}.
    return.
  {label,103}.
    {move,{literal,{badrecord,test_record}},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",241}]}.
    {call_ext,1,{extfunc,erlang,error,1}}.


{function, '-intermediary_list/1-lc$^0/1-0-', 1, 105}.
  {label,104}.
    {line,[{location,"lc_tests_opt.erl",237}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-intermediary_list/1-lc$^0/1-0-'},1}.
  {label,105}.
    {test,is_nonempty_list,{f,106},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_tuple,{f,107},[{x,1}]}.
    {test,test_arity,{f,107},[{x,1},3]}.
    {get_tuple_element,{x,1},0,{x,0}}.
    {get_tuple_element,{x,1},2,{x,3}}.
    {test,is_eq_exact,{f,107},[{x,0},{atom,test_record}]}.
    {move,{x,2},{x,0}}.
    {move,{x,3},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",237}]}.
    {call,1,{f,105}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,106}.
    {test,is_nil,{f,104},[{x,0}]}.
    return.
  {label,107}.
    {move,{literal,{badrecord,test_record}},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",237}]}.
    {call_ext,1,{extfunc,erlang,error,1}}.


{function, '-make_n/2-lc$^0/1-0-', 2, 109}.
  {label,108}.
    {line,[{location,"lc_tests_opt.erl",232}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-make_n/2-lc$^0/1-0-'},2}.
  {label,109}.
    {test,is_nonempty_list,{f,110},[{x,0}]}.
    {allocate,1,2}.
    {get_list,{x,0},{x,2},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",232}]}.
    {call,2,{f,109}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,110}.
    {test,is_nil,{f,111},[{x,0}]}.
    return.
  {label,111}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",232}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-sorted_gen/1-fun-0-', 2, 113}.
  {label,112}.
    {line,[{location,"lc_tests_opt.erl",223}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-sorted_gen/1-fun-0-'},2}.
  {label,113}.
    {test,is_tuple,{f,114},[{x,0}]}.
    {test,test_arity,{f,114},[{x,0},3]}.
    {get_tuple_element,{x,0},0,{x,2}}.
    {get_tuple_element,{x,0},1,{x,3}}.
    {test,is_eq_exact,{f,114},[{x,2},{atom,test_record}]}.
    {test,is_tuple,{f,114},[{x,1}]}.
    {test,test_arity,{f,114},[{x,1},3]}.
    {get_tuple_element,{x,1},0,{x,0}}.
    {get_tuple_element,{x,1},1,{x,2}}.
    {test,is_eq_exact,{f,114},[{x,0},{atom,test_record}]}.
    {bif,'<',{f,0},[{x,3},{x,2}],{x,0}}.
    return.
  {label,114}.
    {move,{literal,{badrecord,test_record}},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",224}]}.
    {call_ext,1,{extfunc,erlang,error,1}}.


{function, '-sorted_gen/1-lc$^0/1-0-', 1, 116}.
  {label,115}.
    {line,[{location,"lc_tests_opt.erl",222}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-sorted_gen/1-lc$^0/1-0-'},1}.
  {label,116}.
    {test,is_nonempty_list,{f,117},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_tuple,{f,118},[{x,1}]}.
    {test,test_arity,{f,118},[{x,1},3]}.
    {get_tuple_element,{x,1},0,{x,0}}.
    {get_tuple_element,{x,1},1,{x,3}}.
    {test,is_eq_exact,{f,118},[{x,0},{atom,test_record}]}.
    {move,{x,2},{x,0}}.
    {move,{x,3},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",223}]}.
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
    {line,[{location,"lc_tests_opt.erl",222}]}.
    {call_ext,1,{extfunc,erlang,error,1}}.


{function, '-function_param_lc/2-lc$^0/1-0-', 2, 120}.
  {label,119}.
    {line,[{location,"lc_tests_opt.erl",218}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-function_param_lc/2-lc$^0/1-0-'},2}.
  {label,120}.
    {test,is_nonempty_list,{f,121},[{x,0}]}.
    {allocate,2,2}.
    {get_list,{x,0},{y,0},{x,0}}.
    {move,{x,1},{y,1}}.
    {line,[{location,"lc_tests_opt.erl",218}]}.
    {call,2,{f,120}}.
    {test_heap,6,1}.
    {put_list,{y,0},nil,{x,1}}.
    {put_list,{y,1},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,121}.
    {test,is_nil,{f,122},[{x,0}]}.
    return.
  {label,122}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",218}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-record_extract/1-lc$^0/1-0-', 1, 124}.
  {label,123}.
    {line,[{location,"lc_tests_opt.erl",214}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-record_extract/1-lc$^0/1-0-'},1}.
  {label,124}.
    {test,is_nonempty_list,{f,125},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_tuple,{f,126},[{x,1}]}.
    {test,test_arity,{f,126},[{x,1},3]}.
    {get_tuple_element,{x,1},0,{x,0}}.
    {get_tuple_element,{x,1},1,{x,3}}.
    {test,is_eq_exact,{f,126},[{x,0},{atom,test_record}]}.
    {move,{x,2},{x,0}}.
    {move,{x,3},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",214}]}.
    {call,1,{f,124}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,125}.
    {test,is_nil,{f,123},[{x,0}]}.
    return.
  {label,126}.
    {move,{literal,{badrecord,test_record}},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",214}]}.
    {call_ext,1,{extfunc,erlang,error,1}}.


{function, '-single_filter/1-lc$^0/1-0-', 1, 128}.
  {label,127}.
    {line,[{location,"lc_tests_opt.erl",208}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-single_filter/1-lc$^0/1-0-'},1}.
  {label,128}.
    {test,is_nonempty_list,{f,130},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_integer,{f,129},[{x,1}]}.
    {allocate,1,3}.
    {move,{x,2},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",208}]}.
    {call,1,{f,128}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,129}.
    {move,{x,2},{x,0}}.
    {call_only,1,{f,128}}.
  {label,130}.
    {test,is_nil,{f,127},[{x,0}]}.
    return.


{function, '-unused/0-lc$^0/1-0-', 1, 132}.
  {label,131}.
    {line,[{location,"lc_tests_opt.erl",201}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-unused/0-lc$^0/1-0-'},1}.
  {label,132}.
    {test,is_nonempty_list,{f,133},[{x,0}]}.
    {allocate,1,1}.
    {get_list,{x,0},{x,1},{y,0}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",201}]}.
    {call_ext,1,{extfunc,timer,sleep,1}}.
    {move,{y,0},{x,0}}.
    {call_last,1,{f,132},1}.
  {label,133}.
    {test,is_nil,{f,131},[{x,0}]}.
    return.


{function, '-pen2_lazier/1-anonymous-5-', 3, 135}.
  {label,134}.
    {line,[]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_lazier/1-anonymous-5-'},3}.
  {label,135}.
    {call_only,3,{f,137}}.


{function, '-pen2_lazier/1-Red_xs/1-1-', 3, 137}.
  {label,136}.
    {line,[{location,"lc_tests_opt.erl",191}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_lazier/1-Red_xs/1-1-'},3}.
  {label,137}.
    {allocate,3,3}.
    {move,{x,0},{y,2}}.
    {move,{x,1},{x,0}}.
    {move,{x,2},{x,1}}.
    {move,{x,0},{y,0}}.
    {move,{x,1},{y,1}}.
    {make_fun2,{f,142},0,0,2}.
    {test,is_nonempty_list,{f,139},[{y,2}]}.
    {get_list,{y,2},{x,1},{x,2}}.
    {gc_bif,'rem',{f,138},3,[{x,1},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,138},[{x,3},{integer,0}]}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",192}]}.
    {call_fun,0}.
    {move,{y,0},{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{y,1},{x,3}}.
    {move,{y,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",192}]}.
    {call_fun,3}.
    {deallocate,3}.
    return.
  {label,138}.
    {move,{x,0},{y,2}}.
    {trim,2,1}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",193}]}.
    {call_fun,0}.
    {move,{y,0},{x,1}}.
    {line,[{location,"lc_tests_opt.erl",193}]}.
    {call_fun,1}.
    {deallocate,1}.
    return.
  {label,139}.
    {test,is_nil,{f,140},[{y,2}]}.
    {move,nil,{x,0}}.
    {deallocate,3}.
    return.
  {label,140}.
    {test_heap,2,0}.
    {put_tuple,1,{x,1}}.
    {put,{y,2}}.
    {line,[{location,"lc_tests_opt.erl",191}]}.
    {case_end,{x,1}}.


{function, '-pen2_lazier/1-anonymous-4-', 3, 142}.
  {label,141}.
    {line,[]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_lazier/1-anonymous-4-'},3}.
  {label,142}.
    {call_only,3,{f,137}}.


{function, '-pen2_lazier/1-anonymous-3-', 3, 144}.
  {label,143}.
    {line,[]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_lazier/1-anonymous-3-'},3}.
  {label,144}.
    {call_only,3,{f,146}}.


{function, '-pen2_lazier/1-Red_ys/3-0-', 3, 146}.
  {label,145}.
    {line,[{location,"lc_tests_opt.erl",186}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_lazier/1-Red_ys/3-0-'},3}.
  {label,146}.
    {allocate,5,3}.
    {init,{y,0}}.
    {init,{y,1}}.
    {move,{x,2},{y,2}}.
    {move,{x,1},{y,3}}.
    {move,{x,0},{y,4}}.
    {make_fun2,{f,151},0,0,0}.
    {test,is_nonempty_list,{f,149},[{y,4}]}.
    {get_list,{y,4},{x,1},{x,2}}.
    {test,is_nonempty_list,{f,148},[{y,3}]}.
    {get_list,{y,3},{x,3},{x,4}}.
    {gc_bif,'rem',{f,147},5,[{x,3},{integer,2}],{x,2}}.
    {test,is_eq_exact,{f,147},[{x,2},{integer,1}]}.
    {gc_bif,'+',{f,147},5,[{x,1},{x,3}],{x,2}}.
    {gc_bif,'rem',{f,147},5,[{x,2},{integer,5}],{x,2}}.
    {bif,'=/=',{f,147},[{x,2},{integer,0}],{x,2}}.
    {move,{x,2},{x,5}}.
    {test,is_eq_exact,{f,147},[{x,5},{atom,true}]}.
    {move,{x,0},{y,0}}.
    {move,{x,4},{x,0}}.
    {move,{x,3},{y,1}}.
    {move,{x,1},{y,3}}.
    {line,[{location,"lc_tests_opt.erl",188}]}.
    {call_fun,0}.
    {move,{y,2},{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,3}}.
    {move,{y,4},{x,0}}.
    {move,{y,1},{y,4}}.
    {trim,3,2}.
    {line,[{location,"lc_tests_opt.erl",188}]}.
    {call_fun,3}.
    {test_heap,6,1}.
    {put_list,{y,1},nil,{x,1}}.
    {put_list,{y,0},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,147}.
    {move,{x,0},{y,3}}.
    {move,{x,4},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",189}]}.
    {call_fun,0}.
    {move,{y,2},{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{y,3},{x,3}}.
    {move,{y,4},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",189}]}.
    {call_fun,3}.
    {deallocate,5}.
    return.
  {label,148}.
    {test,is_nil,{f,149},[{y,3}]}.
    {kill,{y,3}}.
    {kill,{y,4}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",190}]}.
    {call_fun,0}.
    {move,{y,2},{x,1}}.
    {line,[{location,"lc_tests_opt.erl",190}]}.
    {call_fun,1}.
    {deallocate,5}.
    return.
  {label,149}.
    {test_heap,4,0}.
    {put_tuple,3,{x,1}}.
    {put,{y,4}}.
    {put,{y,3}}.
    {put,{y,2}}.
    {line,[{location,"lc_tests_opt.erl",186}]}.
    {case_end,{x,1}}.


{function, '-pen2_lazier/1-anonymous-2-', 3, 151}.
  {label,150}.
    {line,[]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_lazier/1-anonymous-2-'},3}.
  {label,151}.
    {call_only,3,{f,146}}.


{function, '-pen2_lazier/1-fun-0-', 1, 153}.
  {label,152}.
    {line,[{location,"lc_tests_opt.erl",185}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_lazier/1-fun-0-'},1}.
  {label,153}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {call_only,2,{f,12}}.


{function, '-pen2_mr/1-anonymous-5-', 3, 155}.
  {label,154}.
    {line,[]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_mr/1-anonymous-5-'},3}.
  {label,155}.
    {call_only,3,{f,157}}.


{function, '-pen2_mr/1-Red_xs/1-1-', 3, 157}.
  {label,156}.
    {line,[{location,"lc_tests_opt.erl",175}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_mr/1-Red_xs/1-1-'},3}.
  {label,157}.
    {allocate,3,3}.
    {move,{x,0},{y,2}}.
    {move,{x,1},{x,0}}.
    {move,{x,2},{x,1}}.
    {move,{x,0},{y,0}}.
    {move,{x,1},{y,1}}.
    {make_fun2,{f,162},0,0,2}.
    {test,is_nonempty_list,{f,159},[{y,2}]}.
    {get_list,{y,2},{x,1},{x,2}}.
    {gc_bif,'rem',{f,158},3,[{x,1},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,158},[{x,3},{integer,0}]}.
    {move,{x,0},{x,2}}.
    {move,{y,0},{x,1}}.
    {move,{y,1},{x,3}}.
    {move,{y,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",176}]}.
    {call_fun,3}.
    {deallocate,3}.
    return.
  {label,158}.
    {move,{x,0},{x,1}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",177}]}.
    {call_fun,1}.
    {deallocate,3}.
    return.
  {label,159}.
    {test,is_nil,{f,160},[{y,2}]}.
    {move,nil,{x,0}}.
    {deallocate,3}.
    return.
  {label,160}.
    {test_heap,2,0}.
    {put_tuple,1,{x,1}}.
    {put,{y,2}}.
    {line,[{location,"lc_tests_opt.erl",175}]}.
    {case_end,{x,1}}.


{function, '-pen2_mr/1-anonymous-4-', 3, 162}.
  {label,161}.
    {line,[]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_mr/1-anonymous-4-'},3}.
  {label,162}.
    {call_only,3,{f,157}}.


{function, '-pen2_mr/1-anonymous-3-', 3, 164}.
  {label,163}.
    {line,[]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_mr/1-anonymous-3-'},3}.
  {label,164}.
    {call_only,3,{f,166}}.


{function, '-pen2_mr/1-Red_ys/3-0-', 3, 166}.
  {label,165}.
    {line,[{location,"lc_tests_opt.erl",169}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_mr/1-Red_ys/3-0-'},3}.
  {label,166}.
    {allocate,3,3}.
    {move,{x,1},{y,0}}.
    {move,{x,0},{y,1}}.
    {move,{x,2},{y,2}}.
    {make_fun2,{f,171},0,0,0}.
    {test,is_nonempty_list,{f,169},[{y,1}]}.
    {get_list,{y,1},{x,1},{x,2}}.
    {test,is_nonempty_list,{f,168},[{y,0}]}.
    {get_list,{y,0},{x,3},{x,4}}.
    {gc_bif,'rem',{f,167},5,[{x,3},{integer,2}],{x,2}}.
    {test,is_eq_exact,{f,167},[{x,2},{integer,1}]}.
    {gc_bif,'+',{f,167},5,[{x,1},{x,3}],{x,2}}.
    {gc_bif,'rem',{f,167},5,[{x,2},{integer,5}],{x,2}}.
    {bif,'=/=',{f,167},[{x,2},{integer,0}],{x,2}}.
    {move,{x,2},{x,5}}.
    {test,is_eq_exact,{f,167},[{x,5},{atom,true}]}.
    {move,{x,3},{x,6}}.
    {move,{x,1},{x,5}}.
    {move,{y,2},{x,2}}.
    {move,{x,4},{x,1}}.
    {move,{x,0},{x,3}}.
    {move,{y,1},{x,0}}.
    {move,{x,6},{y,1}}.
    {move,{x,5},{y,2}}.
    {trim,1,2}.
    {line,[{location,"lc_tests_opt.erl",171}]}.
    {call_fun,3}.
    {test_heap,6,1}.
    {put_list,{y,0},nil,{x,1}}.
    {put_list,{y,1},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,167}.
    {move,{y,2},{x,2}}.
    {move,{x,4},{x,1}}.
    {move,{x,0},{x,3}}.
    {move,{y,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",172}]}.
    {call_fun,3}.
    {deallocate,3}.
    return.
  {label,168}.
    {test,is_nil,{f,169},[{y,0}]}.
    {move,{y,2},{x,1}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",174}]}.
    {call_fun,1}.
    {deallocate,3}.
    return.
  {label,169}.
    {test_heap,4,0}.
    {put_tuple,3,{x,1}}.
    {put,{y,1}}.
    {put,{y,0}}.
    {put,{y,2}}.
    {line,[{location,"lc_tests_opt.erl",169}]}.
    {case_end,{x,1}}.


{function, '-pen2_mr/1-anonymous-2-', 3, 171}.
  {label,170}.
    {line,[]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_mr/1-anonymous-2-'},3}.
  {label,171}.
    {call_only,3,{f,166}}.


{function, '-pen2_listsfuns/1-fun-1-', 1, 173}.
  {label,172}.
    {line,[{location,"lc_tests_opt.erl",163}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_listsfuns/1-fun-1-'},1}.
  {label,173}.
    {line,[{location,"lc_tests_opt.erl",163}]}.
    {gc_bif,'rem',{f,0},1,[{x,0},{integer,2}],{x,0}}.
    {bif,'=:=',{f,0},[{x,0},{integer,0}],{x,0}}.
    return.


{function, '-pen2_listsfuns/1-lc$^0/1-0-', 2, 175}.
  {label,174}.
    {line,[{location,"lc_tests_opt.erl",163}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_listsfuns/1-lc$^0/1-0-'},2}.
  {label,175}.
    {test,is_nonempty_list,{f,176},[{x,0}]}.
    {allocate,3,2}.
    {get_list,{x,0},{y,1},{y,2}}.
    {move,{x,1},{y,0}}.
    {make_fun2,{f,179},0,0,0}.
    {move,{y,0},{x,1}}.
    {line,[{location,"lc_tests_opt.erl",164}]}.
    {call_ext,2,{extfunc,lists,filter,2}}.
    {move,{y,1},{x,2}}.
    {move,{y,2},{x,3}}.
    {move,{y,0},{x,1}}.
    {call_last,4,{f,181},3}.
  {label,176}.
    {test,is_nil,{f,177},[{x,0}]}.
    return.
  {label,177}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",163}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pen2_listsfuns/1-fun-2-', 1, 179}.
  {label,178}.
    {line,[{location,"lc_tests_opt.erl",164}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_listsfuns/1-fun-2-'},1}.
  {label,179}.
    {line,[{location,"lc_tests_opt.erl",164}]}.
    {gc_bif,'rem',{f,0},1,[{x,0},{integer,2}],{x,0}}.
    {bif,'=:=',{f,0},[{x,0},{integer,1}],{x,0}}.
    return.


{function, '-pen2_listsfuns/1-lc$^1/1-1-', 4, 181}.
  {label,180}.
    {line,[{location,"lc_tests_opt.erl",163}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_listsfuns/1-lc$^1/1-1-'},4}.
  {label,181}.
    {test,is_nonempty_list,{f,182},[{x,0}]}.
    {allocate,2,4}.
    {get_list,{x,0},{y,0},{x,0}}.
    {move,{x,2},{y,1}}.
    {line,[{location,"lc_tests_opt.erl",164}]}.
    {call,4,{f,181}}.
    {test_heap,6,1}.
    {put_list,{y,0},nil,{x,1}}.
    {put_list,{y,1},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,182}.
    {test,is_nil,{f,183},[{x,0}]}.
    {move,{x,3},{x,0}}.
    {call_only,2,{f,175}}.
  {label,183}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",163}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pen2_listsfuns/1-fun-0-', 1, 185}.
  {label,184}.
    {line,[{location,"lc_tests_opt.erl",162}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_listsfuns/1-fun-0-'},1}.
  {label,185}.
    {test,is_nonempty_list,{f,184},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_nonempty_list,{f,184},[{x,2}]}.
    {get_list,{x,2},{x,3},{x,4}}.
    {test,is_nil,{f,184},[{x,4}]}.
    {line,[{location,"lc_tests_opt.erl",162}]}.
    {gc_bif,'+',{f,0},4,[{x,1},{x,3}],{x,0}}.
    {line,[{location,"lc_tests_opt.erl",162}]}.
    {gc_bif,'rem',{f,0},1,[{x,0},{integer,5}],{x,0}}.
    {bif,'=/=',{f,0},[{x,0},{integer,0}],{x,0}}.
    return.


{function, '-pen2_foldr/1-fun-1-', 3, 187}.
  {label,186}.
    {line,[{location,"lc_tests_opt.erl",147}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_foldr/1-fun-1-'},3}.
  {label,187}.
    {gc_bif,'rem',{f,188},3,[{x,0},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,188},[{x,3},{integer,0}]}.
    {allocate,2,3}.
    {move,{x,2},{y,0}}.
    {move,{x,1},{y,1}}.
    {make_fun2,{f,190},0,0,1}.
    {move,{y,0},{x,2}}.
    {move,nil,{x,1}}.
    {trim,1,1}.
    {line,[{location,"lc_tests_opt.erl",149}]}.
    {call_ext,3,{extfunc,lists,foldr,3}}.
    {move,{y,0},{x,1}}.
    {line,[{location,"lc_tests_opt.erl",155}]}.
    {call_ext_last,2,{extfunc,erlang,'++',2},1}.
  {label,188}.
    {move,{x,1},{x,0}}.
    return.


{function, '-pen2_foldr/1-fun-0-', 3, 190}.
  {label,189}.
    {line,[{location,"lc_tests_opt.erl",150}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_foldr/1-fun-0-'},3}.
  {label,190}.
    {gc_bif,'rem',{f,191},3,[{x,0},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,191},[{x,3},{integer,1}]}.
    {gc_bif,'+',{f,191},3,[{x,2},{x,0}],{x,3}}.
    {gc_bif,'rem',{f,191},4,[{x,3},{integer,5}],{x,3}}.
    {test,is_ne_exact,{f,191},[{x,3},{integer,0}]}.
    {test_heap,6,3}.
    {put_list,{x,0},nil,{x,0}}.
    {put_list,{x,2},{x,0},{x,2}}.
    {put_list,{x,2},{x,1},{x,0}}.
    return.
  {label,191}.
    {move,{x,1},{x,0}}.
    return.


{function, '-pen2_foldl/1-fun-1-', 3, 193}.
  {label,192}.
    {line,[{location,"lc_tests_opt.erl",131}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_foldl/1-fun-1-'},3}.
  {label,193}.
    {gc_bif,'rem',{f,194},3,[{x,0},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,194},[{x,3},{integer,0}]}.
    {allocate,2,3}.
    {move,{x,2},{y,0}}.
    {move,{x,1},{y,1}}.
    {make_fun2,{f,196},0,0,1}.
    {move,{y,0},{x,2}}.
    {move,nil,{x,1}}.
    {trim,1,1}.
    {line,[{location,"lc_tests_opt.erl",133}]}.
    {call_ext,3,{extfunc,lists,foldl,3}}.
    {move,{y,0},{x,1}}.
    {line,[{location,"lc_tests_opt.erl",139}]}.
    {call_ext_last,2,{extfunc,erlang,'++',2},1}.
  {label,194}.
    {move,{x,1},{x,0}}.
    return.


{function, '-pen2_foldl/1-fun-0-', 3, 196}.
  {label,195}.
    {line,[{location,"lc_tests_opt.erl",134}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2_foldl/1-fun-0-'},3}.
  {label,196}.
    {gc_bif,'rem',{f,197},3,[{x,0},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,197},[{x,3},{integer,1}]}.
    {gc_bif,'+',{f,197},3,[{x,2},{x,0}],{x,3}}.
    {gc_bif,'rem',{f,197},4,[{x,3},{integer,5}],{x,3}}.
    {test,is_ne_exact,{f,197},[{x,3},{integer,0}]}.
    {test_heap,6,3}.
    {put_list,{x,0},nil,{x,0}}.
    {put_list,{x,2},{x,0},{x,2}}.
    {put_list,{x,2},{x,1},{x,0}}.
    return.
  {label,197}.
    {move,{x,1},{x,0}}.
    return.


{function, '-pen2/1-lc$^0/1-0-', 2, 199}.
  {label,198}.
    {line,[{location,"lc_tests_opt.erl",121}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2/1-lc$^0/1-0-'},2}.
  {label,199}.
    {test,is_nonempty_list,{f,201},[{x,0}]}.
    {get_list,{x,0},{x,2},{x,3}}.
    {gc_bif,'rem',{f,200},4,[{x,2},{integer,2}],{x,4}}.
    {test,is_eq_exact,{f,200},[{x,4},{integer,0}]}.
    {allocate,3,4}.
    {move,{integer,1},{x,0}}.
    {move,{x,1},{y,0}}.
    {move,{x,3},{y,1}}.
    {move,{x,2},{y,2}}.
    {line,[{location,"lc_tests_opt.erl",123}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,1},{x,2}}.
    {move,{y,2},{x,3}}.
    {move,{y,0},{x,1}}.
    {call_last,4,{f,204},3}.
  {label,200}.
    {move,{x,3},{x,0}}.
    {call_only,2,{f,199}}.
  {label,201}.
    {test,is_nil,{f,202},[{x,0}]}.
    return.
  {label,202}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",121}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pen2/1-lc$^1/1-1-', 4, 204}.
  {label,203}.
    {line,[{location,"lc_tests_opt.erl",121}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2/1-lc$^1/1-1-'},4}.
  {label,204}.
    {test,is_nonempty_list,{f,206},[{x,0}]}.
    {get_list,{x,0},{x,4},{x,5}}.
    {gc_bif,'rem',{f,205},6,[{x,4},{integer,2}],{x,6}}.
    {gc_bif,'+',{f,205},7,[{x,3},{x,4}],{x,7}}.
    {gc_bif,'rem',{f,205},8,[{x,7},{integer,5}],{x,8}}.
    {test,is_eq_exact,{f,205},[{x,6},{integer,1}]}.
    {test,is_ne_exact,{f,205},[{x,8},{integer,0}]}.
    {allocate,2,6}.
    {move,{x,5},{x,0}}.
    {move,{x,4},{y,0}}.
    {move,{x,3},{y,1}}.
    {line,[{location,"lc_tests_opt.erl",123}]}.
    {call,4,{f,204}}.
    {test_heap,6,1}.
    {put_list,{y,0},nil,{x,1}}.
    {put_list,{y,1},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,205}.
    {move,{x,5},{x,0}}.
    {call_only,4,{f,204}}.
  {label,206}.
    {test,is_nil,{f,207},[{x,0}]}.
    {move,{x,2},{x,0}}.
    {call_only,2,{f,199}}.
  {label,207}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",121}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pen/1-lc$^0/1-0-', 2, 209}.
  {label,208}.
    {line,[{location,"lc_tests_opt.erl",114}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen/1-lc$^0/1-0-'},2}.
  {label,209}.
    {test,is_nonempty_list,{f,211},[{x,0}]}.
    {get_list,{x,0},{x,2},{x,3}}.
    {gc_bif,'rem',{f,210},4,[{x,2},{integer,2}],{x,4}}.
    {test,is_eq_exact,{f,210},[{x,4},{integer,0}]}.
    {allocate,3,4}.
    {move,{integer,1},{x,0}}.
    {move,{x,1},{y,0}}.
    {move,{x,3},{y,1}}.
    {move,{x,2},{y,2}}.
    {line,[{location,"lc_tests_opt.erl",116}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,1},{x,2}}.
    {move,{y,2},{x,3}}.
    {move,{y,0},{x,1}}.
    {call_last,4,{f,214},3}.
  {label,210}.
    {move,{x,3},{x,0}}.
    {call_only,2,{f,209}}.
  {label,211}.
    {test,is_nil,{f,212},[{x,0}]}.
    return.
  {label,212}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",114}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pen/1-lc$^1/1-1-', 4, 214}.
  {label,213}.
    {line,[{location,"lc_tests_opt.erl",114}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen/1-lc$^1/1-1-'},4}.
  {label,214}.
    {test,is_nonempty_list,{f,216},[{x,0}]}.
    {get_list,{x,0},{x,4},{x,5}}.
    {gc_bif,'rem',{f,215},6,[{x,4},{integer,2}],{x,6}}.
    {test,is_eq_exact,{f,215},[{x,6},{integer,1}]}.
    {allocate,2,6}.
    {move,{x,5},{x,0}}.
    {move,{x,4},{y,0}}.
    {move,{x,3},{y,1}}.
    {line,[{location,"lc_tests_opt.erl",116}]}.
    {call,4,{f,214}}.
    {test_heap,6,1}.
    {put_list,{y,0},nil,{x,1}}.
    {put_list,{y,1},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,215}.
    {move,{x,5},{x,0}}.
    {call_only,4,{f,214}}.
  {label,216}.
    {test,is_nil,{f,217},[{x,0}]}.
    {move,{x,2},{x,0}}.
    {call_only,2,{f,209}}.
  {label,217}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",114}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-ses/1-lc$^0/1-0-', 1, 219}.
  {label,218}.
    {line,[{location,"lc_tests_opt.erl",109}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-ses/1-lc$^0/1-0-'},1}.
  {label,219}.
    {test,is_nonempty_list,{f,221},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {gc_bif,'rem',{f,220},3,[{x,1},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,220},[{x,3},{integer,0}]}.
    {allocate_zero,1,3}.
    {line,[{location,"lc_tests_opt.erl",109}]}.
    {gc_bif,'*',{f,0},3,[{x,1},{x,1}],{y,0}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",109}]}.
    {call,1,{f,219}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,220}.
    {move,{x,2},{x,0}}.
    {call_only,1,{f,219}}.
  {label,221}.
    {test,is_nil,{f,218},[{x,0}]}.
    return.


{function, '-ss/1-lc$^0/1-0-', 1, 223}.
  {label,222}.
    {line,[{location,"lc_tests_opt.erl",105}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-ss/1-lc$^0/1-0-'},1}.
  {label,223}.
    {test,is_nonempty_list,{f,224},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {line,[{location,"lc_tests_opt.erl",105}]}.
    {gc_bif,'*',{f,0},3,[{x,1},{x,1}],{y,0}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",105}]}.
    {call,1,{f,223}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,224}.
    {test,is_nil,{f,222},[{x,0}]}.
    return.


{function, '-all_tests/0-fun-18-', 0, 226}.
  {label,225}.
    {line,[{location,"lc_tests_opt.erl",98}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-18-'},0}.
  {label,226}.
    {call_only,0,{f,57}}.


{function, '-all_tests/0-fun-17-', 1, 228}.
  {label,227}.
    {line,[{location,"lc_tests_opt.erl",97}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-17-'},1}.
  {label,228}.
    {call_only,1,{f,75}}.


{function, '-all_tests/0-fun-16-', 1, 230}.
  {label,229}.
    {line,[{location,"lc_tests_opt.erl",96}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-16-'},1}.
  {label,230}.
    {call_only,1,{f,73}}.


{function, '-all_tests/0-fun-15-', 1, 232}.
  {label,231}.
    {line,[{location,"lc_tests_opt.erl",95}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-15-'},1}.
  {label,232}.
    {call_only,1,{f,71}}.


{function, '-all_tests/0-fun-14-', 1, 234}.
  {label,233}.
    {line,[{location,"lc_tests_opt.erl",94}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-14-'},1}.
  {label,234}.
    {call_only,1,{f,69}}.


{function, '-all_tests/0-fun-13-', 2, 236}.
  {label,235}.
    {line,[{location,"lc_tests_opt.erl",93}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-13-'},2}.
  {label,236}.
    {call_only,2,{f,67}}.


{function, '-all_tests/0-fun-12-', 1, 238}.
  {label,237}.
    {line,[{location,"lc_tests_opt.erl",92}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-12-'},1}.
  {label,238}.
    {call_only,1,{f,65}}.


{function, '-all_tests/0-fun-11-', 2, 240}.
  {label,239}.
    {line,[{location,"lc_tests_opt.erl",91}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-11-'},2}.
  {label,240}.
    {call_only,2,{f,63}}.


{function, '-all_tests/0-fun-10-', 1, 242}.
  {label,241}.
    {line,[{location,"lc_tests_opt.erl",90}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-10-'},1}.
  {label,242}.
    {call_only,1,{f,61}}.


{function, '-all_tests/0-fun-9-', 1, 244}.
  {label,243}.
    {line,[{location,"lc_tests_opt.erl",89}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-9-'},1}.
  {label,244}.
    {call_only,1,{f,59}}.


{function, '-all_tests/0-fun-8-', 1, 246}.
  {label,245}.
    {line,[{location,"lc_tests_opt.erl",88}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-8-'},1}.
  {label,246}.
    {call_only,1,{f,55}}.


{function, '-all_tests/0-fun-7-', 1, 248}.
  {label,247}.
    {line,[{location,"lc_tests_opt.erl",87}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-7-'},1}.
  {label,248}.
    {call_only,1,{f,53}}.


{function, '-all_tests/0-fun-6-', 1, 250}.
  {label,249}.
    {line,[{location,"lc_tests_opt.erl",86}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-6-'},1}.
  {label,250}.
    {call_only,1,{f,51}}.


{function, '-all_tests/0-fun-5-', 1, 252}.
  {label,251}.
    {line,[{location,"lc_tests_opt.erl",85}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-5-'},1}.
  {label,252}.
    {call_only,1,{f,47}}.


{function, '-all_tests/0-fun-4-', 1, 254}.
  {label,253}.
    {line,[{location,"lc_tests_opt.erl",84}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-4-'},1}.
  {label,254}.
    {call_only,1,{f,49}}.


{function, '-all_tests/0-fun-3-', 1, 256}.
  {label,255}.
    {line,[{location,"lc_tests_opt.erl",83}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-3-'},1}.
  {label,256}.
    {call_only,1,{f,45}}.


{function, '-all_tests/0-fun-2-', 1, 258}.
  {label,257}.
    {line,[{location,"lc_tests_opt.erl",82}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-2-'},1}.
  {label,258}.
    {call_only,1,{f,43}}.


{function, '-all_tests/0-fun-1-', 1, 260}.
  {label,259}.
    {line,[{location,"lc_tests_opt.erl",81}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-1-'},1}.
  {label,260}.
    {call_only,1,{f,41}}.


{function, '-all_tests/0-fun-0-', 1, 262}.
  {label,261}.
    {line,[{location,"lc_tests_opt.erl",80}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-0-'},1}.
  {label,262}.
    {call_only,1,{f,39}}.


{function, '-test/3-fun-0-', 3, 264}.
  {label,263}.
    {line,[{location,"lc_tests_opt.erl",47}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-test/3-fun-0-'},3}.
  {label,264}.
    {allocate_zero,4,3}.
    {bif,self,{f,0},[],{y,0}}.
    {move,{x,1},{y,1}}.
    {move,{x,2},{y,2}}.
    {move,{x,0},{y,3}}.
    {line,[{location,"lc_tests_opt.erl",48}]}.
    {call,0,{f,18}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {kill,{y,0}}.
    {line,[{location,"lc_tests_opt.erl",48}]}.
    {call_ext,2,{extfunc,erlang,process_info,2}}.
    {move,{x,0},{x,2}}.
    {move,{y,1},{x,1}}.
    {move,{y,2},{x,0}}.
    {move,{x,2},{y,2}}.
    {kill,{y,1}}.
    {line,[{location,"lc_tests_opt.erl",49}]}.
    {call_ext,2,{extfunc,erlang,apply,2}}.
    {bif,self,{f,0},[],{y,0}}.
    {move,{x,0},{y,1}}.
    {line,[{location,"lc_tests_opt.erl",51}]}.
    {call,0,{f,18}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {trim,1,3}.
    {line,[{location,"lc_tests_opt.erl",51}]}.
    {call_ext,2,{extfunc,erlang,process_info,2}}.
    {test_heap,4,1}.
    {put_tuple,3,{x,1}}.
    {put,{y,1}}.
    {put,{x,0}}.
    {put,{y,0}}.
    {move,{y,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",52}]}.
    send.
    {deallocate,3}.
    return.


{function, '-lazy_seq/2-fun-0-', 2, 266}.
  {label,265}.
    {line,[{location,"lc_tests_opt.erl",32}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-lazy_seq/2-fun-0-'},2}.
  {label,266}.
    {line,[{location,"lc_tests_opt.erl",32}]}.
    {gc_bif,'+',{f,0},2,[{x,1},{integer,1}],{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{x,2},{x,0}}.
    {call_only,2,{f,12}}.


{function, '-random_bin/1-lc$^0/1-0-', 1, 268}.
  {label,267}.
    {line,[{location,"lc_tests_opt.erl",18}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-random_bin/1-lc$^0/1-0-'},1}.
  {label,268}.
    {test,is_nonempty_list,{f,269},[{x,0}]}.
    {allocate,1,1}.
    {get_list,{x,0},{x,1},{y,0}}.
    {move,{integer,255},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",18}]}.
    {call_ext,1,{extfunc,random,uniform,1}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",18}]}.
    {call,1,{f,268}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,269}.
    {test,is_nil,{f,267},[{x,0}]}.
    return.
