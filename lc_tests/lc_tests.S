{module, lc_tests}.  %% version = 0

{exports, [{function_param_lc,2},
           {intermediary_list,1},
           {intermediary_list2,1},
           {intermediary_list3,1},
           {make_n,2},
           {module_info,0},
           {module_info,1},
           {nested_lc_gen,1},
           {pen,1},
           {pen2,1},
           {random,1},
           {random_bin,1},
           {random_test_records,2},
           {record_extract,1},
           {rng,2},
           {ses,1},
           {single_filter,1},
           {sorted_gen,1},
           {ss,1},
           {stats,0},
           {test,2}]}.

{attributes, []}.

{labels, 135}.


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
    {call,1,{f,133}}.
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


{function, stats, 0, 12}.
  {label,11}.
    {line,[{location,"lc_tests.erl",32}]}.
    {func_info,{atom,lc_tests},{atom,stats},0}.
  {label,12}.
    {move,{literal,[memory,heap_size,total_heap_size,stack_size,
                    garbage_collection]},
          {x,0}}.
    return.


{function, test, 2, 14}.
  {label,13}.
    {line,[{location,"lc_tests.erl",34}]}.
    {func_info,{atom,lc_tests},{atom,test},2}.
  {label,14}.
    {test,is_list,{f,18},[{x,1}]}.
    {allocate,1,2}.
    {move,{x,1},{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{x,2},{x,0}}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,128},0,0,2}.
    {line,[{location,"lc_tests.erl",35}]}.
    {call_ext,1,{extfunc,erlang,spawn,1}}.
    {test_heap,3,1}.
    {bif,self,{f,0},[],{x,1}}.
    {put_tuple,2,{x,2}}.
    {put,{x,1}}.
    {put,{y,0}}.
    {move,{x,2},{x,1}}.
    {kill,{y,0}}.
    {line,[{location,"lc_tests.erl",48}]}.
    send.
    {line,[{location,"lc_tests.erl",49}]}.
  {label,15}.
    {loop_rec,{f,17},{x,0}}.
    {test,is_tuple,{f,16},[{x,0}]}.
    {test,test_arity,{f,16},[{x,0},3]}.
    {get_tuple_element,{x,0},0,{x,1}}.
    {get_tuple_element,{x,0},1,{x,2}}.
    {get_tuple_element,{x,0},2,{x,3}}.
    remove_message.
    {test_heap,6,4}.
    {put_tuple,3,{x,0}}.
    {put,{x,1}}.
    {put,{x,2}}.
    {put,{x,3}}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{literal,"~p~n"},{x,0}}.
    {line,[{location,"lc_tests.erl",50}]}.
    {call_ext_last,2,{extfunc,io,format,2},1}.
  {label,16}.
    {loop_rec_end,{f,15}}.
  {label,17}.
    {wait,{f,15}}.
  {label,18}.
    {test_heap,2,2}.
    {put_list,{x,1},nil,{x,1}}.
    {call_only,2,{f,14}}.


{function, ss, 1, 20}.
  {label,19}.
    {line,[{location,"lc_tests.erl",58}]}.
    {func_info,{atom,lc_tests},{atom,ss},1}.
  {label,20}.
    {allocate,0,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests.erl",59}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {line,[{location,"lc_tests.erl",59}]}.
    {call,1,{f,125}}.
    {line,[{location,"lc_tests.erl",59}]}.
    {call_ext_last,1,{extfunc,lists,sum,1},0}.


{function, ses, 1, 22}.
  {label,21}.
    {line,[{location,"lc_tests.erl",62}]}.
    {func_info,{atom,lc_tests},{atom,ses},1}.
  {label,22}.
    {allocate,0,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests.erl",63}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {line,[{location,"lc_tests.erl",63}]}.
    {call,1,{f,121}}.
    {line,[{location,"lc_tests.erl",63}]}.
    {call_ext_last,1,{extfunc,lists,sum,1},0}.


{function, pen, 1, 24}.
  {label,23}.
    {line,[{location,"lc_tests.erl",67}]}.
    {func_info,{atom,lc_tests},{atom,pen},1}.
  {label,24}.
    {allocate,1,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests.erl",68}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,0},{x,1}}.
    {call_last,2,{f,111},1}.


{function, pen2, 1, 26}.
  {label,25}.
    {line,[{location,"lc_tests.erl",74}]}.
    {func_info,{atom,lc_tests},{atom,pen2},1}.
  {label,26}.
    {allocate,1,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests.erl",75}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,0},{x,1}}.
    {call_last,2,{f,101},1}.


{function, single_filter, 1, 28}.
  {label,27}.
    {line,[{location,"lc_tests.erl",84}]}.
    {func_info,{atom,lc_tests},{atom,single_filter},1}.
  {label,28}.
    {call_only,1,{f,97}}.


{function, record_extract, 1, 30}.
  {label,29}.
    {line,[{location,"lc_tests.erl",90}]}.
    {func_info,{atom,lc_tests},{atom,record_extract},1}.
  {label,30}.
    {call_only,1,{f,93}}.


{function, function_param_lc, 2, 32}.
  {label,31}.
    {line,[{location,"lc_tests.erl",94}]}.
    {func_info,{atom,lc_tests},{atom,function_param_lc},2}.
  {label,32}.
    {move,{x,1},{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{x,2},{x,0}}.
    {call_only,2,{f,89}}.


{function, sorted_gen, 1, 34}.
  {label,33}.
    {line,[{location,"lc_tests.erl",98}]}.
    {func_info,{atom,lc_tests},{atom,sorted_gen},1}.
  {label,34}.
    {allocate,1,1}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,82},0,0,0}.
    {move,{y,0},{x,1}}.
    {trim,1,0}.
    {line,[{location,"lc_tests.erl",100}]}.
    {call_ext,2,{extfunc,lists,sort,2}}.
    {call_last,1,{f,85},0}.


{function, make_n, 2, 36}.
  {label,35}.
    {line,[{location,"lc_tests.erl",108}]}.
    {func_info,{atom,lc_tests},{atom,make_n},2}.
  {label,36}.
    {allocate,1,2}.
    {move,{x,0},{y,0}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests.erl",109}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,0},{x,1}}.
    {call_last,2,{f,78},1}.


{function, intermediary_list, 1, 38}.
  {label,37}.
    {line,[{location,"lc_tests.erl",112}]}.
    {func_info,{atom,lc_tests},{atom,intermediary_list},1}.
  {label,38}.
    {allocate,0,1}.
    {move,{integer,50},{x,1}}.
    {line,[{location,"lc_tests.erl",113}]}.
    {call,2,{f,9}}.
    {call_last,1,{f,74},0}.


{function, intermediary_list2, 1, 40}.
  {label,39}.
    {line,[{location,"lc_tests.erl",116}]}.
    {func_info,{atom,lc_tests},{atom,intermediary_list2},1}.
  {label,40}.
    {allocate,0,1}.
    {move,{integer,50},{x,1}}.
    {line,[{location,"lc_tests.erl",117}]}.
    {call,2,{f,9}}.
    {line,[{location,"lc_tests.erl",118}]}.
    {call,1,{f,70}}.
    {call_last,1,{f,67},0}.


{function, intermediary_list3, 1, 42}.
  {label,41}.
    {line,[{location,"lc_tests.erl",121}]}.
    {func_info,{atom,lc_tests},{atom,intermediary_list3},1}.
  {label,42}.
    {allocate,0,1}.
    {move,{integer,50},{x,1}}.
    {line,[{location,"lc_tests.erl",122}]}.
    {call,2,{f,9}}.
    {line,[{location,"lc_tests.erl",123}]}.
    {call,1,{f,64}}.
    {line,[{location,"lc_tests.erl",124}]}.
    {call,1,{f,61}}.
    {call_last,1,{f,57},0}.


{function, nested_lc_gen, 1, 44}.
  {label,43}.
    {line,[{location,"lc_tests.erl",131}]}.
    {func_info,{atom,lc_tests},{atom,nested_lc_gen},1}.
  {label,44}.
    {allocate,0,1}.
    {line,[{location,"lc_tests.erl",133}]}.
    {call,1,{f,50}}.
    {call_last,1,{f,54},0}.


{function, module_info, 0, 46}.
  {label,45}.
    {line,[]}.
    {func_info,{atom,lc_tests},{atom,module_info},0}.
  {label,46}.
    {move,{atom,lc_tests},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 48}.
  {label,47}.
    {line,[]}.
    {func_info,{atom,lc_tests},{atom,module_info},1}.
  {label,48}.
    {move,{x,0},{x,1}}.
    {move,{atom,lc_tests},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.


{function, '-nested_lc_gen/1-lc$^0/1-1-', 1, 50}.
  {label,49}.
    {line,[{location,"lc_tests.erl",133}]}.
    {func_info,{atom,lc_tests},{atom,'-nested_lc_gen/1-lc$^0/1-1-'},1}.
  {label,50}.
    {test,is_nonempty_list,{f,52},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {gc_bif,'rem',{f,51},3,[{x,1},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,51},[{x,3},{integer,1}]}.
    {allocate,1,3}.
    {move,{x,2},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests.erl",133}]}.
    {call,1,{f,50}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,51}.
    {move,{x,2},{x,0}}.
    {call_only,1,{f,50}}.
  {label,52}.
    {test,is_nil,{f,49},[{x,0}]}.
    return.


{function, '-nested_lc_gen/1-lc$^1/1-0-', 1, 54}.
  {label,53}.
    {line,[{location,"lc_tests.erl",132}]}.
    {func_info,{atom,lc_tests},{atom,'-nested_lc_gen/1-lc$^1/1-0-'},1}.
  {label,54}.
    {test,is_nonempty_list,{f,55},[{x,0}]}.
    {allocate_zero,2,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {line,[{location,"lc_tests.erl",132}]}.
    {gc_bif,'*',{f,0},3,[{x,1},{x,1}],{y,0}}.
    {move,{x,2},{x,0}}.
    {move,{x,1},{y,1}}.
    {line,[{location,"lc_tests.erl",133}]}.
    {call,1,{f,54}}.
    {test_heap,6,1}.
    {put_tuple,3,{x,1}}.
    {put,{atom,test_record}}.
    {put,{y,1}}.
    {put,{y,0}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,55}.
    {test,is_nil,{f,53},[{x,0}]}.
    return.


{function, '-intermediary_list3/1-lc$^2/1-2-', 1, 57}.
  {label,56}.
    {line,[{location,"lc_tests.erl",125}]}.
    {func_info,{atom,lc_tests},{atom,'-intermediary_list3/1-lc$^2/1-2-'},1}.
  {label,57}.
    {test,is_nonempty_list,{f,59},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_tuple,{f,58},[{x,1}]}.
    {test,test_arity,{f,58},[{x,1},2]}.
    {get_tuple_element,{x,1},0,{x,3}}.
    {get_tuple_element,{x,1},1,{x,4}}.
    {test,is_tuple,{f,58},[{x,4}]}.
    {test,test_arity,{f,58},[{x,4},2]}.
    {get_tuple_element,{x,4},0,{x,5}}.
    {get_tuple_element,{x,4},1,{x,6}}.
    {test,is_tuple,{f,58},[{x,6}]}.
    {test,test_arity,{f,58},[{x,6},3]}.
    {get_tuple_element,{x,6},0,{x,7}}.
    {get_tuple_element,{x,6},1,{x,8}}.
    {get_tuple_element,{x,6},2,{x,9}}.
    {test,is_eq_exact,{f,58},[{x,7},{atom,test_record}]}.
    {allocate_zero,1,10}.
    {line,[{location,"lc_tests.erl",125}]}.
    {gc_bif,'*',{f,0},10,[{x,3},{x,5}],{x,0}}.
    {line,[{location,"lc_tests.erl",125}]}.
    {gc_bif,'*',{f,0},10,[{x,0},{x,8}],{x,0}}.
    {line,[{location,"lc_tests.erl",125}]}.
    {gc_bif,'*',{f,0},10,[{x,0},{x,9}],{y,0}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests.erl",125}]}.
    {call,1,{f,57}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,58}.
    {move,{x,2},{x,0}}.
    {call_only,1,{f,57}}.
  {label,59}.
    {test,is_nil,{f,56},[{x,0}]}.
    return.


{function, '-intermediary_list3/1-lc$^1/1-1-', 1, 61}.
  {label,60}.
    {line,[{location,"lc_tests.erl",124}]}.
    {func_info,{atom,lc_tests},{atom,'-intermediary_list3/1-lc$^1/1-1-'},1}.
  {label,61}.
    {test,is_nonempty_list,{f,62},[{x,0}]}.
    {allocate,2,1}.
    {get_list,{x,0},{y,1},{y,0}}.
    {move,{integer,100},{x,0}}.
    {line,[{location,"lc_tests.erl",124}]}.
    {call_ext,1,{extfunc,random,uniform,1}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests.erl",124}]}.
    {call,1,{f,61}}.
    {test_heap,5,1}.
    {put_tuple,2,{x,1}}.
    {put,{y,0}}.
    {put,{y,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,62}.
    {test,is_nil,{f,60},[{x,0}]}.
    return.


{function, '-intermediary_list3/1-lc$^0/1-0-', 1, 64}.
  {label,63}.
    {line,[{location,"lc_tests.erl",123}]}.
    {func_info,{atom,lc_tests},{atom,'-intermediary_list3/1-lc$^0/1-0-'},1}.
  {label,64}.
    {test,is_nonempty_list,{f,65},[{x,0}]}.
    {allocate,2,1}.
    {get_list,{x,0},{y,1},{y,0}}.
    {move,{integer,50},{x,0}}.
    {line,[{location,"lc_tests.erl",123}]}.
    {call_ext,1,{extfunc,random,uniform,1}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests.erl",123}]}.
    {call,1,{f,64}}.
    {test_heap,5,1}.
    {put_tuple,2,{x,1}}.
    {put,{y,0}}.
    {put,{y,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,65}.
    {test,is_nil,{f,63},[{x,0}]}.
    return.


{function, '-intermediary_list2/1-lc$^1/1-1-', 1, 67}.
  {label,66}.
    {line,[{location,"lc_tests.erl",119}]}.
    {func_info,{atom,lc_tests},{atom,'-intermediary_list2/1-lc$^1/1-1-'},1}.
  {label,67}.
    {test,is_nonempty_list,{f,68},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {line,[{location,"lc_tests.erl",119}]}.
    {gc_bif,'*',{f,0},3,[{x,1},{x,1}],{y,0}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests.erl",119}]}.
    {call,1,{f,67}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,68}.
    {test,is_nil,{f,66},[{x,0}]}.
    return.


{function, '-intermediary_list2/1-lc$^0/1-0-', 1, 70}.
  {label,69}.
    {line,[{location,"lc_tests.erl",118}]}.
    {func_info,{atom,lc_tests},{atom,'-intermediary_list2/1-lc$^0/1-0-'},1}.
  {label,70}.
    {test,is_nonempty_list,{f,71},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_tuple,{f,72},[{x,1}]}.
    {test,test_arity,{f,72},[{x,1},3]}.
    {get_tuple_element,{x,1},0,{x,0}}.
    {get_tuple_element,{x,1},2,{x,3}}.
    {test,is_eq_exact,{f,72},[{x,0},{atom,test_record}]}.
    {move,{x,2},{x,0}}.
    {move,{x,3},{y,0}}.
    {line,[{location,"lc_tests.erl",118}]}.
    {call,1,{f,70}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,71}.
    {test,is_nil,{f,69},[{x,0}]}.
    return.
  {label,72}.
    {move,{literal,{badrecord,test_record}},{x,0}}.
    {line,[{location,"lc_tests.erl",118}]}.
    {call_ext,1,{extfunc,erlang,error,1}}.


{function, '-intermediary_list/1-lc$^0/1-0-', 1, 74}.
  {label,73}.
    {line,[{location,"lc_tests.erl",114}]}.
    {func_info,{atom,lc_tests},{atom,'-intermediary_list/1-lc$^0/1-0-'},1}.
  {label,74}.
    {test,is_nonempty_list,{f,75},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_tuple,{f,76},[{x,1}]}.
    {test,test_arity,{f,76},[{x,1},3]}.
    {get_tuple_element,{x,1},0,{x,0}}.
    {get_tuple_element,{x,1},2,{x,3}}.
    {test,is_eq_exact,{f,76},[{x,0},{atom,test_record}]}.
    {move,{x,2},{x,0}}.
    {move,{x,3},{y,0}}.
    {line,[{location,"lc_tests.erl",114}]}.
    {call,1,{f,74}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,75}.
    {test,is_nil,{f,73},[{x,0}]}.
    return.
  {label,76}.
    {move,{literal,{badrecord,test_record}},{x,0}}.
    {line,[{location,"lc_tests.erl",114}]}.
    {call_ext,1,{extfunc,erlang,error,1}}.


{function, '-make_n/2-lc$^0/1-0-', 2, 78}.
  {label,77}.
    {line,[{location,"lc_tests.erl",109}]}.
    {func_info,{atom,lc_tests},{atom,'-make_n/2-lc$^0/1-0-'},2}.
  {label,78}.
    {test,is_nonempty_list,{f,79},[{x,0}]}.
    {allocate,1,2}.
    {get_list,{x,0},{x,2},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests.erl",109}]}.
    {call,2,{f,78}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,79}.
    {test,is_nil,{f,80},[{x,0}]}.
    return.
  {label,80}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests.erl",109}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-sorted_gen/1-fun-0-', 2, 82}.
  {label,81}.
    {line,[{location,"lc_tests.erl",100}]}.
    {func_info,{atom,lc_tests},{atom,'-sorted_gen/1-fun-0-'},2}.
  {label,82}.
    {test,is_tuple,{f,83},[{x,0}]}.
    {test,test_arity,{f,83},[{x,0},3]}.
    {get_tuple_element,{x,0},0,{x,2}}.
    {get_tuple_element,{x,0},1,{x,3}}.
    {test,is_eq_exact,{f,83},[{x,2},{atom,test_record}]}.
    {test,is_tuple,{f,83},[{x,1}]}.
    {test,test_arity,{f,83},[{x,1},3]}.
    {get_tuple_element,{x,1},0,{x,0}}.
    {get_tuple_element,{x,1},1,{x,2}}.
    {test,is_eq_exact,{f,83},[{x,0},{atom,test_record}]}.
    {bif,'<',{f,0},[{x,3},{x,2}],{x,0}}.
    return.
  {label,83}.
    {move,{literal,{badrecord,test_record}},{x,0}}.
    {line,[{location,"lc_tests.erl",101}]}.
    {call_ext,1,{extfunc,erlang,error,1}}.


{function, '-sorted_gen/1-lc$^0/1-0-', 1, 85}.
  {label,84}.
    {line,[{location,"lc_tests.erl",99}]}.
    {func_info,{atom,lc_tests},{atom,'-sorted_gen/1-lc$^0/1-0-'},1}.
  {label,85}.
    {test,is_nonempty_list,{f,86},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_tuple,{f,87},[{x,1}]}.
    {test,test_arity,{f,87},[{x,1},3]}.
    {get_tuple_element,{x,1},0,{x,0}}.
    {get_tuple_element,{x,1},1,{x,3}}.
    {test,is_eq_exact,{f,87},[{x,0},{atom,test_record}]}.
    {move,{x,2},{x,0}}.
    {move,{x,3},{y,0}}.
    {line,[{location,"lc_tests.erl",100}]}.
    {call,1,{f,85}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,86}.
    {test,is_nil,{f,84},[{x,0}]}.
    return.
  {label,87}.
    {move,{literal,{badrecord,test_record}},{x,0}}.
    {line,[{location,"lc_tests.erl",99}]}.
    {call_ext,1,{extfunc,erlang,error,1}}.


{function, '-function_param_lc/2-lc$^0/1-0-', 2, 89}.
  {label,88}.
    {line,[{location,"lc_tests.erl",95}]}.
    {func_info,{atom,lc_tests},{atom,'-function_param_lc/2-lc$^0/1-0-'},2}.
  {label,89}.
    {test,is_nonempty_list,{f,90},[{x,0}]}.
    {allocate,2,2}.
    {get_list,{x,0},{y,0},{x,0}}.
    {move,{x,1},{y,1}}.
    {line,[{location,"lc_tests.erl",95}]}.
    {call,2,{f,89}}.
    {test_heap,6,1}.
    {put_list,{y,0},nil,{x,1}}.
    {put_list,{y,1},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,90}.
    {test,is_nil,{f,91},[{x,0}]}.
    return.
  {label,91}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests.erl",95}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-record_extract/1-lc$^0/1-0-', 1, 93}.
  {label,92}.
    {line,[{location,"lc_tests.erl",91}]}.
    {func_info,{atom,lc_tests},{atom,'-record_extract/1-lc$^0/1-0-'},1}.
  {label,93}.
    {test,is_nonempty_list,{f,94},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_tuple,{f,95},[{x,1}]}.
    {test,test_arity,{f,95},[{x,1},3]}.
    {get_tuple_element,{x,1},0,{x,0}}.
    {get_tuple_element,{x,1},1,{x,3}}.
    {test,is_eq_exact,{f,95},[{x,0},{atom,test_record}]}.
    {move,{x,2},{x,0}}.
    {move,{x,3},{y,0}}.
    {line,[{location,"lc_tests.erl",91}]}.
    {call,1,{f,93}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,94}.
    {test,is_nil,{f,92},[{x,0}]}.
    return.
  {label,95}.
    {move,{literal,{badrecord,test_record}},{x,0}}.
    {line,[{location,"lc_tests.erl",91}]}.
    {call_ext,1,{extfunc,erlang,error,1}}.


{function, '-single_filter/1-lc$^0/1-0-', 1, 97}.
  {label,96}.
    {line,[{location,"lc_tests.erl",85}]}.
    {func_info,{atom,lc_tests},{atom,'-single_filter/1-lc$^0/1-0-'},1}.
  {label,97}.
    {test,is_nonempty_list,{f,99},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_integer,{f,98},[{x,1}]}.
    {allocate,1,3}.
    {move,{x,2},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests.erl",85}]}.
    {call,1,{f,97}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,98}.
    {move,{x,2},{x,0}}.
    {call_only,1,{f,97}}.
  {label,99}.
    {test,is_nil,{f,96},[{x,0}]}.
    return.


{function, '-pen2/1-lc$^0/1-0-', 2, 101}.
  {label,100}.
    {line,[{location,"lc_tests.erl",75}]}.
    {func_info,{atom,lc_tests},{atom,'-pen2/1-lc$^0/1-0-'},2}.
  {label,101}.
    {test,is_nonempty_list,{f,103},[{x,0}]}.
    {get_list,{x,0},{x,2},{x,3}}.
    {gc_bif,'rem',{f,102},4,[{x,2},{integer,2}],{x,4}}.
    {test,is_eq_exact,{f,102},[{x,4},{integer,0}]}.
    {allocate,3,4}.
    {move,{integer,1},{x,0}}.
    {move,{x,1},{y,0}}.
    {move,{x,3},{y,1}}.
    {move,{x,2},{y,2}}.
    {line,[{location,"lc_tests.erl",77}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,1},{x,2}}.
    {move,{y,2},{x,3}}.
    {move,{y,0},{x,1}}.
    {call_last,4,{f,106},3}.
  {label,102}.
    {move,{x,3},{x,0}}.
    {call_only,2,{f,101}}.
  {label,103}.
    {test,is_nil,{f,104},[{x,0}]}.
    return.
  {label,104}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests.erl",75}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pen2/1-lc$^1/1-1-', 4, 106}.
  {label,105}.
    {line,[{location,"lc_tests.erl",75}]}.
    {func_info,{atom,lc_tests},{atom,'-pen2/1-lc$^1/1-1-'},4}.
  {label,106}.
    {test,is_nonempty_list,{f,108},[{x,0}]}.
    {get_list,{x,0},{x,4},{x,5}}.
    {gc_bif,'rem',{f,107},6,[{x,4},{integer,2}],{x,6}}.
    {gc_bif,'+',{f,107},7,[{x,3},{x,4}],{x,7}}.
    {gc_bif,'rem',{f,107},8,[{x,7},{integer,5}],{x,8}}.
    {test,is_eq_exact,{f,107},[{x,6},{integer,1}]}.
    {test,is_ne_exact,{f,107},[{x,8},{integer,0}]}.
    {allocate,2,6}.
    {move,{x,5},{x,0}}.
    {move,{x,4},{y,0}}.
    {move,{x,3},{y,1}}.
    {line,[{location,"lc_tests.erl",77}]}.
    {call,4,{f,106}}.
    {test_heap,6,1}.
    {put_list,{y,0},nil,{x,1}}.
    {put_list,{y,1},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,107}.
    {move,{x,5},{x,0}}.
    {call_only,4,{f,106}}.
  {label,108}.
    {test,is_nil,{f,109},[{x,0}]}.
    {move,{x,2},{x,0}}.
    {call_only,2,{f,101}}.
  {label,109}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests.erl",75}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pen/1-lc$^0/1-0-', 2, 111}.
  {label,110}.
    {line,[{location,"lc_tests.erl",68}]}.
    {func_info,{atom,lc_tests},{atom,'-pen/1-lc$^0/1-0-'},2}.
  {label,111}.
    {test,is_nonempty_list,{f,113},[{x,0}]}.
    {get_list,{x,0},{x,2},{x,3}}.
    {gc_bif,'rem',{f,112},4,[{x,2},{integer,2}],{x,4}}.
    {test,is_eq_exact,{f,112},[{x,4},{integer,0}]}.
    {allocate,3,4}.
    {move,{integer,1},{x,0}}.
    {move,{x,1},{y,0}}.
    {move,{x,3},{y,1}}.
    {move,{x,2},{y,2}}.
    {line,[{location,"lc_tests.erl",70}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,1},{x,2}}.
    {move,{y,2},{x,3}}.
    {move,{y,0},{x,1}}.
    {call_last,4,{f,116},3}.
  {label,112}.
    {move,{x,3},{x,0}}.
    {call_only,2,{f,111}}.
  {label,113}.
    {test,is_nil,{f,114},[{x,0}]}.
    return.
  {label,114}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests.erl",68}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pen/1-lc$^1/1-1-', 4, 116}.
  {label,115}.
    {line,[{location,"lc_tests.erl",68}]}.
    {func_info,{atom,lc_tests},{atom,'-pen/1-lc$^1/1-1-'},4}.
  {label,116}.
    {test,is_nonempty_list,{f,118},[{x,0}]}.
    {get_list,{x,0},{x,4},{x,5}}.
    {gc_bif,'rem',{f,117},6,[{x,4},{integer,2}],{x,6}}.
    {test,is_eq_exact,{f,117},[{x,6},{integer,1}]}.
    {allocate,2,6}.
    {move,{x,5},{x,0}}.
    {move,{x,4},{y,0}}.
    {move,{x,3},{y,1}}.
    {line,[{location,"lc_tests.erl",70}]}.
    {call,4,{f,116}}.
    {test_heap,6,1}.
    {put_list,{y,0},nil,{x,1}}.
    {put_list,{y,1},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,117}.
    {move,{x,5},{x,0}}.
    {call_only,4,{f,116}}.
  {label,118}.
    {test,is_nil,{f,119},[{x,0}]}.
    {move,{x,2},{x,0}}.
    {call_only,2,{f,111}}.
  {label,119}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests.erl",68}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-ses/1-lc$^0/1-0-', 1, 121}.
  {label,120}.
    {line,[{location,"lc_tests.erl",63}]}.
    {func_info,{atom,lc_tests},{atom,'-ses/1-lc$^0/1-0-'},1}.
  {label,121}.
    {test,is_nonempty_list,{f,123},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {gc_bif,'rem',{f,122},3,[{x,1},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,122},[{x,3},{integer,0}]}.
    {allocate_zero,1,3}.
    {line,[{location,"lc_tests.erl",63}]}.
    {gc_bif,'*',{f,0},3,[{x,1},{x,1}],{y,0}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests.erl",63}]}.
    {call,1,{f,121}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,122}.
    {move,{x,2},{x,0}}.
    {call_only,1,{f,121}}.
  {label,123}.
    {test,is_nil,{f,120},[{x,0}]}.
    return.


{function, '-ss/1-lc$^0/1-0-', 1, 125}.
  {label,124}.
    {line,[{location,"lc_tests.erl",59}]}.
    {func_info,{atom,lc_tests},{atom,'-ss/1-lc$^0/1-0-'},1}.
  {label,125}.
    {test,is_nonempty_list,{f,126},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {line,[{location,"lc_tests.erl",59}]}.
    {gc_bif,'*',{f,0},3,[{x,1},{x,1}],{y,0}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests.erl",59}]}.
    {call,1,{f,125}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,126}.
    {test,is_nil,{f,124},[{x,0}]}.
    return.


{function, '-test/2-fun-0-', 2, 128}.
  {label,127}.
    {line,[{location,"lc_tests.erl",36}]}.
    {func_info,{atom,lc_tests},{atom,'-test/2-fun-0-'},2}.
  {label,128}.
    {allocate_zero,4,2}.
    {move,{x,0},{y,2}}.
    {move,{x,1},{y,3}}.
    {line,[{location,"lc_tests.erl",37}]}.
  {label,129}.
    {loop_rec,{f,131},{x,0}}.
    {test,is_tuple,{f,130},[{x,0}]}.
    {test,test_arity,{f,130},[{x,0},2]}.
    {get_tuple_element,{x,0},0,{x,1}}.
    {get_tuple_element,{x,0},1,{x,2}}.
    {test,is_eq_exact,{f,130},[{x,2},{y,2}]}.
    remove_message.
    {bif,self,{f,0},[],{y,0}}.
    {move,{x,1},{y,1}}.
    {line,[{location,"lc_tests.erl",40}]}.
    {call,0,{f,12}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {trim,1,3}.
    {line,[{location,"lc_tests.erl",40}]}.
    {call_ext,2,{extfunc,erlang,process_info,2}}.
    {move,{x,0},{x,2}}.
    {move,{y,1},{x,1}}.
    {move,{y,2},{x,0}}.
    {move,{x,2},{y,2}}.
    {kill,{y,1}}.
    {line,[{location,"lc_tests.erl",41}]}.
    {call_ext,2,{extfunc,erlang,apply,2}}.
    {bif,self,{f,0},[],{y,1}}.
    {line,[{location,"lc_tests.erl",43}]}.
    {call,0,{f,12}}.
    {move,{x,0},{x,1}}.
    {move,{y,1},{x,0}}.
    {kill,{y,1}}.
    {line,[{location,"lc_tests.erl",43}]}.
    {call_ext,2,{extfunc,erlang,process_info,2}}.
    {test_heap,4,1}.
    {bif,self,{f,0},[],{x,1}}.
    {put_tuple,3,{x,2}}.
    {put,{x,1}}.
    {put,{y,2}}.
    {put,{x,0}}.
    {move,{x,2},{x,1}}.
    {move,{y,0},{x,0}}.
    {line,[{location,"lc_tests.erl",44}]}.
    send.
    {deallocate,3}.
    return.
  {label,130}.
    {loop_rec_end,{f,129}}.
  {label,131}.
    {wait,{f,129}}.


{function, '-random_bin/1-lc$^0/1-0-', 1, 133}.
  {label,132}.
    {line,[{location,"lc_tests.erl",18}]}.
    {func_info,{atom,lc_tests},{atom,'-random_bin/1-lc$^0/1-0-'},1}.
  {label,133}.
    {test,is_nonempty_list,{f,134},[{x,0}]}.
    {allocate,1,1}.
    {get_list,{x,0},{x,1},{y,0}}.
    {move,{integer,255},{x,0}}.
    {line,[{location,"lc_tests.erl",18}]}.
    {call_ext,1,{extfunc,random,uniform,1}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests.erl",18}]}.
    {call,1,{f,133}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,134}.
    {test,is_nil,{f,132},[{x,0}]}.
    return.
