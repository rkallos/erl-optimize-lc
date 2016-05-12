{module, lc_tests_opt}.  %% version = 0

{exports, [{all_tests,0},
           {function_param_lc,2},
           {int_arg,0},
           {intermediary_list,1},
           {intermediary_list2,1},
           {intermediary_list3,1},
           {list_arg,0},
           {make_n,2},
           {module_info,0},
           {module_info,1},
           {nested_lc_gen,1},
           {pen,1},
           {pen2,1},
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
           {test,3}]}.

{attributes, []}.

{labels, 171}.


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
    {call,1,{f,169}}.
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


{function, stats, 0, 12}.
  {label,11}.
    {line,[{location,"lc_tests_opt.erl",32}]}.
    {func_info,{atom,lc_tests_opt},{atom,stats},0}.
  {label,12}.
    {move,{literal,[memory,heap_size,total_heap_size,stack_size,
                    garbage_collection]},
          {x,0}}.
    return.


{function, test, 3, 14}.
  {label,13}.
    {line,[{location,"lc_tests_opt.erl",34}]}.
    {func_info,{atom,lc_tests_opt},{atom,test},3}.
  {label,14}.
    {test,is_list,{f,18},[{x,2}]}.
    {allocate_zero,1,3}.
    {bif,self,{f,0},[],{x,3}}.
    {move,{x,0},{y,0}}.
    {move,{x,1},{x,5}}.
    {move,{x,2},{x,1}}.
    {move,{x,5},{x,2}}.
    {move,{x,3},{x,0}}.
    {make_fun2,{f,167},0,0,3}.
    {line,[{location,"lc_tests_opt.erl",36}]}.
    {call_ext,1,{extfunc,erlang,spawn,1}}.
    {line,[{location,"lc_tests_opt.erl",44}]}.
  {label,15}.
    {loop_rec,{f,17},{x,0}}.
    {test,is_tuple,{f,16},[{x,0}]}.
    {test,test_arity,{f,16},[{x,0},3]}.
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
  {label,16}.
    {loop_rec_end,{f,15}}.
  {label,17}.
    {wait,{f,15}}.
  {label,18}.
    {test_heap,2,3}.
    {put_list,{x,2},nil,{x,2}}.
    {call_only,3,{f,14}}.


{function, run_tests, 2, 20}.
  {label,19}.
    {line,[{location,"lc_tests_opt.erl",50}]}.
    {func_info,{atom,lc_tests_opt},{atom,run_tests},2}.
  {label,20}.
    {test,is_nonempty_list,{f,21},[{x,0}]}.
    {get_list,{x,0},{x,2},{x,3}}.
    {test,is_tuple,{f,19},[{x,2}]}.
    {test,test_arity,{f,19},[{x,2},3]}.
    {allocate,2,4}.
    {get_tuple_element,{x,2},0,{x,0}}.
    {get_tuple_element,{x,2},1,{x,5}}.
    {get_tuple_element,{x,2},2,{x,2}}.
    {move,{x,1},{y,0}}.
    {move,{x,5},{x,1}}.
    {move,{x,3},{y,1}}.
    {line,[{location,"lc_tests_opt.erl",53}]}.
    {call,3,{f,14}}.
    {test_heap,2,1}.
    {put_list,{x,0},{y,0},{x,1}}.
    {move,{y,1},{x,0}}.
    {call_last,2,{f,20},2}.
  {label,21}.
    {test,is_nil,{f,19},[{x,0}]}.
    {move,{x,1},{x,0}}.
    return.


{function, run_tests, 1, 23}.
  {label,22}.
    {line,[{location,"lc_tests_opt.erl",56}]}.
    {func_info,{atom,lc_tests_opt},{atom,run_tests},1}.
  {label,23}.
    {test,is_list,{f,22},[{x,0}]}.
    {move,nil,{x,1}}.
    {call_only,2,{f,20}}.


{function, int_arg, 0, 25}.
  {label,24}.
    {line,[{location,"lc_tests_opt.erl",59}]}.
    {func_info,{atom,lc_tests_opt},{atom,int_arg},0}.
  {label,25}.
    {move,{integer,50},{x,0}}.
    return.


{function, list_arg, 0, 27}.
  {label,26}.
    {line,[{location,"lc_tests_opt.erl",61}]}.
    {func_info,{atom,lc_tests_opt},{atom,list_arg},0}.
  {label,27}.
    {move,{integer,50},{x,1}}.
    {move,{integer,50},{x,0}}.
    {call_only,2,{f,6}}.


{function, record_arg, 0, 29}.
  {label,28}.
    {line,[{location,"lc_tests_opt.erl",63}]}.
    {func_info,{atom,lc_tests_opt},{atom,record_arg},0}.
  {label,29}.
    {move,{integer,50},{x,1}}.
    {move,{integer,50},{x,0}}.
    {call_only,2,{f,9}}.


{function, all_tests, 0, 31}.
  {label,30}.
    {line,[{location,"lc_tests_opt.erl",66}]}.
    {func_info,{atom,lc_tests_opt},{atom,all_tests},0}.
  {label,31}.
    {allocate_zero,17,0}.
    {move,{integer,50},{x,1}}.
    {move,{integer,500},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",68}]}.
    {call,2,{f,6}}.
    {move,{x,0},{y,16}}.
    {move,{integer,50},{x,1}}.
    {move,{integer,500},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",69}]}.
    {call,2,{f,9}}.
    {move,{x,0},{y,15}}.
    {make_fun2,{f,165},0,0,0}.
    {move,{x,0},{y,14}}.
    {make_fun2,{f,163},0,0,0}.
    {move,{x,0},{y,13}}.
    {make_fun2,{f,161},0,0,0}.
    {move,{x,0},{y,12}}.
    {make_fun2,{f,159},0,0,0}.
    {move,{x,0},{y,11}}.
    {make_fun2,{f,157},0,0,0}.
    {test_heap,2,1}.
    {put_list,{y,16},nil,{y,9}}.
    {move,{x,0},{y,10}}.
    {make_fun2,{f,155},0,0,0}.
    {test_heap,2,1}.
    {put_list,{y,15},nil,{y,7}}.
    {move,{x,0},{y,8}}.
    {make_fun2,{f,153},0,0,0}.
    {test_heap,4,1}.
    {put_list,{y,16},nil,{x,1}}.
    {put_list,{atom,param},{x,1},{y,5}}.
    {move,{x,0},{y,6}}.
    {make_fun2,{f,151},0,0,0}.
    {test_heap,2,1}.
    {put_list,{y,15},nil,{y,4}}.
    {move,{x,0},{y,15}}.
    {make_fun2,{f,149},0,0,0}.
    {move,{x,0},{y,3}}.
    {make_fun2,{f,147},0,0,0}.
    {move,{x,0},{y,2}}.
    {make_fun2,{f,145},0,0,0}.
    {move,{x,0},{y,1}}.
    {make_fun2,{f,143},0,0,0}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,141},0,0,0}.
    {test_heap,80,1}.
    {put_list,{y,16},nil,{x,1}}.
    {put_tuple,3,{x,2}}.
    {put,{literal,"nested_lc_gen"}}.
    {put,{x,0}}.
    {put,{x,1}}.
    {put_list,{x,2},nil,{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"intermediary_list3"}}.
    {put,{y,0}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"intermediary_list2"}}.
    {put,{y,1}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"intermediary_list"}}.
    {put,{y,2}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"make_n"}}.
    {put,{y,3}}.
    {put,{literal,[{param1,param2},500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"sorted_gen"}}.
    {put,{y,15}}.
    {put,{y,4}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"function_param"}}.
    {put,{y,6}}.
    {put,{y,5}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"record_extract"}}.
    {put,{y,8}}.
    {put,{y,7}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"single_filter"}}.
    {put,{y,10}}.
    {put,{y,9}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"pen2"}}.
    {put,{y,11}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"pen"}}.
    {put,{y,12}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {put_tuple,3,{x,1}}.
    {put,{literal,"ses"}}.
    {put,{y,13}}.
    {put,{literal,[500]}}.
    {put_list,{x,1},{x,0},{x,1}}.
    {put_tuple,3,{x,2}}.
    {put,{literal,"ss"}}.
    {put,{y,14}}.
    {put,{literal,[500]}}.
    {put_list,{x,2},{x,1},{x,0}}.
    {deallocate,17}.
    return.


{function, ss, 1, 33}.
  {label,32}.
    {line,[{location,"lc_tests_opt.erl",88}]}.
    {func_info,{atom,lc_tests_opt},{atom,ss},1}.
  {label,33}.
    {test,is_integer,{f,32},[{x,0}]}.
    {allocate,0,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",89}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {line,[{location,"lc_tests_opt.erl",89}]}.
    {call,1,{f,138}}.
    {line,[{location,"lc_tests_opt.erl",89}]}.
    {call_ext_last,1,{extfunc,lists,sum,1},0}.


{function, ses, 1, 35}.
  {label,34}.
    {line,[{location,"lc_tests_opt.erl",92}]}.
    {func_info,{atom,lc_tests_opt},{atom,ses},1}.
  {label,35}.
    {test,is_integer,{f,34},[{x,0}]}.
    {allocate,0,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",93}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {line,[{location,"lc_tests_opt.erl",93}]}.
    {call,1,{f,134}}.
    {line,[{location,"lc_tests_opt.erl",93}]}.
    {call_ext_last,1,{extfunc,lists,sum,1},0}.


{function, pen, 1, 37}.
  {label,36}.
    {line,[{location,"lc_tests_opt.erl",97}]}.
    {func_info,{atom,lc_tests_opt},{atom,pen},1}.
  {label,37}.
    {test,is_integer,{f,36},[{x,0}]}.
    {allocate,1,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",98}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,0},{x,1}}.
    {call_last,2,{f,124},1}.


{function, pen2, 1, 39}.
  {label,38}.
    {line,[{location,"lc_tests_opt.erl",104}]}.
    {func_info,{atom,lc_tests_opt},{atom,pen2},1}.
  {label,39}.
    {test,is_integer,{f,38},[{x,0}]}.
    {allocate,1,1}.
    {move,{x,0},{x,1}}.
    {move,{integer,1},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",105}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,0},{x,1}}.
    {call_last,2,{f,114},1}.


{function, single_filter, 1, 41}.
  {label,40}.
    {line,[{location,"lc_tests_opt.erl",114}]}.
    {func_info,{atom,lc_tests_opt},{atom,single_filter},1}.
  {label,41}.
    {test,is_list,{f,40},[{x,0}]}.
    {call_only,1,{f,110}}.


{function, record_extract, 1, 43}.
  {label,42}.
    {line,[{location,"lc_tests_opt.erl",120}]}.
    {func_info,{atom,lc_tests_opt},{atom,record_extract},1}.
  {label,43}.
    {test,is_list,{f,42},[{x,0}]}.
    {call_only,1,{f,106}}.


{function, function_param_lc, 2, 45}.
  {label,44}.
    {line,[{location,"lc_tests_opt.erl",124}]}.
    {func_info,{atom,lc_tests_opt},{atom,function_param_lc},2}.
  {label,45}.
    {test,is_list,{f,44},[{x,1}]}.
    {move,{x,1},{x,2}}.
    {move,{x,0},{x,1}}.
    {move,{x,2},{x,0}}.
    {call_only,2,{f,102}}.


{function, sorted_gen, 1, 47}.
  {label,46}.
    {line,[{location,"lc_tests_opt.erl",128}]}.
    {func_info,{atom,lc_tests_opt},{atom,sorted_gen},1}.
  {label,47}.
    {test,is_list,{f,46},[{x,0}]}.
    {allocate,1,1}.
    {move,{x,0},{y,0}}.
    {make_fun2,{f,95},0,0,0}.
    {move,{y,0},{x,1}}.
    {trim,1,0}.
    {line,[{location,"lc_tests_opt.erl",130}]}.
    {call_ext,2,{extfunc,lists,sort,2}}.
    {call_last,1,{f,98},0}.


{function, make_n, 2, 49}.
  {label,48}.
    {line,[{location,"lc_tests_opt.erl",138}]}.
    {func_info,{atom,lc_tests_opt},{atom,make_n},2}.
  {label,49}.
    {test,is_integer,{f,48},[{x,1}]}.
    {allocate,1,2}.
    {move,{x,0},{y,0}}.
    {move,{integer,1},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",139}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,0},{x,1}}.
    {call_last,2,{f,91},1}.


{function, intermediary_list, 1, 51}.
  {label,50}.
    {line,[{location,"lc_tests_opt.erl",142}]}.
    {func_info,{atom,lc_tests_opt},{atom,intermediary_list},1}.
  {label,51}.
    {test,is_integer,{f,50},[{x,0}]}.
    {allocate,0,1}.
    {move,{integer,50},{x,1}}.
    {line,[{location,"lc_tests_opt.erl",143}]}.
    {call,2,{f,9}}.
    {call_last,1,{f,87},0}.


{function, intermediary_list2, 1, 53}.
  {label,52}.
    {line,[{location,"lc_tests_opt.erl",146}]}.
    {func_info,{atom,lc_tests_opt},{atom,intermediary_list2},1}.
  {label,53}.
    {test,is_integer,{f,52},[{x,0}]}.
    {allocate,0,1}.
    {move,{integer,50},{x,1}}.
    {line,[{location,"lc_tests_opt.erl",147}]}.
    {call,2,{f,9}}.
    {line,[{location,"lc_tests_opt.erl",148}]}.
    {call,1,{f,83}}.
    {call_last,1,{f,80},0}.


{function, intermediary_list3, 1, 55}.
  {label,54}.
    {line,[{location,"lc_tests_opt.erl",151}]}.
    {func_info,{atom,lc_tests_opt},{atom,intermediary_list3},1}.
  {label,55}.
    {test,is_integer,{f,54},[{x,0}]}.
    {allocate,0,1}.
    {move,{integer,50},{x,1}}.
    {line,[{location,"lc_tests_opt.erl",152}]}.
    {call,2,{f,9}}.
    {line,[{location,"lc_tests_opt.erl",153}]}.
    {call,1,{f,77}}.
    {line,[{location,"lc_tests_opt.erl",154}]}.
    {call,1,{f,74}}.
    {call_last,1,{f,70},0}.


{function, nested_lc_gen, 1, 57}.
  {label,56}.
    {line,[{location,"lc_tests_opt.erl",161}]}.
    {func_info,{atom,lc_tests_opt},{atom,nested_lc_gen},1}.
  {label,57}.
    {test,is_list,{f,56},[{x,0}]}.
    {allocate,0,1}.
    {line,[{location,"lc_tests_opt.erl",163}]}.
    {call,1,{f,63}}.
    {call_last,1,{f,67},0}.


{function, module_info, 0, 59}.
  {label,58}.
    {line,[]}.
    {func_info,{atom,lc_tests_opt},{atom,module_info},0}.
  {label,59}.
    {move,{atom,lc_tests_opt},{x,0}}.
    {line,[]}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 61}.
  {label,60}.
    {line,[]}.
    {func_info,{atom,lc_tests_opt},{atom,module_info},1}.
  {label,61}.
    {move,{x,0},{x,1}}.
    {move,{atom,lc_tests_opt},{x,0}}.
    {line,[]}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.


{function, '-nested_lc_gen/1-lc$^0/1-1-', 1, 63}.
  {label,62}.
    {line,[{location,"lc_tests_opt.erl",163}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-nested_lc_gen/1-lc$^0/1-1-'},1}.
  {label,63}.
    {test,is_nonempty_list,{f,65},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {gc_bif,'rem',{f,64},3,[{x,1},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,64},[{x,3},{integer,1}]}.
    {allocate,1,3}.
    {move,{x,2},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",163}]}.
    {call,1,{f,63}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,64}.
    {move,{x,2},{x,0}}.
    {call_only,1,{f,63}}.
  {label,65}.
    {test,is_nil,{f,62},[{x,0}]}.
    return.


{function, '-nested_lc_gen/1-lc$^1/1-0-', 1, 67}.
  {label,66}.
    {line,[{location,"lc_tests_opt.erl",162}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-nested_lc_gen/1-lc$^1/1-0-'},1}.
  {label,67}.
    {test,is_nonempty_list,{f,68},[{x,0}]}.
    {allocate_zero,2,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {line,[{location,"lc_tests_opt.erl",162}]}.
    {gc_bif,'*',{f,0},3,[{x,1},{x,1}],{y,0}}.
    {move,{x,2},{x,0}}.
    {move,{x,1},{y,1}}.
    {line,[{location,"lc_tests_opt.erl",163}]}.
    {call,1,{f,67}}.
    {test_heap,6,1}.
    {put_tuple,3,{x,1}}.
    {put,{atom,test_record}}.
    {put,{y,1}}.
    {put,{y,0}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,68}.
    {test,is_nil,{f,66},[{x,0}]}.
    return.


{function, '-intermediary_list3/1-lc$^2/1-2-', 1, 70}.
  {label,69}.
    {line,[{location,"lc_tests_opt.erl",155}]}.
    {func_info,{atom,lc_tests_opt},
               {atom,'-intermediary_list3/1-lc$^2/1-2-'},
               1}.
  {label,70}.
    {test,is_nonempty_list,{f,72},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_tuple,{f,71},[{x,1}]}.
    {test,test_arity,{f,71},[{x,1},2]}.
    {get_tuple_element,{x,1},0,{x,3}}.
    {get_tuple_element,{x,1},1,{x,4}}.
    {test,is_tuple,{f,71},[{x,4}]}.
    {test,test_arity,{f,71},[{x,4},2]}.
    {get_tuple_element,{x,4},0,{x,5}}.
    {get_tuple_element,{x,4},1,{x,6}}.
    {test,is_tuple,{f,71},[{x,6}]}.
    {test,test_arity,{f,71},[{x,6},3]}.
    {get_tuple_element,{x,6},0,{x,7}}.
    {get_tuple_element,{x,6},1,{x,8}}.
    {get_tuple_element,{x,6},2,{x,9}}.
    {test,is_eq_exact,{f,71},[{x,7},{atom,test_record}]}.
    {allocate_zero,1,10}.
    {line,[{location,"lc_tests_opt.erl",155}]}.
    {gc_bif,'*',{f,0},10,[{x,3},{x,5}],{x,0}}.
    {line,[{location,"lc_tests_opt.erl",155}]}.
    {gc_bif,'*',{f,0},10,[{x,0},{x,8}],{x,0}}.
    {line,[{location,"lc_tests_opt.erl",155}]}.
    {gc_bif,'*',{f,0},10,[{x,0},{x,9}],{y,0}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",155}]}.
    {call,1,{f,70}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,71}.
    {move,{x,2},{x,0}}.
    {call_only,1,{f,70}}.
  {label,72}.
    {test,is_nil,{f,69},[{x,0}]}.
    return.


{function, '-intermediary_list3/1-lc$^1/1-1-', 1, 74}.
  {label,73}.
    {line,[{location,"lc_tests_opt.erl",154}]}.
    {func_info,{atom,lc_tests_opt},
               {atom,'-intermediary_list3/1-lc$^1/1-1-'},
               1}.
  {label,74}.
    {test,is_nonempty_list,{f,75},[{x,0}]}.
    {allocate,2,1}.
    {get_list,{x,0},{y,1},{y,0}}.
    {move,{integer,100},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",154}]}.
    {call_ext,1,{extfunc,random,uniform,1}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",154}]}.
    {call,1,{f,74}}.
    {test_heap,5,1}.
    {put_tuple,2,{x,1}}.
    {put,{y,0}}.
    {put,{y,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,75}.
    {test,is_nil,{f,73},[{x,0}]}.
    return.


{function, '-intermediary_list3/1-lc$^0/1-0-', 1, 77}.
  {label,76}.
    {line,[{location,"lc_tests_opt.erl",153}]}.
    {func_info,{atom,lc_tests_opt},
               {atom,'-intermediary_list3/1-lc$^0/1-0-'},
               1}.
  {label,77}.
    {test,is_nonempty_list,{f,78},[{x,0}]}.
    {allocate,2,1}.
    {get_list,{x,0},{y,1},{y,0}}.
    {move,{integer,50},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",153}]}.
    {call_ext,1,{extfunc,random,uniform,1}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",153}]}.
    {call,1,{f,77}}.
    {test_heap,5,1}.
    {put_tuple,2,{x,1}}.
    {put,{y,0}}.
    {put,{y,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,78}.
    {test,is_nil,{f,76},[{x,0}]}.
    return.


{function, '-intermediary_list2/1-lc$^1/1-1-', 1, 80}.
  {label,79}.
    {line,[{location,"lc_tests_opt.erl",149}]}.
    {func_info,{atom,lc_tests_opt},
               {atom,'-intermediary_list2/1-lc$^1/1-1-'},
               1}.
  {label,80}.
    {test,is_nonempty_list,{f,81},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {line,[{location,"lc_tests_opt.erl",149}]}.
    {gc_bif,'*',{f,0},3,[{x,1},{x,1}],{y,0}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",149}]}.
    {call,1,{f,80}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,81}.
    {test,is_nil,{f,79},[{x,0}]}.
    return.


{function, '-intermediary_list2/1-lc$^0/1-0-', 1, 83}.
  {label,82}.
    {line,[{location,"lc_tests_opt.erl",148}]}.
    {func_info,{atom,lc_tests_opt},
               {atom,'-intermediary_list2/1-lc$^0/1-0-'},
               1}.
  {label,83}.
    {test,is_nonempty_list,{f,84},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_tuple,{f,85},[{x,1}]}.
    {test,test_arity,{f,85},[{x,1},3]}.
    {get_tuple_element,{x,1},0,{x,0}}.
    {get_tuple_element,{x,1},2,{x,3}}.
    {test,is_eq_exact,{f,85},[{x,0},{atom,test_record}]}.
    {move,{x,2},{x,0}}.
    {move,{x,3},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",148}]}.
    {call,1,{f,83}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,84}.
    {test,is_nil,{f,82},[{x,0}]}.
    return.
  {label,85}.
    {move,{literal,{badrecord,test_record}},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",148}]}.
    {call_ext,1,{extfunc,erlang,error,1}}.


{function, '-intermediary_list/1-lc$^0/1-0-', 1, 87}.
  {label,86}.
    {line,[{location,"lc_tests_opt.erl",144}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-intermediary_list/1-lc$^0/1-0-'},1}.
  {label,87}.
    {test,is_nonempty_list,{f,88},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_tuple,{f,89},[{x,1}]}.
    {test,test_arity,{f,89},[{x,1},3]}.
    {get_tuple_element,{x,1},0,{x,0}}.
    {get_tuple_element,{x,1},2,{x,3}}.
    {test,is_eq_exact,{f,89},[{x,0},{atom,test_record}]}.
    {move,{x,2},{x,0}}.
    {move,{x,3},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",144}]}.
    {call,1,{f,87}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,88}.
    {test,is_nil,{f,86},[{x,0}]}.
    return.
  {label,89}.
    {move,{literal,{badrecord,test_record}},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",144}]}.
    {call_ext,1,{extfunc,erlang,error,1}}.


{function, '-make_n/2-lc$^0/1-0-', 2, 91}.
  {label,90}.
    {line,[{location,"lc_tests_opt.erl",139}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-make_n/2-lc$^0/1-0-'},2}.
  {label,91}.
    {test,is_nonempty_list,{f,92},[{x,0}]}.
    {allocate,1,2}.
    {get_list,{x,0},{x,2},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",139}]}.
    {call,2,{f,91}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,92}.
    {test,is_nil,{f,93},[{x,0}]}.
    return.
  {label,93}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",139}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-sorted_gen/1-fun-0-', 2, 95}.
  {label,94}.
    {line,[{location,"lc_tests_opt.erl",130}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-sorted_gen/1-fun-0-'},2}.
  {label,95}.
    {test,is_tuple,{f,96},[{x,0}]}.
    {test,test_arity,{f,96},[{x,0},3]}.
    {get_tuple_element,{x,0},0,{x,2}}.
    {get_tuple_element,{x,0},1,{x,3}}.
    {test,is_eq_exact,{f,96},[{x,2},{atom,test_record}]}.
    {test,is_tuple,{f,96},[{x,1}]}.
    {test,test_arity,{f,96},[{x,1},3]}.
    {get_tuple_element,{x,1},0,{x,0}}.
    {get_tuple_element,{x,1},1,{x,2}}.
    {test,is_eq_exact,{f,96},[{x,0},{atom,test_record}]}.
    {bif,'<',{f,0},[{x,3},{x,2}],{x,0}}.
    return.
  {label,96}.
    {move,{literal,{badrecord,test_record}},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",131}]}.
    {call_ext,1,{extfunc,erlang,error,1}}.


{function, '-sorted_gen/1-lc$^0/1-0-', 1, 98}.
  {label,97}.
    {line,[{location,"lc_tests_opt.erl",129}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-sorted_gen/1-lc$^0/1-0-'},1}.
  {label,98}.
    {test,is_nonempty_list,{f,99},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_tuple,{f,100},[{x,1}]}.
    {test,test_arity,{f,100},[{x,1},3]}.
    {get_tuple_element,{x,1},0,{x,0}}.
    {get_tuple_element,{x,1},1,{x,3}}.
    {test,is_eq_exact,{f,100},[{x,0},{atom,test_record}]}.
    {move,{x,2},{x,0}}.
    {move,{x,3},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",130}]}.
    {call,1,{f,98}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,99}.
    {test,is_nil,{f,97},[{x,0}]}.
    return.
  {label,100}.
    {move,{literal,{badrecord,test_record}},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",129}]}.
    {call_ext,1,{extfunc,erlang,error,1}}.


{function, '-function_param_lc/2-lc$^0/1-0-', 2, 102}.
  {label,101}.
    {line,[{location,"lc_tests_opt.erl",125}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-function_param_lc/2-lc$^0/1-0-'},2}.
  {label,102}.
    {test,is_nonempty_list,{f,103},[{x,0}]}.
    {allocate,2,2}.
    {get_list,{x,0},{y,0},{x,0}}.
    {move,{x,1},{y,1}}.
    {line,[{location,"lc_tests_opt.erl",125}]}.
    {call,2,{f,102}}.
    {test_heap,6,1}.
    {put_list,{y,0},nil,{x,1}}.
    {put_list,{y,1},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,103}.
    {test,is_nil,{f,104},[{x,0}]}.
    return.
  {label,104}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",125}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-record_extract/1-lc$^0/1-0-', 1, 106}.
  {label,105}.
    {line,[{location,"lc_tests_opt.erl",121}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-record_extract/1-lc$^0/1-0-'},1}.
  {label,106}.
    {test,is_nonempty_list,{f,107},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_tuple,{f,108},[{x,1}]}.
    {test,test_arity,{f,108},[{x,1},3]}.
    {get_tuple_element,{x,1},0,{x,0}}.
    {get_tuple_element,{x,1},1,{x,3}}.
    {test,is_eq_exact,{f,108},[{x,0},{atom,test_record}]}.
    {move,{x,2},{x,0}}.
    {move,{x,3},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",121}]}.
    {call,1,{f,106}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,107}.
    {test,is_nil,{f,105},[{x,0}]}.
    return.
  {label,108}.
    {move,{literal,{badrecord,test_record}},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",121}]}.
    {call_ext,1,{extfunc,erlang,error,1}}.


{function, '-single_filter/1-lc$^0/1-0-', 1, 110}.
  {label,109}.
    {line,[{location,"lc_tests_opt.erl",115}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-single_filter/1-lc$^0/1-0-'},1}.
  {label,110}.
    {test,is_nonempty_list,{f,112},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {test,is_integer,{f,111},[{x,1}]}.
    {allocate,1,3}.
    {move,{x,2},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",115}]}.
    {call,1,{f,110}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,111}.
    {move,{x,2},{x,0}}.
    {call_only,1,{f,110}}.
  {label,112}.
    {test,is_nil,{f,109},[{x,0}]}.
    return.


{function, '-pen2/1-lc$^0/1-0-', 2, 114}.
  {label,113}.
    {line,[{location,"lc_tests_opt.erl",105}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2/1-lc$^0/1-0-'},2}.
  {label,114}.
    {test,is_nonempty_list,{f,116},[{x,0}]}.
    {get_list,{x,0},{x,2},{x,3}}.
    {gc_bif,'rem',{f,115},4,[{x,2},{integer,2}],{x,4}}.
    {test,is_eq_exact,{f,115},[{x,4},{integer,0}]}.
    {allocate,3,4}.
    {move,{integer,1},{x,0}}.
    {move,{x,1},{y,0}}.
    {move,{x,3},{y,1}}.
    {move,{x,2},{y,2}}.
    {line,[{location,"lc_tests_opt.erl",107}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,1},{x,2}}.
    {move,{y,2},{x,3}}.
    {move,{y,0},{x,1}}.
    {call_last,4,{f,119},3}.
  {label,115}.
    {move,{x,3},{x,0}}.
    {call_only,2,{f,114}}.
  {label,116}.
    {test,is_nil,{f,117},[{x,0}]}.
    return.
  {label,117}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",105}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pen2/1-lc$^1/1-1-', 4, 119}.
  {label,118}.
    {line,[{location,"lc_tests_opt.erl",105}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen2/1-lc$^1/1-1-'},4}.
  {label,119}.
    {test,is_nonempty_list,{f,121},[{x,0}]}.
    {get_list,{x,0},{x,4},{x,5}}.
    {gc_bif,'rem',{f,120},6,[{x,4},{integer,2}],{x,6}}.
    {gc_bif,'+',{f,120},7,[{x,3},{x,4}],{x,7}}.
    {gc_bif,'rem',{f,120},8,[{x,7},{integer,5}],{x,8}}.
    {test,is_eq_exact,{f,120},[{x,6},{integer,1}]}.
    {test,is_ne_exact,{f,120},[{x,8},{integer,0}]}.
    {allocate,2,6}.
    {move,{x,5},{x,0}}.
    {move,{x,4},{y,0}}.
    {move,{x,3},{y,1}}.
    {line,[{location,"lc_tests_opt.erl",107}]}.
    {call,4,{f,119}}.
    {test_heap,6,1}.
    {put_list,{y,0},nil,{x,1}}.
    {put_list,{y,1},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,120}.
    {move,{x,5},{x,0}}.
    {call_only,4,{f,119}}.
  {label,121}.
    {test,is_nil,{f,122},[{x,0}]}.
    {move,{x,2},{x,0}}.
    {call_only,2,{f,114}}.
  {label,122}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",105}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pen/1-lc$^0/1-0-', 2, 124}.
  {label,123}.
    {line,[{location,"lc_tests_opt.erl",98}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen/1-lc$^0/1-0-'},2}.
  {label,124}.
    {test,is_nonempty_list,{f,126},[{x,0}]}.
    {get_list,{x,0},{x,2},{x,3}}.
    {gc_bif,'rem',{f,125},4,[{x,2},{integer,2}],{x,4}}.
    {test,is_eq_exact,{f,125},[{x,4},{integer,0}]}.
    {allocate,3,4}.
    {move,{integer,1},{x,0}}.
    {move,{x,1},{y,0}}.
    {move,{x,3},{y,1}}.
    {move,{x,2},{y,2}}.
    {line,[{location,"lc_tests_opt.erl",100}]}.
    {call_ext,2,{extfunc,lists,seq,2}}.
    {move,{y,1},{x,2}}.
    {move,{y,2},{x,3}}.
    {move,{y,0},{x,1}}.
    {call_last,4,{f,129},3}.
  {label,125}.
    {move,{x,3},{x,0}}.
    {call_only,2,{f,124}}.
  {label,126}.
    {test,is_nil,{f,127},[{x,0}]}.
    return.
  {label,127}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",98}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-pen/1-lc$^1/1-1-', 4, 129}.
  {label,128}.
    {line,[{location,"lc_tests_opt.erl",98}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-pen/1-lc$^1/1-1-'},4}.
  {label,129}.
    {test,is_nonempty_list,{f,131},[{x,0}]}.
    {get_list,{x,0},{x,4},{x,5}}.
    {gc_bif,'rem',{f,130},6,[{x,4},{integer,2}],{x,6}}.
    {test,is_eq_exact,{f,130},[{x,6},{integer,1}]}.
    {allocate,2,6}.
    {move,{x,5},{x,0}}.
    {move,{x,4},{y,0}}.
    {move,{x,3},{y,1}}.
    {line,[{location,"lc_tests_opt.erl",100}]}.
    {call,4,{f,129}}.
    {test_heap,6,1}.
    {put_list,{y,0},nil,{x,1}}.
    {put_list,{y,1},{x,1},{x,1}}.
    {put_list,{x,1},{x,0},{x,0}}.
    {deallocate,2}.
    return.
  {label,130}.
    {move,{x,5},{x,0}}.
    {call_only,4,{f,129}}.
  {label,131}.
    {test,is_nil,{f,132},[{x,0}]}.
    {move,{x,2},{x,0}}.
    {call_only,2,{f,124}}.
  {label,132}.
    {test_heap,2,1}.
    {put_list,{x,0},nil,{x,1}}.
    {move,{atom,function_clause},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",98}]}.
    {call_ext,2,{extfunc,erlang,error,2}}.


{function, '-ses/1-lc$^0/1-0-', 1, 134}.
  {label,133}.
    {line,[{location,"lc_tests_opt.erl",93}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-ses/1-lc$^0/1-0-'},1}.
  {label,134}.
    {test,is_nonempty_list,{f,136},[{x,0}]}.
    {get_list,{x,0},{x,1},{x,2}}.
    {gc_bif,'rem',{f,135},3,[{x,1},{integer,2}],{x,3}}.
    {test,is_eq_exact,{f,135},[{x,3},{integer,0}]}.
    {allocate_zero,1,3}.
    {line,[{location,"lc_tests_opt.erl",93}]}.
    {gc_bif,'*',{f,0},3,[{x,1},{x,1}],{y,0}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",93}]}.
    {call,1,{f,134}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,135}.
    {move,{x,2},{x,0}}.
    {call_only,1,{f,134}}.
  {label,136}.
    {test,is_nil,{f,133},[{x,0}]}.
    return.


{function, '-ss/1-lc$^0/1-0-', 1, 138}.
  {label,137}.
    {line,[{location,"lc_tests_opt.erl",89}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-ss/1-lc$^0/1-0-'},1}.
  {label,138}.
    {test,is_nonempty_list,{f,139},[{x,0}]}.
    {allocate_zero,1,1}.
    {get_list,{x,0},{x,1},{x,2}}.
    {line,[{location,"lc_tests_opt.erl",89}]}.
    {gc_bif,'*',{f,0},3,[{x,1},{x,1}],{y,0}}.
    {move,{x,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",89}]}.
    {call,1,{f,138}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,139}.
    {test,is_nil,{f,137},[{x,0}]}.
    return.


{function, '-all_tests/0-fun-12-', 1, 141}.
  {label,140}.
    {line,[{location,"lc_tests_opt.erl",82}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-12-'},1}.
  {label,141}.
    {call_only,1,{f,57}}.


{function, '-all_tests/0-fun-11-', 1, 143}.
  {label,142}.
    {line,[{location,"lc_tests_opt.erl",81}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-11-'},1}.
  {label,143}.
    {call_only,1,{f,55}}.


{function, '-all_tests/0-fun-10-', 1, 145}.
  {label,144}.
    {line,[{location,"lc_tests_opt.erl",80}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-10-'},1}.
  {label,145}.
    {call_only,1,{f,53}}.


{function, '-all_tests/0-fun-9-', 1, 147}.
  {label,146}.
    {line,[{location,"lc_tests_opt.erl",79}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-9-'},1}.
  {label,147}.
    {call_only,1,{f,51}}.


{function, '-all_tests/0-fun-8-', 2, 149}.
  {label,148}.
    {line,[{location,"lc_tests_opt.erl",78}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-8-'},2}.
  {label,149}.
    {call_only,2,{f,49}}.


{function, '-all_tests/0-fun-7-', 1, 151}.
  {label,150}.
    {line,[{location,"lc_tests_opt.erl",77}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-7-'},1}.
  {label,151}.
    {call_only,1,{f,47}}.


{function, '-all_tests/0-fun-6-', 2, 153}.
  {label,152}.
    {line,[{location,"lc_tests_opt.erl",76}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-6-'},2}.
  {label,153}.
    {call_only,2,{f,45}}.


{function, '-all_tests/0-fun-5-', 1, 155}.
  {label,154}.
    {line,[{location,"lc_tests_opt.erl",75}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-5-'},1}.
  {label,155}.
    {call_only,1,{f,43}}.


{function, '-all_tests/0-fun-4-', 1, 157}.
  {label,156}.
    {line,[{location,"lc_tests_opt.erl",74}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-4-'},1}.
  {label,157}.
    {call_only,1,{f,41}}.


{function, '-all_tests/0-fun-3-', 1, 159}.
  {label,158}.
    {line,[{location,"lc_tests_opt.erl",73}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-3-'},1}.
  {label,159}.
    {call_only,1,{f,39}}.


{function, '-all_tests/0-fun-2-', 1, 161}.
  {label,160}.
    {line,[{location,"lc_tests_opt.erl",72}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-2-'},1}.
  {label,161}.
    {call_only,1,{f,37}}.


{function, '-all_tests/0-fun-1-', 1, 163}.
  {label,162}.
    {line,[{location,"lc_tests_opt.erl",71}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-1-'},1}.
  {label,163}.
    {call_only,1,{f,35}}.


{function, '-all_tests/0-fun-0-', 1, 165}.
  {label,164}.
    {line,[{location,"lc_tests_opt.erl",70}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-all_tests/0-fun-0-'},1}.
  {label,165}.
    {call_only,1,{f,33}}.


{function, '-test/3-fun-0-', 3, 167}.
  {label,166}.
    {line,[{location,"lc_tests_opt.erl",37}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-test/3-fun-0-'},3}.
  {label,167}.
    {allocate_zero,4,3}.
    {bif,self,{f,0},[],{y,0}}.
    {move,{x,1},{y,1}}.
    {move,{x,2},{y,2}}.
    {move,{x,0},{y,3}}.
    {line,[{location,"lc_tests_opt.erl",38}]}.
    {call,0,{f,12}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {kill,{y,0}}.
    {line,[{location,"lc_tests_opt.erl",38}]}.
    {call_ext,2,{extfunc,erlang,process_info,2}}.
    {move,{x,0},{x,2}}.
    {move,{y,1},{x,1}}.
    {move,{y,2},{x,0}}.
    {move,{x,2},{y,2}}.
    {kill,{y,1}}.
    {line,[{location,"lc_tests_opt.erl",39}]}.
    {call_ext,2,{extfunc,erlang,apply,2}}.
    {bif,self,{f,0},[],{y,0}}.
    {move,{x,0},{y,1}}.
    {line,[{location,"lc_tests_opt.erl",41}]}.
    {call,0,{f,12}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {trim,1,3}.
    {line,[{location,"lc_tests_opt.erl",41}]}.
    {call_ext,2,{extfunc,erlang,process_info,2}}.
    {test_heap,4,1}.
    {put_tuple,3,{x,1}}.
    {put,{y,1}}.
    {put,{x,0}}.
    {put,{y,0}}.
    {move,{y,2},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",42}]}.
    send.
    {deallocate,3}.
    return.


{function, '-random_bin/1-lc$^0/1-0-', 1, 169}.
  {label,168}.
    {line,[{location,"lc_tests_opt.erl",18}]}.
    {func_info,{atom,lc_tests_opt},{atom,'-random_bin/1-lc$^0/1-0-'},1}.
  {label,169}.
    {test,is_nonempty_list,{f,170},[{x,0}]}.
    {allocate,1,1}.
    {get_list,{x,0},{x,1},{y,0}}.
    {move,{integer,255},{x,0}}.
    {line,[{location,"lc_tests_opt.erl",18}]}.
    {call_ext,1,{extfunc,random,uniform,1}}.
    {move,{x,0},{x,1}}.
    {move,{y,0},{x,0}}.
    {move,{x,1},{y,0}}.
    {line,[{location,"lc_tests_opt.erl",18}]}.
    {call,1,{f,169}}.
    {test_heap,2,1}.
    {put_list,{y,0},{x,0},{x,0}}.
    {deallocate,1}.
    return.
  {label,170}.
    {test,is_nil,{f,168},[{x,0}]}.
    return.
