#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname make

main([]) ->
    Cd = os:getenv("PWD"),
    io:format("~s~n", [Cd]),
    io:format("Copying lc_tests~n"),
    copy_tests(),
    io:format("Compiling Erlang Compiler~n"),
    compile_compiler(),
    io:format("Compiling lc_tests.erl~n"),
    c:cd(Cd),
    compile_tests(),
    io:format("Loading alternate compiler~n"),
    load_opt_compiler(),
    io:format("Compiling lc_tests_opt.erl~n"),
    compile_opt_tests();
main(_) ->
    usage().

usage() ->
    io:format("Just ./make. Make sure it is +x.").

copy_tests() ->
    os:cmd("cp lc_tests.erl lc_tests_opt.erl"),
    os:cmd("sed -i 's/lc_tests/lc_tests_opt/g' lc_tests_opt.erl").

compile_compiler() ->
    set_erl_top(),
    c:cd(os:getenv("ERL_TOP") ++ "lib/compiler/"),
    os:cmd("make").

erl_top() ->
    "/home/adgear/code/otp_src_18.2/".

set_erl_top() ->
    case os:getenv("ERL_TOP") of
       [] -> os:putenv("ERL_TOP", erl_top());
       true -> ok
    end.

load_opt_compiler() ->
    code:add_patha(os:getenv("ERL_TOP") ++ "lib/compiler/ebin/").

compile_tests() ->
    compile(lc_tests).

compile_opt_tests() ->
    application:start(compiler),
    compile(lc_tests_opt).

compile(Module) ->
    Opts = [
            [to_core],
            [to_asm],
            ['P'],
            ['E']
           ],
    lists:foldl(
      fun(Opt, Acc) ->
              [c:c(Module, Opt) | Acc]
      end,
      [],
      Opts).
    
