#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname make

main([]) ->
    Cd = os:getenv("PWD"),
    io:format("~s~n", [Cd]),
    stage(fun() -> copy_tests() end, "Copying lc_tests"),
    stage(fun() -> compile_compiler() end, "Compiling Erlang Compiler"),
    
    c:cd(Cd),
    stage(fun() -> compile_tests() end, "Compiling lc_tests.erl"),
    stage(fun() -> compile_alt_compiler() end, "Compiling alternate compiler"),
    stage(fun() -> load_alt_compiler() end, "Loading alternate compiler"),
    stage(fun() -> compile_opt_tests() end, "Compiling lc_tests_opt.erl");
main(_) ->
    usage().

usage() ->
    io:format("Just ./make. Make sure it is +x.").

stage(Fn, Text) ->
    io:format("~s...", [Text]),
    Fn(),
    io:format("done~n").

copy_tests() ->
    os:cmd("cp lc_tests/lc_tests.erl lc_tests_opt/lc_tests_opt.erl"),
    os:cmd("sed -i 's/lc_tests/lc_tests_opt/g' lc_tests_opt/lc_tests_opt.erl").

compile_compiler() ->
    check_erl_top(),
    c:cd(os:getenv("ERL_TOP") ++ "lib/compiler/"),
    os:cmd("make").

check_erl_top() ->
    case os:getenv("ERL_TOP") of
       [] -> io:format("~nERROR: Please export ERL_TOP.~n"),
             exit(badenv);
       _ -> ok
    end.

compile_alt_compiler() ->
    c:cd("lib/compiler"),
    os:cmd("make"),
    c:cd("../../").

load_alt_compiler() ->
    code:add_patha("lib/compiler/ebin/").

compile_tests() ->
    c:cd("lc_tests"),
    compile(lc_tests),
    c:cd("..").

compile_opt_tests() ->
    application:start(compiler),
    c:cd("lc_tests_opt"),
    compile(lc_tests_opt),
    c:cd("..").

compile(Module) ->
    Opts = [[],
            [to_core],
            [to_asm],
            ['P'],
            ['E']
           ],
    [c:c(Module,Opt) || Opt <- Opts ].
    
