#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -sname make

main([]) ->
    Cd = os:getenv("PWD"),
    io:format("~s~n", [Cd]),
    stage(fun() -> copy_tests() end, "Copying lc_tests"),
    
    c:cd(Cd),
    stage(fun() -> compile_tests() end, "Compiling lc_tests.erl"),
    stage(fun() -> compile_alt_compiler() end, "Building alternate compiler"),
    stage(fun() -> compile_opt_tests() end, "Compiling lc_tests_opt.erl");
main(_) ->
    usage().

usage() ->
    io:format("Just ./make. Make sure it is +x.").

stage(Fn, Text) ->
    io:format("~s...~n", [Text]),
    Fn(),
    io:format("done~n").

copy_tests() ->
    os:cmd("cp lc_tests/lc_tests.erl lc_tests_opt/lc_tests_opt.erl"),
    os:cmd("sed -i 's/lc_tests/lc_tests_opt/g' lc_tests_opt/lc_tests_opt.erl").

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

compile_tests() ->
    c:cd("lc_tests"),
    application:start(compiler),
    io:format("~p~n", [application:which_applications()]),
    compile(lc_tests),
    application:stop(compiler),
    application:unload(compiler),
    c:cd("..").

compile_opt_tests() ->
    code:add_patha("lib/compiler/ebin/"),
    application:start(compiler),
    io:format("~p~n", [application:which_applications()]),
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
    [compile:file(Module,Opt) || Opt <- Opts ].
    
