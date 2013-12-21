extract-offsets-from-redis-bitmap
=================================

This module is used to extract offsets from value that redis bitset stores. You might need to adjust it if you have performance issue when you use it. Anyhow, I attached my profiling report.

## To build this module

erl -make

## Profling

Prepare bitmap:

```
redis 127.0.0.1:6379> setbit a1 1 1
(integer) 0
redis 127.0.0.1:6379> setbit a1 2 1
(integer) 0
redis 127.0.0.1:6379> setbit a1 3 1
(integer) 0
redis 127.0.0.1:6379> setbit a1 5 1
(integer) 0
redis 127.0.0.1:6379> setbit a1 5000000 1
(integer) 0
redis 127.0.0.1:6379> setbit a2 5000000 1
(integer) 0
redis 127.0.0.1:6379> setbit a2 5000001 1
(integer) 0
redis 127.0.0.1:6379> setbit a2 5000002 1
(integer) 0
redis 127.0.0.1:6379> setbit a2 5000003 1
(integer) 0
redis 127.0.0.1:6379> setbit a1 5000003 1
(integer) 0
redis 127.0.0.1:6379> setbit a1 3 1
(integer) 1
redis 127.0.0.1:6379> setbit a2 3 1
(integer) 0
redis 127.0.0.1:6379> bitop AND a3 a1 a2
(integer) 625001
```

```
> {ok, C} = eredis:start_link().
> {ok, V} = eredis:q(C, ["GET", "a3"]).

> extract_offsets:start(V).
Cpu Time: 70000 microseconds; Elapsed Time: 70000 microseconds
FUNCTION                                            CALLS      %   TIME  [uS / CALLS]
--------                                            -----    ---   ----  [----------]
io:format/2                                             1   0.00      0  [      0.00]
io:format/3                                             1   0.00      0  [      0.00]
gen:do_call/4                                           1   0.00      0  [      0.00]
re:check_for_unicode/2                                  2   0.00      0  [      0.00]
gen_server:call/3                                       1   0.00      0  [      0.00]
net_kernel:dflag_unicode_io/1                           1   0.00      0  [      0.00]
erlang:group_leader/0                                   1   0.00      0  [      0.00]
io:o_request/3                                          1   0.00      1  [      1.00]
io:request/2                                            1   0.00      1  [      1.00]
io:execute_request/2                                    1   0.00      1  [      1.00]
io:io_request/2                                         1   0.00      1  [      1.00]
gen:call/4                                              1   0.00      1  [      1.00]
code:call/1                                             2   0.00      1  [      0.50]
io_lib_format:collect_cseq/2                            2   0.00      1  [      0.50]
io_lib_format:encoding/2                                2   0.00      1  [      0.50]
io_lib_format:pcount/1                                  2   0.00      1  [      0.50]
io_lib_format:decr_pc/2                                 2   0.00      1  [      0.50]
io_lib_format:base/1                                    2   0.00      1  [      0.50]
io_lib_format:term/5                                    2   0.00      1  [      0.50]
io_lib_format:cond_lowercase/2                          2   0.00      1  [      0.50]
re:do_grun/5                                            2   0.00      1  [      0.50]
string:right/3                                          2   0.00      1  [      0.50]
string:r_pad/3                                          1   0.00      1  [      1.00]
extract_offsets:'-parition_binary/5-lc$^0/1-0-'/1       2   0.00      1  [      0.50]
io:default_output/0                                     1   0.00      2  [      2.00]
io:wait_io_mon_reply/2                                  1   0.00      2  [      2.00]
io:bc_req/3                                             1   0.00      2  [      2.00]
error_handler:undefined_function/3                      2   0.00      2  [      1.00]
code:ensure_loaded/1                                    2   0.00      2  [      1.00]
io_lib_format:field_width/2                             2   0.00      2  [      1.00]
io_lib_format:field_width/3                             2   0.00      2  [      1.00]
io_lib_format:precision/2                               2   0.00      2  [      1.00]
io_lib_format:pad_char/2                                2   0.00      2  [      1.00]
io_lib_format:collect_cc/2                              2   0.00      2  [      1.00]
io_lib_format:pcount/2                                  4   0.00      2  [      0.50]
io_lib_format:control/8                                 2   0.00      2  [      1.00]
re:process_parameters/5                                 4   0.00      2  [      0.50]
re:postprocess/5                                        2   0.00      2  [      1.00]
re:grun/3                                               2   0.00      2  [      1.00]
re:grun2/3                                              2   0.00      2  [      1.00]
extract_offsets:loop/3                                  5   0.00      2  [      0.40]
erlang:iolist_to_binary/1                               2   0.00      2  [      1.00]
erlang:send/3                                           1   0.00      2  [      2.00]
io_lib_format:fwrite/2                                  2   0.00      3  [      1.50]
io_lib_format:collect/2                                 4   0.00      3  [      0.75]
io_lib_format:field_value/2                             4   0.00      3  [      0.75]
io_lib_format:build/3                                   4   0.00      3  [      0.75]
io_lib_format:unprefixed_integer/6                      2   0.00      3  [      1.50]
re:to_binary/2                                          2   0.00      3  [      1.50]
erlang:integer_to_list/3                               13   0.00      3  [      0.23]
string:chars/3                                          4   0.00      3  [      0.75]
extract_offsets:list_offsets/2                          3   0.00      3  [      1.00]
extract_offsets:loop_positions/3                        4   0.00      3  [      0.75]
erlang:demonitor/1                                      1   0.00      3  [      3.00]
io_lib:format/2                                         2   0.01      4  [      2.00]
io_lib_format:field_value/3                             4   0.01      4  [      1.00]
re:loopexec/6                                           5   0.01      4  [      0.80]
erlang:integer_to_list/2                                2   0.01      4  [      2.00]
erlang:function_exported/3                              2   0.01      4  [      2.00]
error_handler:ensure_loaded/1                           2   0.01      5  [      2.50]
erlang:whereis/1                                        3   0.01      5  [      1.67]
code_server:call/2                                      2   0.01      8  [      4.00]
erlang:demonitor/2                                      1   0.01      8  [      8.00]
erlang:monitor/2                                        2   0.02     13  [      6.50]
re:run/3                                                6   0.04     27  [      4.50]
erlang:statistics/1                                     4   0.05     31  [      7.75]
erlang:'++'/2                                         628   0.16    108  [      0.17]
binary:part/2                                         625   0.21    136  [      0.22]
binary:part/3                                         625   0.26    170  [      0.27]
lists:append/2                                        628   0.41    269  [      0.43]
extract_offsets:parition_binary/5                     626   0.65    428  [      0.68]
erlang:binary_to_list/1                               626   5.63   3729  [      5.96]
extract_offsets:'-parition_binary/5-lc$^1/1-1-'/1  625625  92.38  61147  [      0.10]

```

##Note

I use [eredis](https://github.com/wooga/eredis) to do the testing. 
