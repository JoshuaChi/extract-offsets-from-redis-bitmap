extract-offsets-from-redis-bitmap
=================================

This module is used to extract offsets from value that redis bitset stores. You might need to adjust it if you have performance issue when use it.

## To build this module

erl -make

## Example

>setbit mykey 1 1
>setbit mykey 4 1
>setbit mykey 5 1



>extract_offsets:loop_positions(binary_to_list($RedisValue), 0, []).
[1, 4, 5]

##Note

I use [eredis](https://github.com/wooga/eredis) to do the testing. 
