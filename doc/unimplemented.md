# Unimplemented instances

These are instances that have been considered but have been rejected or that I have failed to implement.

## `instance HasResolution (Neither a b)`

The only reasonable value for `resolution` is `0`, as `Neither a b` spans only one value.
However, this causes a division-by-zero error.

## `instance Read1 (Neither a)` & `instance Show1 (Neither a)`

I don’t know how the Read and Show types work.
