# Unimplemented instances

These are instances that have been considered but have been rejected or that I have failed to implement.

## `instance HasResolution (Neither a b)`

The only reasonable value for `resolution` is `0`, as `Neither a b` spans only one value.
However, this causes a division-by-zero error.

## `instance Read1 (Neither a)`, `instance Show1 (Neither a)`, `instance Read2 Neither`, `instance Show2 Neither`

I don’t know how the Read and Show types work.

## `instance RealFloat (Neither a b)`

There’s no compelling way to justify this instance in my opinion. No equivalent float value seems entirely natural, except possibly +0 or NaN.

## Classes not found in `base`

Currently, Neither-data only implements instances for classes in `base`. This may change in the future.
