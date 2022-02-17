with builtins;
f:
let
  g = a:
    if isAttrs a then
      f a // {
        __functor = self: b: g b;
        __functionArgs = functionArgs f;
      }
    else
      f a;
in g { }
