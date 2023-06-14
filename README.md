# micgar

Improve thread readability and ergonomics by adding a new rune: `;>`.

`;>` is very similar to `;<`, but removes some of the boilerplate, cleaning up your threads.

## Proposed usage example

Here is how `;<` is used in threads:

```hoon
++  foo
  =/  m  (strand ,vase)
  ^-  form:m
  ;<  my-vase=vase  bind:m  bar
  =+  !<(my-returned-value=@ud my-vase)
  ::  ...
  ::  do stuff with my-returned-value
  ::  ...
  (pure:m !>(~))
```

In contrast, the vision for `;>` is something more like:

```hoon
++  foo
  ;>  my-returned-value=@ud  bar
  ::  ...
  ::  do stuff with my-returned-value
  ::  ...
  (pure:m !>(~))
```

So `;>` is doing the following:
1. `bind:m` is implicit
2. The `form:m` returned must be a `vase`.
3. That `vase` is automatically unpacked.

Why?

1. Get rid of boilerplate.
2. Threads must return a `vase` to be compatible with %khan.
   Since `vase`-`form`d threads are now the new standard, provide affordances for reducing boilerplate when working with them.
3. Get rid of boilerplate.
