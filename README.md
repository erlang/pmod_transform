Parameterized Modules 
=====================

In October 2012 the OTP Technical Board decided to remove
the experimental feature "Parameterized Modules" from
Erlang/OTP, but to release a parse transform free to use
and include in any package needing it. See:

  http://www.erlang.org/news/35

The parse transform is found in src/pmod_pt.erl.

Feel free to copy that file into your project. If you do any changes
to the parse transform, you should probably change the module name
to avoid potential conflicts with other applications that might use
the parse transform.
