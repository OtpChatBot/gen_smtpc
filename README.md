gen_smtpc
=============

`gen_smtpc` - is a RFC-5321 compatible smtp client for erlang.

Usage
--------------

Add `gen_smtpc` to your `rebar.config`:

```erlang
%% Dependencies
{deps, [
   {gen_smtpc, "*", {git, "git://github.com/0xAX/gen_smtpc.git", "master"}}
   ]
}.
```

And send mail from your `erlang` application:

```erlang
application:start(gen_smtpc),
Options = [{use_ssl, true}, {host, "smtp.gmail.com"}, {port, 465}]),
gen_smtpc:send({"usermail@gmail.com", "password"}, "user2@gmail.com", "Subject", "Mail body", Options).
```

Contribute
--------------

  * Fork [gen_smtpc](https://github.com/0xAX/gen_smtpc)
  * Make changes
  * Send pull request

Author
--------------

[@0xAX](https://twitter.com/0xAX)

License
--------------

Apache License Version 2.0.
