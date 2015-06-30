rebar3_idl_compiler
=====

This is a plugin for compiling Erlang IDL files using Rebar3.

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { rebar3_idl_compiler, ".*", {git, "git@github.com:sebastiw/rebar3_idl_compiler.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 rebar3_idl_compiler
    ===> Fetching rebar3_idl_compiler
    ===> Compiling rebar3_idl_compiler
    <Plugin Output>


There are two options that can now be specified in your rebar.config:


    {idl_paths, [Paths]}.
            Path = {file, FilePath, Options} | {file, FilePath} |
                   {dir, Directory, Options} | {dir, Directory}

    {idl_opts, [Options]}.
            Options: Options specified by the ic module, check
                     http://www.erlang.org/doc/man/ic.html for more info.


If the options is not given with the path tuple, then the idl_opts option
is used.


Example:
    {idl_opts, [{'Wall', true}]}.
    {idl_files, [
                 {file, "idl/system.idl", [{outdir, "generated"}]},
                 {dir, "idlfiles"}
                 ]}.


In the given example, all *.idl-files found under the path
"app_root/idlfiles" would be compiled with the 'Wall' option, while the
file "app_root/idl/system.idl" would be compiled into the directory
"generated".
