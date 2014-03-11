
H - FCP
=======

This is a Haskell library allowing to talk to [Freenet][1] nodes using the
Freenet Client Protocol (FCP), version 2. It contains the library itself,
exporting the `Network.FCP` module, and some command line utilities using the
library.

Munin Plugin
------------

A [Munin][2] plugin is provided, which allows to monitor the status of a
running node. The plugin is capable of generating multiple graphs. As it's
quite common for Munin plugins, the name of the executable decides which graph
it generates. Currently, the following names are available:

~~~
fn_bandwidth        -> Bandwidth Usage
fn_fetch_count      -> Fetch Count
fn_fetch_success    -> Fetch Success
fn_opennet_size     -> Opennet Size Estimate
fn_remotes          -> Remote Transfers
fn_store_access_chk -> CHK Store Access Rates
fn_store_success    -> Store Lookup Success
~~~

To generate a graph, just symlink the executable under the desired name to
your Munin plugins directory (usually `/etc/munin/plugins`).

By default, the plugin tries to talk to the Freenet host at `localhost`, port
9481. If you want to talk to another Node, this can be configured by
specifying a section like

~~~
[fn_*]
env.host = example.com
env.port = 1234
~~~

in your Munin node config file (usually under `/etc/munin/plugin-conf.d/`).

Installation
------------

If you don't already have a Haskell development environment set up, I'd
remommend just installing the [Haskell Platform][3]. Then just clone the
repository and do a `cabal configure && cabal build`. If all goes well, the
executable(s) will be located under the newly created `dist` directory.

[1]: https://freenetproject.org/
[2]: http://munin-monitoring.org/
[3]: http://www.haskell.org/platform/
