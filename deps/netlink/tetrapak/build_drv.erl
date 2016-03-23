-task({"build:drv", "Build the netlink driver"}).
-task({"clean:drv", "Clean the netlink driver"}).

run("build:drv", _) ->
    tetrapak:outputcmd(tetrapak:subdir("c_src"), "make", [cflags(), "all"]);

run("clean:drv", _) ->
    tetrapak:outputcmd(tetrapak:subdir("c_src"), "make", [cflags(), "clean"]).

cflags() ->
    ["CFLAGS=", "-O2 ", ["-I", code:root_dir(), "/erts-", erlang:system_info(version), "/include"]].
