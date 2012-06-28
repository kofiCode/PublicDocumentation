[FAQ](http://weechat.org/files/doc/weechat_faq.en.html)

Start: `weechat-curses`

After any of the `set` commands below, you can `/save` the settings to
the conf file, or they'll be persisted on `/quit`.

Create an IRC Server named `freenode` with value: `freenode.irc.net`,
on port 6667 in config file:

    /server add freenode irc.freenode.net/6667

Enable auto-connect to server `freenode`, defined above on startup:

    /set irc.server.freenode.autoconnect on

To auto-join channels, do:

    /set irc.server.freenode.autojoin "#clojure,#emacs,#weechat,#archlinux"

Where `#clojure,...` are the names of the channels u want to join.

To message someone, do first letter then <TAB> complete it out.

To split your window vertically

    /window splitv

To merge with an adjacent window (say #4) do:

    /window merge -window 4
    
To switch to new channel (in same window)

    Alt + left/right arrows (F5/F6)

To switch windows

    F7/F8
    
Scrollback: `PgUp/PgDn`

To filter joins/parts/quits:

    /filter add joinquit * irc_join,irc_part,irc_quit *



