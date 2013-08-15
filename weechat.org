* [Getting Started Guide](http://www.weechat.org/files/doc/stable/weechat_quickstart.en.html)
* [FAQ](http://weechat.org/files/doc/weechat_faq.en.html)
* [KeyBindings](http://www.weechat.org/files/doc/stable/weechat_user.en.html#key_bindings)

Start: `weechat-curses`

After any of the `/set` commands below, you can `/save` the settings to
the conf file, or they'll be persisted on a `/quit`.

Create an IRC Server named `freenode` with value: `freenode.irc.net`,
on port `6667`:

    /server add freenode irc.freenode.net/6667

Enable auto-connect to server `freenode`, defined above, on startup:

    /set irc.server.freenode.autoconnect on

To auto-join channels, do:

    /set irc.server.freenode.autojoin "#emacs,#archlinux,#haskell,#xmonad,#yesod,#gentoo"

Where `#clojure,...` are the names of the channels u want to autojoin.

## Identify

Some networks require you to register before you can post questions.
You do this by doing:

    /msg nickserv register PASSWORD EMAIL 

You must replace PASSWORD with a password of your choice and EMAIL
with a VALID email address. It must be valid as some networks like you
to verify it exists by sending you an email. Once you have registered
your nick, when you come back to the network you must identify

    /msg nickserv identify PASSWORD 

OR 

    /nickserv identify PASSWORD (should work, does on most networks) 

To run a command after connection to server, for example to identify
with nickserv:

    /set irc.server.freenode.command "/msg nickserv identify gumnuts8"

yourself, you can do this with these commands..

Sometimes you'll be logged in with a different nick, and you can
switch to your regular nick, in this case: ftravers, with:

    /msg NickServ IDENTIFY ftravers password

## Tab Shortcut nicknames

To message someone, do first letter then <TAB> complete it out their
nick to save you typing it.

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

To add proxy server:

    /proxy add myproxy http sample.host.org 8888
    /set irc.server.freenode.proxy "myproxy"

