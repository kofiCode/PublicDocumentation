# Shell Scripting with Haskell for Newbies

I have a problem where I scan for wireless networks with the `iw`
command.  This command returns a lot of junk and I want to parse it
and present it in a way that is useful for me.  Perhaps later
extending the script so that after I can easily see which networks are
available I can also easily join one of them.  The joining part may
require a password if the network is locked down.

To 'remember' a network a file needs to be created in
`/etc/network.d/`, so the 'joining' part would require writing a file
out to that location.  But we'll leave that to a later time.  For now
we just concern ourself with reading which networks are available,
their wireless signal strength and their name.  The unix command that
we'll be running is: `sudo iw wlan0 scan`.  I have setup the `iw`
command to not require a password using `visudo`.

Since running this command is slow, we'll run it once and save it to a
file called `networks.txt` and pass that as a command line argument to
our program for now.  Speed up testing! :)

Ideas from this scripting came from:
http://donsbot.wordpress.com/2010/08/17/practical-haskell/ however I
found that website to be over my head, being a newbie haskeller.
Therefore, some cut and paste isn't necessarily well understood and
therefore I may not explain properly???  We'll see.

First lets get a very basic haskell setup going.  I've got a folder
called: `net-hask` with a single file called `Main.hs` in it.  Like
so: 

```
net-hask/
|-- Main.hs
`-- networks.txt
```

import Text.Printf
import Process 

main = do
  s <-run "sysctl hw.setperf"
  let old = clean s
       new | old == 100 = 0
           | otherwise = 100
  run $ "sudo sysctl -w hw.setperf=" ++ show new 
  printf ³cpu: %d -> %d\n´ old new

  s <-run "sysctl hw.setperf"
  let clock = fromIntegral (clean s') / 1000
  printf "clock: %f Ghz\n" clock
    where 
      clean = read . init . tail . dropWhile (/= `=`)


