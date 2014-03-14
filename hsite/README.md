
HSite
=====

HSite is a command line tool to insert and maintain Freesites from the
command line. Because I did not settle on the final form of the
commands and parameters that are ultimately needed, here's just a
small how-to showing it in action in it's current form:

First, you will want a directory where you'll maintain your Freesite
and then initialize a hsite project there:

~~~bash
$ mkdir hsite-test && cd hsite-test
$ hsite init
~~~

This will create an subdirectory called `.hsite` inside your project
folder. This will be used to keep track of the insert state, and also
store some additional metadata. We'll have a look at it's contents
later on. Note that this directory is only kept on your local storage,
but never inserted into Freenet. Now that the project was initialized,
we may have a look at it's status:

~~~
$ hsite status
Files needing an insert:
0.00 B in 0 files.
~~~

Ok, that's kind of expected: Nothing to do. So let's add our very
first file:

~~~
$ echo "So I created another Freesite?" > test.txt
$ hsite status
Files needing an insert:
     fresh: test.txt (31.00 B)
31.00 B in 1 files.
~~~

So hsite as really noticed it. The "fresh" indicates that this is a
new file which was not inserted before. Seems legit. We can actually
insert this now, using the `insertFiles` command:

~~~
$ hsite insertFiles *.txt
/tmp/hsite-test/.hsite/node: openFile: does not exist (No such file or directory)
CHK@0YGXErExxsMozrTdlvOV3qZDhofVK018iq-Lm0HZtRM,rHlY~VZRPKd~ZH5C488shkGCmXcUpL39Pi78ni0DVrA,AAMC--8
2/2insert took 972.98897s (0.00 B/s)
~~~

The line starting with `CHK@` is the URI where your data can be
fetched. This information is also stored in hsite's database we've
created with the `init` command. I'm aware that this output is a bit
garbled currently, but it get's the message over. Also, it's true:
Freenet used 16 minutes to insert a 31 byte file. It's more efficient
for larger inserts, though. You may also try to fetch that file now.

But what we have so far is not really a Freesite, but just a blob of
data with an key. Let's fix that by adding an `index.html` file:

~~~
$ cat > index.html << EOF
<html>
  <head>
    <title>My glorious HSite test page</title>
  </head>

  <body>
    <h1>And so it begins</h1>
    <p>Look, I can even reference a <a href="test.txt">text file</a>.</p>
  </body>
</html>
EOF
$ echo 'Fresh content!' > test.txt
$ hsite status
Files needing an insert:
     fresh: index.html (205.00 B)
  modified: test.txt (15.00 B)
220.00 B in 2 files.
~~~

As we see, there is a "fresh" `index.html` file, and the `test.txt`
was modified since the last insert. So let's finally give inserting
our site a first try:

~~~
hsite insert --chk
CHK@wwZqe6lwqnj0vptcngfc1LVtL8LRiwhEWfxbOaeaRTQ,1wqHyvtIOq0VOc1bIPpKeuZyOacUaspWwDiB3jzQsQQ,AAMC--8
~~~

This will really insert the site:

  * something that has a manifest pointing to `index.html` as the
    "default entry", and containing all files either directly or as
    redirects to the key where that file was inserted the first time

  * having `index.html` the default entry is hardcoded currently, but
    can be fixed easy enough. I hope this causes not too much trouble
    for now.

Beware that when inserting the site with the `--chk` flag, hsite will
do just that and insert using an `CHK` key. This means the site cannot
be easily updated, because you the next edition will get a different
key, which you would have to distribute manually. But we can do better
by just omitting that flag and doing a `SSK` insert:

~~~
$ hsite insert
hsite: you can only do --chk inserts until you generated keys
~~~

Oh, there's something missing as it seems: We need to tell hsite about
the keys it should use for the insert (there's some public key crypto
going on under the hood for updatable size). That's easy enough, too:

~~~
$ hsite keygen test
/tmp/hsite-test/.hsite/keys: openFile: does not exist (No such file or directory)
/tmp/hsite-test/.hsite/node: openFile: does not exist (No such file or directory)
~~~

Please ignore these error messages for now, everything went
well. Check your keys (if you like to) and try the insert again:

~~~
$ cat .hsite/keys
(omitted)
$ hsite insert
SSK@4xJRb0dbh-15ltJDV-TVd0~A1x0WxMhu9a9LrkJBX8g,vqcAr7bX5WOwQ45S1Zr39jGppZQl0nfCNo73B6NOYjg,AQACAAE/test-0
2/2your latest revision is at:
USK@4xJRb0dbh-15ltJDV-TVd0~A1x0WxMhu9a9LrkJBX8g,vqcAr7bX5WOwQ45S1Zr39jGppZQl0nfCNo73B6NOYjg,AQACAAE/test/0
~~~

Congratulations, now that's a real Freesite. Things you should be aware of:

  * hsite will always try to minimize the amount of data that it
    inserts into Freenet. But it can not yet detect if files are
    renamed or moved around within your site and will detect them as
    "fresh" and insert them again. This can and should be fixed as
    well.

  * Doing `--chk` inserts can be quite useful to make sure Freenet's
    content filters did not mess with your HTML or CSS too much,
    without needlessly inserting new revisions and confusing your
    audience. You can just do `CHK` inserts until you are satisfied
    with the result, and then do the real insert once. The files
    already inserted to the `CHK` key(s) before will be reused via
    redirects when you finally update your site, so this is quite
    efficient.

  * If your site contains relatively large files (maybe larger than
    1MB or so), it can make sense to insert them first using the
    `insertFiles` command, and insert the rest of the site once they
    are out of the way. The will not be re-inserted but just be
    redirected from the site's manifest.

Finally, about that missing `node` file hsite keeps complaining about:
By default, hsite will try to talk to your Freenet node at
`localhost`, port 9481. This is the Freenet default and should be
suitable for most users. To make this message go away simply create
that file:

~~~
$ echo '("localhost", 9481)' > .hsite/node
~~~

I think you'll get the idea what to do if your node is not running on
`localhost`. Oh, one last thing to be aware of: hsite currently does
not emply `DDA` transfers. This means every file will be read by hsite
from the disk and passed on to Freenet through the socket. This might
be a problem for really large files, but never caused an issue for me.

Enjoy, and please file bugs or feature requests.
