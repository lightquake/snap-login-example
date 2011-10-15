snap-login-example
==================

Requirements
------------
As of the time I write this, snap is still in 0.5. Therefore, in order to compile this, you'll need to pull down [snap](https://github.com/snapframework/snap/), [snap-core](https://github.com/snapframework/snap-core), [snap-server](https://github.com/snapframework/snap-server), and [heist](https://github.com/snapframework/heist), as well as [digestive-functors-snap-heist](https://github.com/dbp/digestive-functors-snap-heist) (note that for snap, snap-core, and snap-server, you will need to check out the 0.6 branch since master tracks 0.5!). The versions I used to build this project are as follows:

    digestive-functors-snap-heist commit b1c7656b92032aac906c9571ea3a054e077c3139
    heist commit 0f2c668ab4c23dd169cd7e1fa74dc3e721f15225
    snap commit 1373df6fec754d98afccbc29245a526b98a9d786
    snap-core commit 0eca37edb3a3aa9e359112f7516826da49f69a03
    snap-server commit 8cabdeabe4fa7c466de0c9202c50727081d2879a

All other dependencies can be satisfied through hackage.

Documentation
----------
I've tried to make the code fairly documented; this assumes a middling level of Haskell, but no familiarity with Snap or Heist or anything like it. I intend to document it deeper in a series of blog posts, and hopefully contribute documetation back into snap itself.

The CSS is hotlinked (with permission) from Twitter's [Bootstrap](http://twitter.github.com/bootstrap/).

License
----------
MIT license.