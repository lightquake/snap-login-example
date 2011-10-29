snap-login-example
==================

Requirements
------------
The main requirement is snap 0.6; you can install it with `cabal install snap`. If you're using ghc 7.0, you can use `cabal install snap -fhint` to add autoreloading support.

The other requirement is [digestive-functors-snap-heist](https://github.com/dbp/digestive-functors-snap-heist); the version tested to work with this is `b1c7656`, though I can't imagine later versions breaking too badly.

Documentation
----------
I've tried to make the code fairly documented; this assumes a middling level of Haskell, but no familiarity with Snap or Heist or anything like it. I intend to document it deeper in a series of blog posts, and hopefully contribute documetation back into snap itself.

The CSS is hotlinked (with permission) from Twitter's [Bootstrap](http://twitter.github.com/bootstrap/).

License
----------
MIT license.