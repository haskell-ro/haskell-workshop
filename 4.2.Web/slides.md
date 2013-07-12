% Title
% Author
% July 12, 2013

# Static site generation

* Pros
	* Speed

	* Security

	* Focus on *content*

* Cons
	* Lack of functionality

# Static site generators

* Jekyll, nanoc (Ruby)

* Hyde (Python)

* Templer (Perl)

* Hakyll (Haskell)

# Installing Hakyll

~~~~ {.bash}
$ cabal install hakyll
~~~~

* For local installations, add `.cabal/bin` to `$PATH`

# Using Hakyll

* Init and compile site

~~~~ {.bash}
$ hakyll-init mycoolsite
$ cd mycoolsite
$ ghc site.hs
~~~~

* Build `_site`

~~~~ {.bash}
$ ./site build
~~~~

# Using Hakyll (2)

* Live preview and autocompile

~~~~ {.bash}
$ ./site preview
~~~~

* Clean generated site

~~~~ {.bash}
$ ./site clean
~~~~

* Others

~~~~ {.bash}
$ ./site help
~~~~

# Hakyll basics: templates

* Template pages

	* Content + **fields**
	
	* e.g. `$body$`, `$author$`

* Fields belong to a `Context`

	* `defaultContext`: `$body$`, `$url$`, post metadata etc.
	
	* Lists of items: use `$for(field)$`

# Hakyll basics: the `Rules` monad

* `match`: globbing on content

* `create`: create page from scratch

* `route`: generate file path

* `compile`: add compilation rule


# Hakyll basics: content

* "Content is king"

* Write in any language supported by Pandoc

* Compile using `pandocCompiler`

# Hakyll basics: conclusion

* Easy to configure for blog generation

	* Ideal for small to medium sites

* Can be integrated with commenting services

	* e.g. Disqus, IntenseDebate

* Doesn't scale well for large sites

# Yesod

* TODO
