`map-indexed` is defined as:

```
(map-indexed f coll)

Returns a lazy sequence consisting of the result of applying f to 0
and the first item of coll, followed by applying f to 1 and the second
item in coll, etc, until coll is exhausted. Thus function f should
accept 2 arguments, index and item.
```

It then shows the following example:

```clojure
user=> (map-indexed (fn [idx itm] [idx itm]) "foobar")
([0 \f] [1 \o] [2 \o] [3 \b] [4 \a] [5 \r])
```

The part that looks like:

```clojure
(fn [idx itm] [idx itm])
```

is creating an anonymous function, that has two arguments `idx` or
index and `itm` or item.  It then returns a vector of those two
arguments. 

So if we have a list of strings like so:

```clojure
(def my-list '("human" "deer" "berries"))
```

We could generate:

```clojure
([0 "human"] [1 "deer"] [2 "berry"])
```

with:

```clojure
(map-indexed (fn [i s] [i s]) my-list)
```
