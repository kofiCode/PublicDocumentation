So the following are notes about projects that I wanted to do in
Clojure.  Perhaps someone will follow them and learn something too?
We'll see.

The first task that came to me was that we needed to accept a hashmap
(in the java sense) of parameters and values.  We also needed those
parameters to be sorted for some soap that we were going to put the
params into.

So we might receive a request like:

    age: 14, name: Fenton, city: vancouver

but we might say we need them sorted like:

    name, age, city 

for soap that might look like:

```xml
<name>Fenton</name>
<age>14</age>
<city>vancouver</city>
```

So I thought I'd create an list that contained the order:

```clojure
(def ordered-list '("name" "age" "city"))
```

Then I thought I'd go through the ordered list and pull out the
element from the hashmap and put it into a new ordered hashmap.

But before I did this I thought it prudent to build up some tests.  We
use the `defrecord` syntax:

```clojure
(defrecord Person [name age city])
(def fenton (Person. "Fenton" 14 "vancouver"))
```

You can access an element of the `Person` `fenton` with:

```clojure
user> (:name fenton)
"fenton"
```

Continuing with the tests:

```clojure
(deftest sort-list
  (testing "Sorting the parameters of a record"
    (is (=
