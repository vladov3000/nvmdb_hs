import Data.Char           (isSpace)
import Data.Maybe          (listToMaybe)
import Data.Set            (Set)
import Data.Functor        ((<&>), ($>))
import Control.Applicative (Alternative, empty, (<|>), many, some)
import Control.Monad       (when)

import qualified Data.Set as Set

{-
Usually we would have a good parsing library to use, but dependency
management in haskell is shit so we'll just be writing a quick one.

The type below represents a Parser parsing a stream of `i`s, returning
an object of type `a` and the rest of the `i`s if it succeeds, otherwise
it returns `Nothing` if it fails.
-}

newtype Parser i a = Parser { runParser :: [i] -> Maybe (a, [i]) }

{-
`newtype` means we are defining a new type with only 1 constructor.

`Parser i a` means it is polymorphic over 2 types: i and a. You can think
of the equivalent declaration in Java as `class Parser<i, a>`.

We name the single constructor the same name as the type: `Parser`. The curly
brace indicate this is a record constructor i.e. we have named fields. Unlike
most other languages, the names you specify are not the names of the actual
fields but rather the getters to access them. For example, we would use
`runParser p` where p is a `Parser` to access the field. In our case we have a
single field, `runParser`.

The :: is used to say we are declaring the type of something, in this case the
type of the field. (->) is the type constructor for functions: it takes two types
and returns the type of a function mapping the first type to the second. E.g.
x -> y is a function that takes some type x and returns some type y. [x] is a
list of elements with type x. Note that haskell is lazily evaluated and so
lists may be infinite, making this more analogous to the `Stream` class in Java
rather than `List`. (x, y) is a tuple where the first element is of type x and
the second is of type y. `Maybe` is another type constructor that takes a single
type and returns an "optional" version of it. For example, a `Maybe Int` can contain
a `Just 5` or `Nothing`. Its 2 constructors are `Just x` and `Nothing` which represent
the stored value `x` or nothing, respectively.

The equivalent java code may look something like this:
class Parser<i, a> {
   Function<Stream<i>, Optional<Pair<a, Stream<i>> parseFunction;

   Parser(Function<Stream<i>, Optional<Pair<a, Stream<i>> f) {
       parseFunction = f;
   }

   Function<Stream<i>, Optional<Pair<a, Stream<i>> runParser() {
      return parseFunction;
   }
}

// I don't know how generics and inheritance mix in Java,
// so this may not work but hopefully the idea is still clear.
abstract class Stream<i> {}
class Nil  extends Stream<i> { public Nil() {} }
class Cons extends Stream<i> {
   i head;
   Stream<i> tail;

   Cons(i head, Stream<i> tail) { ... }
}
// Note that this is not lazy so this is not super accurate, but
// it still works conceptually.
// You can do `x instanceof Nil` or `x instance of Cons` to check
// which constructor `x` is the result of.

// This ones obvious
class Pair<a, b> { a first; b second; ... }

abstract class Optional<a> {}
class Just    extends Optional<a> { a value; Just(a value) { ... } }
class Nothing extends Optional<a> { Nothing() {} }
-}

{-
Haskell has a notion of typeclass, essentially more powerful
java interfaces.

The first one we'll implement for our `Parser` type is Functor.
The syntax to implement a typeclass for a type is:

instance <typeclass name> <type> where
    <functions required by the typeclass>

Functor requires that we implement a function `fmap` with
the following type:
`fmap :: (a -> b) -> Parser a -> Parser b`

Note that haskell functions only take one argument and return one
result. More explicity written,
`fmap :: (a -> b) -> (Parser a -> Parser b)`

`->` is right associative so the parentheses weren't necessary before.

Thus, the type of `fmap` in english is:
A function that
  takes a function that maps some type `a` to a type `b`
  and return a function that
    takes a Parser that parses type `a` (the same one as before)
    and returns a Parser that parses type `b`

More intuitively, if we have a parser that parses `a`s and a function
that can convert those `a`s to `b`s we can trivially produce a parser
that produces `b`s by running the first parser and then applying the
function. Indeed, that is exactly how we implement fmap. A few things
to know before reading the implementation:

In haskell, function application (the calling of a function) has this
syntax: <function> <argument>. Function application is left associative,
so ((f x) y) z = f x y z.

Additionally, haskell has an operator for function application:
`($) :: (a -> b) -> a -> b` e.g. f $ x = f x. You may wonder
what the point of the operator is and that is its right associativity
and precedence. Consider the expression `f (g (h x))`. Writing
all the parantheses at the end is a waste, especially in a large
expression, so we can instead replace this with `f $ g $ h x`. Note
that `f g h x = ((f g) h) x` which is the opposite of what we want
(why the parantheses were necessary in the first place).

The syntax for lambda (anonymous) functions in haskell is
\<argument> -> <expression using argument>. This has the same value
as if you defined `f <argument> = <expression using argument>` and
use `f` except the upside of this approach is that you don't need to
add an extraneous name to the function. A better way of looking at
functions is that they are all just anonymous functions bound to variable
names i.e. `f` in the example above could have been defined as
`f = \<argument> -> <expression using argument>`.

Unfortunately, to understand the next construct, do-notation, you need
to understand Monads, a typeclass that inherits from Functors. So we have
a bit of a circular definition situation. For now, I'll give a bad `magic`
explanation and later give a much better one.

Specifically in our implementation, we will rely on the Maybe instance of Monad
to "unlock" do-notation. We will later implement Monad for our Parser type
where the typeclass will be fully explained.

`do-notation` gives us the illusion of imperative programming and is often
a more concise way to describe operations. You should know, it is still just
syntax sugar which just unwraps to a bunch of lambda functions and operators
under the hood. It is more restrictive than procedural code: if else statements
must return the same value, there is no control flow like goto or while. You
will understand why this is the case later but for now understand these simple
semantics:

- Each line evaluates to a Monad, the same Monad for the whole do block
  (You can also use this syntax for do blocks: `do { line1; line2 }`)
- The value of the last line is the value of the whole do block
- The syntax <variable> <- <some expression that evals to a Monad> binds
    the value "stored" inside the Monad to that variable for future use.

For example,
  `do { x <- Just 5; pure $ x + 1 }` evaluates to Just 6
  `do { x <- Nothing; pure $ x + 1 }` evaluates to Nothing; trying to bind to
  Nothing "short circuited" the evaluation of the do block.

We will mostly be working with the Maybe instance of Monad, so we will use
do-notation to not have to add a bunch of if statements checking if the operation
returned Nothing i.e. the parse failed.

Finally, we can describe the implementation of fmap.
-}

instance Functor (Parser i) where
  fmap f p = Parser $ \input -> do
    (value, rest) <- runParser p input
    pure (f value, rest)

{-
`fmap f p = ` indicates we are defining a function named `fmap`
that takes a parameter named `f` and returns a function that
takes a parameter named `p` and returns something. Note that
`f :: (a -> b)` and `p :: Parser i a`.

We call our Parser constructor named `Parser` which takes our parse function
as its only input and returns a new parser as ouput:
`Parser :: ([i] -> Maybe (a, [i])) -> Parser i a`

We then use the `$` operator so we don't have to put parantheses around the lambda function.

Our lambda function takes an argument `input` which must be of type `[i]`
and returns a do-block which evaluates to a value of type `Maybe (a, [i])`.

The the first line of the do block evaluates the expression `(runParser p input) :: Maybe (a, [i])`.
If this is nothing the do-block short circuits and returns Nothing. If it is not, it binds the first
element of the tuple (the parsed value) to the variable `value` and the second element of the tuple
(rest of the input to parse) to the variable `rest`. Now, it returns a new Maybe tuple where `f` was
applied to `value` and `rest` stayed the same.

`pure` is equivalent to the Maybe constructor `Just` in this case. It has type `a -> f a` and it
is implemented in the Applicative instance of Maybe. We will look at the Applicative typeclass next.
From the type, you can tell that it basically just wraps a value of type `a` in a Applicative `f`.
For Maybe, `pure x = Just x`. We may want to add actual error handling to our parser in the future,
swapping Maybe for a better type, so it's good to generalize our code over any Applicative.

There is also an operator defined to make `fmap` easier to use: `fmap x y = x <$> y`
-}

{-
Next, we will define `Applicative` for our Parser type. It requires us to implement
2 functions:
`pure :: a -> f a`
`(<*>) :: f (a -> b) -> f a -> f b`

As described in the previous section, `pure` wraps values in our Applicative.
In our case, `pure x` will produce a parser that doesn't consume any of the ouptput
stream and just returns `x`. For example, `pure 1` will not parse anything and just
always return 1.

`<*>`, more commonly referred to `apply`, is an operator that applies a wrapped
value to a wrapped function. For example, `(+1)` is a function that increments a number,
and `pure (+1) <*> Just 5 = Just 6` while `pure (+1) <*> Nothing = Nothing`. Why is this necessary?
It seems as though having a Parser that parses an actual function would be insecure to run?
This is necessary since functions can only take one argument, so often functions return functions.
For example, suppose we had a parser `intParser :: Parse Char Int` defined. We want a `sumParser` that
parses 2 ints and adds them. We implement this as `sumParser = (+) <$> intParser <*> intParser`.
`(+) <$> intParser` approximately has the type `Parser Char (Int -> Int)`, so we can't just `fmap/<$>`
again. Instead, we have to `apply` because the function is wrapped in the parser.

For the implementation, we want to run the first parser to get the mapping function and the rest of the
stream. We feed the rest of the stream to the second parser to get some value. Finally, we apply
the parsed function to the parsed value and return it along with the remaining input.

It should also be noted that the Applicative typeclass inherits from Functor i.e. every instance of Applicative
must also implement Functor.
-}
instance Applicative (Parser i) where
  pure x  = Parser $ \input -> pure (x, input)
  f <*> p = Parser $ \input -> do
    (f', rest)     <- runParser f input
    (value, rest') <- runParser p rest
    pure (f' value, rest')

{-
Next, we'll implement Monad which inherits from Applicative. For Monad,
we have to implement the function `(>>=) :: m a -> (a -> m b) -> m b`, commonly known as bind.

Applicative allows us to chain together parsers, but it is still too limiting. We cannot
conditionally change how we parse while we parse. For example, consider the case where we
are given a stream of bits and we want to parse 2 consecutive bits with the same value.
We can create a `anyParser :: Parser i i` that will just return whatever is in the stream
and a `bitParser :: Bit -> Parser Bit Bit` which returns a parser that only parses successfully
if the bit in the bit stream is the same as the one that was passed into `bitParser`. We can
then chain them together using `bind` to parse 2 consecutive bits that are equal:
`anyParser >>= bitParser`.

If you notice what is going on here, we are generating a novel parser on the fly based on what
we have parsed previously. This is incredibly desirable and implementing this interface
gives access to standard library functions that can express most of these tasks we want
to do.

Furthermore, Monads give us access to `do-notation`. The binding in do-blocks the bind operator
in disguise:

myBitParser :: Parser Bit (Bit, Bit)
myBitParser = do
  x <- anyParser
  y <- bitParser (not x)
  pure (x, y)

myBitParser =
  anyParser >>= \x ->
  bitParser (not x) >>= \y ->
  pure (x, y)

We can replace variable bindings with binds and lambda functions.
The other rule where only that last value is returned from the do block
can be desugared like so:

rmTwoBits :: Parser Bit ()
rmTwoBits = do
  anyParser
  anyParser
  pure ()

rmTwoBits =
  anyParser *>
  anyParser *>
  pure ()

The (*>) or (>>) operator (they are equivalent) have the type signature
`f a -> f b -> f b`. From the type signature, you can see how it essentially
discards the first argument and returns the second, except it uses the apply
operator to make sure the value isn't just forgotten. We can only use *> if we
implement Applicative and >> if we implement Monad.
-}
instance Monad (Parser i) where
  p >>= f = Parser $ \input -> do
    (value, rest) <- runParser p input
    runParser (f value) rest

{-
Alternative is pretty self explanatory. It lets us try one parser, then if that
one fails try another and return the value from the first one that suceeds.

empty is a parser that always fails. p <|> p' will try p first, then try p' if
p failed. We exploit that fact that Maybe is already an instance of Alternative
and reuse its implementation of empty and <|>.

`const :: a -> b -> a` is a function that returns a function that always returns
the first argument passed to const.
-}
instance Alternative (Parser i) where
  empty    = Parser $ const empty
  p <|> p' = Parser $ \input -> (runParser p input) <|> (runParser p' input)

{-
Semigroups are types that can be concatonated. E.g. lists, tuples, even Maybe.
We implement semigroup by running two parsers consecutively and then concatonating
their results. `Semigroup a =>` in the declaration specifies that type `a` is
also a Semigroup; this is what allows us to concatonate the results together.
-}
instance Semigroup a => Semigroup (Parser i a) where
  p <> p' = Parser $ \input -> do
    (value,  rest)  <- runParser p  input
    (value', rest') <- runParser p' rest
    pure (value <> value', rest')

{-
A Monoid is a Semigroup with an identity element `mempty`.
Concatonating with mempty shouldn't do anything. E.g.
`<> = +` and `mempty = 0` or `<> = *` and `mempty = 1` are
both valid implementations of Monoid for Integer.

We create a parser that consumes no input, just returns
the mempty type of the value being parsed (this requires
the parsed type to be a Monoid itself).
-}
instance Monoid a => Monoid (Parser i a) where
  mempty = pure mempty

{-
Below are the fruits of our labor. Now with beautiful abstractions
made, we can proceed to actually write some parsers.

The first one is `satisfy`. It checks if the first
element in the stream satisfies a predicate and
returns it if so, otherwise errors out.

`listToMaybe :: [a] -> Maybe a` returns Just the
head of the list if it is not empty, or Nothing if it is empty

`tail :: [a] -> [a]` returns the list without its first element.
-}
satisfy :: (i -> Bool) -> Parser i i
satisfy f = Parser $ \input -> do
  first <- listToMaybe input
  if f first then Just (first, tail input) else Nothing

-- char ensures that the first element is equal to its argument
char :: Eq i => i -> Parser i i
char i = satisfy (== i)

-- string ensures the first elements of the stream are equal to its argument
string :: Eq i => [i] -> Parser i [i]
string = traverse char

-- eof ensures the stream is empty
-- null returns if a list is empty
-- () is a tuple with no elements. Its used to represent that fact that the
-- return value cannot be used for anything.
eof :: Parser i ()
eof = Parser $ \input -> if null input then Just ((), input) else Nothing

-- whitespace removes any whitespace from a character stream
-- `many :: Applicative f => f a -> f [a]` chains zero or more of
-- an applicative and returns a list of their results
-- `$> :: Functor f => f a -> b -> f b` is used to swap out the return
-- value of the parser with a unit tuple so that we don't accidently
-- use it.
whitespace :: Parser Char ()
whitespace = many (satisfy isSpace) $> ()

-- literal is like `string` but with leading and trailing whitespace
literal :: String -> Parser Char ()
literal s = whitespace *> string s <* whitespace $> ()

-- token is like `literal` but can have any value in it
-- `some` is `many` but requires 1 or more results
token :: Parser Char String
token = whitespace *> (some $ satisfy $ not . isSpace) <* whitespace

-- Command is a type with 2 constructors for inserting and finding
-- `deriving (Show, Eq)` will automatically implement the typeclasses
-- Show, Eq which provide a function to convert to a string and to check equality,
-- respectively.
data Command = Insert String | Find String deriving (Show, Eq)

commandParser :: Parser Char Command
commandParser = (insert <|> find) <* eof
  where insert     = literal "insert" *>
                     common <*
                     field "customer" <*
                     field "count" <&>
                     tail <&>
                     Insert
        find       = literal "find" *> common <&> tail <&> Find
        common     = field "hotelId" <> field "inDate" <> field "outDate"
        field name = literal name *> pure " " <> token

-- repl is one iteration read eval print loop

-- IO is a Monad which is used to represent a side effect.
-- Instead of `getLine` reading from stdin, it returns an object that
-- represents us reading from stdin. Then, we fmap/apply/bind to that
-- object to use that value without that value having actually been
-- used yet.

-- `repl` takes the current keys stored as input and returns a new
-- set wrapped in an IO Monad

-- `fst :: (a, b) -> a` extracts the first value of a tuple

-- `case ... of ...` pattern matches against an object, essentially
-- reversing constructors and allows us to extract the data it holds.

-- `Set.insert :: Ord a => a -> Set a -> Set a` takes a type that can
-- be ordered (an instance of the Ord typeclass) and a set and returns
-- a new set with the type inserted.

-- `getLine :: IO String` is an object that represents reading from stdin
-- `putStrLn :: String -> IO ()` returns an object that represents printing
--  the given String to stdout.
repl :: Set String -> IO (Set String)
repl set = do
  line <- getLine
  let command = fst <$> runParser commandParser line
  case command of
    Just (Insert s) -> pure (Set.insert s set) <* putStrLn "insert OK"
    Just (Find   s) -> pure set <* when (Set.member s set) 
                       (putStrLn "find FOUND")
    Nothing         -> pure set <* putStrLn "unknown input"

-- `main` returns an IO object that represents what our program has to do.
-- The haskell runtime will actually evaluate this object and do IO
-- accordingly.

-- We define a helper function `go :: Set String -> IO ()` that takes an
-- initial state and calls `repl`, then binding the result to `go`,
-- producing an infinite recursion. Haskell has no loops! 
main :: IO ()
main = go Set.empty
  where go state = repl state >>= go

