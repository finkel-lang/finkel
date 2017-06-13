-- | File with documentation header comments.
--
-- Some more documentation strings in consequent lines. Some more
-- documentation strings in consequent lines. Some more documentation
-- strings in consequent lines.
--
-- Foo foo foo foo. Foo foo foo foo. Foo foo foo foo. Foo foo foo
-- foo. Foo foo foo foo. Foo foo foo foo. Foo foo foo foo. Foo foo foo
-- foo.
module Test10 where
{-# LINE 16 "test/data/0004-comment.lisp" #-}
-- | Main entry function.
main = foo "Module with doc comments."
{-# LINE 21 "test/data/0004-comment.lisp" #-}
-- | Comment for function foo.
foo str = (>>) (putStrLn str) (bar 15 27)
{-# LINE 34 "test/data/0004-comment.lisp" #-}
-- | Comment for function bar.
--
-- This comment spans multiple lines. Bar bar bar bar bar bar bar bar
-- bar bar bar bar bar bar bar bar bar bar.
--
-- Bar bar bar bar bar bar bar bar bar bar bar bar bar bar bar bar bar
-- bar bar bar bar bar bar bar bar bar bar bar bar bar bar bar bar bar
-- bar bar bar.
--
bar a b = putStrLn ((++) "From bar: " (show ((+) a b)))
