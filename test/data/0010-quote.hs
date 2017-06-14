module Main where
import SK.Core.Form
{-# LINE 7 "test/data/0010-quote.lisp" #-}
f1 arg
  = print
      [Atom (ASymbol "a"), Atom (ASymbol "b"), arg, Atom (ASymbol "d")]
{-# LINE 10 "test/data/0010-quote.lisp" #-}
f2 arg
  = print
      (List
         [Atom (ASymbol "a"), Atom (ASymbol "b"), toForm arg,
          Atom (ASymbol "d")])
{-# LINE 13 "test/data/0010-quote.lisp" #-}
f3 arg
  = print
      (List
         (concat
            [[Atom (ASymbol "a"), Atom (ASymbol "b")], splice arg,
             [Atom (ASymbol "d")]]))
main :: IO ()
{-# LINE 17 "test/data/0010-quote.lisp" #-}
main
  = do { print (Atom (ASymbol "foo"));
         print (Atom (ASymbol "foo"));
         print (List [Atom (ASymbol "quote"), Atom (ASymbol "foo")]);
         print
           (List
              [Atom (ASymbol "quote"),
               List [Atom (ASymbol "quote"), Atom (ASymbol "foo")]]);
         print (Atom (AString "string"));
         print (Atom (AInteger 42));
         print [1, 2, 3];
         print (Atom AUnit);
         print (Atom (ASymbol "foo"));
         print (Atom (ASymbol "foo"));
         f1 (Atom (ASymbol "foo"));
         f2 (Atom (ASymbol "foo"));
         f3
           (List
              [Atom (ASymbol "foo"), Atom (ASymbol "bar"),
               Atom (ASymbol "buzz")]) }
