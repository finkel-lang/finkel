;;; -*- mode: finkel -*-
;;;; Orphan instances for QuickCheck

;;; This module contains duplicated codes with `Orphan' module used by
;;; finkel-kernel test. At the moment, could not find a nice way to
;;; avoid adding QuickCheck package dependency without code
;;; duplication.

%p(OPTIONS_GHC -fno-warn-orphans)

(:require Finkel.Core)

(defmodule Orphan
  (import
   ;; QuickCheck
   (Test.QuickCheck
    [(Arbitrary ..) (Gen) arbitraryUnicodeChar elements getUnicodeString
     listOf oneof scale])

   ;; finkel-kernel
   (Language.Finkel.Form)))

(instance (Arbitrary Atom)
  (defn arbitrary
    (lept [headChars (++ [#'A .. #'Z] [#'a .. #'z] "_!$%*+./<=>?@^~:")
           tailChars (++ headChars "0123456789'-")
           symbolG (<*> (pure :) (elements headChars)
                        (listOf (elements tailChars)))
           stringG (fmap getUnicodeString arbitrary)]
      (oneof [(return AUnit)
              (fmap aSymbol symbolG)
              (fmap (AChar NoSourceText) arbitraryUnicodeChar)
              (fmap (aString NoSourceText) stringG)
              (fmap aIntegral (:: arbitrary (Gen Integer)))
              (fmap aFractional (:: arbitrary (Gen Double)))]))))

(instance (=> (Arbitrary a) (Arbitrary (Form a)))
  (defn arbitrary
    (oneof [(fmap Atom arbitrary)
            (fmap List (listOf (scale (flip div 3) arbitrary)))
            (fmap HsList (listOf (scale (flip div 3) arbitrary)))]))
  (defn shrink [x]
    (case x
      (Atom _)    []
      (List xs)   (++ (map unCode xs) (map List (shrink xs)))
      (HsList xs) (++ (map unCode xs) (map HsList (shrink xs)))
      TEnd        [])))

(instance (=> (Arbitrary a) (Arbitrary (LForm a)))
  (defn arbitrary (fmap (. LForm genSrc) arbitrary)))
