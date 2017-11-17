-- You may have seen mad libs before. The idea is to take a template of
-- phrases, fill them in with blindly selected categories of words, and
-- see if saying the final version is amusing.
-- Using a lightly edited example from the Wikipedia article on Mad
-- Libs:

import Data.Monoid

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation
           -> Adverb
           -> Noun
           -> Adjective
           -> String
madlibbin' e adv noun adj =
    e <> "! he said " <>
    adv <> " as he jumped into his car " <>
    noun <> " and drove off with his " <>
    adj <> " wife."

-- Now you’re going to refactor this code a bit! Rewrite it using
-- mconcat.
madlibbinBetter' :: Exclamation
                 -> Adverb
                 -> Noun
                 -> Adjective
                 -> String
madlibbinBetter' e adv noun adj =
    mconcat 
      [ e,
        "! he said ",
        adv,
        " as he jumped into his car ",
        noun,
        " and drove off with his ",
        adj,
        " wife."
      ]

