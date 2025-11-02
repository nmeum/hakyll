--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Core.Dependencies.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import           Data.List                (delete)
import qualified Data.Map                 as M
import qualified Data.Set                 as S
import           Test.Tasty               (TestTree, testGroup)
import           Test.Tasty.HUnit         (Assertion, (@=?))


--------------------------------------------------------------------------------
import           Hakyll.Core.Dependencies
import           Hakyll.Core.Identifier
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Hakyll.Core.Dependencies.Tests" $
    fromAssertions "analyze" [case01, case02, case03, case04]


--------------------------------------------------------------------------------
oldUniverse :: [Identifier]
oldUniverse = M.keys oldFacts


--------------------------------------------------------------------------------
oldFacts :: DependencyFacts
oldFacts = M.fromList
    [ ("posts/01.md",
        [])
    , ("posts/02.md",
        [])
    , ("index.md",
        [ Dependency KindContent $ PatternDependency "posts/*"
            (S.fromList ["posts/01.md", "posts/02.md"])
        , Dependency KindContent $ IdentifierDependency "posts/01.md"
        , Dependency KindContent $ IdentifierDependency "posts/02.md"
        ])
    , ("sidebar",
        [ Dependency KindMetadata $ PatternDependency "posts/*"
            (S.fromList ["posts/01.md", "posts/02.md"])
        ])
    ]


--------------------------------------------------------------------------------
-- | posts/02.md has changed
case01 :: Assertion
case01 = S.fromList ["posts/02.md", "index.md"] @=? ood
  where
    (ood, _, _) = outOfDate oldUniverse (S.singleton "posts/02.md") S.empty oldFacts


--------------------------------------------------------------------------------
-- | about.md was added
case02 :: Assertion
case02 = S.singleton "about.md" @=? ood
  where
    (ood, _, _) = outOfDate ("about.md" : oldUniverse) S.empty S.empty oldFacts


--------------------------------------------------------------------------------
-- | posts/01.md was removed
case03 :: Assertion
case03 = S.fromList ["index.md", "sidebar"] @=? ood
  where
    (ood, _, _) =
        outOfDate ("posts/01.md" `delete` oldUniverse) S.empty S.empty oldFacts


--------------------------------------------------------------------------------
-- | metadata of posts/01.md was changed
case04 :: Assertion
case04 = S.singleton "sidebar" @=? ood
  where
    (ood, _, _) =
        outOfDate oldUniverse S.empty (S.singleton "posts/01.md") oldFacts
