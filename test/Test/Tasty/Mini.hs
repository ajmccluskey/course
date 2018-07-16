{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImplicitPrelude       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Test.Tasty.Mini where

import qualified Test.Tasty            as T
import qualified Test.Tasty.HUnit      as T
import qualified Test.Tasty.QuickCheck as T

import           Test.Mini             (PropertyTester (..), Tester (..),
                                        UnitTester (..))


newtype TastyAssertion =
  TA {getTA :: IO ()}

newtype TastyTree =
  TT {getTT :: T.TestTree}

instance Tester TastyTree T.TestName where
  testGroup n =
    TT . T.testGroup n . fmap getTT

  test = T.defaultMain . getTT

instance UnitTester TastyTree T.TestName TastyAssertion where
  testCase n =
    TT . T.testCase n . getTA

  (@?=) = (TA .) . (T.@?=)

instance T.Testable a => PropertyTester TastyTree T.TestName a where
  testProperty n =
    TT . T.testProperty n


tastyTest ::
  TastyTree
  -> IO ()
tastyTest = test

