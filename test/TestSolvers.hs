module TestSolvers where
import Distribution.TestSuite
import qualified Problem1
import qualified Problem15
import qualified Problem37


tests :: IO [Test]
tests = return tests'
  where
    tests' = map (\tc -> Test tc) testCases

    testCases = [tc1, tc37]

    testCase problem bool = TestInstance
          { run = return $ check bool
          , name = problem
          , tags = []
          , options = []
          , setOption = \_ _ -> Left problem
          }

    tc1  = testCase "Problem1"  (Problem1.solve  == 233168)
    tc15 = testCase "Problem15" (Problem15.solve == 137846528820)
    tc37 = testCase "Problem37" (Problem37.solve == 748317)

    check bool =
      if bool
      then Finished Pass
      else Finished $ Fail "Incorrect!"
