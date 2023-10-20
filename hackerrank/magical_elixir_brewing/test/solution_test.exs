defmodule SolutionTest do
  use ExUnit.Case
  doctest Solution  # This line is important!

  test "Inputs from test case 0" do
    assert Solution.min_cauldrons({1, 5}) == 2
    assert Solution.min_cauldrons({2, 6}) == 3
    assert Solution.min_cauldrons({4, 3}) == 3
    assert Solution.min_cauldrons({3, 5}) == 4
  end

  test "Inputs from test case 1" do
    assert Solution.min_cauldrons({9, 9}) == 21
    assert Solution.min_cauldrons({9, 7}) == 16
    assert Solution.min_cauldrons({5, 5}) == 7
    assert Solution.min_cauldrons({7, 7}) == 13
    assert Solution.min_cauldrons({1, 9}) == 3
    assert Solution.min_cauldrons({2, 10}) == 5
  end

  test "Inputs from test case 2" do
    assert Solution.min_cauldrons({10, 7}) == 18
    assert Solution.min_cauldrons({10, 8}) == 20
    assert Solution.min_cauldrons({10, 9}) == 23
    assert Solution.min_cauldrons({10, 10}) == 25
  end

end
