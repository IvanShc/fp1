package com.tkroman.kpi.y2022.l1

import munit.FunSuite

class ListSuite extends FunSuite {
  test("Reverse list on Nil") {
    val expected = List.Nil
    val actual = reverse(List.Nil)
    assertEquals(actual, expected)
  }
  test("Reverse list on Cons") {
    val expected = List(4, 3, 2, 1)
    val actual = reverse(List(1, 2, 3, 4))
    assertEquals(actual, expected)
  }
  test("Count list on Nil") {
    val expected = 0
    val actual = count(List.Nil, 0)
    assertEquals(actual, expected)
  }
  test("Count list on Cons") {
    val expected = 3
    val actual = count(List(1,2,1,3,4,1,5), 1)
    assertEquals(actual, expected)
  }
  test("Last element on Nil") {
    val expected = None
    val actual = last(List.Nil)
    assertEquals(actual, expected)
  }
  test("Last element on Cons") {
    val expected = Some(5)
    val actual = last(List(1,2,3,4,5))
    assertEquals(actual, expected)
  }
  test("First element on Nil") {
    val expected = None
    val actual = first(List.Nil)
    assertEquals(actual, expected)
  }
  test("First element on Cons") {
    val expected = Some(1)
    val actual = first(List(1,2,3,4,5))
    assertEquals(actual, expected)
  }
  test("Corresponds lists 1") {
    val expected = true
    val actual = corresponds(List.Nil, List.Nil, (x, y) => x == y)
    assertEquals(actual, expected)
  }
  test("Correspondents lists 2") {
    val expected = false
    val actual = corresponds(List.Nil, List(0), (x, y) => x == y)
    assertEquals(actual, expected)
  }
  test("Correspondents lists 3") {
    val expected = true
    val actual = corresponds(List(1,2,3), List(2,4,6), (x, y) => x*2 == y)
    assertEquals(actual, expected)
  }
}