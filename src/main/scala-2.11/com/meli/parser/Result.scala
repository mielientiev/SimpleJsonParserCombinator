package com.meli.parser

sealed abstract class Result[+A] {
  def map[U](fun: A => U): Result[U]
  def isFailure: Boolean
  def get: A
  def getOrElse[B >: A](default: => B): B =  if (isFailure) default else this.get
}

case class Success[A](result: A, in: String) extends Result[A] {
  override def map[U](fun: (A) => U): Result[U] = Success(fun(result), in)

  override def isFailure = false

  override def get: A = result
}

case class Failure(msg: String, in: String) extends Result[Nothing] {
  override def map[U](fun: (Nothing) => U): Result[U] = Failure(msg, in)

  override def isFailure = true

  override def get = throw new NoSuchElementException("Failure.get")
}
