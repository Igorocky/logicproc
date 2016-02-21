package org.igye.logic.predicates

trait ObjectCache[T] {
  private var cache_ = Set[T]()

  def cache(obj: T): T = {
    cache_.find(_ == obj).getOrElse{
      cache_ += obj
      obj
    }
  }
}
