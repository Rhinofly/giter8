package giter8.interaction

sealed trait UserResponse
case object Append extends UserResponse
case object Override extends UserResponse
case object Skip extends UserResponse