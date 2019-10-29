package com.foo.mars

case class RoverPosition(x: Int, y: Int)
case class RoverDirection(d: Direction)
case class RoverState(position: RoverPosition, roverDirection: RoverDirection){
  override def toString: String = position.x + " " + position.y + " " + roverDirection
}

sealed trait Direction
case object N extends Direction
case object E extends Direction
case object W extends Direction
case object S extends Direction


sealed trait Instruction
case object L extends Instruction
case object R extends Instruction
case object M extends Instruction

