package com.foo.mars

object Driver extends App{


  def calculateNewDirection(direction: Direction, instruction: Instruction): Direction ={
    direction match {
      case N => instruction match {
        case L => W
        case R => E
      }
      case W => instruction match {
        case L => S
        case R => N
      }
      case E => instruction match {
        case L => N
        case R => S
      }
      case S => instruction match {
        case L => E
        case R => W
      }
    }
  }
  def calculateNewPosition(previousState: RoverState): RoverPosition = {
    previousState.roverDirection.d match {
      case N => RoverPosition(previousState.position.x, previousState.position.y + 1)
      case E =>RoverPosition(previousState.position.x + 1, previousState.position.y)
      case W =>RoverPosition(previousState.position.x - 1, previousState.position.y)
      case S => RoverPosition(previousState.position.x, previousState.position.y - 1)
    }
  }

 def move(instruction: Instruction): State[RoverState, Instruction] = State{(previousState: RoverState) =>
   println(previousState)
   instruction match {
     case L | R =>
       val direction = calculateNewDirection(previousState.roverDirection.d, instruction)
       (RoverState(previousState.position, RoverDirection(direction)),instruction)
     case M =>
       val roverPosition = calculateNewPosition(previousState)
       (RoverState(roverPosition, previousState.roverDirection),instruction)
   }
 }

  case class State[S, A](run: S => (S, A)){
    def flatMap[B](f: (A) => State[S,B]): State[S,B]= State{(s0: S) =>
      val (s1, a) = run(s0)
      f(a).run(s1)
    }

    def map[B](f: A => B): State[S,B] = flatMap(a => State.point(f(a)))
  }

  object State{
    def point[S,A](v: A): State[S,A] = State(run = s => (s,v))
  }
  val initialStateOfRover1 = RoverState(RoverPosition(1,2),RoverDirection(N))
  val initialStateOfRover2 = RoverState(RoverPosition(3,3),RoverDirection(E))
  val modifiedState2: State[RoverState, Instruction]  = for{
    _ <- move(M)
    _ <- move(M)
    _ <- move(R)
    _ <- move(M)
    _ <- move(M)
    _ <- move(R)
    _ <- move(M)
    _ <- move(R)
    _ <- move(R)
    finalState <- move(M)
  } yield finalState

  val modifiedState1: State[RoverState, Instruction]  = for{
    _ <- move(L)
    _ <- move(M)
    _ <- move(L)
    _ <- move(M)
    _ <- move(L)
    _ <- move(M)
    _ <- move(L)
    _ <- move(M)
    finalState <- move(M)
  } yield finalState
val res2 =   modifiedState2.run(initialStateOfRover2)
val res1 = modifiedState1.run(initialStateOfRover1)

println(res1)
println(res2)
}
