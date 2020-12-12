package days

import cats.data.StateT
import cats.effect.{ContextShift, IO}
import cats.implicits._
import days.Twelve.CompassDirection._
import days.Twelve.{Command, CompassDirection, RelativeDirection}
import main.Main.Logger

class Twelve(implicit val cs: ContextShift[IO], val log: Logger[IO]) extends Day {
  override implicit val name: DayName = DayName("twelve")
  override type IN1 = Command
  override type IN2 = Command


  override def calculatePartOne(input: List[Command]): IO[String] = {
    case class Position(facing: CompassDirection, x: Int, y: Int)
    def applyCommand(command: Command): StateT[IO, Position, Unit] =
      StateT.modify { (position: Position) =>
        command match {
          case Command.Forward(distance) => position.facing match {
            case CompassDirection.North => position.copy(y = position.y + distance)
            case CompassDirection.South => position.copy(y = position.y - distance)
            case CompassDirection.East => position.copy(x = position.x + distance)
            case CompassDirection.West => position.copy(x = position.x - distance)
          }
          case Command.Turn(RelativeDirection.Right) => position.copy(facing = position.facing.right)
          case Command.Turn(RelativeDirection.Left) => position.copy(facing = position.facing.left)
          case Command.Turn(RelativeDirection.OneEighty) => position.copy(facing = position.facing.opposite)
          case Command.Move(direction, distance) => direction match {
            case CompassDirection.North => position.copy(y = position.y + distance)
            case CompassDirection.South => position.copy(y = position.y - distance)
            case CompassDirection.East => position.copy(x = position.x + distance)
            case CompassDirection.West => position.copy(x = position.x - distance)
          }
        }
      }

    input
      .map(applyCommand)
      .sequence
      .runS(Position(East, 0, 0))
      .map(p => s"$p, distance:${p.x.abs + p.y.abs}")
  }

  override def calculatePartTwo(input: List[Command]): IO[String] = {
    case class BoatPosition(facing: CompassDirection, x: Int, y: Int)
    case class FullState(boatPosition: BoatPosition, waypointX: Int, waypointY: Int)

    def applyCommand(command: Command): StateT[IO, FullState, Unit] =
      StateT.modify { case FullState(boatPosition, waypointX, waypointY) =>
        command match {
          case Command.Move(direction, distance) => direction match {
            case CompassDirection.North => FullState(boatPosition, waypointX, waypointY + distance)
            case CompassDirection.South => FullState(boatPosition, waypointX, waypointY - distance)
            case CompassDirection.East => FullState(boatPosition, waypointX + distance, waypointY)
            case CompassDirection.West => FullState(boatPosition, waypointX - distance, waypointY)
          }
          case Command.Forward(distance) =>
            val newX = boatPosition.x + (distance * waypointX)
            val newY = boatPosition.y + (distance * waypointY)
            FullState(boatPosition.copy(x = newX, y = newY), waypointX, waypointY)
          case Command.Turn(RelativeDirection.Right) => FullState(boatPosition, waypointY, -waypointX)
          case Command.Turn(RelativeDirection.Left) => FullState(boatPosition, -waypointY, waypointX)
          case Command.Turn(RelativeDirection.OneEighty) => FullState(boatPosition, -waypointX, -waypointY)
        }
      }

    input
      .map(applyCommand)
      .sequence
      .runS(FullState(BoatPosition(East, 0, 0), 10, 1))
      .map(s => s"$s, distance:${s.boatPosition.x.abs + s.boatPosition.y.abs}")
  }

  override def parsePartOne: String => IO[Command] = s => Command.parse(s).liftTo[IO](new IllegalArgumentException(s"Can't parse $s"))

  override def parsePartTwo: String => IO[Command] = parsePartOne
}

object Twelve {

  sealed trait CompassDirection {
    def right: CompassDirection

    def left: CompassDirection

    def opposite: CompassDirection
  }

  object CompassDirection {

    case object North extends CompassDirection {
      override def right: CompassDirection = East

      override def left: CompassDirection = West

      override def opposite: CompassDirection = South
    }

    case object South extends CompassDirection {
      override def right: CompassDirection = West

      override def left: CompassDirection = East

      override def opposite: CompassDirection = North
    }

    case object East extends CompassDirection {
      override def right: CompassDirection = South

      override def left: CompassDirection = North

      override def opposite: CompassDirection = West
    }

    case object West extends CompassDirection {
      override def right: CompassDirection = North

      override def left: CompassDirection = South

      override def opposite: CompassDirection = East
    }

  }

  sealed trait RelativeDirection

  object RelativeDirection {

    case object Left extends RelativeDirection

    case object Right extends RelativeDirection

    case object OneEighty extends RelativeDirection

  }

  sealed trait Command

  object Command {

    case class Forward(distance: Int) extends Command

    case class Turn(direction: RelativeDirection) extends Command

    case class Move(direction: CompassDirection, distance: Int) extends Command

    def parse(str: String): Option[Command] =
      str.splitAt(1) match {
        case ("N", d) => d.toIntOption.map(i => Move(North, i))
        case ("S", d) => d.toIntOption.map(i => Move(South, i))
        case ("E", d) => d.toIntOption.map(i => Move(East, i))
        case ("W", d) => d.toIntOption.map(i => Move(West, i))
        case ("L", "90") | ("R", "270") => Turn(RelativeDirection.Left).some
        case ("R", "90") | ("L", "270") => Turn(RelativeDirection.Right).some
        case ("R", "180") | ("L", "180") => Turn(RelativeDirection.OneEighty).some
        case ("F", d) => d.toIntOption.map(Forward)
        case _ => None
      }
  }

}


