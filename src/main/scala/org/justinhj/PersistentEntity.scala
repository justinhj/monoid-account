package org.justinhj

import java.time.Instant

import cats._
import cats.implicits._
import cats.{ Eq, Monoid, Show }

trait PersistentEntity[IDType, T <: PersistentEntity[IDType, T]] {
  type Command
  type Event
  type State

  def id: IDType
  def state: State

  def processCommand(command: Command): List[Event]
  def processEvent(event: Event): T
}

sealed trait BankAccountCommand
case class DepositCmd(time: Instant, amount: Int)                       extends BankAccountCommand
case class PurchaseCmd(time: Instant, amount: Int)                      extends BankAccountCommand
case class AssignAccountHolderCmd(time: Instant, accountHolder: String) extends BankAccountCommand

sealed trait BankAccountEvent
case class DepositEvt(time: Instant, amount: Int)                       extends BankAccountEvent
case class PurchaseEvt(time: Instant, amount: Int)                      extends BankAccountEvent
case class AssignAccountHolderEvt(time: Instant, accountHolder: String) extends BankAccountEvent

object LastOptionHelper {
  object LastOption {
    implicit def lastOptionMonoid[A]: Monoid[LastOption[A]] =
      new Monoid[LastOption[A]] {
        def combine(a1: LastOption[A], a2: LastOption[A]): LastOption[A] =
          LastOption(a2.opt.orElse(a1.opt))

        def empty: LastOption[A] = LastOption(None)
      }
    implicit def lastOptionEq[A]: Eq[LastOption[A]] = new Eq[LastOption[A]] {
      def eqv(a1: LastOption[A], a2: LastOption[A]): Boolean =
        a1.opt == a2.opt
    }
    implicit def lastOptiowShow[A: Show]: Show[LastOption[A]] =
      new Show[LastOption[A]] {
        def show(a: LastOption[A]): String =
          a.opt match {
            case Some(a) =>
              a.show
            case None =>
              "None"
          }
      }
  }

  implicit final class LastOption[A](val opt: Option[A]) extends AnyVal
}

import LastOptionHelper._

case class AccountState(balance: Int, accountHolder: LastOption[String])

object AccountState {

  implicit def accountStateShow[A] = new Show[AccountState] {
    def show(a: AccountState): String =
      show"Account holder: ${a.accountHolder}\nBalance: ${a.balance}"
  }

  implicit val accountMonoid = new Monoid[AccountState] {
    def empty: AccountState = AccountState(0, None)
    def combine(p1: AccountState, p2: AccountState): AccountState =
      AccountState(
        p1.balance |+| p2.balance,
        p1.accountHolder |+| p2.accountHolder
      )
  }
}

case class AccountEntity(id: Int, state: AccountState) extends PersistentEntity[Int, AccountEntity] {

  override type Command = BankAccountCommand
  override type Event   = BankAccountEvent
  override type State   = AccountState

  // Applies the command to the current state, returning a list of events
  def processCommand(command: Command): List[Event] =
    command match {
      case DepositCmd(time, amount) =>
        List(DepositEvt(time, amount))
      case PurchaseCmd(time, amount) =>
        if (amount <= state.balance)
          List(PurchaseEvt(time, amount))
        else
          List.empty
      case AssignAccountHolderCmd(time, accountHolder) =>
        List(AssignAccountHolderEvt(time, accountHolder))
    }

  // Processing an event applies changes to the current state
  // Since our entity is immutable we return a new one
  def processEvent(event: Event): AccountEntity =
    event match {
      case DepositEvt(time, amount) =>
        AccountEntity(
          id,
          AccountState(state.balance + amount, state.accountHolder)
        )
      case PurchaseEvt(time, amount) =>
        AccountEntity(
          id,
          AccountState(state.balance - amount, state.accountHolder)
        )
      case AssignAccountHolderEvt(time, accountHolder) =>
        AccountEntity(id, AccountState(state.balance, accountHolder.some))
    }

}

object BankAccountSample {

  def main(args: Array[String]): Unit = {
    val t1 = Instant.now

    val sampleAccount = AccountEntity(1, AccountState(0, None))

    val commands = List(
      DepositCmd(t1.plusSeconds(10), 100),
      PurchaseCmd(t1.plusSeconds(20), 120),
      AssignAccountHolderCmd(t1.plusSeconds(40), "Bob Johnson"),
      DepositCmd(t1.plusSeconds(40), 100),
      AssignAccountHolderCmd(t1.plusSeconds(50), "Ben Johnson"),
      PurchaseCmd(t1.plusSeconds(60), 120)
    )

    // With Monoids. We need a way to convert events to states
    def eventToState(event: BankAccountEvent): AccountState =
      event match {
        case DepositEvt(time, amount) =>
          AccountState(amount, None)
        case PurchaseEvt(time, amount) =>
          AccountState(-amount, None)
        case AssignAccountHolderEvt(time, accountHolder) =>
          AccountState(0, accountHolder.some)
      }

    val (finalEntity, finalEvents) =
      commands.foldLeft((sampleAccount, List.empty[BankAccountEvent])) {
        case ((acc, events), cmd) =>
          val newEvents = acc.processCommand(cmd)
          val newAcc = newEvents.foldLeft(acc) {
            case (acc, evt) =>
              acc.processEvent(evt)
          }
          (newAcc, events ++ newEvents)
      }

    println(show"Final state 1:\n${finalEntity.state}")

    val finalState2 = finalEvents.map(eventToState).combineAll

    println(show"Final state 2:\n$finalState2")
  }
}
