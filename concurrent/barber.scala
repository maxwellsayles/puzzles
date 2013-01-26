/**
 * This is my Scala solution to the sleeping barber problem
 * (http://en.wikipedia.org/wiki/Sleeping_barber_problem).
 *
 * We create one actor per customer, one actor for the barber, and
 * one actor for a shop.  The shop owns a queue of seats.  The customer
 * asks the shop if a seat is available.  If there's a seat, the customer
 * is added to the queue, otherwise the shop lets the customer know to try
 * again later.  When the barber is finished taking a break, it asks the
 * shop if anyone is seated.  If there is, the shop asks the barber to
 * shave the customer and removes him from the queue.  The barber then
 * notifies the customer that he has been shaved, and the customer changes
 * his state and then quites acting.  If there is no one seated in the shop,
 * the shop tells the barber to take another break. Once the barber has
 * shaved a number of customers equal to the total number of customers,
 * he notifies the shop to close down, and the simulation that it's over.
 * The simulation then verifies that all customers are shaved.
 */

import scala.actors.Actor
import scala.collection.mutable.Queue

val totalSeats = 4
val totalCustomers = 1000

// Messages
case class LookingForWork(barber: Actor)
case object TakeABreak
case object CloseUpShop
case object SorryTryAgain
case object Shaved
case class TakeASeat(cust: Actor)
case class PleaseShave(cust: Actor)

// Represents a queue of customers.
class Shop extends Actor {
  private val seated = Queue[Actor]()
  def act {
    loop {
      react {
	case TakeASeat(cust) =>
	  // If a seat is available, add the customer, otherwise
	  // tell him to try again.
	  if (seated.size < totalSeats) {
	    seated += cust
	  } else {
	    cust ! SorryTryAgain
	  }
	
	case LookingForWork(barber) =>
	  // If there's customer in the seat, ask the barber to
	  // shave him.  Either way, tell the barber to take a break.
	  if (!seated.isEmpty) {
	    barber ! PleaseShave(seated.dequeue)
	  }
	  barber ! TakeABreak
	
	case CloseUpShop =>
	  exit
      }
    }
  }
}

// A customer's face is either 'hairy or 'shaved.
// The customer tries to take a seat in the barber shop.
// If no seat is available the shop will tell him to try again.
class Customer(shop: Actor) extends Actor {
  private var face = 'hairy
  def getFace = face
  def act {
    react {
      case SorryTryAgain =>
	shop ! TakeASeat(this)
	act
      case Shaved =>
	face = 'shaved
    }
  }
}

// When the barber is on break, he asks the shop for work.
// If there's a customer in the shop, the shop will ask
// the barber to please shave the customer.
class Barber(sim: Actor, shop: Actor) extends Actor {
  private var hairCutsGiven = 0
  def act {
    loop {
      receive {
	case PleaseShave(cust) =>
	  hairCutsGiven += 1
	  println("Shave #" + hairCutsGiven)
	  cust ! Shaved
	  if (hairCutsGiven == totalCustomers) {
	    shop ! CloseUpShop
	    sim ! CloseUpShop
	    exit
	  }
	case TakeABreak =>
	  shop ! LookingForWork(this)
	  act
      }
    }
  }
}

// The simulation owns all the actors and starts them up.
// Once everyone is shaved, the Sim is notified and it verifies
// that all customer's are 'shaved.
class Sim extends Actor {
  private val shop = new Shop
  private val customers = 1 to totalCustomers map { _ => new Customer(shop) }
  private val barber = new Barber(this, shop)
  barber.start
  shop.start
  customers foreach (cust => cust.start)
  barber ! TakeABreak
  customers foreach (cust => cust ! SorryTryAgain)

  def act {
    react {
      case CloseUpShop =>
	if (customers.forall(cust => cust.getFace == 'shaved))
	  println("Every one is bald!")
	else
	  println("Something went wrong!")
    }
  }
}

val sim = new Sim
sim.start
