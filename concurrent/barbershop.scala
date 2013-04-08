/**
 * This is my Scala solution to the sleeping barber problem
 * (http://en.wikipedia.org/wiki/Sleeping_barber_problem).
 *
 * We create one actor per customer, one actor for the barber, and
 * one actor for a shop.  The shop owns a queue of seats.  The customer
 * asks the shop if a seat is available.  If there's a seat, the customer
 * is added to the queue, otherwise the shop lets the customer know to try
 * again later.  The barber tells the shop he's looking for work.  If
 * there's a customer waiting, the barber is asked to shave the customer.
 * Otherwise, the barber is added to a queue.  When a customer walks in,
 * the barber is removed from the queue and asked to shave the customer.
 * Once the barber has shaved a number of customers equal to the total
 * number of customers, he notifies the shop and simulation to shutdown.
 * The simulation then verifies that all customers are shaved.
 */

import scala.actors.Actor
import scala.collection.mutable.Queue

val totalSeats = 4
val totalCustomers = 1000

// Messages
case class LookingForWork(barber: Actor)
case object CloseUpShop
case object SorryTryAgain
case object Shaved
case class TakeASeat(cust: Actor)
case class PleaseShave(cust: Actor)

// Represents queues of customers and barbers.
class Shop extends Actor {
  private val customers = Queue[Actor]()
  private val barbers   = Queue[Actor]()
  def act {
    loop {
      react {
	case TakeASeat(cust) =>
	  if (!barbers.isEmpty) {
	    barbers.dequeue ! PleaseShave(cust)
	  } else {
	    if (customers.size < totalSeats) {
	      customers += cust
	    } else {
	      cust ! SorryTryAgain
	    }
	  }

	case LookingForWork(barber) =>
	  if (!customers.isEmpty) {
	    barber ! PleaseShave(customers.dequeue)
	  } else {
	    barbers += barber
	  }
	
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
    shop ! TakeASeat(this)
    react {
      case SorryTryAgain =>
	act
      case Shaved =>
	face = 'shaved
    }
  }
}

// The barber tells the shop that he's looking for work.
// If there's a customer in the shop, the shop will ask
// the barber to please shave the customer.
class Barber(sim: Actor, shop: Actor) extends Actor {
  private var hairCutsGiven = 0
  def act {
    loop {
      shop ! LookingForWork(this)
      react {
	case PleaseShave(cust) =>
	  hairCutsGiven += 1
	  println("Shave #" + hairCutsGiven)
	  cust ! Shaved
	  if (hairCutsGiven == totalCustomers) {
	    shop ! CloseUpShop
	    sim ! CloseUpShop
	    exit
	  }
      }
    }
  }
}

// The simulation owns all the actors and starts them up.
// Once everyone is shaved, the Sim is notified and it verifies
// that all customer's are 'shaved.
class Sim extends Actor {
  private val shop = new Shop
  private val barber = new Barber(this, shop)
  private val customers = 1 to totalCustomers map { _ => new Customer(shop) }
  shop.start
  barber.start
  customers foreach (cust => cust.start)

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
