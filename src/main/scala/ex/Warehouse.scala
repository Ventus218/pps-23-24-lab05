package ex

import util.Optionals.Optional
import util.Sequences.*
import polyglot.a01b.length

trait Item:
  def code: Int
  def name: String
  def tags: Sequence[String]

object Item:
  private case class ItemImpl(val code: Int, val name: String, val tags: Sequence[String]) extends Item

  def apply(code: Int, name: String, tags: String*): Item =
    val tagsSequence = tags.foldLeft(Sequence[String]())((seq, tag) => seq concat Sequence(tag))
    ItemImpl(code, name, tagsSequence)

/**
 * A warehouse is a place where items are stored.
 */
trait Warehouse:
  /**
   * Stores an item in the warehouse.
   * @param item the item to store
   */
  def store(item: Item): Unit
  /**
   * Searches for items with the given tag.
   * @param tag the tag to search for
   * @return the list of items with the given tag
   */
  def searchItems(tag: String): Sequence[Item]
  /**
   * Retrieves an item from the warehouse.
   * @param code the code of the item to retrieve
   * @return the item with the given code, if present
   */
  def retrieve(code: Int): Optional[Item]
  /**
   * Removes an item from the warehouse.
   * @param item the item to remove
   */
  def remove(item: Item): Unit
  /**
   * Checks if the warehouse contains an item with the given code.
   * @param itemCode the code of the item to check
   * @return true if the warehouse contains an item with the given code, false otherwise
   */
  def contains(itemCode: Int): Boolean
end Warehouse

object Warehouse:
  def apply(): Warehouse = WarehouseImpl(Sequence())

  private class WarehouseImpl(private var items: Sequence[Item]) extends Warehouse:
    override def store(item: Item): Unit =
      items = items concat Sequence(item)

    override def searchItems(tag: String): Sequence[Item] =
      items.filter(_.tags.contains(tag))

    override def retrieve(code: Int): Optional[Item] =
      items.find(_.code == code)

    override def remove(item: Item): Unit =
      items = items.filter(_ != item)

    override def contains(itemCode: Int): Boolean =
      !retrieve(itemCode).isEmpty

extension [T] (opt: Optional[T])
  def get(): T = opt match
    case Optional.Just(n) => n
    case Optional.Empty() => throw new NoSuchElementException("Unwrapped Optional was empty")

extension [E] (s: Sequence[E])
  def firstWhere(predicate: E => Boolean): Optional[E] = s match
    case Sequence.Cons(h, t) => if predicate(h) then Optional.Just(h) else t.firstWhere(predicate)
    case _ => Optional.Empty()

object OptionalToOption:
  def apply[A](optional: Optional[A]): Option[A] = optional match
    case Optional.Just(a) => Option(a)
    case Optional.Empty() => Option.empty

object sameTag:
  def unapply(items: Sequence[Item]): Option[String] =
    if items.head.isEmpty then
      Option.empty
    else
      // if there is a tag common to all items then checking the tags of the first item is just enough
      val firstItem = items.head.get()
      OptionalToOption(firstItem.tags.firstWhere(tag => items.filter(_.tags.contains(tag)).length() == items.length()))


@main def mainWarehouse(): Unit =
  val warehouse = Warehouse()

  val dellXps = Item(33, "Dell XPS 15", "notebook")
  val dellInspiron = Item(34, "Dell Inspiron 13", "notebook")
  val xiaomiMoped = Item(35, "Xiaomi S1", "moped", "mobility")

  println:
    warehouse.contains(dellXps.code) // false
  println:
    warehouse.store(dellXps) // side effect, add dell xps to the warehouse
  println:
    warehouse.contains(dellXps.code) // true
  println:
    warehouse.store(dellInspiron) // side effect, add dell Inspiron to the warehouse
  println:
    warehouse.store(xiaomiMoped) // side effect, add xiaomi moped to the warehouse
  println:
    warehouse.searchItems("mobility") // Sequence(xiaomiMoped)
  println:
    warehouse.searchItems("notebook") // Sequence(dellXps, dell Inspiron)
  println:
    warehouse.retrieve(11) // None
  println:
    warehouse.retrieve(dellXps.code) // Just(dellXps)
  println:
    warehouse.remove(dellXps) // side effect, remove dell xps from the warehouse
  println:
    warehouse.retrieve(dellXps.code) // None

  val itemsWithCommonTag = Sequence(dellXps, dellInspiron)
  itemsWithCommonTag match
    case sameTag(t) => println(s"TEST PASSED: items have same tag $t")
    case _ => println(s"TEST FAILED: items do not have common tags")

  val itemsWithoutCommonTag = Sequence(dellXps, dellInspiron, xiaomiMoped)
  itemsWithoutCommonTag match
    case sameTag(t) => println(s"TEST FAILED: items should not have same tag $t")
    case _ => println(s"TEST PASSED: items do not have common tags")

/** Hints:
 * - Implement the Item with a simple case class
 * - Implement the Warehouse keeping a private List of items
 * - Start implementing contains and store
 * - Implement searchItems using filter and contains
 * - Implement retrieve using find
 * - Implement remove using filter
 * - Refactor the code of Item accepting a variable number of tags (hint: use _*)
*/