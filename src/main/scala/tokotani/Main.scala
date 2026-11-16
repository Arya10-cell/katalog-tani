package tokotani

import scala.annotation.tailrec
import upickle.default.*

// ---------- Model ----------

case class Produk(
    id: Int,
    name: String,
    category: String,
    price: Int,
    unit: String,
    stock: Int
) derives ReadWriter

enum StockMode:
  case All, Available, Empty

object StockMode:
  def parse(s: String): StockMode = s.trim.toLowerCase match
    case "available" => Available
    case "empty"     => Empty
    case _           => All

// ---------- Filtering (rekursi buatan sendiri) ----------

object Filtering:
  type Pred[A] = A => Boolean

  /** Menyaring elemen yang memenuhi predicate, tanpa List.filter.
    * Memakai tail recursion + akumulator agar aman untuk daftar besar.
    */
  def filterRec[A](xs: List[A], pred: Pred[A]): List[A] =
    @tailrec
    def loop(rest: List[A], acc: List[A]): List[A] = rest match
      case Nil          => acc.reverse
      case head :: tail => loop(tail, if pred(head) then head :: acc else acc)
    loop(xs, Nil)

  /** Menggabungkan banyak predicate menjadi satu (AND), juga secara rekursif. */
  def allOf[A](preds: List[Pred[A]]): Pred[A] = preds match
    case Nil => _ => true
    case p :: rest =>
      val restPred = allOf(rest)
      a => p(a) && restPred(a)

// ---------- Predicate untuk produk ----------

object Predicates:
  import Filtering.Pred

  /** Nama mengandung kata kunci (tidak peka huruf besar/kecil). Kosong = semua. */
  def nameContains(query: String): Pred[Produk] =
    val q = query.trim.toLowerCase
    p => q.isEmpty || p.name.toLowerCase.contains(q)

  /** Kategori sama persis (tidak peka huruf besar/kecil). Kosong = semua. */
  def inCategory(category: String): Pred[Produk] =
    val c = category.trim
    p => c.isEmpty || p.category.equalsIgnoreCase(c)

  /** Ketersediaan stok: semua, tersedia (stok > 0), atau habis (stok = 0). */
  def stockIs(mode: StockMode): Pred[Produk] = mode match
    case StockMode.All       => _ => true
    case StockMode.Available => _.stock > 0
    case StockMode.Empty     => _.stock == 0

// ---------- Data ----------

object Catalog:
  val all: List[Produk] = List(
    Produk(1, "Pupuk Urea 50 kg", "Pupuk", 275000, "karung", 48),
    Produk(2, "Pupuk NPK Phonska 50 kg", "Pupuk", 295000, "karung", 32),
    Produk(3, "Pupuk ZA 50 kg", "Pupuk", 185000, "karung", 0),
    Produk(4, "Pupuk Kandang Organik 25 kg", "Pupuk", 45000, "karung", 120),
    Produk(5, "Pupuk KCl 50 kg", "Pupuk", 520000, "karung", 6),
    Produk(6, "Benih Padi Inpari 32 (5 kg)", "Benih", 82000, "kantong", 40),
    Produk(7, "Benih Jagung Hibrida (1 kg)", "Benih", 95000, "kantong", 18),
    Produk(8, "Benih Cabai Rawit (10 g)", "Benih", 28000, "sachet", 0),
    Produk(9, "Benih Melon Golden (10 g)", "Benih", 45000, "sachet", 25),
    Produk(10, "Benih Kangkung (100 g)", "Benih", 15000, "sachet", 60),
    Produk(11, "Insektisida Decis (100 ml)", "Pestisida", 68000, "botol", 14),
    Produk(12, "Fungisida Antracol (250 g)", "Pestisida", 72000, "bungkus", 9),
    Produk(13, "Herbisida Roundup (1 L)", "Pestisida", 115000, "botol", 0),
    Produk(14, "Pestisida Nabati Neem (500 ml)", "Pestisida", 58000, "botol", 22),
    Produk(15, "Cangkul Baja", "Alat Pertanian", 85000, "buah", 30),
    Produk(16, "Sabit Bergerigi", "Alat Pertanian", 35000, "buah", 44),
    Produk(17, "Sprayer Elektrik 16 L", "Alat Pertanian", 420000, "unit", 5),
    Produk(18, "Selang Irigasi 50 m", "Alat Pertanian", 160000, "gulung", 11),
    Produk(19, "Mulsa Plastik Hitam Perak", "Alat Pertanian", 380000, "roll", 0)
  )

  def categories: List[String] = all.map(_.category).distinct.sorted

// ---------- Server (Cask) ----------

object Main extends cask.MainRoutes:
  override def host: String = "0.0.0.0"
  override def port: Int = 8080

  @cask.get("/")
  def index() = cask.model.StaticResource(
    "static/index.html",
    getClass.getClassLoader,
    Seq("Content-Type" -> "text/html; charset=utf-8")
  )

  @cask.get("/api/categories")
  def categories(): ujson.Value = writeJs(Catalog.categories)

  @cask.get("/api/products")
  def products(q: String = "", category: String = "", stock: String = "all"): ujson.Value =
    val predicate = Filtering.allOf(
      List(
        Predicates.nameContains(q),
        Predicates.inCategory(category),
        Predicates.stockIs(StockMode.parse(stock))
      )
    )
    val items = Filtering.filterRec(Catalog.all, predicate)
    ujson.Obj(
      "total" -> Catalog.all.size,
      "count" -> items.size,
      "items" -> writeJs(items)
    )

  initialize()
