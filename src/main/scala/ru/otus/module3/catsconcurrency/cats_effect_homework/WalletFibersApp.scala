package ru.otus.module3.catsconcurrency.cats_effect_homework

import cats.effect.{IO, IOApp}
import cats.implicits._
import scala.concurrent.duration._

// Поиграемся с кошельками на файлах и файберами.

// Нужно написать программу где инициализируются три разных кошелька и для каждого из них работает фоновый процесс,
// который регулярно пополняет кошелек на 100 рублей раз в определенный промежуток времени. Промежуток надо сделать разный, чтобы легче было наблюдать разницу.
// Для определенности: первый кошелек пополняем раз в 100ms, второй каждые 500ms и третий каждые 2000ms.
// Помимо этих трёх фоновых процессов (подсказка - это файберы), нужен четвертый, который раз в одну секунду будет выводить балансы всех трех кошельков в консоль.
// Основной процесс программы должен просто ждать ввода пользователя (IO.readline) и завершить программу (включая все фоновые процессы) когда ввод будет получен.
// Итого у нас 5 процессов: 3 фоновых процесса регулярного пополнения кошельков, 1 фоновый процесс регулярного вывода балансов на экран и 1 основной процесс просто ждущий ввода пользователя.

// Можно делать всё на IO, tagless final тут не нужен.

// Подсказка: чтобы сделать бесконечный цикл на IO достаточно сделать рекурсивный вызов через flatMap:
// def loop(): IO[Unit] = IO.println("hello").flatMap(_ => loop())
object WalletFibersApp extends IOApp.Simple {

  def topupLoop(wallet: Wallet[IO], interval: FiniteDuration): IO[Unit] = {
    IO.println(s"Пополняем кошелек").flatMap(_ => IO.sleep(interval) *> wallet.topup(100) *> topupLoop(wallet, interval))
  }

  def printBalancesLoop(wallets: List[Wallet[IO]]): IO[Unit] = 
    for {
      balances <- wallets.traverse(_.balance)
      _ <- IO.println(s"Балансы кошельков: ${balances.zipWithIndex.map { case (b, i) => s"Кошелек ${i + 1}: $b" }.mkString(", ")}")
      _ <- IO.sleep(1.second)
      _ <- printBalancesLoop(wallets)
    } yield ()

  def run: IO[Unit] =
    for {
      _ <- IO.println("Введите любой символ, чтобы завершить программу...")
      wallet1 <- Wallet.fileWallet[IO]("1")
      wallet2 <- Wallet.fileWallet[IO]("2")
      wallet3 <- Wallet.fileWallet[IO]("3")
      
      // Запускаем фоновые процессы
      topup1 <- topupLoop(wallet1, 100.millis).start
      topup2 <- topupLoop(wallet2, 500.millis).start
      topup3 <- topupLoop(wallet3, 2000.millis).start
      printer <- printBalancesLoop(List(wallet1, wallet2, wallet3)).start
      
      // Ждем ввода пользователя
      _ <- IO.readLine
      
      // Отменяем все фоновые процессы
      _ <- topup1.cancel
      _ <- topup2.cancel
      _ <- topup3.cancel
      _ <- printer.cancel
    } yield ()
}