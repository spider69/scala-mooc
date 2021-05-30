package homeworks.futures

import scala.concurrent.{ExecutionContext, Future}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] =
  {
    val futuresWithError = futures.map { future =>
      future
        .map(Right(_))
        .recover {
          case ex: Throwable =>
            Left(ex)
        }
    }

    Future.sequence(futuresWithError).map { list =>
      list.foldRight((List.empty[A], List.empty[Throwable])) {
        case (value, (results, errors)) =>
          value match {
            case Left(error) =>
              (results, error :: errors)
            case Right(result) =>
              (result :: results, errors)
          }
      }
    }
  }

}
